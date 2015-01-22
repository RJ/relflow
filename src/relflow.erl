-module(relflow).
-compile(export_all).
-export([main/1]).
-include("relflow.hrl").
-record(app, {name, vsn, src, ebin, mods=[]}).
-record(release, {name, vsn, apps=[]}).

-define(otp_apps, [
    asn1, common_test, compiler, cosEvent, cosEventDomain, cosFileTransfer,
    cosNotification, cosProperty, cosTime, cosTransactions, crypto, debugger,
    dialyzer, diameter, edoc, eldap, erl_docgen, erl_interface, erts, et,
    eunit, gs, hipe, ic, inets, jinterface, kernel, megaco, mnesia, observer,
    odbc, orber, os_mon, ose, otp_mibs, parsetools, percept, public_key,
    reltool, runtime_tools, sasl, snmp, ssh, ssl, stdlib, syntax_tools,
    test_server, tools, typer, webtool, wx, xmerl
]).

%% Design notes:
%% 1) Construct two #releases{} containing a list of #app{} each
%% 2) Compare apps from the two releases, and generate .appups
%% 3) Profit.

main(Args) ->
    case relflow_cli:parse_args(Args) of
        {ok, State = #state{}} -> run(State);
        _ -> init:stop(1)
    end.

%-record(state, {
        %relname
    %,   relpath
    %,   upfrom
    %,   relvsn
%}).

stderr(S) -> stderr(S,[]).
stderr(S,A) -> io:put_chars(standard_error, [io_lib:format(S,A),"\n"]).

run(State = #state{}) ->
    %io:format("# RUN ~p~n",[State]),
    try
        run_directives(State#state.directives, State)
    catch
        throw:{err, S, A} ->
            stderr(S,A),
            init:stop(1)
    end.

run_directives([], State) ->
    State;

run_directives([appups|Directives], State) ->
    NewState = do_appups(State),
    run_directives(Directives, NewState);

run_directives([listrels|Directives], State) ->
    Rels = list_releases(State),
    %io:format("~p\n",[Rels]),
    run_directives(Directives, State).

list_releases(#state{relpath=Relpath}) ->
    Dir = Relpath ++ "/releases/",
    case file:list_dir("_rel/relsandbox/releases") of
        {ok, Files} ->
            lists:reverse(lists:foldl(fun(F, Acc) ->
                Path = Dir ++ F,
                case filelib:is_dir(Path) of
                    true -> [{F, Path}|Acc];
                    false -> Acc
                end
            end, [], Files));
        {error, _Err} ->
            throw({err, "No releases dir found @ ~s", [Dir]})
    end.

get_oldrel_info(State=#state{upfrom=OldV,relname=RelName}) ->
    OldRelPath = filename:join([State#state.relpath,"releases",OldV, RelName ++ ".rel"]),
    %io:format("oldrelpath: ~s\n",[OldRelPath]),
    FilterFun = fun(_) -> true end,
    {RelName, OldVsn, OldApps} = parse_rel_file(OldRelPath, "_rel/"++RelName++"/lib", FilterFun),
    {OldVsn, OldApps}.


do_appups(State = #state{}) ->
    {OldVsn, AllOldApps} = get_oldrel_info(State),
    %io:format("OLD vsn: ~p apps: ~p\n",[OldVsn, AllOldApps]),
    %% based on old release, find the local paths to the apps from the old rel
    %% and then check if any of the modules are new/changed, so we can generate
    %% appups.
    %% Apps will either be in deps/ or src,ebin or apps/*/src,ebin
    AllCurrentApps = parse_local_appvers(["ebin", "apps", "deps"]),
    %io:format("NEW apps (all): ~p\n",[AllCurrentApps]),

    %% since we dont need appup instructions for added/removed apps, just find
    %% the list of new apps that also are in the old release, for diffing.
    %% we want a list of [ {OldApp, NewApp}, ... ]
    AppPairs = lists:foldl(
        fun(OldApp, Acc) ->
            case lists:keyfind(OldApp#app.name, #app.name, AllCurrentApps) of
                false -> Acc;
                CurrApp -> [ {OldApp, CurrApp} | Acc ]
            end
        end, [], AllOldApps),

    %io:format("Apps to diff: ~p\n",[AppPairs]),
    AppPlans = lists:map(fun(Pair={OApp,NApp}) ->
        Plan = make_appup(Pair),
        {OApp,NApp,Plan}
    end, AppPairs),

    %io:format("APPPLANS:~p\n",[AppPlans]),
    print_app_plans(AppPlans),
    case prompt_yn("Write .appup(s) and bump versions?", true) of
        true ->
            process_app_plans(AppPlans),
            RelxFile = "./relx.config",
            {ok, NewRelVsn} = relflow_vsn:bump_relx_vsn(RelxFile, list_to_atom(State#state.relname), State#state.upfrom),
            State;
        false ->
            State
    end.

process_app_plans(L) ->
    [ process_app_plan(D) || D <- L ].

process_app_plan({_OApp, _NApp=#app{name=Appname,vsn=Vsn}, not_upgraded}) ->
    io:format("App '~s' version '~s' - not upgraded, no changes needed.~n",[Appname, Vsn]);

process_app_plan({OApp,NApp,{needs_version_bump, UpInstructions, Level}}) when is_atom(Level) ->
    %% we know we need to dump the app vsn at this point.
    #app{name=Appname,vsn=CurrVsn} = OApp,
    io:format("App '~s' version '~s' - changes detected, needs version bump.~n",[Appname, CurrVsn]),
    AppSrcFile = NApp#app.src  ++ "/" ++ atom_to_list(NApp#app.name) ++ ".app.src",
    AppFile    = NApp#app.ebin ++ "/" ++ atom_to_list(NApp#app.name) ++ ".app",
    {ok, CurrVsn, NewVsn} = relflow_vsn:bump_dot_apps(AppSrcFile, AppFile, Level),
    DownInstructions = reverse_appup_instructions(UpInstructions),
    AppUpTerm = {NewVsn,
                 [{CurrVsn, UpInstructions}],
                 [{CurrVsn, DownInstructions}]
                },
    AppUpPath = NApp#app.ebin ++ "/" ++ atom_to_list(NApp#app.name) ++ ".appup",
    %% TODO what if this vsn->vsn upgrade term exists in appup already
    ok = file:write_file(AppUpPath, io_lib:format("~p.\n\n",[AppUpTerm])),
    io:format("Wrote ~s\n",[AppUpPath]),
    ok.

%process_app_plan({N, Va, Vb, _App, {appup, Path, Contents}}) ->
    %io:format("~s.appup generated for ~s --> ~s~n",[N, Va, Vb]),
    %io:format("~p~n", [Contents]),
    %case filelib:is_file(Path) of
        %true ->
            %{ok, Terms} = file:consult(Path),
            %case lists:member(Contents, Terms) of
                %true ->
                    %io:format("Identical directive already in file.~n");
                %false ->
                    %NewContents = prep_appfile_contents([ 
                            %Contents | remove_appup(Terms, Va, Vb) ]),
                    %ok = file:write_file(Path, NewContents),
                    %io:format("Wrote: ~s~n", [Path])
            %end;
        %false ->
            %ok = file:write_file(Path, prep_appfile_contents([Contents]))
    %end.

print_app_plans(Plans) ->
    lists:foreach(
        fun ({OApp, App, not_upgraded}) ->
                io:format("~20.. s    no change @ ~s\n",[App#app.name, App#app.vsn]);
            ({OApp, App, {needs_version_bump,Instrs,Level}}) ->
                NewVsn = relflow_vsn:bump_version(App#app.vsn, Level),
                io:format("~20.. s    APPUP ~s -> ~s\n",[App#app.name, App#app.vsn, NewVsn]),
                lists:foreach(fun(Inst) ->
                    io:format("                         * ~p\n",[Inst])
                end, Instrs)
        end, Plans).

parse_local_appvers(Dirs) ->
    AppFiles = find_app_files(Dirs),
    AppSpecs = lists:map(fun(AppFile) ->
        parse_app_file(AppFile)
    end, AppFiles),
    sort_apps(AppSpecs).


find_app_files(Dirs) -> find_app_files(Dirs, []).

find_app_files([],         Acc) -> Acc;
find_app_files([Dir|Dirs], Acc) ->
    AppFiles = filelib:fold_files(Dir, ".*\\.app$", true, 
                                  fun(F,A) -> [F|A] end, []),
    find_app_files(Dirs, Acc ++ AppFiles).

make_appup({OldApp = #app{}, NewApp = #app{}}) ->
    {UpInstructions, Level} = make_appup_instructions(OldApp, NewApp),
    case UpInstructions of
        [] when OldApp#app.vsn =:= NewApp#app.vsn ->
            not_upgraded;
        _L when is_list(_L) ->
            {needs_version_bump, UpInstructions, Level}
    end.

make_appup_instructions(OldApp = #app{}, NewApp = #app{}) ->
    OldMods = OldApp#app.mods,
    NewMods = NewApp#app.mods,
    %% Detect newly added, removed, or modules that exist in both apps:
    %% Modules in both old and new will be compared to see if they need loading
    AddedMods   = lists:dropwhile(fun(E)->lists:member(E,OldMods)end, NewMods),
    RemovedMods = lists:dropwhile(fun(E)->lists:member(E,NewMods)end, OldMods),
    SameMods    = sets:to_list(sets:intersection(sets:from_list(OldMods),
                                                 sets:from_list(NewMods))),
    Instructions = lists:flatten([ appup_instruction_for_module(M, OldApp, NewApp) || M <- sort_mods(SameMods) ]),
    %% Figure out if it's a minor or patch bump.
    %% patch bumps are all load_modules
    Level = case lists:filter(fun({load_module,_}) -> false ; (_) -> true end, Instructions) of
        [] -> patch;
        _  -> minor
    end,
    FinalInstructions = lists:flatten([
        [ {add_module, M} || M <- AddedMods ],
        Instructions,
        [ {delete_module, M} || M <- RemovedMods ]
    ]),
    {FinalInstructions, Level}.

%% Sort _sup modules first
sort_mods(Mods) ->
    lists:sort(
        fun(A,B) ->
                As = atom_to_list(A),
                Bs = atom_to_list(B),
                case {lists:suffix("_sup", As), lists:suffix("_sup", Bs)} of
                    {true, false} -> true;
                    {false, true} -> false;
                    _             -> A =< B
                end
        end,
    Mods).

reverse_appup_instructions(L) ->
    reverse_appup_instructions(L,[]).

reverse_appup_instructions([], Acc) -> Acc;
reverse_appup_instructions([I|Rest], Acc) ->
    reverse_appup_instructions(Rest, [reverse_appup_instruction(I) | Acc]).

reverse_appup_instruction({add_module, M})    -> {delete_module, M};
reverse_appup_instruction({delete_module, M}) -> {add_module, M};
reverse_appup_instruction({update, M, {advanced, [A,B]}}) -> {update, M, {advanced, [B,A]}};
reverse_appup_instruction({apply, {M, sup_upgrade_notify, [A,B]}}) -> {apply, {M, sup_upgrade_notify, [B,A]}};
reverse_appup_instruction(Other) -> Other.

%% detect nature of change to a beam file
beam_diff(PathA, PathB) when PathA == PathB ->
    false;
beam_diff(PathA, PathB) ->
    {ok, BeamA} = file:read_file(PathA),
    {ok, BeamB} = file:read_file(PathB),
    case BeamA == BeamB of
        true  -> false; %% same file contents
        false -> beam_diff_type(BeamA, BeamB)
    end.

beam_diff_type(BeamA, BeamB) when is_binary(BeamA), is_binary(BeamB) ->
    Minfo = extract_module_info(BeamB),
    HasBehaviour = fun(B) ->
        lists:member(B, proplists:get_value(behaviours, Minfo, []))
    end,
    HasCodeChange = lists:member({code_change, 3}, Minfo) orelse
                        lists:member({code_change, 4}, Minfo),
    case HasBehaviour(supervisor) of
        true ->
            {supervisor, Minfo};
        false ->
            case HasCodeChange of
                true ->
                    case did_state_record_change(BeamA, BeamB) of
                        true  -> code_change;
                        false -> load_module
                    end;
                false ->
                    load_module
            end
    end.

appup_instruction_for_module(M, OldApp, NewApp) ->
    OldBeamPath = filename:join([OldApp#app.ebin, atom_to_list(M) ++ ".beam"]),
    NewBeamPath = filename:join([NewApp#app.ebin, atom_to_list(M) ++ ".beam"]),
    case beam_diff(OldBeamPath, NewBeamPath) of
        false ->
            [];
        load_module ->
            [{load_module, M}];
        {supervisor, Minfo} ->
            appup_for_supervisor(M, Minfo, OldApp, NewApp);
        code_change ->
            appup_for_code_change(M, OldApp, NewApp)
    end.

appup_for_code_change(M, OldApp, NewApp) ->
    [{update, M, {advanced, [OldApp#app.vsn, NewApp#app.vsn]}}].

appup_for_supervisor(M, Minfo, OldApp, NewApp) ->
    I = {update, M, supervisor},
    %% Support the Dukes of Erl erlrc convention
    %% ie. call sup_upgrade_notify/2 if exported.
    case lists:member({sup_upgrade_notify, 2},
            proplists:get_value(exports, Minfo, [])) of
        true ->
            %% bit of a nasty assumption here, we don't know the new app vsn yet
            %% so we assume it's a minor bump. can't be patch bump since there
            %% is a change to a supervisor.
            [I, {apply, {M, sup_upgrade_notify,
                         [OldApp#app.vsn, relflow_vsn:bump_version(OldApp#app.vsn,minor)]}}];
        false ->
            [I]
    end.


read_beam_records(Beam) ->
    AbstChunks = beam_lib:chunks(Beam,[abstract_code]),
    {ok, {_ModName, [{abstract_code, {raw_abstract_v1, AC}}]}} = AbstChunks,
    Recs = lists:foldl(fun
        ({attribute,_Line,record,RecTuple={_RecName,_RecInfo}},Acc) ->
            [RecTuple|Acc];
        (_, Acc) ->
            Acc
    end, [], AC),
    lists:sort(Recs).

%% would be better to use the abstract code to detect the record name
%% returned from init/1, ie {ok, #state{}}, and check if that has changed.
%% but 99.9% of the time it's called 'state', so this will do for now.
did_state_record_change(BeamA, BeamB) ->
    proplists:get_value(state, read_beam_records(BeamA)) /=
        proplists:get_value(state, read_beam_records(BeamB)).

extract_module_info(Beam) ->
    Chunker = fun(K) ->
        case beam_lib:chunks(Beam, [K]) of
            {ok, {_, [{K, Result}]}} -> Result;
            _ -> []
        end
    end,
    Exports = Chunker(exports),
    %% Tidy up the americanised spelling of behaviour
    Behaviours = lists:usort(
                    lists:flatten(
                        proplists:get_value(behaviour, Chunker(attributes), []) ++
                        proplists:get_value(behavior,  Chunker(attributes), []))),
    [
        {behaviours, Behaviours},
        {exports, Exports}
    ].

parse_app_file(Path) ->
    %io:format("parse_app_file ~s\n",[Path]),
    {ok, [{application, AppName, Sections}]} = file:consult(Path),
    Vsn = proplists:get_value(vsn, Sections),
    Mods = proplists:get_value(modules, Sections),
    PathParts = filename:split(Path),
    EbinPath = absname(filename:join(lists:sublist(PathParts, length(PathParts)-1))),
    SrcPath = absname(filename:join([EbinPath, "..", "src"])),
    #app{name=AppName, vsn=Vsn, src=SrcPath, ebin=EbinPath, mods=Mods}.

parse_rel_file(Path, LibsDir, FilterFun) ->
    {ok, [Term]} = file:consult(Path),
    {release, {RelName, RelVsn}, {erts, _ErtsVsn}, AppVers} = Term,
    %% filter out the OTP-provided apps:
    AppFiles = lists:foldl(fun({AppNameA,AppVer}, Acc) ->
        case lists:member(AppNameA, ?otp_apps) of
            true -> Acc;
            false ->
                AppName = atom_to_list(AppNameA),
                AppFile = filename:join([LibsDir, AppName ++ "-" ++ AppVer, "ebin", AppName ++ ".app"]),
                [AppFile | Acc]
        end
    end, [], AppVers),
    %io:format("APPFILES: ~p\n",[AppFiles]),
    Apps0 = [ parse_app_file(F) || F <- AppFiles ],
    Apps = lists:filter(fun(#app{name=Name}) ->
        FilterFun(atom_to_list(Name))
    end, Apps0),
    {RelName, RelVsn, sort_apps(Apps)}.


%% sort [#app{},...] by name
sort_apps(Apps) -> lists:sort(fun(#app{name=A},#app{name=B}) -> A =< B end, Apps).


prep_appfile_contents(Terms) ->
    [
        io_lib:format("~p.~n",[C]) 
        || C <- Terms
    ].

remove_appup(Terms, Va, Vb) ->
    lists:filter(fun ({A, [{B,_}], [{B,_}]}) -> 
                        not (A =:= Va andalso B =:= Vb);
                     (_) -> 
                        true 
                 end,
                 Terms).


%% via http://www.codecodex.com/wiki/Determine_if_two_file_paths_refer_to_the_same_file
%% hope it works properly..
absname(Path) ->
    Path2 = filename:split(Path),
    case string:chr(hd(Path2), $/) of
        0 -> {ok, Cwd} = file:get_cwd(),
             Abs = lists:reverse(filename:split(Cwd)),
             absname(Path2, Abs);
        _ -> absname(Path2, [])
    end.

absname([], Absname) ->
    filename:join(lists:reverse(Absname));
absname([H|T], Absname) ->
    case H of
        "."  -> absname(T, Absname);
        ".." -> absname(T, tl(Absname));
        _    -> absname(T, [H|Absname])
    end.

prompt_yn(Prompt, Default) when is_boolean(Default) ->
    YN = case Default of
        true -> "Yn";
        false -> "yN"
    end,
    Str = lists:flatten(io_lib:format("~s [~s] > ", [Prompt, YN])),
    case io:get_line(Str) of
        "\n"  -> Default;
        "y\n" -> true;
        "Y\n" -> true;
        "n\n" -> false;
        "N\n" -> false;
        _ -> false
    end.
