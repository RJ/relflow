-module(relflow).
-compile(export_all).
-export([main/1]).
-include("relflow.hrl").
-record(app, {name, vsn, src, ebin, mods=[]}).
%-record(release, {name, vsn, apps=[]}).

main(Args) ->
    application:load(relflow),
    InitialState = #state{},
    relflow_log:init(relflow, "info"),
    case relflow_cli:parse_args(Args, InitialState) of
        {ok, State = #state{}} ->
            %?INFO("STATE: ~p",[State]),
            relflow_log:init(relflow, State#state.loglevel),
            try run(State) of
                _ -> ok
            catch
                throw:Thrown ->
                    ?ERROR("Uncaught error: ~p\n",[Thrown]),
                    erlang:halt(1)
            end;
        _ ->
            erlang:halt(1)
    end.

run(State = #state{}) ->
    try
        do_appups(State)
    catch
        throw:{err, S, A} ->
            ?ERROR(S,A),
            init:stop(1)
    end.

get_oldrel_info(State=#state{upfrom=OldV,relname=RelName}) ->
    OldRelPath = filename:join([State#state.relpath,"releases",OldV, RelName ++ ".rel"]),
    ?DEBUG("upfrom release relfile: ~s",[OldRelPath]),
    FilterFun = fun(_) -> true end,
    case ec_file:exists(OldRelPath) of
        true ->
            {RelName, OldVsn, OldApps} = parse_rel_file(OldRelPath, "_rel/"++RelName++"/lib", FilterFun),
            {ok, OldVsn, OldApps};
        false ->
            {error, rel_not_found}
    end.

do_appups(State = #state{}) ->
    case get_oldrel_info(State) of
        {ok, OldVsn, AllOldApps} ->
            do_appups_1(State, OldVsn, AllOldApps);
        {error, rel_not_found} ->
            ?ERROR("Can't find upfrom release vsn ~s",[State#state.upfrom]),
            erlang:halt(1)
    end.

do_appups_1(State, OldVsn, AllOldApps) ->
    ?DEBUG("upfrom release vsn: ~p apps: ~p",[OldVsn, AllOldApps]),
    %% based on old release, find the local paths to the apps from the old rel
    %% and then check if any of the modules are new/changed, so we can generate
    %% appups.
    %% Apps will either be in deps/ or src,ebin or apps/*/src,ebin
    AllCurrentApps = parse_local_appvers(["ebin", "apps", "deps"]),
    ?DEBUG("new release apps: ~p",[AllCurrentApps]),
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

    %?DEBUG("Apps to diff: ~p",[AppPairs]),
    AppPlans = lists:map(fun(Pair={OApp,NApp}) ->
        Plan = make_appup(Pair),
        {OApp,NApp,Plan}
    end, AppPairs),
    ActionablePlans = filter_actionable_plans(AppPlans),
    ?DEBUG("App Plans (~B): ~p",[length(ActionablePlans),ActionablePlans]),
    case ActionablePlans of
        [] ->
            ?INFO("No changed apps detected. Nothing to do.",[]),
            erlang:halt(0);
        _ ->
            maybe_exec_plans(State, ActionablePlans)
    end.

filter_actionable_plans(Plans) ->
    lists:filter(fun({_,_,not_upgraded}) -> false; (_) -> true end, Plans).

maybe_exec_plans(State, AppPlans) ->
    print_app_plans(AppPlans),
    case relflow_utils:prompt_yn("Write .appup(s) and bump versions?", true) of
        true ->
            exec_plans(State, AppPlans);
        false ->
            State
    end.

exec_plans(State, AppPlans) ->
    State2 = process_app_plans(AppPlans, State),
    RelxFile = State2#state.relxfile,
    {ok, NewRelVsn} = relflow_vsn:bump_relx_vsn(RelxFile, list_to_atom(State2#state.relname), State2#state.upfrom),
    ?INFO("New release version: ~s", [NewRelVsn]),
    State2.

process_app_plans(L, State) ->
    lists:foldl(fun process_app_plan/2, State, L).

process_app_plan({_OApp, _NApp=#app{name=Appname,vsn=Vsn}, not_upgraded}, State) ->
    ?DEBUG("app: ~s-~s unchanged",[Appname, Vsn]),
    State;

process_app_plan({OApp,NApp,{needs_version_bump, UpInstructions, Level}}, State) when is_atom(Level) ->
    %% we know we need to dump the app vsn at this point.
    #app{name=AppName,vsn=CurrVsn} = OApp,
    AppSrcFile = NApp#app.src  ++ "/" ++ atom_to_list(NApp#app.name) ++ ".app.src",
    AppFile    = NApp#app.ebin ++ "/" ++ atom_to_list(NApp#app.name) ++ ".app",
    {ok, CurrVsn, NewVsn} = relflow_vsn:bump_dot_apps(AppSrcFile, AppFile, Level),
    ?INFO("app: ~s-~s\t  --[~s bump]-->\t  ~s-~s",[AppName, CurrVsn, Level, AppName, NewVsn]),
    DownInstructions = reverse_appup_instructions(UpInstructions),
    AppUpTerm = {NewVsn,
                 [{CurrVsn, UpInstructions}],
                 [{CurrVsn, DownInstructions}]
                },
    AppUpPath = NApp#app.ebin ++ "/" ++ atom_to_list(NApp#app.name) ++ ".appup",
    %% TODO what if this vsn->vsn upgrade term exists in appup already
    ok = file:write_file(AppUpPath, io_lib:format("~p.\n\n",[AppUpTerm])),
    ?INFO("Wrote ~s.appup: ~s",[AppName, AppUpPath]),
    State.

print_app_plans(Plans) ->
    lists:foreach(
        fun ({_OApp, _App, not_upgraded}) ->
                ok;
            ({_OApp, App, {needs_version_bump,Instrs,Level}}) ->
                NewVsn = relflow_vsn:bump_version(App#app.vsn, Level),
                ?INFO("~s-~s\t ~s increase to ~s",[App#app.name, App#app.vsn, Level, NewVsn]),
                lists:foreach(fun(Inst) ->
                    ?DEBUG(" * ~p",[Inst])
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
    ModsA = [ {M, filename:join([OldApp#app.ebin, atom_to_list(M) ++ ".beam"])}
              || M <- OldApp#app.mods ],
    ModsB = [ {M, filename:join([NewApp#app.ebin, atom_to_list(M) ++ ".beam"])}
              || M <- NewApp#app.mods ],
    OV = OldApp#app.vsn,
    NV = NewApp#app.vsn,
    AppUpT = relflow_beamcmp:appup_from_beam_paths(ModsA, ModsB, OV, NV),
    ?INFO("APPUP ~p",[AppUpT]),
    AppUpT.

%% Figure out if it's a minor or patch bump.
%% patch bumps are all load_modules
guess_level_from_instructions(Instructions) ->
    case lists:filter(fun({load_module,_}) -> false ; (_) -> true end, Instructions) of
        [] -> patch;
        _  -> minor
    end.

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


appup_instruction_for_module(M, OldApp, NewApp) ->
    OldBeamPath = filename:join([OldApp#app.ebin, atom_to_list(M) ++ ".beam"]),
    NewBeamPath = filename:join([NewApp#app.ebin, atom_to_list(M) ++ ".beam"]),
    #app{vsn=OV} = OldApp,
    #app{vsn=NV} = NewApp,
    relflow_beamcmp:instructions_for_upgrade(OldBeamPath, NewBeamPath, OV, NV, M).

appup_for_code_change(M, OldApp, NewApp) ->
    [{update, M, {advanced, [OldApp#app.vsn, NewApp#app.vsn]}}].

appup_for_supervisor(M, Minfo, OldApp, _NewApp) ->
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
parse_app_file(Path) ->
    %io:format("parse_app_file ~s\n",[Path]),
    {ok, [{application, AppName, Sections}]} = file:consult(Path),
    Vsn = proplists:get_value(vsn, Sections),
    Mods = proplists:get_value(modules, Sections),
    PathParts = filename:split(Path),
    EbinPath = relflow_utils:absname(filename:join(lists:sublist(PathParts, length(PathParts)-1))),
    SrcPath = relflow_utils:absname(filename:join([EbinPath, "..", "src"])),
    #app{name=AppName, vsn=Vsn, src=SrcPath, ebin=EbinPath, mods=Mods}.

parse_rel_file(Path, LibsDir, FilterFun) ->
    {ok, [Term]} = file:consult(Path),
    {release, {RelName, RelVsn}, {erts, _ErtsVsn}, AppVers} = Term,
    %% filter out the OTP-provided apps:
    AppFiles = lists:foldl(fun({AppNameA,AppVer}, Acc) ->
        case lists:member(AppNameA, relflow_utils:otp_app_names()) of
            true -> Acc;
            false ->
                AppName = atom_to_list(AppNameA),
                AppFile = filename:join([LibsDir, AppName ++ "-" ++ AppVer, "ebin", AppName ++ ".app"]),
                [AppFile | Acc]
        end
    end, [], AppVers),
    ?DEBUG("Appfiles: ~p",[AppFiles]),
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


