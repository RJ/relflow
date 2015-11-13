%% does sccm things using os:cmd("git...") and parses the output.
%%
%% This has some hardcoded paths like src and apps
%% ideally it would ask rebar about paths so it works with non-standard setups
%%
%% for now, single-app repos (src/) and apps/<app..> repos will work
-module(relflow_sccm).
-export([since/2, is_clean/1, relver_at/2, commit/6]).

get_cmd(hg, add) ->"hg add ~s";
get_cmd(hg, tag) ->"hg tag  \"v~s\" -m \"~s\"";
get_cmd(hg, commit) ->"hg commit -m\"relflow ~s --> ~s\"";
get_cmd(hg, diff)->"hg sum | grep -q 'commit: (clean)' && echo clean || echo dirty";
get_cmd(hg, show)->"hg cat -r ~s ~s | grep relflow-release-version-marker | awk -F '\"' '{print $2; exit}'";
get_cmd(hg, stat_multi)->"hg cat -r ~s apps/~s/src/~s.app.src";
get_cmd(hg, stat_single)->"hg cat -r ~s src/~s.app.src";

get_cmd(git, add) ->"git add ~s";
get_cmd(git, tag) ->"git tag -a \"v~s\" -m \"~s\"";
get_cmd(git, commit) ->"git commit -m\"relflow ~s --> ~s\"";
get_cmd(git, diff)->"git diff-index --quiet HEAD -- && echo clean || echo dirty";
get_cmd(git, show)->"git show ~s:~s | grep relflow-release-version-marker | awk -F '\"' '{print $2; exit}'";
get_cmd(git, stat_multi)->"git show ~s:apps/~s/src/~s.app.src";
get_cmd(git, stat_single)->"git show ~s:src/~s.app.src".

get_cmd(hg, diff_names, Rev)->"hg status --rev "++Rev++" -I 'src/**/*.erl' -I'apps/**/*.erl'";
get_cmd(git, diff_names, Rev)-> "git diff --name-status "++Rev++" | grep -E '\.erl$' | grep -E \"\t(apps|src)\"".

status_to_atom(git, "D") -> deleted;
status_to_atom(git, "A") -> added;
status_to_atom(git, _)   -> modified;
status_to_atom(hg, "R") -> deleted;
status_to_atom(hg, "A") -> added;
status_to_atom(hg, "M")   -> modified.

commit(FilesTouched, Sccm, AutoCommit, Force, OldVer, NewRelVsn)->
    AddCmds = [ fmt(get_cmd(Sccm, add), [AddFile]) || AddFile <- FilesTouched ],
    SccmCmds = AddCmds ++ [
        fmt(get_cmd(Sccm, commit), [OldVer, NewRelVsn]),
        fmt(get_cmd(Sccm, tag), [NewRelVsn, NewRelVsn])
    ],
    case {AutoCommit, Force} of
        {true, false}  -> exec_sccm(SccmCmds);
        {false, true}  -> exec_sccm(SccmCmds);
        {false, false} -> print_cmds(SccmCmds);
        {true, true}   ->
            rebar_api:warn("Not running sccm commands, because you --forced",[]),
            print_cmds(SccmCmds)
    end.

since(Rev, Sccm) when is_list(Rev) ->
    R1 = since_revision(Rev, Sccm),
    R2 = add_app_paths(R1),
    appvers_at_revision(R2, Rev, Sccm).

is_clean(Sccm) ->
    rebar_api:info("sccm is ~p",[Sccm]),
    case os:cmd(get_cmd(Sccm, diff)) of
        "clean" ++ _ -> true;
        "dirty" ++ _ -> false
    end.

relver_at(Rev, Sccm) ->
    File = "./rebar.config",
    Cmd = fmt(get_cmd(Sccm, show), [Rev, File]),
    RelVsn = string:strip(os:cmd(Cmd), right, $\n),
    RelVsn.


add_app_paths(Changes) ->
    maps:map(
      fun(AppName, AppMap) ->
              case maps:get(single_src_app, AppMap) of
                  true ->
                      AppSrc = fmt("src/~s.app.src", [AppName]),
                      Appup  = fmt("ebin/~s.appup",  [AppName]),
                      M1 = maps:put(appup_path,  Appup,  AppMap),
                      maps:put(appsrc_path, AppSrc, M1);
                  false ->
                      AppSrc = fmt("apps/~s/src/~s.app.src", [AppName, AppName]),
                      Appup  = fmt("apps/~s/ebin/~s.appup",  [AppName, AppName]),
                      M1 = maps:put(appup_path,  Appup,  AppMap),
                      maps:put(appsrc_path, AppSrc, M1)
              end
      end,
      Changes
     ).

since_revision(Rev, Sccm) when is_list(Rev) ->
    gather_changed_modules(changed_modules_since(Rev, Sccm)).

appver_at_revision(Rev, Name, DirType, Sccm) ->
    Cmd = case DirType of
        apps_dir ->
            fmt(get_cmd(Sccm, stat_multi), [Rev, Name, Name]);
        single_src_dir ->
            fmt(get_cmd(Sccm, stat_single), [Rev, Name]);
        X -> throw({unhandled_dirtpe, X})
    end,
    Str = os:cmd(Cmd),
    {application, Name, AppOpts} = eval(Str),
    proplists:get_value(vsn, AppOpts).

appvers_at_revision(Changes, Rev, Sccm) when is_list(Rev) ->
    maps:map(
      fun(AppName, AppMap) ->
              SS = case maps:get(single_src_app, AppMap) of
                       true  -> single_src_dir;
                       false -> apps_dir
                   end,
              Vsn = appver_at_revision(Rev, AppName, SS, Sccm),
              maps:put(vsn, Vsn, AppMap)
      end,
      Changes
     ).

eval(S) ->
    {ok,Scanned,_} = erl_scan:string(S),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    Env = [],
    {value, Term, Env} = erl_eval:exprs(Parsed,Env),
    Term.

diff_names(Rev, Sccm) when is_list(Rev) ->
    string:tokens(os:cmd(get_cmd(Sccm, diff_names, Rev)), "\n").


changed_modules_since(Rev, Sccm) when is_list(Rev) ->
    lists:sort(lists:flatten(lists:map(fun(Line) ->
                                           rebar_api:debug("lines: ~p", [Rev]),
        case string:tokens(Line, "\t") of
            [Status, Path] ->
                StatusAtom = status_to_atom(Sccm, Status),
                Module = list_to_atom(filename:basename(Path, ".erl")),
                ModInfo = #{status => StatusAtom, path => Path},
                %% This depends on unchanged rebar3 defaults for source locations,
                %% but we only support apps/ and src/
                %%
                %% directories where OTP applications for the project can be located
                %% {project_app_dirs, ["apps", "lib", "."]}.
                case filename:split(Path) of
                    ["apps", _, "test" | _] ->
                        [];

                    ["test" | _] ->
                        [];

                    ["apps", AppName, "src" | _] ->
                        {AppName, Module, ModInfo};

                    ["src" | _] ->
                        %% we lookup the app name at the end, during gathering:
                        AppName = "$$single_app",
                        {AppName, Module, ModInfo}
                end;
            _Else ->
                io:format("git error: ~s\n",[Line]),
                throw(sccm_error)
        end
        end,
        diff_names(Rev, Sccm)
    ))).

gather_changed_modules(List) ->
    lists:foldl(fun({AppStr, Changes}, Acc) ->
           {Name, IsSrcApp} = case AppStr of
                "$$single_app" ->
                    AppSrc = hd(filelib:wildcard("src/*.app.src")),
                    {list_to_atom(filename:basename(AppSrc, ".app.src")), true};
                _ ->
                    {list_to_atom(AppStr), false}
            end,
            maps:put(Name, #{changes => maps:from_list(Changes),
                             single_src_app =>  IsSrcApp}, Acc)
        end,
        #{},
        gather_changed_modules(List, [])
    ).

gather_changed_modules([{AppName, Filename, ModInfo} | Rest], [{AppName,Changes} | AccRest]) ->
    gather_changed_modules(Rest, [{AppName, [{Filename, ModInfo}|Changes]}|AccRest]);
gather_changed_modules([{AppName, Filename, ModInfo} | Rest], Acc) ->
    gather_changed_modules(Rest, [{AppName, [{Filename, ModInfo}]}|Acc]);
gather_changed_modules([], Acc) ->
    Acc.


exec_sccm(Cmds) ->
    lists:foreach(fun(Cmd) ->
        case os:cmd(Cmd) of
            ""  -> rebar_api:info("$ ~s", [Cmd]);
            Res -> rebar_api:info("$ ~s\n~s", [Cmd, string:strip(Res, right)])
        end
    end, Cmds).

print_cmds(Cmds) ->
    S = iolist_to_binary([ [C, "\n"] || C <- Cmds ]),
    rebar_api:info("Recommended commands:\n~s", [S]).

fmt(S,A) -> lists:flatten(io_lib:format(S,A)).
