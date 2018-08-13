%% does git things using os:cmd("git...") and parses the output.
%%
%% This has some hardcoded paths like src and apps
%% ideally it would ask rebar about paths so it works with non-standard setups
%%
%% for now, single-app repos (src/) and apps/<app..> repos will work
-module(relflow_git).
-export([since/1, is_clean/0, relver_at/1]).

since(Rev) when is_list(Rev) ->
    R1 = since_revision(Rev),
    R2 = add_app_paths(R1),
    appvers_at_revision(R2, Rev).

is_clean() ->
    Cmd = "git diff-index --quiet HEAD -- && echo clean || echo dirty",
    case os:cmd(Cmd) of
        "clean" ++ _ -> true;
        "dirty" ++ _ -> false
    end.

relver_at(Rev) ->
    File = "./rebar.config",
    Cmd = fmt("git show ~s:~s | grep relflow-release-version-marker | awk -F '\"' '{print $2; exit}'", [Rev, File]),
    RelVsn = string:strip(os:cmd(Cmd), right, $\n),
    RelVsn.
%%

fmt(S,A) -> lists:flatten(io_lib:format(S,A)).

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

since_revision(Rev) when is_list(Rev) ->
    gather_changed_modules(changed_modules_since(Rev)).


appver_at_revision(Rev, Name, DirType) ->
    Cmd = case DirType of
        apps_dir ->
            fmt("git show ~s:apps/~s/src/~s.app.src", [Rev, Name, Name]);
        single_src_dir ->
            fmt("git show ~s:src/~s.app.src", [Rev, Name])
    end,
    Str = os:cmd(Cmd),
    {application, Name, AppOpts} = eval(Str),
    proplists:get_value(vsn, AppOpts).

appvers_at_revision(Changes, Rev) when is_list(Rev) ->
    maps:map(
      fun(AppName, AppMap) ->
              SS = case maps:get(single_src_app, AppMap) of
                       true  -> single_src_dir;
                       false -> apps_dir
                   end,
              Vsn = appver_at_revision(Rev, AppName, SS),
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

git_diff_names(Rev) when is_list(Rev) ->
    Cmd = "git diff --name-status "++Rev++" | grep -E '\.(erl|hrl|src)$' | grep -E \"\t(apps|src)\"",
    Lines = string:tokens(os:cmd(Cmd), "\n"),
    %io:format("LINES: ~s\n~s\n",[Cmd,Lines]),
    Lines.

git_status_to_atom("D") -> deleted;
git_status_to_atom("A") -> added;
git_status_to_atom(_)   -> modified.

changed_modules_since(Rev) when is_list(Rev) ->
    lists:sort(lists:flatten(lists:map(fun(Line) ->
        case string:tokens(Line, "\t") of
            [Status, Path] ->
                StatusAtom = git_status_to_atom(Status),
                %% Module is usually the module name, for use in the .appup
                %% If the change is to a .hrl or .app.src, we don't necessarily
                %% need an appup instruction. We emit $appmeta here, indicating
                %% we should just bump the app version with no special appup instructions needed.
                Module = case filename:extension(Path) of
                    ".erl" -> list_to_atom(filename:basename(Path, ".erl"));
                    ".hrl" -> rebar_api:warn("WARNING: ~s changed, relflow doesn't deduce which .erls use it\n", [Path]),
                              %% you better make a whitespace change or something, to
                              %% any erl files that -include() this hrl. Otherwise relflow
                              %% won't generate an appup instruction for them.
                              %% This is considered a bug. Could be fixed in a nasty way
                              %% by grepping for include statements in erl files :)
                              '$appmeta';
                    ".src" -> %% .app.src
                              '$appmeta'
                end,
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
                        {AppName, Module, ModInfo};

                    %% assume that any other path isn't relevant to relup stuff: 
                    _ ->
                        []
                end;
            _Else ->
                io:format("git error: ~s\n",[Line]),
                throw(git_error)
        end
        end,
        git_diff_names(Rev)
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

gather_changed_modules([{AppName, Module, ModInfo} | Rest], [{AppName,Changes} | AccRest]) ->
    gather_changed_modules(Rest, [{AppName, [{Module, ModInfo}|Changes]}|AccRest]);
gather_changed_modules([{AppName, Module, ModInfo} | Rest], Acc) ->
    gather_changed_modules(Rest, [{AppName, [{Module, ModInfo}]}|Acc]);
gather_changed_modules([], Acc) ->
    Acc.


