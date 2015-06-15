-module(relflow_changed_files).
-include("relflow.hrl").
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
    Cmd = fmt("git show ~s:~s", [Rev, File]),
    Str = os:cmd(Cmd),
    Terms = eval("["++Str++"]."),
    Relx = proplists:get_value(relx, Terms),
    {release, {_RelName,RelVsn}, _} = lists:keysearch(release, 1, Relx),
    RelVsn.
%%

fmt(S,A) -> lists:flatten(io_lib:format(S,A)).

add_app_paths(Changes) ->
    maps:map(
      fun(AppName, AppMap) ->
              AppSrc = fmt("apps/~s/src/~s.app.src", [AppName, AppName]),
              Appup  = fmt("apps/~s/ebin/~s.appup",  [AppName, AppName]),
              M1 = maps:put(appup_path,  Appup,  AppMap),
              M2 = maps:put(appsrc_path, AppSrc, M1),
              M2
      end,
      Changes
     ).

since_revision(Rev) when is_list(Rev) ->
    gather_changed_modules(
      lists:sort(
          changed_modules_since(
            Rev))).


appver_at_revision(Rev, Name) ->
    Cmd = fmt("git show ~s:apps/~s/src/~s.app.src", [Rev, Name, Name]),
    Str = os:cmd(Cmd),
    {application, Name, AppOpts} = eval(Str),
    proplists:get_value(vsn, AppOpts).

appvers_at_revision(Changes, Rev) when is_list(Rev) ->
    maps:map(
      fun(AppName, AppMap) ->
              Vsn = appver_at_revision(Rev, AppName),
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
    Cmd = "git diff --name-status "++Rev++" | grep -E '\.erl$' | grep \"\tapps\"",
    Lines = string:tokens(os:cmd(Cmd), "\n"),
    %io:format("LINES: ~s\n~s\n",[Cmd,Lines]),
    Lines.

changed_modules_since(Rev) when is_list(Rev) ->
    lists:map(fun(Line) ->
        case string:tokens(Line, "\t") of
            [Status, Path] ->
                case string:tokens(Path, "/") of
                    ["apps", AppName, "src" | _PathElements] ->
                        Module = list_to_atom(filename:basename(Path, ".erl")),
                        StatusAtom = case Status of
                            "D" -> deleted;
                            "A" -> added;
                            _   -> modified
                        end,
                        ModInfo = #{status => StatusAtom, path => Path},
                        {AppName, Module, ModInfo}
                end;
            _Else ->
                io:format("git error: ~s\n",[Line]),
                throw(git_error)
        end
        end,
        git_diff_names(Rev)
    ).

gather_changed_modules(List) ->
    lists:foldl(fun({AppStr, Changes}, Acc) ->
            Name = list_to_atom(AppStr),
            maps:put(Name, #{changes => maps:from_list(Changes)}, Acc)
        end,
        #{},
        gather_changed_modules(List, [])
    ).

gather_changed_modules([{AppName, Filename, Line} | Rest], [{AppName,Changes} | AccRest]) ->
    gather_changed_modules(Rest, [{AppName, [{Filename, Line}|Changes]}|AccRest]);
gather_changed_modules([{AppName, Filename, Line} | Rest], Acc) ->
    gather_changed_modules(Rest, [{AppName, [{Filename, Line}]}|Acc]);
gather_changed_modules([], Acc) ->
    Acc.


