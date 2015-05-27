-module(relflow).
-export([main/1]).
-include("relflow.hrl").

main(Args) ->
    application:load(relflow),
    InitialState = #state{},
    relflow_log:init(command_line, "info"),
    case relflow_cli:parse_args(Args, InitialState) of
        {ok, State = #state{}} ->
            %?INFO("STATE: ~p",[State]),
            relflow_log:init(command_line, State#state.loglevel),
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


run(State = #state{upfrom=Rev}) ->
    try
        Changes = relflow_changed_files:since(Rev),
        Changes2 = relflow_appup:generate_appups(Changes),
        %io:format("Changes->\n~p\n",[Changes2]),
        maybe_exec(State, Changes2),
        erlang:halt(0)
    catch
        throw:{err, S, A} ->
            ?ERROR(S,A),
            init:stop(1)
    end.


maybe_exec(State, Map) ->
    print_plan(Map),
    case relflow_utils:prompt_yn("Write .appup(s) and bump versions?", true) of
        true ->
            exec(State, Map);
        false ->
            State
    end.

new_rel_vsn() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    lists:flatten(
      io_lib:format("rel-~4.10.0B~2.10.0B~2.10.0B.~2.10.0B~2.10.0B~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec])).

exec(_State, Map) ->
    lists:foreach(fun({_AppName, #{appup_path := Path,
                                   appup_term := T,
                                   appsrc_path := AppSrc,
                                   next_vsn := NextVsn}}) ->
        ?INFO("Writing ~s",[Path]),
        Contents = io_lib:format("~p.",[T]),
        ok = filelib:ensure_dir(Path),
        ok = file:write_file(Path, Contents),
        ?INFO("Bumping version in ~s", [AppSrc]),
        ok = relflow_rewriter:set_appfile_version(AppSrc, NextVsn)
    end, maps:to_list(Map)),
    NewRelVsn = new_rel_vsn(),
    ?INFO("Updating rebar.config release version to: ~s", [NewRelVsn]),
    relflow_rewriter:set_rebar_relx_version("rebar.config", NewRelVsn),
    %% print summary
    FilesTouched = lists:foldl(
        fun(#{appup_path := F1, appsrc_path := F2}, Acc) ->
        [F1, F2 | Acc]
    end, ["rebar.config"], maps:values(Map)),
    io:format("Now run:\n\n"),
    io:format("  git add ~s\n\n", [string:join(FilesTouched, " ")]),
    io:format("  git commit -m\"relflow --> ~s\"\n\n", [NewRelVsn]),
    io:format("  git tag ~s\n\n", [NewRelVsn]),
    io:format("  git push\n\n").

print_plan(Map) ->
    ?INFO("# APPUPS",[]),
    lists:foreach(fun({_AppName, #{appup_path := Path, appup_instructions := Instrs}}) ->
        ?INFO("",[]),
        ?INFO("~s",[Path]),
        lists:foreach(fun(I) ->
            ?INFO(" ~p",[I])
        end, Instrs)
    end, maps:to_list(Map)),
    ?INFO("",[]).
