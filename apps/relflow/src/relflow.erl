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

safe_to_run(#state{force=true}) ->
    true;
safe_to_run(_State) ->
    case relflow_changed_files:is_clean() of
        true -> true;
        false ->
            ?ERROR("Uncommitted changes, aborting. Commit stuff first, please.", []),
            false
    end.

run(State0 = #state{upfrom=Rev}) ->
    try
        case safe_to_run(State0) of
            false -> erlang:halt(4);
            true  -> ok
        end,
        OldRelVer = relflow_changed_files:relver_at(Rev),
        ?INFO("Upgrading from release vsn: ~s",[OldRelVer]),
        State = State0#state{oldrelver=OldRelVer},
        Changes = relflow_changed_files:since(Rev),
        Changes2 = relflow_appup:generate_appups(Changes, State),
        %io:format("Changes->\n~p\n",[Changes2]),
        maybe_exec(State, Changes2),
        erlang:halt(0)
    catch
        throw:{err, S, A} ->
            ?ERROR(S,A),
            erlang:halt(3)
    end.


maybe_exec(State, Map) ->
    print_plan(Map),
    case relflow_utils:prompt_yn("Write .appup(s) and bump versions?", true) of
        true ->
            exec(State, Map);
        false ->
            erlang:halt(1)
    end.

new_rel_vsn(#state{nextver=V}) when V =/= "auto", is_list(V) ->
    V;
new_rel_vsn(_State) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime_to_universaltime(erlang:localtime()),
    lists:flatten(
      io_lib:format("~4.10.0B~2.10.0B~2.10.0B.~2.10.0B~2.10.0B~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec])).

exec(State, Map) ->
    NewRelVsn = new_rel_vsn(State),
    case NewRelVsn > State#state.oldrelver of
        true  -> ok;
        false -> throw("NewRelVsn is lower than OldRelVsn? ERRRRR")
    end,
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
    ?INFO("Updating rebar.config release version to: ~s", [NewRelVsn]),
    relflow_rewriter:set_rebar_relx_version("rebar.config", NewRelVsn),
    %% print summary
    FilesTouched = lists:foldl(
        fun(#{appup_path := F1, appsrc_path := F2}, Acc) ->
        [F1, F2 | Acc]
    end, ["rebar.config"], maps:values(Map)),
    exec_cmd("git add ~s", [string:join(FilesTouched, " ")]),
    exec_cmd("git commit -m\"relflow ~s --> ~s\"", [State#state.oldrelver, NewRelVsn]),
    exec_cmd("git tag v~s", [NewRelVsn]),
    io:format("Now run:\n  git push\n").

exec_cmd(S,A) ->
    Str = lists:flatten(io_lib:format(S,A)),
    io:format("[running] $ ~s\n",[Str]),
    io:format("~s",[os:cmd(Str)]).

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
