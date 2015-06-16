-module('relflow').
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'relflow').
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 relflow"}, % How to use the plugin
            {opts, opts()},                   % list of options understood by the plugin
            {short_desc, "release workflow util"},
            {desc, "release workflow util"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

opts() ->
    [
     {upfrom, $u, "upfrom", string,
      "Git revision/tag to upgrade from (for appup generation)"},
     {nextver, $x, "nextversion", {string, "auto"},
      "The (deb-compatible) version string to use for the next release"},
     {relxfile, $r, "relxfile", {string, "./rebar.config"},
      "Path to relx.config, for adding new release section"},
     {force, $f, "force", {boolean, false},
      "Force relflow to run even with uncommitted local changes"}
    ].

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RebarState) ->
    Time = utctime(),
    State0 = relflow_state:set_default_nextver(new_rel_vsn(Time),
              relflow_state:nextappver(new_app_vsn(Time),
               relflow_state:new(RebarState))),

    Rev = relflow_state:upfrom(State0),
    OldRelVer = relflow_git:relver_at(Rev),
    State = relflow_state:oldrelver(OldRelVer, State0),

    ChangesSinceRev = relflow_git:since(Rev),
    ChangeMap = relflow_appup:generate_appups(ChangesSinceRev, State),

    rebar_api:debug("relflow upgrading from release ~s to ~s",[relflow_state:oldrelver(State),
                                                               relflow_state:nextver(State)]),
    rebar_api:debug("bumped applications will use vsn: ~s", [relflow_state:nextappver(State)]),
    exec(ChangeMap, State),
    {ok, RebarState}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).


utctime() -> erlang:localtime_to_universaltime(erlang:localtime()).

new_rel_vsn(Time) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = Time,
    lists:flatten(
      io_lib:format("~4.10.0B~2.10.0B~2.10.0B.~2.10.0B~2.10.0B~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec])).

new_app_vsn(Time) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = Time,
    lists:flatten(
      io_lib:format("~4.10.0B~2.10.0B~2.10.0B-~2.10.0B~2.10.0B~2.10.0B-relflow",
        [Year, Month, Day, Hour, Min, Sec])).

exec(Map, State) ->
    NewRelVsn = relflow_state:nextver(State),
    case NewRelVsn > relflow_state:oldrelver(State) of
        true  -> ok;
        false -> throw("NewRelVsn is lower than OldRelVsn? ERRRRR")
    end,
    lists:foreach(fun({_AppName, #{appup_path := Path,
                                   appup_term := T,
                                   appsrc_path := AppSrc,
                                   next_vsn := NextVsn}}) ->
        rebar_api:info("Replace ~s",[Path]),
        Contents = io_lib:format("~p.",[T]),
        ok = filelib:ensure_dir(Path),
        ok = file:write_file(Path, Contents),
        %rebar_api:info("Bumping version in ~s", [AppSrc]),
        ok = relflow_rewriter:set_appfile_version(AppSrc, NextVsn)
    end, maps:to_list(Map)),
    rebar_api:info("Rewriting release vsn in rebar.config: ~s", [NewRelVsn]),
    relflow_rewriter:set_rebar_relx_version("rebar.config", NewRelVsn),
    %% print summary
    FilesTouched = lists:foldl(
        fun(#{appup_path := F1, appsrc_path := F2}, Acc) ->
        [F1, F2 | Acc]
    end, ["rebar.config"], maps:values(Map)),
    %% git things
    exec_cmd("git add ~s", [string:join(FilesTouched, " ")]),
    exec_cmd("git commit -m\"relflow ~s --> ~s\"", [relflow_state:oldrelver(State), NewRelVsn]),
    exec_cmd("git tag v~s", [NewRelVsn]),
    rebar_api:info("Now run:  git push", []).

exec_cmd(S,A) ->
    Str = lists:flatten(io_lib:format(S,A)),
    rebar_api:info("[running] $ ~s",[Str]),
    ok.
    %rebar_api:info("~s",[os:cmd(Str)]).
