-module('relflow').
%-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'relflow').
-define(DEPS, [app_discovery]).
-define(PRV_ERROR(Reason), {error, {?MODULE, Reason}}).

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
            {desc, desc()}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

desc() -> "
Relflow
=======

Examples:
    rebar3 relflow -u v1.2.3        # upgrade from last release, at git tag v1.2.3
    rebar3 relflow init-versions    # reset all vsns using relflow format
    rebar3 relflow --version        # print relflow version

".

opts() ->
    [
     {upfrom, $u, "upfrom", string,
      "Git revision/tag to upgrade from (for appup generation)"},
     {nextver, $x, "nextversion", {string, "auto"},
      "The version string to use for the next release"},
     {autogit, $g, "autogit", {boolean, true},
      "Automatically add and commit relflow changes to git"},
     {force, $f, "force", {boolean, false},
      "Force relflow to run even with uncommitted local changes"},
     {version, $v, "version", undefined,
      "Print relflow version and exit"},
     {semver, $s, "semver", {boolean, false},
      "Uses semantic versioning to compare versions"}
    ].

relflow_version() ->
    {ok, V} = application:get_key(relflow, vsn),
    V.

do(RebarState) ->
    Time = utctime(),
    State0 = relflow_state:set_default_nextver(new_rel_vsn(Time),
              relflow_state:nextappver(new_app_vsn(Time),
               relflow_state:new(RebarState))),

    case relflow_state:version(State0) of
        undefined ->
            do_0(State0);
        _ ->
            io:format("~s\n",[relflow_version()]),
            {ok, RebarState}
    end.

do_0(State) ->
    case relflow_state:task(State) of
        undefined ->
            do_1(State);
        "init-versions" ->
            do_init_versions(State)
    end.

do_1(State0) ->
    case relflow_state:upfrom(State0) of
        undefined ->
            ?PRV_ERROR(no_upfrom);
        Rev ->
            OldRelVer = relflow_git:relver_at(Rev),
            State = relflow_state:oldrelver(OldRelVer, State0),
            do_2(State)
    end.

do_2(State) ->
    rebar_api:debug("relflow upgrading from release ~s to ~s",[
                        relflow_state:oldrelver(State),
                        relflow_state:nextver(State)]),
    rebar_api:debug("bumped applications will use vsn: ~s", [
                        relflow_state:nextappver(State)]),
    ChangesSinceRev = relflow_git:since(relflow_state:upfrom(State)),
    ChangeMap       = relflow_appup:generate_appups(ChangesSinceRev, State),
    exec(ChangeMap, State, fun(A,B) -> exec_1(A,B,fun exec_2/3) end).

do_init_versions(State0) ->
    NextAppVer = relflow_state:nextappver(State0),
    rebar_api:info("New application vsn: ~s", [NextAppVer]),
    BumperFun = fun(_Map, State, NewRelVsn) ->
        Apps = [ {rebar_app_info:name(A), rebar_app_info:app_file_src(A)}
                 || A <- rebar_state:project_apps(relflow_state:rebar_state(State0))
               ],
        lists:foreach(fun({_AppName, AppFile}) ->
            %rebar_api:info("Setting app vsn in ~s to ~s", [AppName, NextAppVer]),
            ok = relflow_rewriter:set_appfile_version(AppFile, NextAppVer)
        end, Apps),
        rebar_api:info("Setting release vsn in rebar.config to ~s", [NewRelVsn]),
        case relflow_rewriter:set_rebar_relx_version("rebar.config", NewRelVsn) of
            {error, ErrReason} ->
                ?PRV_ERROR(ErrReason);
            _ ->
                rebar_api:info("You are advised to commit and tag this as '~s'",[NewRelVsn]),
                {ok, relflow_state:rebar_state(State)}
        end
    end,
    Changes = #{},
    exec(Changes, State0, fun(A,B) -> exec_1(A,B,BumperFun) end).

format_error(relflow_marker_missing) ->
    "You must have a '%% relflow-release-version-marker' line in rebar.config\n     " ++
    "See the README at github.com/RJ/relflow";
format_error(unclean_git) ->
    "Relflow modifies files in-place. Will not run with uncommitted changes.";
format_error(no_upfrom) ->
    "Missing git revision to upgrade from, eg: rebar3 relflow -u abc123\n     " ++
    "(or try: rebar3 help relflow)";
format_error({relvsn_ordering, Old, New}) ->
    io_lib:format("New release vsn is less than old! (new:~s < old:~s)", [New, Old]);
format_error(Reason) ->
    io_lib:format("Unhandled relflow error: ~p", [Reason]).


utctime() -> erlang:localtime_to_universaltime(erlang:localtime()).

new_rel_vsn({{Year,Month,Day},{Hour,Min,Sec}}) ->
    lists:flatten(
      io_lib:format("~4.10.0B~2.10.0B~2.10.0B.~2.10.0B~2.10.0B~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec])).

new_app_vsn({{Year,Month,Day},{Hour,Min,Sec}}) ->
    lists:flatten(
      io_lib:format("~4.10.0B~2.10.0B~2.10.0B-~2.10.0B~2.10.0B~2.10.0B-relflow",
        [Year, Month, Day, Hour, Min, Sec])).

exec(Map, State, Next) ->
    case relflow_state:force(State) orelse relflow_git:is_clean() of
        true -> Next(Map, State);
        false -> ?PRV_ERROR(unclean_git)
    end.

exec_1(Map, State, Next) ->
    NewRelVsn = relflow_state:nextver(State),
    OldRelVsn = relflow_state:oldrelver(State),
    case greater_than(NewRelVsn, OldRelVsn, relflow_state:semver(State)) of
        true  -> Next(Map, State, NewRelVsn);
        false -> ?PRV_ERROR({relvsn_ordering, OldRelVsn, NewRelVsn})
    end.

greater_than(NewVersion, OldVersion, true) ->
    [NewMajor, NewMinor, NewPatch] = lists:map(fun string:to_integer/1, string:tokens(NewVersion, ".")),
    [OldMajor, OldMinor, OldPatch] = lists:map(fun string:to_integer/1, string:tokens(OldVersion, ".")),
    if 
        NewMajor > OldMajor -> true;
        NewMajor < OldMajor -> false;
        NewMajor == OldMajor ->
            if 
                NewMinor > OldMinor -> true;
                NewMinor < OldMinor -> false;
                NewMinor == OldMinor -> NewPatch > OldPatch
            end
    end;
greater_than(NewVersion, OldVersion, _UsingSemanticVersion) -> NewVersion > OldVersion.


exec_2(Map, State, NewRelVsn) ->
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
    GitAddCmds = [ fmt("git add ~s", [AddFile]) || AddFile <- FilesTouched ],
    GitCmds = GitAddCmds ++ [
        %fmt("git add ~s", [string:join(FilesTouched, " ")]),
        fmt("git commit -m\"relflow ~s --> ~s\"", [relflow_state:oldrelver(State), NewRelVsn]),
        fmt("git tag -a \"v~s\" -m \"~s\"", [NewRelVsn, NewRelVsn])
    ],
    case {relflow_state:autogit(State), relflow_state:force(State)} of
        {true, false}  -> exec_git(GitCmds);
        {false, true}  -> exec_git(GitCmds);
        {false, false} -> print_git(GitCmds);
        {true, true}   ->
            rebar_api:warn("Not running git commands, because you --forced",[]),
            print_git(GitCmds)
    end,
    {ok, relflow_state:rebar_state(State)}.

exec_git(Cmds) ->
    lists:foreach(fun(Cmd) ->
        case os:cmd(Cmd) of
            ""  -> rebar_api:info("$ ~s", [Cmd]);
            Res -> rebar_api:info("$ ~s\n~s", [Cmd, string:strip(Res, right)])
        end
    end, Cmds).

print_git(Cmds) ->
    S = iolist_to_binary([ [C, "\n"] || C <- Cmds ]),
    rebar_api:info("Recommended git commands:\n~s", [S]).

fmt(S,A) -> lists:flatten(io_lib:format(S,A)).
