-module(relflow_cli).
-include("relflow.hrl").
-export([parse_args/1]).

stderr(S) -> stderr(S,[]).
stderr(S,A) -> io:put_chars(standard_error, [io_lib:format(S,A),"\n"]).

parse_args(Args) ->
    case getopt:parse(opt_specs(), Args) of
        {ok, {Opts, NonOpts}} ->
            case lists:member(version, Opts) of
                true  -> show_version();
                false ->
                    case lists:member(help, Opts) of
                        true  -> show_usage();
                        false ->
                            case opts2state(Opts, NonOpts) of
                                S = #state{} -> {ok, S};
                                _ -> error
                            end
                    end
            end;
        {error, Err} ->
            stderr("Opts parsing error ~p",[Err]),
            error
    end.

opt_specs() ->
    [{relname, $n, "relname", string,
      "The release name you gave relx"},
     {relpath, $p, "relpath", {string, "_rel/$relname"},
      "The path to the releases dir (contains ./releases/ dir)"},
     {upfrom, $u, "upfrom", string,
      "The release version to upgrade from"},
     %{relvsn, $v, "relvsn", string,
      %"The new release version"},
     {listrels, $l, "listrels", undefined,
      "Print list of release versions relflow can find"},
     {appups, $a, "appups", undefined,
      "Do appup stuff.."},
     {help, $h, "help", undefined,
      "Print usage message"},
     {version, undefined, "version", undefined,
      "Print relflow version"}
    ].

opts2state(Opts, _NonOpts) ->
    S = check_state(
         fix_state(
          opt2state(Opts, #state{}))),
    case S of
        #state{} -> S;
        _ -> error
    end.

opt2state([], State) ->
    State#state{directives=lists:reverse(State#state.directives)};
opt2state([listrels | Opts], State) ->
    opt2state(Opts, State#state{directives = [listrels|State#state.directives]});
opt2state([appups | Opts], State) ->
    opt2state(Opts, State#state{directives = [appups|State#state.directives]});
opt2state([{relname, N} | Opts], State) ->
    opt2state(Opts, State#state{relname=N});
opt2state([{relpath, P} | Opts], State) ->
    opt2state(Opts, State#state{relpath=P});
opt2state([{upfrom, U} | Opts], State) ->
    opt2state(Opts, State#state{upfrom=U}).
%opt2state([{relvsn, V} | Opts], State) ->
    %opt2state(Opts, State#state{relvsn=V}).

%% TODO read defaults from relx.config?
fix_state(S=#state{relname=RN, relpath="_rel/$relname"}) when is_list(RN) ->
    fix_state(S#state{relpath="_rel/"++RN});
fix_state(S=#state{}) ->
    S.

usage_err(S) ->
    stderr(S).

%check_state(#state{relvsn=undefined}) ->
    %usage_err("Missing relvsn");
%check_state(#state{upfrom=undefined}) ->
    %usage_err("Missing relupfrom");
%check_state(#state{relname=undefined}) ->
    %usage_err("Missing relname");
check_state(S=#state{}) ->
    S.


show_version() ->
    application:load(relflow),
    {ok, Vsn} = application:get_key(relflow, vsn),
    io:format("~s\n",[Vsn]).

show_usage() ->
    getopt:usage(opt_specs(), "relflow", "<options..>"),
    io:format("Example: relflow -n myrelease -p _rel/myrelease -u 1.0 -v 2.0\n\n").

