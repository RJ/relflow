-module(relflow_cli).
-include("relflow.hrl").
-export([parse_args/2]).

stderr(S) -> stderr(S,[]).
stderr(S,A) -> io:put_chars(standard_error, [io_lib:format(S,A),"\n"]).

parse_args(Args, State0) ->
    case getopt:parse(opt_specs(), Args) of
        {ok, {Opts, NonOpts}} ->
            case lists:member(version, Opts) of
                true  -> show_version();
                false ->
                    case lists:member(help, Opts) of
                        true  -> show_usage();
                        false ->
                            case opts2state(Opts, NonOpts, State0) of
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
     {relxfile, $r, "relxfile", {string, "./relx.config"},
      "Path to relx.config, for adding new release section"},
     {loglevel, $l, "loglevel", {string, "info"},
      "Verbosity: debug, info, warn, error"},

     {help, $h, "help", undefined,
      "Print usage message"},
     {version, undefined, "version", undefined,
      "Print relflow version"}
    ].

opts2state(Opts, _NonOpts, InitialState = #state{}) ->
    case opt2state(Opts, InitialState) of
        error ->
            error;
        S = #state{} ->
            case fix_state(S) of
                error ->
                    error;
                S2 = #state{} ->
                    check_state(S2)
            end
    end.

opt2state([], State) ->
    State;
opt2state([{relxfile, F} | Opts], State) ->
    case ec_file:exists(F) of
        true ->
            opt2state(Opts, State#state{relxfile=F});
        false ->
            ?ERROR("Could not read relx file at: ~s", [F]),
            error
    end;
opt2state([{loglevel, L} | Opts], State) ->
    NewState = State#state{loglevel=L},
    opt2state(Opts, NewState);
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
fix_state(S=#state{upfrom=undefined,relpath=RP}) ->
    %% get upfrom from RELEASES file
    Path = RP ++ "/releases/RELEASES",
    case ec_file:exists(Path) of
        true ->
            Vsn = get_current_vsn_from_RELEASES(Path),
            true = is_list(Vsn),
            ?WARN("No previous release version specified (with --upfrom)",[]),
            ?WARN("Assuming --upfrom ~s (extracted from ~s)",[Vsn,Path]),
            fix_state(S#state{upfrom=Vsn});
        false ->
            ?ERROR("Previous release version missing, specify with --upfrom",[]),
            error
    end;

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

get_current_vsn_from_RELEASES(F) ->
    {ok, [L]} = file:consult(F),
    [{release,_RelName,Vsn,_ErtsVsn,_Apps,permanent}] = lists:filter(fun({_,_,_,_,_,permanent}) -> true; (_) -> false end, L),
    Vsn.
