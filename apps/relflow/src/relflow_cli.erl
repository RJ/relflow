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
     {upfrom, $u, "upfrom", string,
      "The release version to upgrade from"},
     {relxfile, $r, "relxfile", {string, "./rebar.config"},
      "Path to relx.config, for adding new release section"},
     {loglevel, $l, "loglevel", {string, "info"},
      "Verbosity: debug, info, warn, error"},

     {help, $h, "help", undefined,
      "Print usage message"},
     {version, $v, "version", undefined,
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
opt2state([{loglevel, L} | Opts], State) ->
    NewState = State#state{loglevel=L},
    opt2state(Opts, NewState);
opt2state([{relxfile, Fn}|Opts], State) ->
    case first_file_that_exists([Fn, "./relx.config", "./rebar.config"]) of
        undefined ->
            error;
        F ->
            opt2state(Opts, State#state{relxfile=F})
    end;
opt2state([{relname, N} | Opts], State) ->
    opt2state(Opts, State#state{relname=N});
opt2state([{relpath, P} | Opts], State) ->
    opt2state(Opts, State#state{relpath=P});
opt2state([{upfrom, U} | Opts], State) ->
    opt2state(Opts, State#state{upfrom=U}).
%opt2state([{relvsn, V} | Opts], State) ->
    %opt2state(Opts, State#state{relvsn=V}).

first_file_that_exists([]) -> undefined;
first_file_that_exists([F|Rest]) ->
    case ec_file:exists(F) of
        true -> F;
        false -> first_file_that_exists(Rest)
    end.

%fix_state(S=#state{relxfile=undefined, relname=undefined}) ->
    %case first_file_that_exists(["./relx.config", "./rebar.config"]) of
        %undefined ->
            %?ERROR("Can't find a relx or rebar.config, so can't guess release name", []),
            %error;
        %F ->
            %fix_state(S#state{relxfile=F})
    %end;

fix_state(S=#state{relname=undefined,relxfile=RP}) when RP =/= undefined ->
    case guess_relname_from_relx(RP) of
        {ok, Relname} ->
            ?WARN("Assuming '--relname ~s' (extracted from ~s)",[Relname,RP]),
            fix_state(S#state{relname=atom_to_list(Relname)});
        {error, undefined} ->
            ?ERROR("Can't guess --relname, no {release, ...} section in ~s",[RP]),
            error;
        {error, {ambiguous,Names}} ->
            ?ERROR("Can't guess --relname, multiple release names found in ~s: ~w",[RP, Names]),
            error
    end;
fix_state(S=#state{relname=RN, relpath="_rel/$relname"}) when is_list(RN) ->
    %% TODO we could read relx.config instead of assuming _rel
    RP = "_rel/"++RN,
    case ec_file:exists(RP) of
       true ->
           ?WARN("Assuming '--relpath ~s' (relx default)", [RP]),
           fix_state(S#state{relpath=RP});
       false ->
           ?ERROR("Can't guess --relpath, tried: ~s", [RP]),
           error
   end;
fix_state(S=#state{upfrom=undefined,relpath=RP}) ->
    %% get upfrom from RELEASES file
    Path = RP ++ "/releases/RELEASES",
    case ec_file:exists(Path) of
        true ->
            Vsn = get_current_vsn_from_RELEASES(Path),
            true = is_list(Vsn),
            ?WARN("Assuming '--upfrom ~s' (extracted from ~s)",[Vsn,Path]),
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
    io:format("Example: relflow -u v1.0\n\n").

get_current_vsn_from_RELEASES(F) ->
    {ok, [L]} = file:consult(F),
    [{release,_RelName,Vsn,_ErtsVsn,_Apps,permanent}] = lists:filter(fun({_,_,_,_,_,permanent}) -> true; (_) -> false end, L),
    Vsn.

guess_relname_from_relx(RP) ->
    RelxOpts = case file:consult(RP) of
        {ok, L} ->
           case proplists:get_value(relx, L) of
               undefined ->
                    L;
               Else ->
                   Else
           end
    end,
    Rels = lists:filter(fun(T) ->
                is_tuple(T) andalso element(1,T) == release
            end, RelxOpts),
    case lists:usort(lists:map(fun(T) -> element(1,element(2,T)) end, Rels)) of
        [Relname] -> {ok, Relname};
        [] -> {error, undefined};
        Lst -> {error, {ambiguous, Lst}}
    end.
