%% small wrapper around rebar_state to get our conf values,
%% plus a couple of extra config values we store ourselves to be passed
%% to our various helper modules
-module(relflow_state).

-record(relflow_st, {
            rebar_state,
            nextappver,
            oldrelver,
            nextver,
            sccm
         }).

-export([
    new/1,
    task/1,
    version/1,
    relname/1,
    build_dir/1,
    upfrom/1,
    profile/1,
    force/1,
    autocommit/1,
    rebar_state/1,
    nextver/1,
    set_default_nextver/2,
    oldrelver/1,
    oldrelver/2,
    nextappver/1,
    nextappver/2,
    sccm/1
]).

new(RebarSt) -> #relflow_st{rebar_state = RebarSt}.

task(State) -> parg(task, State).

version(State) -> parg(version, State).

rebar_state(#relflow_st{rebar_state = R}) -> R.

relname(_State) -> "ircshard".

build_dir(#relflow_st{rebar_state = State}) -> rebar_dir:base_dir(State).

upfrom(State) -> parg(upfrom, State).

autocommit(State) -> parg(autocommit, State) == true.

profile(State) -> lists:last(filename:split(build_dir(State))).

nextappver(#relflow_st{nextappver=V}) -> V.
nextappver(NV, State = #relflow_st{}) -> State#relflow_st{nextappver=NV}.

oldrelver(#relflow_st{oldrelver=V}) -> V.
oldrelver(NV, State = #relflow_st{}) -> State#relflow_st{oldrelver=NV}.

nextver(#relflow_st{nextver=V}) -> V.

sccm(State) -> list_to_atom(parg(sccm, State)).

%% only set if not provided on cli options
set_default_nextver(NV, State = #relflow_st{}) ->
   case parg(nextver, State) of
       "auto" -> State#relflow_st{nextver=NV};
       V      -> State#relflow_st{nextver=V}
   end.

force(State) -> parg(force, State) == true.

%%

parg(K, #relflow_st{rebar_state = State}) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    proplists:get_value(K, Opts).
