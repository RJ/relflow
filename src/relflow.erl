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
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "release workflow util"},
            {desc, "release workflow util"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    BaseDir = rebar_dir:base_dir(State),  %% _build/$profile
    RelName = "ircshard", %% TODO - get from config/state
    RelDir  = filename:join([BaseDir, "rel", RelName]),
    rebar_api:info("relflow plugin says hi.\n~s", [RelDir]),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
