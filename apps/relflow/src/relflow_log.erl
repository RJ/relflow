-module(relflow_log).
-include("relflow.hrl").

-export([
         init/2,
         set_level/1,
         log/3
        ]).

-define(is_level(L), (L==info orelse L==debug orelse L==warn orelse L==error)).

init(Caller, Verbosity) when is_list(Verbosity) ->
    Level = atomize_level(Verbosity),
    LogState = ec_cmd_log:new(Level, Caller),
    application:set_env(relflow, log_state, LogState).

set_level(Level) when is_list(Level) ->
    set_level(atomize_level(Level));
set_level(Level) when ?is_level(Level) ->
    ok = application:set_env(relflow, log_level, Level).

log(Level, Str, Args) when ?is_level(Level) ->
    {ok, LogState} = application:get_env(relflow, log_state),
    ec_cmd_log:Level(LogState, ?FMT("[~s] ~s\n",[Level,Str]), Args).

atomize_level("error") -> error;
atomize_level("warn") -> warn;
atomize_level("info") -> info;
atomize_level("debug") -> debug.
