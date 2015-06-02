-module(relflow_appup).
-include("relflow.hrl").
-export([generate_appups/2]).

generate_appups(Map, #state{profile=Profile}) when is_map(Map) ->
    NextVsn = next_vsn(),
    Ctx = #{next_vsn => NextVsn, profile => Profile},
    maps:map(fun(K,V) ->
          create_appup_term(generate_appup(K,V,Ctx))
    end, Map).

%%
next_vsn() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    lists:flatten(
      io_lib:format("~4.10.0B~2.10.0B~2.10.0B-~2.10.0B~2.10.0B~2.10.0B-relflow",
        [Year, Month, Day, Hour, Min, Sec])).

create_appup_term(#{vsn := FromVer,
                    next_vsn := ToVer,
                    appup_instructions := Instructions} = M) ->
    T = {ToVer,
         [{FromVer, Instructions}],
         [{FromVer, []}]
        },
    maps:put(appup_term, T, M).

generate_appup(AppName, AppMap = #{changes := Changes, vsn := Vsn}, Ctx0 = #{next_vsn := NextVsn}) ->
    Ctx = Ctx0#{appname => AppName, vsn => Vsn},
    Instructions = maps:fold(
        fun(Mod, ModMap, InstrAcc) ->
            [module_instructions(Mod, ModMap, Ctx) | InstrAcc]
        end,
        [],
        Changes
    ),
    SortedInstructions = sort_instructions(
                           lists:flatten(
                             lists:reverse(Instructions))),
    AppMap2 = AppMap#{ vsn => Vsn, next_vsn => NextVsn},
    maps:put(appup_instructions, SortedInstructions, AppMap2).


module_instructions(Mod, #{status := added}, _Ctx) ->
    [{add_module, Mod}];

module_instructions(Mod, #{status := deleted}, _Ctx) ->
    [{delete_module, Mod}];

module_instructions(Mod, #{status := modified}, #{vsn := FromVer, next_vsn := ToVer, appname := AppName, profile := Profile}) ->
    BeamInfo = beam_info(AppName, Mod, Profile),
    case is_supervisor(BeamInfo) of
        true ->
            case has_sup_upgrade_notify(BeamInfo) of
                true ->
                    [{update, Mod, supervisor},
                     {apply, {Mod, sup_upgrade_notify, [FromVer, ToVer]}}
                    ];
                false ->
                    [{update, Mod, supervisor}]
            end;
        false ->
            case has_code_change(BeamInfo) of
                false ->
                    [{load_module, Mod}];
                true ->
                    [{update, Mod, {advanced, {FromVer, ToVer, []}}}]
            end
    end
    ++
    %% does the module have an upgrade hook callback exported
    case has_upgrade_hook(BeamInfo) of
        true ->
            [{apply, {Mod, appup_upgrade_hook, [FromVer, ToVer]}}];
        false ->
            []
    end
    .

is_supervisor(#{behaviours := Behavs}) ->
    lists:member(supervisor, Behavs).

has_sup_upgrade_notify(#{exports := Exports}) ->
    lists:member({sup_upgrade_notify, 2}, Exports).

has_code_change(#{exports := Exports}) ->
    lists:member({code_change, 3}, Exports) orelse
    lists:member({code_change, 4}, Exports).

has_upgrade_hook(#{exports := Exports}) ->
    lists:member({appup_upgrade_hook, 2}, Exports).


beam_info(AppName, Mod, Profile) ->
    BeamInfo = read_beam(beam_path(AppName, Mod, Profile)),
    BeamInfo#{name => Mod, appname => AppName}.

beam_path(AppName, Mod, RebarProfile) when is_atom(Mod) ->
    lists:flatten(
      io_lib:format(
        "_build/~s/lib/~s/ebin/~s.beam",
        [RebarProfile, AppName, Mod])).


read_beam(Path) ->
    case file:read_file(Path) of
        {ok, Beam} ->
            extract_module_info(Beam);
        _ ->
            io:format("Could not read ~s\n", [Path]),
            throw(fail_beam_read)
    end.

extract_module_info(Beam) when is_binary(Beam) ->
    Chunker = fun(K) ->
        case beam_lib:chunks(Beam, [K]) of
            {ok, {_, [{K, Result}]}} -> Result;
            _ -> []
        end
    end,
    Exports = Chunker(exports),
    %% Tidy up the americanised spelling of behaviour
    Behaviours = lists:usort(
                    lists:flatten(
                        proplists:get_value(behaviour, Chunker(attributes), []) ++
                        proplists:get_value(behavior,  Chunker(attributes), []))),
    #{
        behaviours => Behaviours,
        exports => Exports
    }.


sort_instructions(L) ->
    lists:sort(fun sort_instr_cmp/2, L).

%% we always want to load/add/update a module before calling apply MFA on it:
sort_instr_cmp({load_module, M},   {apply, {M, _F, _A}}) ->
    true;
sort_instr_cmp({add_module, M},    {apply, {M, _F, _A}}) ->
    true;
sort_instr_cmp({update, M, _},     {apply, {M, _F, _A}}) ->
    true;
%% and we always do the apply MFA before we delete the module
sort_instr_cmp({delete_module, M}, {apply, {M, _F, _A}}) ->
    false;
%%
%% sorting precedence for various types of appup instruction
sort_instr_cmp({add_module,_}=A,{add_module,_}=B) ->
    compare(A,B);
%% add_module to the top
sort_instr_cmp({add_module,_},_) ->
    true;
sort_instr_cmp({delete_module,_}=A,{delete_module,_}=B) ->
    compare(A,B);
%% delete_module to the bottom
sort_instr_cmp({delete_module,_},_) ->
    false;
sort_instr_cmp({update,_,supervisor}=A,{update,_,supervisor}=B) ->
    compare(A,B);
%% supervisor updates before other module types
sort_instr_cmp({update,_,supervisor},_) ->
    true;
%% otherwise compare module name using cmp_name/2
sort_instr_cmp(A,B) ->
    compare(A,B).

compare(A,B) ->
    cmp(unpack(A),unpack(B)).

cmp(A,B) when is_atom(A), is_atom(B) ->
    cmp(atom_to_list(A), atom_to_list(B));
cmp(A,B) when is_list(A), is_list(B) ->
    ASup = lists:suffix("_sup", A),
    BSup = lists:suffix("_sup", B),
    case {ASup, BSup} of
        {true, false} -> true;
        {false, true} -> false;
        _             -> A =< B
    end;
cmp(A,B) ->
    A =< B.

unpack({add_module,M}) -> M;
unpack({delete_module,M}) -> M;
unpack({load,M}) -> M;
unpack({update,M,_}) -> M;
unpack({apply,{M,_,_}}) -> M;
unpack(M) -> M.
