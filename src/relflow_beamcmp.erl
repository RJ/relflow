%%
%% Compare two .beam files, detect nature of the changes between versions
%%
-module(relflow_beamcmp).
-export([appup_from_beam_paths/4]).
-include("relflow.hrl").

%% Mods: [ {ModNameAtom, PathToBeamString}, ... ]
appup_from_beam_paths(ModsA, ModsB, OV, NV) ->
    %?DEBUG("appup_from_beam_paths ~p ~p ~s ~s",[ModsA,ModsB,OV,NV]),
    NamesA = [N || {N,_} <- ModsA],
    NamesB = [N || {N,_} <- ModsB],
    AddedModNames = NamesB -- NamesA,
    DeletedModNames = NamesA -- NamesB,
    SameMods = lists:filter(fun({MName,MPath}) -> not lists:member(MName,AddedModNames) end, ModsB),
    ?DEBUG("Added: ~p",[AddedModNames]),
    ?DEBUG("Deleted: ~p",[DeletedModNames]),
    ?DEBUG("Same: ~p",[SameMods]),
    Pairs =
        [ {[{add_module,M}],[{delete_module,M}]} || M <- AddedModNames ] ++
        [ {[{delete_module,M}],[{add_module,M}]} || M <- DeletedModNames ] ++
        lists:map(
            fun({M,PathB}) ->
                PathA = proplists:get_value(M, ModsA),
                appup_instructions_between_beams(PathA, PathB, OV, NV, M)
            end,
            SameMods),
    {Ups, Downs} = flatten_updown_pairs(Pairs),
    case {Ups, Downs} of
        {[], []} ->
            undefined;
        _ ->
            {NV,
             [{OV, sort_ais(Ups)}],
             [{OV, sort_ais(Downs)}]
            }
    end.

appup_instructions_between_beams(PathA, PathB, OV, NV, M) when is_list(PathA), is_list(PathB), is_atom(M) ->
    {ok, BeamA} = file:read_file(PathA),
    {ok, BeamB} = file:read_file(PathB),
    case BeamA == BeamB of
        true  ->
            undefined;
        false ->
            ?DEBUG("appup_instructions_between_beams ~s ~s ~s ~s ~s",[PathA,PathB,OV,NV,M]),
            appup_instructions_between_beams(BeamA, BeamB, OV, NV, M)
    end;

%% list of appup instructions between modules
appup_instructions_between_beams(BeamA, BeamB, OV, NV, M) when is_binary(BeamA), is_binary(BeamB), is_atom(M) ->
    Minfo = extract_module_info(BeamB),
    Exports = proplists:get_value(exports, Minfo, []),
    IsSup = lists:member(supervisor, proplists:get_value(behaviours, Minfo, [])),
    HasCC = lists:member({code_change, 3}, Exports) orelse
                            lists:member({code_change, 4}, Exports),
    HasUK = lists:member({appup_apply_upgrade_hook,3},Exports),
    HasDK = lists:member({appup_apply_downgrade_hook,3},Exports),
    Props = [],
    F = flatten_updown_pairs([
        case IsSup of
            true  ->
                HasUN = lists:member({sup_upgrade_notify,2}, Exports),
                UpdateM = {update, M, supervisor},
                case HasUN of
                    false ->
                        {[UpdateM],[UpdateM]};
                    true ->
                        ApplyUp = {apply, {M, sup_upgrade_notify, [OV,NV]}},
                        ApplyDown = {apply, {M, sup_upgrade_notify, [NV,OV]}},
                        {[UpdateM, ApplyUp], [UpdateM, ApplyDown]}
                end;
            false ->
                case HasCC andalso did_state_record_change(BeamA, BeamB) of
                    true  ->
                        Up = [{update, M, {advanced, {OV,NV,Props}}}],
                        Dn = [{update, M, {advanced, {NV,OV,Props}}}],
                        {Up, Dn};
                    false ->
                        Up = [{load_module, M}],
                        Dn = [{load_module, M}],
                        {Up, Dn}
                end
        end] ++ lists:flatten([
        case HasUK of
            true ->
                {[{apply, {M, appup_apply_upgrade_hook,   [OV,NV,Props]}}],[]};
            false ->
                []
        end,
        case HasDK of
           true ->
               {[], [{apply, {M, appup_apply_downgrade_hook, [NV,OV,Props]}}]};
           false ->
               []
        end
    ])),
    case F of
        {[],[]} -> undefined;
        _ -> F
    end.

flatten_updown_pairs(Pairs) ->
    flatten_updown_pairs(Pairs, [], []).

flatten_updown_pairs([undefined|Rest], UpAcc, DownAcc) ->
    flatten_updown_pairs(Rest, UpAcc, DownAcc);
flatten_updown_pairs([], UpAcc, DownAcc) ->
    {UpAcc, DownAcc};
flatten_updown_pairs([{Ups,Dns}|Rest], UpAcc, DownAcc) ->
    flatten_updown_pairs(Rest, UpAcc ++ Ups, DownAcc ++ Dns).

%% would be better to use the abstract code to detect the record name
%% returned from init/1, ie {ok, #state{}}, and check if that has changed.
%% but 99.9% of the time it's called 'state', so this will do for now.
did_state_record_change(BeamA, BeamB) ->
    proplists:get_value(state, read_beam_records(BeamA)) /=
        proplists:get_value(state, read_beam_records(BeamB)).

read_beam_records(Beam) ->
    AbstChunks = beam_lib:chunks(Beam,[abstract_code]),
    {ok, {_ModName, [{abstract_code, {raw_abstract_v1, AC}}]}} = AbstChunks,
    Recs = lists:foldl(fun
        ({attribute,_Line,record,RecTuple={_RecName,_RecInfo}},Acc) ->
            [RecTuple|Acc];
        (_, Acc) ->
            Acc
    end, [], AC),
    lists:sort(Recs).

extract_module_info(Beam) ->
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
    [
        {behaviours, Behaviours},
        {exports, Exports}
    ].


sort_ais(L) ->
    lists:sort(fun sort_ais_cmp/2, L).

%% we always want to load/add/update a module before calling apply MFA on it:
sort_ais_cmp({load_module, M},   {apply, {M, _F, _A}}) ->
    true;
sort_ais_cmp({add_module, M},    {apply, {M, _F, _A}}) ->
    true;
sort_ais_cmp({update, M, _},     {apply, {M, _F, _A}}) ->
    true;
%% and we always do the apply MFA before we delete the module
sort_ais_cmp({delete_module, M}, {apply, {M, _F, _A}}) ->
    false;
%%
%% sorting precedence for various types of appup instruction
sort_ais_cmp({add_module,_}=A,{add_module,_}=B) ->
    compare(A,B);
%% add_module to the top
sort_ais_cmp({add_module,_},_) ->
    true;
sort_ais_cmp({delete_module,_}=A,{delete_module,_}=B) ->
    compare(A,B);
%% delete_module to the bottom
sort_ais_cmp({delete_module,_},_) ->
    false;
sort_ais_cmp({update,_,supervisor}=A,{update,_,supervisor}=B) ->
    compare(A,B);
%% supervisor updates before other module types
sort_ais_cmp({update,_,supervisor},_) ->
    true;
%% otherwise compare module name using cmp_name/2
sort_ais_cmp(A,B) ->
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

