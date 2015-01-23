%%
%% Compare two .beam files, detect nature of the changes between versions
%%
-module(relflow_beamcmp).
-export([diff/2]).
-include("relflow.hrl").

%% detect nature of change to a beam file
%% returns:
%%  load_module
%%  code_change
%%  {supervisor, [...]}
diff(PathA, PathB) when is_list(PathA), is_list(PathB) ->
    case filename:basename(PathA) == filename:basename(PathB) of
        true ->
            do_diff(PathA, PathB);
        false ->
            throw("diffing beams with different names, wtf")
    end.

%%% ----

do_diff(PathA, PathB) when PathA == PathB ->
    false;
do_diff(PathA, PathB) when is_list(PathA), is_list(PathB) ->
    {ok, BeamA} = file:read_file(PathA),
    {ok, BeamB} = file:read_file(PathB),
    case BeamA == BeamB of
        true  -> false; %% same file contents
        false -> beam_diff_type(BeamA, BeamB)
    end.

beam_diff_type(BeamA, BeamB) when is_binary(BeamA), is_binary(BeamB) ->
    Minfo = extract_module_info(BeamB),
    HasBehaviour = fun(B) ->
        lists:member(B, proplists:get_value(behaviours, Minfo, []))
    end,
    HasCodeChange = lists:member({code_change, 3}, Minfo) orelse
                        lists:member({code_change, 4}, Minfo),
    case HasBehaviour(supervisor) of
        true ->
            {supervisor, Minfo};
        false ->
            case HasCodeChange of
                true ->
                    case did_state_record_change(BeamA, BeamB) of
                        true  -> code_change;
                        false -> load_module
                    end;
                false ->
                    load_module
            end
    end.

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

