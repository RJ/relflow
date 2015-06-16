%% rewrite app and app.src files to update vsn field
-module(relflow_rewriter).
-export([set_appfile_version/2, set_rebar_relx_version/2]).

-define(AppHeader, "%% Vsn auto-managed by relflow utility.\n%% DO NOT CHANGE VSN FIELD MANUALLY!").

set_appfile_version(Filepath, NewVsn) when is_list(Filepath) ->
    rebar_api:info("Rewriting vsn in ~s",[ filename:basename(Filepath)]),
    {ok, [{application, AppName, Sections}]} = file:consult(Filepath),
    Vsn = proplists:get_value(vsn, Sections),  %% don't need to know previous vsn
    NewSections = [{vsn, NewVsn} | proplists:delete(vsn, Sections)],
    Contents = io_lib:format("~s\n~p.~n",[?AppHeader, {application, AppName, NewSections}]),
    ok = file:write_file(Filepath, Contents),
    rebar_api:debug("Vsn changed from ~s --> ~s in: ~s",[Vsn, NewVsn, Filepath]),
    ok.

set_rebar_relx_version(Filepath, NewVsn) ->
    {ok, Bin} = file:read_file(Filepath),
    Lines = lists:map(fun binary_to_list/1, binary:split(Bin, <<"\n">>, [global])),
    Contents = set_rebar_relx_version_1(NewVsn, Lines, false, []),
    ok = file:write_file(Filepath, strip(Contents)).

set_rebar_relx_version_1(NewVsn, [Line | Lines], Found, Acc) ->
    case string:str(Line, "relflow-release-version-marker") > 0 of
        true ->
            NewLine = io_lib:format("    \"~s\" %% relflow-release-version-marker", [NewVsn]),
            set_rebar_relx_version_1(NewVsn, Lines, true, [NewLine|Acc]);
        false ->
            set_rebar_relx_version_1(NewVsn, Lines, Found, [Line|Acc])
    end;
set_rebar_relx_version_1(_, [], false, _Acc) ->
    rebar_api:error("You must have a '%% relflow-release-version-marker' line in rebar.config",[]),
    throw(reflow_marker_missing);
set_rebar_relx_version_1(_, [], true, Acc) ->
    [ [Line, <<"\n">>] || Line <- lists:reverse(Acc) ].

strip(IO) ->
    S = binary_to_list(iolist_to_binary([IO])),
    string:strip(S).

