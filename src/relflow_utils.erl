-module(relflow_utils).
-include("relflow.hrl").
-export([
        absname/1,
        prompt_yn/2,
        otp_app_names/0
    ]).



%% via http://www.codecodex.com/wiki/Determine_if_two_file_paths_refer_to_the_same_file
%% hope it works properly..
absname(Path) ->
    Path2 = filename:split(Path),
    case string:chr(hd(Path2), $/) of
        0 -> {ok, Cwd} = file:get_cwd(),
             Abs = lists:reverse(filename:split(Cwd)),
             absname(Path2, Abs);
        _ -> absname(Path2, [])
    end.

absname([], Absname) ->
    filename:join(lists:reverse(Absname));
absname([H|T], Absname) ->
    case H of
        "."  -> absname(T, Absname);
        ".." -> absname(T, tl(Absname));
        _    -> absname(T, [H|Absname])
    end.

prompt_yn(Prompt, Default) when is_boolean(Default) ->
    YN = case Default of
        true -> "Yn";
        false -> "yN"
    end,
    Str = lists:flatten(io_lib:format("~s [~s] > ", [Prompt, YN])),
    case io:get_line(Str) of
        "\n"  -> Default;
        "y\n" -> true;
        "Y\n" -> true;
        "n\n" -> false;
        "N\n" -> false;
        _ -> false
    end.

otp_app_names() ->
  %% probably better to get this dynamically..
  [
    asn1, common_test, compiler, cosEvent, cosEventDomain, cosFileTransfer,
    cosNotification, cosProperty, cosTime, cosTransactions, crypto, debugger,
    dialyzer, diameter, edoc, eldap, erl_docgen, erl_interface, erts, et,
    eunit, gs, hipe, ic, inets, jinterface, kernel, megaco, mnesia, observer,
    odbc, orber, os_mon, ose, otp_mibs, parsetools, percept, public_key,
    reltool, runtime_tools, sasl, snmp, ssh, ssl, stdlib, syntax_tools,
    test_server, tools, typer, webtool, wx, xmerl
  ].


%stderr(S) -> stderr(S,[]).
%stderr(S,A) -> io:put_chars(standard_error, [io_lib:format(S,A),"\n"]).
