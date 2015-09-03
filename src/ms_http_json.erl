-module(ms_http_json).
-include("ms_http.hrl").

-export([
    req/1
]).

%% public
-spec req(#ms_req {}) -> binary().

req(Req) ->
    Prop = filter(record_info(fields, ms_req), tl(tuple_to_list(Req))),
    jiffy:encode({Prop}, [force_utf8]).

%% private
filter([], []) ->
    [];
filter([_ | T], [undefined | T2]) ->
    filter(T, T2);
filter([timestamp | T], [H2 | T2]) ->
    [{<<"timestamp">>, unix_time(H2)} | filter(T, T2)];
filter([H | T], [H2 | T2]) ->
    [{atom_to_binary(H, utf8), H2} | filter(T, T2)].

unix_time({Mega, Sec, Micro}) ->
    (Mega * 1000000 * 1000000 + Sec * 1000000) + Micro.
