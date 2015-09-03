-module(ms_http_cowboy).
-include("ms_http.hrl").

-export([
    onrequest/1,
    onresponse/4
]).

%% public
-spec onrequest(cowboy_req:req()) -> cowboy_req:req().

onrequest(Req) ->
    cowboy_req:set_meta(timestamp, os:timestamp(), Req).

-spec onresponse(cowboy:http_status(), cowboy:http_headers(), iodata(),
    cowboy_req:req()) -> cowboy_req:req().

onresponse(Status, Headers, Body, Req) ->
    {Timestamp, Req2} = cowboy_req:meta(timestamp, Req),
    Diff = integer_to_binary(now_diff(Timestamp)),
    Headers2 = Headers ++ [{<<"x-response-time">>, Diff}],
    {ok, Req3} = cowboy_req:reply(Status, Headers2, Body, Req2),
    Req3.

%% private
now_diff(Timestamp) ->
     timer:now_diff(os:timestamp(), Timestamp).
