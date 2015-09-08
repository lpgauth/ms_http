-module(ms_http_cowboy).
-include("ms_http.hrl").

-export([
    onrequest/1,
    onresponse/4,
    start/0,
    stop/0
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

-spec start() -> ok.

start() ->
    HttpAcceptors = ?ENV(http_acceptors, ?DEFAULT_HTTP_ACCEPTORS),
    HttpIp = ?ENV(http_ip, ?DEFAULT_HTTP_IP),
    HttpMaxConnections = ?ENV(http_max_connections,
        ?DEFAULT_HTTP_MAX_CONNECTIONS),
    HttpMaxKeepAlive = ?ENV(http_max_keepalive,
        ?DEFAULT_HTTP_MAX_KEEP_ALIVE),
    HttpPort = ?ENV(http_port, ?DEFAULT_HTTP_PORT),
    HttpTimeout = ?ENV(http_timeout, ?DEFAULT_HTTP_TIMEOUT),

    Dispatch = cowboy_router:compile([{'_', [
        {"/api/[:version]/kv/[:key]", ms_http_kv_rest, []},
        {"/api/[:version]/logger/[:key]", ms_http_logger_rest, []}
    ]}]),

    try cowboy:start_http(?APP, HttpAcceptors, [
        {ip, HttpIp},
        {max_connections, HttpMaxConnections},
        {port, HttpPort}
    ], [
        {env, [{dispatch, Dispatch}]},
        {onrequest, fun ms_http_cowboy:onrequest/1},
        {onresponse, fun ms_http_cowboy:onresponse/4},
        {max_keepalive, HttpMaxKeepAlive},
        {timeout, HttpTimeout}
    ]) of
        {ok, _Pid} ->
            ok;
        {error, Reason} ->
            lager:warning("ms_http_cowboy start_http error: ~p~n", [Reason]),
            ok
    catch
        _:_ -> ok
    end.

-spec stop() -> ok.

stop() ->
     cowboy:stop_listener(?APP).

%% private
now_diff(Timestamp) ->
     timer:now_diff(os:timestamp(), Timestamp).
