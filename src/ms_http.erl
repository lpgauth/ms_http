-module(ms_http).
-include("ms_http.hrl").

-export([
    start/0,
    stop/0
]).

%% public
-spec start() -> ok.

start() ->
    HttpAcceptors = ?ENV(http_acceptors, ?DEFAULT_HTTP_ACCEPTORS),
    HttpIp = ?ENV(http_ip, ?DEFAULT_HTTP_IP),
    HttpMaxConnections = ?ENV(http_max_connections,
        ?DEFAULT_HTTP_MAX_CONNECTIONS),
    HttpMaxKeepAlive = ?ENV(http_max_connections,
        ?DEFAULT_HTTP_MAX_KEEP_ALIVE),
    HttpPort = ?ENV(http_port, ?DEFAULT_HTTP_PORT),
    HttpTimeout = ?ENV(http_timeout, ?DEFAULT_HTTP_TIMEOUT),

    Dispatch = cowboy_router:compile([{'_', [
        {"/api/[:version]/kv/[:key]", ms_http_kv_rest, []}
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
            lager:warning("ms_http_app start_http error: ~p~n", [Reason]),
            ok
    catch
        _:_ -> ok
    end.

-spec stop() -> ok.

stop() ->
     cowboy:stop_listener(?APP).
