-module(ms_http_app).
-include("ms_http.hrl").

%% public
-export([
    start/0,
    stop/0
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

%% public
-spec start() -> {ok, [atom()]}.

start() ->
    application:ensure_all_started(?APP).

-spec stop() -> ok | {error, {not_started, ?APP}}.

stop() ->
    application:stop(?APP).

%% application callbacks
-spec start(application:start_type(), term()) -> {ok, pid()}.

start(_StartType, _StartArgs) ->
    ms_http_cowboy:start(),
    ms_http_sup:start_link().

-spec stop(term()) -> ok.

stop(_State) ->
    ms_http_cowboy:stop(),
    ok.
