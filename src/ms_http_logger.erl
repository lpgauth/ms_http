-module(ms_http_logger).
-include("ms_http.hrl").

-export([
    log/2
]).

%% public
-ifndef(TEST).

-spec log(erlang:timestamp() | binary(), binary()) -> ok.

log(Name, Bin) ->
    ms_base:apply(ms_logger, ms_logger, log, [Name, Bin]).

-else.

-spec log(binary(), binary()) -> ok.

log(_Name, _Bin) ->
    ok.

-endif.
