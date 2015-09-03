-module(ms_http_kv).
-include("ms_http.hrl").

-export([
    delete/1,
    get/1,
    put/2
]).

%% public
-spec delete(binary()) -> ok.

delete(_Key) ->
    ok.

-spec get(binary()) -> {ok, binary()}.

get(<<"test">>) ->
    {ok, <<"test">>};
get(_) ->
    not_found.

-spec put(binary(), binary()) -> ok.

put(_Key, _Value) ->
    ok.
