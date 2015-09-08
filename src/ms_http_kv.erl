-module(ms_http_kv).
-include("ms_http.hrl").

-export([
    get/1,
    put/2
]).

%% public
-ifndef(TEST).

-spec get(binary()) -> {ok, binary()} | not_found.

get(Key) ->
    ms_base:apply(ms_kv, ms_kv, get, [Key]).

-spec put(binary(), binary()) -> ok.

put(Key, Value) ->
    ms_base:apply(ms_kv, ms_kv, put, [Key, Value]).

-else.

-spec get(binary()) -> {ok, binary()} | not_found.

get(<<"foo2">>) ->
    {ok, <<"bar">>};
get(_) ->
    not_found.

-spec put(binary(), binary()) -> ok.

put(_Key, _Value) ->
    ok.

-endif.
