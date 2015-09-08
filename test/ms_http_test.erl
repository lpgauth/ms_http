-module(ms_http_test).
-include("../include/ms_http.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.

%% runners
-spec ms_http_test_() -> ok.

ms_http_test_() ->
    {setup,
        fun () -> setup() end,
        fun (_) -> cleanup() end,
    [
        fun json_req/0,
        fun kv_rest_get/0,
        fun kv_rest_put/0,
        fun logger_rest_put/0
    ]}.

%% tests
json_req() ->
    Expected = list_to_binary("{" ++
        "\"timestamp\":1441451289487493," ++
        "\"method\":\"PUT\"," ++
        "\"path\":\"/api/v1/kv/foo\"," ++
        "\"found\":true," ++
        "\"key\":\"foo\"," ++
        "\"value\":\"bar\"" ++
    "}"),

    ?assertEqual(Expected, ms_http_json:req(#ms_req {
        timestamp = {1441, 451289, 487493},
        method = <<"PUT">>,
        path = <<"/api/v1/kv/foo">>,
        found = true,
        key = <<"foo">>,
        value = <<"bar">>
    })),

    Expected2 = list_to_binary("{" ++
        "\"timestamp\":1441451289487493," ++
        "\"method\":\"PUT\"," ++
        "\"path\":\"/api/v1/kv/foo\"," ++
        "\"found\":false," ++
        "\"key\":\"foo\"" ++
    "}"),

    ?assertEqual(Expected2, ms_http_json:req(#ms_req {
        timestamp = {1441, 451289, 487493},
        method = <<"PUT">>,
        path = <<"/api/v1/kv/foo">>,
        found = false,
        key = <<"foo">>
    })).

kv_rest_get() ->
    Url = url("/api/v1/kv/foo"),
    Response = perform_request(get, Url),

    assert_status(404, Response),
    assert_header("x-response-time", Response),
    assert_header_value("0", "content-length", Response),
    assert_header_value("text/plain", "content-type", Response),

    Url2 = url("/api/v1/kv/foo2"),
    Response2 = perform_request(get, Url2),

    assert_status(200, Response2),
    assert_header("x-response-time", Response2),
    assert_header_value("3", "content-length", Response2),
    assert_header_value("text/plain", "content-type", Response2).

kv_rest_put() ->
    Url = url("/api/v1/kv/foo"),
    Headers = [{"content-type", "text/plain"}],
    Body = "bar",

    Response = perform_request(put, Url, Headers, Body),

    assert_status(201, Response),
    assert_header("x-response-time", Response),
    assert_header_value("0", "content-length", Response),
    assert_header_value("text/plain", "content-type", Response),

    Url2 = url("/api/v1/kv/foo2"),
    Response2 = perform_request(put, Url2, Headers, Body),
    assert_status(400, Response2),

    Url3 = url("/api/v1/kv"),
    Response3 = perform_request(put, Url3, Headers, Body),
    assert_status(400, Response3).

logger_rest_put() ->
    Url = url("/api/v1/logger/2015-09-06-05"),
    Headers = [{"content-type", "text/plain"}],
    Body = "bar",

    Response = perform_request(put, Url, Headers, Body),

    assert_status(204, Response),
    assert_header("x-response-time", Response),
    assert_header_value("0", "content-length", Response),
    assert_header_value("text/plain", "content-type", Response),

    Url2 = url("/api/v1/logger"),
    Response2 = perform_request(put, Url2, Headers, Body),
    assert_status(400, Response2).

%% utils
assert_header_value(Expected, Header, {ok, _, Headers, _}) ->
    ?assertEqual(Expected, lookup(Header, Headers, undefined)).

assert_header(Header, {ok, _, Headers, _}) ->
    ?assertNotEqual(undefined, lookup(Header, Headers, undefined)).

assert_status(Expected, {ok, Status, _, _}) ->
    ?assertEqual(Expected, Status).

cleanup() ->
    ms_http_app:stop(),
    inets:stop().

content_type(Headers) ->
    lookup("content-type", Headers,
        "application/x-www-form-urlencoded").

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

perform_request(Method, Url) ->
    perform_request(Method, Url, [], undefined).

perform_request(Method, Url, Headers, Body) ->
    Request = case Method of
        get -> {Url, Headers};
        delete -> {Url, Headers};
        _   -> {Url, Headers, content_type(Headers), Body}
    end,
    case httpc:request(Method, Request, [], []) of
        {ok, Response} ->
            {{_, StatusCode, _}, ResHeaders, ResBody} = Response,
            {ok, StatusCode, ResHeaders, ResBody};
        {error, Reason} ->
            {error, Reason}
    end.

setup() ->
    error_logger:tty(false),
    application:load(?APP),
    application:load(lager),
    application:set_env(lager, error_logger_redirect, false),
    inets:start(),
    ms_http_app:start().

url(Path) ->
    Port = ?ENV(http_port, ?DEFAULT_HTTP_PORT),
    "http://127.0.0.1:" ++ integer_to_list(Port) ++ Path.
