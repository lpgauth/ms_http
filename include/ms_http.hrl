-define(APP, ms_http).
-define(DEFAULT_HTTP_ACCEPTORS, 32).
-define(DEFAULT_HTTP_IP, {127, 0, 0, 1}).
-define(DEFAULT_HTTP_MAX_CONNECTIONS, 65536).
-define(DEFAULT_HTTP_MAX_KEEP_ALIVE, infinity).
-define(DEFAULT_HTTP_PORT, 8080).
-define(DEFAULT_HTTP_TIMEOUT, infinity).
-define(ENV(Key, Default), application:get_env(?APP, Key, Default)).

-record(ms_req, {
    timestamp     :: erlang:timestamp() | undefined,
    method        :: binary() | undefined,
    path          :: binary() | undefined,
    found = false :: boolean(),
    key           :: binary() | undefined,
    value         :: binary() | undefined
}).

-type ms_req() :: #ms_req {}.
