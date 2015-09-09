-module(ms_http_logger_rest).
-include("ms_http.hrl").

%% public
-export([
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    init/3,
    put/2,
    resource_exists/2,
    rest_init/2,
    rest_terminate/2
]).

-define(HTTP_METHODS, [<<"PUT">>]).

-record(state, {
    timestamp :: erlang:timestamp(),
    logger    :: binary() | undefined
}).

-type state() :: #state {}.

%% public
-spec allowed_methods(cowboy_req:req(), state()) ->
    {[binary()], cowboy_req:req(), state()}.

allowed_methods(Req, State) ->
    {?HTTP_METHODS, Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.

content_types_accepted(Req, State) ->
    {[{<<"text/plain">>, put}], Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.

content_types_provided(Req, State) ->
    {[{<<"text/plain">>, '_'}], Req, State}.

-spec init({tcp, http}, cowboy_req:req(), []) ->
    {upgrade, protocol, cowboy_rest}.

init(_Transport, _Req, _Opts) ->
    ms_base_metric:increment(<<"ms_http_logger.request">>, 1, ?SAMPLE_RATE),
    {upgrade, protocol, cowboy_rest}.

-spec put(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.

put(Req, #state {logger = undefined} = State) ->
    {false, Req, State};
put(Req, #state {logger = Logger} = State) ->
    case cowboy_req:body(Req) of
        {ok, Bin, Req2} ->
            ms_http_logger:log(Logger, Bin),
            {true, Req2, State};
        {error, _Reason} ->
            {false, Req, State}
    end.

-spec resource_exists(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.

resource_exists(Req, State) ->
    case cowboy_req:binding(key, Req) of
        {undefined, Req2} ->
            {false, Req2, State};
        {Logger, Req2} ->
             {true, Req2, State#state {
                 logger = Logger
             }}
    end.

-spec rest_init(cowboy_req:req(), []) ->
    {ok, cowboy_req:req(), state()}.

rest_init(Req, []) ->
    {ok, Req, #state {
        timestamp = os:timestamp()
    }}.

-spec rest_terminate(cowboy_req:req(), state()) -> ok.

rest_terminate(_Req, #state {timestamp = Timestamp}) ->
    Diff = timer:now_diff(os:timestamp(), Timestamp),
    ms_base_metric:timing(<<"ms_http_kv.request">>, Diff, ?SAMPLE_RATE).
