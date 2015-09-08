-module(ms_http_logger_rest).
-include("ms_http.hrl").

%% public
-export([
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    init/3,
    put/2,
    resource_exists/2
]).

-define(HTTP_METHODS, [
    <<"PUT">>
]).

%% public
-spec allowed_methods(cowboy_req:req(), term()) ->
    {[binary()], cowboy_req:req(), term()}.

allowed_methods(Req, State) ->
    {?HTTP_METHODS, Req, State}.

-spec content_types_accepted(cowboy_req:req(), term()) ->
    {[{binary(), atom()}], cowboy_req:req(), term()}.

content_types_accepted(Req, State) ->
    {[{<<"text/plain">>, put}], Req, State}.

-spec content_types_provided(cowboy_req:req(), term()) ->
    {[{binary(), atom()}], cowboy_req:req(), term()}.

content_types_provided(Req, State) ->
    {[{<<"text/plain">>, '_'}], Req, State}.

-spec init({tcp, http}, cowboy_req:req(), []) ->
    {upgrade, protocol, cowboy_rest}.

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-spec put(cowboy_req:req(), term()) ->
    {boolean(), cowboy_req:req(), term()}.

put(Req, undefined) ->
    {false, Req, undefined};
put(Req, Logger) ->
    case cowboy_req:body(Req) of
        {ok, Bin, Req2} ->
            ms_http_logger:log(Logger, Bin),
            {true, Req2, Logger};
        {error, _Reason} ->
            {false, Req, Logger}
    end.

-spec resource_exists(cowboy_req:req(), term()) ->
    {boolean(), cowboy_req:req(), term()}.

resource_exists(Req, State) ->
    case cowboy_req:binding(key, Req) of
        {undefined, Req2} ->
            {false, Req2, State};
        {Logger, Req2} ->
             {true, Req2, Logger}
    end.
