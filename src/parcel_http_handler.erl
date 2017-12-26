-module(parcel_http_handler).

%% API
-export([
    handle_request/4
]).

-export_type([
    path/0,
    body/0
]).

-type path() :: binary().
-type body() :: binary().

-define(CONTENT_TYPE, #{<<"content-type">> => <<"application/octet-stream">>}).

-callback get(path())          -> {ok, any()} | {error, any()}.
-callback post(path(), body()) -> ok | {error, any()}.
-callback delete(path())       -> {ok, no_content} | {error, any()}.

-spec handle_request(module(), binary(), map(), list()) ->
    {ok, map(), list()}.
handle_request(Mod, PathMatch, Req, Opts) ->
    Path = parse_path(Req, PathMatch),
    NewReq = case cowboy_req:method(Req) of
                 <<"GET">>    -> get(Mod, Path, Req);
                 <<"POST">>   -> post(Mod, Path, Req);
                 <<"DELETE">> -> delete(Mod, Path, Req)
             end,
    {ok, NewReq, Opts}.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
get(Mod, Path, Req) ->
    handle_response(fun Mod:get/1, [Path], Req).

post(Mod, Path, Req) ->
    case cowboy_req:body_length(Req) of
        Length when Length > 0 ->
            {ok, Body, NewReq} = read_body(Req),
            handle_response(fun Mod:post/2, [Path, Body], NewReq);
        _Other ->
            cowboy_reply(400, Req)
    end.

delete(Mod, Path, Req) ->
    handle_response(fun Mod:delete/1, [Path], Req).

handle_response(Fun, Args, Req) ->
    Result =
        case erlang:apply(Fun, Args) of
            ok                   -> {201, #{}, <<>>};
            {ok, no_content}     -> {204, #{}, <<>>};
            {ok, Resp}           -> {200, ?CONTENT_TYPE, Resp};
            {error, not_found}   -> 404;
            {error, bad_request} -> 400;
            {error, Reason}      -> {500, #{}, format_error(Reason)}
        end,
    cowboy_reply(Result, Req).

cowboy_reply({Code, CT, Data}, Req) -> cowboy_req:reply(Code, CT, Data, Req);
cowboy_reply(Code, Req)             -> cowboy_req:reply(Code, Req).

parse_path(Req, PathMatch) ->
    Path = cowboy_req:path(Req),
    [_, Rest0] = binary:split(Path, list_to_binary(PathMatch)),
    [_|Rest1]  = binary:split(Rest0, <<"/">>, [global]),
    Rest1.

read_body(Req) ->
    read_body(Req, <<>>).

read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, More, NewReq}   -> {ok, <<Acc/binary, More/binary>>, NewReq};
        {more, More, NewReq} -> read_body(NewReq, <<Acc/binary, More/binary>>)
    end.

format_error(Reason) ->
    jiffy:encode(#{result => error,
                   reason => parcel_util:format_to_binary(Reason)}).
