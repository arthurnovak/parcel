-module(parcel_v1_http_content).
-behaviour(parcel_http_handler).

-export([
    init/2,
    get/1,
    post/2,
    delete/1
]).

-include("parcel.hrl").

-spec init(map(), list()) -> {ok, map(), list()}.
init(Req, Opts) ->
    parcel_http_handler:handle_request(?MODULE, ?API_PATH_CONTENT, Req, Opts).

get([]) ->
    parcel_content_local:get_content();
get([<<"version">>]) ->
    parcel_content_local:get_version();
get([<<"version">>, Version]) ->
    parcel_content_local:get_content(Version);
get(_) ->
    {error, bad_request}.

post([<<"version">>], Version) ->
    parcel_content_local:set_version(Version);
post([<<"version">>, Version], Content) ->
    parcel_content_local:put_content(Version, Content);
post(_, _) ->
    {error, bad_request}.

delete([<<"version">>]) ->
    case parcel_content_local:unset_version() of
        ok    -> {ok, no_content};
        Error -> Error
    end;
delete(_) ->
    {error, bad_request}.
