-module(parcel_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("parcel.hrl").

start(_StartType, _StartArgs) ->
    ok = parcel_python:bootstrap(),
    {ok, _} = start_cowboy(),
    parcel_sup:start_link().

stop(_State) ->
    ok.

start_cowboy() ->
    ContentSpec = {?API_PATH_CONTENT ++ "/[...]", parcel_v1_http_content, []},
    MeasureSpec = {?API_PATH_MEASURE ++ "/[...]", parcel_v1_http_measure, []},
    Dispatch = cowboy_router:compile([{'_', [ContentSpec, MeasureSpec]}]),
    cowboy:start_clear(parcel_http_listener,
                       [{port, 8080}],
                       #{env => #{dispatch => Dispatch}}).
