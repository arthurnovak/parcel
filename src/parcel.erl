-module(parcel).

%% API
-export([
    start/0,
    stop/0,
    get_env/1,
    get_env/2
]).

-export_type([
    data_type/0,
    content/0,
    version/0
]).

-include("parcel.hrl").
-include("parcel_content.hrl").

-type data_type() :: binary().
-type content()   :: #content{}.
-type version()   :: binary().

-spec start() -> ok.
start() ->
    {ok, _Started} = application:ensure_all_started(?APP, permanent),
    ok.

-spec stop() -> ok.
stop() ->
    application:stop(?APP).

-spec get_env(atom()) -> any().
get_env(Key) ->
    get_env(Key, undefined).

-spec get_env(atom(), any()) -> any().
get_env(Key, Default) ->
    application:get_env(?APP, Key, Default).
