-module(parcel_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, T), {I, {I, start_link, []}, permanent, 10000, T, [I]}).

%%%-----------------------------------------------------------------------------
%% API functions
%%%-----------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @hidden
init([]) ->
    Children = [
        ?CHILD(parcel_content_mgr_srv, worker),
        ?CHILD(parcel_stream_sup, supervisor)
    ],
    {ok, {{one_for_all, 10, 10}, Children}}.
