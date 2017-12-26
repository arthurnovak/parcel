-module(parcel_stream_sup).
-behaviour(supervisor).

%% API
-export([
    get_stream/1,
    get_stream_name/2,
    get_stream_names_by_data_type/1
]).

-export([start_link/0, start_child/2, terminate_child/1]).
-export([init/1]).

-include("parcel.hrl").

-export_type([
    stream_name/0
]).

-type stream_name() :: binary().

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(StreamName, DataType) ->
    MFA = {M = parcel_stream_worker_srv, start_link, [StreamName, DataType]},
    ChildSpec = #{id       => StreamName,
                  start    => MFA,
                  restart  => transient,
                  shutdown => 5000,
                  type     => worker,
                  modules  => [M]},
    supervisor:start_child(?MODULE, ChildSpec).

terminate_child(StreamName) ->
    ?INFO("Terminate stream '~s'", [StreamName]),
    supervisor:terminate_child(?MODULE, StreamName),
    supervisor:delete_child(?MODULE, StreamName).

init([]) ->
    {ok, {{one_for_one, 1, 5}, []}}.

-spec get_stream_names_by_data_type(binary()) -> [stream_name()].
get_stream_names_by_data_type(DataType) ->
    Children = supervisor:which_children(?MODULE),
    [ Name || {Name, _, _, _} <- Children, is_data_type_stream(DataType, Name) ].

is_data_type_stream(DataType, StreamName) ->
    [DT|_] = binary:split(StreamName, <<"-">>),
    DataType =:= DT.

-spec get_stream(stream_name()) -> pid() | undefined.
get_stream(StreamName) ->
    Children = supervisor:which_children(?MODULE),
    case lists:keyfind(StreamName, 1, Children) of
        false                   -> undefined;
        {StreamName, Pid, _, _} -> Pid
    end.

-spec get_stream_name(binary(), binary()) -> stream_name().
get_stream_name(DataType, SensorId) ->
    <<DataType/binary, $-, SensorId/binary>>.
