-module(parcel_proc).

%% API
-export([process/1]).

-include("parcel.hrl").
-include("parcel_content.hrl").

-define(SENSOR_DATA_SCHEMA, "sensor_data_schema.json").

-spec process([map()]) -> ok.
%% @doc We assume that data batch came from one source has similar data_type.
%% Which means that one sensor cannot send data of different types.
%%
process([First|_] = Data) ->
    #{<<"sensorId">> := SensorId,
      <<"dataType">> := DataType} = First,
    case parcel_content_mgr_srv:get_content(DataType) of
        {_, undefined} ->
            ?INFO("No '~s' content to process incoming data", [DataType]),
            ok;
        {_, #content{}} ->
            {ok, Pid} = maybe_start_stream(SensorId, DataType),
            ok = parcel_stream_worker_srv:put_records(Pid, Data)
    end.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
maybe_start_stream(SensorId, DataType) ->
    Name = parcel_stream_sup:get_stream_name(DataType, SensorId),
    case parcel_stream_sup:start_child(Name, DataType) of
        {ok, Pid} ->
            ?INFO("Start stream '~s'", [Name]),
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid}
    end.
