-module(parcel_v1_http_measure).
-behaviour(parcel_http_handler).

-export([
    init/2,
    get/1,
    post/2,
    delete/1
]).

-include("parcel.hrl").

-define(SENSOR_DATA_SCHEMA, "sensor_data_schema.json").

-spec init(map(), list()) -> {ok, map(), list()}.
init(Req, Opts) ->
    parcel_http_handler:handle_request(?MODULE, ?API_PATH_MEASURE, Req, Opts).

get(_) ->
    {error, bad_request}.

post([], Data) ->
    case parse_validate_measurements(Data) of
        {ok, Decoded} -> parcel_proc:process(Decoded);
        Error         -> Error
    end;
post(_, _) ->
    {error, bad_request}.

delete(_) ->
    {error, bad_request}.

parse_validate_measurements(Data) ->
    try
        Decoded = decode_measurements(Data),
        Schema  = get_schema(),
        [ validate_schema(Json, Schema) || Json <- Decoded ],
        {ok, Decoded}
    catch
        _Any:Reason -> {error, {Reason, erlang:get_stacktrace()}}
    end.

decode_measurements(Data) ->
    case parcel_util:decode_json(Data) of
        List when is_list(List) -> List;
        Map                     -> [Map]
    end.

validate_schema(Json, Schema) ->
    Opt = [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>},
           {parser_fun, fun jesse_parser_fun/1}],
    case jesse:validate_with_schema(Schema, Json, Opt) of
        {ok, _}         -> ok;
        {error, Reason} -> throw({schema_validation_failure, Json, Reason})
    end.

jesse_parser_fun(Json) when is_binary(Json) -> parcel_util:decode_json(Json);
jesse_parser_fun(Obj)                       -> Obj.

get_schema() ->
    parcel_util:decode_json(parcel_util:read_priv_file(?SENSOR_DATA_SCHEMA)).
