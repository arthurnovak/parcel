-module(parcel_content_parser).

%% API
-export([
    parse/1
]).

-define(CONTENT_SCHEMA, "content_schema.json").

-include("parcel_content.hrl").

-export_type([content_module/0]).

-type content_module() :: {string(), binary()}.

-spec parse(binary() | [{string(), binary()}]) ->
    {ok, [parcel:content()]} | {error, any()}.
parse(Archive) when is_binary(Archive) ->
    case extract_zip(Archive) of
        {ok, Content} ->
            parse(Content);
        Error         -> Error
    end;
parse(PyMods) when is_list(PyMods) ->
    Schema = get_schema(?CONTENT_SCHEMA),
    {ok, Driver} = parcel_python:new(PyMods),
    try
        {ok, parse_content(PyMods, Driver, Schema)}
    catch
        _Any:Reason -> {error, Reason}
    after
        parcel_python:destroy(Driver)
    end.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
extract_zip(Archive) ->
    case zip:extract(Archive, [memory]) of
        {ok, Data} ->
            {ok, [ {filename:basename(N, ".py"), Bin} || {N, Bin} <- Data ]};
        {error, Reason} ->
            {error, {archive_invalid, Reason}}
    end.

parse_content(PyMods, Driver, Schema) ->
    parse_content(PyMods, Driver, Schema, []).

parse_content([], _, _, Acc) ->
    check_unique_data_types(Acc),
    Acc;
parse_content([{PyMod, PyBin}|Tail], Driver, Schema, Acc) ->
    InitCfg = parcel_util:decode_json(call_py(Driver, PyMod, init_wrapper, [])),
    validate_schema(Schema, PyMod, InitCfg),
    Record = get_record(PyBin, InitCfg),
    parse_content(Tail, Driver, Schema, [Record|Acc]).

call_py(Driver, Module, Fun, Args) ->
    case parcel_python:call(Driver, Module, Fun, Args) of
        {ok,    Result} -> Result;
        {error, Reason} -> throw({call_fail, Module, Fun, Reason})
    end.

validate_schema(Schema, PyMod, InitCfg) ->
    Opt = [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>},
           {parser_fun, fun jesse_parser_fun/1}],
    case jesse:validate_with_schema(Schema, InitCfg, Opt) of
        {ok, _}         -> ok;
        {error, Reason} -> throw({schema_validation_fail, PyMod, Reason})
    end.

jesse_parser_fun(Json) when is_binary(Json) -> parcel_util:decode_json(Json);
jesse_parser_fun(Obj)                       -> Obj.

check_unique_data_types(Content) ->
    check_unique_elements([ C#content.data_type || C <- Content ], data_types).

check_unique_elements(List, Desc) ->
    case lists:sort(List) =:= lists:usort(List) of
        true  -> ok;
        false -> throw({not_unique, Desc, List})
    end.

get_record(Py, #{<<"dataType">>         := DataType,
                 <<"computeWindowSec">> := WindowSec,
                 <<"computePeriodSec">> := PeriodSec}) ->
    #content{
        data_type   = DataType,
        data_script = {DataType, Py},
        window_sec  = WindowSec,
        period_sec  = PeriodSec
    }.

get_schema(SchemaFileName) ->
    parcel_util:decode_json(parcel_util:read_priv_file(SchemaFileName)).
