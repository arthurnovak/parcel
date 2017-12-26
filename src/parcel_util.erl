-module(parcel_util).

%% API
-export([
    decode_json/1,
    read_priv_file/1,
    format_to_binary/1,
    epoch_millis/0,
    iso_8601_to_epoch_millis/1,
    epoch_millis_to_iso_8601/1,
    datetime_to_epoch_seconds/1,
    epoch_seconds_to_datetime/1
]).

-include("parcel.hrl").

-define(UTC_TO_GREGORIAN, 62167219200).

-spec decode_json(binary()) -> jiffy:json_value().
decode_json(Json) ->
    jiffy:decode(Json, [return_maps]).

-spec read_priv_file(file:filename()) -> binary().
read_priv_file(FileName) ->
    PrivDir = case code:priv_dir(?APP) of
                  {error, _} -> "priv";
                  Dir        -> Dir
              end,
    {ok, File} = file:read_file(filename:join(PrivDir, FileName)),
    File.

-spec format_to_binary(term()) -> binary().
format_to_binary(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).

epoch_millis() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000 + Micro div 1000.

iso_8601_to_epoch_millis(Bin) ->
    {ok, {Date, Time, MicroSec, _}} = rfc3339:parse(Bin),
    MilliSec = round(MicroSec/1000),
    datetime_to_epoch_seconds({Date, Time}) * 1000 + MilliSec.

epoch_millis_to_iso_8601(EpochMilliSec) ->
    EpochSec = EpochMilliSec div 1000,
    MilliSec = EpochMilliSec - EpochSec * 1000,
    {Date, Time} = epoch_seconds_to_datetime(EpochSec),
    {ok, Bin} = rfc3339:format({Date, Time, MilliSec * 1000, undefined}),
    Bin.

datetime_to_epoch_seconds(Datetime) ->
    Timestamp = calendar:datetime_to_gregorian_seconds(Datetime),
    (Timestamp - ?UTC_TO_GREGORIAN).

epoch_seconds_to_datetime(Seconds) ->
    calendar:gregorian_seconds_to_datetime(?UTC_TO_GREGORIAN + Seconds).
