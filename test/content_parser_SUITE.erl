-module(content_parser_SUITE).

%% API
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("parcel_content.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_valid_content/1,
    test_invalid_schema/1,
    test_non_unique_data_types/1
]).

-define(DATA_TYPE, <<"test">>).

all() ->
    [
        test_valid_content,
        test_invalid_schema,
        test_non_unique_data_types
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    meck:unload().

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

test_valid_content(Config) ->
    ContentDir = "test_valid_content",
    Archive    = zip_content(ContentDir, Config),
    {ok, Py}   = file:read_file(get_path(Config, ContentDir, "test.py")),
    Record = #content{data_type   = ?DATA_TYPE,
                      data_script = {?DATA_TYPE, Py},
                      window_sec  = 60,
                      period_sec  = 5},
    ?assertEqual({ok, [Record]},
                 parcel_content_parser:parse(Archive)).

test_invalid_schema(Config) ->
    ContentDir  = "test_invalid_schema",
    Archive     = zip_content(ContentDir, Config),
    Description = <<"Sliding window to compute average measurements">>,
    Reason = {
        schema_validation_fail,
        "test",
        [{data_invalid,
          #{<<"description">> => Description,
            <<"minimum">>     => 1,
            <<"type">>        => <<"integer">>},
          wrong_type,
          <<"invalid">>,
          [<<"computeWindowSec">>]}]
    },
    ?assertMatch({error, Reason}, parcel_content_parser:parse(Archive)).

test_non_unique_data_types(Config) ->
    ContentDir = "test_non_unique_data_types",
    Archive    = zip_content(ContentDir, Config),
    ?assertMatch({error, {not_unique, data_types, [<<"test">>,<<"test">>]}},
                 parcel_content_parser:parse(Archive)).

zip_content(ContentDir, Config) ->
    Path = ?config(data_dir, Config) ++ ContentDir ++ "/",
    {ok, {_, Archive}} = zip:create("zip", [Path], [memory]),
    Archive.

get_path(Config, ContentDir, File) ->
    ?config(data_dir, Config) ++ ContentDir ++ "/" ++ File.
