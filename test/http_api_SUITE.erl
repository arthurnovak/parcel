-module(http_api_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("parcel.hrl").

-export([all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_get_set_delete_version/1,
    test_get_put_content/1,
    test_put_measurements/1
]).

all() ->
    [
        test_get_set_delete_version,
        test_get_put_content,
        test_put_measurements
    ].

init_per_suite(Config) ->
    application:load(?APP),
    TmpPath = ?config(data_dir, Config) ++ "tmp_content",
    application:set_env(?APP, content_path, TmpPath),
    ok = parcel:start(),
    case ?config(url, Config) of
        undefined -> [{url, "http://localhost:8080/parcel/api"}|Config];
        _         -> Config
    end.

end_per_suite(_Config) ->
    ok = parcel:stop(),
    meck:unload().

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

test_get_set_delete_version(Config) ->
    Url  = join([?config(url, Config), "content", "version"]),
    Post = {Url, [], "application/json", <<"v1.1">>},
    ?assertMatch(
        {ok, {{_, 201, _}, _, []}},
        httpc:request(post, Post, [], [])
    ),
    ?assertMatch(
        {ok, {{_, 200, _}, _, "v1.1"}},
        httpc:request(Url)
    ),
    ?assertMatch(
        {ok, {{_, 204, _}, _, ""}},
        httpc:request(delete, {Url, []}, [], [])
    ),
    ?assertMatch(
        {ok, {{_, 404, _}, _, ""}},
        httpc:request(Url)
    ),
    PostEmptyBody = {Url, [], "application/json", <<>>},
    ?assertMatch(
        {ok, {{_, 400, _}, _, ""}},
        httpc:request(post, PostEmptyBody, [], [])
    ).

test_get_put_content(Config) ->
    UrlPost = join([?config(url, Config), "content", "version", "v1.0"]),
    Content = zip_content("test_valid_content/", Config),
    Req     = {UrlPost, [], "application/octet-stream", Content},
    ?assertMatch(
        {ok, {{_, 201, _}, _, []}},
        httpc:request(post, Req, [], [])
    ),
    UrlGet = join([?config(url, Config), "content"]),
    {ok, {{_, 200, _}, _, Result1}} = httpc:request(UrlGet),
    {ok, {{_, 200, _}, _, Result2}} = httpc:request(UrlPost),
    ?assertEqual(Content, list_to_binary(Result1)),
    ?assertEqual(Content, list_to_binary(Result2)).

test_put_measurements(Config) ->
    Url = join([?config(url, Config), "measurements"]),
    Record = #{<<"sensorId">>    => <<"sid1">>,
               <<"dataUnit">>    => <<"m/s^2">>,
               <<"dataType">>    => <<"accelerometer">>,
               <<"data">>        => [1,2,3],
               <<"createdTime">> => <<"2017-12-27T16:28:22.260000Z">>},
    Post1 = {Url, [], "application/json", jiffy:encode(Record)},
    ?assertMatch(
        {ok, {{_, 201, _}, _, []}},
        httpc:request(post, Post1, [], [])
    ),
    InvalidRecord = Record#{<<"createdTime">> => 123},
    Post2 = {Url, [], "application/json", jiffy:encode(InvalidRecord)},
    ?assertMatch(
        {ok, {{_, 500, _}, _, _}},
        httpc:request(post, Post2, [], [])
    ).

join(List) ->
    string:join(List, "/").

zip_content(ContentDir, Config) ->
    Path = ?config(data_dir, Config) ++ ContentDir,
    {ok, {_, Archive}} = zip:create("zip", [Path], [memory]),
    Archive.
