-module(parcel_elastic_search).

%% API
-export([
    index_document/2,
    index_exists/1,
    search/1,
    count/1,
    delete_index/0
]).

-include("parcel.hrl").
-include_lib("erlastic_search/include/erlastic_search.hrl").

-spec index_document(binary(), binary() | map()) ->
    {ok, erlastic_success_result()} | {error, any()}.
index_document(Type, Data) ->
    erlastic_search:index_doc(get_index(), Type, Data).

-spec index_exists(binary()) -> boolean() | {error, any()}.
index_exists(Index) ->
    case erlastic_search:index_exists(Index) of
        {ok, Bool} -> Bool;
        Error      -> Error
    end.

-spec search(binary()) -> {ok, erlastic_success_result()} | {error, any()}.
search(Query) ->
    erlastic_search:search(get_index(), Query).

-spec count(binary()) -> {ok, erlastic_success_result()} | {error, any()}.
count(Query) ->
    erlastic_search:search(get_index(), Query).

-spec delete_index() -> {ok, erlastic_success_result()} | {error, any()}.
delete_index() ->
    erlastic_search:delete_index(get_index()).

get_index() ->
    atom_to_binary(parcel:get_env(es_index, ?APP), utf8).
