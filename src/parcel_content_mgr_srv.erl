-module(parcel_content_mgr_srv).
-behaviour(gen_server).

%% API
-export([get_version/0, get_content/0, get_content/1]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-include("parcel.hrl").
-include("parcel_content.hrl").

-record(state, {
    version     :: undefined | parcel:version(),
    content_tid :: undefined | ets:tid()
}).

-define(VERSION_TBL, current_version_tbl).
-define(CONTENT_TBL, content_tbl).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_version() -> parcel:version() | undefined.
get_version() ->
    case ets:lookup(?VERSION_TBL, version) of
        []                -> undefined;
        [{_, Version, _}] -> Version
    end.

-spec get_content() -> {parcel:version(), [parcel:content()]}.
get_content() ->
    {Version, Tid} = get_version_tid(),
    {Version, do_get_content(Tid)}.

-spec get_content(parcel:data_type()) ->
    {parcel:version(), parcel:content() | undefined}.
get_content(DataType) ->
    {Version, Tid} = get_version_tid(),
    {Version, do_lookup(Tid, DataType)}.

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------
%% @hidden
init([]) ->
    bootstrap_version_tbl(),
    self() ! update_content,
    {ok, #state{}}.

%% @hidden
handle_call(_Call, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Cast, State) ->
    {noreply, State}.

%% @hidden
handle_info(update_content, State) ->
    {noreply, update_content(State)};
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
code_change(_, State, _) ->
    {ok, State}.

%% @hidden
terminate(_Reason, _) ->
    ok.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
bootstrap_version_tbl() ->
    ?VERSION_TBL = ets:new(?VERSION_TBL, [named_table, set]).

update_content(#state{version = Version} = State) ->
    PollInterval = parcel:get_env(content_poll_interval_sec, 30),
    erlang:send_after(PollInterval * 1000, self(), update_content),
    case parcel_content_local:get_version() of
        {ok, Version} ->
            State;
        {ok, NewVersion} ->
            ?INFO("New content version '~s'", [NewVersion]),
            case parcel_content_local:get_content(NewVersion) of
                {ok, Archive} ->
                    parse_and_process_content(Archive, NewVersion, State);
                {error, not_found} ->
                    ?ERROR("Content not found for version '~s'", [NewVersion]),
                    State
            end;
        {error, not_found} ->
            ?WARNING("Current version file not found"),
            update_content([], undefined, State)
    end.

parse_and_process_content(Archive, NewVersion, State) ->
    case parcel_content_parser:parse(Archive) of
        {ok, Content} ->
            update_content(Content, NewVersion, State);
        {error, Reason} ->
            ?ERROR("Content parsing error: ~p", [Reason]),
            State
    end.

update_content(NewContent, NewVersion,
               #state{content_tid = CurrentTid} = State) ->
    CurrentContent = do_get_content(CurrentTid),
    Changed = get_changed_content(NewContent, CurrentContent),
    Deleted = get_deleted_content(NewContent, CurrentContent),
    ?INFO("Updated content: ~p. Deleted content: ~p", [Changed, Deleted]),
    stop_streams(Deleted),
    NewContentTid = write_content(NewContent, CurrentTid, NewVersion),
    State#state{version     = NewVersion,
                content_tid = NewContentTid}.

stop_streams(Deleted) ->
    StreamNames = lists:append(
        [ parcel_stream_sup:get_stream_names_by_data_type(DT) || DT <- Deleted ]
    ),
    [ parcel_stream_sup:terminate_child(Name) || Name <- StreamNames ].

get_version_tid() ->
    [{_, Version, Tid}] = ets:lookup(?VERSION_TBL, version),
    {Version, Tid}.

do_get_content(undefined) -> [];
do_get_content(Tid)       -> ets:tab2list(Tid).

do_lookup(Tid, DataType) ->
    case ets:lookup(Tid, DataType) of
        [Content] -> Content;
        []        -> undefined
    end.

write_content([], ContentTid, NewVersion) ->
    write_version(ContentTid, NewVersion, undefined);
write_content(NewContent, ContentTid, NewVersion) ->
    NewContentTid = ets:new(?CONTENT_TBL, [set, {keypos, #content.data_type}]),
    true = ets:insert(NewContentTid, NewContent),
    write_version(ContentTid, NewVersion, NewContentTid).

write_version(ContentTid, NewVersion, NewContentTid) ->
    true = ets:insert(?VERSION_TBL, {version, NewVersion, NewContentTid}),
    delete_obsolete_content_tbl(ContentTid),
    NewContentTid.

delete_obsolete_content_tbl(undefined) -> ok;
delete_obsolete_content_tbl(Tid)       -> true = ets:delete(Tid).

get_deleted_content(NewContent, Current) ->
    get_data_type(Current) -- get_data_type(NewContent).

get_changed_content(NewContent, Current) ->
    [ New#content.data_type || New <- NewContent,
                               is_changed_content(New, Current) ].

is_changed_content(New, Current) ->
    case lists:keyfind(New#content.data_type, #content.data_type, Current) of
        false -> true;
        Old   -> not is_content_equal(New, Old)
    end.

is_content_equal(#content{data_script = NewDS},
                 #content{data_script = OldDS}) ->
    NewDS =:= OldDS.

get_data_type(Content) ->
    [ DT || #content{data_type = DT} <- Content ].

%%------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(DATA_TYPE_1, <<"accelerometer">>).
-define(DATA_TYPE_2, <<"x-ray-meter">>).

-define(CONTENT_1,
    #content{
        data_type   = ?DATA_TYPE_1,
        data_script = {?DATA_TYPE_1, <<"python here">>},
        window_sec  = 60,
        period_sec  = 5
    }
).

-define(CONTENT_2,
    #content{
        data_type   = ?DATA_TYPE_2,
        data_script = {?DATA_TYPE_2, <<"python here">>},
        window_sec  = 120,
        period_sec  = 30
    }
).

-define(CONTENT_3,
    #content{
        data_type   = ?DATA_TYPE_1,
        data_script = {?DATA_TYPE_1, <<"other python here">>},
        window_sec  = 100,
        period_sec  = 5
    }
).

test_setup() ->
    meck:new(ContentLocal = parcel_content_local),
    meck:expect(ContentLocal, get_content, 1, fun mock_get_content/1),
    meck:new(Parser = parcel_content_parser),
    meck:expect(Parser, parse, 1, fun mock_parse/1),
    bootstrap_version_tbl(),
    [ContentLocal, Parser].

test_teardown(Mods) ->
    meck:unload(Mods),
    ets:delete(?VERSION_TBL),
    ok.

parcel_content_mgr_srv_test_() ->
    {foreach,
     local,
     fun test_setup/0,
     fun test_teardown/1,
     [
         {"Test content received and parsed",      fun test_content_received/0},
         {"Test parsing content fails",            fun test_content_parse_failed/0},
         {"Test content not found",                fun test_content_not_found/0},
         {"Test version not found",                fun test_version_not_found/0},
         {"Test get content by key / get version", fun test_get_content/0},
         {"Test get changed deleted content",
          fun test_get_changed_deleted_content/0}
     ]}.

test_content_received() ->
    V1 = <<"v1.0">>,
    V2 = <<"v1.1">>,
    meck:sequence(parcel_content_local, get_version, 0,
                  [{ok, V1}, {ok, V1}, {ok, V2}]),
    State1 = update_content(#state{}),
    Expected1 = #state{version     = V1,
                       content_tid = get_tid()},
    ?assertEqual(Expected1, State1),
    ?assertEqual({V1, [?CONTENT_1]}, get_content()),
    State2 = update_content(State1),
    ?assertEqual(Expected1, State2),
    ?assertEqual({V1, [?CONTENT_1]}, get_content()),
    State3 = update_content(State1),
    Expected3 = #state{version     = V2,
                       content_tid = get_tid()},
    ?assertEqual(Expected3, State3),
    {Version, Content} = get_content(),
    ?assertEqual(
        {V2, [?CONTENT_1, ?CONTENT_2]},
        {Version, lists:sort(Content)}
    ).

test_content_parse_failed() ->
    mock_get_version_s3({ok, <<"v1.2">>}),
    State = update_content(InitialState = #state{}),
    ?assertEqual(InitialState, State).

test_content_not_found() ->
    mock_get_version_s3({ok, <<"v1.3">>}),
    State = update_content(InitialState = #state{}),
    ?assertEqual(InitialState, State).

test_version_not_found() ->
    mock_get_version_s3({error, not_found}),
    State = update_content(InitialState = #state{}),
    ?assertEqual(InitialState#state{version = undefined}, State).

test_get_content() ->
    ?assertEqual(undefined, get_version()),
    mock_get_version_s3({ok, V = <<"v1.0">>}),
    update_content(#state{}),
    ?assertEqual(V, get_version()),
    ?assertEqual({V, ?CONTENT_1}, get_content(?DATA_TYPE_1)).

test_get_changed_deleted_content() ->
    New = [?CONTENT_3],
    Old = [?CONTENT_1, ?CONTENT_2],
    ?assertEqual([?DATA_TYPE_1], get_changed_content(New, Old)),
    ?assertEqual([?DATA_TYPE_2], get_deleted_content(New, Old)).

mock_get_version_s3(Result) ->
    meck:expect(parcel_content_local, get_version, 0, Result).

mock_get_content(<<"v1.0">>) -> {ok, <<"archive_1_0">>};
mock_get_content(<<"v1.1">>) -> {ok, <<"archive_1_1">>};
mock_get_content(<<"v1.2">>) -> {ok, <<"archive_1_2">>};
mock_get_content(<<"v1.3">>) -> {error, not_found}.

mock_parse(<<"archive_1_0">>) -> {ok, [?CONTENT_1]};
mock_parse(<<"archive_1_1">>) -> {ok, [?CONTENT_1, ?CONTENT_2]};
mock_parse(<<"archive_1_2">>) -> {error, some_reason}.

get_tid() ->
    element(2, get_version_tid()).

-endif.
