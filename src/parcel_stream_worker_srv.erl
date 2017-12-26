-module(parcel_stream_worker_srv).
-behaviour(gen_server).

%% API
-export([
    put_records/2
]).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-include("parcel.hrl").
-include("parcel_content.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {
    stream     :: binary(),
    data_type  :: binary(),
    version    :: undefined | parcel:version(),
    py_driver  :: undefined | parcel_python:driver(),
    window_sec :: pos_integer(),
    period_sec :: pos_integer(),
    queue      :: undefined | parcel_queue:queue()
}).

start_link(StreamName, DataType) ->
    gen_server:start_link(?MODULE, [StreamName, DataType], []).

put_records(Pid, Records) ->
    gen_server:cast(Pid, {put_records, Records}).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------
%% @hidden
init([StreamName, DataType]) ->
    self() ! init,
    {ok, #state{stream    = StreamName,
                data_type = DataType,
                queue     = parcel_queue_proc:new()}}.

%% @hidden
handle_call(_Call, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast({put_records, Records}, State) ->
    ?INFO("Put records: ~p", [Records]),
    {noreply, do_put_records(Records, State)};
handle_cast(_Cast, State) ->
    {noreply, State}.

%% @hidden
handle_info(init, State0) ->
    #state{period_sec = PeriodSec} = State1 = init_state(State0),
    erlang:send_after(PeriodSec * 1000, self(), process),
    {noreply, State1};
handle_info(process, #state{period_sec = PeriodSec} = State) ->
    erlang:send_after(PeriodSec * 1000, self(), process),
    Timestamp = parcel_util:epoch_millis(),
    NewState  = maybe_update_content(State),
    do_process_records(Timestamp, NewState),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
code_change(_, State, _) ->
    {ok, State}.

%% @hidden
terminate(_Reason, #state{queue = Q}) ->
    parcel_queue_proc:delete(Q).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
do_put_records(Records, #state{queue = Q} = State) ->
    Data = [ {parcel_util:iso_8601_to_epoch_millis(IsoDT), Record}
             || #{<<"createdTime">> := IsoDT} = Record <- Records ],
    State#state{queue = parcel_queue_proc:in(Data, Q)}.

do_process_records(ToTs, #state{stream     = Stream,
                                py_driver  = Drv,
                                data_type  = DataType,
                                window_sec = WindowSec,
                                queue      = Q0} = State) ->
    FromTs = ToTs - WindowSec * 1000,
    SelectMS = ets:fun2ms(fun({TS, D}) when TS >= FromTs, TS < ToTs -> D end),
    {Data, Q1} = parcel_queue_proc:out(SelectMS, Q0),
    case Data of
        [] ->
            ?INFO("No data in '~s' stream queue for current window", [Stream]);
        Data ->
            Args = [jiffy:encode(Data), get_metadata(FromTs, ToTs)],
            case parcel_python:call(Drv, DataType, process_wrapper, Args) of
                {ok, Results} ->
                    index_transformed_data(Results, DataType, Stream);
                {error, Reason} ->
                    ?ERROR("Python process call fail for stream '~s' with "
                           "reason: ~p", [Stream, Reason])
            end
    end,
    DeleteMS = ets:fun2ms(fun({TS, _}) -> TS < FromTs end),
    Q2 = parcel_queue_proc:remove(DeleteMS, Q1),
    State#state{queue = Q2}.

init_state(#state{data_type = DataType} = State) ->
    {Version, Content} = parcel_content_mgr_srv:get_content(DataType),
    State#state{version    = Version,
                window_sec = Content#content.window_sec,
                period_sec = Content#content.period_sec,
                py_driver  = init_python_driver(Content)}.

maybe_update_content(#state{version   = Version,
                            py_driver = Driver} = State) ->
    case parcel_content_mgr_srv:get_version() of
        Version ->
            State;
        _ ->
            parcel_python:destroy(Driver),
            init_state(State)
    end.

index_transformed_data(Data, DataType, Stream) ->
    case parcel_elastic_search:index_document(DataType, Data) of
        {ok, _} ->
            ?INFO("Successfully put data to Elastic Search. Data: ~p", [Data]),
            ok;
        {error, Reason} ->
            ?ERROR("Failed to index processed data for '~s' stream with "
                   "reason: ~p. Data: ~p", [Stream, Reason, Data])
    end.

get_metadata(From, To) ->
    Meta = #{from => parcel_util:epoch_millis_to_iso_8601(From),
             to   => parcel_util:epoch_millis_to_iso_8601(To)},
    jiffy:encode(Meta).

init_python_driver(#content{data_script = DS}) ->
    {ok, Driver} = parcel_python:new([DS]),
    Driver.

%%------------------------------------------------------------------------------
%% Unit Tests
%%------------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(DATA_TYPE,  <<"accelerometer">>).
-define(WINDOW_SEC, 60).

-define(DATA_SCRIPT,
    <<"def process(data, meta):\n",
      "    return data",
      "\n">>
).

-define(CONTENT,
    #content{
        data_type   = ?DATA_TYPE,
        data_script = {?DATA_TYPE, ?DATA_SCRIPT},
        window_sec  = ?WINDOW_SEC,
        period_sec  = 5
    }
).

test_setup() ->
    meck:new(ContentMgr = parcel_content_mgr_srv),
    meck:expect(ContentMgr, get_content, 1, {<<"v1.0">>, ?CONTENT}),
    meck:new(ES = parcel_elastic_search),
    meck:expect(ES, index_document, 2, {ok, undefined}),
    [ContentMgr, ES].

parcel_stream_worker_srv_test_() ->
    {foreach,
     local,
     fun test_setup/0,
     fun meck:unload/1,
     [
         {"Test process records", fun test_process_records/0}
     ]}.

test_process_records() ->
    Time1 = parcel_util:epoch_millis(),
    Time2 = Time1 - ?WINDOW_SEC * 1000,
    CreatedTime1 = parcel_util:epoch_millis_to_iso_8601(Time1),
    CreatedTime2 = parcel_util:epoch_millis_to_iso_8601(Time2),
    Record1 = #{<<"createdTime">> => CreatedTime1,
                <<"data">>        => <<"blah">>},
    Record2 = #{<<"createdTime">> => CreatedTime2,
                <<"data">>        => <<"nothing interesting">>},
    Queue0 = parcel_queue_proc:new(),
    ?assertEqual(#{queue => [],
                   size  => 0},
                 parcel_queue_proc:dump(Queue0)),
    State0 = init_state(#state{data_type = ?DATA_TYPE,
                               queue     = Queue0}),
    ?assertMatch(#state{data_type  = ?DATA_TYPE,
                        version    = <<"v1.0">>,
                        window_sec = ?WINDOW_SEC,
                        period_sec = 5},
                 State0),
    #state{queue = Queue1} = State1 = do_put_records([Record1, Record2], State0),
    ?assertEqual(#{queue =>[{Time2, Record2},
                            {Time1, Record1}],
                   size  => 2},
                 parcel_queue_proc:dump(Queue1)),
    #state{queue = Queue2} = do_process_records(Time1 + 1000, State1),
    ?assertEqual(#{queue =>[{Time1, Record1}],
                   size  => 1}, parcel_queue_proc:dump(Queue2)).

-endif.
