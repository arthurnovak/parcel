-module(parcel_queue_proc).

%% API
-export([
    new/0,
    in/2,
    out/2,
    remove/2,
    delete/1,
    dump/1
]).

-export_type([
    queue/0
]).

-record(queue, {
    size = 0 :: non_neg_integer(),
    table    :: ets:tid()
}).

-opaque queue() :: #queue{}.

-spec new() -> queue().
new() ->
    #queue{table = ets:new(queue_tbl, [ordered_set])}.

-spec in([any()], queue()) -> queue().
in([], Q) ->
    Q;
in(Data, #queue{table = Tbl,
                size  = Size} = Q) ->
    ets:insert(Tbl, Data),
    Q#queue{size = Size + length(Data)}.

-spec out(ets:match_spec(), queue()) -> {[map()], queue()}.
out(MatchSpec, #queue{table = Tbl} = Q) ->
    Records = ets:select(Tbl, MatchSpec),
    {Records, Q}.

-spec remove(ets:match_spec(), queue()) -> queue().
remove(MatchSpec, #queue{table = Tbl,
                         size  = Size} = Q) ->
    Deleted = ets:select_delete(Tbl, MatchSpec),
    Q#queue{size = Size - Deleted}.

-spec delete(queue()) -> ok.
delete(#queue{table = Tbl}) ->
    true = ets:delete(Tbl),
    ok.

-spec dump(queue()) -> list().
dump(#queue{table = QTbl,
            size  = QSize}) ->
    #{queue => ets:tab2list(QTbl),
      size  => QSize}.
