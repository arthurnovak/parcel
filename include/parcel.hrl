-ifndef(parcel_hrl).
-define(parcel_hrl, 1).

-define(APP, parcel).

-define(INFO(Msg),    ?INFO(Msg, [])).
-define(WARNING(Msg), ?WARNING(Msg, [])).
-define(ERROR(Msg),   ?ERROR(Msg, [])).

-define(INFO(Msg, Args),    lager:info(Msg, Args)).
-define(WARNING(Msg, Args), lager:warning(Msg, Args)).
-define(ERROR(Msg, Args),   lager:error(Msg, Args)).

-define(API_PATH_CONTENT, "/parcel/api/content").
-define(API_PATH_MEASURE, "/parcel/api/measurements").

-endif.
