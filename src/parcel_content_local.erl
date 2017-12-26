-module(parcel_content_local).

%% API
-export([
    get_version/0,
    set_version/1,
    unset_version/0,
    get_content/0,
    get_content/1,
    put_content/2,
    get_content_folder/0
]).

-include("parcel.hrl").

-define(VERSION_FILE, "current.version").
-define(ARCHIVE_EXT,  ".tar.gz").

-spec get_version() -> {ok, parcel:version()} | {error, any()}.
get_version() ->
    case file:read_file(get_version_path()) of
        {ok, Version0} ->
            [Version1|_] = binary:split(Version0, <<"\n">>),
            {ok, Version1};
        _Other ->
            {error, not_found}
    end.

-spec set_version(binary()) -> ok | {error, any()}.
set_version(Version) ->
    VersionPath = get_version_path(),
    file:write_file(VersionPath, Version, [write]).

-spec unset_version() -> ok | {error, any()}.
unset_version() ->
    case file:delete(get_version_path()) of
        {error, enoent} -> ok;
        Other           -> Other
    end.

-spec get_content() -> {ok, binary()} | {error, any()}.
get_content() ->
    case get_version() of
        {ok, Version} -> get_content(Version);
        Other         -> Other
    end.

-spec get_content(parcel:version()) ->
    {ok, binary()} | {error, not_found}.
get_content(Version) ->
    ContentFile = [binary_to_list(Version), ?ARCHIVE_EXT],
    ArchivePath = filename:join([get_content_folder(), ContentFile]),
    case file:read_file(ArchivePath) of
        {ok, Content} -> {ok, Content};
        _Other        -> {error, not_found}
    end.

-spec put_content(parcel:version(), binary()) -> ok | {error, any()}.
put_content(Version, Content) ->
    Path = [get_content_folder(), "/", binary_to_list(Version), ?ARCHIVE_EXT],
    case file:write_file(Path, Content, [write]) of
        ok    -> set_version(Version);
        Error -> Error
    end.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
get_content_folder() ->
    DefaultPath = filename:join([code:priv_dir(?APP), "content"]),
    parcel:get_env(content_path, DefaultPath).

get_version_path() ->
    filename:join([get_content_folder(), ?VERSION_FILE]).
