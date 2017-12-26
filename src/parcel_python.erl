-module(parcel_python).
-behaviour(gen_server).

%% API
-export([bootstrap/0]).
-export([new/1, destroy/1, call/4, get_tmp_dir/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export_type([
    driver/0
]).

-include("parcel.hrl").

-type driver() :: pid().

-record(state, {
    id         :: string(),
    path_dir   :: file:filename(),
    python_pid :: pid(),
    parent_pid :: pid()
}).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------
-spec bootstrap() -> ok.
bootstrap() ->
    TmpDir = get_tmp_dir(),
    ok = del_dir(TmpDir),
    ok = file:make_dir(TmpDir).

-spec new([{string(), binary()}]) -> {ok, driver()} | {error, any()}.
new(PyMods) ->
    Wrapped = parcel_python_content:wrap_all(PyMods),
    gen_server:start_link(?MODULE, [self(), Wrapped], []).

-spec destroy(driver()) -> ok.
destroy(Drv) ->
    gen_server:call(Drv, destroy, infinity).

-spec call(driver(), string(), atom(), [any()]) ->
    {ok, any()} | {error, {atom(), string(), [tuple()]}}.
call(Drv, Module, Fun, Args) when is_binary(Module) ->
    call(Drv, binary_to_atom(Module, utf8), Fun, Args);
call(Drv, Module, Fun, Args) when is_list(Module) ->
    call(Drv, list_to_atom(Module), Fun, Args);
call(Drv, Module, Fun, Args) ->
    gen_server:call(Drv, {call, Module, Fun, Args}, infinity).

-spec get_tmp_dir() -> string().
get_tmp_dir() ->
    application:get_env(?APP, python_path_dir, "/tmp/parcel").

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------
%% @hidden
init([ParentPid, PyFiles]) ->
    process_flag(trap_exit, true),
    Id = uuid:to_string(uuid:uuid1()),
    {ok, PathDir} = write_files(PyFiles, ".py", Id, get_tmp_dir()),
    {ok, PythonPid} = python:start_link([{python_path, PathDir}]),
    {ok, #state{id         = Id,
                path_dir   = PathDir,
                parent_pid = ParentPid,
                python_pid = PythonPid}}.

%% @hidden
handle_call({call, Mod, Fun, Args},  _From, #state{python_pid = Py} = State) ->
    Result =
        try
            {ok, python:call(Py, Mod, Fun, Args)}
        catch
            error:{python, ExcClass, ExcArgs, ExcStack} ->
                {error, {ExcClass, ExcArgs, ExcStack}}
        end,
    {reply, Result, State};
handle_call(destroy, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Call,  _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Cast, State) ->
    {noreply, State}.

%% @hidden
handle_info({'EXIT', Pid, Reason},
            #state{parent_pid = Parent, python_pid = Python} = State)
  when Pid =:= Parent; Pid =:= Python ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
code_change(_, State, _) ->
    {ok, State}.

%% @hidden
terminate(_Reason, #state{path_dir = Dir, python_pid = Python}) ->
    python:stop(Python),
    del_dir(Dir),
    ok.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
write_files(PyFiles, Ext, Id, TmpDir) ->
    PathDir = to_list(filename:join([TmpDir, Id])),
    case file:make_dir(PathDir) of
        ok ->
            [ ok = file:write_file([to_list(filename:join(PathDir, Mod)), Ext], to_binary(Wrapped))
              || {Mod, Wrapped} <- PyFiles ],
            {ok, PathDir};
        Error ->
            Error
    end.

del_dir(Dir) ->
    case file:list_dir(Dir) of
        {error, enoent} ->
            ok;
        {ok, Files} ->
            [ ok = del_file(filename:join(Dir, F)) || F <- Files ],
            file:del_dir(Dir)
    end.

del_file(File) ->
    case filelib:is_dir(File) of
        true  -> del_dir(File);
        false -> file:delete(File)
    end.

to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L)   -> L.

to_binary(L) when is_list(L)   -> list_to_binary(L);
to_binary(B) when is_binary(B) -> B.
