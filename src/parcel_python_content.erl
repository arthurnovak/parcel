-module(parcel_python_content).

%% API
-export([wrap_all/1, wrap/1]).

-spec wrap_all([parcel_content_parser:content_module()]) ->
    [{any(), iodata()}].
wrap_all(PyMods) ->
    [ {Module, wrap(PyBin)} || {Module, PyBin} <- PyMods ].

-spec wrap(iodata()) -> iodata().
wrap(Content) ->
    [
        import(),
        Content, "\n",
        init_wrapper(),
        process_wrapper()
    ].

import() ->
    "import json\n\n".

init_wrapper() ->
    "def init_wrapper():\n"
    "    return json.dumps(init())\n"
    "\n".

process_wrapper() ->
    "def process_wrapper(data, meta):\n"
    "    return json.dumps(process(json.loads(data), json.loads(meta)))\n"
    "\n".
