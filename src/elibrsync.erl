-module(elibrsync).

-export([
    signature/3,
    delta/3,
    patch/3,
    signature_begin/1,
    signature_iterate/2,
    signature_end/2
]).

-on_load(init/0).

-define(DEFAULT_BLOCK_LEN, 2048).

-define(NIF_STUB, nif_stub_error(?LINE)).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, elibrsync), 0).

signature(_BasisFileName, _SigFileName, _BlockLen) ->
    ?NIF_STUB.

delta(_SigFileName, _NewFileName, _DeltaFileName) ->
    ?NIF_STUB.  

patch(_BasisFileName, _DeltaFileName, _NewFileName) ->
    ?NIF_STUB.  

signature_begin(_BlockLen) ->
    ?NIF_STUB.

signature_iterate(Context, Data) ->
    signature_iterate(Context, Data, 0).

signature_iterate(_Context, _Data, _Eof) ->
    ?NIF_STUB.

signature_end(Context, Data) ->
    {ok, Ret} = signature_iterate(Context, Data, 1),
    {ok, _} = signature_end(Context),
    {ok, Ret}.

signature_end(_Context) ->
    ?NIF_STUB.
