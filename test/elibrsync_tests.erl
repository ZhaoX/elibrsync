-module(elibrsync_tests).

-include_lib("eunit/include/eunit.hrl").

signature_test() ->
    {ok, Job} = elibrsync:signature_begin(),
    {ok, IoDevice} = file:open("data/file", [binary]),
    {ok, Signature} = run_signature(Job, IoDevice, file:read(IoDevice, 1024), <<>>),
    {ok, RealSignature} = file:read_file("data/signature"),
    ?assertEqual(RealSignature, Signature).

run_signature(Job, _IoDevice, eof, Acc) ->
    {ok, Ret} = elibrsync:signature_end(Job, <<>>),
    {ok, <<Acc/binary, Ret/binary>>};
run_signature(Job, IoDevice, {ok, Data}, Acc) ->
    {ok, Ret} = elibrsync:signature_iterate(Job, Data), 
    run_signature(Job, IoDevice, file:read(IoDevice, 1024), <<Acc/binary, Ret/binary>>).

delta_test() ->
    {ok, Signature} = file:read_file("data/signature"),
    {ok, HashTable} = elibrsync:build_hash_table(Signature),
    {ok, Job} = elibrsync:delta_begin(HashTable),
    {ok, IoDevice} = file:open("data/new_file", [binary]),
    {ok, Delta} = run_delta(Job, IoDevice, file:read(IoDevice, 1024), <<>>),
    {ok, RealDelta} = file:read_file("data/delta"),
    {ok, _} = elibrsync:free_hash_table(HashTable),
    ?assertEqual(RealDelta, Delta).

run_delta(Job, _IoDevice, eof, Acc) ->
    {ok, Ret} = elibrsync:delta_end(Job, <<>>),
    {ok, <<Acc/binary, Ret/binary>>};
run_delta(Job, IoDevice, {ok, Data}, Acc) ->
    {ok, Ret} = elibrsync:delta_iterate(Job, Data), 
    run_delta(Job, IoDevice, file:read(IoDevice, 1024), <<Acc/binary, Ret/binary>>).

patch_test() ->
    {ok, Job} = elibrsync:patch_begin("data/file"),
    {ok, IoDevice} = file:open("data/delta", [binary]),
    {ok, NewFile} = run_patch(Job, IoDevice, file:read(IoDevice, 1024), <<>>),
    {ok, RealNewFile} = file:read_file("data/new_file"),
    ?assertEqual(RealNewFile, NewFile).

run_patch(Job, _IoDevice, eof, Acc) ->
    {ok, Ret} = elibrsync:patch_end(Job, <<>>),
    {ok, <<Acc/binary, Ret/binary>>};
run_patch(Job, IoDevice, {ok, Data}, Acc) ->
    {ok, Ret} = elibrsync:patch_iterate(Job, Data), 
    run_patch(Job, IoDevice, file:read(IoDevice, 1024), <<Acc/binary, Ret/binary>>).
