#include <stdio.h>

#include "erl_nif.h"
#include "librsync.h"

//#define MD4_LEN 16

//*******************************************************************************
//Function rs_mdfour_file exists in librsync.h, but it has no implementation.
//*******************************************************************************
//static ERL_NIF_TERM md4_file_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
//    char file_name[4096];
//    FILE *file;
//    ERL_NIF_TERM ret;
//
//    if(!enif_get_string(env, argv[0], file_name, sizeof(file_name), ERL_NIF_LATIN1)) {
//        return enif_make_badarg(env);
//    } 
//
//    file = fopen(file_name, "r");
//    rs_mdfour_file(file, (char*)enif_make_new_binary(env, MD4_LEN, &ret));
//  
//    return ret;
//}

static ERL_NIF_TERM signature_begin_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int new_block_len;
    size_t strong_sum_len = 0;
    rs_magic_number sig_magic = RS_BLAKE2_SIG_MAGIC;

    if(!enif_get_int(env, argv[0], &new_block_len)){
        return enif_make_badarg(env);
    }

    rs_job_t *job = rs_sig_begin((size_t)new_block_len, strong_sum_len, sig_magic);

    return enif_make_long(env, (long)job);
}

static ErlNifFunc nif_funcs[] = {
//    {"md4_file", 0, md4_file_nif},
    {"signature_begin", 1, signature_begin_nif}
};

ERL_NIF_INIT(elibrsync, nif_funcs, NULL, NULL, NULL, NULL)
