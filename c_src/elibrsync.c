#include <stdio.h>

#include "erl_nif.h"
#include "librsync.h"

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;

static ERL_NIF_TERM  make_ret_term(ErlNifEnv* env, rs_result result) {
    char const *result_str;
    ERL_NIF_TERM ret_term;

    result_str = rs_strerror(result);

    if(result == RS_DONE) {
        ret_term = enif_make_tuple2(env, atom_ok, 
                                    enif_make_string(env, result_str, ERL_NIF_LATIN1)); 
    } else {
        ret_term = enif_make_tuple2(env, atom_error, 
                                    enif_make_string(env, result_str, ERL_NIF_LATIN1)); 
    }    

    return ret_term;
}

static ERL_NIF_TERM iterate_rs_job(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifUInt64 job;
    ErlNifBinary src_bin, ret_bin;
    int ret_size, eof_in;
    rs_buffers_t buffers;
    rs_result result;
    ERL_NIF_TERM ret_term;

    if(!enif_get_uint64(env, argv[0], &job)){
        return enif_make_badarg(env);
    }

    if(!enif_inspect_binary(env, argv[1], &src_bin)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int(env, argv[2], &eof_in)){
        return enif_make_badarg(env);
    }

    enif_alloc_binary(1024 + src_bin.size, &ret_bin);

    buffers.next_in = (char*)src_bin.data;
    buffers.avail_in = src_bin.size;
    buffers.eof_in = eof_in;
    buffers.next_out = (char*)ret_bin.data;
    buffers.avail_out = ret_bin.size;

    result = rs_job_iter((rs_job_t*)job, &buffers);

    if(eof_in != 0) {
        while(result == RS_BLOCKED) {
            result = rs_job_iter((rs_job_t*)job, &buffers);
        }
    }

    ret_size = ret_bin.size - buffers.avail_out;
    enif_realloc_binary(&ret_bin, ret_size);

    if(result == RS_DONE || result == RS_BLOCKED) {
        ret_term = enif_make_tuple2(env, atom_ok, enif_make_binary(env, &ret_bin));
    } else {
        ret_term = make_ret_term(env, result);
    } 
    enif_release_binary(&ret_bin);

    return ret_term;
}



static ERL_NIF_TERM signature_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char basis_file_name[1024], sig_file_name[1024];
    FILE *basis_file, *sig_file;
    int block_len;
    size_t strong_len = 0;
    rs_magic_number sig_magic = RS_BLAKE2_SIG_MAGIC;
    rs_result result;

    if(!enif_get_string(env, argv[0], basis_file_name, sizeof(basis_file_name), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    } 

    if(!enif_get_string(env, argv[1], sig_file_name, sizeof(sig_file_name), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int(env, argv[2], &block_len)) {
        return enif_make_badarg(env);
    }

    basis_file = fopen(basis_file_name, "rb");
    sig_file = fopen(sig_file_name, "wb");

    result = rs_sig_file(basis_file, sig_file, (size_t)block_len, strong_len,
                         sig_magic, NULL); 

    fclose(basis_file);
    fclose(sig_file);

    return make_ret_term(env, result);
}

static ERL_NIF_TERM delta_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char sig_file_name[1024], new_file_name[1024], delta_file_name[1024];
    FILE *sig_file, *new_file, *delta_file;
    rs_signature_t  *signature;
    rs_result result;

    if(!enif_get_string(env, argv[0], sig_file_name, sizeof(sig_file_name), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    } 

    if(!enif_get_string(env, argv[1], new_file_name, sizeof(new_file_name), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_string(env, argv[2], delta_file_name, sizeof(delta_file_name), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    sig_file = fopen(sig_file_name, "rb");
    new_file = fopen(new_file_name, "rb");
    delta_file = fopen(delta_file_name, "wb");

    result = rs_loadsig_file(sig_file, &signature, NULL);
    if (result != RS_DONE)
        return enif_make_tuple2(env, atom_error, enif_make_string(env, rs_strerror(result), ERL_NIF_LATIN1)); 

    if ((result = rs_build_hash_table(signature)) != RS_DONE)
        return enif_make_tuple2(env, atom_error, enif_make_string(env, rs_strerror(result), ERL_NIF_LATIN1));

    result = rs_delta_file(signature, new_file, delta_file, NULL); 

    fclose(sig_file);
    fclose(new_file);
    fclose(delta_file);

    return make_ret_term(env, result);

}

static ERL_NIF_TERM patch_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char basis_file_name[1024], delta_file_name[1024], new_file_name[1024];
    FILE *basis_file, *delta_file, *new_file;
    rs_result result;

    if(!enif_get_string(env, argv[0], basis_file_name, sizeof(basis_file_name), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    } 

    if(!enif_get_string(env, argv[1], delta_file_name, sizeof(delta_file_name), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_string(env, argv[2], new_file_name, sizeof(new_file_name), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    basis_file = fopen(basis_file_name, "rb");
    delta_file = fopen(delta_file_name, "rb");
    new_file = fopen(new_file_name, "wb");

    result = rs_patch_file(basis_file, delta_file, new_file, NULL);

    fclose(basis_file);
    fclose(delta_file);
    fclose(new_file);

    return make_ret_term(env, result);
}


static ERL_NIF_TERM signature_begin_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int new_block_len;
    size_t strong_sum_len = 0;
    rs_magic_number sig_magic = RS_BLAKE2_SIG_MAGIC;

    if(!enif_get_int(env, argv[0], &new_block_len)){
        return enif_make_badarg(env);
    }

    rs_job_t *job = rs_sig_begin((size_t)new_block_len, strong_sum_len, sig_magic);

    return enif_make_tuple2(env, atom_ok, enif_make_uint64(env, (ErlNifUInt64)job));
}

static ERL_NIF_TERM signature_iterate_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return iterate_rs_job(env, argc, argv);
}

static ERL_NIF_TERM signature_end_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifUInt64 job;
    rs_result result;

    if(!enif_get_uint64(env, argv[0], &job)){
        return enif_make_badarg(env);
    }

    result = rs_job_free((rs_job_t*)job);  

    return make_ret_term(env, result);
}

static ERL_NIF_TERM build_hash_table_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary src_bin;
    rs_job_t *job;
    rs_signature_t *signature;
    rs_buffers_t buffers;
    rs_result result;

    if(!enif_inspect_binary(env, argv[0], &src_bin)) {
        return enif_make_badarg(env);
    }

    job = rs_loadsig_begin(&signature);

    buffers.next_in = (char*)src_bin.data;
    buffers.avail_in = src_bin.size;
    buffers.eof_in = 1;
 
    result = rs_job_iter(job, &buffers);
    
    if (result != RS_DONE && result != RS_BLOCKED)
        return make_ret_term(env, result); 

    if ((result = rs_build_hash_table(signature)) != RS_DONE)
        return make_ret_term(env, result);

    return enif_make_tuple2(env, atom_ok, enif_make_uint64(env, (ErlNifUInt64)signature));
}

static ERL_NIF_TERM delta_begin_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifUInt64 signature;

    if(!enif_get_uint64(env, argv[0], &signature)){
        return enif_make_badarg(env);
    }

    rs_job_t *job = rs_delta_begin((rs_signature_t*)signature);

    return enif_make_tuple2(env, atom_ok, enif_make_uint64(env, (ErlNifUInt64)job));
}

static ERL_NIF_TERM delta_iterate_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return iterate_rs_job(env, argc, argv);
}

static ERL_NIF_TERM delta_end_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifUInt64 job;
    rs_result result;

    if(!enif_get_uint64(env, argv[0], &job)){
        return enif_make_badarg(env);
    }

    result = rs_job_free((rs_job_t*)job);  

    return make_ret_term(env, result);
}

static ErlNifFunc nif_funcs[] = {
    {"signature", 3, signature_nif},
    {"delta", 3, delta_nif},
    {"patch", 3, patch_nif},
    {"signature_begin", 1, signature_begin_nif},
    {"signature_iterate", 3, signature_iterate_nif},
    {"signature_end", 1, signature_end_nif},
    {"build_hash_table", 1, build_hash_table_nif},
    {"delta_begin", 1, delta_begin_nif},
    {"delta_iterate", 3, delta_iterate_nif},
    {"delta_end", 1, delta_end_nif}
};

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  atom_ok = enif_make_atom(env, "ok");
  atom_error = enif_make_atom(env, "error");
  return 0;
}

ERL_NIF_INIT(elibrsync, nif_funcs, &on_load, NULL, NULL, NULL);
