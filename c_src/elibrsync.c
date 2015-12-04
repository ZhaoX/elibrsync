#include "erl_nif.h"

static ERL_NIF_TERM signature_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int ret = 5;
    return enif_make_int(env, ret);
}

static ErlNifFunc nif_funcs[] = {
    {"signature", 0, signature_nif}
};

ERL_NIF_INIT(elibrsync, nif_funcs, NULL, NULL, NULL, NULL)
