/*
 * Copyright 2018 Helium Systems Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "erl_nif.h"
#include "hazmat.h"

static ERL_NIF_TERM
erl_sss_create_keyshares(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int n = -1,k = -1;
    ErlNifBinary key;
    // check we got the right arguments
    if (argc != 3  || !enif_inspect_binary(env, argv[0], &key) ||
            !enif_get_int(env, argv[1], &n) ||
            !enif_get_int(env, argv[2], &k)) {
        return enif_make_badarg(env);
    }

    // check n and k are sane
    if (n < 2 || k < 1 || k > n) {
        return enif_make_badarg(env);
    }

    // check the binary is 32 bytes
    if (key.size != 32) {
        return enif_make_badarg(env);
    }

    // allocate n ErlNifBinaries to hold the shares

    // split the key into the N shares

    // return the list of binaries
}

static ERL_NIF_TERM
erl_sss_combine_keyshares(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    // check that argv[0] is a list of 33 byte binaries
    // check K (argv[1]) is reasonable (same length as list of binaries) XXX maybe K is just the length of the list??

    // allocate a 32 byte binary to hold the key

    // combine the shares

    // return the reassembled key
}


static ErlNifFunc nif_funcs[] =
    {{"sss_create_keyshares", 3, erl_sss_create_keyshares, 0},
     {"sss_combine_keyshares", 2, erl_sss_combine_keyshares, 0}};

#define ATOM(Id, Value)                                                        \
    {                                                                          \
        Id = enif_make_atom(env, Value);                                       \
    }

static int
load(ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;

    return 0;
}

ERL_NIF_INIT(erlang_sss, nif_funcs, load, NULL, NULL, NULL);

