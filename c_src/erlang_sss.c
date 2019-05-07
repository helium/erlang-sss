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
#include <string.h>

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

    // create n buffers for keyshares
    unsigned char keyshares[n][sss_KEYSHARE_LEN];

    // split the key into the N shares
    sss_create_keyshares(keyshares, key.data, n, k);

    // allocate n ErlNifBinaries to hold the shares
    ERL_NIF_TERM share_list = enif_make_list(env, 0);
    for(int i=0; i < n; i++) {
        ERL_NIF_TERM share_bin = {0};
        unsigned char * share_data = enif_make_new_binary(env, sss_KEYSHARE_LEN, &share_bin);
        memcpy(share_data, keyshares[i], sss_KEYSHARE_LEN);
        share_list = enif_make_list_cell(env, share_bin, share_list);
    }

    // return the list of binaries
    return share_list;
}

static ERL_NIF_TERM
erl_sss_combine_keyshares(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int k = 0;
    unsigned int n = 0;
    // check that argv[0] is a list of 33 byte binaries
    if (argc != 2 || !enif_get_int(env, argv[1], &k) || k < 1 || !enif_get_list_length(env, argv[0], &n) || n < 1 || n < (unsigned int)k) {
        return enif_make_badarg(env);
    }

    unsigned char keyshares[n][sss_KEYSHARE_LEN];
    ERL_NIF_TERM head, tail = argv[0];
    int i = 0;
    while (enif_get_list_cell(env, tail, &head, &tail)) {
        ErlNifBinary share;
        if (!enif_inspect_binary(env, head, &share) || share.size != sss_KEYSHARE_LEN) {
            return enif_make_badarg(env);
        } else {
            memcpy(keyshares[i], share.data, sss_KEYSHARE_LEN);
            i++;
        }
    }

    // allocate a 32 byte binary to hold the key
    ERL_NIF_TERM key = {0};
    unsigned char* key_data = enif_make_new_binary(env, 32, &key);

    // combine the shares
    sss_combine_keyshares(key_data, keyshares, k);

    // return the reassembled key
    return key;
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
    (void)env;

    return 0;
}

ERL_NIF_INIT(erlang_sss, nif_funcs, load, NULL, NULL, NULL);

