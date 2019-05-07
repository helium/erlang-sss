-module(erlang_sss_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
         all/0,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

-export([
         normal_test/1,
         threshold_test/1,
         many_shares_test/1,
         not_enough_shares_test/1,
         too_many_secrets_test/1
        ]).

all() ->
    [
     normal_test,
     many_shares_test,
     not_enough_shares_test,
     too_many_secrets_test
    ].

init_per_testcase(_, Config) ->
    Data = crypto:strong_rand_bytes(32),
    [{data, Data} | Config].

end_per_testcase(_, _Config) ->
    ok.

normal_test(Config) ->
    %% simple unit test
    Data = proplists:get_value(data, Config),
    N = 1,
    K = 1,
    Shares = erlang_sss:sss_create_keyshares(Data, N, K),
    Restored = erlang_sss:sss_combine_keyshares(Shares, K),
    Data = Restored.

threshold_test(Config) ->
    %% a basic threshold test
    Data = proplists:get_value(data, Config),
    N = 10,
    K = 4,
    Shares = erlang_sss:sss_create_keyshares(Data, N, K),
    Restored = erlang_sss:sss_combine_keyshares(Shares, K),
    Data = Restored.

many_shares_test(Config) ->
    %% a lot of shares should also work
    Data = proplists:get_value(data, Config),
    N = 255,
    K = 255,
    Shares = erlang_sss:sss_create_keyshares(Data, N, K),
    Restored = erlang_sss:sss_combine_keyshares(Shares, K),
    Data = Restored.

not_enough_shares_test(Config) ->
    %% not enough shares to restore secret
    Data = proplists:get_value(data, Config),
    N = 100,
    K = 100,
    Shares = erlang_sss:sss_create_keyshares(Data, N, K),
    %% This should blow up
    Restored = erlang_sss:sss_combine_keyshares(Shares, K-1),
    Data = Restored.

too_many_secrets_test(Config) ->
    %% too many secret shares should also restore the secret
    Data = proplists:get_value(data, Config),
    N = 200,
    K = 100,
    Shares = erlang_sss:sss_create_keyshares(Data, N, K),
    Restored = erlang_sss:sss_combine_keyshares(Shares, K+K),
    Data = Restored.
