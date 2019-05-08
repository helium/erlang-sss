-module(combine_shares_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([
         prop_combine_shares_match/0,
         prop_combine_less_shares_mismatch/0,
         prop_combine_duplicate_shares_mismatch/0
        ]).

prop_combine_shares_match() ->
    ?FORALL({N, K}, gen_n_k(),
            begin
                Data = crypto:strong_rand_bytes(32),
                Shares = erlang_sss:sss_create_keyshares(Data, N, K),
                Restored = erlang_sss:sss_combine_keyshares(Shares, K),
                ?WHENFAIL(begin
                              io:format("Data ~p~n", [Data]),
                              io:format("N ~p~n", [N]),
                              io:format("K ~p~n", [K]),
                              io:format("Shares ~p~n", [Shares])
                          end,
                          conjunction([
                                       {secret_equality, eqc:equals(Data, Restored)}
                                      ]))
            end).

prop_combine_less_shares_mismatch() ->
    ?FORALL({N, K}, gen_n_k(),
            begin
                Data = crypto:strong_rand_bytes(32),
                Shares = erlang_sss:sss_create_keyshares(Data, N, K),
                %% Try to restore with one less share than expected
                %% should cause the restored data to be different than original data
                Restored = erlang_sss:sss_combine_keyshares(Shares, K-1),
                ?WHENFAIL(begin
                              io:format("Data ~p~n", [Data]),
                              io:format("N ~p~n", [N]),
                              io:format("K ~p~n", [K]),
                              io:format("Shares ~p~n", [Shares])
                          end,
                          conjunction([
                                       {secret_inequality, not (Data == Restored)}
                                      ]))
            end).

prop_combine_duplicate_shares_mismatch() ->
    ?FORALL({N, K}, gen_n_k(),
            begin
                Data = crypto:strong_rand_bytes(32),
                Shares = erlang_sss:sss_create_keyshares(Data, N, K),
                [Share1 | _] = Shares,
                DuplicateShares = [Share1 || _ <- lists:seq(1, length(Shares))],
                %% Try to restore with the same share
                %% should cause the restored data to be different than original data
                Restored = erlang_sss:sss_combine_keyshares(DuplicateShares, K),
                ?WHENFAIL(begin
                              io:format("Data ~p~n", [Data]),
                              io:format("N ~p~n", [N]),
                              io:format("K ~p~n", [K]),
                              io:format("Shares ~p~n", [Shares])
                          end,
                          conjunction([
                                       {secret_inequality, not (Data == Restored)}
                                      ]))
            end).

gen_n_k() ->
    ?SUCHTHAT({N, K},
              ?LET({X, Y},
                   ?SUCHTHAT({A, B}, {int(), int()}, A > 0 andalso B >= 0 andalso A > B),
                   {X*3, X - Y}),
              N > 3*K andalso N < 256 andalso K > 1).
