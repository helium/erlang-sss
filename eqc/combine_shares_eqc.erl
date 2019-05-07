-module(combine_shares_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_combine_shares_match/0]).

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

gen_n_k() ->
    ?SUCHTHAT({N, K},
              ?LET({X, Y},
                   ?SUCHTHAT({A, B}, {int(), int()}, A > 0 andalso B >= 0 andalso A > B),
                   {X*3, X - Y}),
              N > 3*K andalso N < 256 andalso K > 1).
