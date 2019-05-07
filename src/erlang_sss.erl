-module(erlang_sss).

%% API exports
-export([sss_create_keyshares/3, sss_combine_keyshares/2]).

-define(APPNAME, erlang_sss).
-define(LIBNAME, erlang_sss).

-on_load(init/0).

-type shares() :: [<<_:264>>, ...].
-type key() :: <<_:256>>.

%%====================================================================
%% API functions
%%====================================================================

-spec sss_create_keyshares(Data :: key(), N :: pos_integer(), K :: pos_integer()) -> shares().
sss_create_keyshares(_Data, _N, _K) ->
    not_loaded(?LINE).

-spec sss_combine_keyshares(Shares :: shares(), K :: pos_integer()) -> key().
sss_combine_keyshares(_Shares, _K) ->
    not_loaded(?LINE).

%%====================================================================
%% Internal functions
%%====================================================================
init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
