%%%-------------------------------------------------------------------
%%% @author zhangr <zhangrong@moyou.me>
%%% @copyright (C) 2016, zhangr
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2016 by zhangr <zhangrong@moyou.me>
%%%-------------------------------------------------------------------
-module(lib_transformations_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("define_vector2d.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
%% -define(setup(F), {setup, fun start/0, fun stop/1, F}).

zero_test_() ->
    V1 = #vector2d{x = 0, y = 0},
    V2 = #vector2d{x = 1, y = 1},
    V3 = #vector2d{x = 0.0000001, y = 0.0000001},
    [?_assert(lib_vector2d:is_zero(V1)),
     ?_assertNot(lib_vector2d:is_zero(V2)),
     ?_assert(lib_vector2d:is_zero(V3))
    ].
