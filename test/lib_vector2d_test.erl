%%%-------------------------------------------------------------------
%%% @author zhangr <zhangrong@moyou.me>
%%% @copyright (C) 2016, zhangr
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2016 by zhangr <zhangrong@moyou.me>
%%%-------------------------------------------------------------------
-module(lib_vector2d_test).

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

normalize_test_() ->
    V1 = #vector2d{x = 1, y = 1},
    V2 = #vector2d{x = 3, y = 4},
    [?_assertEqual(#vector2d{
                      x = 0.7071067811865475,
                      y = 0.7071067811865475
                     }, lib_vector2d:normalize(V1)),
     ?_assertEqual(#vector2d{
                      x = 0.6,
                      y = 0.8
                     }, lib_vector2d:normalize(V2))].

length_test_() ->
    V1 = #vector2d{x = 1, y = 1},
    V2 = #vector2d{x = 3, y = 4},
    [?_assertEqual(1.4142135623730951, lib_vector2d:length(V1)),
     ?_assertEqual(2, lib_vector2d:length_sq(V1)),
     ?_assertEqual(5.0, lib_vector2d:length(V2)),
     ?_assertEqual(25, lib_vector2d:length_sq(V2))].


distance_test_() ->
    V1 = #vector2d{x = 1, y = 1},
    V2 = #vector2d{x = 4, y = 5},
    V3 = #vector2d{x = 2, y = 2},
    [?_assertEqual(5.0, lib_vector2d:distance(V1, V2)),
     ?_assertEqual(25, lib_vector2d:distance_sq(V1, V2)),
     ?_assertEqual(1.4142135623730951, lib_vector2d:distance(V1, V3)),
     ?_assertEqual(2, lib_vector2d:distance_sq(V1, V3))].
