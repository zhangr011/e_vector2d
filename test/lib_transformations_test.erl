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

to_world_space_test_() ->
    [%% test vector transform
     inner_vector_transform_test(?VECTOR_45, ?VECTOR_45, ?VECTOR_0),
     inner_vector_transform_test(?VECTOR_90, ?VECTOR_45, ?VECTOR_45),
     inner_vector_transform_test(?VECTOR_0, ?VECTOR_NEG45, ?VECTOR_45),
     inner_vector_transform_test(?VECTOR_0, ?VECTOR_45, ?VECTOR_NEG45),
     inner_vector_transform_test(?VECTOR_NEG90, ?VECTOR_NEG45, ?VECTOR_NEG45),
     inner_vector_transform_test(?VECTOR_60, ?VECTOR_30, ?VECTOR_30),
     inner_vector_transform_test(?VECTOR_0, ?VECTOR_NEG30, ?VECTOR_30),
     inner_vector_transform_test(?VECTOR_90, ?VECTOR_60, ?VECTOR_30),
     inner_vector_transform_test(?VECTOR_NEG30, ?VECTOR_NEG60, ?VECTOR_30),

     %% test point transform
     inner_vector_transform_test(lib_vector2d:plus(?VECTOR_45, ?VECTOR_0),
                                 ?VECTOR_45, ?VECTOR_0, ?VECTOR_0),
     inner_vector_transform_test(lib_vector2d:plus(?VECTOR_60, ?VECTOR_45),
                                 ?VECTOR_30, ?VECTOR_30, ?VECTOR_45),
     inner_vector_transform_test(lib_vector2d:plus(?VECTOR_NEG30, ?VECTOR_30),
                                 ?VECTOR_NEG60, ?VECTOR_30, ?VECTOR_30),
     %% test angle
     inner_vector_transform_test(?VECTOR_45, ?VECTOR_45, 0),
     inner_vector_transform_test(?VECTOR_30, ?VECTOR_0, 30),
     inner_vector_transform_test(?VECTOR_60, ?VECTOR_30, 30),
     inner_vector_transform_test(?VECTOR_30, ?VECTOR_60, -30),
     inner_vector_transform_test(?VECTOR_NEG30, ?VECTOR_30, -60)
    ].

to_local_space_test_() ->
    [inner_local_vector_transform_test(?VECTOR_0, ?VECTOR_45, ?VECTOR_45),
     inner_local_vector_transform_test(?VECTOR_NEG30, ?VECTOR_60, ?VECTOR_90),
     inner_local_vector_transform_test(?VECTOR_45, ?VECTOR_90, ?VECTOR_45),
     inner_local_vector_transform_test(?VECTOR_NEG45, ?VECTOR_45, ?VECTOR_90),
     inner_local_vector_transform_test(?VECTOR_0, ?VECTOR_NEG60, ?VECTOR_NEG60),
     inner_local_vector_transform_test(?VECTOR_30, ?VECTOR_NEG60, ?VECTOR_NEG90),
     inner_local_vector_transform_test(#vector2d{}, ?VECTOR_0, ?VECTOR_0, ?VECTOR_0),
     inner_local_vector_transform_test(#vector2d{x = -1}, #vector2d{}, ?VECTOR_0, ?VECTOR_0),
     inner_local_vector_transform_test(?VECTOR_0, ?VECTOR_0, ?VECTOR_0, #vector2d{}),
     inner_local_vector_transform_test(#vector2d{
                                          x = 10 * math:sqrt(2),
                                          y = -10 * math:sqrt(2)
                                         }, #vector2d{
                                               x = 30,
                                               y = 10
                                              }, ?VECTOR_45, #vector2d{
                                                                x = 10,
                                                                y = 10
                                                               }),
     inner_local_vector_transform_test(#vector2d{
                                          x = 10 + 5 * math:sqrt(3),
                                          y = 10 * math:sqrt(3) - 5
                                         }, #vector2d{
                                               x = 20,
                                               y = 30
                                              }, ?VECTOR_30, #vector2d{
                                                                x = 10,
                                                                y = 10
                                                               }),
     inner_local_vector_transform_test(#vector2d{
                                          x = 7.5 + 10 * math:sqrt(3),
                                          y = 10 - 7.5 * math:sqrt(3)
                                         }, #vector2d{
                                               x = 30,
                                               y = 30
                                              }, ?VECTOR_60, #vector2d{
                                                                x = 15,
                                                                y = 10
                                                               })
    ].

inner_vector_transform_test(VTo, VLocal, VHead) ->
    VWorld = lib_transformations:to_world_space(VLocal, VHead),
    ?_assert(lib_vector2d:equal(VTo, VWorld)).

inner_vector_transform_test(VTo, VLocal, VHead, VPoint) ->
    VWorld = lib_transformations:to_world_space(VLocal, VHead, VPoint),
    ?_assert(lib_vector2d:equal(VTo, VWorld)).

inner_local_vector_transform_test(VTo, VWorld, Head, Point) ->
    VLocal = lib_transformations:to_local_space(VWorld, Head, Point),
    %% ?debugFmt("~p, ~p", [VLocal, VTo]),
    ?_assert(lib_vector2d:equal(VTo, VLocal)).

inner_local_vector_transform_test(VTo, VWorld, Head) ->
    VLocal = lib_transformations:to_local_space(VWorld, Head),
    ?_assert(lib_vector2d:equal(VTo, VLocal)).
