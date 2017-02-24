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
                                 ?VECTOR_NEG60, ?VECTOR_30, ?VECTOR_30)
    ].

inner_vector_transform_test(VTo, VLocal, VHead) ->
    VWorld = lib_transformations:to_world_space(VLocal, VHead),
    ?_assert(lib_vector2d:is_equal(VTo, VWorld)).

inner_vector_transform_test(VTo, VLocal, VHead, VPoint) ->
    VWorld = lib_transformations:to_world_space(VLocal, VHead, VPoint),
    ?_assert(lib_vector2d:is_equal(VTo, VWorld)).
