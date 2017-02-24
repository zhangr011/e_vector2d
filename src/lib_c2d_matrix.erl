%%%-------------------------------------------------------------------
%%% @author 张嵘 <zhangrong@zhangrongdeMBP.lan>
%%% @copyright (C) 2017, 张嵘
%%% @doc
%%%
%%% @end
%%% Created : 23 Feb 2017 by 张嵘 <zhangrong@zhangrongdeMBP.lan>
%%%-------------------------------------------------------------------
-module(lib_c2d_matrix).

%% API
-export([
         rotate/2,
         translate/3,
         multiply/2
        ]).

-include("define_vector2d.hrl").
-include("define_c2dmatrix.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec rotate(#c2d_matrix{}, Head :: #vector2d{} | float()) ->
                    #c2d_matrix{}.
rotate(#c2d_matrix{} = Matrix, #vector2d{} = Heading) ->
    multiply(Matrix, #c2d_matrix{
                        i11 = Heading#vector2d.x,
                        i12 = Heading#vector2d.y,
                        i13 = 0,
                        i21 = -Heading#vector2d.y,
                        i22 = Heading#vector2d.x,
                        i23 = 0,
                        i31 = 0,
                        i32 = 0,
                        i33 = 1
                       });
rotate(#c2d_matrix{} = Matrix, Angle) ->
    Cos = math:cos(Angle),
    Sin = math:sin(Angle),
    multiply(Matrix, #c2d_matrix{
                        i11 = Cos,
                        i12 = Sin,
                        i13 = 0,
                        i21 = -Sin,
                        i22 = Cos,
                        i23 = 0,
                        i31 = 0,
                        i32 = 0,
                        i33 = 1
                       }).

-spec translate(#c2d_matrix{}, float(), float()) ->
                       #c2d_matrix{}.
translate(#c2d_matrix{} = Matrix, X, Y) ->
    multiply(Matrix, #c2d_matrix{
                        i11 = 1,
                        i12 = 0,
                        i13 = 0,
                        i21 = 0,
                        i22 = 1,
                        i23 = 0,
                        i31 = X,
                        i32 = Y,
                        i33 = 1
                       }).

-spec multiply(#c2d_matrix{}, #c2d_matrix{}) ->
                      #c2d_matrix{}.
multiply(#c2d_matrix{} = Matrix, #c2d_matrix{} = Target) ->
    Matrix#c2d_matrix{
      %% first
      i11 = Matrix#c2d_matrix.i11 * Target#c2d_matrix.i11 +
          Matrix#c2d_matrix.i12 * Target#c2d_matrix.i21 +
          Matrix#c2d_matrix.i13 * Target#c2d_matrix.i31,
      i12 = Matrix#c2d_matrix.i11 * Target#c2d_matrix.i12 +
          Matrix#c2d_matrix.i12 * Target#c2d_matrix.i22 +
          Matrix#c2d_matrix.i13 * Target#c2d_matrix.i32,
      i13 = Matrix#c2d_matrix.i11 * Target#c2d_matrix.i13 +
          Matrix#c2d_matrix.i12 * Target#c2d_matrix.i23 +
          Matrix#c2d_matrix.i13 * Target#c2d_matrix.i33,
      %% second
      i21 = Matrix#c2d_matrix.i21 * Target#c2d_matrix.i11 +
          Matrix#c2d_matrix.i22 * Target#c2d_matrix.i21 +
          Matrix#c2d_matrix.i23 * Target#c2d_matrix.i31,
      i22 = Matrix#c2d_matrix.i21 * Target#c2d_matrix.i12 +
          Matrix#c2d_matrix.i22 * Target#c2d_matrix.i22 +
          Matrix#c2d_matrix.i23 * Target#c2d_matrix.i32,
      i23 = Matrix#c2d_matrix.i21 * Target#c2d_matrix.i13 +
          Matrix#c2d_matrix.i22 * Target#c2d_matrix.i23 +
          Matrix#c2d_matrix.i23 * Target#c2d_matrix.i33,
      %% third
      i31 = Matrix#c2d_matrix.i31 * Target#c2d_matrix.i11 +
          Matrix#c2d_matrix.i32 * Target#c2d_matrix.i21 +
          Matrix#c2d_matrix.i33 * Target#c2d_matrix.i31,
      i32 = Matrix#c2d_matrix.i31 * Target#c2d_matrix.i12 +
          Matrix#c2d_matrix.i32 * Target#c2d_matrix.i22 +
          Matrix#c2d_matrix.i33 * Target#c2d_matrix.i32,
      i33 = Matrix#c2d_matrix.i31 * Target#c2d_matrix.i13 +
          Matrix#c2d_matrix.i32 * Target#c2d_matrix.i23 +
          Matrix#c2d_matrix.i33 * Target#c2d_matrix.i33
     }.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
