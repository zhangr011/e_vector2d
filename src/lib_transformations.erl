%%%-------------------------------------------------------------------
%%% @author 张嵘 <zhangrong@zhangrongdeMBP.lan>
%%% @copyright (C) 2017, 张嵘
%%% @doc
%%%
%%% @end
%%% Created : 23 Feb 2017 by 张嵘 <zhangrong@zhangrongdeMBP.lan>
%%%-------------------------------------------------------------------
-module(lib_transformations).

%% API
-export([to_world_space/2,
         to_world_space/3,
         transform_vector2d/2]).

-include("define_vector2d.hrl").
-include("define_c2dmatrix.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec to_world_space(Vector :: #vector2d{},
                     AgentHead :: #vector2d{} | float()) ->
                            #vector2d{}.
to_world_space(#vector2d{} = Vector, #vector2d{} = AgentHead) ->
    Mat = lib_c2d_matrix:rotate(?MATRIX_IDENTITY, AgentHead),
    transform_vector2d(Mat, Vector);
to_world_space(#vector2d{} = Vector, Angle) ->
    to_world_space(Vector, lib_vector2d:vector2d(Angle)).

-spec to_world_space(Local :: #vector2d{}, AgentHead :: #vector2d{} | float(),
                     AgentPoint :: #vector2d{}) ->
                            #vector2d{}.
to_world_space(#vector2d{} = Local, #vector2d{} = AgentHead,
               #vector2d{} = AgentPoint) ->
    Mat = lib_c2d_matrix:rotate(?MATRIX_IDENTITY, AgentHead),
    Mat2 = lib_c2d_matrix:translate(
             Mat, AgentPoint#vector2d.x, AgentPoint#vector2d.y),
    transform_vector2d(Mat2, Local);
to_world_space(#vector2d{} = Local, Angle, #vector2d{} = AgentPoint) ->
    to_world_space(Local, lib_vector2d:vector2d(Angle), AgentPoint).

-spec transform_vector2d(#c2d_matrix{}, #vector2d{} | list()) ->
                                #vector2d{} | list().
transform_vector2d(#c2d_matrix{} = Matrix, #vector2d{} = Point) ->
    #vector2d{
       x = Matrix#c2d_matrix.i11 * Point#vector2d.x +
           Matrix#c2d_matrix.i21 * Point#vector2d.y + Matrix#c2d_matrix.i31,
       y = Matrix#c2d_matrix.i12 * Point#vector2d.x +
           Matrix#c2d_matrix.i22*Point#vector2d.y + Matrix#c2d_matrix.i32
      };
transform_vector2d(#c2d_matrix{} = Matrix, List) ->
    lists:map(fun (Point) ->
                      transform_vector2d(Matrix, Point)
              end, List).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
