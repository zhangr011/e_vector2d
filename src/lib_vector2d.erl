-module(lib_vector2d).

%% API exports
-export([is_zero/1,
         perp/1,
         normalize/1,
         length/1,
         length_sq/1,
         distance/2,
         distance_sq/2
        ]).

-include("define_vector2d.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec is_zero(#vector2d{}) ->
                     true | false.
is_zero(#vector2d{x = X, y = Y}) ->
    X * X + Y * Y < ?MIN_POS_FLOAT.

-spec perp(#vector2d{}) ->
                  #vector2d{}.
perp(#vector2d{x = X, y = Y}) ->
    #vector2d{
       x = -Y,
       y = X
      }.

-spec normalize(#vector2d{}) ->
                       #vector2d{}.
normalize(#vector2d{x = X, y = Y} = Vec) ->
    Length = lib_vector2d:length(Vec),
    if
        Length > ?MIN_POS_FLOAT ->
            #vector2d{
               x = X / Length,
               y = Y / Length
              };
        true ->
            Vec
    end.

-spec length(#vector2d{}) ->
                    float().
length(#vector2d{x = X, y = Y}) ->
    math:sqrt(X * X + Y * Y).

-spec length_sq(#vector2d{}) ->
                       float().
length_sq(#vector2d{x = X, y = Y}) ->
    X * X + Y * Y.

-spec distance(#vector2d{}, #vector2d{}) ->
                      float().
distance(#vector2d{x = X1, y = Y1}, #vector2d{x = X2, y = Y2}) ->
    Dx = X1 - X2,
    Dy = Y1 - Y2,
    math:sqrt(Dx * Dx + Dy * Dy).

-spec distance_sq(#vector2d{}, #vector2d{}) ->
                         float().
distance_sq(#vector2d{x = X1, y = Y1}, #vector2d{x = X2, y = Y2}) ->
    Dx = X1 - X2,
    Dy = Y1 - Y2,
    Dx * Dx + Dy * Dy.

%%====================================================================
%% Internal functions
%%====================================================================