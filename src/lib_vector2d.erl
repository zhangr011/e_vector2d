-module(lib_vector2d).

%% API exports
-export([
         vector2d/1,
         is_zero/1,
         equal/2,
         plus/2,
         plus/3,
         neg/1,
         dot/2,
         multiply/2,
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

-spec vector2d(float()) ->
                      #vector2d{}.
vector2d(Angle) ->
    Ra = math:pi() * Angle / 180,
    #vector2d{
       x = math:cos(Ra),
       y = math:sin(Ra)
      }.

-spec is_zero(#vector2d{}) ->
                     true | false.
is_zero(#vector2d{x = X, y = Y}) ->
    X * X + Y * Y < ?MIN_POS_FLOAT.

-spec equal(#vector2d{}, #vector2d{}) ->
                   true | false.
equal(#vector2d{x = X1, y = Y1}, #vector2d{x = X2, y = Y2}) ->
    Dx = X1 - X2,
    Dy = Y1 - Y2,
    Dx * Dx + Dy * Dy < ?MIN_POS_FLOAT.

-spec plus(#vector2d{}, #vector2d{}) ->
                  #vector2d{}.
plus(#vector2d{x = X1, y = Y1}, #vector2d{x = X2, y = Y2}) ->
    #vector2d{x = X1 + X2, y = Y1 + Y2}.

-spec plus(#vector2d{}, Heading :: #vector2d{}, Length :: float()) ->
                  #vector2d{}.
plus(#vector2d{} = Vector, #vector2d{} = Heading, Length) ->
    plus(Vector, multiply(Heading, Length)).

-spec neg(#vector2d{}) ->
                 #vector2d{}.
neg(#vector2d{x = X, y = Y}) ->
    #vector2d{x = -X, y = -Y}.

-spec dot(#vector2d{}, #vector2d{}) ->
                 float().
dot(#vector2d{x = X1, y = Y1}, #vector2d{x = X2, y = Y2}) ->
    X1 * X2 + Y1 * Y2.

-spec multiply(#vector2d{}, float()) ->
                      #vector2d{}.
multiply(#vector2d{x = X, y = Y}, Length) ->
    #vector2d{
       x = X * Length,
       y = Y * Length
      }.

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
