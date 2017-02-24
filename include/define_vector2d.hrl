-ifndef(DEFINE_VECTOR2D_HRL).
-define(DEFINE_VECTOR2D_HRL, true).

-define(MIN_POS_FLOAT, 0.000001).

-record(vector2d, {
          x = 0,
          y = 0
         }).

-endif.
