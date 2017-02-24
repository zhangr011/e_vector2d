-ifndef(DEFINE_VECTOR2D_HRL).
-define(DEFINE_VECTOR2D_HRL, true).

-define(MIN_POS_FLOAT, 0.000001).

-record(vector2d, {
          x = 0,
          y = 0
         }).

-define(VECTOR_0, #vector2d{
                     x = 1
                    }).

-define(VECTOR_30, #vector2d{
                      x = 0.8660254037844386,
                      y = 0.5
                     }).

-define(VECTOR_NEG30, #vector2d{
                         x = 0.8660254037844386,
                         y = -0.5
                        }).

-define(VECTOR_45, #vector2d{
                      x = 0.7071067811865475,
                      y = 0.7071067811865475
                     }).

-define(VECTOR_NEG45, #vector2d{
                         x = 0.7071067811865475,
                         y = -0.7071067811865475
                        }).

-define(VECTOR_60, #vector2d{
                      x = 0.5,
                      y = 0.8660254037844386
                     }).

-define(VECTOR_NEG60, #vector2d{
                         x = 0.5,
                         y = -0.8660254037844386
                        }).

-define(VECTOR_90, #vector2d{
                      y = 1
                     }).

-define(VECTOR_NEG90, #vector2d{
                         y = -1
                        }).

-endif.
