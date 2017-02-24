-ifndef(DEFINE_C2DMATRIX_HRL).
-define(DEFINE_C2DMATRIX_HRL, true).

-record(c2d_matrix, {
          i11 = 0,
          i12 = 0,
          i13 = 0,
          i21 = 0,
          i22 = 0,
          i23 = 0,
          i31 = 0,
          i32 = 0,
          i33 = 0
         }).

-define(MATRIX_IDENTITY, #c2d_matrix{
                            i11 = 1,
                            i12 = 0,
                            i13 = 0,
                            i21 = 0,
                            i22 = 1,
                            i23 = 0,
                            i31 = 0,
                            i32 = 0,
                            i33 = 1
                           }).

-endif.
