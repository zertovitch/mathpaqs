-- Author: Jon Squire
-- Original code here:
--   http://www.cs.umbc.edu/~squire/adaclass/gnatmath95/
-- Changes:
--   - Generic_Real_Arrays = Ada.Numerics.Generic_Real_Arrays (Ada 2005)
--   - removed dependency on others non-standard packages
--   - (18-Mar-2015): Integer_Vector as generic parameter. Added a few "_JS"
--       suffixes to address ambiguities with Ada.Numerics.Generic_Real_Arrays
--   - many fixes

with Ada.Numerics.Generic_Real_Arrays;

generic
  type Real is digits <> ;
  with package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays(Real);
  type Integer_Vector is array (Integer range <>) of Integer;

package Generic_Real_Linear_Equations is

  use Real_Arrays;

  function Linear_Equations ( A : REAL_MATRIX ;
                              Y : REAL_VECTOR ) return REAL_VECTOR ;

  function Linear_Equations ( A : REAL_MATRIX ;
                              Y : REAL_MATRIX ) return REAL_MATRIX ;

  function Determinant_JS ( A : REAL_MATRIX ) return REAL ;
  -- Added "_JS" to address ambiguity with
  -- Ada.Numerics.Generic_Real_Arrays.Determinant

  function Inverse_JS ( A : REAL_MATRIX ) return REAL_MATRIX ;
  -- Added "_JS" to address ambiguity with
  -- Ada.Numerics.Generic_Real_Arrays.Inverse

  procedure Inverse_JS ( A : in out REAL_MATRIX ) ;

  function Crout_Solve ( A : REAL_MATRIX ;
                         Y : REAL_VECTOR ) return REAL_VECTOR ;
  -- !! Flawed !!

  function Cholesky_Decomposition ( A : REAL_MATRIX) return REAL_MATRIX ;

  function Cholesky_Solve ( L : REAL_MATRIX ;
                            Y : REAL_VECTOR ) return REAL_VECTOR ;

  procedure LU_Decomposition ( A : REAL_MATRIX ;
                               L : in out REAL_MATRIX ;
                               U : in out REAL_MATRIX ;
                               P : in out INTEGER_VECTOR) ;

  function LU_Solve ( L : REAL_MATRIX ;
                      U : REAL_MATRIX ;
                      P : INTEGER_VECTOR ;
                      Y : REAL_VECTOR ) return REAL_VECTOR ;

  procedure QR_Decomposition ( A : REAL_MATRIX ;
                               Q : in out REAL_MATRIX ;
                               R : in out REAL_MATRIX ) ;
  -- !! Fixed, but still implemented only for square matrices !!

  function QR_Solve ( Q : REAL_MATRIX ;
                      R : REAL_MATRIX ;
                      Y : REAL_VECTOR ) return REAL_VECTOR ;

  procedure SV_Decomposition ( A : REAL_MATRIX ;
                               UU : in out REAL_MATRIX ;
                               VV : in out REAL_MATRIX ;
                               WW : in out REAL_VECTOR ) ;
  -- !! Caution - not tested !!

  function SV_SOLVE ( U : REAL_MATRIX ;
                      V : REAL_MATRIX ;
                      W : REAL_VECTOR ;
                      Y : REAL_VECTOR ) return REAL_VECTOR ;
  -- !! Caution - not tested !!

  Matrix_Data_Error : exception; -- raised for singular, non positive definate,
                                 -- not symmetric, etc.

end Generic_Real_Linear_Equations ;
