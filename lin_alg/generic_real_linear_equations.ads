-- Author: Jon Squire
-- Original code here:
--   http://www.cs.umbc.edu/~squire/adaclass/gnatmath95/
-- Changes:
--   - Generic_Real_Arrays = Ada.Numerics.Generic_Real_Arrays (Ada 2005)
--   - removed dependency on others non-standard packages
--   - a few fixes
--   - (18-Mar-2015): Integer_Vector as generic parameter added a few "_JS"
--       suffixes to address ambiguities with Ada.Numerics.Generic_Real_Arrays

with Ada.Numerics.Generic_Real_Arrays;

generic
  type Real is digits <> ;
  with package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays(Real);
  type Integer_Vector is array (Integer range <>) of Integer;

package Generic_Real_Linear_Equations is

  use Real_Arrays;

  function LINEAR_EQUATIONS ( A : REAL_MATRIX ;
                              Y : REAL_VECTOR ) return REAL_VECTOR ;

  function LINEAR_EQUATIONS ( A : REAL_MATRIX ;
                              Y : REAL_MATRIX ) return REAL_MATRIX ;

  function DETERMINANT_JS ( A : REAL_MATRIX ) return REAL ;
  -- Added "_JS" to address ambiguity with
  -- Ada.Numerics.Generic_Real_Arrays.Determinant

  function INVERSE_JS ( A : REAL_MATRIX ) return REAL_MATRIX ;
  -- Added "_JS" to address ambiguity with
  -- Ada.Numerics.Generic_Real_Arrays.Inverse

  procedure INVERSE ( A : in out REAL_MATRIX ) ;

  function CROUT_SOLVE ( A : REAL_MATRIX ;
                         Y : REAL_VECTOR ) return REAL_VECTOR ;

  function CHOLESKY_DECOMPOSITION ( A : REAL_MATRIX) return REAL_MATRIX ;

  function CHOLESKY_SOLVE ( L : REAL_MATRIX ;
                            Y : REAL_VECTOR ) return REAL_VECTOR ;

  procedure LU_DECOMPOSITION ( A : REAL_MATRIX ;
                               L : in out REAL_MATRIX ;
                               U : in out REAL_MATRIX ;
                               P : in out INTEGER_VECTOR) ;

  function LU_SOLVE ( L : REAL_MATRIX ;
                      U : REAL_MATRIX ;
                      P : INTEGER_VECTOR ;
                      Y : REAL_VECTOR ) return REAL_VECTOR ;

  procedure QR_DECOMPOSITION ( A : REAL_MATRIX ;
                               Q : in out REAL_MATRIX ;
                               R : in out REAL_MATRIX ) ;

  function QR_SOLVE ( Q : REAL_MATRIX ;
                      R : REAL_MATRIX ;
                      Y : REAL_VECTOR ) return REAL_VECTOR ;

  procedure SV_DECOMPOSITION ( A : REAL_MATRIX ;
                               UU : in out REAL_MATRIX ;
                               VV : in out REAL_MATRIX ;
                               WW : in out REAL_VECTOR ) ;

  function SV_SOLVE ( U : REAL_MATRIX ;
                      V : REAL_MATRIX ;
                      W : REAL_VECTOR ;
                      Y : REAL_VECTOR ) return REAL_VECTOR ;

  MATRIX_DATA_ERROR : exception; -- raised for singular, non positive definate,
                                 -- not symmetric, etc.

  ARRAY_INDEX_ERROR : exception; -- renames ARRAY_EXCEPTIONS.ARRAY_INDEX_ERROR;


end Generic_Real_Linear_Equations ;
