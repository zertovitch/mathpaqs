--  Author: Jon Squire
--  Original code here:
--    http://www.cs.umbc.edu/~squire/adaclass/gnatmath95/
--  Changes:
--    - Generic_Real_Arrays = Ada.Numerics.Generic_Real_Arrays (Ada 2005)
--    - removed dependency on others non-standard packages
--    - (18-Mar-2015): Integer_Vector as generic parameter.
--    - Added a few "_JS" suffixes to address ambiguities with Ada.Numerics.Generic_Real_Arrays
--    - many fixes
--    - (05-Dec-2024): Renamed function Linear_Equations as Solve_JS.

with Ada.Numerics.Generic_Real_Arrays;

generic
  type Real is digits <>;
  with package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (Real);
  type Integer_Vector is array (Integer range <>) of Integer;

package Generic_Real_Linear_Equations is

  use Real_Arrays;

  --  Suffix "_JS" was added to address ambiguity with
  --  Ada 2005's Ada.Numerics.Generic_Real_Arrays.Determinant, .Inverse, .Solve, ...

  function Solve_JS (A : Real_Matrix;
                     Y : Real_Vector) return Real_Vector;

  function Solve_JS (A : Real_Matrix;
                     Y : Real_Matrix) return Real_Matrix;

  function Determinant_JS (A : Real_Matrix) return Real;

  function Inverse_JS (A : Real_Matrix) return Real_Matrix;

  procedure Inverse_JS (A : in out Real_Matrix);

  --  !! Crout_Solve is flawed !!
  function Crout_Solve (A : Real_Matrix;
                        Y : Real_Vector) return Real_Vector;

  function Cholesky_Decomposition (A : Real_Matrix) return Real_Matrix;

  function Cholesky_Solve (L : Real_Matrix;
                           Y : Real_Vector) return Real_Vector;

  procedure LU_Decomposition (A : Real_Matrix;
                              L : in out Real_Matrix;
                              U : in out Real_Matrix;
                              P : in out Integer_Vector);

  function LU_Solve (L : Real_Matrix;
                     U : Real_Matrix;
                     P : Integer_Vector;
                     Y : Real_Vector) return Real_Vector;

  --  !! QR_Decomposition fixed, but still implemented only for square matrices !!
  procedure QR_Decomposition (A : Real_Matrix;
                              Q : in out Real_Matrix;
                              R : in out Real_Matrix);

  function QR_Solve (Q : Real_Matrix;
                     R : Real_Matrix;
                     Y : Real_Vector) return Real_Vector;

  --  !! SV_Decomposition: Caution - not tested !!
  procedure SV_Decomposition (A : Real_Matrix;
                              UU : in out Real_Matrix;
                              VV : in out Real_Matrix;
                              WW : in out Real_Vector);

  --  !! SV_Solve: Caution - not tested !!
  function SV_Solve (U : Real_Matrix;
                     V : Real_Matrix;
                     W : Real_Vector;
                     Y : Real_Vector) return Real_Vector;

  --  Matrix_Data_Error is raised for singular, non positive definate, not symmetric, etc.
  Matrix_Data_Error : exception;

end Generic_Real_Linear_Equations;
