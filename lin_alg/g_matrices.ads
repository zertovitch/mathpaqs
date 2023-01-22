------------------------------------------------------------------------------
--  File:            gmatrice.ads or g_matrices.ads
--
--  Description:     Simple generic matrix package.
--                   The scalars (Field_elt) can be of any kind
--                   (real, complex, rationals, ...)
--
--  IMPORTANT:
--  =========
--            With Ada 2005+ and Field_elt = real or complex (floating-point)
--            numbers, it is better, for performance and compatibility reasons,
--            to use the package  Ada.Numerics.Generic_Real_Arrays  instead.
--
--
--  Date / Version:  3-Feb-2005 ; 16-Oct-2001 ; ... ; 22 XII 1997
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

generic  --  Requires an algebraic field with a square root
  --  e.g. floating point numbers, fixed point numbers

  type Field_elt is private; --  Element of the algebraic field
  zero, one : Field_elt;                       --  0 and 1 elements

  with function "-" (a: Field_elt) return Field_elt;     --  unary operator
  with function Sqrt (a: Field_elt) return Field_elt;

  with function "+" (a, b: Field_elt) return Field_elt;  --  binary operator
  with function "-" (a, b: Field_elt) return Field_elt;
  with function "*" (a, b: Field_elt) return Field_elt;
  with function "/" (a, b: Field_elt) return Field_elt;
  pragma Unreferenced ("/");
  --  no more "=" for Ada 83 compatibility

  --  Change 2005: Vector, Matrix added as parameters.
  --  Allows generic on generic e.g. G_Matrices in generic FEK's body.
  type Vector is array (Integer range <>) of Field_elt;
  type Matrix is array (Integer range <>, Integer range <>) of Field_elt;

package G_Matrices is

  --  Vector operations

  function "*" (l: Field_elt; v: Vector) return Vector;
  function "-" (a: Vector) return Vector;
  function "+" (a, b: Vector) return Vector;
  function "-" (a, b: Vector) return Vector;
  function "*" (a, b: Vector) return Field_elt;       --  produit scalaire
  function Norm (a: Vector) return Field_elt;
  function Square_norm (a: Vector) return Field_elt;  --  norme au carr\'e
  function Distance (a, b: Vector) return Field_elt;
  function Square_dist (a, b: Vector) return Field_elt;

  --  Matrix operations

  function Transpose (A: Matrix) return Matrix;
  function Id (order: Positive) return Matrix;
  function "*" (l: Field_elt; A: Matrix) return Matrix;
  function "*" (A, B: Matrix) return Matrix;
  function "+" (A, B: Matrix) return Matrix;
  function "-" (A, B: Matrix) return Matrix;

  --  Matrix-Vector operations

  function "*" (A: Matrix; x: Vector) return Vector;

end G_Matrices;
