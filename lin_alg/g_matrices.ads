------------------------------------------------------------------------------
--  File:            g_matrices.ads
--
--  Description:     Simple generic dense matrix package.
--
--                   The scalars (Field_Element) can be of any kind
--                   (real, complex, rationals, ...) but must provide
--                   a Sqrt for the intersection with real numbers.
--
--
--  IMPORTANT:
--  =========
--             With Ada 2005+ and Field_elt = real or complex
--             (floating-point) numbers, it may be better to use
--             the package `Ada.Numerics.Generic_Real_Arrays` instead.
--
--
--  Date / Version:  3-Feb-2005 ; 16-Oct-2001 ; ... ; 22 XII 1997
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

generic

  type Field_Element is private;  --  Element of the algebraic field
  zero, one : Field_Element;                                      --  0 and 1 elements

  with function "-" (a : Field_Element) return Field_Element;     --  unary operator
  with function Sqrt (a : Field_Element) return Field_Element;

  with function "+" (a, b : Field_Element) return Field_Element;  --  binary operator
  with function "-" (a, b : Field_Element) return Field_Element;
  with function "*" (a, b : Field_Element) return Field_Element;
  with function "/" (a, b : Field_Element) return Field_Element;
  pragma Unreferenced ("/");
  --  no more "=" for Ada 83 compatibility

  --  Change 2005: Vector, Matrix added as parameters.
  --  Allows generic on generic e.g. G_Matrices in generic FEK's body.
  type Vector is array (Integer range <>) of Field_Element;
  type Matrix is array (Integer range <>, Integer range <>) of Field_Element;

package G_Matrices is

  --  Vector operations

  function "*" (l : Field_Element; v : Vector) return Vector;
  function "-" (a : Vector) return Vector;
  function "+" (a, b : Vector) return Vector;
  function "-" (a, b : Vector) return Vector;
  function "*" (a, b : Vector) return Field_Element;
  --  Euclidean norm and distance:
  function Norm (a : Vector) return Field_Element;
  function Square_Norm (a : Vector) return Field_Element;
  function Distance (a, b : Vector) return Field_Element;
  function Square_Distance (a, b : Vector) return Field_Element;

  --  Matrix operations

  function Transpose (A : Matrix) return Matrix;
  function Id (order : Positive) return Matrix;
  function "*" (l : Field_Element; A : Matrix) return Matrix;
  function "*" (A, B : Matrix) return Matrix;
  function "+" (A, B : Matrix) return Matrix;
  function "-" (A, B : Matrix) return Matrix;

  --  Matrix-Vector operations

  function "*" (A : Matrix; x : Vector) return Vector;

end G_Matrices;
