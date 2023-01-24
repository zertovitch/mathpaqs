--  Draft of a specification for an universal matrix package.

--  The elements of the vectors and matrices can be of any algebraic ring
--  and their implementation, possibly approximate:
--
--    - integers (stored as Integer, Integer_n or Big_Integer)
--    - modular integers
--    - real numbers (approximated as floating-point, fixed-point or Big_Real)
--    - complex numbers
--    - rational numbers
--    - polynomials (of real, complex, rational, ...)
--    - other matrices
--    - ...

with Ada.Containers.Vectors;
--   ^ Temporary, used for checking the specification;
--     we will build a Vector container type more appropriate for
--     the purpose: we need a direct access to the data array.
--     Only the Set_Length, Length, Is_Empty, Element and
--     Replace_Element methods are kept from Ada.Containers.Vectors.

with Ada.Finalization;

generic

  type Ring_Element is private;  --  Element of an algebraic ring

  zero, one : Ring_Element;                                     --  0 and 1 elements

  with function "-" (a : Ring_Element) return Ring_Element;     --  Unary operators

  with function "+" (a, b : Ring_Element) return Ring_Element;  --  Binary operators
  with function "-" (a, b : Ring_Element) return Ring_Element;
  with function "*" (a, b : Ring_Element) return Ring_Element;

package Universal_Matrices is

  package UM_Vectors is new Ada.Containers.Vectors (Positive, Ring_Element);

  subtype Vector is UM_Vectors.Vector;

  -------------------------
  --  Vector operations  --
  -------------------------

  function "*" (v : Vector; factor : Ring_Element) return Vector;
  function "*" (factor : Ring_Element; v : Vector) return Vector;

  --  v := factor * v :
  procedure Scale_Left (v : in out Vector; factor : Ring_Element);

  function "-" (v : Vector) return Vector;

  function "+" (v, w : Vector) return Vector;
  --  v := v + w :
  procedure Add (v : in out Vector; w : Vector);
  --  v := v + factor * w :
  procedure Add_Left_Scaled (v : in out Vector; factor : Ring_Element; w : Vector);

  function "-" (v, w : Vector) return Vector;
  --  v := v - w :
  procedure Subtract (v : in out Vector; w : Vector);

  function "*" (v, w : Vector) return Ring_Element;

  --  Euclidean norm and distance:
  function Square_L2_Norm (v : Vector) return Ring_Element;
  function Square_L2_Distance (v, w : Vector) return Ring_Element;

  -------------------------------------------------------------------
  --  Root matrix type.                                            --
  --  Possible derivations: dense, sparse, band storage matrices.  --
  -------------------------------------------------------------------

  type Matrix is abstract new Ada.Finalization.Controlled with null record;

  -------------------------
  --  Matrix operations  --
  -------------------------

  function Transpose (A : Matrix) return Matrix is abstract;
  function Identity (order : Positive) return Matrix is abstract;
  function "*" (factor : Ring_Element; A : Matrix) return Matrix is abstract;
  function "*" (A : Matrix; factor : Ring_Element) return Matrix is abstract;
  function "*" (A, B : Matrix) return Matrix is abstract;
  function "+" (A, B : Matrix) return Matrix is abstract;
  function "-" (A, B : Matrix) return Matrix is abstract;

  --  Matrix-Vector operations

  function "*" (A : Matrix; x : Vector) return Vector is abstract;

end Universal_Matrices;
