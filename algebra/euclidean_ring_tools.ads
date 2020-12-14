------------------------------------------------------------------------------
--  File:            euclidean_ring_tools.ads
--  Description:     Generic package for euclidean rings
--  Date/version:    14-Dec-2020; 22.12.1996
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

generic
  --  Ring element type: integer, polynomial, matrix, ... of any kind.
  type Ring_Element is private;

  --  Additive and multiplicative neutral elements
  --     (0 and 1, zero matrix and unit matrix, ...) :
  zero, one : Ring_Element;

  --  Binary operators:
  with function "+" (a, b : Ring_Element) return Ring_Element;
  with function "-" (a, b : Ring_Element) return Ring_Element;
  with function "*" (a, b : Ring_Element) return Ring_Element;
  with function "/" (a, b : Ring_Element) return Ring_Element;
  --
  --  "/" returns the quotient where a = b * quotient + rest.

  with function Eq (a, b : Ring_Element) return Boolean;  --  Usually, "="

  --  For integers, it's just destination := source;
  --  But sometimes it's more complicated (e.g. polynomials as unconstrained types).
  --
  --  with procedure Fill (destination : out Ring_Element; source : Ring_Element);

  type Ring_Element_Array is array (Integer range <>) of Ring_Element;

package Euclidean_Ring_Tools is

  --  Greatest Common Divisor
  procedure GCD (a, b : Ring_Element; the_gcd : out Ring_Element);

  --  Find the GCD and s, t for the
  --  ` GCD (a, b) = a * s + b * t ` factorization (Bezout theorem):
  procedure Bezout (a, b : in Ring_Element; s, t : out Ring_Element);

  procedure GCD_and_Bezout (a, b : in Ring_Element; s, t, the_gcd : out Ring_Element);

  --  Find x such as x = a_i mod n_i for all i.
  --  prod is the product of the n_i.
  --  For Ring_Element = some sort of integers,
  --  the smallest positive or zero representation of x is: (x mod prod).
  --  Not_Coprime is raised if some pair (n_i, n_j) is not co-prime,
  --  that is the GCD is not 1.
  --
  procedure Chinese_Remainder_Theorem (
    a, n    : in     Ring_Element_Array;
    x, prod :    out Ring_Element
  );

  Not_Coprime : exception;

end Euclidean_Ring_Tools;
