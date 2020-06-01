------------------------------------------------------------------------------
--  File:            complex_polynomial_roots.ads
--  Description:     Complex roots of polynomials
--  Date / Version:  01-Jun-2020; 11-Jan-2004; ... ; 10-Jan-2004 ; ...
--  Author:          Gautier de Montmollin and (?) : Ada package.
--  References:        - Bronstein-Semendjajev,
--                         Taschenbuch der Mathematik,
--                         Verlag Harri Deutsch, 1966
--                     - John M. Gamble, programmation of Ferrari's solution
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Complex_Types;

generic
  with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (<>);

package Complex_Polynomial_Roots is
  use Complex_Types;
  --  Complex_Types Defines the type Complex.
  --  The type Real is itself a generic formal of Generic_Complex_Types.

  -- Degree 2: Solves a x^2 + b x + c
  procedure Solve (a,b,c: Real; r1,r2: out Complex);
  procedure Solve (a,b,c: Complex; r1,r2: out Complex);

  -- Degree 3: Solves a x^3 + b x^2 + c x + d
  procedure Solve (a,b,c,d: Real; r1,r2,r3: out Complex);

  -- Degree 4: Solves a x^4 + b x^3 + c x^2 + d x + e
  procedure Solve (a,b,c,d,e: Real; r1,r2,r3,r4: out Complex);

  dominant_coefficient_a_is_zero: exception;

end Complex_Polynomial_Roots;
