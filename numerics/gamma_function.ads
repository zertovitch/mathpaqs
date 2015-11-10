--  Gamma special function and Log(Gamma)
--  Author: Stephen L. Moshier

generic
  type Real is digits <>;

package Gamma_function is

  function Gamma(x : Real) return Real;
  --  Gamma function
  --
  --  Domain:
  --   0 < x < 171.6
  --   -170 < x < 0, x is not an integer.
  --  History and notes about accuracy in the implementation

  function Log_Gamma(x : Real) return Real;
  --  Natural logarithm of gamma function
  --
  --  Domain:
  --   0 < x < 2.55e305
  --   -2.55e305 < x < 0, x is not an integer.
  --  History and notes about accuracy in the implementation

end Gamma_function;
