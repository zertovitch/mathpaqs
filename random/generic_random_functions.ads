-- Generic_Random_Functions
--
-- Functions facilitating computations with various random distributions

generic

  type Real is digits <> ;

package Generic_Random_Functions is

  --------------------------------------------------
  -- Inverse CDF's and other simulation functions --
  --------------------------------------------------

  -- Box-Muller: returns a pair of
  -- independent N(0,1) normal variables from
  -- a pair of independent U(0,1) uniform variables
  --
  procedure Box_Muller(u1,u2: in Real; n1,n2: out Real);
  pragma inline(Box_Muller);

  -----------
  -- CDF's --
  -----------

  function Normal_CDF(x: Real) return Real;

end Generic_Random_Functions;
