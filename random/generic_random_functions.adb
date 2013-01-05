with Ada.Numerics;                      use Ada.Numerics;

with Ada.Numerics.Generic_Elementary_Functions;

package body Generic_Random_Functions is

  package GEF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  use GEF;

  -- Algorithm 26.2.17 <http://www.math.sfu.ca/~cbm/aands/page_932.htm>
  -- from Abromowitz and Stegun, Handbook of Mathematical Functions.
  -- It has a maximum absolute error of 7.5e-8.

  function Normal_CDF(x: Real) return Real is
    t: Real;
    b1 : constant:=  0.31938_1530;
    b2 : constant:= -0.35656_3782;
    b3 : constant:=  1.78147_7937;
    b4 : constant:= -1.82125_5978;
    b5 : constant:=  1.33027_4429;
    p  : constant:=  0.23164_19;
    c  : constant:=  0.39894228; -- = 1/Sqrt(2*pi)
  begin
    if x >= 0.0 then
      t := 1.0 / (1.0 + p * x);
      return (1.0 - c * Exp(-x * x / 2.0) * t *
      (t * (t * (t * (t * b5 + b4) + b3) + b2) + b1));
    else
      t := 1.0 / (1.0 - p * x);
      return (c * Exp(-x * x / 2.0) * t *
      (t * (t * (t * (t * b5 + b4) + b3) + b2) + b1));
    end if;
  end Normal_CDF;

  function Poisson(lambda: Real) return Natural is
    lower_limit, product: Real;
    k: Integer;
  begin
    -- Algo found in:
    -- http://en.wikipedia.org/wiki/Poisson_distribution#Generating_Poisson-distributed_random_variables
    -- referring to:
    -- Knuth (whom else ?!), The Art of Computer Programming,
    -- Volume 2, Seminumerical algorithms, 3.4.1. Numerical Distributions,
    -- F. Important integer-valued distributions.
    lower_limit:= exp(-lambda);
    k:= -1;
    product:= 1.0;
    loop
      k:= k + 1;
      product:= product * U;
      exit when product <= lower_limit;
    end loop;
    return k;
  end Poisson;

  procedure Box_Muller(u1,u2: in Real; n1,n2: out Real) is
    phi, z, r: Real;
  begin
    phi:= 2.0 * pi* u1;
    z:= -Log(u2);
    r:= Sqrt(2.0 * z);
    n1:= r * Cos(phi);
    n2:= r * Sin(phi);
  end Box_Muller;

  function Pareto_inverse_CDF(q, threshold, minus_inv_alpha: Real) return Real is
  begin
    return threshold * q ** (minus_inv_alpha);
  end Pareto_inverse_CDF;

end Generic_Random_Functions;
