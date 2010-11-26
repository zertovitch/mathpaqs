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

  procedure Box_Muller(u1,u2: in Real; n1,n2: out Real) is
    phi, z, r: Real;
  begin
    phi:= 2.0 * pi* u1;
    z:= -Log(u2);
    r:= Sqrt(2.0 * z);
    n1:= r * Cos(phi);
    n2:= r * Sin(phi);
  end Box_Muller;

end Generic_Random_Functions;