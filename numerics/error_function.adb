--  See specification for license and credits.

with Ada.Numerics.Generic_Elementary_Functions;

package body Error_function is

  -- Ada 95 Quality and Style Guide, 7.2.7:
  -- Tests for
  --
  -- (1) absolute "equality" to 0 in storage,
  -- (2) absolute "equality" to 0 in computation,
  -- (3) relative "equality" to 0 in storage, and
  -- (4) relative "equality" to 0 in computation:
  --
  --  abs X <= Float_Type'Model_Small                      -- (1)
  --  abs X <= Float_Type'Base'Model_Small                 -- (2)
  --  abs X <= abs X * Float_Type'Model_Epsilon            -- (3)
  --  abs X <= abs X * Float_Type'Base'Model_Epsilon       -- (4)

  function Sign (x: Real) return Real is
  begin
    if x < 0.0 then
      return -1.0;
    elsif x > 0.0 then
      return +1.0;
    else
      return 0.0;
    end if;
  end Sign;

  package GEF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  use GEF;

  --  ************************************************************************
  --  Complementary error function
  --
  --  1 - erf(x) =
  --
  --                           inf.
  --                            -
  --                 2         | |          2
  --  erfc(x)  =  --------     |    exp( - t  ) dt
  --              sqrt(pi)   | |
  --                          -
  --                          x
  --
  --
  --  For small x, erfc(x) = 1 - erf(x); otherwise rational
  --  approximations are computed.
  --
  --
  --  ACCURACY:
  --
  --                    Relative error:
  --  arithmetic   domain     # trials      peak         rms
  --  IEEE        0,26.6417     30000       5.7e-14      1.5e-14
  --
  --  Cephes Math Library Release 2.8:  June, 2000
  --  Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  --  ************************************************************************

  function ErfC (x : Real) return Real is
    P, Q: Real;
  begin
    if x < 0.0 then
      return 2.0 - ErfC(-x);
    end if;
    if x < 0.5 then
      return 1.0 - Erf(x);
    end if;
    if x >= 10.0 then
      return 0.0;
    end if;
    P := 0.0;
    P := 0.5641877825507397413087057563+x*P;
    P := 9.675807882987265400604202961+x*P;
    P := 77.08161730368428609781633646+x*P;
    P := 368.5196154710010637133875746+x*P;
    P := 1143.262070703886173606073338+x*P;
    P := 2320.439590251635247384768711+x*P;
    P := 2898.0293292167655611275846+x*P;
    P := 1826.3348842295112592168999+x*P;
    Q := 1.0;
    Q := 17.14980943627607849376131193+x*Q;
    Q := 137.1255960500622202878443578+x*Q;
    Q := 661.7361207107653469211984771+x*Q;
    Q := 2094.384367789539593790281779+x*Q;
    Q := 4429.612803883682726711528526+x*Q;
    Q := 6089.5424232724435504633068+x*Q;
    Q := 4958.82756472114071495438422+x*Q;
    Q := 1826.3348842295112595576438+x*Q;
    return Exp (-x*x) * P/Q;
  end ErfC;

  function Erf (x : Real) return Real is
    Xa  : constant Real := abs x;
    XSq : Real;
    S   : constant Real := Sign(x);
    P, Q: Real;
  begin
    if Xa < 0.5 then
      XSq := x*x;
      P := 0.007547728033418631287834;
      P := 0.288805137207594084924010+XSq*P;
      P := 14.3383842191748205576712+XSq*P;
      P := 38.0140318123903008244444+XSq*P;
      P := 3017.82788536507577809226+XSq*P;
      P := 7404.07142710151470082064+XSq*P;
      P := 80437.3630960840172832162+XSq*P;
      Q := 0.0;
      Q := 1.00000000000000000000000+XSq*Q;
      Q := 38.0190713951939403753468+XSq*Q;
      Q := 658.070155459240506326937+XSq*Q;
      Q := 6379.60017324428279487120+XSq*Q;
      Q := 34216.5257924628539769006+XSq*Q;
      Q := 80437.3630960840172826266+XSq*Q;
      return 1.1283791670955125738961589031*x*P/Q;
    end if;
    if Xa >= 10.0 then
      return S;
    end if;
    return S * (1.0 - ErfC(Xa));
  end Erf;

end Error_function;
