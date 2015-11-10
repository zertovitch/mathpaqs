-- Translated on 10-Nov-2015 by (New) P2Ada v. 28-Oct-2009

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics; use Ada.Numerics;

package body Gamma_function is

  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  use REF;

  --  This uses Stirling's approximation of n!
  --
  function GammaStirF(x : Real) return Real is
    y, w, v: Real;
    Stir: Real;
  begin
    w := 1.0 / x;
    Stir :=  7.87311395793093628397E-4;
    Stir := -2.29549961613378126380E-4 + w*Stir;
    Stir := -2.68132617805781232825E-3 + w*Stir;
    Stir :=  3.47222221605458667310E-3 + w*Stir;
    Stir :=  8.33333333333482257126E-2 + w*Stir;
    w := 1.0 + w*Stir;
    y := Exp(x);
    if x > 143.01608 then
      v := x ** (0.5*x-0.25);
      y := v*(v/y);
    else
      y := x ** (x-0.5) / y;
    end if;
    return 2.50662827463100050242*y*w;
  end GammaStirF;

  subtype Integer_for_Gamma is Integer;

  function Almost_zero (x: Real) return Boolean is
  begin
    return abs x <= Real'Base'Model_Small;
  end Almost_zero;

  --  ************************************************************************
  --  Gamma function
  --
  --  Input parameters:
  --   x   -   argument
  --
  --  Domain:
  --   0 < x < 171.6
  --   -170 < x < 0, x is not an integer.
  --
  --  Relative error:
  --  arithmetic   domain     # trials      peak         rms
  --   IEEE    -170, -33     20000       2.3e-15     3.3e-16
  --   IEEE     -33,  33     20000       9.4e-16     2.2e-16
  --   IEEE      33, 171.6   20000       2.3e-15     3.2e-16
  --
  --  Cephes Math Library Release 2.8:  June, 2000
  --  Original copyright 1984, 1987, 1989, 1992, 2000 by Stephen L. Moshier
  --  Translated to AlgoPascal by Bochkanov Sergey (2005, 2006, 2007).
  --  Translated on 10-Nov-2015 by (New) P2Ada v. 28-Oct-2009
  --  ************************************************************************

  function Gamma(x : Real) return Real is
    xv: Real:= x;
    p, PP, q, QQ, z, SgnGam: Real;
    i: Integer_for_Gamma;
  begin
    SgnGam := 1.0;
    q := abs(xv);
    if q > 33.0 then
      if xv < 0.0 then
        p := Real'Floor(q);
        i := Integer_for_Gamma(p);
        if i rem 2 = 0 then
          SgnGam := -1.0;
        end if;
        z := q - p;
        if z > 0.5 then
          p := p + 1.0;
          z := q-p;
        end if;
        z := q*Sin(Pi*z);
        z := abs(z);
        z := Pi/(z*GammaStirF(q));
      else
        z := GammaStirF(xv);
      end if;
      return SgnGam*z;
    end if;
    z := 1.0;
    while xv >= 3.0 loop
      xv := xv - 1.0;
      z := z * xv;
    end loop;
    while xv < 0.0 loop
      if  xv > -0.000000001 then
        return z/((1.0 + 0.5772156649015329*xv)*xv);
      end if;
      z := z / xv;
      xv := xv + 1.0;
    end loop;
    while xv < 2.0 loop
      if xv < 0.000000001 then
        return z / ((1.0 + 0.5772156649015329*xv)*xv);
      end if;
      z := z / xv;
      xv := xv + 1.0;
    end loop;
    if Almost_zero(xv - 2.0) then
        return z;
    end if;
    xv := xv - 2.0;
    PP :=  1.60119522476751861407E-4;
    PP :=  1.19135147006586384913E-3 + xv*PP;
    PP :=  1.04213797561761569935E-2 + xv*PP;
    PP :=  4.76367800457137231464E-2 + xv*PP;
    PP :=  2.07448227648435975150E-1 + xv*PP;
    PP :=  4.94214826801497100753E-1 + xv*PP;
    PP :=  9.99999999999999996796E-1 + xv*PP;
    QQ := -2.31581873324120129819E-5;  
    QQ :=  5.39605580493303397842E-4 + xv*QQ;
    QQ := -4.45641913851797240494E-3 + xv*QQ;
    QQ :=  1.18139785222060435552E-2 + xv*QQ;
    QQ :=  3.58236398605498653373E-2 + xv*QQ;
    QQ := -2.34591795718243348568E-1 + xv*QQ;
    QQ :=  7.14304917030273074085E-2 + xv*QQ;
    QQ :=  1.00000000000000000320    + xv*QQ;
    return z*PP/QQ;
  end Gamma;

  --  ************************************************************************
  --  Natural logarithm of gamma function
  --
  --  Input parameters:
  --   x       -   argument
  --
  --  Result:
  --   logarithm of the absolute value of the Gamma(x).
  --
  --
  --  ACCURACY:
  --  arithmetic      domain        # trials     peak         rms
  --  IEEE    0, 3                 28000     5.4e-16     1.1e-16
  --  IEEE    2.718, 2.556e305     40000     3.5e-16     8.3e-17
  --  The error criterion was relative when the function magnitude
  --  was greater than one but absolute when it was less than one.
  --
  --  The following test used the relative error criterion, though
  --  at certain points the relative error could be much higher than
  --  indicated.
  --  IEEE    -200, -4             10000     4.8e-16     1.3e-16
  --
  --  Cephes Math Library Release 2.8:  June, 2000
  --  Copyright 1984, 1987, 1989, 1992, 2000 by Stephen L. Moshier
  --  Translated to AlgoPascal by Bochkanov Sergey (2005, 2006, 2007).
  --  Translated on 10-Nov-2015 by (New) P2Ada v. 28-Oct-2009
  --  ************************************************************************

function Log_Gamma(x : Real) return Real is
  xv: Real:= x;
  A, B, C: Real;
  p, q: Real;
  u, w, z: Real;
  -- i: Integer_for_Gamma;
  LogPi: constant := 1.14472988584940017414;
  LS2PI: constant := 0.91893853320467274178;
  -- SgnGam : Real;
begin
    -- SgnGam := 1.0;
    if xv < -34.0 then    
      q := -xv;
      w := Log_Gamma(q);
      p := Real'Floor(q);
      -- i := Integer_for_Gamma(p);
      -- if i rem  2=0 then
      --   SgnGam := -1.0;
      -- else
      --   SgnGam := 1.0;
      -- end if;
      z := q-p;
      if z > 0.5 then        
        p := p + 1.0;
        z := p - q;
      end if;
      z := q*Sin(Pi*z);
      return LogPi-Log(z)-w;
    end if;
    if xv < 13.0 then
      z := 1.0;
      p := 0.0;
      u := xv;
      while u >= 3.0 loop        
          p := p - 1.0;
          u := xv + p;
          z := z * u;
      end loop;
      while u < 2.0 loop
          z := z/u;
          p := p + 1.0;
          u := xv+p;
      end loop;
      if z < 0.0 then
        -- sgngam := -1.0;
        z := -z;        
      else   
        -- sgngam := 1.0;
        null;
      end if;
      if Almost_zero(u - 2.0) then
        return Log(z);
      end if;
      p := p - 2.0;
      xv := xv + p;
      B := -1378.25152569120859100;
      B := -38801.6315134637840924 + xv*B;
      B := -331612.992738871184744 + xv*B;
      B := -1162370.97492762307383 + xv*B;
      B := -1721737.00820839662146 + xv*B;
      B := -853555.664245765465627 + xv*B;
      C := 1.0;                      
      C := -351.815701436523470549 + xv*C;
      C := -17064.2106651881159223 + xv*C;
      C := -220528.590553854454839 + xv*C;
      C := -1139334.44367982507207 + xv*C;
      C := -2532523.07177582951285 + xv*C;
      C := -2018891.41433532773231 + xv*C;
      p := xv * B/C;
      return Log(z)+p;
    end if;
    q := (xv-0.5)*Log(xv)-xv + LS2PI;
    if xv > 100_000_000.0 then
      return q;
    end if;
    p := 1.0 / (xv*xv);
    if xv >= 1000.0 then    
        q := q +
         (
           (7.9365079365079365079365*0.0001*p-2.7777777777777777777778*0.001) * p + 
           0.0833333333333333333333
         ) / xv;
    else
        A :=  8.11614167470508450300*0.0001;
        A := -5.95061904284301438324*0.0001+ p*A;
        A :=  7.93650340457716943945*0.0001+ p*A;
        A := -2.77777777730099687205*0.001 + p*A;
        A :=  8.33333333333331927722*0.01  + p*A;
        q := q + A/xv;
    end if;
    return q;
  end Log_Gamma;

end Gamma_function;
