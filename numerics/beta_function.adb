-- Translated on 9-Mar-2018 by (New) P2Ada v. 28-Oct-2009

with Ada.Numerics.Generic_Elementary_Functions;

with Gamma_function, Phi_function;

package body Beta_function is

  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  package RPhF is new Phi_function (Real);
  package RGF is new Gamma_function(Real);

  use REF, RGF;

  --  ************************************************************************
  --  Beta function
  --
  --
  --                    -     -
  --                   | (a) | (b)
  --  beta( a, b )  =  -----------.
  --                     -
  --                    | (a+b)
  --
  --  For large arguments the logarithm of the function is
  --  evaluated using lgam(), then exponentiated.
  --
  --  ACCURACY:
  --
  --                    Relative error:
  --  arithmetic   domain     # trials      peak         rms
  --  IEEE       0,30       30000       8.1e-14     1.1e-14
  --
  --  Cephes Math Library Release 2.0:  April, 1987
  --  Copyright 1984, 1987 by Stephen L. Moshier
  --  ************************************************************************

  function Beta (a, b: Real) return Real is
    a_b, lg_a, lg_b, lg_a_b, sg_a, sg_b, sg_a_b, g_a_b: Real;
  begin
    a_b := a + b;
    if abs(a_b) > 171.624376956302725 then
      Log_Gamma(a_b, lg_a_b, sg_a_b);
      Log_Gamma(b, lg_b, sg_b);
      Log_Gamma(a, lg_a, sg_a);
      return sg_a_b * sg_a * sg_b * Exp(lg_a + lg_b - lg_a_b);
    end if;
    g_a_b := Gamma(a_b);
    if a > b then
      return (Gamma(a) / g_a_b) * Gamma(b);
    else
      return (Gamma(b) / g_a_b) * Gamma(a);
    end if;
  end Beta;

  ----------------------------------------------------------
  --  Now the tough part: the *incomplete* Beta function  --
  ----------------------------------------------------------

  subtype Integer_for_Beta is Integer;

  MachineEpsilon : constant Real :=  5.0E-16;
  MaxRealNumber : constant Real :=  1.0E300;
  MinRealNumber : constant Real :=  1.0E-300;

  function Almost_zero (x: Real) return Boolean is
  begin
    return abs x <= Real'Base'Model_Small;
  end Almost_zero;

  --  ************************************************************************
  --  Continued fraction expansion #1 for incomplete beta integral
  --
  --  Cephes Math Library, Release 2.8:  June, 2000
  --  Copyright 1984, 1995, 2000 by Stephen L. Moshier
  --  ************************************************************************

  function IncompleteBetaFE (a, b, x, big, biginv : Real) return Real is
    xk, pk, pkm1, pkm2, qk, qkm1, qkm2, k1, k2, k3, k4, k5, k6, k7, k8, r, t, ans, thresh: Real;
    n: Integer_for_Beta;
  begin
    k1 := a;
    k2 := a + b;
    k3 := a;
    k4 := a + 1.0;
    k5 := 1.0;
    k6 := b - 1.0;
    k7 := k4;
    k8 := a + 2.0;
    pkm2 := 0.0;
    qkm2 := 1.0;
    pkm1 := 1.0;
    qkm1 := 1.0;
    ans  := 1.0;
    r := 1.0;
    n := 0;
    thresh := 3.0 * MachineEpsilon;
    loop
      xk := -x*k1*k2/(k3*k4);
      pk := pkm1+pkm2*xk;
      qk := qkm1+qkm2*xk;
      pkm2 := pkm1;
      pkm1 := pk;
      qkm2 := qkm1;
      qkm1 := qk;
      xk := x*k5*k6/(k7*k8);
      pk := pkm1+pkm2*xk;
      qk := qkm1+qkm2*xk;
      pkm2 := pkm1;
      pkm1 := pk;
      qkm2 := qkm1;
      qkm1 := qk;
      if not Almost_zero (qk) then
        r := pk/qk;
      end if;
      if not Almost_zero (r) then
        t := abs((ans-r)/r);
        ans := r;
      else
        t :=  1.0;
      end if;
      exit when t < thresh;
      k1 := k1+ 1.0;
      k2 := k2+ 1.0;
      k3 := k3+2.0;
      k4 := k4+2.0;
      k5 := k5+ 1.0;
      k6 := k6- 1.0;
      k7 := k7+2.0;
      k8 := k8+2.0;
      if abs(qk) + abs(pk) > big then
        pkm2 := pkm2*biginv;
        pkm1 := pkm1*biginv;
        qkm2 := qkm2*biginv;
        qkm1 := qkm1*biginv;
      end if;
      if abs(qk) < biginv or abs(pk) < biginv then
        pkm2 := pkm2*big;
        pkm1 := pkm1*big;
        qkm2 := qkm2*big;
        qkm1 := qkm1*big;
      end if;
      n := n + 1;
      exit when n = 300;
    end loop;
    return ans;
  end IncompleteBetaFE;

  --  ************************************************************************
  --  Continued fraction expansion #2
  --  for incomplete beta integral
  --
  --  Cephes Math Library, Release 2.8:  June, 2000
  --  Copyright 1984, 1995, 2000 by Stephen L. Moshier
  --  ************************************************************************

  function IncompleteBetaFE2 (a, b, x, big, biginv : Real) return Real is
    xk, pk, pkm1, pkm2, qk, qkm1, qkm2, k1, k2, k3, k4, k5, k6, k7, k8, r, t, ans, z, thresh: Real;
    n: Integer_for_Beta;
  begin
    k1 := a;
    k2 := b- 1.0;
    k3 := a;
    k4 := a+ 1.0;
    k5 :=  1.0;
    k6 := a+b;
    k7 := a+ 1.0;
    k8 := a+2.0;
    pkm2 := 0.0;
    qkm2 :=  1.0;
    pkm1 :=  1.0;
    qkm1 :=  1.0;
    z := x / (1.0 - x);
    ans :=  1.0;
    r :=  1.0;
    n := 0;
    thresh := 3.0 * MachineEpsilon;
    loop
      xk := -z*k1*k2/(k3*k4);
      pk := pkm1+pkm2*xk;
      qk := qkm1+qkm2*xk;
      pkm2 := pkm1;
      pkm1 := pk;
      qkm2 := qkm1;
      qkm1 := qk;
      xk := z*k5*k6/(k7*k8);
      pk := pkm1+pkm2*xk;
      qk := qkm1+qkm2*xk;
      pkm2 := pkm1;
      pkm1 := pk;
      qkm2 := qkm1;
      qkm1 := qk;
      if not Almost_zero (qk) then
        r := pk/qk;
      end if;
      if not Almost_zero (r) then
        t := abs((ans-r)/r);
        ans := r;
      else
        t :=  1.0;
      end if;
      exit when t < thresh;
      k1 := k1+ 1.0;
      k2 := k2- 1.0;
      k3 := k3+2.0;
      k4 := k4+2.0;
      k5 := k5+ 1.0;
      k6 := k6+ 1.0;
      k7 := k7+2.0;
      k8 := k8+2.0;
      if abs(qk) + abs(pk) > big then
        pkm2 := pkm2*biginv;
        pkm1 := pkm1*biginv;
        qkm2 := qkm2*biginv;
        qkm1 := qkm1*biginv;
      end if;
      if abs(qk) < biginv or abs(pk) < biginv then
        pkm2 := pkm2*big;
        pkm1 := pkm1*big;
        qkm2 := qkm2*big;
        qkm1 := qkm1*big;
      end if;
      n := n + 1;
      exit when n = 300;
    end loop;
    return ans;
  end IncompleteBetaFE2;

  --  ************************************************************************
  --  Power series for incomplete beta integral.
  --  Use when b*x is small and x not too close to 1.
  --
  --  Cephes Math Library, Release 2.8:  June, 2000
  --  Copyright 1984, 1995, 2000 by Stephen L. Moshier
  --  ************************************************************************

  function IncompleteBetaPS(a, b, x, MAXGAM : Real) return Real is
    s, t, u, v, n, t1, z, ai:Real;
  begin
    ai := 1.0 / a;
    u := (1.0 - b) * x;
    v := u / (a + 1.0);
    t1 := v;
    t := u;
    n := 2.0;
    s := 0.0;
    z := MachineEpsilon * ai;
    while abs(v) > z loop
      u := (n-b) * x/n;
      t := t * u;
      v := t / (a + n);
      s := s + v;
      n := n + 1.0;
    end loop;
    s := s + t1;
    s := s + ai;
    u := a * Log(x);
    if  a+b < MAXGAM and u < Log(MaxRealNumber) then
      t := Gamma(a+b)/(Gamma(a)*Gamma(b));
      s := s*t* (x ** a);
    else
      t := Log_Gamma(a+b)-Log_Gamma(a)-Log_Gamma(b)+u+Log(s);
      if t < Log(MinRealNumber) then
        s := 0.0;
      else
        s := Exp(t);
      end if;
    end if;
    return s;
  end IncompleteBetaPS;

  function Beta (x, a, b: Real) return Real is
  begin
    return Regularized_Beta (x, a, b) * Beta (a, b);
  end Beta;

  function Inverse_Beta (y, a, b : Real) return Real is
  begin
    return Inverse_Regularized_Beta (y / Beta (a, b), a, b);
  end Inverse_Beta;

  --  ************************************************************************
  --  Incomplete beta integral
  --
  --  Returns incomplete beta integral of the arguments, evaluated
  --  from zero to x.  The function is defined as
  --
  --       x
  --       -
  --      | |  a-1     b-1
  --      |   t   (1-t)   dt.
  --    | |
  --     -
  --     0
  --
  --  The domain of definition is 0 <= x <= 1.  In this
  --  implementation a and b are restricted to positive values.
  --  The integral from x to 1 may be obtained by the symmetry
  --  relation
  --
  --  1 - incbet( a, b, x )  =  incbet( b, a, 1-x ).
  --
  --  The integral is evaluated by a continued fraction expansion
  --  or, when b*x is small, by a power series.
  --
  --  ACCURACY:
  --
  --  Tested at uniformly distributed random points (a,b,x) with a and b
  --  in "domain" and x between 0 and 1.
  --                                      Relative error
  --  arithmetic   domain     # trials      peak         rms
  --  IEEE      0,5         10000       6.9e-15     4.5e-16
  --  IEEE      0,85       250000       2.2e-13     1.7e-14
  --  IEEE      0,1000      30000       5.3e-12     6.3e-13
  --  IEEE      0,10000    250000       9.3e-11     7.1e-12
  --  IEEE      0,100000    10000       8.7e-10     4.8e-11
  --  Outputs smaller than the IEEE gradual underflow threshold
  --  were excluded from these statistics.
  --
  --  Cephes Math Library, Release 2.8:  June, 2000
  --  Copyright 1984, 1995, 2000 by Stephen L. Moshier
  --  ************************************************************************

  function Regularized_Beta(x, a, b: Real) return Real is
    aa, bb, xx : Real;
    t, xc, w, y:Real;
    flag : Integer_for_Beta := 0;
    big    : constant := 4.503599627370496e15;
    biginv : constant := 2.22044604925031308085e-16;
    MAXGAM : constant := 171.624376956302725;
    MINLOG : constant Real := Log(MinRealNumber);
    MAXLOG : constant Real := Log(MaxRealNumber);
  begin
    if Almost_zero (x) then
      return 0.0;
    end if;
    if Almost_zero (x - 1.0) then
      return 1.0;
    end if;
    if b*x <= 1.0 and x <= 0.95 then
      return IncompleteBetaPS(a, b, x, MAXGAM);
    end if;
    --  Ada: "in" parameters are constant; a,b,x are copied variables in Pascal
    aa := a;
    bb := b;
    xx := x;
    --
    w := 1.0 - xx;
    if xx > aa / (aa+bb) then
      flag := 1;
      t := aa;
      aa := bb;
      bb := t;
      xc := xx;
      xx := w;
    else
      xc := w;
    end if;
    if flag = 1 and bb*xx <= 1.0 and xx <= 0.95 then
      t := IncompleteBetaPS(aa, bb, xx, MAXGAM);
      if  t <= MachineEpsilon then
        return 1.0 - MachineEpsilon;
      else
        return 1.0 - t;
      end if;
    end if;
    y := xx * (aa+bb - 2.0) - (aa - 1.0);
    if y < 0.0 then
      w := IncompleteBetaFE(aa, bb, xx, big, biginv);
    else
      w := IncompleteBetaFE2(aa, bb, xx, big, biginv) / xc;
    end if;
    y := aa * Log(xx);
    t := bb * Log(xc);
    if aa+bb < MAXGAM  and abs(y) < MAXLOG and abs(t) < MAXLOG then
        t := xc ** bb;
        t := t * (xx ** aa);
        t := t / aa;
        t := t * w;
        t := t * (Gamma(aa+bb) / (Gamma(aa) * Gamma(bb)));
        if flag = 1 then
          if t <= MachineEpsilon then
            return 1.0 - MachineEpsilon;
          else
            return 1.0 - t;
          end if;
        else
          return t;
        end if;
    end if;
    y := y + t + Log_Gamma(aa+bb) - Log_Gamma(aa) - Log_Gamma(bb);
    y := y + Log(w/aa);
    if y < MINLOG then
      t := 0.0;
    else
      t := Exp(y);
    end if;
    if flag = 1 then
      if  t <= MachineEpsilon then
        t := 1.0 - MachineEpsilon;
      else
        t := 1.0 - t;
      end if;
    end if;
    return t;
  end Regularized_Beta;

  --  ************************************************************************
  --  Inverse of incomplete beta integral
  --
  --  Given y, the function finds x such that
  --
  --  incbet( a, b, x ) = y .
  --
  --  The routine performs interval halving or Newton iterations to find the
  --  root of incbet(a,b,x) - y = 0.
  --
  --
  --  ACCURACY:
  --
  --                    Relative error:
  --              x     a,b
  --  arithmetic   domain  domain  # trials    peak       rms
  --  IEEE      0,1    .5,10000   50000    5.8e-12   1.3e-13
  --  IEEE      0,1   .25,100    100000    1.8e-13   3.9e-15
  --  IEEE      0,1     0,5       50000    1.1e-12   5.5e-15
  --  With a and b constrained to half-integer or integer values:
  --  IEEE      0,1    .5,10000   50000    5.8e-12   1.1e-13
  --  IEEE      0,1    .5,100    100000    1.7e-14   7.9e-16
  --  With a = .5, b constrained to half-integer or integer values:
  --  IEEE      0,1    .5,10000   10000    8.3e-11   1.0e-11
  --
  --  Cephes Math Library Release 2.8:  June, 2000
  --  Copyright 1984, 1996, 2000 by Stephen L. Moshier
  --  ************************************************************************

  function Inverse_Regularized_Beta (y, a, b: Real) return Real is
      aaa:Real;
      bbb:Real;
      y0:Real;
      d:Real;
      yyy:Real;
      x:Real;
      x0:Real := 0.0;
      x1:Real := 1.0;
      lgm:Real;
      yp:Real;
      di:Real;
      dithresh:Real;
      yl:Real := 0.0;
      yh:Real := 1.0;
      xt:Real;
      i:Integer_for_Beta;
      rflg:Integer_for_Beta;
      dir:Integer_for_Beta;
      nflg:Integer_for_Beta := 0;
      MainLoopPos:Integer_for_Beta := 0;
      ihalve: constant := 1;
      ihalvecycle: constant := 2;
      newt: constant := 3;
      newtcycle: constant := 4;
      breaknewtcycle: constant := 5;
      breakihalvecycle: constant := 6;
  begin
    i := 0;
    if Almost_zero(y) then
      return 0.0;
    end if;
    if Almost_zero(y - 1.0) then
      return 1.0;
    end if;
    Main_Loop:
    loop
      --
      --  start
      --
      if MainLoopPos = 0 then
            if a <=  1.0 or b <=  1.0 then
                dithresh := 1.0e-6;
                rflg := 0;
                aaa := a;
                bbb := b;
                y0 := y;
                x := aaa / (aaa+bbb);
                yyy := Regularized_Beta (x, aaa, bbb);
                MainLoopPos := ihalve;
                goto Continue;
            else
                dithresh := 1.0e-4;
            end if;
            yp := - RPhF.Inverse_Phi (y);  --  ALGLIB: InvNormalDistribution
            if y > 0.5 then
                rflg := 1;
                aaa := b;
                bbb := a;
                y0 := 1.0 - y;
                yp := -yp;
            else
                rflg := 0;
                aaa := a;
                bbb := b;
                y0 := y;
            end if;
            lgm := (yp*yp-3.0) / 6.0;
            x := 2.0 / ( 1.0 / (2.0*aaa - 1.0)+ 1.0 / (2.0*bbb - 1.0));
            d := yp * Sqrt(x+lgm) / x - ( 1.0/(2.0*bbb- 1.0) - 1.0/(2.0*aaa- 1.0))*
                   (lgm + 5.0/6.0 - 2.0/(3.0*x));
            d := 2.0*d;
            if  d < Log(MinRealNumber) then
                x := 0.0;
                exit Main_Loop;
            end if;
            x := aaa / (aaa+bbb*Exp(d));
            yyy := Regularized_Beta (x, aaa, bbb);
            yp := (yyy-y0) / y0;
            if abs(yp) < Real(0.2) then
                MainLoopPos := newt;
                goto Continue;
            end if;
            MainLoopPos := ihalve;
            goto Continue;
        end if;
        --
        --  ihalve
        --
        if MainLoopPos = ihalve then
            dir := 0;
            di := 0.5;
            i := 0;
            MainLoopPos := ihalvecycle;
            goto Continue;
        end if;
        --
        --  ihalvecycle
        --
        if MainLoopPos = ihalvecycle then
            if i <= 99 then
                if  i/=0 then
                    x := x0+di*(x1-x0);
                    if Almost_zero(x - 1.0) then
                        x :=  1.0 - MachineEpsilon;
                    end if;
                    if Almost_zero(x) then
                        di := 0.5;
                        x := x0+di * (x1-x0);
                        exit Main_Loop when Almost_zero(x);
                    end if;
                    yyy := Regularized_Beta(x, aaa, bbb);
                    yp := (x1-x0) / (x1+x0);
                    if abs(yp) < dithresh then
                        MainLoopPos := newt;
                        goto Continue;
                    end if;
                    yp := (yyy-y0) / y0;
                    if abs(yp) < dithresh then
                        MainLoopPos := newt;
                        goto Continue;
                    end if;
                end if;
                if yyy < y0 then
                    x0 := x;
                    yl := yyy;
                    if dir < 0 then
                        dir := 0;
                        di := 0.5;
                    else
                        if dir > 3 then
                            di :=  1.0 - (1.0-di) * ( 1.0-di);
                        else
                            if dir > 1 then
                                di := 0.5 * di + 0.5;
                            else
                                di := (y0-yyy) / (yh-yl);
                            end if;
                        end if;
                    end if;
                    dir := dir + 1;
                    if x0 > 0.75 then
                        if  rflg = 1 then
                            rflg := 0;
                            aaa := a;
                            bbb := b;
                            y0 := y;
                        else
                            rflg := 1;
                            aaa := b;
                            bbb := a;
                            y0 :=  1.0-y;
                        end if;
                        x :=  1.0 - x;
                        yyy := Regularized_Beta(x, aaa, bbb);
                        x0 := 0.0;
                        yl := 0.0;
                        x1 :=  1.0;
                        yh :=  1.0;
                        MainLoopPos := ihalve;
                        goto Continue;
                    end if;
                else
                    x1 := x;
                    if rflg = 1 and x1 < MachineEpsilon then
                        x := 0.0;
                        exit Main_Loop;
                    end if;
                    yh := yyy;
                    if dir > 0 then
                        dir := 0;
                        di := 0.5;
                    else
                        if dir < -3 then
                            di := di*di;
                        else
                            if  dir < -1 then
                                di := 0.5 * di;
                            else
                                di := (yyy-y0) / (yh-yl);
                            end if;
                        end if;
                    end if;
                    dir := dir - 1;
                end if;
                i := i + 1;
                MainLoopPos := ihalvecycle;
                goto Continue;
            else
                MainLoopPos := breakihalvecycle;
                goto Continue;
            end if;
        end if;
        --
        --  breakihalvecycle
        --
        if MainLoopPos = breakihalvecycle then
            if x0 >= 1.0 then
                x :=  1.0 - MachineEpsilon;
                exit Main_Loop;
            end if;
            if x <= 0.0 then
                x := 0.0;
                exit Main_Loop;
            end if;
            MainLoopPos := newt;
            goto Continue;
        end if;
        --
        --  newt
        --
        if MainLoopPos = newt then
            exit Main_Loop when nflg /= 0;
            nflg := 1;
            lgm := Log_Gamma(aaa+bbb) - Log_Gamma(aaa) - Log_Gamma(bbb);
            i := 0;
            MainLoopPos := newtcycle;
            goto Continue;
        end if;
        --
        --  newtcycle
        --
        if  MainLoopPos = newtcycle then
            if  i <= 7 then
                if  i /= 0 then
                    yyy := Regularized_Beta (x, aaa, bbb);
                end if;
                if  yyy < yl then
                    x := x0;
                    yyy := yl;
                else
                    if yyy > yh then
                        x := x1;
                        yyy := yh;
                    else
                        if yyy < y0 then
                            x0 := x;
                            yl := yyy;
                        else
                            x1 := x;
                            yh := yyy;
                        end if;
                    end if;
                end if;
                if Almost_zero(x - 1.0) or Almost_zero(x) then
                    MainLoopPos := breaknewtcycle;
                    goto Continue;
                end if;
                d := (aaa - 1.0) * Log(x) + (bbb - 1.0) * Log(1.0 - x) + lgm;
                exit Main_Loop when d < Log(MinRealNumber);
                if  d > Log(MaxRealNumber) then
                    MainLoopPos := breaknewtcycle;
                    goto Continue;
                end if;
                d := Exp(d);
                d := (yyy-y0)/d;
                xt := x-d;
                if xt <= x0 then
                    yyy := (x-x0) / (x1-x0);
                    xt := x0 + 0.5 * yyy * (x-x0);
                    if xt <= 0.0 then
                        MainLoopPos := breaknewtcycle;
                        goto Continue;
                    end if;
                end if;
                if xt >= x1 then
                    yyy := (x1-x) / (x1-x0);
                    xt := x1 - 0.5 * yyy * (x1-x);
                    if xt >= 1.0 then
                        MainLoopPos := breaknewtcycle;
                        goto Continue;
                    end if;
                end if;
                x := xt;
                exit Main_Loop when abs(d/x) < 128.0 * MachineEpsilon;
                i := i + 1;
                MainLoopPos := newtcycle;
                goto Continue;
            else
                MainLoopPos := breaknewtcycle;
                goto Continue;
            end if;
        end if;
        --
        --  breaknewtcycle
        --
        if  MainLoopPos = breaknewtcycle then
            dithresh := 256.0 * MachineEpsilon;
            MainLoopPos := ihalve;
            goto Continue;
        end if;
        <<Continue>> null;
    end loop Main_Loop;
    --
    --  done
    --
    if rflg /= 0 then
        if  x <= MachineEpsilon then
            x :=  1.0 - MachineEpsilon;
        else
            x :=  1.0 - x;
        end if;
    end if;
    return x;
  end Inverse_Regularized_Beta;

end Beta_function;
