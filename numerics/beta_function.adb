-- Translated on 9-Mar-2018 by (New) P2Ada v. 28-Oct-2009

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics; use Ada.Numerics;

with Gamma_function;

package body Beta_function is

  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  use REF;

  package Real_Gamma is new Gamma_function(Real);
  use Real_Gamma;

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
    k2 := a+b;
    k3 := a;
    k4 := a+ 1.0;
    k5 :=  1.0;
    k6 := b- 1.0;
    k7 := k4;
    k8 := a+Real(2.0);
    pkm2 := Real(0.0);
    qkm2 :=  1.0;
    pkm1 :=  1.0;
    qkm1 :=  1.0;
    ans :=  1.0;
    r :=  1.0;
    n := 0;
    thresh := Real(3.0) * MachineEpsilon;
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
        k3 := k3+Real(2.0);
        k4 := k4+Real(2.0);
        k5 := k5+ 1.0;
        k6 := k6- 1.0;
        k7 := k7+Real(2.0);
        k8 := k8+Real(2.0);
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
    k8 := a+Real(2.0);
    pkm2 := Real(0.0);
    qkm2 :=  1.0;
    pkm1 :=  1.0;
    qkm1 :=  1.0;
    z := x / (1.0 - x);
    ans :=  1.0;
    r :=  1.0;
    n := 0;
    thresh := Real(3.0) * MachineEpsilon;
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
        k3 := k3+Real(2.0);
        k4 := k4+Real(2.0);
        k5 := k5+ 1.0;
        k6 := k6+ 1.0;
        k7 := k7+Real(2.0);
        k8 := k8+Real(2.0);
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

  function IncompleteBetaPS(a : Real;
     b : Real;
     x : Real;
     MAXGAM : Real) return Real
  is
      s:Real;
      t:Real;
      u:Real;
      v:Real;
      n:Real;
      t1:Real;
      z:Real;
      ai:Real;
  begin
    ai :=  1.0/a;
    u := ( 1.0-b)*x;
    v := u/(a+ 1.0);
    t1 := v;
    t := u;
    n := Real(2.0);
    s := Real(0.0);
    z := MachineEpsilon*ai;
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
            s := Real(0.0);
        else
            s := Exp(t);
        end if;
    end if;
    return s;
  end IncompleteBetaPS;

  --  ************************************************************************
  --  Incomplete beta integral
  --
  --  Returns incomplete beta integral of the arguments, evaluated
  --  from zero to x.  The function is defined as
  --
  --                  x
  --     -            -
  --    | (a+b)      | |  a-1     b-1
  --  -----------    |   t   (1-t)   dt.
  --   -     -     | |
  --  | (a) | (b)   -
  --                0
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

  function Beta(x, a, b: Real) return Real is
  begin
    return Regularized_Beta(x,a,b) * Beta(a,b);
  end Beta;

  function Regularized_Beta(x, a, b: Real) return Real is
    aa, bb, xx : Real;
    t:Real;
    xc:Real;
    w:Real;
    y:Real;
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
        return  1.0 - MachineEpsilon;
      else
        return  1.0 - t;
      end if;
    end if;
    y := xx * (aa+bb-Real(2.0)) - (aa - 1.0);
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
            return  1.0 - MachineEpsilon;
          else
            return  1.0 - t;
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

  --  function InvIncompleteBeta(a : Real; b : Real; y : Real) return Real is
  --      aaa:Real;
  --      bbb:Real;
  --      y0:Real;
  --      d:Real;
  --      yyy:Real;
  --      x:Real;
  --      x0:Real;
  --      x1:Real;
  --      lgm:Real;
  --      yp:Real;
  --      di:Real;
  --      dithresh:Real;
  --      yl:Real;
  --      yh:Real;
  --      xt:Real;
  --      i:Integer_for_Beta;
  --      rflg:Integer_for_Beta;
  --      dir:Integer_for_Beta;
  --      nflg:Integer_for_Beta;
  --      s:Real;
  --      MainLoopPos:Integer_for_Beta;
  --      ihalve:Integer_for_Beta;
  --      ihalvecycle:Integer_for_Beta;
  --      newt:Integer_for_Beta;
  --      newtcycle:Integer_for_Beta;
  --      breaknewtcycle:Integer_for_Beta;
  --      breakihalvecycle:Integer_for_Beta;
  --  begin
  --    i := 0;
  --    if Almost_zero(y) then
  --      return 0.0;
  --    end if;
  --    if Almost_zero(y - 1.0) then
  --      return 1.0;
  --    end if;
  --    x0 := Real(0.0);
  --    yl := Real(0.0);
  --    x1 :=  1.0;
  --    yh :=  1.0;
  --    nflg := 0;
  --    MainLoopPos := 0;
  --    ihalve := 1;
  --    ihalvecycle := 2;
  --    newt := 3;
  --    newtcycle := 4;
  --    breaknewtcycle := 5;
  --    breakihalvecycle := 6;
  --    loop
  --      --
  --      --  start
  --      --
  --      if MainLoopPos=0 then
  --            if  (a <=  1.0)  or  (b <=  1.0) then
  --                dithresh := Real(1.0e-6);
  --                rflg := 0;
  --                aaa := a;
  --                bbb := b;
  --                y0 := y;
  --                x := aaa/(aaa+bbb);
  --                yyy := IncompleteBeta(aaa, bbb, x);
  --                MainLoopPos := ihalve;
  --                Continue;
  --            else
  --                dithresh := Real(1.0e-4);
  --            end if;
  --            yp := -InvNormalDistribution(y);
  --            if (y > Real(0.5)) then
  --                rflg := 1;
  --                aaa := b;
  --                bbb := a;
  --                y0 :=  1.0-y;
  --                yp := -yp;
  --            else
  --                rflg := 0;
  --                aaa := a;
  --                bbb := b;
  --                y0 := y;
  --            end if;
  --            lgm := (yp*yp-Real(3.0))/Real(6.0);
  --            x := Real(2.0)/( 1.0/(Real(2.0)*aaa- 1.0)+ 1.0/(Real(2.0)*bbb- 1.0));
  --            d := yp*sqrt(x+lgm)/x-( 1.0/(Real(2.0)*bbb- 1.0)- 1.0/(Real(2.0)*aaa- 1.0))*
  --                   (lgm+Real(5.0)/Real(6.0)-Real(2.0)/(Real(3.0)*x));
  --            d := Real(2.0)*d;
  --            if  (d < Log(MinRealNumber)) then
  --                x := 0.0;
  --                exit;
  --            end if;
  --            x := aaa/(aaa+bbb*exp(d));
  --            yyy := IncompleteBeta(aaa, bbb, x);
  --            yp := (yyy-y0)/y0;
  --            if  (abs(yp) < Real(0.2)) then
  --                MainLoopPos := newt;
  --                Continue;
  --            end if;
  --            MainLoopPos := ihalve;
  --            Continue;
  --        end if;
  --        --
  --        --  ihalve
  --        --
  --        if MainLoopPos = ihalve then
  --            dir := 0;
  --            di := Real(0.5);
  --            i := 0;
  --            MainLoopPos := ihalvecycle;
  --            Continue;
  --        end if;
  --        --
  --        --  ihalvecycle
  --        --
  --        if MainLoopPos = ihalvecycle then
  --            if  i<=99 then
  --                if  i/=0 then
  --                    x := x0+di*(x1-x0);
  --                    if  AP_FP_Eq(x, 1.0) then
  --                        x :=  1.0-MachineEpsilon;
  --                    end if;
  --                    if  AP_FP_Eq(x,Real(0.0)) then
  --                        di := Real(0.5);
  --                        x := x0+di*(x1-x0);
  --                        if  AP_FP_Eq(x,Real(0.0)) then
  --                            exit;
  --                        end if;
  --                    end if;
  --                    yyy := IncompleteBeta(aaa, bbb, x);
  --                    yp := (x1-x0)/(x1+x0);
  --                    if  (abs(yp) < dithresh) then
  --                        MainLoopPos := newt;
  --                        Continue;
  --                    end if;
  --                    yp := (yyy-y0)/y0;
  --                    if (abs(yp) < dithresh) then
  --                        MainLoopPos := newt;
  --                        Continue;
  --                    end if;
  --                end if;
  --                if yyy < y0 then
  --                    x0 := x;
  --                    yl := yyy;
  --                    if dir < 0 then
  --                        dir := 0;
  --                        di := Real(0.5);
  --                    else
  --                        if dir > 3 then
  --                            di :=  1.0-( 1.0-di)*( 1.0-di);
  --                        else
  --                            if dir > 1 then
  --                                di := Real(0.5)*di+Real(0.5);
  --                            else
  --                                di := (y0-yyy)/(yh-yl);
  --                            end if;
  --                        end if;
  --                    end if;
  --                    dir := dir + 1;
  --                    if  (x0 > Real(0.75)) then
  --                        if  rflg = 1 then
  --                            rflg := 0;
  --                            aaa := a;
  --                            bbb := b;
  --                            y0 := y;
  --                        else
  --                            rflg := 1;
  --                            aaa := b;
  --                            bbb := a;
  --                            y0 :=  1.0-y;
  --                        end if;
  --                        x :=  1.0-x;
  --                        yyy := IncompleteBeta(aaa, bbb, x);
  --                        x0 := Real(0.0);
  --                        yl := Real(0.0);
  --                        x1 :=  1.0;
  --                        yh :=  1.0;
  --                        MainLoopPos := ihalve;
  --                        Continue;
  --                    end if;
  --                else
  --                    x1 := x;
  --                    if  (rflg=1)  and (x1 < MachineEpsilon) then
  --                        x := Real(0.0);
  --                        exit;
  --                    end if;
  --                    yh := yyy;
  --                    if dir > 0 then
  --                        dir := 0;
  --                        di := Real(0.5);
  --                    else
  --                        if dir < -3 then
  --                            di := di*di;
  --                        else
  --                            if  dir < -1 then
  --                                di := Real(0.5)*di;
  --                            else
  --                                di := (yyy-y0)/(yh-yl);
  --                            end if;
  --                        end if;
  --                    end if;
  --                    dir := dir - 1;
  --                end if;
  --                i := i + 1;
  --                MainLoopPos := ihalvecycle;
  --                Continue;
  --            else
  --                MainLoopPos := breakihalvecycle;
  --                Continue;
  --            end if;
  --        end if;
  --        --
  --        --  breakihalvecycle
  --        --
  --        if MainLoopPos=breakihalvecycle then
  --            if x0 >= 1.0 then
  --                x :=  1.0 - MachineEpsilon;
  --                exit;
  --            end if;
  --            if (x <= Real(0.0)) then
  --                x := Real(0.0);
  --                exit;
  --            end if;
  --            MainLoopPos := newt;
  --            Continue;
  --        end if;
  --        --
  --        --  newt
  --        --
  --        if  MainLoopPos=newt then
  --
  --            if  nflg/=0 then
  --
  --                exit;
  --            end if;
  --            nflg := 1;
  --            lgm := Log_Gamma(aaa+bbb, s)-Log_Gamma(aaa, s)-Log_Gamma(bbb, s);
  --            i := 0;
  --            MainLoopPos := newtcycle;
  --            Continue;
  --        end if;
  --        --
  --        --  newtcycle
  --        --
  --        if  MainLoopPos = newtcycle then
  --            if  i<=7 then
  --                if  i/=0 then
  --                    yyy := IncompleteBeta(aaa, bbb, x);
  --                end if;
  --                if  (yyy < yl) then
  --                    x := x0;
  --                    yyy := yl;
  --                else
  --                    if (yyy > yh) then
  --                        x := x1;
  --                        yyy := yh;
  --                    else
  --                        if  (yyy < y0) then
  --                            x0 := x;
  --                            yl := yyy;
  --                        else
  --                            x1 := x;
  --                            yh := yyy;
  --                        end if;
  --                    end if;
  --                end if;
  --                if  AP_FP_Eq(x, 1.0)  or  AP_FP_Eq(x,Real(0.0)) then
  --                    MainLoopPos := breaknewtcycle;
  --                    Continue;
  --                end if;
  --                d := (aaa- 1.0)*Log(x)+(bbb- 1.0)*Log( 1.0-x)+lgm;
  --                if  (d < Log(MinRealNumber)) then
  --                    exit;
  --                end if;
  --                if  (d > Log(MaxRealNumber)) then
  --                    MainLoopPos := breaknewtcycle;
  --                    Continue;
  --                end if;
  --                d := exp(d);
  --                d := (yyy-y0)/d;
  --                xt := x-d;
  --                if (xt <= x0) then
  --                    yyy := (x-x0)/(x1-x0);
  --                    xt := x0+Real(0.5)*yyy*(x-x0);
  --                    if  (xt <= Real(0.0)) then
  --                        MainLoopPos := breaknewtcycle;
  --                        Continue;
  --                    end if;
  --                end if;
  --                if  (xt >= x1) then
  --                    yyy := (x1-x)/(x1-x0);
  --                    xt := x1-Real(0.5)*yyy*(x1-x);
  --                    if  (xt >=  1.0) then
  --                        MainLoopPos := breaknewtcycle;
  --                        Continue;
  --                    end if;
  --                end if;
  --                x := xt;
  --                if (abs(d/x) < Real(128.0) * MachineEpsilon) then
  --                    exit;
  --                end if;
  --                i := i+1;
  --                MainLoopPos := newtcycle;
  --                Continue;
  --            else
  --                MainLoopPos := breaknewtcycle;
  --                Continue;
  --            end if;
  --        end if;
  --    --
  --    --  breaknewtcycle
  --    --
  --    if  MainLoopPos=breaknewtcycle then
  --            dithresh := Real(256.0)*MachineEpsilon;
  --            MainLoopPos := ihalve;
  --            Continue;
  --        end if;
  --    end loop;
  --    --
  --    --  done
  --    --
  --    if rflg /= 0 then
  --        if  (x <= MachineEpsilon) then
  --            x :=  1.0-MachineEpsilon;
  --        else
  --            x :=  1.0 - x;
  --        end if;
  --    end if;
  --    return x;
  --  end InvIncompleteBeta;
  --
end Beta_function;
