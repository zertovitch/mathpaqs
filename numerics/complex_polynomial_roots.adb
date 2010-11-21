with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Complex_Elementary_Functions;

-- with Ada.Text_IO;                       use Ada.Text_IO;

package body Complex_Polynomial_Roots is
  package GEF is new
    Ada.Numerics.Generic_Elementary_Functions(Complex_Types.Real'Base);
  package GCEF is new
    Ada.Numerics.Generic_Complex_Elementary_Functions(Complex_Types);

  use Ada.Numerics, GEF, GCEF;

  epsilon: Real:= Real'Model_Epsilon;
  epsilon2: Real:= epsilon**2;

  procedure Solve (a,b,c: Real; r1,r2: out Complex) is
    dis_sqrt: Complex; denom: Real;
  begin
    if abs a <= epsilon then
      raise dominant_coefficient_a_is_zero;
    end if;
    dis_sqrt:= sqrt((b*b - a * 4.0 * c,0.0));
    denom:= 1.0 / (2.0 * a);
    r1:= (-b + dis_sqrt) * denom;
    r2:= (-b - dis_sqrt) * denom;
  end;

  procedure Solve (a,b,c: Complex; r1,r2: out Complex) is
    dis_sqrt: Complex; denom: Complex;
  begin
    if abs a <= epsilon then
      raise dominant_coefficient_a_is_zero;
    end if;
    dis_sqrt:= sqrt(b*b - a * 4.0 * c);
    denom:= 1.0 / (2.0 * a);
    r1:= (-b + dis_sqrt) * denom;
    r2:= (-b - dis_sqrt) * denom;
  end;

  third: constant:= 1.0/3.0;
  sqrt3: constant Real:= sqrt(3.0);

  procedure Solve (a,b,c,d: Real; r1,r2,r3: out Complex) is
    p,q, r, dr, qsr3, ymx, phi: Real;
  begin
    if abs a <= epsilon then
      raise dominant_coefficient_a_is_zero;
    end if;

    if abs d < epsilon then
      r1:= (0.0,0.0);
      Solve(a,b,c, r2,r3);
    else
      -- Equivalent equation: y^3 + 3p y + 2q = 0 where y = x + b/(3*a)
      ymx:= b/(3.0*a); -- = y-x. Then x = y - ymx
      p:= (3.0 * a*c - b**2) / (9.0 * a**2);
      q:= (b**3) / (27.0 * a**3) - b*c / (6.0*a**2) + d / (2.0 * a);
      
      if abs p < epsilon then -- y^3 = -2q
        if q < 0.0 then       -- y^3 =  2|q| 
          r1:= ( 2.0 * (abs q)**third - ymx, 0.0);
        else                  -- y^3 = -2|q| 
          r1:= (-2.0 * (abs q)**third - ymx, 0.0);
        end if;
        r2:= r1;
        r3:= r1;
      else
        r:= sqrt(abs p);
        if q < 0.0 then
          r:= -r;
        end if;
        qsr3:= q / (r**3);
        dr:= r+r;
        if p < 0.0 then -- also: p < -epsilon < 0
          if p**3 + q**2 <= 0.0 then
            phi:= arccos(qsr3);
            r1:= ( -dr * cos(phi * third) - ymx, 0.0 );
            r2:= (  dr * cos((pi-phi)* third) - ymx, 0.0);
            r3:= (  dr * cos((pi+phi)* third) - ymx, 0.0);
          else
            phi:= arccosh(qsr3);
            p:= cosh(phi* third);
            r1:= ( -dr * p - ymx, 0.0 );
            r2:= (   r * p - ymx, sqrt3 * r * sinh(phi* third));
            r3:= Conjugate(r2);
          end if;
        else -- p > epsilon > 0
          phi:= arcsinh(qsr3);
          p:= sinh(phi* third);
          r1:= ( -dr * p - ymx, 0.0 );
          r2:= (   r * p - ymx,  sqrt3 * r * cosh(phi* third));
          r3:= Conjugate(r2);
        end if;
      end if;
    end if;
  end;

  procedure Solve_A (a,b,c,d,e: Real; r1,r2,r3,r4: out Complex) is
    -- Bronstein-Semendjajev
    bb,cc,dd,ee, y, L2,L, f: Real;
    yc, d1,d2: Complex;
  begin
    if abs a <= epsilon then
      raise dominant_coefficient_a_is_zero;
    end if;

    if abs e < epsilon then
      r1:= (0.0,0.0);
      Solve(a,b,c,d, r2,r3,r4);
    else
      bb:= b / a;
      cc:= c / a;
      dd:= d / a;
      ee:= e / a;
      -- Solve the resolvant cubic
      Solve( 8.0, -4.0*cc, 2.0*bb*dd - 8.0*ee, ee*(4.0*cc-bb**2)-dd**2, yc,d1,d2 );
      y:= Re(yc);
      -- y is the cubic's root in \IR
      L2:= 8.0*y + bb**2 - 4.0*cc;
      if abs L2 < epsilon2 then
        r1:= (0.0,0.0);
      elsif L2 > 0.0 then
        L:= Sqrt(L2);
        f:= (bb*y - dd) / L;
        Solve( 1.0, 0.5*(bb + L), y + f, r1,r2 );
        Solve( 1.0, 0.5*(bb - L), y - f, r3,r4 );
      else
        L:= Sqrt(-L2);
        f:= (bb*y - dd) / L;
        Solve( (1.0,0.0), 0.5*(bb, -L), (y, - f), r1,r2 );
        Solve( (1.0,0.0), 0.5*(bb,  L), (y,   f), r3,r4 );
      end if;
    end if;
  end Solve_A;

  procedure Solve_B (a,b,c,d,e: Real; r1,r2,r3,r4: out Complex) is
    -- Ferrari (John M. Gamble)
    b4,bb,cc,dd,ee,f,g,h: Real;
    alpha, beta, gamma, rho: Complex;
    p, q, z: Complex;
  begin
    if abs a <= epsilon then
      raise dominant_coefficient_a_is_zero;
    end if;

    if abs e < epsilon then
      r1:= (0.0,0.0);
      Solve(a,b,c,d, r2,r3,r4);
    else
      --
      -- First step:  Divide by the leading coefficient.
      --
      bb:= b / a;
      cc:= c / a;
      dd:= d / a;
      ee:= e / a;
      --
      -- Second step: simplify the equation to the
      -- "resolvant cubic"  y**4 + fy**2 + gy + h.
      --
      -- (This is done by setting x:= y - b/4).
      --
      b4:= bb * 0.25;
      --
      -- The f, g, and h values are:
      --
      f:= cc - 6.0 * b4 * b4;
      g:= dd + 2.0 * b4 * (-cc + 4.0 * b4 * b4);
      h:= ee + b4 * (-dd + b4 * (cc - 3.0 * b4 * b4));

      if abs h < epsilon then
        --
        -- Special case: h = 0.  We have a cubic times y.
        --
        r1:= (0.0,0.0);
        Solve(1.0, 0.0, f, g, r2,r3,r4);
      elsif abs g < epsilon then
        --
        -- Another special case: g = 0.  We have a quadratic
        -- with y-squared.
        --
        Solve(1.0,f,h, p,q);
        p:= sqrt(p);
        q:= sqrt(q);
        r1:=  p;
        r2:= -p;
        r3:=  q;
        r4:= -q;
      else
        --
        -- Special cases don't apply, so continue on with Ferrari's
        -- method.  This involves setting up the resolvant cubic
        -- as the product of two quadratics.
        --
        -- After setting up conditions that guarantee that the
        -- coefficients come out right (including the zero value
        -- for the third-power term), we wind up with a 6th
        -- degree polynomial with, fortunately, only even-powered
        -- terms.  In other words, a cubic with z:= y**2.
        --
        -- Take a root of that equation, and get the
        -- quadratics from it.
        --
        Solve(1.0, 2.0*f, f*f - 4.0*h, -g*g, z,p,q); -- p,q: dummy
        alpha:= sqrt(z);
        rho:= g/alpha;
        beta:= (f + z - rho)*0.5;
        gamma:= (f + z + rho)*0.5;

        Solve((1.0,0.0),  alpha,  beta, r1,r2);
        Solve((1.0,0.0), -alpha, gamma, r3,r4);
      end if;

      r1:= r1 - b4;
      r2:= r2 - b4;
      r3:= r3 - b4;
      r4:= r4 - b4;
    end if;
  end Solve_B;

  procedure Solve_comp (a,b,c,d,e: Real; r1,r2,r3,r4: out Complex) is
    subtype Method is Integer range 1..2;
    r: array(Method,1..4) of Complex;
    n: array(Method) of Real;
    mm: Method;
    df,dr: Real;
  begin
    if abs a <= epsilon then
      raise dominant_coefficient_a_is_zero;
    end if;

    if abs e < epsilon then
      r1:= (0.0,0.0);
      Solve(a,b,c,d, r2,r3,r4);
    else
      Solve_A(a,b,c,d,e, r(1,1),r(1,2),r(1,3),r(1,4));
      Solve_B(a,b,c,d,e, r(2,1),r(2,2),r(2,3),r(2,4));
      for m in Method loop
        n(m):= 0.0;
        for i in 1..4 loop
          n(m):= n(m) + abs r(m,i);
        end loop;
      end loop;
      if n(1) < n(2) then
        --@@ Put('A');
        mm:= 1;
      else
        --@@ Put('B');
        mm:= 2;
      end if;
      df:= abs(n(1)-n(2));
      dr:= df / abs(n(2));
      if dr > 0.1 then
        --@@ Put("("&Real'image(n(mm))&"<"&Real'image(n(3-mm))&")");
        null;
      end if;
      r1:= r(mm,1);
      r2:= r(mm,2);
      r3:= r(mm,3);
      r4:= r(mm,4);
    end if;
  end Solve_comp;

  procedure Solve (a,b,c,d,e: Real; r1,r2,r3,r4: out Complex)
    renames Solve_comp;

end Complex_Polynomial_Roots;

