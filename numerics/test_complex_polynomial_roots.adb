-- Test by Gautier de Montmollin 10-Jan-2004

with Ada.Numerics.Generic_Complex_Types; 
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Float_Random;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Text_IO.Complex_IO;

with Complex_Polynomial_Roots;

procedure Test_Complex_Polynomial_Roots is

  subtype TReal is Long_Long_Float;

  package CT is new Ada.Numerics.Generic_Complex_Types(TReal);
  package CPR is new Complex_Polynomial_Roots(CT);
  
  package EF is new
    Ada.Numerics.Generic_Elementary_Functions(TReal);

  package RIO is new Float_IO(TReal);
  package CIO is new Ada.Text_IO.Complex_IO(CT);

  use CT,EF,CPR,RIO,CIO;
  
  G: Ada.Numerics.Float_Random.Generator;

  function Rnd return TReal is
  begin
    return TReal(Ada.Numerics.Float_Random.Random(G));
  end Rnd;

  function Wide_dev_Rdn return TReal is
    r: TReal;
  begin
    r:= Exp((Rnd-0.5) * 10.0);
    -- ^ Finds as many small numbers as large ones
    if Rnd > 0.5 then
    -- ^ Finds as many positive numbers as negative ones 
      return  r;
    else
      return -r;
    end if;
  end Wide_dev_Rdn;

  p: Complex;
  a: array(0..4) of TReal;
  r,Pr: array(1..4) of Complex;
  ok: array(r'Range) of Boolean;
  all_ok: Boolean;

  ntest: constant:= 10_000_000;
  tol: TReal;
  ptol: array(2..4) of Integer;

begin
  Put(TReal'Epsilon);       New_Line; -- Ada 83
  Put(TReal'Model_Epsilon); New_Line; -- Ada 95
  Put(Integer'Image(TReal'Digits)); New_Line;

  case TReal'Digits is
    when  1..15  => ptol:= ( -9,-4,+2); -- GNAT: LF
    when 16..18  => ptol:= (-12,-5,+2); -- GNAT: LLF
    when others  => ptol:= ( -9,-4,+2);
  end case;

  Ada.Numerics.Float_Random.Reset(G);

  degrees: for d in 2..4 loop
    tol:= 10.0**ptol(d);
    Put_Line(
      "Testing degree" & Integer'Image(d) &
      " with " & Integer'Image(ntest) &
      " random polynomials");
    tests: for test in 1..ntest loop
      -- Fill polynomial with random parameters:
      for t in 0..d loop
        a(t):= Wide_dev_Rdn;
      end loop;
      -- Solve polynomial:
      case d is
        when 2 =>
          Solve(a(2),a(1),a(0), r(1),r(2));
        when 3 =>
          Solve(a(3),a(2),a(1),a(0), r(1),r(2),r(3));
        when 4 =>
          Solve(a(4),a(3),a(2),a(1),a(0), r(1),r(2),r(3),r(4));
      end case;

      all_ok:= True;
      roots_test: for j in 1..d loop -- Test the jth root
        p:= (a(0),0.0);
        for t in 1..d loop
          p:= p + a(t) * r(j) ** t;
        end loop;
        ok(j):= abs p < tol;
        all_ok:= all_ok and ok(j);
        Pr(j):= p;
      end loop roots_test;
      if not all_ok then
        Put_Line(
          " Test" & Integer'Image(test) &
          " P(x): "
        );
        for t in reverse 0..d loop
          Put("   ");
          Put(a(t),7,8,0);
          Put(" x^" & Integer'Image(t));
          New_Line;
        end loop;
        roots_show: for j in 1..d loop
          Put("r_" & Integer'Image(j) & "=");
          Put(r(j),0,5,0);
          Put("   P(r_" & Integer'Image(j) & ")=");
          Put(Pr(j),0,Integer'Max(3,ptol(d)),0);
          if not ok(j) then Put("  BAD"); end if;
          New_Line;
        end loop roots_show;
      end if;

    end loop tests;
  end loop degrees;
end;
