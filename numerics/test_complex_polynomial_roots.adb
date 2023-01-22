--  Test by Gautier de Montmollin 10-Jan-2004 and later.
--  gprbuild -P mathpaqs -XMathpaqs_Build_Mode=Debug Test_Complex_Polynomial_Roots

with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Float_Random;
with Ada.Text_IO.Complex_IO;

with Complex_Polynomial_Roots;

procedure Test_Complex_Polynomial_Roots is

  type TReal is digits 18;

  package CT is new Ada.Numerics.Generic_Complex_Types (TReal);
  package CPR is new Complex_Polynomial_Roots (CT);

  package EF is new
    Ada.Numerics.Generic_Elementary_Functions (TReal);

  package RIO is new Ada.Text_IO.Float_IO(TReal);
  package CIO is new Ada.Text_IO.Complex_IO(CT);

  use CT,EF,CPR, Ada.Text_IO, RIO,CIO;

  G: Ada.Numerics.Float_Random.Generator;

  function Rnd return TReal is
  begin
    return TReal(Ada.Numerics.Float_Random.Random(G));
  end Rnd;

  function Wide_dev_Rdn return TReal is
    r : TReal;
  begin
    r := Exp((Rnd - 0.5) * 10.0);
    -- ^ Finds as many small numbers as large ones
    if Rnd > 0.5 then
    -- ^ Finds as many positive numbers as negative ones
      return  r;
    else
      return -r;
    end if;
  end Wide_dev_Rdn;

  function IImg (I: Natural) return String is  --  Get rid of the leading ' '.
    s : constant String := Integer'Image (I);
  begin
    return s (s'First + 1 .. s'Last);
  end IImg;

  p : Complex;
  a : array(0..4) of TReal;
  r, Pr : array(1..4) of Complex;
  ok : array(r'Range) of Boolean;
  all_ok : Boolean;

  ntest_img : constant String := "100_000_000";
  ntest : constant Integer := Integer'Value (ntest_img);
  tol : TReal;
  ptol : array (2..4) of Integer;

begin
  Put ("TReal'Model_Epsilon: ");  Put(TReal'Model_Epsilon); New_Line;
  Put ("TReal'Digits:         "); Put(IImg(TReal'Digits));  New_Line;
  Put_Line ("--");

  case TReal'Digits is
    when 16..18  => ptol := (-12, -5, +1);
    when others  => ptol := ( -9, -4, +2);
  end case;

  Ada.Numerics.Float_Random.Reset(G);

  degrees: for d in reverse 2 .. 4 loop
    tol := 10.0 ** ptol(d);
    Put(
      "Testing degree " & IImg(d) &
      " with " & ntest_img &
      " random polynomials. Tolerance is ");
    Put (tol, 0, 2, 2);
    New_Line;
    tests: for test in 1..ntest loop
      --  Fill polynomial with random parameters:
      for t in 0 .. d loop
        a(t) := Wide_dev_Rdn;
      end loop;
      --  Solve polynomial:
      case d is
        when 2 =>
          Solve (a(2),a(1),a(0), r(1),r(2));
        when 3 =>
          Solve (a(3),a(2),a(1),a(0), r(1),r(2),r(3));
        when 4 =>
          Solve (a(4),a(3),a(2),a(1),a(0), r(1),r(2),r(3),r(4));
      end case;
      --
      all_ok:= True;
      --
      roots_test: for j in 1 .. d loop  --  Test the jth root
        p := (a(0), 0.0);
        for t in 1 .. d loop
          p := p + a(t) * r(j) ** t;
        end loop;
        ok(j) := abs p < tol;
        all_ok := all_ok and ok(j);
        Pr(j) := p;  --  Store the evaluation of P(r_j).
      end loop roots_test;
      --
      if not all_ok then
        Put_Line(" Test " & IImg(test) & " failed: P(x) = ");
        for t in reverse 0 .. d loop
          Put("   ");
          Put(a(t),7,8,0);
          Put(" x^" & IImg(t));
          New_Line;
        end loop;
        roots_show: for j in 1 .. d loop
          Put("Root r_" & IImg(j) & " = ");
          Put(r(j), 0, 6, 0);
          Put("   P(r_" & IImg(j) & ") = ");
          Put(Pr(j), 0, Integer'Max(4, ptol(d)),0);
          if not ok(j) then
            Put ("  BAD. Tolerance is ");
            Put (tol, 0, 2, 2);
          end if;
          New_Line;
        end loop roots_show;
      end if;
    end loop tests;
  end loop degrees;
end Test_Complex_Polynomial_Roots;
