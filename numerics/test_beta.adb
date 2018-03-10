-- Testing the Beta special function

-- ** TO DO **:
--
--   - list pathological cases of inverse incomplete inaccuracy (could be a wrong epsilon...)
--   - try gcov to see if all branches are covered
--

with Beta_function;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Float_Random;

with Ada.Text_IO;                       use Ada.Text_IO;

with System;

procedure Test_Beta is

  type Real is digits System.Max_Digits;
  -- 15 for Long_Float (double);
  -- 18 max. for GNAT on x86

  package RB is new Beta_function(Real);
  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  use RB, REF;

  --  Check relative quasi-equality
  --
  function Very_Close (x, y, tol: Real) return Boolean is
  begin
    if abs y <= Real'Base'Model_Small then
      return abs x <= Real'Base'Model_Small;
    else
      return abs (x / y - 1.0) <= tol;
    end if;
  end Very_Close;

  Different_Beta_values: exception;
  Different_Incomplete_Beta_values: exception;
  Different_Inverse_Beta_values_1: exception;
  Different_Inverse_Beta_values_2: exception;
  Different_Regularized_Beta_values: exception;
  Different_Symmetric_Regularized_Beta_values: exception;
  Different_Inverse_Regularized_Beta_values: exception;

  procedure Test_complete(a, b, pbab: Real; comment: String:= "") is
    -- pbab is precomputed Beta(a,b), using another tool.
    bab: constant Real:= Beta(a, b);
  begin
    Put_Line(
      Real'Image(a) & "; " &
      Real'Image(b) & "; " &
      Real'Image(bab) & "; " & Real'Image(pbab) & "; " & comment
    );
    if not Very_Close(bab, pbab, 1.0E-13) then
      raise Different_Beta_values;
    end if;
  end Test_complete;

  procedure Test_incomplete_and_inverse(x, a, b, pBxab: Real; comment: String:= "") is
    -- pBabx is precomputed Beta(x,a,b), using another tool.
    Bxab: constant Real:= Beta(x, a, b);
    x_again: constant Real:= Inverse_Beta (Bxab, a, b);
    x_again_from_precomp: constant Real:= Inverse_Beta (pBxab, a, b);
  begin
    Put_Line(
      Real'Image(x) & "; " &
      Real'Image(a) & "; " &
      Real'Image(b) & "; " &
      Real'Image(Bxab) & "; " &
      Real'Image(pBxab) & "; " & comment
    );
    if not Very_Close(Bxab, pBxab, 1.0E-13) then
      raise Different_Incomplete_Beta_values;
    end if;
    if not Very_Close(x, x_again, 1.0E-13) then
      raise Different_Inverse_Beta_values_1;
    end if;
    if not Very_Close(x, x_again_from_precomp, 1.0E-12) then
      raise Different_Inverse_Beta_values_2;
    end if;
  end Test_incomplete_and_inverse;

  --  Specific test when the "precomputed" value is I_x(a, b)
  --  Typically, Excel, or some exact values
  --
  procedure Test_regularized(x, a, b, pBxab: Real; comment: String:= "") is
    -- pBxab is precomputed Regularized_Beta(x,a,b), using another tool.
    Bxab: constant Real:= Regularized_Beta(x, a, b);
    y_sym: constant Real:= 1.0 - Regularized_Beta(1.0 - x, b, a);
  begin
    Put_Line(
      Real'Image(x) & "; " &
      Real'Image(a) & "; " &
      Real'Image(b) & "; " &
      Real'Image(Bxab) & "; " &
      Real'Image(pBxab) & "; " & comment
    );
    if not Very_Close(Bxab, pBxab, 1.0E-13) then
      raise Different_Regularized_Beta_values;
    end if;
    --  Check accuracy using symmetry property
    --
    if not Very_Close(Bxab, y_sym, 1.0E-13) then
      raise Different_Symmetric_Regularized_Beta_values;
    end if;
  end Test_regularized;

  procedure Test_regularized_on_analytic_values is
    use Ada.Numerics.Float_Random;
    gen: Generator;
    x, y, a, b, diff_yb1, diff_ya1, max_diff_yb1, max_diff_ya1: Real;
    iter : constant := 1_000_000;
  begin
    Put_Line(
      "Random test with Regularized on cases with an analytic value; #iterations:" &
      Integer'Image(iter)
    );
    max_diff_ya1 := 0.0;
    max_diff_yb1 := 0.0;
    for i in 1 .. iter loop
      --  I_x(a,1) = x^a
      x := Real (Random (gen));
      a := Real (Random (gen)) * 20.0;
      y := Regularized_Beta (x, a, 1.0);
      diff_yb1 := abs(y - x ** a);
      max_diff_yb1 := Real'Max (max_diff_yb1, diff_yb1);
      --  I_x(1,b) = 1 - (1-x)^b
      x := Real (Random (gen));
      b := Real (Random (gen)) * 20.0;
      y := Regularized_Beta (x, 1.0, b);
      diff_ya1 := abs(y-(1.0-(1.0-x)**b));
      max_diff_ya1 := Real'Max (max_diff_ya1, diff_ya1);
    end loop;
    Put_Line ("Maximum difference for cases where a=1: " & Real'Image(max_diff_ya1));
    Put_Line ("Maximum difference for cases where b=1: " & Real'Image(max_diff_yb1));
  end Test_regularized_on_analytic_values;

  procedure Test_inverse_regularized(y, a, b, pbiyab: Real; comment: String:= "") is
    -- pbiyab is precomputed Inverse_Regularized_Beta(y,a,b), using another tool.
    biyab: constant Real:= Inverse_Regularized_Beta(y, a, b);
    diff: constant Real := abs(biyab - pbiyab);
  begin
    Put_Line(
      Real'Image(y) & "; " &
      Real'Image(a) & "; " &
      Real'Image(b) & "; " &
      Real'Image(biyab)  & "; " &
      Real'Image(pbiyab) & "; " &
      Real'Image(diff)   & "; " & comment
    );
    --  !!  The inverse function seems much less acurate...
    --      We search the tolerance with the number below.
    if not Very_Close(biyab, pbiyab, 1.0E-5) then
      raise Different_Inverse_Regularized_Beta_values;
    end if;
  end Test_inverse_regularized;

  procedure Test_regularized_and_inverse_regularized is
    use Ada.Numerics.Float_Random;
    gen: Generator;
    x, y, x2, y2, a, b, diff_x, diff_y, max_diff_x, max_diff_y: Real;
    iter : constant := 100_000;
  begin
    Put_Line(
      "Random test with Regularized then Inverse_Regularized, or vice versa; #iterations:" &
      Integer'Image(iter)
    );
    Reset (gen, 1);
    max_diff_x := 0.0;
    max_diff_y := 0.0;
    for i in 1 .. iter loop
      x := Real (Random (gen));  --  Must be in [0;1], the domain of Regularized_Beta
      a := Real (Random (gen)) * 5.0;
      b := Real (Random (gen)) * 5.0;
      y := Regularized_Beta (x, a, b);
      x2 := Inverse_Regularized_Beta (y, a, b);
      diff_x := abs(x-x2);
      --  Put_Line (Real'Image(abs(x-x2)));
      max_diff_x := Real'Max (max_diff_x, diff_x);
      --  Now the other way round
      y := Real (Random (gen));  --  Must be in [0;1] (possible values of Regularized_Beta)
      x := Inverse_Regularized_Beta (y, a, b);
      y2 := Regularized_Beta (x, a, b);
      diff_y := abs(y-y2);
      max_diff_y := Real'Max (max_diff_y, diff_y);
    end loop;
    Put_Line ("Maximum difference between x and IRB(RB(x)): " & Real'Image(max_diff_x));
    Put_Line ("Maximum difference between y and RB(IRB(y)): " & Real'Image(max_diff_y));
  end Test_regularized_and_inverse_regularized;

begin
  Put_Line("Digits:" & Integer'Image(Real'Digits));
  Put_Line(" a;                        b;                       " &
           " Beta(a,b);                Precomputed Beta(a,b);");
  --
  --  https://www.wolframalpha.com/input/?i=Beta%5B5,+4%5D
  --
  Test_complete( 5.0,  4.0,  0.003571428571428571428571428571428571428571428571428571428);
  Test_complete( 0.1,  3.5,  8.505924710949334225853324032128878460780976159294903434800);
  Test_complete( 1.23, 4.56, 0.136801545778037926966061632922397598240588019774266210675);
  --
  Put_Line(" x;                        a;                        b;                       " &
           " Beta(x,a,b);              Precomputed Beta(x,a,b);");
  --  Wolfram
  Test_incomplete_and_inverse( 0.01, 5.0, 4.0,  1.95042732142857142857142857E-11,  "a=5, b=4");
  Test_incomplete_and_inverse( 0.1 , 5.0, 4.0,  1.541607142857142857142857E-6,     "a=5, b=4");
  Test_incomplete_and_inverse( 0.5 , 5.0, 4.0,  0.0012974330357142857142857142857, "a=5, b=4");
  Test_incomplete_and_inverse( 0.9 , 5.0, 4.0,  0.0035534844642857142857142857,    "a=5, b=4");
  Test_incomplete_and_inverse( 0.99, 5.0, 4.0,  0.0035714261504342732142857142857, "a=5, b=4");
  --
  Test_incomplete_and_inverse( 0.01, 0.5, 0.5,  0.2003348423231195926910463589053, "a=0.5, b=0.5");
  Test_incomplete_and_inverse( 0.1 , 0.5, 0.5,  0.6435011087932843868028092287173, "a=0.5, b=0.5");
  Test_incomplete_and_inverse( 0.5 , 0.5, 0.5,  1.5707963267948966192313216916397, "a=0.5, b=0.5");
  Test_incomplete_and_inverse( 0.9 , 0.5, 0.5,  2.4980915447965088516598341545621, "a=0.5, b=0.5");
  Test_incomplete_and_inverse( 0.99, 0.5, 0.5,  2.9412578112666736457715970243741, "a=0.5, b=0.5");
  --
  Test_incomplete_and_inverse( 0.01, 5.0, 1.0,  2.0E-11,       "a=5, b=1");
  Test_incomplete_and_inverse( 0.1 , 5.0, 1.0,  2.0E-06,       "a=5, b=1");
  Test_incomplete_and_inverse( 0.5 , 5.0, 1.0,  0.00625,       "a=5, b=1");
  Test_incomplete_and_inverse( 0.9 , 5.0, 1.0,  0.118098,      "a=5, b=1");
  Test_incomplete_and_inverse( 0.99, 5.0, 1.0,  0.19019800998, "a=5, b=1");
  --
  Test_incomplete_and_inverse( 0.01, 1.0, 3.0,  0.00990033333333333333333333333333333, "a=1, b=3");
  Test_incomplete_and_inverse( 0.1 , 1.0, 3.0,  0.09033333333333333333333333333333333, "a=1, b=3");
  Test_incomplete_and_inverse( 0.5 , 1.0, 3.0,  0.29166666666666666666666666666666666, "a=1, b=3");
  Test_incomplete_and_inverse( 0.9 , 1.0, 3.0,  0.333,                                 "a=1, b=3");
  Test_incomplete_and_inverse( 0.99, 1.0, 3.0,  0.333333,                              "a=1, b=3");
  --
  Test_incomplete_and_inverse( 0.01, 2.0, 2.0,  0.00004966666666666666666666666666666, "a=2, a=2");
  Test_incomplete_and_inverse( 0.1 , 2.0, 2.0,  0.00466666666666666666666666666666666, "a=2, a=2");
  Test_incomplete_and_inverse( 0.5 , 2.0, 2.0,  0.08333333333333333333333333333333333, "a=2, a=2");
  Test_incomplete_and_inverse( 0.9 , 2.0, 2.0,  0.162,                                 "a=2, a=2");
  Test_incomplete_and_inverse( 0.99, 2.0, 2.0,  0.166617,                              "a=2, a=2");
  --
  Test_incomplete_and_inverse( 0.01, 2.0, 5.0,  0.00004868158683333333333333333333333, "a=2, a=5");
  Test_incomplete_and_inverse( 0.1 , 2.0, 5.0,  0.00380883333333333333333333333333333, "a=2, a=5");
  Test_incomplete_and_inverse( 0.5 , 2.0, 5.0,  0.0296875,                             "a=2, a=5");
  Test_incomplete_and_inverse( 0.9 , 2.0, 5.0,  0.0333315,                             "a=2, a=5");
  Test_incomplete_and_inverse( 0.99, 2.0, 5.0,  0.0333333333135,                       "a=2, a=5");
  --
  Put_Line(" x;                        a;                        b;                       " &
           " Regularized_Beta(x,a,b);  Precomp. Reg.Beta(x,a,b);");
  Test_regularized( 0.1, 5.0, 4.0,  0.00043165,"Excel 2013");
  Test_regularized( 0.2, 5.0, 4.0,  0.01040640,"Excel 2013");
  Test_regularized( 0.3, 5.0, 4.0,  0.05796765,"Excel 2013");
  Test_regularized( 0.4, 5.0, 4.0,  0.17367040,"Excel 2013");
  Test_regularized( 0.5, 5.0, 4.0,  0.36328125,"Excel 2013");
  Test_regularized( 0.6, 5.0, 4.0,  0.59408640,"Excel 2013");
  Test_regularized( 0.7, 5.0, 4.0,  0.80589565,"Excel 2013");
  Test_regularized( 0.8, 5.0, 4.0,  0.94371840,"Excel 2013");
  Test_regularized( 0.9, 5.0, 4.0,  0.99497565,"Excel 2013");
  Test_regularized( 0.9, 5.0, 4.0,  0.99497565,"Excel 2013");
  --
  --  Exact values
  Test_regularized( 0.1234, 6.54321, 1.0,  0.1234 ** 6.54321, "I_x(a,1) = x^a");
  Test_regularized( 0.7531, 1.0, 1.246,  1.0 - (1.0 - 0.7531) ** 1.246, "I_x(1,b) = 1 - (1-x)^b");
  --  We do the same, randomized.
  Test_regularized_on_analytic_values;
  --
  Put_Line(" y;                        a;                        b;                       " &
           " Inv.Reg.Beta(y,a,b);      Precomp. I.R.Beta(y,a,b); difference;");
  Test_inverse_regularized( 0.01, 5.0, 4.0,  0.198202133178711, "Excel 2002");
  Test_inverse_regularized( 0.1 , 5.0, 4.0,  0.344623088836670, "Excel 2002");
  Test_inverse_regularized( 0.9 , 5.0, 4.0,  0.760338306427001, "Excel 2002");
  Test_inverse_regularized( 0.99, 5.0, 4.0,  0.879049301147460, "Excel 2002");
  --
  Test_inverse_regularized( 0.01, 2.0, 5.0,  0.026762962341309, "Excel 2002");
  Test_inverse_regularized( 0.1 , 2.0, 5.0,  0.092595100402832, "Excel 2002");
  Test_inverse_regularized( 0.9 , 2.0, 5.0,  0.510316371917724, "Excel 2002");
  Test_inverse_regularized( 0.99, 2.0, 5.0,  0.705684661865234, "Excel 2002");
  --
  Test_regularized_and_inverse_regularized;
end Test_Beta;
