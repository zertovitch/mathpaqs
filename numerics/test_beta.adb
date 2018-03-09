-- Testing the Beta special function

-- ** TO DO **:
--
--   - test randomized with incomplete and inverse incomplete
--   - try gcov to see if all branches are covered
--

with Beta_function;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics; use Ada.Numerics;

with Ada.Text_IO;                       use Ada.Text_IO;

with System;

procedure Test_Beta is

  type Real is digits System.Max_Digits;
  -- 15 for Long_Float (double);
  -- 18 max. for GNAT on x86

  package RB is new Beta_function(Real);
  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  use RB, REF;

  function Very_Close (x, y: Real) return Boolean is
  begin
    if abs y <= Real'Base'Model_Small then
      return abs x <= Real'Base'Model_Small;
    else
      return abs (x / y - 1.0) <= 1.0E-13;
    end if;
  end Very_Close;

  Different_Beta_values: exception;
  Different_Incomplete_Beta_values: exception;
  Different_Regularized_Beta_values: exception;

  procedure Test_complete(a, b, pbab: Real; comment: String:= "") is
    -- pbab is precomputed Beta(a,b), using another tool.
    bab: constant Real:= Beta(a, b);
  begin
    Put_Line(
      Real'Image(a) & "; " &
      Real'Image(b) & "; " &
      Real'Image(bab) & "; " & Real'Image(pbab) & "; " & comment
    );
    if not Very_Close(bab, pbab) then
      raise Different_Beta_values;
    end if;
  end Test_complete;

  procedure Test_incomplete(x, a, b, pbxab: Real; comment: String:= "") is
    -- pbabx is precomputed Beta(a,b,x), using another tool.
    bxab: constant Real:= Beta(x, a, b);
  begin
    Put_Line(
      Real'Image(x) & "; " &
      Real'Image(a) & "; " &
      Real'Image(b) & "; " &
      Real'Image(bxab) & "; " &
      Real'Image(pbxab) & "; " & comment
    );
    if not Very_Close(bxab, pbxab) then
      raise Different_Incomplete_Beta_values;
    end if;
  end Test_incomplete;

  procedure Test_regularized(x, a, b, pbxab: Real; comment: String:= "") is
    -- pbabx is precomputed Regularized_Beta(a,b,x), using another tool.
    bxab: constant Real:= Regularized_Beta(x, a, b);
  begin
    Put_Line(
      Real'Image(x) & "; " &
      Real'Image(a) & "; " &
      Real'Image(b) & "; " &
      Real'Image(bxab) & "; " &
      Real'Image(pbxab) & "; " & comment
    );
    if not Very_Close(bxab, pbxab) then
      raise Different_Regularized_Beta_values;
    end if;
  end Test_regularized;

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
  Test_incomplete( 0.01, 5.0, 4.0,  1.95042732142857142857142857142857142857E-11,  "a=5, b=4");
  Test_incomplete( 0.1 , 5.0, 4.0,  1.541607142857142857142857142857142857E-6,     "a=5, b=4");
  Test_incomplete( 0.5 , 5.0, 4.0,  0.0012974330357142857142857142857142857142857, "a=5, b=4");
  Test_incomplete( 0.9 , 5.0, 4.0,  0.0035534844642857142857142857142857142857,    "a=5, b=4");
  Test_incomplete( 0.99, 5.0, 4.0,  0.0035714261504342732142857142857142857142857, "a=5, b=4");
  --
  Test_incomplete( 0.01, 0.5, 0.5,  0.2003348423231195926910463589053866371373519, "a=0.5, b=0.5");
  Test_incomplete( 0.1 , 0.5, 0.5,  0.6435011087932843868028092287173226380415105, "a=0.5, b=0.5");
  Test_incomplete( 0.5 , 0.5, 0.5,  1.5707963267948966192313216916397514420985846, "a=0.5, b=0.5");
  Test_incomplete( 0.9 , 0.5, 0.5,  2.4980915447965088516598341545621802461556588, "a=0.5, b=0.5");
  Test_incomplete( 0.99, 0.5, 0.5,  2.9412578112666736457715970243741162470598174, "a=0.5, b=0.5");
  --
  Test_incomplete( 0.01, 5.0, 1.0,  2.0E-11,       "a=5, b=1");
  Test_incomplete( 0.1 , 5.0, 1.0,  2.0E-06,       "a=5, b=1");
  Test_incomplete( 0.5 , 5.0, 1.0,  0.00625,       "a=5, b=1");
  Test_incomplete( 0.9 , 5.0, 1.0,  0.118098,      "a=5, b=1");
  Test_incomplete( 0.99, 5.0, 1.0,  0.19019800998, "a=5, b=1");
  --
  Test_incomplete( 0.01, 1.0, 3.0,  0.0099003333333333333333333333333333333333333, "a=1, b=3");
  Test_incomplete( 0.1 , 1.0, 3.0,  0.0903333333333333333333333333333333333333333, "a=1, b=3");
  Test_incomplete( 0.5 , 1.0, 3.0,  0.2916666666666666666666666666666666666666666, "a=1, b=3");
  Test_incomplete( 0.9 , 1.0, 3.0,  0.333,                                         "a=1, b=3");
  Test_incomplete( 0.99, 1.0, 3.0,  0.333333,                                      "a=1, b=3");
  --
  Test_incomplete( 0.01, 2.0, 2.0,  0.0000496666666666666666666666666666666666666, "a=2, a=2");
  Test_incomplete( 0.1 , 2.0, 2.0,  0.0046666666666666666666666666666666666666666, "a=2, a=2");
  Test_incomplete( 0.5 , 2.0, 2.0,  0.0833333333333333333333333333333333333333333, "a=2, a=2");
  Test_incomplete( 0.9 , 2.0, 2.0,  0.162,                                         "a=2, a=2");
  Test_incomplete( 0.99, 2.0, 2.0,  0.166617,                                      "a=2, a=2");
  --
  Test_incomplete( 0.01, 2.0, 5.0,  0.0000486815868333333333333333333333333333333, "a=2, a=5");
  Test_incomplete( 0.1 , 2.0, 5.0,  0.0038088333333333333333333333333333333333333, "a=2, a=5");
  Test_incomplete( 0.5 , 2.0, 5.0,  0.0296875,                                     "a=2, a=5");
  Test_incomplete( 0.9 , 2.0, 5.0,  0.0333315,                                     "a=2, a=5");
  Test_incomplete( 0.99, 2.0, 5.0,  0.0333333333135,                               "a=2, a=5");
  --
  Put_Line(" x;                        a;                        b;                       " &
           " Regularized_Beta(x,a,b);  Precomp. Reg.Beta(x,a,b);");
  Test_regularized( 0.1, 5.0, 4.0,  0.00043165);  --  Excel
  Test_regularized( 0.2, 5.0, 4.0,  0.01040640);  --  Excel
  Test_regularized( 0.3, 5.0, 4.0,  0.05796765);  --  Excel
  Test_regularized( 0.4, 5.0, 4.0,  0.17367040);  --  Excel
  Test_regularized( 0.5, 5.0, 4.0,  0.36328125);  --  Excel
  Test_regularized( 0.6, 5.0, 4.0,  0.59408640);  --  Excel
  Test_regularized( 0.7, 5.0, 4.0,  0.80589565);  --  Excel
  Test_regularized( 0.8, 5.0, 4.0,  0.94371840);  --  Excel
  Test_regularized( 0.9, 5.0, 4.0,  0.99497565);  --  Excel
end Test_Beta;
