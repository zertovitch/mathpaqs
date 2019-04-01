--  Testing the Normal_CDF and Normal_inverse_CDF functions.
--
--  For a test of **simulations** using Normal_inverse_CDF
--  vs. the Box-Muller method, see Test_Samples.

with Ada.Text_IO;                       use Ada.Text_IO;

with Generic_Random_Functions;
with System;

procedure Test_Normal is

  type Real is digits System.Max_Digits;
  -- 15 for Long_Float (double);
  -- 18 max. for GNAT on x86

  package GRF is new Generic_Random_Functions (Real);
  package RIO is new Float_IO (Real);
  use GRF, RIO;

  pos_steps   : constant := 2000;
  total_steps : constant := 1 + 2 * pos_steps;
  bound       : constant := 8.0;  --  The x range tested will be: [-bound .. +bound]

  x, y, z, diff, max_diff, sum_diff, avg_diff : Real;

begin
  Put_Line ("Real'Digits =" & Integer'Image (Real'Digits));
  Put ("Real'First  ="); Put (Real'First); New_Line;
  Put ("Real'Last   ="); Put (Real'Last);  New_Line;
  Put_Line ("Total steps =" & Integer'Image (total_steps));
  max_diff:= 0.0;
  sum_diff:= 0.0;
  for i in -pos_steps .. pos_steps loop
    x:= Real(i) * bound / Real(pos_steps);
    y:= Normal_CDF(x);
    z:= Normal_inverse_CDF(y);  --  Ideally, x = z.
    diff:= x-z;
    --
    --  We take absolute differences to avoid errors to compensate (sum)
    --  or to hide on negative values (max).
    --
    max_diff:= Real'Max(max_diff, abs diff);
    sum_diff:= sum_diff + abs diff;
    --
    --  Display some values around 0 and close to bounds:
    --
    if abs i > pos_steps - 3 or else abs i < 4 then
      Put("    x=;");
      Put(x);
      Put("; y=F(x)=;");
      Put(y);
      Put("; z={F^-1}(y)=;");
      Put(z);
      Put("; diff = x-z=;");
      Put(diff);
      New_Line;
    end if;
  end loop;
  avg_diff:= sum_diff / Real(total_steps);
  Put("  Average abs diff = "); Put(avg_diff); New_Line;
  Put("  Maximum abs diff = "); Put(max_diff); New_Line;
--  Skip_Line;
end Test_Normal;
