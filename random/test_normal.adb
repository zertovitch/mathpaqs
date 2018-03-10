--  Testing Normal_CDF and Normal_inverse_CDF.
--  For a test of simulations using Normal_inverse_CDF
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

  max : constant := 2000;
  bound : constant := 8.0;

  x, y, z, diff, max_diff, sum_diff, avg_diff : Real;

begin
  Put_Line ("Real'Digits =" & Integer'Image (Real'Digits));
  Put ("Real'First ="); Put (Real'First); New_Line;
  Put ("Real'Last ="); Put (Real'Last);  New_Line;
  Put_Line ("Total steps =" & Integer'Image (max*2));
  max_diff:= 0.0;
  sum_diff:= 0.0;
  for i in -max .. max loop
    x:= Real(i) * bound / Real(max);
    y:= Normal_CDF(x);
    z:= Normal_inverse_CDF(y);
    diff:= x-z;
    max_diff:= Real'Max(max_diff, abs diff);
    sum_diff:= sum_diff + abs diff;
    if abs i > max - 3 or else abs i < 4 then
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
  avg_diff:= sum_diff / Real(1 + 2 * max);
  Put("  Average diff = "); Put(avg_diff); New_Line;
  Put("  Maximum diff = "); Put(max_diff); New_Line;
--  Skip_Line;
end Test_Normal;
