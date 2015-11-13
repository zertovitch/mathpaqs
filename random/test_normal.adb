with Ada.Text_IO;                       use Ada.Text_IO;

with Generic_Random_Functions;
with System;

procedure Test_Normal is

  type Real is digits System.Max_Digits;
  -- 15 for Long_Float (double);
  -- 18 max. for GNAT on x86

  package GRF is new Generic_Random_Functions(Real);
  package RIO is new Float_IO(Real);
  use GRF, RIO;

  max: constant := 1000;

  x, y, z: Real;

begin
  Put_Line("Digits:" & Integer'Image(Real'Digits));
  Put(Real'First); New_Line;
  Put(Real'Last);  New_Line;
  for i in -max..max loop
    x:= Real(i) * 8.0 / Real(max);
    y:= Normal_CDF(x);
    z:= Normal_inverse_CDF(y);
    Put("x=;");
    Put(x);
    Put("; y=F(x)=;");
    Put(y);
    Put("; z={F^-1}(y)=;");
    Put(z);
    Put("; x-z=;");
    Put(x-z);
    New_Line;
  end loop;
  --  Skip_Line;
end Test_Normal;
