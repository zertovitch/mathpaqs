with Ada.Text_IO;                       use Ada.Text_IO;

with Generic_Random_Functions;

procedure Test_Normal is

  subtype Real is Long_Float;

  package GRF is new Generic_Random_Functions(Real);

  package RIO is new Float_IO(Real);

  use GRF, RIO;

  max: constant := 10000;

  x: Real;
  y: Real;

begin
  for i in -max..max loop
    x:= Real(i) * 20.0 / Real(max);
    y:= Normal_CDF(x);
    Put("x= ");
    Put(x);
    Put("; F(x)= ");
    Put(y);
    New_Line;
  end loop;
  Skip_Line;
end Test_Normal;