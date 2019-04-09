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

  procedure Test_external (x: Real; Phi_x_external: Real; sys: String) is
    Phi_x : constant Real := Normal_CDF (x);
  begin
    Put_Line (
      Real'Image (x) & "; " &
      Real'Image (Phi_x) & "; " &
      Real'Image (Phi_x_external) & "; " &
      Real'Image (abs (Phi_x - Phi_x_external)) & ";" & sys
    );
  end Test_external;

  pos_steps   : constant := 2000;
  total_steps : constant := 1 + 2 * pos_steps;
  bound       : constant := 8.0;  --  The x range tested will be: [-bound .. +bound]

  x, y, z, diff, max_diff, sum_diff, avg_diff : Real;

begin
  Put_Line ("Real'Digits =" & Integer'Image (Real'Digits));
  Put ("Real'First  ="); Put (Real'First); New_Line;
  Put ("Real'Last   ="); Put (Real'Last);  New_Line;
  New_Line;
  Put_Line ("Testing numerical integrity: Phi^{-1}(Phi(x)) = x .");
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
  New_Line;
  Put_Line ("Testing a few values of Phi(x) computed by other systems.");
  Put_Line ("x; F(x); F_ext(x); abs diff");
  --  Wolfram Alpha - https://www.wolframalpha.com/ :
  --    (1/sqrt(2*pi)) * int e^(-(t^2)/2) dt, t=-Infinity..x
  --    simplified to : 1/2 (1 + erf(x/sqrt(2)))
  --    then replaced x by numerical values.
  --
  --  Wide range around 0
  Test_external ( 2.92574259484689, 0.998281756504653,  "Excel 2002");
  Test_external ( 2.92574259484689, 0.9982818243270286, "Wolfram Alpha");
  Test_external (-3.13959349378472, 0.000845978859679408, "Excel 2002");
  Test_external (-3.13959349378472, 0.000845912096478163, "Wolfram Alpha");
  Test_external (-1.8344324597817, 0.033294853131841,    "Excel 2002");
  Test_external (-1.8344324597817, 0.033294912473320504, "Wolfram Alpha");
  Test_external (-3.8479175160397, 5.95848598385906E-05, "Excel 2002");
  Test_external (1.71534497414443, 0.956859036195643, "Excel 2002");
  Test_external (-1.4837389275487, 0.0689391294297074, "Excel 2002");
  Test_external (0.313262544183896, 0.622959332529198, "Excel 2002");
  Test_external (-1.33528356995208, 0.090891839019253, "Excel 2002");
  Test_external (-1.70667049934907, 0.0439416239849417, "Excel 2002");
  Test_external (0.902295892739891, 0.816550179338618, "Excel 2002");
  Test_external (3.56123259846485, 0.999815401180952, "Excel 2002");
  Test_external (-3.02138047666817, 0.00125819400952465, "Excel 2002");
  Test_external (-0.864391358049604, 0.193686422939807, "Excel 2002");
  Test_external (4.38878617828374, 0.999994296169036, "Excel 2002");
  Test_external (4.38232692457851, 0.99999412440124, "Excel 2002");
  Test_external (0.945996285569901, 0.827924775276737, "Excel 2002");
  Test_external (2.74279058199888, 0.996953969302392, "Excel 2002");
  Test_external (-0.940424188959881, 0.173499991852874, "Excel 2002");
  Test_external (-3.79268977498914, 7.45369454716593E-05, "Excel 2002");
  Test_external (4.10698983163692, 0.999979946570136, "Excel 2002");
  --  Narrow range around 0
  Test_external (0.665217254914504, 0.747044297125409,  "Excel 2002");
  Test_external (0.665217254914504, 0.7470442271516357, "Wolfram Alpha");
  Test_external (0.147223920954224, 0.558522381481477,  "Excel 2002");
  Test_external (0.147223920954224, 0.5585223599335758, "Wolfram Alpha");
  Test_external (0.308781691432673, 0.621256136213673, "Excel 2002");
  Test_external (0.967645560278044, 0.83338929941956, "Excel 2002");
  Test_external (-0.330298509333767, 0.370587273953922, "Excel 2002");
  Test_external (0.560993833072889, 0.712599166257638, "Excel 2002");
  Test_external (-0.0569113911552463, 0.477307820605794, "Excel 2002");
  Test_external (-0.504118535674396, 0.307089032841084, "Excel 2002");
  Test_external (-0.525484858238929, 0.299623068736426, "Excel 2002");
  Test_external (-0.364138764520489, 0.357877253289638, "Excel 2002");
  Test_external (0.215847644275459, 0.585446712270178, "Excel 2002");
  Test_external (0.262368429286052, 0.603481238250016, "Excel 2002");
  Test_external (-0.857001981679202, 0.195721845891328, "Excel 2002");
  Test_external (-0.193808071445531, 0.423163100973266, "Excel 2002");
  Test_external (-0.966415666556947, 0.166918106410473, "Excel 2002");
  Test_external (-0.745607255772077, 0.227952276241425, "Excel 2002");
  Test_external (-0.186204937569133, 0.426142037654162, "Excel 2002");
  Test_external (-0.296037866451158, 0.383600646078336, "Excel 2002");
  Test_external (-0.965232416827455, 0.167214198868332, "Excel 2002");
  Test_external (0.875088337567958, 0.809237121585063, "Excel 2002");
  --  Narrower range around 0: [-1/2 ; 1/2]
  Test_external (-0.0749623361718745, 0.470122270435875,  "Excel 2002");
  Test_external (-0.0749623361718745, 0.4701223394613545, "Wolfram Alpha");
  Test_external (-0.450646843702305, 0.326122070242433, "Excel 2002");
  Test_external (0.408674999664122, 0.658610867247291, "Excel 2002");
  Test_external (-0.191501298662019, 0.424066447405214, "Excel 2002");
  Test_external (0.477072840308855, 0.68334486665824, "Excel 2002");
  Test_external (-0.456308326731822, 0.324084156117255, "Excel 2002");
  Test_external (-0.192749833223648, 0.42357746373417, "Excel 2002");
  Test_external (0.215214910551101, 0.5852000839114, "Excel 2002");
  Test_external (-0.16303647764164, 0.435244849159761, "Excel 2002");
  Test_external (0.0300800814537923, 0.511998460058164, "Excel 2002");
  Test_external (-0.372193042306193, 0.354874618345957, "Excel 2002");
  Test_external (-0.0965414891169869, 0.461545201846925, "Excel 2002");
  Test_external (0.0773623820716502, 0.530832435525767, "Excel 2002");
  Test_external (-0.0127186743229086, 0.494926092020094, "Excel 2002");
  Test_external (0.301226474714348, 0.618379033723788, "Excel 2002");
  Test_external (-0.443464830387831, 0.328714800529773, "Excel 2002");
  Test_external (-0.0567722388841017, 0.477363244747432, "Excel 2002");
  Test_external (-0.348831080762905, 0.363608125598786, "Excel 2002");
  Test_external (0.341282827506405, 0.633554599926591, "Excel 2002");
  Test_external (-0.0500240596004655, 0.480051540263603, "Excel 2002");
  --  Skip_Line;
end Test_Normal;
