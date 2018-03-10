-- Testing the Gamma special function

with Gamma_function;

with Ada.Numerics.Generic_Elementary_Functions;

with Ada.Text_IO;                       use Ada.Text_IO;

with System;

procedure Test_Gamma is

  type Real is digits System.Max_Digits;
  -- 15 for Long_Float (double);
  -- 18 max. for GNAT on x86

  package RG is new Gamma_function(Real);
  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  use RG, REF;

  function Very_Close (x, y: Real) return Boolean is
  begin
    if abs y <= Real'Base'Model_Small then
      return abs x <= Real'Base'Model_Small;
    else
      return abs (x / y - 1.0) <= 1.0E-13;
    end if;
  end Very_Close;

  Different_Gamma_values: exception;
  Different_Log_Gamma_values: exception;

  procedure Test(x, pgx: Real; comment: String:= "") is
    -- pgx is precomputed Gamma(x) by Excel 2013, unless specified.
    gx: constant Real:= Gamma(x);
    lgx: constant Real:= Log_Gamma(x);
    lpgx: Real;  --  Log of the precomputed pgx, if possible
  begin
    if pgx < 0.0 then
      lpgx:= lgx;
    else
      lpgx:= Log(pgx);
    end if;
    Put_Line(
      Real'Image(x) & "; " &
      Real'Image(gx) & "; " & Real'Image(pgx) & "; " &
      Real'Image(lgx) & "; " & Real'Image(lpgx) & "; " & comment
    );
    if not Very_Close(gx, pgx) then
      raise Different_Gamma_values;
    end if;
    if not Very_Close(lgx, lpgx) then
      raise Different_Log_Gamma_values;
    end if;
  end Test;

  use Ada.Numerics;  --  for Pi.

begin
  Put_Line("Digits:" & Integer'Image(Real'Digits));
  Put_Line(" x;                        Gamma(x);                " &
           " Precalculated Gamma(x);   Log_Gamma(x);             Log(Precalculated Gamma(x))");
  -- Obvious (Integers), or computed by Excel 2013:
  Test( 1.0, 1.0   );
  Test( 1.2, 0.918168742399761 );
  Test( 1.4, 0.887263817503075 );
  Test( 1.6, 0.893515349287690 );
  Test( 1.8, 0.931383770980243 );
  Test( 2.0, 1.0,   "x = 2, Gamma(2) = (2-1)! = 1! = 1");
  Test( 3.0, 2.0,   "x = 3, Gamma(3) = (3-1)! = 2! = 2");
  Test( 4.0, 6.0,   "x = 4, Gamma(4) = (4-1)! = 3! = 3*2 = 6");
  Test( 5.0, 24.0,  "x = 5, Gamma(5) = (5-1)! = 4! = 4*3*2 = 24");
  Test( 6.0, 120.0, "x = 6, Gamma(6) = (6-1)! = 5! = 5*4*3*2 = 120");
  Test( 0.2, 4.590843711998800 );
  Test( 0.4, 2.218159543757690 );
  Test( 0.6, 1.489192248812820 );
  Test( 0.8, 1.164229713725300 );
  Test(-0.2, -5.821148568626520);
  Test(-0.4, -3.722980622032040);
  Test(-0.6, -3.696932572929480);
  Test(-0.8, -5.738554639998510);
  Test(16.93352719482360, 17367760312180.7);  -- random
  Test(14.25326873510880, 12067256954.6542);  -- random
  Test( 4.85101671452917, 19.22383767830230);   -- random
  Test(36.65329170123950, 1.07050304607199E+41);   -- random
  Test(61.02542818086580, 9.23600359736361E+81);   -- random
  Test(21.67334741103300, 1.880051017714E+19);   -- random
  Test(-1.43792716722682,  2.51400078003206);   -- random
  Test(-3.57579956638481,  0.249981780625623);  -- random
  Test(-6.55419960369584, -0.00153259988194934);  -- random
  Test(  +7.7,  2.76983036232732E+03);
  Test(  -7.7,  1.82074166841526E-04);
  Test(  10.0,  3.62880000000000E+05);
  Test(  20.0,  1.21645100408832E+17);
  Test(  30.0,  8.84176199373970E+30);
  Test( 171.0,  7.25741561530800E+306);
  Test(-170.5, -3.31273952153861E-308);
  Test( 123.0, 9.8750442008336E+202);
  Test( 0.1,       9.51350769866873,  "0.1");
  Test( 0.01,     99.4325851191506,  "0.01");
  Test( 0.001,   999.423772484596,  "0.001");
  Test( 0.0001, 9999.42288323162,  "0.0001");
  --  https://en.wikipedia.org/wiki/Particular_values_of_the_Gamma_function :
  Test( 0.5, Sqrt(Pi), "Gamma(1/2) = Sqrt(Pi)");
  Test( 1.5, 0.5 * Sqrt(Pi));
  Test( 2.5, 0.75 * Sqrt(Pi));
  Test( 3.5, (15.0/8.0) * Sqrt(Pi));
  Test(-0.5, -2.0 * Sqrt(Pi));
  Test(-1.5, (4.0/3.0) * Sqrt(Pi));
  Test(-2.5, -(8.0/15.0) * Sqrt(Pi));
  Test(1.0/3.0, 2.6789385347077476337);
  Test(-0.5040830082644554092582693045,-3.5446436111550050891219639933);
  Test(-5.6671624415568855358494741745, 0.0093245944826148505217119238);
  Test(-9.7026725400018637360844267649, 0.0000021574161045228505405031);
end Test_Gamma;
