with Ada.Calendar;                      use Ada.Calendar;
with Ada.Text_IO;                       use Ada.Text_IO;

with Ada.Numerics.Generic_Elementary_Functions;

with Ada.Numerics.Float_Random;
-- GNAT is using http://fr.wikipedia.org/wiki/Blum_Blum_Shub
-- The generator is not appropriate for use in simulations,
-- only for cryptography, because it is very slow.
--
with U_Rand;

with Samples;

procedure Test_Pareto is

  -- Several choices there:
  type Real is digits 15;
  only_mean: constant Boolean:= True;
  -- use_pow  : constant Boolean:= True;

  package Real_U_Rand is new U_Rand(Real);

  -- *** Choice of a random generator: A.N.F_R, or U_Rand (faster), or...:
  package RRand renames
    Ada.Numerics.Float_Random;
    -- Real_U_Rand;

  use RRand;

  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  use REF;

  -- function Pow (X, Y : Long_Float) return Long_Float is
  -- pragma Import (C, Pow, "pow");

  type Quantile_table is array(Positive range <>) of Real;

  quantiles: constant Quantile_table:=
  (
    0.1,
    0.2,
    0.3,
    0.4,
    0.5,
    0.66666666666666666,
    0.75,
    0.8,
    0.875,
    0.9,
    0.93333333333333333,
    0.95,
    0.96,
    0.96666666666666666,
    0.975,
    0.98,
    0.98333333333333328,
    0.9875,
    0.99,
    0.99166666666666666,
    0.99333333333333333,
    0.99444444444444444,
    0.995,
    0.996,
    0.9966666666666667,
    0.99714285714285711,
    0.9975,
    0.998,
    0.9986666666666666,
    0.999,
    0.9995,
    0.9996666666666666,
    0.9998,
    0.9999,   -- 10 k years
    0.999999, -- 1 mio years (added to PM's list)
    1.0       -- infinity... (added to PM's list)
  );

  iter: constant:= 40_000_000;
  bins: constant:= 10_000;

  retention: constant:= 5_000_000.0;
  exposure : constant:= 5_000_000.0;

  k    : constant:=  1_000_000.0;
  alpha: constant:=  0.8;

  package My_Samples is new Samples(Real, Quantile_table);
  use My_Samples;

  samp: Sample(bins);
  meas: Measure(quantiles'Last);

  U, -- uniform
  X, -- ground-up
  Y  -- to layer
  : Real;

  gen: Generator;

  T0, T1: Time;

  sum: Real;

begin
  Put_Line("Start");
  T0:= Clock;
  Reset(gen, 1);
  if only_mean then
    sum:= 0.0;
  else
    Initialize(samp, 0.0, exposure);
  end if;
  for i in 1..iter loop
    U:= Real(Random(gen));
    -- X:= k * Pow(U, (-1.0/alpha));
    -- X:= k * (U ** (-1.0/alpha));
    -- Y:= Real'Min(exposure, Real'Max(0.0, X - retention));
    Y:= U * exposure;
    if only_mean then
      sum:= sum + Y;
    else
      Add_occurence(samp, Y);
    end if;
  end loop;
  if only_mean then
    sum:= sum / Real(iter);
  else
    meas.level:= quantiles;
    Get_Measures(samp, meas);
  end if;
  T1:= Clock;
  --
  Put_Line("Duration in seconds;" & Duration'Image((T1-T0)));
  --
  if only_mean then
    Put_Line("Mean;" & Real'Image(sum));
  else
    Put_Line("Mean;" & Real'Image(meas.mean));
    for q in quantiles'Range loop
      Put_Line(Integer'Image(q) & ';' & Real'Image(quantiles(q)) & ';' & Real'Image(meas.VaR(q)));
    end loop;
  end if;
end Test_Pareto;
