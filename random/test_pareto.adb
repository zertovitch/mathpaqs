-- Test Pareto, eventually truncated

with Generic_Random_Functions;
with U_Rand;
with Samples;

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Test_Pareto is

  -- Several choices there:
  type Real is digits 15;
  only_mean : constant Boolean:= False;
  -- use_pow  : constant Boolean:= True;

  package Real_U_Rand is new U_Rand (Real);

  -- *** Choice of a random generator: A.N.F_R, or U_Rand (faster), or...:
  package RRand renames Real_U_Rand;
  use RRand;

  package REF is new Ada.Numerics.Generic_Elementary_Functions (Real);
  use REF;

  -- function Pow (X, Y : Long_Float) return Long_Float is
  -- pragma Import (C, Pow, "pow");

  package RRF is new Generic_Random_Functions (Real);
  use RRF;

  type Quantile_table is array(Positive range <>) of Real;

  quantiles : constant Quantile_table:=
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
    0.999999, -- 1 mio years
    1.0       -- infinity...
  );

  iter : constant:= 4_000_000;
  bins : constant:= 200_000;

  scale : constant:= 100.0; -- 1_000_000.0;

  minimum : constant:= scale * 5.0;
  maximum : constant:= scale * 10.0;
  big_maximum : constant:= maximum * 100.0;

  threshold : constant :=  scale;
  alpha     : constant :=  1.5;

  package My_Samples is new Samples (Real, Quantile_table, True);
  use My_Samples;

  samp_X, samp_Y: Sample(bins);
  meas_X, meas_Y: Measure(quantiles'Last);

  U, -- uniform
  X, -- Pareto
  Y  -- Truncated Pareto
  : Real;

  gen: Generator;

  T0, T1: Time;

  sum_X, sum_Y: Real:= 0.0;

begin
  Put_Line("Start");
  T0:= Clock;
  Reset(gen, 1);
  if not only_mean then
    Initialize(samp_X, 0.0, big_maximum);
    Initialize(samp_Y, minimum, maximum);
  end if;
  for i in 1..iter loop
    U:= Real(Random(gen));
    X:= Pareto_inverse_CDF(
      q               => U,
      threshold       => threshold,
      minus_inv_alpha => -1.0 / alpha
    );
    X:= Real'Min(big_maximum, X); -- well, we truncate too...
    Y:= Real'Min(maximum, Real'Max(minimum, X));
    if only_mean then
      sum_X:= sum_X + X;
      sum_Y:= sum_Y + Y;
    else
      Add_occurrence(samp_X, X);
      Add_occurrence(samp_Y, Y);
    end if;
  end loop;
  if only_mean then
    sum_X:= sum_X / Real(iter);
    sum_Y:= sum_Y / Real(iter);
  else
    meas_X.level:= quantiles;
    meas_Y.level:= quantiles;
    Get_measures(samp_X, meas_X);
    Get_measures(samp_Y, meas_Y);
  end if;
  T1:= Clock;
  --
  Put_Line("Duration in seconds;" & Duration'Image((T1-T0)));
  --
  if only_mean then
    Put_Line("Mean X;" & Real'Image(sum_X) & ";Mean Y;" & Real'Image(sum_Y));
  else
    Put_Line("Mean X;" & Real'Image(meas_X.mean) & ";Mean Y;" & Real'Image(meas_Y.mean));
    Put_Line("index; quantile; exact X value; statisitical X value; exact CDF of statistical x value; statisitical Y value");
    for q in quantiles'Range loop
      Put_Line(
        Integer'Image(q) & ';' &
        Real'Image(quantiles(q)) & ';' &
        Real'Image(Pareto_inverse_CDF(1.0-quantiles(q),threshold,-1.0/alpha)) & ';' & -- exact x value
        Real'Image(meas_X.VaR(q)) & ';' & -- stat. x value
        Real'Image(Pareto_CDF(meas_X.VaR(q), threshold, alpha)) & ';' & -- exact CDF of statistical x value
        Real'Image(meas_Y.VaR(q)) & ';'
        );
    end loop;
  end if;
end Test_Pareto;
