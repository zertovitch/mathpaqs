with Discrete_Random_Simulation;
with Gamma_function;

with Ada.Calendar; use Ada.Calendar;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Test_Discrete_Random_Simulation is

  type Real is digits 18;
  --  We can also use the Real type, but this adds automatic checks when enabled:
  subtype Prob_Value is Real range 0.0 .. 1.0;
  type Pb_array is array(Integer range <>) of Prob_Value;

  package DRS is new Discrete_Random_Simulation(Prob_Value, Pb_array); use DRS;
  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real); use REF;
  package RG is new Gamma_function(Real); use RG;

  package RIO is new Float_IO(Real); use RIO;

  type Simulation_mode is (linear, dichotomic, alias);

  procedure Test_CDF_by_mode(F: Pb_array; mode: Simulation_mode; comment: String) is
    sample: array(F'Range) of Integer := (others => 0);
    use Ada.Numerics.Float_Random;
    g: Generator;
    n: constant := 50_000_000;
    u, pxi: Prob_Value;
    ii, x: Integer:= 0;
    t0, t1, t2: Time;
    aliases: Alias_tables (F'First, F'Last);
    probs: constant Pb_array (F'Range) := To_probs (F);
    diff, max_diff: Real := 0.0;
  begin
    Put_Line("---------");
    Put_Line(
      "Testing " & comment &
      ", total occurrences=" & Integer'Image(n) &
      ", inverse CDF mode= " & Simulation_mode'Image(mode));
    New_Line;
    Reset(g);
    t0 := Clock;
    if mode = alias then
      Prepare_Aliases (Fx => F, aliases => aliases);
    end if;
    t1 := Clock;
    for i in 1 .. n loop
      ii := i;
      u:= Real(Random(g));
      case mode is
        when linear =>
          x := Index_Linear_Search (u, F);
        when dichotomic =>
          x := Index_Dichotomic_Search (u, F);
        when alias =>
          x := Index_Alias_Method (u, aliases);
      end case;
      sample(x):= sample(x) + 1;
    end loop;
    t2 := Clock;
    --
    for xi in sample'Range loop
      pxi := Real(sample(xi)) / Real(n);
      diff := abs(pxi - probs(xi));
      max_diff := Real'Max(max_diff, diff);
      if xi not in sample'First + 5 .. sample'Last - 5 then
        Put(xi, 4);
        Put(": #occ:");
        Put(sample(xi), 9);
        Put(" ->");
        Put(pxi, 2, 5, 0);
        Put("; exact");
        Put(probs(xi), 2, 5, 0);
        Put("; diff");
        Put(diff, 2, Real'Digits, 0);
        New_Line;
      end if;
    end loop;
    New_Line;
    Put("Max diff");
    Put(max_diff, 2, 6, 0);
    New_Line;
    Put_Line (
      "Elapsed time for " & Simulation_mode'Image(mode) &
      ": " & Duration'Image(t1-t0) &
      " (prep.), " & Duration'Image(t2-t1) & " (simul.)"
    );
  exception
    when others =>
      Put_Line ("Error at iteration" & Integer'Image(ii));
  end Test_CDF_by_mode;

  procedure Test_CDF(F: Pb_array; comment: String) is
  begin
    for mode in Simulation_mode loop
      Test_CDF_by_mode(F, mode, comment);
    end loop;
  end Test_CDF;

  flip_coin: constant Pb_array := (0.0, 0.5);
  dice_1: constant Pb_array (1..6) := (0.0, 1.0/6.0, 2.0/6.0, 3.0/6.0, 4.0/6.0, 5.0/6.0);
  dice_other : constant Pb_array:= To_cumulative((1..6 => 1.0/6.0));

  empiric_a: constant Pb_array := To_cumulative(
    (0.01, 0.02, 0.04, 0.08, 0.16,  --  Sums to 0.31
     0.09,  -- to 0.4
     0.1,   -- to 0.5
     0.5    -- rest: 0.5
    ));

  max_poisson: constant := 25;
  truncated_poisson: Pb_array(0..max_poisson);
  procedure Fill_truncated_poisson is
    lambda: constant := 7.0;
    p: Real;
  begin
    Put("Poisson, lambda=");
    Put(lambda, 2,10,0);
    New_Line;
    for k in 0 .. max_poisson loop
      p:= Exp(-lambda) * (lambda ** k) / Gamma(Real(k+1));
      --  Put("            k="); Put(k, 3);
      --  Put("          p_k="); Put(p, 2,10,0);
      --  New_Line;
      truncated_poisson(k):= p;
    end loop;
  end Fill_truncated_poisson;

  big_one: Pb_array (1..200);
  sum: Real;
  use Ada.Numerics.Float_Random;
  gen: Generator;

  --  *WRONG* usage:
  flip_coin_wrong_side: constant Pb_array := (0.5, 1.0);
  dice_with_value_1: constant Pb_array := (0.0, 1.0/6.0, 2.0/6.0, 3.0/6.0, 4.0/6.0, 5.0/6.0, 1.0);

  do_wrongs: constant Boolean := False;

begin
  Test_CDF(flip_coin, "Flip or coin");
  Test_CDF(dice_1, "Dice index base 1");
  Test_CDF(dice_other, "Dice, using ""To_cumulative"" function");
  Test_CDF(empiric_a, "Empiric A: p = {0.01, 0.02, 0.04, 0.08, 0.16, 0.09, 0.1, 0.5}");
  Fill_truncated_poisson;
  Test_CDF(To_cumulative(truncated_poisson), "Truncated Poisson");
  --
  --  Probabilities are randomly set.
  sum := 0.0;
  for i in big_one'Range loop
    big_one(i) := Real(Random(gen));
    sum := sum + big_one(i);
  end loop;
  for i in big_one'Range loop
    big_one(i) := big_one(i) / sum;
  end loop;
  Test_CDF(To_cumulative(big_one), "Big CDF A, randomly set probs.");
  --
  --  Probabilities are exponenially decreasing.
  --  We test here the quasi-plateau's that slows down
  --  the dichotomic search and where the Alias method might be better.
  sum := 0.0;
  for i in big_one'Range loop
    --  We avoid too large exponents that would make p_i
    --  cumulate to 1 (numerically) before last index ->
    --  see "Caution #2" in spec.
    big_one(i) := Exp(12.0*Real(-i)/Real(big_one'Last));
    sum := sum + big_one(i);
  end loop;
  for i in big_one'Range loop
    big_one(i) := big_one(i) / sum;
  end loop;
  Test_CDF(To_cumulative(big_one), "Big CDF B, probs. expon. decr.");
  --
  if do_wrongs then
    New_Line(4);
    Put_Line("******");
    Put_Line("  Hereafter is a *WRONG* usage of the Cumulative_distribution_function arrays");
    Put_Line("  with prob. value 1.0, or both values 0.0 and 1.0.");
    Put_Line("******");
    Test_CDF(flip_coin_wrong_side, "Flip or coin, WRONG side CDF array");
    Test_CDF(dice_with_value_1,
             "Dice; prob. values 0.0 and 1.0 (WRONG - ""seventh"" face with 0 mathematical prob.)");
  end if;
end Test_Discrete_Random_Simulation;
