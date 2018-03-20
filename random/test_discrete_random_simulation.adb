with Discrete_Random_Simulation;

with Ada.Calendar; use Ada.Calendar;
with Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Discrete_Random_Simulation is

  type Real is digits 18;
  type CDF is array(Integer range <>) of Real;

  package DRS is new Discrete_Random_Simulation(Real, CDF); use DRS;

  package RIO is new Float_IO(Real); use RIO;

  --  Passing of mode looks somewhat complicated, but we want
  --  to be sure it is done at compile-time.
  generic
    mode: Discrete_random_simulation_mode;
  procedure Test_CDF_by_mode(F: CDF; comment: String);

  procedure Test_CDF_by_mode(F: CDF; comment: String) is
    sample: array(F'Range) of Integer := (others => 0);
    use Ada.Numerics.Float_Random;
    g: Generator;
    n: constant := 50_000_000;
    u: Real;
    x: Integer;
    function Index_any is new Index(mode);
    t0, t1: Time;
  begin
    Put_Line("---------");
    Put_Line(
      "Testing " & comment &
      ", total occurrences=" & Integer'Image(n) &
      ", inverse CDF mode= " & Discrete_random_simulation_mode'Image(mode));
    New_Line;
    Reset(g);
    t0 := Clock;
    for i in 1 .. n loop
      u:= Real(Random(g));
      x:= Index_any (u, F);
      sample(x):= sample(x) + 1;
    end loop;
    t1 := Clock;
    --
    for y in sample'Range loop
      Put("    # occurrences:" & Integer'Image(sample(y)) & "   ");
      Put(Real(sample(y)) / Real(n), 2, 10, 0);
      Put_Line(", " & comment);
    end loop;
    New_Line;
    Put_Line ("Elapsed time: " & Duration'Image(t1-t0));
  end Test_CDF_by_mode;

  procedure Test_CDF_linear is new Test_CDF_by_mode(linear);
  procedure Test_CDF_dicho is new Test_CDF_by_mode(dichotomic);

  procedure Test_CDF(F: CDF; comment: String) is
  begin
    for mode in Discrete_random_simulation_mode loop
      case mode is
        when linear =>
          Test_CDF_linear(F, comment);
        when dichotomic =>
          Test_CDF_dicho(F, comment);
      end case;
    end loop;
  end Test_CDF;

  flip_coin: constant CDF := (0.0, 0.5);
  dice_0: constant CDF (0..5) := (0.0, 1.0/6.0, 2.0/6.0, 3.0/6.0, 4.0/6.0, 5.0/6.0);
  dice_1: constant CDF (1..6) := (0.0, 1.0/6.0, 2.0/6.0, 3.0/6.0, 4.0/6.0, 5.0/6.0);

  --  *WRONG* usage:
  flip_coin_wrong_side: constant CDF := (0.5, 1.0);
  dice_with_value_1: constant CDF := (0.0, 1.0/6.0, 2.0/6.0, 3.0/6.0, 4.0/6.0, 5.0/6.0, 1.0);

begin
  Test_CDF(flip_coin, "Flip or coin");
  Test_CDF(dice_0, "Dice index base 0");
  Test_CDF(dice_1, "Dice index base 1");
  New_Line(4);
  Put_Line("******");
  Put_Line("  Hereafter is a *WRONG* usage of the Cumulative_distribution_function arrays");
  Put_Line("  with prob. value 1.0, or both values 0.0 and 1.0.");
  Put_Line("******");
  Test_CDF(flip_coin_wrong_side, "Flip or coin, WRONG side CDF array");
  Test_CDF(dice_with_value_1,
    "Dice; prob. values 0.0 and 1.0 (WRONG - ""seventh"" face with 0 mathematical probability)");
end Test_Discrete_Random_Simulation;
