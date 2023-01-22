with Finite_Distributed_Random;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Numerics.Float_Random;

procedure Test_Finite_distributed_random is

  G : Ada.Numerics.Float_Random.Generator;

  function Rnd return Float is
  begin
    return Ada.Numerics.Float_Random.Random (G);
  end Rnd;

  type ABC is (a, b, c);

  type Proba_Array is array (ABC) of Float;

  function ABC_Rand is new
    Finite_Distributed_Random
      (A_float        => Float,
       Thing          => ABC,
       Proba_array    => Proba_Array);

  --  Instantiation with an integer type

  type Int_2 is range 1 .. 2;

  type Proba_Array_Int is array (Int_2) of Float;

  function Int_Rand is new
    Finite_Distributed_Random
      (A_float        => Float,
       Thing          => Int_2,
       Proba_array    => Proba_Array_Int);

  dummy : Int_2 := Int_Rand ((0.2, 0.8), Rnd);

  procedure Test (tirages : Positive; proba : Proba_Array) is
    sample : array (ABC) of Natural;
    res : ABC;
  begin
    Put_Line ("# of tests in sample: " & Integer'Image (tirages));
    Ada.Numerics.Float_Random.Reset (G);
    sample := (others => 0);
    for i in 1 .. tirages loop
      res := ABC_Rand (proba, Rnd);
      sample (res) := sample (res) + 1;
    end loop;
    for x in ABC loop
      Put (ABC'Image (x) & " :    prob = ");
      Put (proba (x), 1, 4, 0);
      Put (" ;    stat = ");
      Put (Float (sample (x)) / Float (tirages), 1, 4, 0);
      New_Line;
    end loop;
    New_Line;
  end Test;

begin
  --  Test( 1000, (A=> 0.0123, B=> 0.0456, C=> 0.04287) ); (sum /= 1)
  Test     (100_000, (a => 0.50,  b => 0.25,  c => 0.25));
  Test   (1_000_000, (a => 0.25,  b => 0.50,  c => 0.25));
  Test  (10_000_000, (a => 0.25,  b => 0.25,  c => 0.50));
  Test (100_000_000, (a => 0.123, b => 0.456, c => 0.421));
end Test_Finite_distributed_random;
