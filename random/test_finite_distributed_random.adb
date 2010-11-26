with Finite_distributed_random;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Numerics.Float_Random;

procedure Test_Finite_distributed_random is

  G: Ada.Numerics.Float_Random.Generator;

  function URand return Float is
  begin
    return Ada.Numerics.Float_Random.Random(G);
  end;

  type ABC is (a,b,c);
  
  type Proba_array is array(ABC) of Float;

  function ABC_Rand is new
    Finite_distributed_random(
      A_float        => Float,
      Thing          => ABC,
      Proba_array    => Proba_array,
      Uniform_random => URand);
  
  procedure Test( tirages: Positive; proba: Proba_array ) is
    sample: array(ABC) of Natural;
    res: ABC;
  begin
    Put_Line("# of tests in sample: " & Integer'Image(tirages));
    Ada.Numerics.Float_Random.Reset(G);
    sample:= (others => 0);
    for i in 1..tirages loop
      res:= ABC_Rand( proba );
      sample(res):= sample(res) + 1;
    end loop;
    for x in ABC loop
      Put( ABC'image(x) & " :    prob = ");
      Put( proba(x), 1,4,0);
      Put(                " ;    stat = ");
      Put( Float( sample(x) ) / Float( tirages ), 1,4,0);
      New_Line;
    end loop;
    New_Line;
  end Test;
  
begin
  --  Test( 1000, (A=> 0.0123, B=> 0.0456, C=> 0.04287) ); (sum /= 1)
  Test(     100_000, (A=> 0.50, B=> 0.25, C=> 0.25) );
  Test(   1_000_000, (A=> 0.25, B=> 0.50, C=> 0.25) );
  Test(  10_000_000, (A=> 0.25, B=> 0.25, C=> 0.50) );
  Test( 100_000_000, (A=> 0.123, B=> 0.456, C=> 0.421) );
end;