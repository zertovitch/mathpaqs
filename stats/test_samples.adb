with Samples;

with Ada.Text_IO;                       use Ada.Text_IO;

procedure Test_Samples is

  type Real is digits 14;
  type Quantile_table is array(Positive range <>) of Real;

  package RS is new Samples(Real, Quantile_table);

  s: RS.Sample(2000);

  level: constant Quantile_table:=
   (0.0, 0.25, 0.5, 0.75, 0.8,
    0.9, 0.95, 0.975, 0.98,
    0.99, 0.995, 0.996, 0.9975, 0.998,
    0.999, 0.9995, 0.9996, 0.99975, 0.9998,
    0.9999
   );

  m: RS.Measure(level'Last);

  package RIO is new Ada.Text_IO.Float_IO(Real);
  use RIO;

begin
  -- Trivial test 1: add only 0 as occurence
  RS.Initialize(s, 0.0, 1000.0);
  for i in 1..10000 loop
    RS.Add_occurence(s, 0.0);
  end loop;
  m.level:= level;
  RS.Get_measures(s,m);
  Put("Mean, should be zero:"); Put(m.mean); New_Line;
  Put("Std dev, should be zero:"); Put(m.std_dev); New_Line;
  -- Trivial test 2: add only 0.123 as occurence
  RS.Initialize(s, 0.0, 1000.0);
  for i in 1..10000 loop
    RS.Add_occurence(s, 0.123);
  end loop;
  m.level:= level;
  RS.Get_measures(s,m);
  Put("Mean, should be 0.123:"); Put(m.mean); New_Line;
  Put("Std dev, should be zero:"); Put(m.std_dev); New_Line;
end Test_Samples;
