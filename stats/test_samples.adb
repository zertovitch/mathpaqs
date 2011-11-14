with Samples;

with Ada.Text_IO;                       use Ada.Text_IO;

procedure Test_Samples is

  type Real is digits 14;
  type Quantile_table is array(Positive range <>) of Real;

  package RS is new Samples(Real, Quantile_table);

  s: RS.Sample(10_000);

  level: constant Quantile_table:=
   (0.0, 0.0001, 0.001, 0.01,
    0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.75, 0.8,
    0.9, 0.95, 0.975, 0.98,
    0.99, 0.995, 0.996, 0.9975, 0.998,
    0.999, 0.9995, 0.9996, 0.99975, 0.9998,
    0.9999
   );

  m: RS.Measure(level'Last);

  package RIO is new Ada.Text_IO.Float_IO(Real);
  use RIO;

  f: File_Type;

  procedure Title(t: String) is
  begin
    Put_Line(t);
    Put_Line(f, t);
  end Title;

  procedure Display_Measure(m: RS.Measure) is
  begin
    Put("Mean:"); Put(m.mean); New_Line;
    Put("Std dev:"); Put(m.std_dev); New_Line;
    Put("Stat err:"); Put(m.stat_err); New_Line;
    for i in m.level'Range loop
      Put("P(X<");
      Put(m.VaR(i));
      Put(") = ");
      Put(m.level(i));
      New_Line;
      --
      Put(f, m.VaR(i));
      Put(f, ';');
      Put(f, m.level(i));
      New_Line(f);
    end loop;
  end Display_Measure;

  large_1: constant:= 100_000;

begin
  m.level:= level;
  -- Output file for further study, graphics,...
  Create(f, Out_File, "test_samples.csv");
  --
  Title("=== Trivial test 1: add only 0 as occurence");
  RS.Initialize(s, 0.0, 1000.0);
  for i in 1..10_000 loop
    RS.Add_occurence(s, 0.0);
  end loop;
  RS.Get_measures(s,m);
  Put_Line("Mean should be zero, std dev should be zero.");
  Display_Measure(m);
  --
  Title("=== Trivial test 2: add only 0.123 as occurence");
  RS.Initialize(s, 0.0, 1000.0); -- !! narrow it
  for i in 1..100_000 loop
    RS.Add_occurence(s, 0.0123);
  end loop;
  RS.Get_measures(s,m);
  Put_Line("Mean should be 0.0123, std dev should be zero.");
  Display_Measure(m);
  --
  Title("=== Easy test 3: discrete uniform, 2 points");
  RS.Initialize(s, -10.0, 10.0);
  for i in 1..100_000 loop
    RS.Add_occurence(s, -10.0);
    RS.Add_occurence(s,  10.0);
  end loop;
  RS.Get_measures(s,m);
  Put_Line("Mean should be zero, std dev should be 10.");
  Display_Measure(m);
  --
  Title("=== Easy test 4: uniform");
  RS.Initialize(s, -10.0, 10.0);
  for i in 0..large_1 loop
    RS.Add_occurence(s, 20.0 * Real(i) / Real(large_1) - 10.0);
  end loop;
  RS.Get_measures(s,m);
  Put_Line("Mean should be zero, std dev should be 10/sqrt(3) ~= 5.77350269.");
  Display_Measure(m);
  --
  Close(f);
end Test_Samples;
