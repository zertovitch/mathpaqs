with Samples;
with Generic_Random_Functions, U_Rand;

with Ada.Text_IO;                       use Ada.Text_IO;

procedure Test_Samples is

  type Real is digits 14;
  type Quantile_table is array(Positive range <>) of Real;

  package RS is new Samples (Real, Quantile_table, True);

  package GRF is new Generic_Random_Functions (Real);
  package RUR is new U_Rand (Real);
  Gen : RUR.Generator;

  function Uni01 return Real is begin return RUR.Random (Gen); end Uni01;
  function Poisson is new GRF.Poisson (Uni01);

  s : RS.Sample(10_000);

  level : constant Quantile_table:=
   (0.0, 0.0001, 0.001, 0.01,
    0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09,
    0.1, 0.2, 0.25,
    0.3, 0.35, 0.36,
    0.4, 0.41, 0.42,
    0.5, 0.6, 0.75, 0.8,
    0.9, 0.95, 0.975, 0.98,
    0.99, 0.995, 0.996, 0.9975, 0.998,
    0.999, 0.9995, 0.9996, 0.99975, 0.9998,
    0.9999
   );

  -- R:
  -- quantile(t, c(0.0, 0.0001, 0.001, 0.01, ...))
  -- write.csv2(quantile(t, c(0.0, 0.0001, 0.001, 0.01, ...)),file="quant.csv")
  -- ^ this is written in the R commands file

  m : RS.Measure(level'Last);

  package RIO is new Ada.Text_IO.Float_IO (Real);
  use RIO;

  f, r_commands : File_Type;

  sep : constant Character:= ';';

  procedure Title (t: String) is
  begin
    Put_Line (t);
    Put_Line (f, t & sep & "------------");
  end Title;

  procedure Display_Measure (m: RS.Measure) is
  begin
    Put("Mean:"); Put(m.mean); New_Line;
    Put("Std dev:"); Put(m.std_dev); New_Line;
    Put("Stat err:"); Put(m.stat_err); New_Line;
    Put(f,"Mean:" & sep); Put(f,m.mean); New_Line(f);
    Put(f,"Std dev:" & sep); Put(f,m.std_dev); New_Line(f);
    Put(f,"Stat err:" & sep); Put(f,m.stat_err); New_Line(f);
    Put_Line(f, "x" & sep & "P(X<x)");
    for i in m.level'Range loop
      Put("P(X<");
      Put(m.VaR(i));
      Put(") = ");
      Put(m.level(i));
      New_Line;
      --
      Put(f, m.VaR(i));
      Put(f, sep);
      Put(f, m.level(i));
      New_Line(f);
    end loop;
  end Display_Measure;

  large_1: constant:= 100_000;
  multi: constant:= 10;
  u, n1, n2: Real;

  type Sim_Norm is (Cephes, Box_Muller_X, Box_Muller_Y);

begin
  m.level:= level;
  -- Output file for further study, graphics,...
  Create(f, Out_File, "test_samples.csv");
  Create(r_commands, Out_File, "r_commands.txt");
  Put(r_commands, "write.csv2(quantile(t, c(");
  for i in m.level'Range loop
    Put(r_commands, m.level(i));
    if i < m.level'Last then
      Put(r_commands, ',');
    end if;
  end loop;
  Put_Line(r_commands, ")),file=""quant.csv"")");
  --
  Title("=== Trivial test: add only 0 as occurence");
  -- R:
  -- t <- sample(c(0),1000,TRUE)
  RS.Initialize(s, 0.0, 1000.0);
  for i in 1..10_000 loop
    RS.Add_occurrence(s, 0.0);
  end loop;
  RS.Get_measures(s,m);
  Put_Line("Mean should be zero, std dev should be zero.");
  Display_Measure(m);
  --
  Title("=== Trivial test: add only 0.123 as occurence");
  RS.Initialize(s, 0.0, 1000.0); -- !! narrow it
  for i in 1..100_000 loop
    RS.Add_occurrence(s, 0.0123);
  end loop;
  RS.Get_measures(s,m);
  Put_Line("Mean should be 0.0123, std dev should be zero.");
  Display_Measure(m);
  --
  Title("=== Easy test: Discrete Uniform, 2 points (10, 10), 100_000 occurences");
  -- R:
  -- t <- sample(c(-10,10),100000,TRUE)
  RS.Initialize(s, -10.0, 10.0);
  for i in 1..100_000 loop
    RS.Add_occurrence(s, -10.0);
    RS.Add_occurrence(s,  10.0);
  end loop;
  RS.Get_measures(s,m);
  Put_Line("Mean should be zero, std dev should converge to 10.");
  Display_Measure(m);
  --
  Title("=== Easy test: Uniform [0, 1]");
  -- R:
  -- t <- runif(100000,0,1)
  RS.Initialize(s, 0.0, 1.0);
  for i in 0..large_1 loop
    u:= Real(i) / Real(large_1);
    RS.Add_occurrence(s, u);
  end loop;
  RS.Get_measures(s,m);
  Put_Line("Mean should converge to 0.5, std dev should converge to 1/(2 sqrt(3)) ~= 0.288675.");
  Display_Measure(m);
  --
  Title("=== Easy test: Uniform [-10, 10]");
  -- R:
  -- t <- runif(100000,-10,10)
  RS.Initialize(s, -10.0, 10.0);
  for i in 0..large_1 loop
    u:= Real(i) / Real(large_1);
    RS.Add_occurrence(s, 20.0 * u - 10.0);
  end loop;
  RS.Get_measures(s,m);
  Put_Line("Mean should converge to zero, std dev should converge to 10/sqrt(3) ~= 5.77350269.");
  Display_Measure(m);
  --
  for meth in Sim_Norm loop
    Title("=== Random test: Normal(0, 1) using " & Sim_Norm'Image(meth));
    -- R:
    -- t <- rnorm(1000000,0,1)
    RS.Initialize(s, -100.0, 100.0);
    RS.Add_occurrence(s, -100.0); -- !!
    RUR.Reset(Gen);
    for i in 1..1_000_000 loop
      case meth is
        when Cephes =>
          n1:= GRF.Normal_inverse_CDF(Uni01);
        when Box_Muller_X =>
          GRF.Box_Muller(Uni01,Uni01,n1,n2);
        when Box_Muller_Y =>
          GRF.Box_Muller(Uni01,Uni01,n1,n2);
          n1:= n2;
      end case;
      RS.Add_occurrence(s, n1);
    end loop;
    RS.Get_measures(s,m);
    Put_Line("Mean should converge to zero, std dev should converge to 1.");
    Display_Measure(m);
  end loop;
  --
  Title("=== Random test: Poisson(lambda = 0.54321)");
  RS.Initialize(s, 0.0, 100.0);
  RUR.Reset(Gen);
  for i in 1..1_000_000 loop
    RS.Add_occurrence(s, Real(Poisson(0.54321)));
  end loop;
  RS.Get_measures(s,m);
  Put_Line("Mean should converge to lambda, std dev should converge to sqrt(lambda).");
  Display_Measure(m);
  --
  Title("=== Fuzzy discrete sample: values 0, 1, 6 added n, 4n, 7n times");
  RS.Initialize(s, 0.0, 6.0);
  for i in 1..1*multi loop
    RS.Add_occurrence(s, 0.0);
  end loop;
  for i in 1..4*multi loop
    RS.Add_occurrence(s, 1.0);
  end loop;
  for i in 1..7*multi loop
    RS.Add_occurrence(s, 6.0);
  end loop;
  RS.Get_measures(s,m);
  Put_Line("Mean should converge to 23/6 ~= 3.833333.");
  Display_Measure(m);
  --
  Close(f);
  Close(r_commands);
end Test_Samples;
