with Generic_Random_Functions, U_Rand;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Test_Poisson is

  subtype Real is Long_Float;

  package RRF is new Generic_Random_Functions(Real);
  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real);


  package Real_U_Rand is new U_Rand(Real);

  gen: Real_U_Rand.Generator;

  function U return Real is begin return Real_U_Rand.Random(gen); end;
  function Poisson is new RRF.Poisson(U);

  package RIO is new Float_IO(Real);

  use RRF, RIO, REF;

  samples: constant := 1_000_000;

  lambda: constant:= 0.987654;

  bins: array(0..20) of Natural:= (others => 0);

  n: Natural;
  fact_k: Real:= 1.0;
  ps, pe, ms: Real;
  sum_k_pk: Natural:= 0;

begin
  Real_U_Rand.Reset(gen);
  for i in 1..samples loop
    n:= Poisson(lambda);
    if n in bins'Range then
      bins(n):= bins(n) + 1;
    end if;
  end loop;
  for k in bins'Range loop
    Put("k=");
    Put(Integer'Image(k));
    Put("; Statistical P(X=k)= ");
    ps:= Real(bins(k)) / Real(samples);
    Put(ps);
    sum_k_pk:= sum_k_pk + k * bins(k);
    Put("; Exact P(X=k)= ");
    if k > 1 then
      fact_k:= fact_k * Real(k);
    end if;
    pe:= exp(-lambda) * (lambda ** k) / fact_k;
    Put(pe);
    Put("; Difference= ");
    Put(pe-ps);
    New_Line;
  end loop;
  Put("Statistical mean ... = ");
  ms:= Real(sum_k_pk) / Real(samples);
  Put(ms);
  New_Line;
  Put("Exact mean (=lambda) = ");
  Put(lambda);
  New_Line;
  Put("Difference = ");
  Put(lambda-ms);
  New_Line;
  Put("Press return");
  Skip_Line;
end Test_Poisson;
