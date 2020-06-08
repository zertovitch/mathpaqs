--  "SEIR" model for simulating the outbreak of the Coronavirus
--  disease (COVID-19).

--  This program solves a vectorial ordinary differential equation
--  (or a system of ordinary differential equations).
--
--  * The unknown is a vector containing the values S, E, I, R.
--    The letters stands for:
--      S: Susceptible
--      E: Exposed
--      I: Infectious
--      R: Recovered
--  * There is a propagation in the direction S -> E -> I -> R.
--
--  Related publication:
--    Nowcasting and forecasting the potential domestic and
--    international spread of the 2019-nCoV outbreak originating
--    in Wuhan, China: a modelling study
--
--  Simplification here:
--    - no flights: L_{W,I}, L_{W,C}, ... = 0
--    - zoonotic force = 0.

with Ada.Text_IO, Ada.Integer_Text_IO;

procedure COVID_19 is

  type Real is digits 15;

  package PFIO is new Ada.Text_IO.Float_IO (Real);

  type Status is (Susceptible, Exposed, Infectious, Recovered);

  type Status_Vector is array (Status) of Real;

  function "*" (l : Real; v : Status_Vector) return Status_Vector is
    r : Status_Vector;
  begin
    for i in v'Range loop r(i) := v(i) * l; end loop;
    return r;
  end "*";

  function "+" (a, b : Status_Vector) return Status_Vector is
    r : Status_Vector;
  begin
    for i in a'Range loop r(i) := a(i) + b(i); end loop;
    return r;
  end "+";

  inv_incubation_period : constant := 1.0 / 5.2;
  inv_infective_period  : constant := 1.0 / 2.9;

  --  We solve numerically   x' (t) = f (x (t), t)   over the time step h.
  --
  procedure Evolution (xt : in out Status_Vector; reproductive_number : Real; h : Real) is
    --
    function f (x : Status_Vector) return Status_Vector is
      n, inv_n, s_to_e, e_to_i, nb_infected_over_period, susc_rate : Real;
      --
    begin
      --  Count the population at time t.
      n := 0.0;
      for s in Status loop
        n := n + x (s);
      end loop;
      inv_n := 1.0 / n;
      nb_infected_over_period := x (Infectious) * inv_infective_period;
      --  Some Susceptible persons get the virus
      --  from Infectious people and become Exposed.
      susc_rate := x (Susceptible) * inv_n;  --  As proportion of the population.
      s_to_e := susc_rate * reproductive_number * nb_infected_over_period;
      --  Exposed persons become Infectious after incubation.
      e_to_i := x (Exposed) * inv_incubation_period;
      --  Infectious people recover after infective period. -> Recovered.
      --  This rate is already computed: nb_infected_over_period;
      return
        ( Susceptible => -s_to_e,
          Exposed     =>  s_to_e - e_to_i,
          Infectious  =>           e_to_i - nb_infected_over_period,
          Recovered   =>                    nb_infected_over_period
        );
    end f;
    k1, k2, k3, k4 : Status_Vector;
  begin
    --  Runge-Kutta, Order 4
    k1 := f (xt               );
    k2 := f (xt + h * 0.5 * k1);
    k3 := f (xt + h * 0.5 * k2);
    k4 := f (xt + h *       k3);
    xt := xt + h * (1.0/6.0) * (k1 + 2.0 * k2 + 2.0 * k3 + k4);
  end Evolution;

  procedure Simulation is
    use Ada.Text_IO, Ada.Integer_Text_IO, PFIO;
    x : Status_Vector;
    dt : Real;
    reproductive_number : Real;
    n_iter : Integer;
    out_step : Integer;
    rf : File_Type;
    sep : constant Character := ';';
  begin
    dt := 1.0;
    n_iter := 365;
    out_step := 1;
    reproductive_number := 3.4;
    x :=
      ( Susceptible => 1_000_000.0,
        Exposed     =>         0.0,
        Infectious  =>         1.0,  --  The spreader.
        Recovered   =>         0.0
      );
    --  Status numbers at time t = 0.

    Create (rf, Out_File, "covid_19.csv");
    Put (rf, "t");
    for l in Status loop
      Put (rf, sep);
      Put (rf, Status'Image (l));
    end loop;
    New_Line (rf);
    for i in 0 .. n_iter loop
      if i mod out_step = 0 then
        Put (rf, i);
        for l in Status loop
          Put (rf, sep);
          Put (rf, x(l), 4, 5, 0);
        end loop;
        New_Line (rf);
      end if;
      Evolution (x, reproductive_number, dt);
    end loop;
    Close (rf);
  end Simulation;

begin
  Simulation;
end COVID_19;
