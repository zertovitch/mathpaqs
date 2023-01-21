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
--  * There is a propagation of population in the
--    direction S ---> E ---> I ---> R, plus new
--    infections:  ^------<---
--
--  Related publication:
--    Nowcasting and forecasting the potential domestic and
--    international spread of the 2019-nCoV outbreak originating
--    in Wuhan, China: a modelling study
--
--    https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30260-9/fulltext
--
--  Simplification here:
--    - no flights: L_{W,I}, L_{W,C}, ... = 0
--    - zoonotic force = 0.

with Ada.Characters.Handling, Ada.Text_IO, Ada.Integer_Text_IO;

procedure COVID_19 is

  type Real is digits 15;

  package PFIO is new Ada.Text_IO.Float_IO (Real);

  type Status is (Susceptible, Exposed, Infectious, Recovered);

  type Status_Vector is array (Status) of Real;

  function "*" (l : Real; v : Status_Vector) return Status_Vector is
    r : Status_Vector;
  begin
    for i in v'Range loop r (i) := v (i) * l; end loop;
    return r;
  end "*";

  function "+" (a, b : Status_Vector) return Status_Vector is
    r : Status_Vector;
  begin
    for i in a'Range loop r (i) := a (i) + b (i); end loop;
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
         (Susceptible => -s_to_e,
          Exposed     =>  s_to_e - e_to_i,
          Infectious  =>           e_to_i - nb_infected_over_period,
          Recovered   =>                    nb_infected_over_period
        );
    end f;
    k1, k2, k3, k4 : Status_Vector;
  begin
    --  Runge-Kutta, Order 4
    k1 := f (xt);
    k2 := f (xt + h * 0.5 * k1);
    k3 := f (xt + h * 0.5 * k2);
    k4 := f (xt + h *       k3);
    xt := xt + h * (1.0 / 6.0) * (k1 + 2.0 * k2 + 2.0 * k3 + k4);
  end Evolution;

  type Scenario is (No_Lockdown, Lockdown, Lockdown_in_two_Steps);

  procedure Simulation (s : Scenario) is
    use Ada.Text_IO, Ada.Integer_Text_IO, PFIO;
    x : Status_Vector;
    dt : Real;
    reproductive_number : Real;
    n_iter : Integer;
    out_step : Integer;
    rf : File_Type;
    sep : constant Character := ';';
    use Ada.Characters.Handling;
    basic_reproductive_number : constant := 3.5;
  begin
    dt := 1.0;
    n_iter := 365;
    out_step := 1;
    x :=
       (Susceptible => 1_000_000.0,
        Exposed     =>         0.0,
        Infectious  =>         1.0,  --  Patient 1.
        Recovered   =>         0.0
      );
    --  Status numbers at time t = 0.

    Create (rf, Out_File, "covid_19_" & To_Lower (Scenario'Image (s)) & ".csv");
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
          Put (rf, x (l), 4, 5, 0);
        end loop;
        New_Line (rf);
      end if;
      case s is
        when No_Lockdown =>
          reproductive_number := basic_reproductive_number;
        when Lockdown =>
          if i < 40 then
            reproductive_number := basic_reproductive_number;
          else
            reproductive_number := 1.0;
          end if;
        when Lockdown_in_two_Steps =>
          if i < 40 then
            reproductive_number := basic_reproductive_number;
          elsif i < 60 then
            reproductive_number := 2.0;
          else
            reproductive_number := 1.0;
          end if;
      end case;
      Evolution (x, reproductive_number, dt);
    end loop;
    Close (rf);
  end Simulation;

begin
  for s in Scenario loop
    Simulation (s);
  end loop;
end COVID_19;
