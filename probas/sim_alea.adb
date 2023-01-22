------------------------------------------------------------------------------
--  File:            Sim_Alea.adb
--  Description:     Simulation of random variables with ASCII-Art histograms
--  Date/version:    1-Feb-2005; 17-Apr-1997
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Text_IO,
     Ada.Float_Text_IO,
     Ada.Numerics.Elementary_Functions,
     Ada.Numerics.Float_Random;

procedure Sim_Alea is

  use Ada.Numerics, Ada.Numerics.Elementary_Functions;

  --  g: inverse de la fonction de repartition F de la v.a. X

  function g_unif (y : Float) return Float is     --  X v.a. uniforme: X ~ U(a;b)
    a : constant := -10.0;
    b : constant :=  50.0;
  begin
    return a + y * (b - a);
  end g_unif;

  function g_expon (y : Float) return Float is    --  X v.a. expon.: X ~ Exp(l)
    l : constant Float := 1.0 / 9.0;
  begin
    return -Log (1.0 - y) / l;
  end g_expon;

  function g_weibull (y : Float) return Float is  --  X v.a. de Weib.: X ~ W(l;p)
    p : constant Float := 2.0;                    --  X=T^p, T ~ Exp(l)
    l : constant Float := 1.0 / 9.0;
    log_l : constant Float := Log (l);
  begin
    return  Exp (p * (Log (-Log (1.0 - y)) - log_l));
  end g_weibull;

  function g_cauchy (y : Float) return Float is   --  X v.a. de Cauchy
  begin                                           --  X=tan(T), T ~ U(-pi/2;pi/2)
    return Tan (Pi * (y - 0.5));
  end g_cauchy;

  type Fct is access function (x : Float) return Float;

  procedure Tirages (g : Fct; l : String; n : Positive; b_inf, b_sup : Float) is
    use Ada.Text_IO;
    --  loi:         choix de la loi;
    --  n:           nb de tirages
    --  b_inf,b_sup: bornes de l'histogramme

    t, inf, sup, moy, somme : Float;

    mx : constant := 79; mxm1 : constant := mx - 1;
    my : constant := 20;
    ti : array (0 .. mxm1) of Integer := (others => 0);       --  tableau d'incidences
    lbarre : constant Float := (b_sup - b_inf) / Float (mx);  --  largeur d'une barre
    ilb : constant Float := 1.0 / lbarre;
    k : Integer;

    procedure Histogramme is
      corr : constant Integer := n / 1000;                  --  corr. pour aff.
      m : constant Integer := Integer ((moy - b_inf) * ilb);
      use Ada.Float_Text_IO;
    begin
      for i in ti'Range loop             -- correction
        ti (i) := ti (i) / corr;
      end loop;
      Put_Line ("Loi " & l);
      for j in reverse 1 .. my loop
        for i in ti'Range loop
          if ti (i) >= j then if i = m then Put ('|'); else Put ('#'); end if;
                      else Put (' '); end if;
        end loop;
        New_Line;
      end loop;
      Put (b_inf, 6, 2, 0);  Put (b_sup, mx - 24, 2, 0);  New_Line;
      Put_Line ("Stat: inf, moy, sup:");
      Put (inf, 6, 4, 0); Put (moy, 30, 4, 0); Put (sup, mx - 59, 4, 0); New_Line;
      Put ("Pressez ""Return""");
      Skip_Line;
    end Histogramme;

    use Ada.Numerics.Float_Random;

    gen : Generator;

  begin
    Reset (gen);    --  Initialiser le generateur selon l'horloge

    --  Tirages de la v.a.
    --  avec calcul de la somme, de la moyenne et de l'etendue des valeurs

    somme := 0.0;
    for i in 1 .. n loop
      t := g (Random (gen));
      somme := somme + t;
      if i = 1 then
        sup := t;
        inf := t;
      else
        if t > sup then sup := t; end if;
        if t < inf then inf := t; end if;
      end if;
      k := Integer ((t - b_inf) * ilb);
      if k in ti'Range then
        ti (k) := ti (k) + 1;
      end if;
    end loop;
    moy := somme / Float (n);

    Histogramme;

  end Tirages;

begin
  Tirages (g_unif'Access,    "uniforme",       50_000,  -20.0,  60.0);
  Tirages (g_expon'Access,   "exponentielle", 100_000,    0.1,  10.0);
  Tirages (g_weibull'Access, "de Weibull", 300_000,    0.0, 324.0);
  Tirages (g_cauchy'Access,  "de Cauchy",  500_000, -300.0, 300.0);
end Sim_Alea;
