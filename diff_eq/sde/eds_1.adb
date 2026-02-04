------------------------------------------------------------------------------
--  File:            EDS_1.adb
--  Description:     EDS_1
--  Date / Version:  19-Dec-2001 ; 9.VI.1998
--  Author:          Gautier.deMontmollin@Winterthur.ch
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Float_Random;

with Generic_Random_Functions;

with Graph.Standard_fonts;

procedure EDS_1 is

  use Ada.Numerics.Float_Random;

  type Real is digits 15;

  package REF is new Ada.Numerics.Generic_Elementary_functions (Real);
  package RIO is new Ada.Text_IO.Float_IO (Real);

  package RF is new Generic_Random_Functions (Real);

  type Vector is array (Natural range <>) of Real;

  use Graph, Graph.Standard_fonts;

  procedure Trajectoire (T0, T1 : Real; X : Vector; c : String) is
    D : constant Real := (T1 - T0) / Real (X'Length);
    begin
      Nowhere;
      for i in X'Range loop
        LineTo (Float (T0 + Real (i) * D), Float (X (i)));
        if (i /= X'First) and
           (i = X'Last or i mod (X'Length / 8) = 0)
        then
          OutText (c);
        end if;
      end loop;
    end Trajectoire;

  type methode is (euler, euler_impl, milstein,
                   euler_faible, euler_impl_faible);

  procedure Comparaison
    (ntot : Positive;
     me   : methode;
     T0, T1, X0, la, mu : Real;
     eex : Boolean)
  is
    use REF, RIO;
    --   ntot= nombre total de pas (fins)
    DW : array (0 .. ntot) of Real;   -- tirage du mouvement brownien
    X, Y : Vector (0 .. ntot);          -- solutions (X: exacte, Y: num.)

    procedure Simulation (granum : Positive; err : out Real) is
       --  Delta de la methode:
      D : constant Real := (T1 - T0) * Real (granum) / Real (ntot);
      k, ki, kp : Natural;
      Y_ki, DW_kp : Real;

    begin
      err := 0.0;
      Y (0) := X0;

      for i in 1 .. 1 + ntot / granum loop -- "1+" en cas de reste /= 0
        ki :=     i   * granum; -- peut depasser ntot si mauvaise granul.
        kp :=   (i - 1) * granum;
        DW_kp := DW (kp) *  Sqrt (Real (granum)); -- corrige la variance

        case me is
          when euler | euler_faible =>
            Y_ki := Y (kp) * (1.0 + la * D + mu * DW_kp);

          when euler_impl | euler_impl_faible =>
            Y_ki := Y (kp) * (1.0 + mu * DW_kp) / (1.0 - la * D);

          when milstein =>
            Y_ki := Y (kp) * (1.0 + la * D + mu * DW_kp +
                            mu * mu * 0.5 * ((DW_kp**2) - D));
        end case;

        if ki <= ntot then
          err := Real'Max (err, abs (Y_ki - X (ki)));
        end if;
        for j in 1 .. granum loop -- on interpole lin. les valeurs interm.
          k := kp + j;
          if k <= ntot then -- j=granum: Y(ki) sera := Y_ki si ki<= ntot
            Y (k) := Y (kp) + Real (j) * (Y_ki - Y (kp)) / Real (granum);
          end if;
        end loop;
      end loop;
    end Simulation;

  Wt : Real := 0.0;
  D : constant Real := (T1 - T0) / Real (ntot); -- Delta fin
  sqD : constant Real :=  Sqrt (D);
  X1 : constant Real := X0 *  Exp ((Real'Max (la, 0.0) + Real'Max (mu, 0.0) * sqD) * (T1 - T0)) + 0.1;
  granumax : constant Positive := ntot / 10;
  ngranu : constant := 3; -- nb de pas de granularite -1
  g : Positive;
  s : String (1 .. 5); s10 : String (1 .. 10);
  txt_fact : Positive;
  err : Real;

  gen : Generator;

  function Normal_Random return Real is
  (RF.Normal_inverse_CDF (Real (Random (gen))));

  begin
    Reset (gen, 1);

    --  Tirage mvt brownien:
    for i in DW'Range loop
      case me is
        when euler_faible | euler_impl_faible =>
          if Normal_Random > 0.0 then DW (i) := sqD; else DW (i) := -sqD; end if;
        when others =>
          DW (i) := Normal_Random * sqD;
      end case;
    end loop;

    --  Solution exacte
    if eex then
      X (0) := X0;
      for i in 1 .. ntot loop
        Wt := Wt + DW (i);
        X (i) := X0 *  Exp ((la - 0.5 * mu**2) * D * Real (i) + mu * Wt);
      end loop;
    end if;

    current_device := PostScript;
    txt_fact := 5;
    Set_math_plane (Float (T0) - 0.1, -0.2, Float (T1) + 0.1, Float (X1));
    SetTextJustify (LeftText, TopText);
    SetUserCharSize (GetMaxX, 1500, GetMaxY, 2000);
    SetTextStyle (SansSerifFont, HorizDir);
    MoveTo (0.1, Float (X1));
    MoveRel (0, GetMaxY / 40);
    OutText (methode'Image (me));
    SetUserCharSize (GetMaxX, 2000, GetMaxY, 3000);
    MoveRel (0, GetMaxY / 20);
    OutText ("dXt = la Xt dt + mu Xt dWt");
    MoveRel (0, GetMaxY / 40);  Put (s, la, 1, 0);  OutText ("la=" & s);
    MoveRel (0, GetMaxY / 40);  Put (s, mu, 1, 0);  OutText ("mu=" & s);
    SetTextStyle (SmallFont, HorizDir, 5 * txt_fact);
    Draw_axes;
    SetTextStyle (SmallFont, HorizDir, 4 * txt_fact);
    SetTextJustify (RightText, CenterText);
    Put (s, X0, 1, 0);
    OutTextXY (Float (T0), Float (X0), "X0=" & s & ' ');
    SetTextJustify (LeftText, TopText);
    Put (s, T0, 2, 0);
    OutTextXY (Float (T0), -0.1, " T0= " & s);
    Put (s, T1, 2, 0);
    OutTextXY (Float (T1), -0.1, "T1= " & s);
    SetTextStyle (SmallFont, HorizDir, 4 * txt_fact);

    --  Simulation de plus en plus fines:
    for i in reverse 0 .. ngranu loop
      g := Natural (Real (granumax)**(Real (i) / Real (ngranu)));
      Simulation (g, err);
      --  if ecran then
      --   SetColor(pala + ((granumax-g)*palaz)/granumax);
      --  end if;
      SetTextJustify (LeftText, CenterText);

      Trajectoire (T0, T1, Y, Integer'Image (ntot / g));

      MoveTo (0.1, -0.02);
      MoveRel (Float (Real (i) * (T1 - 0.1) / Real (ngranu)), 0.0);
      SetTextJustify (CenterText, TopText);
      Put (s10, err, 3, 0);
      OutText (Integer'Image (ntot / g) & "->" & s10);
    end loop;

    if eex then
      SetTextJustify (LeftText, CenterText);
      Trajectoire (T0, T1, X, " exa");  --  Solution exacte
    end if;

    ClearDevice (PostScript);
  end Comparaison;

begin
  current_device := PostScript;
  InitGraph (mode => PS_half, file_name => "eds.ps");

  Comparaison (1000, euler,
               T0 => 0.0, T1 => 1.0, X0 => 0.5, la => 1.5, mu => 1.0,
               eex => True);

  Comparaison (1000, milstein,
               T0 => 0.0, T1 => 1.0, X0 => 0.5, la => 1.5, mu => 1.0,
               eex => True);

  Comparaison (1000, euler_faible,
               T0 => 0.0, T1 => 1.0, X0 => 0.5, la => 1.5, mu => 1.0,
               eex => True);

  Comparaison (1000, euler_impl,
               T0 => 0.0, T1 => 1.0, X0 => 0.5, la => 1.5, mu => 1.0,
               eex => True);

  Comparaison (1000, euler_impl_faible,
               T0 => 0.0, T1 => 1.0, X0 => 0.5, la => 1.5, mu => 1.0,
               eex => True);

  Comparaison (1000, euler,
               T0 => 0.0, T1 => 1.0, X0 => 0.5, la => 0.5, mu => 3.0,
               eex => True);

  Comparaison (1000, milstein,
               T0 => 0.0, T1 => 1.0, X0 => 0.5, la => 0.5, mu => 3.0,
               eex => True);

  Comparaison (1000, euler,
               T0 => 0.0, T1 => 1.0, X0 => 0.5, la => 3.5, mu => 0.1,
               eex => True);

  Comparaison (1000, euler,
               T0 => 0.0, T1 => 1.0, X0 => 0.5, la => -16.0, mu => 0.2,
               eex => True);

  Comparaison (1000, euler_impl,
               T0 => 0.0, T1 => 1.0, X0 => 0.5, la => -16.0, mu => 0.2,
               eex => True);

  CloseGraph (PostScript);
end EDS_1;
