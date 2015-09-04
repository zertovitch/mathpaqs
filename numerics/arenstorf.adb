------------------------------------------------------------------------------
--  File:            arenstorf.adb
--  Description:     Arenstorf periodic orbits - restricted three body problem
--
--    from: Hairer - Noersett - Wanner,
--      Solving ordinary differential equations I, Springer-Verlag 1987
--      ch. II: Runge-Kutta and extrapolation
--
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_functions;

with Dormand_Prince_8, G_Matrices;
with Graph;                             use Graph;

procedure Arenstorf is

  subtype prec_float is Long_Long_Float;

  package LFEF is new
               Ada.Numerics.Generic_Elementary_functions(prec_float);
  use LFEF;

  type Vector is array (Integer range <>) of prec_float;
  type Matrix is array (Integer range <>, Integer range <>) of prec_float;

  package LFM is new
    G_Matrices(
      prec_float,
      0.0,1.0,
      "-", Sqrt,"+","-","*","/",
      Vector,
      Matrix);
  use LFM;

--  package PFMM is new Min_Max(prec_float,">");
--  use PFMM;

  package PFIO is new Float_IO(prec_float); use PFIO;


-- Restricted three body problem

  mu:  constant:= 0.012277471;   -- moon
  mup: constant:= 1.0 - mu;      -- earth

  t_end_1_tour: constant:= 17.0652165601579625588917206249;

  h, t, t_end: prec_float;

  subtype vector4 is vector(1..4);

  x: vector4;  -- = (x1,x2,x1',x2')
  x0: constant vector4:=
    ( 0.994, 0.0, 0.0, -2.00158510637908252240537862224 );

  type t_meth is
    (Euler1, Runge_Kutta_class4, Runge_Kutta_3_8_4,
     Butcher6, DoPriF7, DoPriF8,
     DoPriV8 );  -- V: variable step size (F: fixed)

  n_meth: constant:= 1 + t_meth'Pos(t_meth'Last) - t_meth'Pos(t_meth'First);

  ntours: constant array(t_meth) of Positive:=
    (1, 1, 1, 4, 4, 4, 5);

  niter_1_tour: constant array(t_meth) of Positive:=
    (200_000,  6_000, 6_000,
     50_000,  20_000, 20_000,
     600);

  niter: array(t_meth) of Positive;

  function f(x:vector4) return vector4 is
    x1p: constant prec_float:= x(1)+mu;
    x1m: constant prec_float:= x(1)-mup;
    D1: constant prec_float:= sqrt(( x1p**2 + x(2)**2  )**3);
    mup_sur_D1: constant prec_float:= mup/D1;
    D2: constant prec_float:= sqrt(( x1m**2 + x(2)**2  )**3);
    mu_sur_D2:  constant prec_float:= mu /D2;
  begin
    return
      ( x(3),    -- x1'
        x(4),    -- x2'
        x(1) + 2.0 * x(4) - mup_sur_D1 * x1p  - mu_sur_D2 * x1m,
        x(2) - 2.0 * x(3) - mup_sur_D1 * x(2) - mu_sur_D2 * x(2) );
  end f;

  -- Pour DoPri8 (pas variable)
  accepted: Boolean;
  eps: constant:= 1.0E-13;   -- entre E-7 et E-13
  uround: constant:= prec_float'Epsilon;
  hnew: prec_float;
  hprem, hdern, hmin, hmax: prec_float;
  it: Natural;

  procedure Evolution(meth: t_meth) is
    use Dormand_Prince_8;
    tiers: constant:= 1.0/3.0;
    k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13,
    xp7, xp8: vector4;
    bu_a: constant array (3..7, 1..6) of prec_float:=
 ( (  2.0/9.0,     4.0/ 9.0, others => 0.0),
   (  7.0/36.0,    2.0/ 9.0,  -1.0/ 12.0, others => 0.0),
   (-35.0/144.0, -55.0/36.0,  35.0/ 48.0,   15.0/8.0, others => 0.0),
   ( -1.0/360.0, -11.0/36.0,  -1.0/  8.0,    0.5,     0.1, others => 0.0),
   (-41.0/260.0,  22.0/13.0,  43.0/156.0, -118.0/39.0, 32.0/195.0, 80.0/39.0));
    bu_b: constant array (1..7) of prec_float:=
   (13.0/200.0, 0.0, 11.0/40.0, 11.0/40.0, 4.0/25.0, 4.0/25.0, 13.0/200.0);
    fac, denom, err: prec_float;
  begin
    case meth is
      when Euler1 => x:= x + h * f(x); t:= t+h;
      when Runge_Kutta_class4 =>
        k1:= f(x);
        k2:= f(x + h * 0.5 * k1);
        k3:= f(x + h * 0.5 * k2);
        k4:= f(x + h *       k3);
        x:= x + h * (1.0/6.0) * (k1 + 2.0 * k2 + 2.0 * k3 + k4);
        t:= t+h;
      when Runge_Kutta_3_8_4 =>
        k1:= f(x);
        k2:= f(x + h * tiers * k1);
        k3:= f(x + h * (-tiers * k1 + k2) );
        k4:= f(x + h * ( k1 - k2 + k3 ) );
        x:= x + h * (1.0/8.0) * (k1 + 3.0 * k2 + 3.0 * k3 + k4);
        t:= t+h;
      when Butcher6 =>
        k1:= f(x);
        k2:= f(x + h * 0.5 * k1);
        k3:= f(x + h * (bu_a(3,1) * k1 + bu_a(3,2) * k2) );
        k4:= f(x + h * (bu_a(4,1) * k1 + bu_a(4,2) * k2
                      + bu_a(4,3) * k3) );
        k5:= f(x + h * (bu_a(5,1) * k1 + bu_a(5,2) * k2
                      + bu_a(5,3) * k3 + bu_a(5,4) * k4) );
        k6:= f(x + h * (bu_a(6,1) * k1 + bu_a(6,2) * k2
                      + bu_a(6,3) * k3 + bu_a(6,4) * k4
                      + bu_a(6,5) * k5) );
        k7:= f(x + h * (bu_a(7,1) * k1 + bu_a(7,2) * k2
                      + bu_a(7,3) * k3 + bu_a(7,4) * k4
                      + bu_a(7,5) * k5 + bu_a(7,6) * k6) );
        x:= x + h * (bu_b(1) * k1                + bu_b(3) * k3 +
                     bu_b(4) * k4 + bu_b(5) * k5 + bu_b(6) * k6+
                     bu_b(7) * k7);
        t:= t+h;
      when DopriF7 | DopriF8 | DoPriV8 =>
        k1:=  f(x);
        k2:=  f(x + h *  a21 * k1);
        k3:=  f(x + h * (a31 * k1 + a32 * k2) );
        k4:=  f(x + h * (a41 * k1 + a43 * k3) );
        k5:=  f(x + h * (a51 * k1 + a53 * k3 + a54 * k4) );
        k6:=  f(x + h * (a61 * k1 + a64 * k4 + a65 * k5) );
        k7:=  f(x + h * (a71 * k1 + a74 * k4 + a75 * k5 + a76 * k6) );
        k8:=  f(x + h * (a81 * k1 + a84 * k4 + a85 * k5 + a86 * k6 +
                         a87 * k7) );
        k9:=  f(x + h * (a91 * k1 + a94 * k4 + a95 * k5 + a96 * k6 +
                         a97 * k7 + a98 * k8) );
        k10:= f(x + h * (a101 * k1 + a104 * k4 + a105 * k5 + a106 * k6 +
                         a107 * k7 + a108 * k8 + a109 * k9) );
        k11:= f(x + h * (a111 * k1 + a114 * k4 + a115 * k5 + a116 * k6 +
                         a117 * k7 + a118 * k8 + a119 * k9 +
                         a1110 * k10) );
        k12:= f(x + h * (a121 * k1 + a124 * k4 + a125 * k5 + a126 * k6 +
                         a127 * k7 + a128 * k8 + a129 * k9 +
                         a1210 * k10 + a1211 * k11) );
        k13:= f(x + h * (a131 * k1 + a134 * k4 + a135 * k5 + a136 * k6 +
                         a137 * k7 + a138 * k8 + a139 * k9 +
                         a1310 * k10 + a1311 * k11) );
        case meth is
          when DoPriF8 =>
            x:= x + h * (b1 * k1 + b6 * k6 + b7 * k7 + b8 * k8 + b9 * k9 +
                         b10 * k10 + b11 * k11 + b12 * k12 + b13 * k13);
            t:= t+h;
          when DoPriF7 =>
            x:= x + h * (bh1 * k1 + bh6 * k6 + bh7 * k7 + bh8 * k8 +
                         bh9 * k9 + bh10 * k10 + bh11 * k11 + bh12 * k12);
            t:= t+h;
          when DoPriV8 =>
            xp8:= x + h * (b1 * k1 + b6 * k6 + b7 * k7 + b8 * k8 + b9 * k9 +
                           b10 * k10 + b11 * k11 + b12 * k12 + b13 * k13);
            xp7:= x + h * (bh1 * k1 + bh6 * k6 + bh7 * k7 + bh8 * k8 +
                           bh9 * k9 + bh10 * k10 + bh11 * k11 + bh12 * k12);

            if it = niter(meth) or else t+0.3*h=t then
              t:= t_end; -- echec!
            else
              err:= 0.0;
              for i in vector4'Range loop
                denom:=
                  prec_float'Max(
                    prec_float'Max(1.0E-6, abs(xp8(i))),
                    prec_float'Max(abs(x(i)), 2.0*uround/eps)
                  );
                err:= err + ((xp8(i)-xp7(i)) / denom) ** 2;
              end loop;
              err:= sqrt(err / prec_float(vector4'Length));
              fac:= prec_float'Max(
                1.0/6.0,
                prec_float'Min(3.0, ((err/eps)**(1.0/8.0)) / 0.9)
              );
              hnew:= h/fac;
              -- if hnew > hprem then hnew:= hprem; end if;
              if err < eps then
                 x:= xp8;
                 t:= t + h;
                 if not accepted and then h < hnew then
                   hnew:= h;
                 end if;
                 accepted:= true;
              else
                 accepted:= false;
              end if;
              h:= hnew;
            end if;
          when others => null;
        end case;
    end case;
  end Evolution;

-- VESA Palette

  pala: constant:= 16; palz: constant:= 255; palaz: constant:=palz-pala;
  spal: constant:= (palaz+1) / n_meth;

  procedure Degrade is
    p, q: Float; c: Natural;
  begin
   for i in 0..n_meth-1 loop
    for j in 0..spal - 1 loop
      c:= pala + spal*i + j;
      p:= Float(i) / Float(n_meth-1);
      q:= Float(j) / Float(spal - 1);
      SetRGBPalette(c, Integer(p*63.0), Integer(q*63.0), 63-Integer(p*63.0));
    end loop;
   end loop;
  end;

  dessine: constant array(t_meth) of Boolean:=
    (Euler1 => true,
     Runge_Kutta_class4 | Runge_Kutta_3_8_4 => false,
     Butcher6 => true,
     DoPriF7 => false,
     DoPriF8 => false,
     DoPriV8 => true );

    rapXY: Float;

begin

  for i in niter'Range loop
    niter(i):= ntours(i) * niter_1_tour(i);
  end loop;

  current_device:= PostScript;
  InitGraph(PostScript, "Arenstorf.ps");

  -- DOS:
  --  current_device:= VESA;
  --  InitGraph(VESA, VESA_1280x1024);
  Degrade;

  rapXY:= Float(GetMaxY)/Float(GetMaxX);
  Set_math_plane(-1.5,-1.5*rapXY,1.5,1.5*rapXY);
  Draw_axes;

  for meth in t_meth loop
    if dessine(meth) then
      t_end:= prec_float(ntours(meth)) * t_end_1_tour;
      x:= x0;
      h:= t_end / prec_float(niter_1_tour(meth));
      hmin:= h;
      hmax:= h;
      hprem:= h;
      t:= 0.0;
      it:= 0;
      accepted:= true;
      MoveTo(Float(x(1)),Float(x(2)));

      while t < t_end * (1.0-prec_float(niter(meth))*uround) loop
        Evolution(meth);
        if accepted then
         LineTo(Float(x(1)),Float(x(2)));
--         SetColor( pala + (t_meth'Pos(meth) - t_meth'Pos(t_meth'First)) * spal
--                        + (it*(spal-1)/niter(meth)) );
        end if;
        if hmin>h then hmin:= h; end if;
        if hmax<h then hmax:= h; end if;
        it:= it+1;
      end loop;
      SetTextJustify(LeftText, CenterText);
      OutText(t_meth'Image(meth));
      MoveRel(-TextWidth('0')/2, TextHeight('8'));
      OutText(Integer'Image(it) & " points");
      MoveRel(0, TextHeight('8'));
      OutText(Integer'Image(ntours(meth)) & " orbits");
      MoveRel(0, TextHeight('8'));
      OutText(Integer'Image(it/ntours(meth)) & " p/o");
      hdern:= h;
    end if; -- dessine
  end loop; -- meth

  CloseGraph;

  put("hprem= "); put(hprem);    new_line;
  put("hmin=  "); put(hmin);     new_line;
  put("hmax=  "); put(hmax);     new_line;
  put("hdern= "); put(hdern);    new_line;

end;
