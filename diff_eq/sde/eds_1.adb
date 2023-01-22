------------------------------------------------------------------------------
--  File:            EDS_1.adb
--  Description:     EDS_1
--  Date / Version:  19-Dec-2001 ; 9.VI.1998
--  Author:          Gautier.deMontmollin@Winterthur.ch
------------------------------------------------------------------------------

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_functions; use Ada.Numerics.Elementary_functions;

with Generic_Random_Functions;

with Graph;                             use Graph;
with Graph.Standard_fonts;              use Graph.Standard_fonts;

procedure EDS_1 is

  package FR is new Generic_Random_Functions (Float);

  type Vector is array(Natural range <>) of Float;

  procedure Trajectoire(T0,T1: Float; X: Vector; c: String) is
    D: constant Float:= (T1-T0) / Float(X'Length);
    begin
      Nowhere;
      for i in X'Range loop
        LineTo( T0 + Float(i)*D, X(i) );
        if (i/=X'First) and 
           (i=X'Last or i mod (X'Length / 8)=0) then OutText(c); end if;
      end loop;
    end;

  pala: constant:= 16; 
  palz: constant:= 255; palaz: constant:=palz-pala;

  procedure Palettes is
    p: Float;
    begin
      for i in 0..palaz loop          -- d‚grad‚ 1
        p:= Float(i) / Float(palaz);
        SetRGBPalette(i+pala, Integer(p*63.0), 30, Integer((1.0-p)*63.0));
      end loop;
    end;

  type methode is (euler, euler_impl, milstein, 
                   euler_faible, euler_impl_faible);

  procedure Comparaison(
              ntot: Positive; me:methode; 
              T0,T1,X0,la,mu: Float;
              eex: Boolean) is
    --   ntot= nombre total de pas (fins)
    DW: array(0..ntot) of Float;   -- tirage du mouvement brownien
    X,Y: Vector(0..ntot);          -- solutions (X: exacte, Y: num.)
  
    procedure Simulation(granum: Positive; err: out Float) is
       -- Delta de la m‚thode:
      D: constant Float:= (T1-T0)* Float(granum) / Float(ntot);
      k,ki,kp: Natural;
      Y_ki,DW_kp: Float;
      begin
        err:= 0.0;
        Y(0):= X0;
        for i in 1..1+ntot / granum loop -- "1+" en cas de reste /= 0
          ki:=     i   * granum; -- peut d‚passer ntot si mauvaise granul.
          kp:=   (i-1) * granum;
          DW_kp:= DW(kp) * sqrt(Float(granum)); -- corrige la variance
          case me is
            when euler|euler_faible =>  
              Y_ki:= Y(kp) * (1.0 + la * D + mu * DW_kp);
            when euler_impl|euler_impl_faible =>
              Y_ki:= Y(kp) * (1.0 + mu * DW_kp) / (1.0 - la * D);
            when milstein =>
              Y_ki:= Y(kp) * (1.0 + la * D + mu * DW_kp +
                              mu*mu * 0.5 * ((DW_kp**2) - D));
          end case;
          if ki<= ntot then
            err:= Float'Max(err,abs(Y_ki-X(ki)));
          end if;
          for j in 1..granum loop -- on interpole lin. les valeurs interm.
            k:= kp + j;
            if k<= ntot then -- j=granum: Y(ki) sera := Y_ki si ki<= ntot
              Y(k):= Y(kp) + Float(j) * (Y_ki-Y(kp)) / Float(granum);
            end if;
          end loop;
        end loop;
      end Simulation;

  Wt: Float:= 0.0;
  D: constant Float:= (T1-T0) / Float(ntot); -- Delta fin
  sqD: constant Float:= sqrt(D);
  X1: constant Float:= X0 * exp( (Float'Max(la,0.0)+Float'Max(mu,0.0)*sqD) * (T1-T0) ) + 0.1;
  granumax: constant Positive:= ntot/10;
  ngranu: constant:= 3; -- nb de pas de granularit‚ -1
  g: Positive;
  s: String(1..5); s10: String(1..10);
  txt_fact: Positive; 
  err: Float;

  begin
    -- Tirage mvt brownien:
    for i in DW'Range loop
      case me is
        when euler_faible|euler_impl_faible =>
          if Normal_random>0.0 then DW(i):= sqD; else DW(i):= -sqD; end if;
        when others =>
          DW(i):= Normal_random * sqD;
      end case;
    end loop;
      
    -- Solution exacte
    if eex then 
      X(0):= X0;
      for i in 1..ntot loop
        Wt:= Wt + DW(i);
        X(i):= X0 * exp( (la-0.5*mu**2) * D * Float(i) + mu * Wt );
      end loop;
    end if;
    
    -- Simulation de plus en plus fines:
    for ecran in reverse false..true loop
      BMF.Restore_random_state; -- Reprendre toujours le meme tirage
      if ecran then current_device:= Windows_GDI; txt_fact:= 1; 
               else current_device:= PostScript; txt_fact:= 5; end if;
      Set_math_plane(T0 - 0.1, - 0.2, T1 + 0.1, X1);
      SetTextJustify(LeftText, TopText);
      SetUserCharSize(GetMaxX,1500,GetMaxY,2000);
      SetTextStyle(SansSerifFont,HorizDir);
      if ecran then SetColor(white); end if;
      MoveTo(0.1,X1);
      MoveRel(0,GetMaxY/40);
      OutText(methode'Image(me));
      SetUserCharSize(GetMaxX,2000,GetMaxY,3000);
      MoveRel(0,GetMaxY/20);
      OutText("dXt = la Xt dt + mu Xt dWt");
      MoveRel(0,GetMaxY/40);  Put(s,la,1,0);  OutText("la=" & s);
      MoveRel(0,GetMaxY/40);  Put(s,mu,1,0);  OutText("mu=" & s);
      SetTextStyle(SmallFont,HorizDir,5*txt_fact);
      Draw_Axes;
      SetTextStyle(SmallFont,HorizDir,4*txt_fact);
      SetTextJustify(RightText, CenterText);
      Put(s,X0,1,0);
      OutTextXY(T0,X0,"X0=" & s & ' ');
      SetTextJustify(LeftText, TopText);
      Put(s,T0,2,0);
      OutTextXY(T0,-0.1," T0= " & s);
      Put(s,T1,2,0);
      OutTextXY(T1,-0.1,"T1= " & s);
      SetTextStyle(SmallFont,HorizDir,4*txt_fact);
      for i in reverse 0..ngranu loop
        g:= Natural(Float(granumax)**(Float(i)/Float(ngranu)));
        Simulation( g, err );
        -- if ecran then
        --   SetColor(pala + ((granumax-g)*palaz)/granumax); 
        -- end if;
        SetTextJustify(LeftText, CenterText);
        Trajectoire(T0,T1, Y, Integer'Image(ntot / g));
        MoveTo(0.1,-0.02);
        MoveRel(Float(i)*(T1-0.1)/Float(ngranu),0.0);
        SetTextJustify(CenterText, TopText);
        Put(s10,err,3,0); OutText(Integer'Image(ntot / g)&"->"&s10);
      end loop;
      if ecran then SetColor(yellow); end if;
      if eex then
        SetTextJustify(LeftText, CenterText);
        Trajectoire(T0,T1, X, " exa");  -- exacte
      end if;
    end loop;
    Skip_Line;
    ClearDevice(Windows_GDI);
    ClearDevice(PostScript);
  end Comparaison;
  
begin
  current_device:= Windows_GDI;
  InitGraph;
  Palettes;
  current_device:= PostScript; InitGraph(mode=> PS_half, file_name=>"eds.ps");

  BMF.Save_random_state;
      
  Comparaison( 1000, euler, 
               T0=> 0.0, T1=> 1.0, X0=>0.5, la=>1.5, mu=> 1.0,
               eex=> true );

  Comparaison( 1000, milstein, 
               T0=> 0.0, T1=> 1.0, X0=>0.5, la=>1.5, mu=> 1.0,
               eex=> true );

  Comparaison( 1000, euler_faible, 
               T0=> 0.0, T1=> 1.0, X0=>0.5, la=>1.5, mu=> 1.0,
               eex=> true );

  Comparaison( 1000, euler_impl, 
               T0=> 0.0, T1=> 1.0, X0=>0.5, la=>1.5, mu=> 1.0,
               eex=> true );

  Comparaison( 1000, euler_impl_faible, 
               T0=> 0.0, T1=> 1.0, X0=>0.5, la=>1.5, mu=> 1.0,
               eex=> true );

  Comparaison( 1000, euler, 
               T0=> 0.0, T1=> 1.0, X0=>0.5, la=>0.5, mu=> 3.0,
               eex=> true );
               
  Comparaison( 1000, milstein, 
               T0=> 0.0, T1=> 1.0, X0=>0.5, la=>0.5, mu=> 3.0,
               eex=> true );

  Comparaison( 1000, euler, 
               T0=> 0.0, T1=> 1.0, X0=>0.5, la=>3.5, mu=> 0.1,
               eex=> true );

  Comparaison( 1000, euler, 
               T0=> 0.0, T1=> 1.0, X0=>0.5, la=>-16.0, mu=> 0.2,
               eex=> true );

  Comparaison( 1000, euler_impl, 
               T0=> 0.0, T1=> 1.0, X0=>0.5, la=>-16.0, mu=> 0.2,
               eex=> true );

  CloseGraph(Windows_GDI);
  CloseGraph(PostScript);
end;
