with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Time_Log;

package body Graph.PostScript_driver is

  -- *******
  -- Interne: primitives PS. But: economiser de la place au mieux!

  colonne: Natural:= 0;

  procedure PsPS(s:String) is  -- SANS saut
  begin
    Put(dev_file(PostScript),s);
    colonne:= colonne + s'length;
  end;

  procedure PesPS(s:String) is  -- separation, SANS saut
  begin
    if colonne>0 then
      PsPS(" ");
    end if;
    PsPS(s);
  end;

  procedure NlPS is  -- saut
  begin
    New_Line(dev_file(PostScript));
    colonne:= 0;
  end;

  tol_saut: constant:= 72;

  procedure PesoPS(s:String) is  -- separation, saut OPTIONNEL
  begin
    PesPS(s);
    if colonne > tol_saut then
      NlPS;
    end if;
  end;

  procedure PsoPS(s:String) is  -- SANS separation, saut OPTIONNEL
  begin
    PsPS(s);
    if colonne > tol_saut then
      NlPS;
    end if;
  end;

  procedure PlPS(s:String) is  -- separation, saut FORCE
  begin
    PesPS(s);
    NlPS;
  end;

  procedure PiPS(i:integer; separ: Boolean:= True) is
    ii: constant String:= Integer'image(i);
    ni: Integer;
  begin
    if ii(ii'first)=' ' then ni:= ii'first+1; else ni:= ii'first; end if;
    declare
      nombre: constant String:= ii(ni..ii'Last);
    begin
      if separ then
        PesoPS(nombre);
      else
        PsoPS(nombre);
      end if;
    end;
  end;

  procedure xyPS(x,y:integer) is begin PiPS(x); PiPS(y); end;
  procedure mvPS(x,y:integer) is begin xyPS(x,y); PesoPS("m"); end;

  -- *******

  -- Chainage de lignes
  last_x_PS, last_y_PS: integer;

  chainage_ouvert: Boolean;

  procedure PS_Prolog is
    BB: constant array(PostScript_modes) of string(1..14):=
     (" 29 61 561 781"," 29 61 473 781"," 29 67 561 781",
      " 39 61 571 781","127 61 571 781"," 39 67 571 781");
  begin
    Create(dev_file(PostScript), Name=> device_file_name(PostScript).all);
    if EPSF then
     PlPS("%!PS-Adobe-3.0 EPSF-3.0");
     PlPS("%%BoundingBox: " & BB(dev_mode(PostScript)));
     PlPS("%%Pages: 1");
     PlPS("%%EndComments");
     PlPS("%%Page: 1 1");
     PlPS("%BeginDocument: Graph EPSF");
    else
     PlPS("%! PS");
    end if;
    PlPS("% ------------------------------------------------------------");
    PlPS("% PostScript prolog for Graph Ada package       v.  8-Dec-2001");
    PlPS("% G. de Montmollin http://www.mysunrise.ch/users/gdm/graph.htm");
    PlPS("% ------------------------------------------------------------");
    PlPS("% Current time at start of output:" & Time_log );
    PlPS("save");
    PlPS("2.0 setlinewidth");
    -- Ajout 8-Dec-2001 a cause du chainage (nouveau) et des
    -- problemes d'angle entre 2 segments consecutifs ('E' Triscript)
    PlPS("0 setlinejoin");
      -- CF PLRM, ch. 8.2 :
      --  0 Miter join. Subjet to miter limit
      --  1 Round join.
      --  2 Bevel join.
    PlPS("2.0 setmiterlimit");
      --> 1.414 cuts off miters (converts them to bevels) at angles less
      --    than 90 degrees.
      -->   2.0 cuts off miters at angles less than 60 degrees.
      -->  10.0 cuts off miters at angles less than 11 degrees.
      -->   1.0 cuts off miters at all angles, so that bevels are
      --    always produced even whenmiters are specified.
      --The default value of the miter limit is 10.0.

    PlPS("30 780 translate");
    if dev_mode(PostScript) >= PS_landscape_10_pt then
      PlPS("540 0 translate 90 rotate");
    end if;
    case dev_mode(PostScript) is
      when PS_12_pt => PlPS("0.2026 -0.25  scale");
      when PS_10_pt => PlPS("0.2431 -0.25  scale");
      when PS_half  => PlPS("0.1216 -0.124 scale");
      when PS_landscape_12_pt => PlPS("-0.25 0.2026  scale");
      when PS_landscape_10_pt => PlPS("-0.25 0.2431  scale");
      when PS_landscape_half  => PlPS("-0.124 0.1216 scale");
      when others => null;
    end case; -- Size = 48
    PlPS("/Courier-Bold findfont [48 0 0 48 neg 0 0] makefont setfont");
    PlPS("/symfont /Symbol findfont [48 0 0 48 neg 0 0] makefont def");
    PlPS("/bd { bind def }  bind def");
    PlPS("/d { newpath 2.00 0 360 arc fill } bd");
    PlPS("/m { moveto } bd");
    PlPS("/n { newpath moveto lineto } bd");
    PlPS("/l { lineto } bd");
    PlPS("/o { stroke } bd");
    PlPS("/c { pop } bd");
    PlPS("/s { show } bd");
    PlPS("/t { gsave show grestore show } bd");
    PlPS("/u { gsave 0 -1.5 rmoveto show grestore show } bd");
    PlPS("/y { gsave symfont setfont show currentpoint grestore moveto } bd");
    PlPS("/p { showpage restore save} bd");
    PlPS("/pcgcl { setrgbcolor} bd"); -- 3-Feb-2001
    PlPS("save");
    PlPS("% End of prolog, start of output");
    last_x_PS:= -1;
    last_y_PS:= -1;
    chainage_ouvert:= False;
    colonne:= 0;
  end PS_Prolog;

  procedure PS_Close is
  begin
    PS_Page;
    PlPS("restore"); PlPS("restore");
    if EPSF then
      PlPS("%%EndDocument"); PlPS("%%Trailer");
    end if;
    Close(dev_file(PostScript));
  end PS_Close;

  procedure Termine_serie_lignes is
  begin
    if chainage_ouvert then
      PesoPS("o"); -- Stroke
    end if;
    chainage_ouvert:= False;
  end;

  procedure PS_Dot(x,y: Integer) is
  begin
    Termine_serie_lignes;
    xyPS(x,y);
    PesoPS("d");
  end PS_Dot;

  procedure PS_Line(x1,y1, x2,y2: Integer) is
  begin
    if x1/=last_x_PS or else y1/=last_y_PS then
      Termine_serie_lignes;
    end if;

    if chainage_ouvert then -- on a deja un segment
      xyPS(x2,y2);
      PesoPS("l");
    else            -- 1er segment
      xyPS(x2,y2);  -- NB: P2 avant P1
      xyPS(x1,y1);
      PesoPS("n");
      chainage_ouvert:= True;
    end if;

    last_x_PS:= x2;
    last_y_PS:= y2;
  end PS_Line;

  procedure PS_Page is
  begin
    Termine_serie_lignes;
    PesoPS("p");
  end;

  procedure PS_Write(x,y: Integer; s:String) is
    procedure chPS(c:character) is
    begin
      if c='(' or c=')' then PesoPS("(\"&c&") s");
                        else PesoPS('('&c&") s"); end if;
    end;
  begin
    Termine_serie_lignes;
    mvPS(x,y);
    for i in s'range loop chPS(s(i)); end loop;
  end PS_Write;

  -- 3-Feb-2001 :
  procedure Translate_RGB_PS( c: gr_colour ) is
    rgb: array(1..3) of Natural; -- 0..99 scale
    chf: String(1..3);
  begin
    case c is -- coding as described in Graph.ads
      when Black     => rgb:= (   0,   0,   0);
      when Blue      => rgb:= (   0,   0,  44);
      when Green     => rgb:= (   0,  44,   0);
      when Cyan      => rgb:= (   0,  44,  44);
      when Red       => rgb:= (  44,   0,   0);
      when Magenta   => rgb:= (  44,   0,  44);
      when Brown     => rgb:= (  44,  44,   0);
      when LightGray => rgb:= (  44,  44,  44);

      when DarkGray      => rgb:= (   0,   0,   0);
      when LightBlue     => rgb:= (   0,   0,  99);
      when LightGreen    => rgb:= (   0,  99,   0);
      when LightCyan     => rgb:= (   0,  99,  99);
      when LightRed      => rgb:= (  99,   0,   0);
      when LightMagenta  => rgb:= (  99,   0,  99);
      when Yellow        => rgb:= (  99,  99,   0);
      when White         => rgb:= (  99,  99,  99);

      when 100 .. 100 + 99_99_99 => -- RGB code, 0..99 scale
        rgb:= (  (c-100) / 1_00_00,
                ((c-100) / 1_00) mod 1_00,
                 (c-100)         mod 1_00 );

      when others => rgb:= (0,0,0); -- no matching colour
    end case;

    Termine_serie_lignes;

    for c in rgb'range loop
      PesPS(".");
      Put( chf, rgb(c)+100 );
      PsoPS( chf(2..3) );
    end loop;
  end Translate_RGB_PS;

  procedure Set_current_PS_Colour( c: Gr_Colour ) is
  begin
    Translate_RGB_PS( c );
    PesoPS("pcgcl");
  end Set_current_PS_Colour;

  function Hexa(n:Natural) return String is
    h: String(1..13);
    p1,p2: Natural;
  begin
    Put( h, n, Base=> 16);
    for i in h'range loop
      if h(i)='#' then p1:= i; exit; end if;
    end loop;
    for i in reverse h'range loop
      if h(i)='#' then p2:= i; exit; end if;
    end loop;
    return h(p1+1..p2-1);
  end Hexa;

  -- 26-Apr-2002 : TP's 8-bit pattern
  procedure PS_Set_bitmap_pattern( p: FillPatternType; c: Gr_Colour ) is
    function Double_bit( i: Integer ) return Integer is
      r, i2, m: Integer;
    begin
      r:= 0;
      i2:= i;
      m:= 1;
      for p in 0..7 loop
        r:= r + m * 3 * (i2 mod 2);
        i2:= i2/2;
        m:= m * 4;
      end loop;
      return r;
    end;
  begin
    Termine_serie_lignes;
    PesoPS("[/Pattern [/DeviceRGB]] setcolorspace");
    Translate_RGB_PS( c );
    PesoPS("<< /PatternType 1 /PaintType 2 /TilingType 1");
    PesoPS("/BBox [0 0 16 16] /XStep 16 /YStep 16");
    PesoPS("/PaintProc {pop 16 16 scale 16 16 true [16 0 0 -16 0 16] {<");
    for i in p'range loop
      PesoPS(Hexa(Double_bit(Integer(p(i)))));
    end loop;
    PesoPS(">} imagemask } >> matrix makepattern setcolor");
  end PS_Set_bitmap_pattern;

  procedure PS_rectfill(x1,y1, w,h: Integer) is
  begin
    Termine_serie_lignes;
    xyPS(x1,y1);
    xyPS(w,h);
    PesoPS("rectfill");
  end PS_rectfill;

end Graph.PostScript_driver;
