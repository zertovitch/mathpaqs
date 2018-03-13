-----------------------------------------------------------------------------
--  File: Graph.adb; see specification (Graph.ads)
-----------------------------------------------------------------------------






with Ada.Text_IO;         use Ada.Text_IO;
-- with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics;        use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with Graph.PostScript_driver;           use Graph.PostScript_driver;

with Time_display;

-- Now reference all devices made by Jerry van Dijk:







package body Graph is

  function Min(a,b: Integer) return Integer renames Integer'Min;
  function Max(a,b: Integer) return Integer renames Integer'Max;
  function Max(a,b: Float) return Float     renames Float'Max;

  PC, VP_a,VP_b: array(device_type) of PointType;
  aspect_ratio: array(device_type) of PointType:= (others=> (1,1));

  -- Math plane area
  type t_math_scr is record
    x1,y1, x2,y2, l,h, fMaxX, fMaxY, toPixX,toPixY, toMathX,toMathY: float;
  end record;
  type p_math_scr is access all t_math_scr;
  math_scr: array(device_type) of aliased t_math_scr;

  needs_a_name: constant array(device_type) of boolean:=
    (PostScript|AutoCAD_DXF=> true, others=> false); -- 31.V.1998

  -- Identifies vectorial devices -- 26-Apr-2002
  vectorial: constant array(Device_type) of Boolean:=
    (PostScript|AutoCAD_DXF=> True, others=> False);

  -- Modes available
  max_mode: constant array(device_type) of positive:= (



   PostScript => 6, others => 1);

  mxmxmode: constant:= 6;

-- max coord.
  gxx:  array(device_type,1..mxmxmode) of integer:= (






     PostScript  =>(2182,2182,4364, 2872,2872,5744),
     AutoCAD_DXF =>(10000,0,0,0,0,0));

  gxy:  array(device_type,1..mxmxmode) of integer:= (






     PostScript  =>(2872,2872,5744, 2182,2182,4364),
     AutoCAD_DXF =>(10000,0,0,0,0,0));

-- misc.
  sizeable: constant array(device_type) of boolean:=
    ( AutoCAD_DXF => true,



      others      => false );

  reversed: constant array(device_type) of boolean:=
    (AutoCAD_DXF => false, others => true);

-- text settings:
  txtset: TextSettingsType;
  txt_rapX,txt_rapY, txtangle, txtsin, txtcos: float;
  txt_bitmap: boolean;

-- misc.
  aspect_r_fl: array(device_type) of float:= (others=>1.0);
  c_colour, c_bgd_colour: array( Device_type ) of Gr_Colour;
  PCnowhere, clipping: array(device_type) of boolean:= (others => true);
  opened: array(device_type) of boolean:= (others => false);
  dern_arc: ArcCoordsType;

  -- Current colours, with specific device types








  vers_rad: constant float:= pi/180.0;

  -- Fill settings [ 26-Apr-2002 ]:
  standard_pattern: constant array( FillStyle ) of FillPatternType :=
  (
   EmptyFill      =>
         (  2#00000000#,
            2#00000000#,
            2#00000000#,
            2#00000000#,
            2#00000000#,
            2#00000000#,
            2#00000000#,
            2#00000000#),
   SolidFill      =>
         (  2#11111111#,
            2#11111111#,
            2#11111111#,
            2#11111111#,
            2#11111111#,
            2#11111111#,
            2#11111111#,
            2#11111111#),
   LineFill       =>
         (  2#11111111#,
            2#11111111#,
            2#00000000#,
            2#00000000#,
            2#11111111#,
            2#11111111#,
            2#00000000#,
            2#00000000#),
   LtSlashFill    =>
         (  2#01000000#,
            2#10000000#,
            2#00000001#,
            2#00000010#,
            2#00000100#,
            2#00001000#,
            2#00010000#,
            2#00100000#),
   SlashFill      =>
         (  2#11100000#,
            2#11000001#,
            2#10000011#,
            2#00000111#,
            2#00001110#,
            2#00011100#,
            2#00111000#,
            2#01110000#),
   BkSlashFill    =>
         (  2#11000011#,
            2#11100001#,
            2#11110000#,
            2#01111000#,
            2#00111100#,
            2#00011110#,
            2#00001111#,
            2#10000111#),
   LtBkSlashFill  =>
         (  2#01011010#,
            2#00101101#,
            2#10010110#,
            2#01001011#,
            2#10100101#,
            2#11010010#,
            2#01101001#,
            2#10110100#),
   HatchFill      =>
         (  2#11111111#,
            2#00100010#,
            2#00100010#,
            2#00100010#,
            2#11111111#,
            2#00100010#,
            2#00100010#,
            2#00100010#),
   XHatchFill     =>
         (  2#10000001#,
            2#01000010#,
            2#00100100#,
            2#00011000#,
            2#00011000#,
            2#00100100#,
            2#01000010#,
            2#10000001#),
   InterleaveFill =>
         (  2#00110011#,
            2#11001100#,
            2#00110011#,
            2#11001100#,
            2#00110011#,
            2#11001100#,
            2#00110011#,
            2#11001100#),
   WideDotFill    =>
         (  2#00001000#,
            2#00000000#,
            2#10000000#,
            2#00000000#,
            2#00001000#,
            2#00000000#,
            2#10000000#,
            2#00000000#),
   CloseDotFill   =>
         (  2#00100010#,
            2#00000000#,
            2#10001000#,
            2#00000000#,
            2#00100010#,
            2#00000000#,
            2#10001000#,
            2#00000000#),
   UserFill       =>
         (  2#10001000#,
            2#00000000#,
            2#00100010#,
            2#00000000#,
            2#10001000#,
            2#00000000#,
            2#00100010#,
            2#00000000#)
  );

  current_fill_style       : FillStyle:= SolidFill; -- TP's default is Solid
  current_fill_pattern     : FillPatternType:= standard_pattern( SolidFill );
  current_fill_pattern_user: FillPatternType:= standard_pattern( UserFill );
  current_fill_colour      : array( Device_type ) of Gr_Colour;
  -- ^GraphDefault (re)sets the fill colour.

-- Vectors
  angle_vect: constant:= pi-0.3;
  cos_angle_vect: constant float:= cos(angle_vect);
  sin_angle_vect: constant float:= sin(angle_vect);

-- Virtual screens 28.XII.1997: SetActivePage, SetVisualPage
  active_page, visual_page: array(device_type) of page_range:= (others=> 0);
  multi_page_available: array(device_type) of boolean:= (



    others=> false);
  multi_page_ON: array(device_type) of boolean:=  (others=> false);
    -- ^ for "manual" page swapping

-- VESA virtual screens (DOS only)









---------------------- internal routines ---------------------------------
  procedure PlAC(s:String) is begin Put_Line(dev_file(AutoCAD_DXF),s); end;
  procedure PiAC(i:integer) is begin PlAC(integer'image(i)); end;

  procedure Ellipse(x,y:integer; a1,a2,rx,ry:float; d:device_type:=current_device) is
    -- version interne (pixel, avec angles & rayons en vir. flott.)
    ar1: constant float:= a1*vers_rad;
    ar2: constant float:= a2*vers_rad;
    npas: constant integer:= integer((ar2-ar1)*Max(rx,ry));
    a: constant float:= (ar2-ar1)/float(npas);
    sa: constant float:= sin(a);
    ca: constant float:= cos(a);
    x0,y0,x1: float;
    begin
      x0:= cos(ar1);
      y0:= sin(ar1);
      dern_arc.xstart:= x+integer(x0*rx);
      dern_arc.ystart:= y-integer(y0*ry);
      MoveTo(dern_arc.xstart,dern_arc.ystart,d);
      for i in 1..npas loop
        x1:= ca*x0 - sa*y0;
        y0:= sa*x0 + ca*y0;
        x0:= x1;
        LineTo(x+integer(x0*rx),y-integer(y0*ry),d);
      end loop;
      dern_arc.xend:=PC(d).x; dern_arc.yend:=PC(d).y;
      MoveTo(x,y,d);
      dern_arc.x:=x; dern_arc.y:=y;
    end;

  procedure Clipped_absolute_line(x1,y1,x2,y2:integer; d:device_type:=current_device) is
  pragma Inline(Clipped_absolute_line);
  begin
    case d is











      when PostScript=>
        PS_line(x1,y1, x2,y2);
      when AutoCAD_DXF=>
        PiAC(0); PlAC("LINE"); PiAC(8); PlAC("0");
        PiAC(10); PiAC(x1); PiAC(20); PiAC(y1); PiAC(30); PiAC(0);
        PiAC(11); PiAC(x2); PiAC(21); PiAC(y2); PiAC(31); PiAC(0);
    end case;
  end;

  procedure Clipped_absolute_hor_line(x1,x2,y:integer; d:device_type:=current_device) is
  begin
    case d is









      when others=> Clipped_absolute_line(x1,y, x2,y, d);
    end case;
  end;

-- Virtual screens 28.XII.1997: SetActivePage, SetVisualPage
  procedure Prepare_multi_page(d:device_type) is
    begin
      if not multi_page_ON(d) then -- prepare virtual screens
        for i in page_range loop
          case d is





            when others=> null;
          end case;
        end loop;
        multi_page_ON(d):= true;
      end if;
    end;

  procedure update_txt_internals is
    t: constant array(1..8) of float:=
     (4.0/7.0, 9.0/14.0, 3.0/4.0, 1.0, 37.0/28.0, 23.0/14.0, 2.0, 2.5);
    s: constant natural:= txtset.CharSize;
  begin
    txt_bitmap:= (txtset.Font=DefaultFont);
    case s is
      when UserCharSize=> null; -- already defined by setuser...
      when 1..8=>   txt_rapX:= t(s);
      when others=> txt_rapX:= float(s)*0.3125;
    end case;
    if s/=UserCharSize then txt_rapY:= txt_rapX; end if;
    case txtset.Direction is
      when HorizDir => txtangle:= 0.0;
      when VertDir =>  txtangle:= pi/2.0;
      when AnyDir =>   null; -- already defined by setuser...
    end case;
    txtsin:= sin(txtangle);
    txtcos:= cos(txtangle);
  end;

  function Code_RGB( R,G,B: Float_0_1 ) return Gr_Colour is
  begin
    return 100 +
           Gr_Colour(R*99.0) * 10000 +
           Gr_Colour(G*99.0) * 100 +
           Gr_Colour(B*99.0);
  end Code_RGB;

---------------------- external routines ---------------------------------
  function "+" (v1,v2:vector2) return vector2 is
    begin return (v1.x+v2.x,v1.y+v2.y); end;

  function "*" (f:float; v:vector2) return vector2 is
    begin return (f*v.x,f*v.y); end;

  function "*" (v:vector2; f:float) return vector2 is
    begin return (f*v.x,f*v.y); end;

  function Norm2(v:vector2) return float is
    begin return sqrt(v.x*v.x+v.y*v.y); end;

  function "+" (pl:polygon_type; pt:PointType) return polygon_type is
    p: polygon_type(pl'range);
    begin
      for i in p'range loop p(i).x:= pl(i).x+pt.x; p(i).y:= pl(i).y+pt.y; end loop;
      return p;
    end;

  function "+" (pl:math_polygon_type; pt:math_PointType) return math_polygon_type is
    p: math_polygon_type(pl'range);
    begin
      for i in p'range loop p(i).x:= pl(i).x+pt.x; p(i).y:= pl(i).y+pt.y; end loop;
      return p;
    end;

  function Dilatation(pl:math_polygon_type; fx,fy:float) return math_polygon_type is
    p: math_polygon_type(pl'range);
    begin
      for i in p'range loop p(i).x:= pl(i).x*fx; p(i).y:= pl(i).y*fy; end loop;
      return p;
    end;

  function "*" (pl:math_polygon_type; f:float) return math_polygon_type is
    begin return Dilatation(pl,f,f); end;

  function "*" (f:float; pl:math_polygon_type) return math_polygon_type is
    begin return Dilatation(pl,f,f); end;

  procedure Adapt_for_view_port(x,y: in out integer; d:device_type:=current_device) is
    begin
      x:= x-VP_a(d).x;
      y:= y-VP_a(d).y;
    end;

  procedure Arc(x,y,a1,a2,r:integer; round_aspect:boolean:= true; d:device_type:=current_device) is
  begin
    if round_aspect then
      Ellipse(x,y,a1,a2,r,(r*aspect_ratio(d).x) / aspect_ratio(d).y,d);
    else
      Ellipse(x,y,a1,a2,r,r,d);
    end if;
  end Arc;

  procedure Arc(x,y,a1,a2,r:float;   round_aspect:boolean:= true; d:device_type:=current_device) is
    i,j: integer;
  begin
    if round_aspect then
      Coord(x,y, i,j, d);
      Arc(i,j,integer(a1),integer(a2),integer(math_scr(d).toPixX*r),true,d);
    else
      Ellipse(x,y,a1,a2,r,r,d);
    end if;
  end Arc;

  procedure Bar_bitmap(x1, y1, x2, y2: Integer; d: Device_type:= current_device) is
    xx1,xx2,yy1,yy2, x8,y8: Integer; mask: Unsigned_8;
  begin
    xx1:= Min(x1,x2);
    xx2:= Max(x1,x2);
    yy1:= Min(y1,y2);
    yy2:= Max(y1,y2);

    for y in yy1..yy2 loop
      y8:= y mod 8;
      for x in xx1..xx2 loop
        x8:= 7-(x mod 8);
        mask:= 2**x8;
        if (current_fill_pattern( y8 ) and mask) /= 0 then
          PutPixel(x,y, current_fill_colour(d), d);
        else
          PutPixel(x,y, c_bgd_colour(d), d);
        end if;
      end loop;
    end loop;
  end Bar_bitmap;

  procedure Bar_vectorial(x1, y1, x2, y2: Integer;
              d: Device_type:= current_device) is
    xx1,xx2,yy1,yy2: Integer;
    ia: constant integer:= VP_a(d).x;
    ib: constant integer:= VP_b(d).x;
    ja: constant integer:= VP_a(d).y;
    jb: constant integer:= VP_b(d).y;
  begin
    xx1:= Min(x1,x2)+ia;
    xx2:= Max(x1,x2)+ia;
    yy1:= Min(y1,y2)+ja;
    yy2:= Max(y1,y2)+ja;

    if xx2 < ia or xx1 > ib or
       yy2 < ja or yy1 > jb then
      return; -- off viewport
    end if;

    -- clipping, ##2 < ##1 impossible due to previous test
    xx1:= Max(ia,xx1);
    xx2:= Min(ib,xx2);
    yy1:= Max(ja,yy1);
    yy2:= Min(jb,yy2);
    case d is
      when PostScript =>
        PS_Set_bitmap_pattern(
          standard_pattern(SolidFill),
          c_bgd_colour(PostScript)
        );
        PS_rectfill(xx1,yy1, xx2-xx1,yy2-yy1);
        PS_Set_bitmap_pattern(
          current_fill_pattern,
          current_fill_colour(PostScript)
        );
        PS_rectfill(xx1,yy1, xx2-xx1,yy2-yy1);
        Set_current_PS_Colour(c_colour(PostScript));
      when others => null; -- Bitmap is a bad solution !
    end case;
  end Bar_vectorial;

  procedure Bar(x1, y1, x2, y2: Integer; d: Device_type:= current_device) is
  begin
    if vectorial(d) then
      Bar_vectorial(x1,y1,x2,y2,d); -- Clip and call the device's routine
    else
      Bar_bitmap(x1,y1,x2,y2,d);    -- We can draw ourselves the bar
    end if;
  end Bar;

  procedure Bar(x1, y1, x2, y2: Float;   d: Device_type:= current_device) is
    i1,i2,j1,j2: Integer;
  begin
    Coord(x1,y1, i1,j1, d);
    Coord(x2,y2, i2,j2, d);
    Bar(i1,j1, i2,j2, d);
  end Bar;

  procedure Bar3D(x1, y1, x2, y2: Integer; Depth: Natural; Top: Boolean;
                  d: Device_type:= current_device) is
    xx1,xx2,yy1,yy2: Integer;
  begin
    Bar(x1,y1,x2,y2,d);
    Rectangle(x1,y1,x2,y2,d);

    xx1:= Min(x1,x2);
    xx2:= Max(x1,x2);
    yy1:= Min(y1,y2);
    yy2:= Max(y1,y2);

    if Top then
      Line(xx1,yy1,xx1+depth,yy1-depth,d);
      LineTo(xx2+depth,yy1-depth,d);
      LineTo(xx2,yy1,d);
    end if;
    Line(xx2+depth,yy1-depth,xx2+depth,yy2-depth,d);
    LineTo(xx2,yy2,d);
  end Bar3D;

  procedure Bar3D(x1, y1, x2, y2: Float;   Depth: Natural; Top: Boolean;
                  d: Device_type:= current_device) is
    i1,i2,j1,j2: Integer;
  begin
    Coord(x1,y1, i1,j1, d);
    Coord(x2,y2, i2,j2, d);
    Bar3D(i1,j1, i2,j2, Depth, Top, d);
  end Bar3D;

  procedure Circle(x,y,r:integer; round_aspect:boolean:= true; d:device_type:=current_device) is
  begin
    Arc(x,y,0,360,r,round_aspect,d);
  end Circle;

  procedure Circle(x,y,r:float;   round_aspect:boolean:= false; d:device_type:=current_device) is
  begin
    Arc(x,y,0.0,360.0,r,round_aspect,d);
  end Circle;

  procedure ClearDevice(d:device_type:=current_device) is
  begin
    if not opened(d) then raise Device_not_opened; end if;
    case d is











      when PostScript=>
        if EPSF then raise EPSF_is_for_1_page; else PS_Page; end if;
      when others=> null;
    end case;
  end;

  procedure ClearViewPort(d:device_type:=current_device) is
  begin
    if not opened(d) then raise Device_not_opened; end if;
    case d is















      when others=> null;
    end case;
  end ClearViewPort;

  procedure CloseGraph(d:device_type:=current_device) is
  begin
    if not opened(d) then raise Device_not_opened; end if;
    case d is






      when PostScript=>
        PS_Close;

      when AutoCAD_DXF=>
        PiAC(0);PlAC("ENDSEC");PiAC(0);PlAC("EOF");
        Close(dev_file(AutoCAD_DXF));
    end case;
    if multi_page_ON(d) then   -- free virtual screens
      for i in page_range loop
        case d is



          when others=> null;
        end case;
      end loop;
      multi_page_ON(d):= false;
    end if;
    opened(d):= false;
  end;

  procedure Coord(x,y:float; i,j:out integer; d:device_type:=current_device) is
    m: constant p_math_scr:= math_scr(d)'Access;
    begin
      i:=   integer( (x-m.x1)*m.toPixX );
      if reversed(d) then
        j:= integer( (m.y1-y)*m.toPixY + m.fMaxY );
      else
        j:= integer( (y-m.y1)*m.toPixY );
      end if;
    end;

  procedure Coord(i,j:integer; x,y:out float; d:device_type:=current_device) is
    m: constant p_math_scr:= math_scr(d)'Access;
    begin
      x:=   m.x1 + float(i) * m.toMathX;
      if reversed(d) then
        y:= m.y2 - float(j) * m.toMathY;
      else
        y:= m.y1 + float(j) * m.toMathY;
      end if;
    end;

  procedure Draw_axes(axes:boolean:= true; units,grid_lines,grid_points,border,scale: boolean:= false; scalex,scaley:positive:= 1; d:device_type:=current_device) is
    m: constant p_math_scr:= math_scr(d)'Access;
    x1: constant float:= m.x1;
    y1: constant float:= m.y1;
    x2: constant float:= m.x2;
    y2: constant float:= m.y2;
    trait_x: constant float:=  m.toMathX*4.0;
    trait_y: constant float:=  m.toMathY*4.0;
    begin
      if axes then
        Line(x1,0.0,x2,0.0,d);
        Line(0.0,y1,0.0,y2,d);
      end if;
      if border then Rectangle(x1,y1,x2,y2,d); end if;
      if units then
        OutTextXY(1.0,0.0-m.toMathY*10.0,"1",d);
        OutTextXY(0.0+m.toMathX*10.0,1.0,"1",d);
      end if;
      if grid_lines then
        for i in integer(x1)+1 .. integer(x2)-1 loop
          Line(float(i),y1,float(i),y2,d);
        end loop;
        for j in integer(y1)+1 .. integer(y2)-1 loop
          Line(x1,float(j),x2,float(j),d);
        end loop;
      elsif grid_points then
        for i in integer(x1)+1 .. integer(x2) loop
        for j in integer(y1)+1 .. integer(y2) loop
          Point(float(i),float(j),d);
        end loop;
        end loop;
      end if;
      if scale then
        for i in integer(x1)+1 .. integer(x2) loop
          if i mod scalex=0 then
            Line(float(i),-trait_y,float(i),trait_y,d);
          end if;
        end loop;
        for j in integer(y1)+1 .. integer(y2) loop
          if j mod scaley=0 then
            Line(-trait_x,float(j),trait_x,float(j),d);
          end if;
        end loop;
      end if;
    end Draw_axes;

  procedure DrawPoly(p:polygon_type; d:device_type:=current_device) is
    begin
      MoveTo(p(p'last).x,p(p'last).y,d);
      for i in p'range loop LineTo(p(i).x,p(i).y,d); end loop;
    end;

  procedure DrawPoly(p:math_polygon_type; d:device_type:=current_device) is
    begin
      MoveTo(p(p'last).x,p(p'last).y,d);
      for i in p'range loop LineTo(p(i).x,p(i).y,d); end loop;
    end;

  procedure Ellipse(x,y,a1,a2,rx,ry:integer; d:device_type:=current_device) is
    begin Ellipse(x,y,float(a1),float(a2),float(rx),float(ry),d); end;

  procedure Ellipse(x,y,a1,a2,rx,ry:float;   d:device_type:=current_device) is
    i,j: integer;
    begin
      Coord(x,y, i,j, d);
      Ellipse(i,j,a1,a2,math_scr(d).toPixX*rx,math_scr(d).toPixY*ry,d);
    end;

  procedure FillEllipse(x,y,rx,ry:integer; d:device_type:=current_device) is
  begin
    -- (!! filling)
    Ellipse(x,y,0,360,rx,ry,d);
  end;

  procedure FillEllipse(x,y,rx,ry:float;   d:device_type:=current_device) is
  begin
    -- (!! filling)
    Ellipse(x,y,0.0,360.0,rx,ry,d);
  end;

  procedure FillPoly(p:polygon_type; d:device_type:=current_device) is
  begin
    -- (!! filling)
    DrawPoly(p,d);
  end;

  procedure FillPoly(p:math_polygon_type; d:device_type:=current_device) is
  begin
    -- (!! filling)
    DrawPoly(p,d);
  end;

  procedure FloodFill(x,y:integer; border:gr_colour; d:device_type:=current_device) is
    -- !! border to be implemented
  begin
    case d is





      when others=> null;
    end case;
  end FloodFill;

  procedure FloodFill(x,y:float;   border:gr_colour; d:device_type:=current_device) is
    i,j: integer;
  begin
    Coord(x,y, i,j, d);
    FloodFill(i,j,border,d);
  end;

  procedure GetArcCoords(c:out ArcCoordsType) is begin c:= dern_arc; end;

  procedure GetAspectRatio(x,y:out integer; d:device_type:=current_device) is
    begin
      x:= Aspect_ratio(d).x;
      y:= Aspect_ratio(d).y;
    end;

  function GetColor(d:device_type:=current_device) return gr_colour   is begin return c_colour(d); end;
  function GetBkColor(d:device_type:=current_device) return gr_colour is begin return c_bgd_colour(d); end;

  function GetDriverName(d:device_type:=current_device) return string is
    begin return device_type'image(d); end;

  procedure GetFillPattern(FillPattern: out FillPatternType) is
  begin
    FillPattern:= current_fill_pattern;
  end;

  procedure GetFillSettings(FillInfo: out FillSettingsType;
                            d: Device_type:= current_device) is
  begin
    FillInfo.style:= current_fill_style;
    FillInfo.color:= current_fill_colour(d);
  end;

  function GetGraphMode(d:device_type:=current_device) return positive is
    begin return dev_mode(d); end;

  function GetMaxColor(d:device_type:=current_device) return gr_colour is
  begin
    case d is






      when PostScript  => return 100 + 99_99_99;
      when others      => return 1;
    end case;
  end GetMaxColor;

  function GetMaxMode(d:device_type:=current_device) return positive is
    begin return max_mode(d); end;

  function GetMaxX(d:device_type:=current_device) return integer is
    begin return gxx(d,dev_mode(d)); end;

  function GetMaxY(d:device_type:=current_device) return integer is
    begin return gxy(d,dev_mode(d)); end;

  function GetModeName(mode:positive; d:device_type:=current_device) return string is
  begin
    if mode not in 1..max_mode(d) then raise Invalid_mode; end if;
    case d is


















      when PostScript=>
        case mode is
          when PS_10_pt => return "10-pitch";
          when PS_12_pt => return "12-pitch";
          when PS_half  => return "10-pitch, 1/2 size";
          when PS_landscape_10_pt => return "10-pitch, landscape";
          when PS_landscape_12_pt => return "12-pitch, landscape";
          when PS_landscape_half  => return "10-pitch, 1/2 size, landscape";
          when others   => null;
        end case;
      when others=> null;
    end case;
    return "";
  end GetModeName;

  procedure GetModeRange(d:device_type; ModeInf, ModeSup: out positive) is
    begin ModeInf:= 1; ModeSup:= max_mode(d); end;

  function GetPixel( x,y: Integer;
                     d: Device_type:= current_device) return Gr_Colour is
    ia: constant integer:= VP_a(d).x;
    ib: constant integer:= VP_b(d).x;
    ja: constant integer:= VP_a(d).y;
    jb: constant integer:= VP_b(d).y;
  begin
    if (not clipping(d)) or else (x in 0..ib-ia and then y in 0..jb-ja) then
      case d is















        when others=> raise Non_Readable_device;
      end case;
    else
      return c_bgd_colour(d); -- tolerant out-of-viewport
    end if;
  end GetPixel;

  function GetPixel( x,y: Float;
                     d: Device_type:= current_device) return Gr_Colour is
    i,j: integer;
  begin
    Coord(x,y, i,j, d);
    return GetPixel(i,j,d);
  end GetPixel;

  procedure GetTextSettings(TextInfo: out TextSettingsType) is
    begin TextInfo:= txtset; end;

  procedure GetViewSettings(ViewPort: out ViewPortType; d:device_type:=current_device) is
    begin
      ViewPort:= (VP_a(d).x,VP_a(d).y,VP_b(d).x,VP_b(d).y,clipping(d));
    end;

  function GetX(d:device_type:=current_device) return integer is
    begin return PC(d).x; end;

  function GetY(d:device_type:=current_device) return integer is
    begin return PC(d).y; end;

  procedure GraphDefaults(d:device_type:=current_device) is
    begin
      PC(d):= (0,0);
      PCnowhere(d):= true;
      case d is









        when others      => -- on-Paper devices (black on white)
          SetColor(Black,d); SetBkColor(White,d);
          current_fill_colour(d):= Black;
      end case;
    end;

  procedure InitGraph(d:device_type:=current_device; mode:natural:= key_for_current_mode) is
  begin
    if opened(d) then raise Device_not_closed; end if;
    opened(d):= true;

    if mode= key_for_current_mode then -- The real "init" for some devices
      SetGraphMode(dev_mode(d),d);
    else
      SetGraphMode(mode,d);
    end if;

    -- GraphDefaults(d); [moved after PS/AutoCAD file creation] 3-Feb-2001
    if needs_a_name(d) and then device_file_name(d)=null then
      raise no_file_name;
    end if;

    case d is

      when PostScript=>
        PS_Prolog;

      when AutoCAD_DXF=>
        Create(dev_file(AutoCAD_DXF), Name=> device_file_name(AutoCAD_DXF).all);
        PiAC(0); PlAC("SECTION");
        PiAC(999);
        PlAC("     AutoCAD-DXF output from Graph Ada package  v. 18-Mar-2001");
        PiAC(999);
        PlAC("     by Gautier de Montmollin - https://sf.net/u/gdemont");
        PiAC(999);
        PlAC("     Current time at start:" & Time_display );
        PiAC(2); PlAC("ENTITIES");













      when others=> null;
    end case;
    GraphDefaults(d);
    SetViewPort(0,0,GetMaxX(d),GetMaxY(d),d=> d);
  end InitGraph;

  procedure InitGraph( d:device_type:=current_device;
                       mode:natural;
                       max_x, max_y:positive ) is
    m: natural:= mode;
  begin
    if not sizeable(d) then raise Non_sizeable_device; end if;
    if mode= key_for_current_mode then m:= dev_mode(d); end if;
    if m not in 1..max_mode(d) then raise Invalid_mode; end if;
    gxx(d,m):= max_x;
    gxy(d,m):= max_y;
    InitGraph(d,m);
  end;

  procedure InitGraph( d:device_type:=current_device;
                       max_x, max_y:positive) is
  begin
    InitGraph(d,key_for_current_mode,max_x,max_y);
  end;

  procedure InitGraph( d:device_type:=current_device; mode:natural;
                       file_name:string) is
  begin
    device_file_name(d):= new string'(file_name);
    InitGraph(d,mode);
  end;

  procedure InitGraph( d:device_type:=current_device;
                       file_name:string) is
  begin
    Initgraph(d,key_for_current_mode,file_name);
  end;

  procedure InitGraph( d:device_type:=current_device;
                       mode:natural;
                       file_name:string;
                       max_x, max_y:positive ) is
  begin
    device_file_name(d):= new string'(file_name);
    InitGraph(d,mode,max_x,max_y);
  end;

  procedure InitGraph( d:device_type:=current_device;
                       file_name:string;
                       max_x, max_y:positive ) is
  begin
    InitGraph(d,key_for_current_mode,file_name,max_x,max_y);
  end;

  procedure Line(x1,y1,x2,y2:integer; d:device_type:=current_device) is
    ia: constant integer:= VP_a(d).x;
    ib: constant integer:= VP_b(d).x;
    iab: constant integer:= ib-ia;
    ja: constant integer:= VP_a(d).y;
    jb: constant integer:= VP_b(d).y;
    jab: constant integer:= jb-ja;
    x0,x3,y0,y3: integer;    -- 0/3: min/max
    uu,vv,diff_u,diff_v,reste,pente,fiab,fjab: float;
    u,v: array(1..2) of float;
--    dedans: boolean;

  begin
    if (not clipping(d)) or else
       (x1 in 0..iab and then x2 in 0..iab and then
        y1 in 0..jab and then y2 in 0..jab) then
      if y1/=y2 then     -- dans le cadre, verticale ou oblique
        Clipped_absolute_line(x1+ia,y1+ja, x2+ia,y2+ja, d);
      else               -- dans le cadre, horizontale
        Clipped_absolute_hor_line(x1+ia,x2+ia,y1+ja,d);
      end if;
    elsif x1=x2 then                        -- … couper, verticale
      y0:= Min(y1,y2); y3:= Max(y1,y2);
      if x1 in 0..iab and then y0<=jab and then y3>=0 then
        Clipped_absolute_line(x1+ia,Max(0,y0)+ja,x1+ia,Min(jab,y3)+ja,d);
      end if;
    elsif y1=y2 then                        -- … couper, horizontale
      x0:= Min(x1,x2); x3:= Max(x1,x2);
      if y1 in 0..jab and then x0<=iab and then x3>=0 then
        Clipped_absolute_hor_line(Max(0,x0)+ia,Min(iab,x3)+ia,y1+ja,d);
      end if;
    elsif not (                     -- 25.II.1998: cas triviaux
     (x1<0 and then x2<0) or else
     (y1<0 and then y2<0) or else
     (x1>iab and then x2>iab) or else
     (y1>jab and then y2>jab)
    ) then                                    -- … couper, oblique
      u(1):=float(x1);
      v(1):=float(y1);
      u(2):=float(x2);
      v(2):=float(y2);
      fiab:=float(iab); fjab:=float(jab);
      reste:= sqrt( (u(2)-u(1))**2+(v(2)-v(1))**2 );
      pente:= (v(2)-v(1))/(u(2)-u(1));
--        dedans:= true;
      for k in 1..2 loop  -- k= extr‚mit‚ 1 ou 2
        uu:= u(k);
        if    uu<0.0  then uu:= 0.0;
        elsif uu>fiab then uu:= fiab; end if;
        diff_u:= uu-u(k);
        diff_v:= diff_u*pente;
        reste:= reste - sqrt( diff_u**2 + diff_v**2 );
        vv:= v(k) + diff_v;
        u(k):= uu;
        v(k):= vv;
        if    vv<0.0  then vv:= 0.0;
        elsif vv>fjab then vv:= fjab; end if;
        diff_v:= vv-v(k);
        diff_u:= diff_v/pente;
        reste:= reste - sqrt( diff_u**2 + diff_v**2 );
        uu:= u(k) + diff_u;
        u(k):= uu;
        v(k):= vv;
--          dedans:= dedans and uu>=0.0 and uu<fiab+1.0 and vv>=0.0 and vv<fjab+1.0;
      end loop;
--        if reste >= 0.0 and dedans then
      if reste >= 0.0 then
--          if not dedans then raise constraint_error; end if;
        Clipped_absolute_line(
          integer(u(1))+ia,integer(v(1))+ja,
          integer(u(2))+ia,integer(v(2))+ja, d);
      end if;
    end if;
    MoveTo(x2,y2, d);
  end Line;

  procedure Line(x1,y1,x2,y2:float; d:device_type:=current_device) is
    m: constant p_math_scr:= math_scr(d)'Access;
    base_x: constant float:= m.x1;
    base_y: constant float:= m.y1;
    tpx:    constant float:= m.toPixX;
    tpy:    constant float:= m.toPixY;
    mxy:    constant float:= m.fMaxY;
    i1,i2,j1,j2: integer;
  begin
    -- Coord(x1,y1, i1,j1, d); Coord(x2,y2, i2,j2, d);
    i1:= integer( (x1-base_x)*tpx );
    i2:= integer( (x2-base_x)*tpx );
    if reversed(d) then
      j1:= integer( (base_y-y1)*tpy + mxy );
      j2:= integer( (base_y-y2)*tpy + mxy );
    else
      j1:= integer( (y1-base_y)*tpy );
      j2:= integer( (y2-base_y)*tpy );
    end if;
    Line(i1,j1, i2,j2, d);
  end Line;

  procedure LineRel(dx,dy:integer; d:device_type:=current_device) is
  begin
    LineTo(PC(d).x+dx,PC(d).y+dy, d);
  end LineRel;

  procedure LineRel(dx,dy:float;   d:device_type:=current_device) is
  begin
    LineTo(PC(d).x + integer(math_scr(d).toPixX*float(dx)),
           PC(d).y + integer(math_scr(d).toPixY*float(dy)), d);
  end LineRel;  -- 22.VII.1998

  procedure LineTo(x,y:integer; d:device_type:=current_device) is
  begin
    if PCnowhere(d) then MoveTo(x,y,d); else
      Line(PC(d).x,PC(d).y, x,y, d);
      PCnowhere(d):= false;
    end if;
  end LineTo;

  procedure LineTo(x,y:float;   d:device_type:=current_device) is
    i,j: integer;
  begin
    Coord(x,y, i,j, d);
    LineTo(i,j,d);
  end LineTo;

  procedure MoveRel(dx,dy:integer; d:device_type:=current_device) is
  begin MoveTo(PC(d).x + dx, PC(d).y + dy, d); end;

  procedure MoveRel(dx,dy:float; d:device_type:=current_device) is
  begin
    MoveTo(PC(d).x + integer(math_scr(d).toPixX*float(dx)),
           PC(d).y + integer(math_scr(d).toPixY*float(dy)), d);
  end;  -- 8.VI.1998

  procedure MoveTo(x,y:integer; d:device_type:=current_device) is
  begin PC(d):= (x,y); PCnowhere(d):= false; end;

  procedure MoveTo(x,y:float;   d:device_type:=current_device) is
    i,j: integer;
  begin
    Coord(x,y, i,j, d);
    MoveTo(i,j,d);
  end;

  function Name (font : Vector_font) return String is
    last: Natural:= font.name'last;
  begin
    while last > 0 and then font.name(last)=' ' loop
      last:= last-1;
    end loop;
    return font.name(1..last);
  end Name;

  procedure Nowhere(d:device_type:=current_device) is
    begin PCnowhere(d):= true; end;

  procedure OutText(s:string; d:device_type:=current_device) is
    begin OutTextXY( PC(d).x, PC(d).y, s, d ); end;

  procedure OutTextXY(x,y:integer; s:string; d:device_type:=current_device) is

    procedure Out_char_vecto(c:character; x,y: integer) is
      i0,im, xxr,yyr: Integer;
      xv,yv: float;
      f: p_vector_font:= txtset.Font;
      deplacement: constant:= 33;
    begin
      if c not in f.first_char..f.last_char then return; end if;
      i0:= natural(f.index(c));
      if c<f.last_char then
       im:= natural(f.index(character'succ(c)))-1;
      else
       im:= f.n_vertices;
      end if;
      for i in i0..im loop
        xv:= float(abs(f.vertex_x(i))-deplacement)*txt_rapX;
        yv:= float(f.vertex_y(i))*txt_rapY;
        if txtset.Slanted then
          xv:= xv + 0.3 * yv;
        end if;
        xxr:= x + integer(  txtcos*xv - txtsin*yv );
        yyr:= y - integer(( txtsin*xv + txtcos*yv)*aspect_r_fl(d));
        if f.vertex_x(i)<0 then
          LineTo(xxr,yyr,d);
        else
          MoveTo(xxr,yyr,d);
        end if;
      end loop;
    end Out_char_vecto;

    -- 26-Apr-2002: + Bold, slanted, underlined
    procedure Out_char_vecto_style( c: Character; x,y: Integer) is
      xxr1,yyr1,xxr2,yyr2: Integer;
      xv1: constant Float:= 0.0;
      xv2, yv : Float;
    begin
      Out_char_vecto(c,x,y);
      if txtset.Bold then
        Out_char_vecto(c,x-1,y  );
        Out_char_vecto(c,x+1,y  );
        Out_char_vecto(c,x  ,y-1);
        Out_char_vecto(c,x,  y+1);
      end if;
      if txtset.Underlined then
        xv2:= Float( txtset.Font.Width(c) ) * txt_rapX;
        yv := Float( txtset.Font.Downline ) * txt_rapY;
        xxr1:=   Integer(  txtcos*xv1 - txtsin*yv );
        yyr1:= - Integer(( txtsin*xv1 + txtcos*yv)*aspect_r_fl(d));
        xxr2:=   Integer(  txtcos*xv2 - txtsin*yv );
        yyr2:= - Integer(( txtsin*xv2 + txtcos*yv)*aspect_r_fl(d));
        Line( x+xxr1,y+yyr1, x+xxr2, y+yyr2, d);
      end if;
    end Out_char_vecto_style;

    procedure Out_bitmap is
      xx,yy: Integer;
    begin
      case txtset.Horiz is -- 7.VI.1998
       when LeftText =>   xx:= x;
       when CenterText => xx:= x - TextWidth(s,d)/2;
       when RightText =>  xx:= x - TextWidth(s,d);
      end case;
      case txtset.Vert is
       when TopText =>    yy:= y;
       when CenterText => yy:= y - TextHeight(s,d) / 2;
       when BottomText => yy:= y - TextHeight(s,d);
      end case;
      if not (xx in VP_a(d).x .. VP_b(d).x-TextWidth(s,d) and then
              yy in VP_a(d).y .. VP_b(d).y-TextHeight(s,d)) then
        return; -- forgetful out-of-viewport
      end if;
      case d is















        when PostScript=>
          PS_Write(xx+VP_a(d).x, yy+VP_a(d).y, s);
        when AutoCAD_DXF=> null;
      end case;
    end Out_bitmap;

     l,h: float; PCmem: PointType; NWmem: boolean;
    begin
     if txt_bitmap then
       Out_bitmap;
     else -- vectorial font:
       case txtset.Horiz is
        when LeftText =>   l:= 0.0;
        when CenterText => l:= - 0.5 * float(TextWidth(s,d));
        when RightText =>  l:= - float(TextWidth(s,d));
       end case;
       case txtset.Vert is
        when TopText =>    h:= - float(TextHeight(s,d));
        when CenterText => h:= - 0.5 * float(TextHeight(s,d));
        when BottomText => h:= 0.0;
       end case;
       PCmem:= PC(d); -- 7.VI.1998
       NWmem:= PCNowhere(d);
       for i in s'range loop
         Out_char_vecto_style(s(i),
          x + integer( txtcos*l - txtsin*h ),
          y - integer((txtsin*l + txtcos*h)*aspect_r_fl(d)));
         l:= l + float(TextWidth(s(i),d));
       end loop;
       PC(d):= PCmem;
       PCnowhere(d):= NWmem;
     end if;




    end;

  procedure OutTextXY(x,y:float;   s:string; d:device_type:=current_device) is
    i,j: integer;
  begin Coord(x,y, i,j, d); OutTextXY(i,j,s,d); end;

  procedure OutText(c:character; d:device_type:=current_device) is
  begin OutText(c&"",d); end;

  procedure OutTextXY(x,y:integer; c:character; d:device_type:=current_device) is
  begin OutTextXY(x,y,c&"",d); end;

  procedure OutTextXY(x,y:float;   c:character; d:device_type:=current_device) is
  begin OutTextXY(x,y,c&"",d); end;

  procedure Point(x,y:integer; d:device_type:=current_device) is
    ia: constant integer:= VP_a(d).x;
    ib: constant integer:= VP_b(d).x;
    ja: constant integer:= VP_a(d).y;
    jb: constant integer:= VP_b(d).y;
  begin
    if (not clipping(d)) or else (x in 0..ib-ia and then y in 0..jb-ja) then
      case d is











        when PostScript=> PS_Dot(x+ia,y+ja);
        when others=> Line(x,y,x,y,d);
      end case;
    end if;
    MoveTo(x,y,d);
  end Point;

  procedure Point(x,y:float;   d:device_type:=current_device) is
    i,j: integer;
  begin
    Coord(x,y, i,j, d);
    Point(i,j,d);
  end Point;

  procedure PutPixel(x,y:integer; colour:gr_colour; d:device_type:=current_device) is
    c: integer:= c_colour(d);
  begin
    SetColor(colour,d);
    Point(x,y,d);
    SetColor(c,d);
  end PutPixel;

  procedure Rectangle(x1,y1,x2,y2:integer; d:device_type:=current_device) is
  begin
    Line(x1,y1,x2,y1,d); Line(x2,y1,x2,y2,d);
    Line(x2,y2,x1,y2,d); Line(x1,y2,x1,y1,d);
  end Rectangle;

  procedure Rectangle(x1,y1,x2,y2:float;   d:device_type:=current_device) is
  begin
    Line(x1,y1,x2,y1,d); Line(x2,y1,x2,y2,d);
    Line(x2,y2,x1,y2,d); Line(x1,y2,x1,y1,d);
  end Rectangle;

  procedure RestoreCrtMode(d:device_type:=current_device) is
  begin
    if not opened(d) then raise Device_not_opened; end if;
    case d is




      when others=> null; -- sense only in DOS
    end case;
  end RestoreCrtMode;

  procedure Sector(x,y,a1,a2,rx,ry:integer; d:device_type:=current_device) is
  begin
    Ellipse(x,y,a1,a2,rx,ry,d);
    Line(x,y,dern_arc.xstart,dern_arc.ystart,d);
    Line(x,y,dern_arc.xend,dern_arc.yend,d);
    -- (filling)
  end Sector;

  procedure Sector(x,y,a1,a2,rx,ry:float;   d:device_type:=current_device) is
    i,j: integer;
  begin
    Coord(x,y, i,j, d);
    Sector(i,j,integer(a1),integer(a2),integer(math_scr(d).toPixX*rx),integer(math_scr(d).toPixY*ry),d);
  end Sector;

  procedure SetActivePage(page_num: page_range; d:device_type:=current_device) is
  begin
    if multi_page_available(d) then
      Prepare_multi_page(d); -- prepare virtual screens if necessary
      active_page(d):= page_num;
      if active_page(d)=visual_page(d) then
        case d is





          when others=> null;
        end case;
      else
        case d is





          when others=> null;
        end case;
      end if;
    end if;
  end SetActivePage;

  procedure SetAspectRatio(x,y: integer; d:device_type:=current_device) is
  begin
    if x*y=0 then
      raise Zero_aspect_ratio;
    else
      Aspect_ratio(d):= (x,y);
      aspect_r_fl(d):= float(x)/float(y);
    end if;
  end SetAspectRatio;

  procedure SetColor(colour:gr_colour; d:device_type:=current_device) is
  begin
    c_colour(d):= colour;
    case d is






     when PostScript=> Set_current_PS_Colour(colour);
     when others=> null;
    end case;
  end SetColor;

  procedure SetColor( R,G,B: Float_0_1; d: Device_type:= current_device ) is
  begin
    case d is
     when PostScript=>
       c_colour(PostScript):= Code_RGB(R,G,B);
       Set_current_PS_Colour(c_colour(PostScript));
     when others=> raise Non_RGB_device;
    end case;
  end SetColor;

  procedure SetBkColor(colour:gr_colour; d:device_type:=current_device) is
  begin
    c_bgd_colour(d):= colour;
    case d is






     when others=> null;
    end case;
  end SetBkColor;

  procedure SetBkColor( R,G,B: Float_0_1; d: Device_type:= current_device ) is
  begin
    case d is
     when PostScript=>
       c_bgd_colour(PostScript):= Code_RGB(R,G,B);
       Set_current_PS_Colour(c_bgd_colour(PostScript));
     when others=> raise Non_RGB_device;
    end case;
  end SetBkColor;

  -- 26-Apr-2002:

  procedure SetFillPattern(Pattern: FillPatternType; Colour: Gr_Colour;
                           d: Device_type:= current_device) is
  begin
    current_fill_style     := UserFill;
    current_fill_pattern   := Pattern;
    current_fill_colour(d) := Colour;
  end SetFillPattern;

  procedure SetFillStyle(Pattern: FillStyle; Colour: Gr_Colour;
                         d: Device_type:= current_device) is
  begin
    current_fill_style     := Pattern;
    case Pattern is
      when UserFill => current_fill_pattern:= current_fill_pattern_user;
      when others   => current_fill_pattern:= standard_pattern( Pattern );
    end case;
    current_fill_colour(d) :=  Colour;
  end SetFillStyle;

  procedure SetGraphMode(mode:positive:= default_mode; d:device_type:=current_device) is
  begin
    if not opened(d) then raise Device_not_opened; end if;
    if mode > max_mode(d) then raise Invalid_mode; end if;
    dev_mode(d):= mode;
    case d is












      when others=> null;
    end case;
  end SetGraphMode;

  procedure Set_math_plane(x1,y1,x2,y2:float; d:device_type:=current_device) is
    fx: constant float:= float(VP_b(d).x-VP_a(d).x);
    fy: constant float:= float(VP_b(d).y-VP_a(d).y);
  begin
    if not opened(d) then raise Device_not_opened; end if;
    if x2=x1 or else y2=y1 then raise Invalid_math_plane; end if;
--      if fx=0.0 or else fy=0.0 then raise Invalid_view_port; end if;
    math_scr(d):=
      (x1,y1,x2,y2,x2-x1,y2-y1,
       fx,fy,fx/(x2-x1),fy/(y2-y1),(x2-x1)/fx,(y2-y1)/fy);
  end Set_math_plane;

  procedure SetRGBPalette(col, r,g,b: Integer; d:device_type:=current_device) is
  begin
    case d is




      when others=> null;
    end case;
  end SetRGBPalette;

  procedure SetTextJustify(Horiz: t_hori_justify; Vert: t_vert_justify) is
  begin
    txtset.Horiz:= Horiz;
    txtset.Vert:=  Vert;
  end SetTextJustify;

  procedure SetTextSettings(TextInfo: TextSettingsType) is
  begin
    txtset:= TextInfo;
    update_txt_internals;
  end SetTextSettings;

  procedure SetTextStyle(
              Font      : p_Vector_font;
              Direction : T_Direction;
              Size      : Natural;
              Bold      : Boolean:= False;
              Slanted   : Boolean:= False;
              Underlined: Boolean:= False) is
  begin
    txtset.CharSize:= Size;
    SetTextStyle(Font,Direction,Bold,Slanted,Underlined);
  end SetTextStyle;

  procedure SetTextStyle(
              Font      : p_Vector_font;
              Direction : T_Direction;
              Bold      : Boolean:= False;
              Slanted   : Boolean:= False;
              Underlined: Boolean:= False) is
  begin
    txtset.Font       := Font;
    txtset.Direction  := Direction;
    txtset.Bold       := Bold;
    txtset.Slanted    := Slanted;
    txtset.Underlined := Underlined;
    update_txt_internals;
  end SetTextStyle;

  procedure SetUserCharSize(MultX, DivX, MultY, DivY: positive) is
  begin
    txtset.CharSize:= 0;
    txt_rapX:= float(MultX)/float(DivX);
    txt_rapY:= float(MultY)/float(DivY);
  end SetUserCharSize;

  procedure SetUserCharDirection(angle:float) is
  begin txtset.Direction:= AnyDir; txtangle:= angle;
        update_txt_internals;
  end SetUserCharDirection;

  procedure SetViewPort(x1,y1,x2,y2:natural; clip:boolean:= true; d:device_type:=current_device) is
  begin
    if not opened(d) then raise Device_not_opened; end if;
    if x2<=x1 or else y2<=y1 or else
       x2>GetMaxX(d) or else y2>GetMaxY(d) then
         raise Invalid_view_port;
    end if;
    VP_a(d):= (x1,y1);
    VP_b(d):= (x2,y2);
    clipping(d):= clip;
    Set_math_plane(float(x1),float(y2),float(x2),float(y1),d);
  end SetViewPort;

  procedure SetVisualPage(page_num: page_range; save_old: boolean:= true; d:device_type:=current_device) is
  begin
    if multi_page_available(d) then
      Prepare_multi_page(d); -- prepare virtual screens if necessary
      if save_old then
        case d is              -- old visual page becomes virtual





          when others=> null;
        end case;
      end if;
      case d is              -- new visual page becomes real





        when others=> null;
      end case;
      visual_page(d):= page_num;       -- refresh setting
      SetActivePage(active_page(d),d); -- refresh screen mapping
    end if;
  end SetVisualPage;

  function TextHeight(c:character; d:device_type:=current_device) return natural is
    f: constant p_vector_font:= txtset.Font;
  begin
   if txt_bitmap then
    case d is





      when PostScript => return 48;
      when others => return 14;
    end case;
   elsif c in f.first_char..f.last_char then
    return integer(float(f.Height)*txt_rapY);
   else
    return 0;
   end if;
  end TextHeight;

  function TextWidth(c:character; d:device_type:=current_device) return natural is
    f: constant p_vector_font:= txtset.Font;
  begin
   if txt_bitmap then
    case d is





      when PostScript => return 24;
      when others => return 8;
    end case;
   elsif c in f.first_char..f.last_char then
    return integer(float(f.Width(c))*txt_rapX);
   else
    return 0;
   end if;
  end TextWidth;

  function TextHeight(TextString: string; d:device_type:=current_device) return natural is
  begin
    if TextString'length=0 then
      return 0;
    else
      return TextHeight(TextString(TextString'First),d);
    end if;
  end TextHeight;

  function TextWidth(TextString: string; d:device_type:=current_device) return natural is
    w: natural:= 0;
  begin
    for i in TextString'range loop
      w:= w + TextWidth(TextString(i),d);
    end loop;
    return w;
  end TextWidth;

  procedure Size_of_pixel(x,y:out float; d:device_type:=current_device) is
  begin x:= math_scr(d).toMathX; y:= math_scr(d).toMathY; end;

  procedure Size_of_math_unit(x,y:out float; d:device_type:=current_device) is
  begin x:= math_scr(d).toPixX; y:= math_scr(d).toPixY; end;

  procedure DrawVector(x,y: float; u,v:float; d:device_type:=current_device) is
    a,b,p,un,vn,luv,iv_luv,absc,ordo:float;
  begin
    a:=x+u; b:=y+v;
    Line(x,y,a,b,d);
    luv:= sqrt(u*u+v*v);
    p:= 0.005 * (abs(math_scr(d).l)+abs(math_scr(d).h));
    -- p= moyenne(largeur, hauteur) * 1%
    if luv>p then   -- arrow main part > arrow side (fixed) => sides drawn
      iv_luv:= 1.0/luv;
      un:= iv_luv*u;
      vn:= iv_luv*v;
      absc:= cos_angle_vect*p;
      ordo:= sin_angle_vect*p;
      LineTo( a + un*absc + vn*ordo,
              b - un*ordo + vn*absc, d);
      Line(   a + un*absc - vn*ordo,
              b + un*ordo + vn*absc, a,b, d);
    end if;
  end DrawVector;

  procedure DrawVector(x,y: float; v:vector2; d:device_type:=current_device) is
  begin DrawVector(x,y,v.x,v.y,d); end;

  procedure DrawVector(p:math_PointType; v:vector2; d:device_type:=current_device) is
  begin DrawVector(p.x,p.y,v.x,v.y,d); end;

begin
  update_txt_internals;
end Graph;
