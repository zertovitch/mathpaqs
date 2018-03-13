-- Display Sierpinski and Barnsley fractals with various levels
--
-- Output: a PostScript (.ps) file.
--
-- Authors: Stéphane Perret and Gautier de Montmollin
--
-- March 2009
-- This procedure was made in around one 1/2 hour...

with Graph; use Graph;

with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

procedure Fractal is

  subtype Real is Float;

  type Pt is record
    x,y:  Real;
  end record;

  subtype Vector is Pt;

  type Figure is array(Positive range <>) of Pt;

  type Matrix22 is array(1..2,1..2) of Real;

  function "+" (p: Pt; v: Vector) return Pt is
  begin
    return (p.x+v.x, p.y+v.y);
  end "+";

  function "*" (M: Matrix22; p: Pt) return Pt is
  begin
    return
      (M(1,1) * p.x + M(1,2) * p.y,
       M(2,1) * p.x + M(2,2) * p.y);
  end "*";

  function "*" (f: Real; p: Pt) return Pt is
  begin
    return (f * p.x, f*p.y);
  end "*";

  function "*" (f: Real; fig: Figure) return Figure is
    res: Figure:= fig;
  begin
    for i in res'Range loop
      res(i):= f * res(i);
    end loop;
    return res;
  end "*";

  type Affine is record
    M: Matrix22;
    v: Vector;
  end record;

  type Affine_array is array(Positive range <>) of Affine;

  function Morph (f: Figure; a: Affine) return Figure is
    mod_f: Figure(f'Range);
  begin
    for i in f'Range loop
      mod_f(i):= a.M*f(i) + a.v;
    end loop;
    return mod_f;
  end Morph;

  procedure Draw(f: Figure; a: Affine_array; level: Natural) is
  begin
    if level = 0 then
      Point( f(f'Last).x, f(f'Last).y );
      for i in f'Range loop
        LineTo( f(i).x, f(i).y );
      end loop;
    else
      for i in a'Range loop
        Draw( Morph(f,a(i)), a, level-1 );
      end loop;
    end if;

  end Draw;

  procedure Plot(f: Figure; a: Affine_array; d:device_type; n:String) is
  begin
    InitGraph(d, file_name=>n);
    Set_math_plane(0.0,0.0, 1.0,1.0, d);
    for level in 1..10 loop
      Draw( f, a, level);
      ClearDevice;
    end loop;
    CloseGraph(d);
  end Plot;

  -- Napperons

  procedure Plot_Sierpinski (d: device_type; n: String) is
    triangle: constant Figure:= ((0.0,0.0), (0.5, Sqrt(3.0)/2.0), (1.0,0.0));
    M: constant Matrix22:= ((0.5,0.0),(0.0,0.5));
    v1: constant Vector:= (0.0,0.0);
    v2: constant Vector:= (0.5,0.0);
    v3: constant Vector:= (0.25,Sqrt(3.0)/4.0);
    transformation: constant Affine_array:=
      ( (M,v1), (M,v2), (M,v3) );
  begin
    Plot( triangle, transformation, d, n);
  end Plot_Sierpinski;

  -- Fougère

  procedure Plot_Barnsley (d: device_type; n: String) is
    triangle: constant Figure:= 0.1 * ((0.0,0.0), (0.5, Sqrt(3.0)/2.0), (1.0,0.0));
    M1: constant Matrix22:= ((0.849, 0.037),
                            (-0.037, 0.849));
    M2: constant Matrix22:= ((0.197, -0.226),
                             (0.226, 0.197));
    M3: constant Matrix22:= ((-0.150, 0.283),
                             (0.260, 0.237));
    M4: constant Matrix22:= ((0.0, 0.0),
                             (0.0, 0.16));
    v1: constant Vector:= (0.075, 0.1830);
    v2: constant Vector:= (0.4, 0.049);
    v3: constant Vector:= (0.575, -0.0840);
    v4: constant Vector:= (0.5, 0.0);
    transformation: constant Affine_array:=
      ( (M1,v1), (M2,v2), (M3,v3), (M4,v4) );
  begin
    Plot( triangle, transformation, d, n);
  end Plot_Barnsley;

begin
  Plot_Sierpinski(PostScript, "sierpinski.ps");
  Plot_Barnsley(PostScript, "barnsley.ps");
end Fractal;
