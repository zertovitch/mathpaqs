---------------------------------------------------------------------------
--  Champ de vecteurs / Vector Field Plot
---------------------------------------------------------------------------

with Graph;                             use Graph;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Champ_vt is

  function F(x,y: Float) return Vector2 is
    r2: constant Float:= x*x+y*y;
    r: constant Float:= Sqrt(r2);
    a: constant:= 0.05;
  begin
    return (a*((r-1.0)*(r-1.0)/r)*x-y,a*((r-1.0)*(r-1.0)/r)*y+x);
    -- return (x,0);
  end F;

  procedure Dessine_Champ(xa,ya,xb,yb: Float; points_x, points_y: Positive) is
    xab: constant Float:= xb-xa;
    yab: constant Float:= yb-ya;
    ipx: constant Float:= 1.0 / Float(points_x+1);
    ipy: constant Float:= 1.0 / Float(points_y+1);
    carr: constant Float:= Float'Max(xab*ipx,yab*ipy);
    x,y,cal: Float;

  begin
    Set_math_plane(xa,ya, xb,yb);
    Draw_axes(units=>true, grid_points=>true, scale=>true);
    cal:= 0.0;
    for calibrate in reverse False..True loop
      for i in 1..points_x loop
        for j in 1..points_y loop
          x:= xa + Float(i)*ipx*xab;
          y:= ya + Float(j)*ipy*yab;
          if calibrate then
            cal:= Float'Max(cal,Norm2(F(x,y)));
          else
            if cal>0.0 then DrawVector(x,y, F(x,y) * (carr/cal) ); end if;
          end if;
        end loop;
      end loop;
    end loop;
  end;

  procedure Dessin(d:device_type) is
  begin
    current_device:= d;
    InitGraph(d, "Champ_Vt.ps");
    Circle(0.0,0.0,1.0);
    Dessine_Champ(-3.0,-3.0,3.0,3.0, 30,20);
    CloseGraph;
  end;

begin
  Dessin(PostScript);
end;

