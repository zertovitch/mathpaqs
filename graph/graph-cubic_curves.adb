-----------------------------------------------------------------------------
--  File: grcubcur.adb; see specification (grcubcur.ads)
-----------------------------------------------------------------------------

package body Graph.Cubic_curves is

  function "+" (cb:Cubic_curve_type; pt:math_PointType) return Cubic_curve_type is
  begin
    return (cb.n_seg, cb.end_pt+pt, cb.c1+pt, cb.c2+pt);
  end;

  function Dilatation(cb:Cubic_curve_type; fx,fy:float) return Cubic_curve_type is
  begin
    return (cb.n_seg, dilatation(cb.end_pt,fx,fy),
            dilatation(cb.c1,fx,fy), dilatation(cb.c2,fx,fy));
  end;

  function "*" (cb:Cubic_curve_type; f:float) return Cubic_curve_type is
  begin return Dilatation(cb,f,f); end;

  function "*" (f:float; cb:Cubic_curve_type) return Cubic_curve_type is
  begin return Dilatation(cb,f,f); end;

  procedure Cubic_curve(c:Cubic_curve_type; show_lines: boolean:= false; d:device_type:=current_device) is
    -- Choosen basis for a cubic curve: { (1-t)^3, t(1-t)^2, t^2(1-t), t^3 }
    -- x(t) = (1-t)^3 x0 + 3t (1-t)^2 x1 + 3t^2 (1-t) x2 + t^3 x3
    -- y(t) = (1-t)^3 y0 + 3t (1-t)^2 y1 + 3t^2 (1-t) y2 + t^3 y3
    -- Then (x0,y0) and (x3,y3) are end points and (x1,y1), (x2,y2) are control points:
    -- [(x0,y0),(x1,y1)] is // to the curve in (x0,y0) and
    -- [(x2,y2),(x3,y3)] is // to the curve in (x3,y3).

    n: constant:= 100;
    ivn: constant float:= 1.0/float(n);
    x0,y0,x1,y1,x2,y2,x3,y3,ax,bx,cx,ay,by,cy,t: float;
  begin
    x3:= c.end_pt(0).x;
    y3:= c.end_pt(0).y;
    MoveTo(x3,y3,d);
    for i in 1..c.n_seg loop
      x0:= x3;
      x1:= c.c1(i).x;
      x2:= c.c2(i).x;
      x3:= c.end_pt(i).x;
      cx:= 3.0*(x1-x0);
      bx:= 3.0*(x2-x1)-cx;
      ax:= x3-x0-cx-bx;
      y0:= y3;
      y1:= c.c1(i).y;
      y2:= c.c2(i).y;
      y3:= c.end_pt(i).y;
      cy:= 3.0*(y1-y0);
      by:= 3.0*(y2-y1)-cy;
      ay:= y3-y0-cy-by;
      if show_lines then
        LineTo(x1,y1,d);
        MoveTo(x2,y2,d);
        LineTo(x3,y3,d);
        MoveTo(x0,y0,d);
      end if;
      for j in 1..n loop
        t:= float(j)*ivn;
        LineTo( x0+t*(cx+t*(bx+t*ax)), y0+t*(cy+t*(by+t*ay)), d);
      end loop;
    end loop;
  end Cubic_curve;

  procedure Solve_Bezier(c: in out Cubic_curve_type; speed,accel: vector2) is
  begin
    c.c1(1).x:= 1.0/3.0*speed.x + c.end_pt(0).x;
    c.c2(1).x:= 2.0*c.c1(1).x - c.end_pt(0).x;
    c.c1(1).y:= 1.0/3.0*speed.y + c.end_pt(0).y;
    c.c2(1).y:= 2.0*c.c1(1).y - c.end_pt(0).y;
    for i in 2..c.n_seg loop
      c.c1(i).x:= c.end_pt(i-1).x - c.c2(i-1).x;
      c.c2(i).x:= c.c1(i-1).x - 2.0*c.c2(i-1).x + 2.0*c.c1(i).x;
      c.c1(i).y:= c.end_pt(i-1).y - c.c2(i-1).y;
      c.c2(i).y:= c.c1(i-1).y - 2.0*c.c2(i-1).y + 2.0*c.c1(i).y;
    end loop;
  end Solve_Bezier;

end Graph.Cubic_curves;
