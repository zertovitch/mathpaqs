------------------------------------------------------------------------------
--  File:            GrCubcur.ads      (possibly extracted from PC_GRAPH.ZIP)
--  Description:     Extension of Graph package, for cubic & B‚zier curves
--  Date/version:    25.IV.1997 / 23.I.1998
--  Author:          G. de Montmollin
------------------------------------------------------------------------------

package Graph.Cubic_curves is

-- * Coordinates of cubic curves in math plane
  type Cubic_curve_type(n_seg: positive) is record
    end_pt: math_polygon_type(0..n_seg);  -- end points
    c1,c2:  math_polygon_type(1..n_seg);  -- control points, can be the same
  end record;

------------ Operations on cubic curves objects
  function "+" (cb:Cubic_curve_type; pt:math_PointType) return Cubic_curve_type;
  function Dilatation(cb:Cubic_curve_type; fx,fy:float) return Cubic_curve_type;
  function "*" (cb:Cubic_curve_type; f:float) return Cubic_curve_type;
  function "*" (f:float; cb:Cubic_curve_type) return Cubic_curve_type;
  
-- + Draw cubic curves
  procedure Cubic_curve(c:Cubic_curve_type; show_lines: boolean:= false; d:device_type:=current_device);

-- + Make a cubic curve with correct smoothness conditions at vertices
--   = B‚zier curve (23.I.1998)

--     Input: speed and accelration condition for the 1st point and
--            a cubic curve with control points to be defined
--    Output: a cubic curve with control points defined

  procedure Solve_Bezier(c: in out Cubic_curve_type; speed,accel: vector2);

end Graph.Cubic_curves;
