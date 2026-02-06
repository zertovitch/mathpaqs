-------------------------------------------------------------------------------
--  File: contour.adb; see specification (contour.ads)
-------------------------------------------------------------------------------

package body Contours is

  procedure Con_Rec (d    : Contour_Data;
                     x, y : Contour_Pos;
                     z    : Contour_Level)
  is
    h, xh, yh  : array (0 .. 4) of Real;
    sh : array (0 .. 4) of Integer;  --  Sign of relative height.
    im : constant array (1 .. 4) of Integer := (0, 1, 1, 0);
    jm : constant array (1 .. 4) of Integer := (0, 0, 1, 1);

    type Case_Value is range 0 .. 9;
    subtype With_Line is Case_Value range 1 .. 9;
    No_Line : constant := 0;
    --  Possible cases
    castab : constant array (-1 .. 1, -1 .. 1, -1 .. 1) of Case_Value :=
      (((0, 0, 8), (0, 2, 5), (7, 6, 9)),
       ((0, 3, 4), (1, 3, 1), (4, 3, 0)),
       ((9, 6, 7), (5, 2, 0), (8, 0, 0)));

    dmin, dmax : Real;
    m1, m2, m3 : Integer;
    x1, y1, x2, y2 : Real;
    cas : Case_Value;

    function xsect (p1, p2 : Natural) return Real is
    begin
      return (h (p2) * xh (p1) - h (p1) * xh (p2)) / (h (p2) - h (p1));
    end xsect;

    function ysect (p1, p2 : Natural) return Real is
    begin
      return (h (p2) * yh (p1) - h (p1) * yh (p2)) / (h (p2) - h (p1));
    end ysect;

  begin
    --
    --     Scan the arrays, top down, left to right within rows
    --
    Top_Down :
    for j in reverse d'First (2) .. d'Last (2) - 1 loop
      Left_Right :
      for i in d'First (1) .. d'Last (1) - 1 loop

        dmin :=
          Real'Min
            (Real'Min (d (i, j), d (i, j + 1)),
             Real'Min (d (i + 1, j), d (i + 1, j + 1)));
        dmax :=
          Real'Max
            (Real'Max (d (i, j), d (i, j + 1)),
             Real'Max (d (i + 1, j), d (i + 1, j + 1)));

        if  dmax >= z (z'First) and dmin <= z (z'Last)  then
          Level_Scan :
          for k in z'Range loop
            if  z (k) in dmin .. dmax  then
              --  Make a local copy of the map within vertices 1,2,3,4; 0 is the centre.
              for m in reverse 0 .. 4 loop
                if  m > 0  then
                  h (m) := d (i + im (m), j + jm (m)) - z (k);
                  xh (m) := x (i + im (m));
                  yh (m) := y (j + jm (m));
                else
                  h (0) := 0.25 * (h (1) + h (2) + h (3) + h (4));
                  xh (0) := 0.5 * (x (i) + x (i + 1));
                  yh (0) := 0.5 * (y (j) + y (j + 1));
                end if;
                --  Record the sign of relative height per vertex.
                if  h (m) > 0.0  then
                  sh (m) := +1;
                elsif  h (m) < 0.0  then
                  sh (m) := -1;
                else
                  sh (m) :=  0;
                end if;
              end loop;
              --
              --     Note: at this stage the relative heights of the corners and the
              --     centre are in the h array, and the corresponding coordinates are
              --     in the xh and yh arrays. The centre of the box is indexed by 0
              --     and the 4 corners by 1 to 4 as shown below.
              --     Each triangle is then indexed by the parameter m, and the 3
              --     vertices of each triangle are indexed by parameters m1,m2,and m3.
              --     It is assumed that the centre of the box is always vertex 2 though
              --     this isimportant only when all 3 vertices lie exactly on the same
              --     contour level, in which case only the side of the box is drawn.
              --
              --
              --           vertex 4 +-------------------+ vertex 3
              --                    | \               / |
              --                    |   \    m=3    /   |
              --                    |     \       /     |
              --                    |       \   /       |
              --                    |  m=2    X   m=2   |       the centre is vertex 0
              --                    |       /   \       |
              --                    |     /       \     |
              --                    |   /    m=1    \   |
              --                    | /               \ |
              --           vertex 1 +-------------------+ vertex 2
              --
              --
              --
              --                    Scan each triangle in the box
              --
              Triangle_Scan :
              for m in 1 .. 4 loop
                m1 := m;            --  m1 is always the vertex m = 1,2,3,4.
                m2 := 0;            --  m2 is always the centre, 0.
                m3 := 1 + m mod 4;  --  m3 is the next vertex after m.
                --  Get the case corresponding to the sign (-1, 0, or 1)
                --  attached to each vertex of the triangle.
                cas := castab (sh (m1), sh (m2), sh (m3));
                case cas is
                  when No_Line =>
                    null;
                  when With_Line =>
                    case With_Line (cas) is
                      --
                      --     Case 1 - Line between vertices 1 and 2
                      --
                      when 1 =>
                        x1 := xh (m1);
                        y1 := yh (m1);
                        x2 := xh (m2);
                        y2 := yh (m2);
                      --
                      --     Case 2 - Line between vertices 2 and 3
                      --
                      when 2 =>
                        x1 := xh (m2);
                        y1 := yh (m2);
                        x2 := xh (m3);
                        y2 := yh (m3);
                      --
                      --     Case 3 - Line between vertices 3 and 1
                      --
                      when 3 =>
                        x1 := xh (m3);
                        y1 := yh (m3);
                        x2 := xh (m1);
                        y2 := yh (m1);
                      --
                      --     Case 4 - Line between vertex 1 and side 2-3
                      --
                      when 4 =>
                        x1 := xh (m1);
                        y1 := yh (m1);
                        x2 := xsect (m2, m3);
                        y2 := ysect (m2, m3);
                      --
                      --     Case 5 - Line between vertex 2 and side 3-1
                      --
                      when 5 =>
                        x1 := xh (m2);
                        y1 := yh (m2);
                        x2 := xsect (m3, m1);
                        y2 := ysect (m3, m1);
                      --
                      --     Case 6 - Line between vertex 3 and side 1-2
                      --
                      when 6 =>
                        x1 := xh (m3);
                        y1 := yh (m3);
                        x2 := xsect (m1, m2);
                        y2 := ysect (m1, m2);
                      --
                      --     Case 7 - Line between sides 1-2 and 2-3
                      --
                      when 7 =>
                        x1 := xsect (m1, m2);
                        y1 := ysect (m1, m2);
                        x2 := xsect (m2, m3);
                        y2 := ysect (m2, m3);
                      --
                      --     Case 8 - Line between sides 2-3 and 3-1
                      --
                      when 8 =>
                        x1 := xsect (m2, m3);
                        y1 := ysect (m2, m3);
                        x2 := xsect (m3, m1);
                        y2 := ysect (m3, m1);
                      --
                      --     Case 9 - Line between sides 3-1 and 1-2
                      --
                      when 9 =>
                        x1 := xsect (m3, m1);
                        y1 := ysect (m3, m1);
                        x2 := xsect (m1, m2);
                        y2 := ysect (m1, m2);
                    end case;
                    Vec_Out (x1, y1, x2, y2, z (k));
                end case;
              end loop Triangle_Scan;
            end if;
          end loop Level_Scan;
        end if;
      end loop Left_Right;
    end loop Top_Down;
  end Con_Rec;

end Contours;
