-------------------------------------------------------------------------------
--  File: contour.adb; see specification (contour.ads)
-------------------------------------------------------------------------------

package body Contours is

   procedure ConRec (d   : contour_data;
                     x,y : contour_pos;
                     z   : contour_level) is

--
--     Local declarations
--

      h, xh, yh  : array(0..4) of Real;
      sh : array(0..4) of Integer;
      im : constant array(1..4) of Integer := (0,1,1,0);
      jm : constant array(1..4) of Integer := (0,0,1,1);

      castab : constant array(-1..1,-1..1,-1..1) of Integer:=

        (( ( 0, 0, 8), (0, 2, 5), (7, 6, 9)),
         ( ( 0, 3, 4), (1, 3, 1), (4, 3, 0)),
         ( ( 9, 6, 7), (5, 2, 0), (8, 0, 0)));

      dmin, dmax : Real;
      m1, m2, m3, cas : Integer;
      x1, y1, x2, y2 : Real;

      function xsect(p1,p2: Natural) return Real is
        begin return (h(p2) * xh(p1) - h(p1) * xh(p2)) / (h(p2) - h(p1)); end xsect;

      function ysect(p1,p2: Natural) return Real is
        begin return (h(p2) * yh(p1) - h(p1) * yh(p2)) / (h(p2) - h(p1)); end ysect;

   begin
--
--     Scan the arrays, top down, left to right within rows
--
      for j in reverse d'First(2)..d'last(2)-1 loop
         for i in d'first(1)..d'last(1)-1 loop

            dmin := Real'Min(Real'Min(d(i, j), d(i, j + 1)), Real'Min(d(i + 1, j), d(i + 1, j + 1)));
            dmax := Real'Max(Real'Max(d(i, j), d(i, j + 1)), Real'Max(d(i + 1, j), d(i + 1, j + 1)));

            if  dmax >= z ( z'First ) and dmin <= z ( z'Last )  then
               for k in z'range loop
                  if  z ( k ) in dmin..dmax  then

                     for m in reverse 0..4 loop
                        if  m > 0  then
                           h(m) := d(i + im(m), j + jm(m)) - z(k);
                           xh(m) := x(i + im(m));
                           yh(m) := y(j + jm(m));
                        else
                           h(0) := 0.25 * (h(1) + h(2) + h(3) + h(4));
                           xh(0) := 0.5 * (x(i) + x(i + 1));
                           yh(0) := 0.5 * (y(j) + y(j + 1));
                        end if;
                        if  h ( m ) > 0.0  then
                           sh(m) := + 1;
                        elsif  h ( m ) < 0.0  then
                           sh(m) := -1;
                        else
                           sh(m) := 0;
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
--                    |   \    m-3    /   |
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
                     for m in 1..4 loop
                      m1 := m;
                      m2 := 0;
                      if  m /= 4  then
                        m3 := m + 1;
                      else
                        m3 := 1;
                      end if;

                      cas:= castab(sh(m1), sh(m2), sh(m3));
                      if cas /= 0 then
                        case cas is

--
--     Case 1 - Line between vertices 1 and 2
--
                         when 1 =>
                           x1 := xh(m1);
                           y1 := yh(m1);
                           x2 := xh(m2);
                           y2 := yh(m2);
--
--     Case 2 - Line between vertices 2 and 3
--
                         when 2 =>
                           x1 := xh(m2);
                           y1 := yh(m2);
                           x2 := xh(m3);
                           y2 := yh(m3);
--
--     Case 3 - Line between vertices 3 and 1
--
                         when 3 =>
                           x1 := xh(m3);
                           y1 := yh(m3);
                           x2 := xh(m1);
                           y2 := yh(m1);
--
--     Case 4 - Line between vertex 1 and side 2-3
--
                         when 4 =>
                           x1 := xh(m1);
                           y1 := yh(m1);
                           x2 := xsect(m2, m3);
                           y2 := ysect(m2, m3);
--
--     Case 5 - Line between vertex 2 and side 3-1
--
                         when 5 =>
                           x1 := xh(m2);
                           y1 := yh(m2);
                           x2 := xsect(m3, m1);
                           y2 := ysect(m3, m1);
--
--     Case 6 - Line between vertex 3 and side 1-2
--
                         when 6 =>
                           x1 := xh(m3);
                           y1 := yh(m3);
                           x2 := xsect(m1, m2);
                           y2 := ysect(m1, m2);
--
--     Case 7 - Line between sides 1-2 and 2-3
--
                         when 7 =>
                           x1 := xsect(m1, m2);
                           y1 := ysect(m1, m2);
                           x2 := xsect(m2, m3);
                           y2 := ysect(m2, m3);
--
--     Case 8 - Line between sides 2-3 and 3-1
--
                         when 8 =>
                           x1 := xsect(m2, m3);
                           y1 := ysect(m2, m3);
                           x2 := xsect(m3, m1);
                           y2 := ysect(m3, m1);
--
--     Case 9 - Line between sides 3-1 and 1-2
--
                         when 9 =>
                           x1 := xsect(m3, m1);
                           y1 := ysect(m3, m1);
                           x2 := xsect(m1, m2);
                           y2 := ysect(m1, m2);

                         when others => raise Constraint_Error;

                        end case;
                        Vecout (x1,y1,x2,y2,z(k));
                      end if;

                     end loop;
                  end if;

               end loop;
            end if;

         end loop;

      end loop;

   end ConRec;

end Contours;
