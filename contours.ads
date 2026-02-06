--======================================================================
--
--     Contours.ads
--
--     CONREC is a contouring subroutine for rectangularly spaced data.
--
--     It emits calls to a line drawing subroutine supplied by the user
--     which draws a contour map corresponding to floating-point data on
--     a randomly spaced rectangular grid. The coordinates emitted are
--     in the same units given in the x() and y() arrays.
--
--     Any number of contour levels may be specified but they must be
--     in order of increasing value.
--
--     procedure ConRec (d, x, y, z)
--       d : matrix of data to contour
--       x : data matrix column coordinates
--       y : data matrix row coordinates
--       z : contour levels in increasing order
--
--       Vecout: user-defined line drawing procedure, of the form
--               (x1, y1, x2, y2, z : Real);
--
--       x1, y1 : point 1
--       x2, y2 : point 2
--       z      : value
--
--     There is often the requirement to distinguish each contour
--     line with a different colour or a different line style. This
--     can be done in many ways using the contour values z for a
--     particular line segment.
--
--     A demo is available in file: cr_demo.adb
--
--     Author: Paul D. Bourke
--
--     Ada version: 23.I.1999, Gautier de Montmollin
--
--     This is part of the Mathpaqs collection of mathematical packages.
--     Latest version may be available at:
--         home page:     http://mathpaqs.sf.net/
--         project page:  http://sf.net/projects/mathpaqs/
--         mirror:        https://github.com/zertovitch/mathpaqs
--======================================================================

generic

  --  Any floating-point type:
  type Real is digits <>;
  --  User-defined line drawing procedure:
  with procedure Vec_Out (x1, y1, x2, y2, z : Real);

package Contours is

  type Contour_Data  is array (Integer range <>, Integer range <>) of Real;
  type Contour_Pos   is array (Integer range <>) of Real;
  type Contour_Level is array (Integer range <>) of Real;

  procedure Con_Rec (d    : Contour_Data;
                     x, y : Contour_Pos;
                     z    : Contour_Level);
  --  d (i, j) represents the data value at point (x (i), y (j)).
  --
  --  Assumption: x'Range = d'Range (1), y'Range = d'Range (2).
  --
  --  Array z contains a selection of z values that ConRec will display
  --  as level curves.

end Contours;
