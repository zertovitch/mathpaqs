--     The following is a simplistic application of the CONREC routine.
--     A mathematical function is evaluated over a regular grid of points
--     on a computer raster graphics screen.
--
--     Paul D. Bourke
--
--     Ada version: 23.I.1999, G. de Montmollin

--  with Ada.Text_IO;              use Ada.Text_IO;
with PDF_Out;
with Contours;

procedure CR_Demo is

  pxmin : constant := 10;
  pymin : constant := 10;
  pxmax : constant := 460;
  pymax : constant := 300;

  nx : constant := 100;  --  Number of x values - 1 (zero-based)
  ny : constant := 80;   --  Number of y values - 1 (zero-based)
  nc : constant := 130;  --  Number of contour levels (z-axis)

  use PDF_Out;

  pdf : PDF_Out_File;

  zmax, zmin : Real;

  procedure col_vecout (x1, y1, x2, y2, z : Real) is
    zz : constant Real := (z - zmin) / (zmax - zmin);
  begin
    pdf.Stroking_Color ((zz, 0.0, 1.0 - zz));
    pdf.Single_Line ((x1, y1), (x2, y2));
  end col_vecout;

  x1, y1, x2, y2 : Real;

  package CRG is new Contours (Real, col_vecout);

  use CRG;

  d : Contour_data  (0 .. nx, 0 .. ny);
  x : Contour_pos   (0 .. nx);
  y : Contour_pos   (0 .. ny);
  z : Contour_level (1 .. nc);

begin
  --
  --     Create an artificial data surface and calculate the
  --     surface bounds for choosing the contour levels.
  --
  zmin :=  1.0e30;
  zmax := -1.0e30;
  for i in 0 .. nx loop
    for j in 0 .. ny loop
      d (i, j) := Real ((i - nx / 2) * (j - ny / 2));
      zmin := Real'Min (zmin, d (i, j));
      zmax := Real'Max (zmax, d (i, j));
    end loop;
  end loop;
  --
  --     Set coordinates in Y array suitable for
  --     automatic plotting on the graphics screen
  --
  for j in 0 .. ny loop
    y (j) := Real (j * (pymax - pymin)) / Real (ny) + Real (pymin);
  end loop;
  --
  --     Set coordinates in X array suitable for
  --     automatic plotting on the graphics screen
  --
  for i in 0 .. nx loop
    x (i) := Real (i * (pxmax - pxmin)) / Real (nx) + Real (pxmin);
  end loop;
  --
  --     Set a full contingent of contour levels
  --
  for i in 1 .. nc loop
    z (i) := Real (i) * (zmax - zmin) / Real (nc + 1);
  end loop;

  --     Init. graphics
  pdf.Create ("cr_demo.pdf");
  pdf.Page_Setup (PDF_Out.A4_portrait);

  --
  --     Draw a border around the contour plot
  --
  x1 := Real (pxmin);
  y1 := Real (pymin);
  x2 := Real (pxmax);
  y2 := Real (pymax);

  pdf.Set_Math_Plane ((x1, y1, x2 - x1, y2 - y1));

  col_vecout (x1, y1, x1, y2, 0.0);
  col_vecout (x1, y2, x2, y2, 0.0);
  col_vecout (x2, y2, x2, y1, 0.0);
  col_vecout (x2, y1, x1, y1, 0.0);

  --     Grid (dots)
  for i in 1 .. nx - 1 loop
    for j in 1 .. ny - 1 loop
      col_vecout (x (i), y (j), x (i), y (j), d (i, j));
    end loop;
  end loop;
  --
  --     Call the contouring routine
  --
  ConRec (d, x, y, z);

  pdf.Close;
end CR_Demo;
