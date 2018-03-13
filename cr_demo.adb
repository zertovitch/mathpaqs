--     The follwoing is a simplistic application of the CONREC routine.
--     A mathematical function is evaluated over a regular grid of points
--     on a computer raster graphics screen.
--
--     Paul D. Bourke
--
--     Ada version: 23.I.1999, G. de Montmollin

-- with Ada.Text_IO;              use Ada.Text_IO;
with Graph;                    use Graph;
with Contours;

procedure CR_Demo is

      pxmin : constant := 10;
      pymin : constant := 10;
      pxmax : constant := 460;
      pymax : constant := 300;

      nx : constant := 100;
      ny : constant := 80;
      nc : constant := 130;  --  Number of contour levels (z-axis)

      x1, y1, x2, y2, zmax, zmin: Float;

      -- pala: constant:= 16; palz: constant:= 255; palaz: constant:=palz-pala;

      procedure col_vecout(x1, y1, x2, y2, z: Float) is
        zz: constant Float:= (z-zmin)/(zmax-zmin);
      begin
        -- SetColor( integer(float(palaz)*(z-zmin)/(zmax-zmin)) + pala );  -- indexed colors
        SetColor(zz,0.0,1.0-zz);
        Line(x1, y1, x2, y2);
      end col_vecout;

      package CRG is new Contours(Float, col_vecout); use CRG;

      d : contour_data(0..nx,0..ny);
      x : contour_pos(0..nx);
      y : contour_pos(0..ny);
      z : contour_level(1..nc);

--
--     Create an artificial data surface and calculate the
--     surface bounds for choosing the contour levels.
--

   begin
      zmin := 1.0e30;
      zmax := -1.0e30;
      for i in 0..nx loop
         for j in 0..ny loop
            d(i,j) := Float((i-nx/2) * (j-ny/2));
            zmin := Float'Min(zmin, d(i, j));
            zmax := Float'Max(zmax, d(i, j));
         end loop;
      end loop;
--
--     Set coordinates in Y array suitable for
--     automatic plotting on the graphics screen
--
      for j in 0..ny loop
         y(j) := Float(j * (pymax - pymin)) / Float(ny) + Float(pymin);
      end loop;
--
--     Set coordinates in X array suitable for
--     automatic plotting on the graphics screen
--
      for i in 0..nx loop
         x(i) := Float(i * (pxmax - pxmin)) / Float(nx) + Float(pxmin);
      end loop;
--
--     Set a full contingent of contour levels
--
      for i in 1..nc loop
         z(i) := Float(i) * (zmax - zmin) / Float(nc + 1);
      end loop;

--     Init. graphics
      current_device:= PostScript;
      InitGraph(PostScript, "CR_Demo.ps");

      --  current_device:= VESA;
      --  InitGraph(VESA, VESA_800x600);
      --  for i in 0..palaz loop          -- d‚grad‚
      --    p:= float(i)/float(palaz);
      --    SetRGBPalette(i+pala, integer(p*63.0), 0, 63-integer(p*63.0));
      --  end loop;

--
--     Draw a border around the contour plot
--
      x1 := Float(pxmin);
      y1 := Float(pymin);
      x2 := Float(pxmax);
      y2 := Float(pymax);

      Set_math_plane(x1,y1,x2,y2);

      col_vecout(x1,y1,x1,y2,0.0);
      col_vecout(x1,y2,x2,y2,0.0);
      col_vecout(x2,y2,x2,y1,0.0);
      col_vecout(x2,y1,x1,y1,0.0);

--     Grid

      for i in 1..nx-1 loop
         for j in 1..ny-1 loop
            col_vecout(x(i),y(j),x(i),y(j),d(i,j));
         end loop;
      end loop;

--
--     Call the contouring routine
--
      ConRec(d,x,y,z);

--     Wait and close graphics

      -- Skip_Line;
      CloseGraph;

   end CR_Demo;
