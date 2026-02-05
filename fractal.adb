--  Display Sierpinski and Barnsley fractals with various levels
--  using linear algebra.
--
--  Output: PDF files.
--
--  Authors: Stephane Perret and Gautier de Montmollin
--
--  March 2009
--  This procedure was made in around one 1/2 hour...

with PDF_Out;

with Ada.Numerics.Generic_Elementary_Functions;

procedure Fractal is

  use PDF_Out;

  pdf : PDF_Out_File;

  package REF is new Ada.Numerics.Generic_Elementary_Functions (Real);
  use Ada.Numerics, REF;

  subtype Vector is Point;

  type Figure is array (Positive range <>) of Point;

  type Matrix22 is array (1 .. 2, 1 .. 2) of Real;

  function "+" (p : Point; v : Vector) return Point is
  begin
    return (p.x + v.x, p.y + v.y);
  end "+";

  function "*" (M : Matrix22; p : Point) return Point is
  begin
    return
      (M (1, 1) * p.x + M (1, 2) * p.y,
       M (2, 1) * p.x + M (2, 2) * p.y);
  end "*";

  function "*" (f : Real; M : Matrix22) return Matrix22 is
  begin
    return
      ((M (1, 1) * f, M (1, 2) * f),
       (M (2, 1) * f, M (2, 2) * f));
  end "*";

  function "*" (M1, M2 : Matrix22) return Matrix22 is
  begin
    return
      ((M1 (1, 1) * M2 (1, 1) + M1 (1, 2) * M2 (2, 1),
        M1 (1, 1) * M2 (1, 2) + M1 (1, 2) * M2 (2, 2)),
       (M1 (2, 1) * M2 (1, 1) + M1 (2, 2) * M2 (2, 1),
        M1 (2, 1) * M2 (1, 2) + M1 (2, 2) * M2 (2, 2)));
  end "*";

  function "*" (f : Real; p : Point) return Point is
  begin
    return (f * p.x, f * p.y);
  end "*";

  function "*" (f : Real; fig : Figure) return Figure is
    res : Figure := fig;
  begin
    for i in res'Range loop
      res (i) := f * res (i);
    end loop;
    return res;
  end "*";
  pragma Unreferenced ("*");

  type Affine is record
    M : Matrix22;
    v : Vector;
  end record;

  type Affine_Array is array (Positive range <>) of Affine;

  function Morph (f : Figure; a : Affine) return Figure is
    mod_f : Figure (f'Range);
  begin
    for i in f'Range loop
      mod_f (i) := a.M * f (i) + a.v;
    end loop;
    return mod_f;
  end Morph;

  procedure Draw (f : Figure; a : Affine_Array; level : Natural) is
  begin

    if level = 0 then
      pdf.Move (f (f'First));
      for i in f'First + 1 .. f'Last loop
        pdf.Line (f (i));
      end loop;
      pdf.Finish_Path (False, stroke, nonzero_winding_number);
    else
      for i in a'Range loop
        Draw (Morph (f, a (i)), a, level - 1);
      end loop;
    end if;

  end Draw;

  procedure Plot (f : Figure; a : Affine_Array; n : String; iterations : Positive := 10) is
    subtype Level_Range is Integer range 0 .. iterations;
    margin : constant := 0.5;
    width  : constant := 1.0 + 2.0 * margin;
    height : constant Real := width * Sqrt (2.0);  --  Proportions of the A* papers.
  begin
    pdf.Create (n);
    pdf.Page_Setup (PDF_Out.A4_portrait);
    pdf.Set_Math_Plane ((-margin, -margin, width, height));
    for level in Level_Range loop
      Draw (f, a, level);
      if level < Level_Range'Last then
        pdf.New_Page;
      end if;
    end loop;
    pdf.Close;
  end Plot;

  triangle : constant Figure := ((0.0, 0.0), (0.5, Sqrt (3.0) / 2.0), (1.0, 0.0), (0.0, 0.0));
  square   : constant Figure := ((0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0), (0.0, 0.0));

  Id : constant Matrix22 :=
    ((1.0, 0.0),
     (0.0, 1.0));

  Halving : constant Matrix22 := 0.5 * Id;

  --  Napkins

  procedure Plot_Sierpinski_Translations (n : String; f : Figure) is

    M : constant Matrix22 := Halving;

    v1 : constant Vector := (0.0, 0.0);
    v2 : constant Vector := (0.5, 0.0);
    v3 : constant Vector := (0.25, Sqrt (3.0) / 4.0);

    transformation : constant Affine_Array := ((M, v1), (M, v2), (M, v3));

  begin
    Plot (f, transformation, n);
  end Plot_Sierpinski_Translations;

  procedure Plot_Dragons (n : String; f : Figure; scale : Real) is

    h : constant Real := Sqrt (3.0) * 0.5;

    M1 : constant Matrix22 := scale *
      --  +60 deg
      ((0.5, -h),
       (+h,  0.5));

    M2 : constant Matrix22 := scale *
      --  -60 deg
      ((0.5, +h),
       (-h,  0.5));

    v2 : constant Vector :=  (0.5, h);

    transformation : constant Affine_Array := ((Id, (0.0, 0.0)), (M1, (0.0, 0.0)), (M2, v2));

  begin
    Plot (f, transformation, n);
  end Plot_Dragons;

  procedure Plot_Sierpinski_Rotated_Segments_1 (n : String; angle : Real := Pi * 0.5 - Arctan (0.5)) is
    --  The hook is not equilateral!

    hook : constant Figure (1 .. 4) := ((0.0, 0.0), (0.25, 0.5), (0.75, 0.5), (1.0, 0.0));

    csa : constant Real := Cos (angle);
    sna : constant Real := Sin (angle);
    obl : constant Real := Sqrt (5.0) * 0.25;

    M1 : constant Matrix22 := obl *
          ((-csa,  sna),
           (-sna, -csa));

    M2 : constant Matrix22 := Halving;

    M3 : constant Matrix22 := obl *
          ((-csa, -sna),
            (sna, -csa));

    v1 : constant Vector := hook (2);
    v2 : constant Vector := hook (2);
    v3 : constant Vector := hook (4);

    transformation : constant Affine_Array := ((M1, v1), (M2, v2), (M3, v3));

  begin
    Plot (hook, transformation, n);
  end Plot_Sierpinski_Rotated_Segments_1;

  procedure Plot_Sierpinski_Rotated_Segments_2 (n : String; angle : Real := Pi / 3.0) is

    h : constant Real := Sqrt (3.0) * 0.25;

    hook : constant Figure (1 .. 4) := ((0.0, 0.0), (0.25, h), (0.75, h), (1.0, 0.0));

    csa : constant Real := Cos (angle);
    sna : constant Real := Sin (angle);

    M1 : constant Matrix22 := Halving *
          ((-csa,  sna),
           (-sna, -csa));

    M2 : constant Matrix22 := Halving;

    M3 : constant Matrix22 := Halving *
          ((-csa, -sna),
            (sna, -csa));

    v1 : constant Vector := hook (2);
    v2 : constant Vector := hook (2);
    v3 : constant Vector := hook (4);

    transformation : constant Affine_Array := ((M1, v1), (M2, v2), (M3, v3));

  begin
    Plot (hook, transformation, n);
  end Plot_Sierpinski_Rotated_Segments_2;

  --  Farn

  procedure Plot_Barnsley (n : String) is

    M1 : constant Matrix22 := ((0.849, 0.037),
                              (-0.037, 0.849));

    M2 : constant Matrix22 := ((0.197, -0.226),
                               (0.226, 0.197));

    M3 : constant Matrix22 := ((-0.150, 0.283),
                               (0.260, 0.237));

    M4 : constant Matrix22 := ((0.0, 0.0),
                               (0.0, 0.16));

    v1 : constant Vector := (0.075, 0.1830);
    v2 : constant Vector := (0.4, 0.049);
    v3 : constant Vector := (0.575, -0.0840);
    v4 : constant Vector := (0.5, 0.0);

    transformation : constant Affine_Array :=
      ((M1, v1), (M2, v2), (M3, v3), (M4, v4));

  begin
    Plot (triangle, transformation, n);
  end Plot_Barnsley;

  procedure Plot_Tree (n : String; scale : Real; angle : Real) is

    csa : constant Real := Cos (angle);
    sna : constant Real := Sin (angle);

    M1 : constant Matrix22 := Id;

    M2 : constant Matrix22 := (1.0 - scale) *
          ((+csa, -sna),
           (+sna, +csa));

    M3 : constant Matrix22 := (1.0 - scale) *
          ((+csa, +sna),
           (-sna, +csa));

    v2 : constant Vector := (0.0, scale);
    v3 : constant Vector := v2;

    transformation : constant Affine_Array :=
      ((M1, (0.0, 0.0)), (M2, v2), (M3, v3));

    trunc : constant Figure := ((0.0, 0.0), (0.0, 1.0));

  begin
    Plot (trunc, transformation, n, 13);
  end Plot_Tree;

  procedure Plot_Dice (n : String; f : Figure) is

    M : constant Matrix22 := 1.0 / 3.0 * Id;

    v1 : constant Vector :=  (0.0,       0.0);
    v2 : constant Vector :=  (1.0 / 3.0, 0.0);
    v3 : constant Vector :=  (2.0 / 3.0, 0.0);
    v4 : constant Vector :=  (0.0,       1.0 / 3.0);
    v5 : constant Vector :=  (2.0 / 3.0, 1.0 / 3.0);
    v6 : constant Vector :=  (0.0,       2.0 / 3.0);
    v7 : constant Vector :=  (1.0 / 3.0, 2.0 / 3.0);
    v8 : constant Vector :=  (2.0 / 3.0, 2.0 / 3.0);

    transformation : constant Affine_Array :=
       ((M, v1), (M, v2), (M, v3), (M, v4), (M, v5), (M, v6), (M, v7), (M, v8));

  begin
    Plot (f, transformation, n, 5);
  end Plot_Dice;

  procedure Plot_Levy (n : String) is

    f : constant Figure := ((0.0, 0.0), (1.0, 0.0));

    M3 : constant Matrix22 := 0.5 *
      --  -90 deg
      ((+0.0, +1.0),
       (-1.0, +0.0));

    M2 : constant Matrix22 := 0.5 * Id;

    M1 : constant Matrix22 := 0.5 *
      --  +90 deg
      ((+0.0, -1.0),
       (+1.0, +0.0));

    transformation : constant Affine_Array :=
      ((M1, (0.0, 0.0)), (M2, (0.0, +0.5)), (M2, (0.5, +0.5)), (M3, (1.0, +0.5)));

  begin
    Plot (f, transformation, n);
  end Plot_Levy;

begin
  Plot_Sierpinski_Translations ("sierpinski_triangles.pdf", triangle);
  Plot_Dragons ("dragon_triangles.pdf", triangle, 0.55);
  Plot_Sierpinski_Rotated_Segments_1 ("sierpinski_hooks_asym.pdf");
  Plot_Sierpinski_Rotated_Segments_1 ("sierpinski_hooks_asym_skewed.pdf", Pi / 6.0);
  Plot_Sierpinski_Rotated_Segments_2 ("sierpinski_hooks.pdf");
  Plot_Barnsley ("barnsley.pdf");
  Plot_Tree ("spiky_tree.pdf", 0.37, 1.1);
  Plot_Dice ("carpet_squares.pdf", square);
  Plot_Levy ("levy.pdf");
end Fractal;
