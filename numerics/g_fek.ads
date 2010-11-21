------------------------------------------------------------------------------
--  File:            g_fek.ads
--  Description:     Finite Elements Kernel (extracts)
--                   - generic -> ok for any precision!
--  Date / Version:  3-Feb-2005 ; 25-Apr-2000 ; ... ; 25.VI.1997
--  Author:          M. Bercovier, Jerusalem
--                   Ada version by G. de Montmollin
--                   http://gautiersblog.blogspot.com/
------------------------------------------------------------------------------

generic

  type FEK_Float is digits <>;
  type Vector is array (Integer range <>) of FEK_Float;
  type Matrix is array (Integer range <>, Integer range <>) of FEK_Float;

package G_FEK is

  type t_element is (L2, Q4, Q9, B27);

  nodes_per_element: constant array(t_element) of Positive:=
    (L2=> 2, Q4=> 4, Q9=> 9, B27=> 27);

  spatial_dimension: constant array(t_element) of Positive:=
    (L2=> 1, Q4=> 2, Q9=> 2, B27=> 3);

  -- Pour elements reguliers:

  faces_per_element: constant array(t_element) of Natural:=
    (L2=> 2, Q4=> 4, Q9=> 4, B27=> 6);

  nodes_per_face: constant array(t_element) of Natural:=
    (L2=> 1, Q4=> 2, Q9=> 3, B27=> 9);

  ------- Conversions de numeros de faces vers numeros d'elements -------

  type t_face_cnv is array(Positive range <>, Positive range <>) of Positive;

  -- 1D linear to 0D pointwise vertices numbering conversion:

  P1_to_L2: constant t_face_cnv(1..2,1..1):= ((1=> 1),(1=> 2));

  -- 2D linear to 1D linear vertices numbering conversion:

  L2_to_Q4: constant t_face_cnv(1..4,1..2):=
    ((1, 2),    -- face 1: y=0
     (2, 3),    -- face 2: x=1
     (3, 4),    -- face 3: y=1
     (4, 1));   -- face 4: x=1

  -- 2D parabolic to 1D parabolic vertices numbering conversion:

  L3_to_Q9: constant t_face_cnv(1..4,1..3):=
    ((1, 5, 2),    -- face 1: y=0
     (2, 6, 3),    -- face 2: x=1
     (3, 7, 4),    -- face 3: y=1
     (4, 8, 1));   -- face 4: x=1

  -- 3D parabolic to 2D parabolic vertices numbering conversion:

  Q9_to_B27: constant t_face_cnv(1..6,1..9):=
    ((1, 2, 6, 5, 9,18,13,17,25),    -- face 1: y=0
     (2, 3, 7, 6,10,19,14,18,22),    -- face 2: x=1
     (3, 4, 8, 7,11,20,15,19,26),    -- face 3: y=1
     (4, 1, 5, 8,12,17,16,20,24),    -- face 4: x=0
     (3, 2, 1, 4,10, 9,12,11,21),    -- face 5: z=0  -- orient. corr. 24.X.1997
     (5, 6, 7, 8,13,14,15,16,23));   -- face 6: z=1

  type p_face_cnv is access t_face_cnv;
  pn_P1_to_L2:  constant p_face_cnv:= new t_face_cnv ' (P1_to_L2);
  pn_L2_to_Q4:  constant p_face_cnv:= new t_face_cnv ' (L2_to_Q4);
  pn_L3_to_Q9:  constant p_face_cnv:= new t_face_cnv ' (L3_to_Q9);
  pn_Q9_to_B27: constant p_face_cnv:= new t_face_cnv ' (Q9_to_B27);

  to_face_conv: constant array(t_element) of p_face_cnv:=
    (L2=> pn_P1_to_L2, Q4=> pn_L2_to_Q4, Q9=> pn_L3_to_Q9, B27=> pn_Q9_to_B27);

  type shape_opt is (loc_der,loc_der_jac,loc_der_jac_glob_der);

  subtype l2_vector is Vector(1..2);
  subtype l2_grad   is Matrix(1..2, 1..1);

  subtype q4_vector is Vector(1..4);
  subtype q4_grad   is Matrix(1..4, 1..2);

  subtype q9_vector is Vector(1..9);
  subtype q9_grad   is Matrix(1..9, 1..2);

  subtype b27_vector is Vector(1..27);
  subtype b27_grad   is Matrix(1..27, 1..3);

  permu_q9: constant array(1..3,1..3) of Positive:=
      ( ( 1,  8,  4), ( 5,  9,  7), ( 2,  6,  3) );

  permu27: constant array(1..3,1..3,1..3) of Positive:=
      (( ( 1, 17,  5), (12, 24, 16), ( 4, 20,  8)),
       ( ( 9, 25, 13), (21, 27, 23), (11, 26, 15)),
       ( ( 2, 18,  6), (10, 22, 14), ( 3, 19,  7)));

  l_coord: constant array(1..2) of FEK_Float:= (-1.0, 1.0);

  q_coord: constant array(1..9,1..2) of FEK_Float:=
      ( (-1.0,-1.0), ( 1.0,-1.0), ( 1.0, 1.0), (-1.0, 1.0),
        ( 0.0,-1.0), ( 1.0, 0.0), ( 0.0, 1.0), (-1.0, 0.0),
        ( 0.0, 0.0) );
  b_coord: constant array(1..27,1..3) of FEK_Float:=
      ( (-1.0,-1.0,-1.0), ( 1.0,-1.0,-1.0), ( 1.0, 1.0,-1.0), (-1.0, 1.0,-1.0),
        (-1.0,-1.0, 1.0), ( 1.0,-1.0, 1.0), ( 1.0, 1.0, 1.0), (-1.0, 1.0, 1.0),
        ( 0.0,-1.0,-1.0), ( 1.0, 0.0,-1.0), ( 0.0, 1.0,-1.0), (-1.0, 0.0,-1.0),
        ( 0.0,-1.0, 1.0), ( 1.0, 0.0, 1.0), ( 0.0, 1.0, 1.0), (-1.0, 0.0, 1.0),
        (-1.0,-1.0, 0.0), ( 1.0,-1.0, 0.0), ( 1.0, 1.0, 0.0), (-1.0, 1.0, 0.0),
        ( 0.0, 0.0,-1.0), ( 1.0, 0.0, 0.0), ( 0.0, 0.0, 1.0), (-1.0, 0.0, 0.0),
        ( 0.0,-1.0, 0.0), ( 0.0, 1.0, 0.0), ( 0.0, 0.0, 0.0) );


  ---------------------
  -- Shape functions --
  ---------------------

  -------------------------------------------------------
  -- L2:  line element, in one dimension.              --
  --      linear shape functions.                      --
  -------------------------------------------------------

  -- NOTE, GdM Apr-2000: The original FEK handles the element
  -- in 2 dimensions with a wrong determinant (a SQRT too much)
  -- but correct global derivatives using it!

  procedure l2shap (x         :        l2_vector;  -- x(i) = coordinate of node i
                    s         :        FEK_Float; -- s = some local point
                    shape     :    out l2_vector;  -- shape fcn value at s
                    dadl,dadg : in out l2_grad;    -- local and global gradients
                    det       :    out FEK_Float; -- jacobian of transformation
                    eps       :        FEK_Float);-- 0 det checking criterion

  procedure l2shap (s: FEK_Float; shape: out l2_vector);

  -------------------------------------------------------
  -- Q4:  quadrilateral element, in two dimensions.    --
  --      linear shape functions.                      --
  -------------------------------------------------------

  procedure q4shap (x1,x2     :        q4_vector;  -- (x1(i),x2(i)) = node i
                    s,t       :        FEK_Float; -- (s,t) = some local point
                    shape     :    out q4_vector;  -- shape fcn value at (s,t)
                    dadl,dadg : in out q4_grad;    -- local and global gradients
                    det       :    out FEK_Float; -- jacobian of transformation
                    calc_shape:        Boolean;    -- calculate shape values ?
                    opt       :        shape_opt;  -- options
                    eps       :        FEK_Float);-- 0 det checking criterion

  procedure q4shap (s,t: FEK_Float; shape: out q4_vector);

  -------------------------------------------------------
  -- Q9:  quadrilateral element, in two dimensions.    --
  --      parabolic shape functions.                   --
  -------------------------------------------------------

  procedure q9shap (x1,x2     :        q9_vector;  -- (x1(i),x2(i)) = node i
                    s,t       :        FEK_Float; -- (s,t) = some local point
                    shape     :    out q9_vector;  -- shape fcn value at (s,t)
                    dadl,dadg : in out q9_grad;    -- local and global gradients
                    det       :    out FEK_Float; -- jacobian of transformation
                    calc_shape:        Boolean;    -- calculate shape values ?
                    opt       :        shape_opt;  -- options
                    eps       :        FEK_Float);-- 0 det checking criterion

  procedure q9shap (s,t: FEK_Float; shape: out q9_vector);

  -------------------------------------------------------
  -- B27: quadrilateral element, in three dimensions.  --
  --      parabolic shape functions.                   --
  -------------------------------------------------------

  procedure b27shp (x1,x2,x3  :        b27_vector; -- (x1(i),x2(i),x3(i)) = node i
                    r,s,t     :        FEK_Float; -- (r,s,t) = some local point
                    shape     :    out b27_vector; -- shape fcn value at (r,s,t)
                    dadl,dadg : in out b27_grad;   -- local and global gradients
                    det       :    out FEK_Float; -- jacobian of transformation
                    calc_shape:        Boolean;    -- calculate shape values ?
                    opt       :        shape_opt;  -- options
                    eps       :        FEK_Float);-- 0 det checking criterion

  procedure b27shp (r,s,t: FEK_Float; shape: out b27_vector);

  zero_jacobian, negative_jacobian: exception;

  -- Translated from Fortran: marked with *

  -- *     SUBROUTINE L2SHAP (X1,X2,S,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  --       SUBROUTINE L3SHAP (X1,X2,S,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  --       SUBROUTINE T3SHAP (X1,X2,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  --       SUBROUTINE T6SHAP (X1,X2,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  -- *     SUBROUTINE Q4SHAP (X1,X2,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  --       SUBROUTINE Q8SHAP (X1,X2,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  -- *     SUBROUTINE Q9SHAP (X1,X2,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  --       SUBROUTINE Q49SHP (X1,X2,NOD,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  --       SUBROUTINE TH4SHP (X1,X2,X3,R,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  --       SUBROUTINE TH10SH (X1,X2,X3,R,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  -- *     SUBROUTINE B27SHP (X1,X2,X3,R,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  --       SUBROUTINE B821SP (X1,X2,X3,NOD,R,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  --       SUBROUTINE B8SHAP (X1,X2,X3,R,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  --       SUBROUTINE PR18SP (X1,X2,X3,R,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)
  --       SUBROUTINE PR6SHP (X1,X2,X3,R,S,T,MNDP,SHAPE,DADL,DADG,DET,IOPT,EPS,IERR)

end G_FEK;