--  Author: Jon Squire
--  Original code here:
--    http://www.cs.umbc.edu/~squire/adaclass/gnatmath95/

--  test_generic_real_linear_equations.adb

--         TEST PROGRAM TO EXERCIZE  GENERIC REAL LINEAR EQUATIONS
--
--
--  THIS TEST:  SETS UP AN INITIAL COEFFICIENT MATRIX
--              SETS UP THE DEPENDENT VECTOR
--              SOLVES THE LINEAR EQUATIONS
--              RECOMPUTES THE DEPENDENT VECTOR
--
with Real_Arrays_IO;
with Generic_Real_Linear_Equations;
with Integer_Arrays;
with Integer_Arrays_IO;
with Ada.Text_IO;
with Ada.Numerics.Generic_Real_Arrays;

with System;

procedure Test_Generic_Real_Linear_Equations is

  type Real is digits System.Max_Digits;

  package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (Real);
  use Real_Arrays;

  package Real_Arr_IO is new REAL_ARRAYS_IO (Real, Real_Arrays);
  use Real_Arr_IO, Real_Arr_IO.Real_IO;

  package INT_IO is new INTEGER_ARRAYS_IO (Integer, Integer_Arrays);
  use INT_IO;

  use Integer_Arrays;

  package Real_Linear_Equations is new Generic_Real_Linear_Equations
            (Real, Real_Arrays, Integer_Vector);
  use Real_Linear_Equations;

  use Ada.Text_IO;

  procedure Put_Rect (A : Real_Matrix) is
  begin
    for i in A'Range (1) loop
      Put ("    ");
      for j in A'Range (2) loop
        Put (A (i, j), 4, 2, 0);
      end loop;
      New_Line;
    end loop;
  end Put_Rect;

  --                        SPECIFICATIONS

  A   : Real_Matrix (1 .. 4, 1 .. 4);
  AC  : Real_Matrix (1 .. 4, 1 .. 4);
  AI  : Real_Matrix (1 .. 4, 1 .. 4);
  AII : Real_Matrix (1 .. 4, 1 .. 4);

  Y : constant Real_Vector (1 .. 4) := (30.0, 31.0, 34.0, 40.0);
  X : Real_Vector (1 .. 4);
  VECTOR_RESULT : Real_Vector (1 .. 4);

  XX : Real_Matrix (1 .. 4, 1 .. 2);

  YY : constant Real_Matrix (1 .. 4, 1 .. 2) :=
     ((30.0, 15.0), (31.0, 15.5),
      (34.0, 17.0), (40.0, 20.0));

  MATRIX_RESULT : Real_Matrix (1 .. 4, 1 .. 2);

  A2 : constant Real_Matrix (-1 .. 1, 0 .. 2) :=
     ((1.0, 2.0, 3.0),
      (4.0, 5.0, 6.0),
      (4.0, 4.0, 4.0));

  A3 : constant Real_Matrix (-1 .. 1, 0 .. 1) :=
     ((1.0, 2.0),
      (4.0, 5.0),
      (4.0, 4.0));

  A2I : Real_Matrix (1 .. 3, 4 .. 6);
  pragma Unreferenced (A2I);

  Y2 : constant Real_Vector (0 .. 2) := (1.5, 2.5, 3.5);
  X2 : Real_Vector (0 .. 2);
  pragma Unreferenced (X2);

  D : Real;

  L : Real_Matrix (1 .. 4, 6 .. 9) := (others => (others => 0.0));
  U : Real_Matrix (-4 .. -1, 0 .. 3) := (others => (others => 0.0));
  P : Integer_Vector (-7 .. -4);
  Q : Real_Matrix (1 .. 4, 6 .. 9) := (others => (others => 0.0));
  QQ : Real_Matrix (1 .. 4, 6 .. 9) := (others => (others => 0.0));
  R : Real_Matrix (-4 .. -1, 0 .. 3) := (others => (others => 0.0));
  RI : Real_Matrix (-4 .. -1, 0 .. 3) := (others => (others => 0.0));

--                       ROUTINE ( USED LOCALLY )

  procedure MAT_INIT_1 (B : out Real_Matrix) is
  begin
    for I in B'First (1) .. B'Last (1) loop
      for J in B'First (1) .. B'Last (1) loop
        if I > J then
          B (I, J) := Real (I);
        else
          B (I, J) := Real (J);
        end if;
      end loop;
    end loop;
  end MAT_INIT_1;

begin
  Put_Line ("Floating point digits: " & Integer'Image (Real'Digits));
  New_Line;

  for I in L'Range (1) loop
    for J in L'Range (2) loop
      if I >= J - L'First (2) + L'First (1) then
        L (I, J) := Real (I + J);
      end if;
    end loop;
  end loop;

  Put_Line ("*** USING LINEAR_EQUATIONS, VECTOR");
  Put_Line ("  INITIAL MATRIX A");
  MAT_INIT_1 (A);
  --  PUT ( A ) ;
  Put_Rect (A);
  Put_Line ("  INITIAL VECTOR Y");
  Put (Y);
  Put_Line ("  SOLVING EQUATIONS");
  X := Solve_JS (A, Y);
  Put_Line ("  X = SOLUTION");
  Put (X);
  Put_Line ("  CHECK IF SOLUTION A*X GIVES BACK Y");
  VECTOR_RESULT := A * X;
  Put (VECTOR_RESULT);
  Put_Line ("  CHECK FOR ZERO VECTOR");
  VECTOR_RESULT := VECTOR_RESULT - Y;
  Put (VECTOR_RESULT);
  New_Line;

  Put_Line ("*** USING LINEAR_EQUATIONS, MATRIX");
  Put_Line ("  INITIAL MATRIX A");
  MAT_INIT_1 (A);
  Put_Rect (A);
  Put_Line ("INITIAL MATRIX YY");
  Put_Rect (YY);
  Put_Line ("SOLVING EQUATIONS");
  XX := Solve_JS (A, YY);
  Put_Line ("XX = SOLUTION");
  Put_Rect (XX);
  Put_Line ("CHECK IF SOLUTION A*XX GIVES BACK YY. A*XX=");
  MATRIX_RESULT := A * XX;
  Put_Rect (MATRIX_RESULT);
  Put_Line ("CHECK FOR ZERO MATRIX");
  MATRIX_RESULT := MATRIX_RESULT - YY;
  Put_Rect (MATRIX_RESULT);
  New_Line;

  PUT_LINE ( "*** CHECK USING CROUT_SOLVE" ) ;
  begin
    PUT_LINE ( "INITIAL MATRIX A" ) ;
    MAT_INIT_1 ( A ) ;
    PUT ( A ) ;
    PUT_LINE ( "INITIAL VECTOR Y" ) ;
    PUT ( Y ) ;
    PUT_LINE ( "SOLVING EQUATIONS" ) ;
    X := CROUT_SOLVE ( A , Y ) ;
    PUT_LINE ( "X = SOLUTION" ) ;
    PUT ( X ) ;
    PUT_LINE ( "CHECK IF SOLUTION A*X GIVES BACK Y" ) ;
    VECTOR_RESULT := A * X ;
    PUT ( VECTOR_RESULT ) ;
    PUT_LINE ( "CHECK FOR ZERO VECTOR" ) ;
    VECTOR_RESULT := VECTOR_RESULT - Y ;
    PUT ( VECTOR_RESULT ) ;
    NEW_LINE ;
  exception
    when others =>
      PUT_LINE ( "CROUT RAISED EXCEPTION" ) ;
  end ;

  PUT_LINE ( "*** CHECK INVERSE" ) ;
  Put_Line("A=");
  Put_rect(A);
  AI := INVERSE_JS ( A ) ;
  PUT_LINE ( "COMPUTED INVERSE" ) ;
  PUT ( AI ) ;
  PUT_LINE ( "CHECK FOR SOLUTION VECTOR, USING A_INVERSE * Y" ) ;
  VECTOR_RESULT := AI * Y ;
  PUT ( VECTOR_RESULT ) ;
  PUT_LINE ( "CHECK FOR ZERO VECTOR" ) ;
  VECTOR_RESULT := VECTOR_RESULT - X ;
  PUT ( VECTOR_RESULT ) ;
  AII := INVERSE_JS ( AI ) ;
  PUT_LINE ( "PRINT INVERSE OF INVERSE" ) ;
  PUT ( AII ) ;
  Put_rect(AII);
  Put_Line("Compare... Ada.Numerics.Generic_Real_Arrays.Inverse");
  Put_Line("   against Generic_Real_Linear_Equations.Inverse_JS.");
  Put_Line("Expected: near zero matrix");
  Put_rect( AI - Inverse(A) );
  NEW_LINE ;

  PUT_LINE ( "*** CHECK FOR EXCEPTIONS IN LINEAR EQUATIONS" ) ;
  begin
    PUT_LINE ( "INITIAL MATRIX A2, singular" ) ;
    PUT ( A2 ) ;
    PUT_LINE ( "INITIAL VECTOR Y2" ) ;
    PUT ( Y2 ) ;
    PUT_LINE ( "SOLVING GENERALIZED EQUATIONS" ) ;
    X2 := Solve_JS ( A2 , Y2 ) ;
    PUT_LINE ( "ERROR IF GET HERE" ) ;
    NEW_LINE ;
  exception
    when MATRIX_DATA_ERROR =>
      PUT_LINE( "MATRIX_DATA_ERROR CORRECTLY RAISED" ) ;
    when others =>
      PUT_LINE( "LINEAR EQUATIONS RAISED WRONG EXCEPTION" ) ;
  end ;

  PUT_LINE ( "*** CHECK FOR EXCEPTIONS IN INVERSE" ) ;
  begin
    PUT_LINE ( "A2" );
    PUT ( A2 ) ;
    A2I := INVERSE_JS ( A2 ) ;
    PUT_LINE ( "ERROR IF GET HERE" ) ;
    NEW_LINE ;
  exception
    when Matrix_Data_Error =>
      PUT_LINE( "Matrix_Data_Error CORRECTLY RAISED" ) ;
    when others =>
      PUT_LINE ( "INVERSE RAISED WRONG EXCEPTION" ) ;
  end ;

  begin
    PUT_LINE ( "*** CALLING LINEAR_EQUATIONS WITH NON SQUARE MATRIX" ) ;
    X2 := Solve_JS ( A3 , Y2 ) ;
    PUT_LINE ( " WRONG, LINEAR_EQUATIONS DID NOT RAISE Constraint_Error" ) ;
  exception
    when Constraint_Error =>
      PUT_LINE ( " CORRECTLY RAISED Constraint_Error" ) ;
    when others =>
      PUT_LINE ( " WRONG, RAISED SOME EXCEPTION, NOT Constraint_Error" ) ;
  end;

  begin
    PUT_LINE ( "*** CALLING INVERSE WITH NON SQUARE MATRIX " ) ;
    A2I := INVERSE_JS ( A3 ) ;
    PUT_LINE ( " WRONG, INVERSE DID NOT RAISE Constraint_Error " ) ;
  exception
    when Constraint_Error =>
      PUT_LINE ( " CORRECTLY RAISED Constraint_Error " ) ;
    when others =>
      PUT_LINE ( " WRONG, RAISED SOME EXCEPTION, NOT Constraint_Error " ) ;
  end;
  NEW_LINE ;

  PUT_LINE ( "CHECK DETERMINANT" ) ;
  PUT_LINE ( "DETERMINANT OF A" ) ;
  D := DETERMINANT_JS ( A ) ;
  PUT ( D ) ;
  PUT_LINE ( " = D" ) ;
  NEW_LINE ;
  PUT_LINE ( "DETERMINANT OF A2, WHICH IS SINGULAR" ) ;
  D := DETERMINANT_JS ( A2 ) ;
  PUT ( D ) ;
  PUT_LINE ( " = D" ) ;
  PUT_LINE ( "DONE DETERMINANT" ) ;
  NEW_LINE ;

  PUT_LINE ( "*** CHECK CHOLESKY DECOMPOSITION AND SOLVE" ) ;
  PUT_LINE ( "L to start with" ) ;
  Put_rect ( L ) ;
  AC := L * TRANSPOSE(L); -- A must be symmetric
  PUT_LINE ( "A := L L^T" ) ;
  Put_rect ( AC ) ;
  L := CHOLESKY_DECOMPOSITION ( AC ) ;
  PUT_LINE ( "CHOLESKY L =" ) ;
  Put_rect ( L ) ;
  PUT_LINE ( "Y =" ) ;
  PUT ( Y ) ;
  X := CHOLESKY_SOLVE ( L , Y ) ;
  PUT_LINE ( "CHOLESKY X solution =" ) ;
  PUT ( X ) ;
  PUT_LINE ( "CHOLESKY zero check (A X - Y) =" ) ;
  PUT ( AC * X - Y ) ;
  NEW_LINE ;

  PUT_LINE ( "*** CHECK LU DECOMPOSITION AND SOLVE" ) ;
  begin
    PUT_LINE ( "INITIAL MATRIX  A" ) ;
    PUT ( A ) ;
    Put_rect(A);
    LU_DECOMPOSITION ( A , L , U , P ) ;
    PUT_LINE ( "LU DECOMPOSITION L =" ) ;
    PUT ( L ) ;
    Put_rect(L);
    PUT_LINE ( "LU DECOMPOSITION U =" ) ;
    PUT ( U ) ;
    Put_rect(U);
    PUT_LINE ( "LU DECOMPOSITION P =" ) ;
    PUT ( P ) ;
    PUT_LINE ( "INITIAL VECTOR Y" ) ;
    PUT ( Y ) ;
    X := LU_SOLVE ( L , U , P , Y ) ;
    PUT_LINE ( "LU DECOMPOSITION SOLVED FOR X =" ) ;
    PUT ( X ) ;
    PUT_LINE ( "LU SOLVE CHECK ZERO (A X - Y) =" ) ;
    PUT ( A * X - Y ) ;
    NEW_LINE ;
  exception
    when others =>
      PUT_LINE ( "LU DECOMPOSITION RAISED EXCEPTION" ) ;
  end ;

  PUT_LINE ( "*** CHECK QR DECOMPOSITION AND SOLVE" ) ;
    PUT_LINE ( "INITIAL MATRIX  A =" ) ;
    PUT ( A ) ;
    Put_rect(A);
    QR_DECOMPOSITION ( A , Q , R ) ;
    PUT_LINE ( "QR DECOMPOSITION Q =" ) ;
    PUT ( Q ) ;
    Put_rect(Q);
    PUT_LINE ( "QR DECOMPOSITION R =" ) ;
    PUT ( R ) ;
    Put_rect(R);
    RI := INVERSE_JS ( R ) ;
    PUT_LINE ( "R INVERSE" ) ;
    PUT ( RI ) ;
    QQ := A * RI ;
    PUT_LINE ( "QQ THIS SHOULD BE THE Q, COMPUTED BY A * INVERSE(R)" ) ;
    PUT ( QQ ) ;
    Put_rect(QQ);
    PUT_LINE ( "INITIAL VECTOR Y" ) ;
    PUT ( Y ) ;
    X := QR_SOLVE ( QQ , R , Y ) ;
    PUT_LINE ( "QR DECOMPOSITION SOLVED FOR X =" ) ;
    PUT ( X ) ;
    PUT_LINE ( "QR SOLVE CHECK ZERO (A X - Y) " ) ;
    PUT ( A * X - Y ) ;
    NEW_LINE ;

  PUT_LINE ( "end TEST_REAL_LINEAR_EQUATIONS" ) ;
end TEST_GENERIC_REAL_LINEAR_EQUATIONS ;
