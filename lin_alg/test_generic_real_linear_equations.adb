-- Author: Jon Squire
-- Original code here:
--   http://www.cs.umbc.edu/~squire/adaclass/gnatmath95/

-- test_generic_real_linear_equations.adb

--         TEST PROGRAM TO EXERCIZE  GENERIC REAL LINEAR EQUATIONS
--
--
-- THIS TEST:  SETS UP AN INITIAL COEFFICIENT MATRIX
--             SETS UP THE DEPENDENT VECTOR
--             SOLVES THE LINEAR EQUATIONS
--             RECOMPUTES THE DEPENDENT VECTOR
--
with Real_Arrays_IO;
with Generic_Real_Linear_Equations;
with Integer_Arrays; use Integer_Arrays;
with Integer_Arrays_IO;
with Ada.Text_IO ; use Ada.Text_IO ;
with Ada.Numerics.Generic_Real_Arrays;

procedure Test_Generic_Real_Linear_Equations is

  type Real is digits 12 ;

  package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays(Real);
  use Real_Arrays;

  package REAL_IO is new REAL_ARRAYS_IO( Real, Real_Arrays ) ;
  use REAL_IO ;
  package INT_IO is new INTEGER_ARRAYS_IO( Integer, Integer_Arrays ) ;
  use INT_IO ;

  package Real_Linear_Equations is new Generic_Real_Linear_Equations
            ( Real, Real_Arrays, Integer_Vector ) ;
  use Real_Linear_Equations;


--                        SPECIFICATIONS
  A : REAL_MATRIX ( 1 .. 4 , 1 .. 4 ) ;
  AC : REAL_MATRIX ( 1 .. 4 , 1 .. 4 ) ;
  AI : REAL_MATRIX ( 1 .. 4 , 1 .. 4 ) ;
  AII : REAL_MATRIX ( 1 .. 4 , 1 .. 4 ) ;
  Y : constant REAL_VECTOR ( 1 .. 4 ) := ( 30.0 , 31.0 , 34.0 , 40.0 ) ;
  X : REAL_VECTOR ( 1 .. 4 ) ;
  VECTOR_RESULT : REAL_VECTOR ( 1 .. 4 ) ;
  XX : REAL_MATRIX ( 1 .. 4 , 1 .. 2 );
  YY : constant REAL_MATRIX ( 1 .. 4 , 1 .. 2 ) := (( 30.0, 15.0), (31.0, 15.5),
                                                    ( 34.0, 17.0), (40.0, 20.0) );
  MATRIX_RESULT : REAL_MATRIX ( 1 .. 4 , 1 .. 2 ) ;

  A2 : constant REAL_MATRIX ( - 1 .. 1 , 0 .. 2 ) := (( 1.0 , 2.0 , 3.0 ) , ( 4.0 , 5.0
     , 6.0 ) , ( 4.0 , 4.0 , 4.0 )) ;
  A3 : constant REAL_MATRIX ( - 1 .. 1 , 0 .. 1 ) := (( 1.0 , 2.0 ) , ( 4.0 , 5.0 ) ,
     ( 4.0 , 4.0 )) ;
  A2I : REAL_MATRIX ( 1 .. 3 , 4 .. 6 ) ;
  pragma Unreferenced (A2I);
  Y2 : constant REAL_VECTOR ( 0 .. 2 ) := ( 1.5 , 2.5 , 3.5 ) ;
  X2 : REAL_VECTOR ( 0 .. 2 ) ;
  pragma Unreferenced (X2);
  D : REAL ;
  L : REAL_MATRIX ( 1 .. 4 , 6 .. 9 ) := (others=>(others=>0.0));
  U : REAL_MATRIX ( -4 .. -1 , 0 .. 3 ) := (others=>(others=>0.0));
  P : INTEGER_VECTOR ( -7 .. -4 );
  Q : REAL_MATRIX ( 1 .. 4 , 6 .. 9 ) := (others=>(others=>0.0));
  QQ : REAL_MATRIX ( 1 .. 4 , 6 .. 9 ) := (others=>(others=>0.0));
  R : REAL_MATRIX ( -4 .. -1 , 0 .. 3 ) := (others=>(others=>0.0));
  RI : REAL_MATRIX ( -4 .. -1 , 0 .. 3 ) := (others=>(others=>0.0));

--                       ROUTINE ( USED LOCALLY )

  procedure MAT_INIT_1 ( B : out REAL_MATRIX ) is
  begin
    for I in B'FIRST ( 1 ) .. B'LAST ( 1 ) loop
      for J in B'FIRST ( 1 ) .. B'LAST ( 1 ) loop
        if I > J then
          B ( I , J ) := REAL ( I ) ;
        else
          B ( I , J ) := REAL ( J ) ;
        end if ;
      end loop ;
    end loop ;
  end MAT_INIT_1 ;
begin
  for I in L'RANGE(1) loop
    for J in L'RANGE(2) loop
      if I >= J-L'FIRST(2)+L'FIRST(1) then
        L(I,J) := REAL(I+J);
      end if;
    end loop;
  end loop;

  PUT_LINE ( "USING LINEAR_EQUATIONS, VECTOR" ) ;
  PUT_LINE ( "INITIAL MATRIX A" ) ;
  MAT_INIT_1 ( A ) ;
  PUT ( A ) ;
  PUT_LINE ( "INITIAL VECTOR Y" ) ;
  PUT ( Y ) ;
  PUT_LINE ( "SOLVING EQUATIONS" ) ;
  X := LINEAR_EQUATIONS ( A , Y ) ;
  PUT_LINE ( "X = SOLUTION" ) ;
  PUT ( X ) ;
  PUT_LINE ( "CHECK IF SOLUTION A*X GIVES BACK Y" ) ;
  VECTOR_RESULT := A * X ;
  PUT ( VECTOR_RESULT ) ;
  PUT_LINE ( "CHECK FOR ZERO VECTOR" ) ;
  VECTOR_RESULT := VECTOR_RESULT - Y ;
  PUT ( VECTOR_RESULT ) ;
  NEW_LINE ;

  PUT_LINE ( "USING LINEAR_EQUATIONS, MATRIX" ) ;
  PUT_LINE ( "INITIAL MATRIX A" ) ;
  MAT_INIT_1 ( A ) ;
  PUT ( A ) ;
  PUT_LINE ( "INITIAL MATRIX YY" ) ;
  PUT ( YY ) ;
  PUT_LINE ( "SOLVING EQUATIONS" ) ;
  XX := LINEAR_EQUATIONS ( A , YY ) ;
  PUT_LINE ( "XX = SOLUTION" ) ;
  PUT ( XX ) ;
  PUT_LINE ( "CHECK IF SOLUTION A*XX GIVES BACK YY" ) ;
  MATRIX_RESULT := A * XX ;
  PUT ( MATRIX_RESULT ) ;
  PUT_LINE ( "CHECK FOR ZERO MATRIX" ) ;
  MATRIX_RESULT := MATRIX_RESULT - YY ;
  PUT ( MATRIX_RESULT ) ;
  NEW_LINE ;

  PUT_LINE ( "CHECK USING CROUT_SOLVE" ) ;
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

  PUT_LINE ( "CHECK INVERSE" ) ;
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
  NEW_LINE ;

  PUT_LINE ( "CHECK FOR EXCEPTIONS IN LINEAR EQUATIONS" ) ;
  begin
    PUT_LINE ( "INITIAL MATRIX A2, singular" ) ;
    PUT ( A2 ) ;
    PUT_LINE ( "INITIAL VECTOR Y2" ) ;
    PUT ( Y2 ) ;
    PUT_LINE ( "SOLVING GENERALIZED EQUATIONS" ) ;
    X2 := LINEAR_EQUATIONS ( A2 , Y2 ) ;
    PUT_LINE ( "ERROR IF GET HERE" ) ;
    NEW_LINE ;
  exception
    when MATRIX_DATA_ERROR =>
      PUT_LINE( "MATRIX_DATA_ERROR CORRECTLY RAISED" ) ;
    when others =>
      PUT_LINE( "LINEAR EQUATIONS RAISED WRONG EXCEPTION" ) ;
  end ;

  PUT_LINE ( "CHECK FOR EXCEPTIONS IN INVERSE" ) ;
  begin
    PUT_LINE ( "A2" );
    PUT ( A2 ) ;
    A2I := INVERSE_JS ( A2 ) ;
    PUT_LINE ( "ERROR IF GET HERE" ) ;
    NEW_LINE ;
  exception
    when MATRIX_DATA_ERROR =>
      PUT_LINE( "MATRIX_DATA_ERROR CORRECTLY RAISED" ) ;
    when others =>
      PUT_LINE ( "INVERSE RAISED WRONG EXCEPTION" ) ;
  end ;

  begin
    PUT_LINE ( "CALLING LINEAR_EQUATIONS WITH NON SQUARE MATRIX" ) ;
    X2 := LINEAR_EQUATIONS ( A3 , Y2 ) ;
    PUT_LINE ( "WRONG, LINEAR_EQUATIONS DID NOT RAISE MATRIX_DATA_ERROR" ) ;
  exception
    when MATRIX_DATA_ERROR =>
      PUT_LINE ( "CORRECTLY RAISED MATRIX_DATA_ERROR" ) ;
    when others =>
      PUT_LINE ( "WRONG, RAISED SOME EXCEPTION, NOT MATRIX_DATA_ERROR" ) ;
  end;

  begin
    PUT_LINE ( " CALLING INVERSE WITH NON SQUARE MATRIX " ) ;
    A2I := INVERSE_JS ( A3 ) ;
    PUT_LINE ( " WRONG, INVERSE DID NOT RAISE MATRIX_DATA_ERROR " ) ;
  exception
    when MATRIX_DATA_ERROR =>
      PUT_LINE ( " CORRECTLY RAISED MATRIX_DATA_ERROR " ) ;
    when others =>
      PUT_LINE ( " WRONG, RAISED SOME EXCEPTION, NOT MATRIX_DATA_ERROR " ) ;
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

  PUT_LINE ( "CHECK CHOLESKY DECOMPOSITION AND SOLVE" ) ;
  PUT_LINE ( "CHOLESKY L to start with" ) ;
  PUT ( L ) ;
  AC := L * TRANSPOSE(L); -- A must be symmetric
  PUT_LINE ( "CHOLESKY new A=" ) ;
  PUT ( AC ) ;
  L := CHOLESKY_DECOMPOSITION ( AC ) ;
  PUT_LINE ( "CHOLESKY L =" ) ;
  PUT ( L ) ;
  PUT_LINE ( "CHOLESKY Y =" ) ;
  PUT ( Y ) ;
  X := CHOLESKY_SOLVE ( L , Y ) ;
  PUT_LINE ( "CHOLESKY X solution =" ) ;
  PUT ( X ) ;
  PUT_LINE ( "CHOLESKY zero check =" ) ;
  PUT ( AC * X - Y ) ;
  NEW_LINE ;

  PUT_LINE ( "CHECK LU DECOMPOSITION AND SOLVE" ) ;
  begin
    PUT_LINE ( "INITIAL MATRIX  A" ) ;
    PUT ( A ) ;
    LU_DECOMPOSITION ( A , L , U , P ) ;
    PUT_LINE ( "LU DECOMPOSITION L =" ) ;
    PUT ( L ) ;
    PUT_LINE ( "LU DECOMPOSITION U =" ) ;
    PUT ( U ) ;
    PUT_LINE ( "LU DECOMPOSITION P =" ) ;
    PUT ( P ) ;
    PUT_LINE ( "INITIAL VECTOR Y" ) ;
    PUT ( Y ) ;
    X := LU_SOLVE ( L , U , P , Y ) ;
    PUT_LINE ( "LU DECOMPOSITION SOLVED FOR X =" ) ;
    PUT ( X ) ;
    PUT_LINE ( "LU SOLVE CHECK ZERO" ) ;
    PUT ( A * X - Y ) ;
    NEW_LINE ;
  exception
    when others =>
      PUT_LINE ( "LU DECOMPOSITION RAISED EXCEPTION" ) ;
  end ;

  PUT_LINE ( "CHECK QR DECOMPOSITION AND SOLVE" ) ;
    PUT_LINE ( "INITIAL MATRIX  A" ) ;
    PUT ( A ) ;
    QR_DECOMPOSITION ( A , Q , R ) ;
    PUT_LINE ( "QR DECOMPOSITION Q =" ) ;
    PUT ( Q ) ;
    PUT_LINE ( "QR DECOMPOSITION R =" ) ;
    PUT ( R ) ;
    RI := INVERSE ( R ) ;
    PUT_LINE ( "R INVERSE" ) ;
    PUT ( RI ) ;
    QQ := A * RI ;
    PUT_LINE ( "QQ THIS SHOUD BE THE Q, COMPUTED BY A * INVERSE(R)" ) ;
    PUT ( QQ ) ;
    PUT_LINE ( "INITIAL VECTOR Y" ) ;
    PUT ( Y ) ;
    X := QR_SOLVE ( QQ , R , Y ) ;
    PUT_LINE ( "QR DECOMPOSITION SOLVED FOR X =" ) ;
    PUT ( X ) ;
    PUT_LINE ( "QR SOLVE CHECK ZERO" ) ;
    PUT ( A * X - Y ) ;
    NEW_LINE ;

  PUT_LINE ( "end TEST_REAL_LINEAR_EQUATIONS" ) ;
end TEST_GENERIC_REAL_LINEAR_EQUATIONS ;
