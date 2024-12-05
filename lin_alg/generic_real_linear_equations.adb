with Ada.Text_IO;  -- for debug
--  with INTEGER_ARRAYS_IO;  -- for debug
--  with REAL_ARRAYS_IO; -- for debug
with Ada.Numerics.Generic_Elementary_Functions;

package body Generic_Real_Linear_Equations is

  function Almost_Zero (x : Real) return Boolean is
  begin
    return abs x <= Real'Base'Model_Small;
  end Almost_Zero;

  package Elementary_Functions is new
    Ada.Numerics.Generic_Elementary_Functions (Real);

--  package REAL_IO is new REAL_ARRAYS_IO
--                            (REAL, Real_Arrays); -- for debug
--  use REAL_IO; -- for debug
--  package INT_IO is new INTEGER_ARRAYS_IO
--                            (Integer, Integer_Arrays); -- for debug
--  use INT_IO; -- for debug

  function Solve_JS (A : Real_Matrix;
                     Y : Real_Vector) return Real_Vector is

    --      PURPOSE : SOLVE THE LINEAR SYSTEM OF EQUATIONS WITH REAL
    --                COEFFICIENTS   [A] * |X| = |Y|
    --
    --      INPUT  : THE REAL MATRIX  A
    --               THE REAL VECTOR  Y
    --
    --      OUTPUT : THE REAL VECTOR  X
    --
    --      METHOD : GAUSS-JORDAN ELIMINATION USING MAXIMUM ELEMENT
    --               FOR PIVOT.
    --
    --      EXCEPTION : MATRIX_DATA_ERROR IF 'A' IS SINGULAR
    --                  ARRAY_INDEX_ERROR IF 'A' IS NOT SQUARE OR
    --                                       LENGTH OF 'Y' /= ROWS OF 'A'
    --
    --      USAGE  :     X := LINEAR_EQUATIONS ( A , Y ) ;
    --
    --      WRITTEN BY : JON SQUIRE , 28 MAY 1983 (Yes, its that old!)
    --      revised 10/8/88 for nested generics
    --      revised 8/8/90 for ISO NRG proposed standard packages interface

    N : constant Integer := A'Length (1);  --  NUMBER OF EQUATIONS
    X : Real_Vector (1 .. N);              --  RESULT BEING COMPUTED
    B : Real_Matrix (1 .. N, 1 .. N + 1);  --  WORKING MATRIX
    ROW : array (1 .. N) of Integer;       --  ROW INTERCHANGE INDICES
    HOLD, I_PIVOT : Integer;               --  PIVOT INDICIES
    PIVOT : Real;                          --  PIVOT ELEMENT VALUE
    ABS_PIVOT : Real;                      --  ABS OF PIVOT ELEMENT
    NORM1 : Real := 0.0;                   --  1 NORM OF MATRIX
  begin
    if A'Length (1) /= A'Length (2) then
      raise Constraint_Error with "Matrix A is not square";
    end if;
    if A'Length (1) /= Y'Length then
      raise Constraint_Error with "Matrix A row count is different than vector Y's";
    end if;

    --                               BUILD WORKING DATA STRUCTURE
    for I in 1 .. N loop
      for J in 1 .. N loop
        B (I, J) := A (I - 1 + A'First (1), J - 1 + A'First (2));
        if abs B (I, J) > NORM1 then
          NORM1 := abs B (I, J);
        end if;
      end loop;
      B (I, N + 1) := Y (I - 1 + Y'First);
    end loop;

    --                               SET UP ROW  INTERCHANGE VECTORS
    for K in 1 .. N loop
      ROW (K) := K;
    end loop;

    --                               BEGIN MAIN REDUCTION LOOP
    for K in 1 .. N loop

      --                             FIND LARGEST ELEMENT FOR PIVOT
      PIVOT := B (ROW (K), K);
      ABS_PIVOT := abs (PIVOT);
      I_PIVOT := K;
      for I in K .. N loop
        if abs (B (ROW (I), K)) > ABS_PIVOT then
          I_PIVOT := I;
          PIVOT := B (ROW (I), K);
          ABS_PIVOT := abs (PIVOT);
        end if;
      end loop;

      --                             CHECK FOR NEAR SINGULAR
      if ABS_PIVOT < Real'Model_Epsilon * NORM1 then
        raise Matrix_Data_Error with "Matrix is near singular";
      end if;

      --                             HAVE PIVOT, INTERCHANGE ROW POINTERS
      HOLD := ROW ( K ) ;
      ROW ( K ) := ROW ( I_PIVOT ) ;
      ROW ( I_PIVOT ) := HOLD ;

      --                           REDUCE ABOUT PIVOT
      for J in K + 1 .. N + 1 loop
        B ( ROW( K ) , J) := B ( ROW( K ) , J) / B ( ROW( K ) , K) ;
      end loop ;

      --                           INNER REDUCTION LOOP
      for I in 1 .. N loop
        if I /= K then
          for J in K + 1 .. N + 1 loop
            B ( ROW( I ) , J) := B ( ROW( I ) , J) - B ( ROW( I ) , K) * B (
               ROW( K ) , J) ;
          end loop ;
        end if ;
      end loop ;

      --                             FINISHED INNER REDUCTION
    end loop ;

    --                               BUILD  X  FOR RETURN, UNSCRAMBLING ROWS
    for I in 1 .. N loop
      X ( I ) := B ( ROW( I ) , N + 1) ;
    end loop ;
    return X ;
  end Solve_JS;

  function Solve_JS (A : Real_Matrix ;
                     Y : Real_Matrix) return Real_Matrix is

    --      PURPOSE : SOLVE THE LINEAR SYSTEM OF EQUATIONS WITH REAL
    --                COEFFICIENTS   [A] * [X] = [Y]
    --
    --      INPUT  : THE REAL MATRIX  A
    --               THE REAL MATRIX  Y
    --
    --      OUTPUT : THE REAL MATRIX  X
    --
    --      METHOD : GAUSS-JORDAN ELIMINATION USING MAXIMUM ELEMENT
    --               FOR PIVOT.
    --
    --      USAGE  :     X := LINEAR_EQUATIONS ( A , Y ) ;
    --
    --      EXCEPTION : MATRIX_DATA_ERROR WILL BE RAISED IF 'A' IS SINGULAR
    --                  Constraint_Error IF 'A' IS NOT SQUARE OR
    --                                       ROWS OF 'Y' /= ROWS OF 'A'

    N : constant Integer := A'Length(1) ;      -- NUMBER OF EQUATIONS
    M : constant Integer := Y'Length(2) ;      -- NUMBER OF SOLUTIONS REQUESTED
    X : Real_Matrix ( 1 .. N , 1 .. M ) ;      -- RESULT BEING COMPUTED
    B : Real_Matrix ( 1 .. N , 1 .. N + M ) ;  -- WORKING MATRIX
    ROW : array ( 1 .. N ) of Integer ;        -- ROW INTERCHANGE INDICIES
    HOLD , I_PIVOT : Integer ;                 -- PIVOT INDICIES
    PIVOT : Real ;                             -- PIVOT ELEMENT VALUE
    ABS_PIVOT : Real ;                         -- ABS OF PIVOT ELEMENT
    NORM1 : Real := 0.0 ;                      -- 1 NORM OF MATRIX
  begin
    if A'Length ( 1 ) /= A'Length ( 2 ) then
      raise Constraint_Error with "Matrix A is not square";
    end if ;
    if A'Length ( 1 ) /= Y'Length ( 1 ) then
      raise Constraint_Error with "Matrix A row count is different than matrix Y's";
    end if ;

    --                               BUILD WORKING DATA STRUCTURE
    for I in 1 .. N loop
      for J in 1 .. N loop
        B ( I , J ) := A ( I - 1 + A'First( 1 ) , J - 1 + A'First ( 2 )) ;
        if abs B ( I , J ) > NORM1 then
          NORM1 := abs B ( I , J ) ;
        end if ;
      end loop ;
      for J in 1 .. M loop
        B ( I , N + J ) := Y ( I - 1 + Y'First(1), J - 1 + Y'First(2) ) ;
      end loop ;
    end loop ;

    --                               SET UP ROW  INTERCHANGE VECTORS
    for K in 1 .. N loop
      ROW ( K ) := K ;
    end loop ;

    --                               BEGIN MAIN REDUCTION LOOP
    for K in 1 .. N loop

      --                             FIND LARGEST ELEMENT FOR PIVOT
      PIVOT := B ( ROW( K ) , K) ;
      ABS_PIVOT := abs ( PIVOT ) ;
      I_PIVOT := K ;
      for I in K .. N loop
        if abs ( B( ROW( I ) , K)) > ABS_PIVOT then
          I_PIVOT := I ;
          PIVOT := B ( ROW( I ) , K) ;
          ABS_PIVOT := abs ( PIVOT ) ;
        end if ;
      end loop ;

      --                             CHECK FOR NEAR SINGULAR
      if ABS_PIVOT < Real'Model_Epsilon * NORM1 then
        raise Matrix_Data_Error with "Matrix is near singular";
      end if;

      --                             HAVE PIVOT, INTERCHANGE ROW POINTERS
      HOLD := ROW ( K ) ;
      ROW ( K ) := ROW ( I_PIVOT ) ;
      ROW ( I_PIVOT ) := HOLD ;

      --                             REDUCE ABOUT PIVOT
      for J in K + 1 .. N + M loop
        B ( ROW( K ) , J) := B ( ROW( K ) , J) / B ( ROW( K ) , K) ;
      end loop ;

      --                             INNER REDUCTION LOOP
      for I in 1 .. N loop
        if I /= K then
          for J in K + 1 .. N + M loop
            B ( ROW( I ) , J) := B ( ROW( I ) , J) - B ( ROW( I ) , K) * B (
               ROW( K ) , J) ;
          end loop ;
        end if ;
      end loop ;
      --                               FINISHED INNER REDUCTION
    end loop ;

    --                               BUILD  X  FOR RETURN, UNSCRAMBLING ROWS
    for I in 1 .. N loop
      for J in 1 .. M loop
        X ( I, J ) := B ( ROW( I ) , N + J) ;
      end loop ;
    end loop ;
    return X ;
  end Solve_JS;

  function Determinant_JS ( A : Real_Matrix ) return Real is

    --      PURPOSE : COMPUTE THE DETERMINANT OF A MATRIX
    --
    --      INPUT  : THE REAL MATRIX  A
    --
    --      OUTPUT : THE REAL VALUE   D
    --
    --      METHOD : GAUSS-JORDAN ELIMINATION USING MAXIMUM ELEMENT
    --               FOR PIVOT.
    --
    --      USAGE  :     D := DETERMINANT ( A ) ;
    --
    --      EXCEPTIONS : Constraint_Error IF THE MATRIX IS NOT SQUARE
    --
    --      WRITTEN BY : JON SQUIRE , 28 MAY 1983

    N : constant Integer := A'Length(1) ;  -- NUMBER OF ROWS
    D : Real := 1.0 ;                      -- DETERMINANT
    B : Real_Matrix ( 1 .. N , 1 .. N ) ;  -- WORKING MATRIX
    ROW : array ( 1 .. N ) of Integer ;    -- ROW INTERCHANGE INDICIES
    HOLD , I_PIVOT : Integer ;             -- PIVOT INDICIES
    PIVOT : Real ;                         -- PIVOT ELEMENT VALUE
    ABS_PIVOT : Real ;                     -- ABS OF PIVOT ELEMENT
  begin
    if A'Length ( 1 ) /= A'Length ( 2 ) then
      raise Constraint_Error with "Matrix A is not square";
    end if ;
    B := A ;

    --                                 SET UP ROW  INTERCHANGE VECTORS
    for K in 1 .. N loop
      ROW ( K ) := K ;
    end loop ;

    --                                 BEGIN MAIN REDUCTION LOOP
    for K in 1 .. N - 1 loop

      --                               FIND LARGEST ELEMENT FOR PIVOT
      PIVOT := B ( ROW( K ) , K) ;
      ABS_PIVOT := abs PIVOT ;
      I_PIVOT := K ;
      for I in K+1 .. N loop
        if abs ( B( ROW( I ) , K)) > ABS_PIVOT then
          I_PIVOT := I ;
          PIVOT := B ( ROW( I ) , K) ;
          ABS_PIVOT := abs PIVOT ;
        end if ;
      end loop ;

      --                               HAVE PIVOT, INTERCHANGE ROW POINTERS
      if I_PIVOT /= K then
        HOLD := ROW ( K ) ;
        ROW ( K ) := ROW ( I_PIVOT ) ;
        ROW ( I_PIVOT ) := HOLD ;
        D := - D ;
      end if ;

      D := D * PIVOT ; -- ACCUMULATE DETERMINANT, MAY UNDERFLOW

      --                               REDUCE ABOUT PIVOT
      for J in K + 1 .. N loop
        B ( ROW( K ) , J) := B ( ROW( K ) , J) / B ( ROW( K ) , K) ;
      end loop ;

      --                               INNER REDUCTION LOOP
      for I in 1 .. N loop
        if I /= K then
          for J in K + 1 .. N loop
            B ( ROW( I ) , J) := B ( ROW( I ) , J) - B ( ROW( I ) , K) *
                                 B ( ROW( K ) , J) ;
          end loop ;
        end if ;
      end loop ;

      --                               FINISHED INNER REDUCTION
    end loop ;
    return D * B ( ROW( N ) , N) ;
  exception
    when Constraint_Error => -- catches overflow
      return 0.0 ;
  end Determinant_JS ;

  function Inverse_JS ( A : Real_Matrix ) return Real_Matrix is

    --      PURPOSE : INVERT AN N BY N MATRIX
    --
    --      INPUT : THE MATRIX  A
    --
    --      OUTPUT : THE INVERSE OF MATRIX  A
    --
    --      METHOD : GAUSS-JORDAN ELIMINATION USING MAXIMUM ELEMENT
    --               FOR PIVOT.
    --
    --      EXCEPTION : MATRIX_DATA_ERROR RAISED IF INVERSE CAN NOT BE COMPUTED
    --                  Constraint_Error IF 'A' IS NOT SQUARE
    --
    --      SAMPLE USE :  NEW_MATRIX := INVERSE ( OLD_MATRIX ) ;
    --                    MATRIX := INVERSE ( MATRIX ) * ANOTHER_MATRIX ;
    --
    --      WRITTEN BY : JON SQUIRE , 3 FEB 1983

    N : constant Integer := A'Length ;        -- SIZE OF MATRIX
    AA : Real_Matrix ( 1 .. N , 1 .. N ) ;    -- WORKING MATRIX
    TEMP : Real_Vector ( 1 .. N ) ;           -- TEMPORARY FOR UNSCRAMBLING ROWS
    ROW , COL : array ( 1 .. N ) of Integer ; -- ROW,COLUMN INTERCHANGE INDICIES
    HOLD , I_PIVOT , J_PIVOT : Integer ;      -- PIVOT INDICIES
    PIVOT : Real ;                            -- PIVOT ELEMENT VALUE
    ABS_PIVOT : Real ;                        -- ABS OF PIVOT ELEMENT
    NORM1 : Real := 0.0 ;                     -- 1 NORM OF MATRIX
  begin
    if A'Length ( 1 ) /= A'Length ( 2 ) then
      raise Constraint_Error with "Matrix A is not square";
    end if ;

    --                              BUILD WORKING DATA STRUCTURE
    AA := A ;
    for I in 1 .. N loop
      for J in 1 .. N loop
        if abs AA ( I , J ) > NORM1 then
          NORM1 := abs AA ( I , J ) ;
        end if ;
      end loop ;
    end loop ;

    --                              SET UP ROW AND COLUMN INTERCHANGE VECTORS
    for K in 1 .. N loop
      ROW ( K ) := K ;
      COL ( K ) := K ;
    end loop ;

    --                              BEGIN MAIN REDUCTION LOOP
    for K in 1 .. N loop

      --                            FIND LARGEST ELEMENT FOR PIVOT
      PIVOT := AA ( ROW( K ) , COL ( K )) ;
      I_PIVOT := K ;
      J_PIVOT := K ;
      for I in K .. N loop
        for J in K .. N loop
          ABS_PIVOT := abs ( PIVOT ) ;
          if abs ( AA( ROW( I ) , COL ( J ))) > ABS_PIVOT then
            I_PIVOT := I ;
            J_PIVOT := J ;
            PIVOT := AA ( ROW( I ) , COL ( J )) ;
          end if ;
        end loop ;
      end loop ;

      --                            TEST FOR SINGULAR
      if ABS_PIVOT < Real'Model_Epsilon * NORM1 then
        raise Matrix_Data_Error with "Matrix is singular";
      end if ;

      HOLD := ROW ( K ) ;
      ROW ( K ) := ROW ( I_PIVOT ) ;
      ROW ( I_PIVOT ) := HOLD ;
      HOLD := COL ( K ) ;
      COL ( K ) := COL ( J_PIVOT ) ;
      COL ( J_PIVOT ) := HOLD ;

      --                            REDUCE ABOUT PIVOT
      AA ( ROW( K ) , COL ( K )) := 1.0 / PIVOT ;
      for J in 1 .. N loop
        if J /= K then
          AA ( ROW( K ) , COL ( J )) := AA ( ROW( K ) , COL ( J )) * AA ( ROW
            ( K ) , COL ( K )) ;
        end if ;
      end loop ;

      --                            INNER REDUCTION LOOP
      for I in 1 .. N loop
        if K /= I then
          for J in 1 .. N loop
            if K /= J then
              AA ( ROW( I ) , COL ( J )) := AA ( ROW( I ) , COL ( J )) - AA (
                 ROW( I ) , COL ( K )) * AA ( ROW( K ) , COL ( J )) ;
            end if ;
          end loop ;
          AA ( ROW( I ) , COL ( K )) := - AA ( ROW( I ) , COL ( K )) * AA (
             ROW( K ) , COL ( K )) ;
        end if ;
      end loop ;

      --                            FINISHED INNER REDUCTION
    end loop ;

    --                              UNSCRAMBLE ROWS
    for J in 1 .. N loop
      for I in 1 .. N loop
        TEMP ( COL( I )) := AA ( ROW( I ) , J) ;
      end loop ;
      for I in 1 .. N loop
        AA ( I , J ) := TEMP ( I ) ;
      end loop ;
    end loop ;

    --                              UNSCRAMBLE COLUMNS
    for I in 1 .. N loop
      for J in 1 .. N loop
        TEMP ( ROW( J )) := AA ( I , COL( J )) ;
      end loop ;
      for J in 1 .. N loop
        AA ( I , J ) := TEMP ( J ) ;
      end loop ;
    end loop ;
    return AA ;
  end Inverse_JS ;

  procedure Inverse_JS ( A : in out Real_Matrix ) is

    --      PURPOSE : INVERT AN N BY N MATRIX IN PLACE
    --
    --      INPUT : THE MATRIX  A
    --
    --      OUTPUT : THE INVERSE OF MATRIX  A, IN PLACE
    --
    --      METHOD : GAUSS-JORDAN ELIMINATION USING MAXIMUM ELEMENT
    --               FOR PIVOT.
    --
    --      EXCEPTION : MATRIX_DATA_ERROR RAISED IF INVERSE CAN NOT BE COMPUTED
    --                  Constraint_Error RAISED IF MATRIX IS NOT SQUARE
    --
    --      SAMPLE USE :  INVERSE ( SOME_MATRIX ) ;

    N : constant Integer := A'Length ;        -- SIZE OF MATRIX
    AA : Real_Matrix ( 1 .. N , 1 .. N ) ;    -- WORKING MATRIX
    ROW , COL : array ( 1 .. N ) of Integer ; -- ROW,COLUMN INTERCHANGE INDICIES
    TEMP : Real_Vector ( 1 .. N ) ;           -- TEMP ARRAY FOR UNSCRAMBLING
    HOLD , I_PIVOT , J_PIVOT : Integer ;      -- PIVOT INDICIES
    PIVOT : Real ;  -- PIVOT ELEMENT VALUE    -- PIVOT ELEMENT
    ABS_PIVOT : Real ;                        -- ABS OF PIVOT ELEMENT
    NORM1 : Real := 0.0 ;                     -- 1 NORM OF MATRIX
  begin
    if A'Length ( 1 ) /= A'Length ( 2 ) then
      raise Constraint_Error with "Matrix A is not square";
    end if ;

    --                              BUILD WORKING DATA STRUCTURE
    AA := A ;
    for I in 1 .. N loop
      for J in 1 .. N loop
        if abs AA ( I , J ) > NORM1 then
          NORM1 := abs AA ( I , J ) ;
        end if ;
      end loop ;
    end loop ;

    --                              SET UP ROW AND COLUMN INTERCHANGE VECTORS
    for K in 1 .. N loop
      ROW ( K ) := K ;
      COL ( K ) := K ;
    end loop ;

    --                              BEGIN MAIN REDUCTION LOOP
    for K in 1 .. N loop

      --                            FIND LARGEST ELEMENT FOR PIVOT
      PIVOT := AA ( ROW( K ) , COL ( K )) ;
      I_PIVOT := K ;
      J_PIVOT := K ;
      for I in K .. N loop
        for J in K .. N loop
          ABS_PIVOT := abs ( PIVOT ) ;
          if abs ( AA( ROW( I ) , COL ( J ))) > ABS_PIVOT then
            I_PIVOT := I ;
            J_PIVOT := J ;
            PIVOT := AA ( ROW( I ) , COL ( J )) ;
          end if ;
        end loop ;
      end loop ;

      --                            CHECK FOR SINGULAR
      if ABS_PIVOT < Real'Model_Epsilon * NORM1 then
        raise Matrix_Data_Error;
      end if ;

      HOLD := ROW ( K ) ;
      ROW ( K ) := ROW ( I_PIVOT ) ;
      ROW ( I_PIVOT ) := HOLD ;
      HOLD := COL ( K ) ;
      COL ( K ) := COL ( J_PIVOT ) ;
      COL ( J_PIVOT ) := HOLD ;

      --                                REDUCE ABOUT PIVOT
      AA ( ROW( K ) , COL ( K )) := 1.0 / PIVOT ;
      for J in 1 .. N loop
        if J /= K then
          AA ( ROW( K ) , COL ( J )) := AA ( ROW( K ) , COL ( J )) * AA ( ROW
            ( K ) , COL ( K )) ;
        end if ;
      end loop ;

      --                            INNER REDUCTION LOOP
      for I in 1 .. N loop
        if K /= I then
          for J in 1 .. N loop
            if K /= J then
              AA ( ROW( I ) , COL ( J )) := AA ( ROW( I ) , COL ( J )) - AA (
                 ROW( I ) , COL ( K )) * AA ( ROW( K ) , COL ( J )) ;
            end if ;
          end loop ;
          AA ( ROW( I ) , COL ( K )) := - AA ( ROW( I ) , COL ( K )) * AA (
             ROW( K ) , COL ( K )) ;
        end if ;
      end loop ;

      --                            FINISHED INNER REDUCTION
    end loop ;

    --                              UNSCRAMBLE ROWS
    for J in 1 .. N loop
      for I in 1 .. N loop
        TEMP ( COL( I )) := AA ( ROW( I ) , J) ;
      end loop ;
      for I in 1 .. N loop
        AA ( I , J ) := TEMP ( I ) ;
      end loop ;
    end loop ;

    --                              UNSCRAMBLE COLUMNS
    for I in 1 .. N loop
      for J in 1 .. N loop
        TEMP ( ROW( J )) := AA ( I , COL( J )) ;
      end loop ;
      for J in 1 .. N loop
        AA ( I , J ) := TEMP ( J ) ;
      end loop ;
    end loop ;
    A := AA ;
  end Inverse_JS ;

  function Crout_Solve ( A : Real_Matrix ;
                         Y : Real_Vector ) return Real_Vector is

    --      PURPOSE : SOLVE THE LINEAR SYSTEM OF EQUATIONS WITH REAL
    --                COEFFICIENTS   [A] * |X| = |Y|
    --
    --      INPUT  : THE REAL MATRIX  A
    --               THE REAL VECTOR  Y
    --
    --      OUTPUT : THE REAL VECTOR  X
    --
    --      METHOD : CROUT REDUCTION AND BACK SUBSTITUTION USING
    --               MAXIMUM ELEMENT FOR DIAGONAL
    --
    --      USAGE  :     X := CROUT_SOLVE ( A , Y ) ;
    --
    --      EXCEPTION : Constraint_Error  RASIED IF 'A' NOT SQUARE OR
    --                                     Y LENGTH /= ROWS OF 'A'
    --                  MATRIX_DATA_ERROR  RAISED IF 'A' SINGULAR
    --
    --      WRITTEN BY : JON SQUIRE , 11/30/88

    N : constant Integer := A'Length ;         -- NUMBER OF EQUATIONS
    X : Real_Vector ( 1 .. N ) ;               -- RESULT BEING COMPUTED
    Z : Real_Vector ( 1 .. N ) ;               -- TEMPORARY IN BACK SUB
    YY : Real_Vector ( 1 .. N ) ;              -- TEMPORARY IN BACK SUB
    B : Real_Matrix ( 1 .. N , 1 .. N ) ;      -- WORKING MATRIX
    ROW : Integer_Vector( 1 .. N ) ;           -- ROW INTERCHANGE INDICIES
    HOLD , I_PIVOT : Integer ;                 -- PIVOT INDICIES
    PIVOT : Real ;                             -- PIVOT ELEMENT VALUE
    ABS_PIVOT : Real ;                         -- ABS OF PIVOT ELEMENT
    NORM1 : Real := 0.0 ;                      -- 1 NORM OF MATRIX
    SUM : Real ;                               -- TEMP VARIBLE
  begin
    if A'Length ( 1 ) /= A'Length ( 2 ) then
      raise Constraint_Error with "Matrix A is not square";
    end if ;
    if A'Length ( 1 ) /= Y'Length then
      raise Constraint_Error with "Matrix A row count is different than vector Y's";
    end if ;
    Z(1):= 0.0; -- calm down "may be referenced before it has a value" warning
    X(1):= 0.0; -- calm down "may be referenced before it has a value" warning

    --                        BUILD WORKING DATA STRUCTURE
    for I in 1 .. N loop
      for J in 1 .. N loop
        B ( I , J ) := A ( I - 1 + A'First( 1 ) , J - 1 + A'First ( 2 )) ;
        if abs B ( I , J ) > NORM1 then
          NORM1 := abs B ( I , J ) ;
        end if ;
      end loop ;
      YY ( I ) := Y ( I - 1 + Y'First ) ;
    end loop ;

    --                        SET UP ROW  INTERCHANGE VECTORS
    for K in 1 .. N loop
      ROW ( K ) := K ;
    end loop ;

    --                         BEGIN MAIN REDUCTION LOOP
    for J in 1..N loop

      --                             FIND LARGEST ELEMENT FOR PIVOT
      PIVOT := B ( ROW( J ) , J) ;
      ABS_PIVOT := abs ( PIVOT ) ;
      I_PIVOT := J ;
      for I in J+1 .. N loop
        if abs ( B( ROW( I ) , J)) > ABS_PIVOT then
          I_PIVOT := I ;
          PIVOT := B ( ROW( I ) , J) ;
          ABS_PIVOT := abs ( PIVOT ) ;
        end if ;
      end loop ;

      --                                CHECK FOR NEAR SINGULAR
      if ABS_PIVOT < Real'Model_Epsilon * NORM1 then  --  degeneration
        raise Matrix_Data_Error;
      end if ;

      --                                HAVE PIVOT, INTERCHANGE ROW POINTERS
      HOLD := ROW ( J ) ;
      ROW ( J ) := ROW ( I_PIVOT ) ;
      ROW ( I_PIVOT ) := HOLD ;

      -- below diagonal computations, alphas
      for I in 1..J loop
        SUM := 0.0 ;
        for K in 1..I-1 loop
          SUM := SUM + B(ROW(I),K)*B(ROW(K),J) ;
        end loop ;
        B(ROW(I),J) := B(ROW(I),J) - SUM ;
      end loop ;

      -- above diagonal computations, betas
      for I in J+1..N loop
        SUM := 0.0 ;
        for K in 1..J-1 loop
          SUM := SUM + B(ROW(I),K)*B(ROW(K),J) ;
        end loop ;
        B(ROW(I),J) := (B(ROW(I),J)-SUM)/B(ROW(J),J) ;
      end loop ;
      Ada.Text_IO.Put_Line ("finished col " & Integer'Image(J));
    end loop ;

    -- back substitute, first part
    for I in 1..N loop
      SUM := 0.0 ;
      for J in 1..I-1 loop
        SUM := SUM + B(ROW(I),J)*Z(J) ;
      end loop ;
      Z(I) := YY(I) - SUM ;
    end loop ;
    Ada.Text_IO.Put_Line ("Finished back sub, part 1");
    -- back substitute, second part
    for I in reverse 1..N loop
      SUM := 0.0 ;
      for J in I+1..N loop
        SUM := SUM + B(ROW(I),J)*X(J) ;
      end loop;
      X(I) := (Z(I)-SUM)/B(ROW(I),I) ;
    end loop;
    Ada.Text_IO.Put_Line ("Finished back sub, part 2");
    return X ;
  end Crout_Solve ;

  function Cholesky_Decomposition ( A : Real_Matrix) return Real_Matrix is

    --      PURPOSE : COMPUTE THE CHOLESKY DECOMPOSITION OF A SYMMETRIC
    --                POSITIVE DEFINATE MATRIX 'A'
    --                CHOLESKEY_SOLVE MAY THEN BE USED TO SOLVE LINEAR EQUATIONS
    --
    --      INPUT  : THE REAL MATRIX  A
    --
    --      OUTPUT : THE REAL MATRIX  L
    --
    --      METHOD : CHOLESKY DECOMPOSITION
    --
    --      USAGE  :     L := CHOLESKY_DECOMPOSITION ( A ) ;
    --
    --      EXCEPTION : Constraint_Error  RASIED IF 'A' NOT SQUARE
    --                  MATRIX_DATA_ERROR  RAISED IF 'A' NOT SYMMETRIC OR
    --                                               'A' NOT POSITIVE DEFINATE
    --
    --      WRITTEN BY : JON SQUIRE , 11/30/88

    L : Real_Matrix ( A'Range(1), A'Range(2) ) := (others=>(others=>0.0)) ;
    SUM : Real;
  begin
    if A'Length(1) /= A'Length(2) then
      raise Matrix_Data_Error ;
    end if;
    for I in A'Range(1) loop          -- check A for being symmetric
      for J in A'First(2)-A'First(1)+I+1 .. A'Last(2) loop
        if not Almost_zero (A(I,J) - A(A'First(1)-A'First(2)+J, A'First(2)-A'First(1)+I)) then
          if abs(A(I,J)-A(A'First(1)-A'First(2)+J, A'First(2)-A'First(1)+I)) >
            2.0 * Real'Model_Epsilon * abs(A(I,J))
          then
            raise Matrix_Data_Error with "Matrix is not symmetric";
          end if;
        end if;
      end loop;
    end loop;

    for J in A'Range(2) loop
      for I in A'First(1)-A'First(2)+J..A'Last(1) loop
        SUM := A(I,J);
        for K in A'First(2)..J-1 loop
          SUM := SUM - L(I,K) * L(A'First(1)-A'First(2)+J,K);
        end loop;
        if I = A'First(1)-A'First(2)+J then
          if SUM <= 0.0 then
            raise Matrix_Data_Error with "Matrix is not positive definite";
          end if;
          L(I,J) := Elementary_Functions.Sqrt(SUM);
        else
          L(I,J) := SUM / L(A'First(1)-A'First(2)+J,J);
        end if;
      end loop;
    end loop;
    return L;
  end Cholesky_Decomposition;

  function Cholesky_Solve ( L : Real_Matrix ;
                            Y : Real_Vector ) return Real_Vector is

    --      PURPOSE : SOLVE THE LINEAR SYSTEM OF EQUATIONS WITH REAL
    --                COEFFICIENTS   [A] * |X| = |Y| USING CHOLESKY
    --                DECOMPOSITION
    --
    --      INPUT  : THE REAL MATRIX  L  =  CHOLESKY_DECOMPOSITION ( A )
    --               THE REAL VECTOR  Y
    --
    --      OUTPUT : THE REAL VECTOR  X
    --
    --      METHOD : BACK SUBSTITUTION USING CHOLESKY DECOMPOSITION
    --
    --      USAGE  :  X := CHOLESKY_SOLVE ( CHOLESKY_DECOMPOSITION ( A ) , Y ) ;
    --
    --      EXCEPTION : Constraint_Error  RASIED IF 'A' NOT SQUARE OR
    --                                     Y LENGTH /= ROWS OF 'A'
    --                  MATRIX_DATA_ERROR  RAISED IF 'A' SINGULAR
    --
    --      WRITTEN BY : JON SQUIRE , 11/30/88

    X : Real_Vector ( Y'Range ) ;
    W : Real_Vector ( Y'Range ) ;
    SUM : Real;
  begin
    X(X'First):= 0.0; -- calm down "may be referenced before..." warning
    W(W'First):= 0.0; -- calm down "may be referenced before..." warning
    --
    for I in Y'Range loop -- solve  L * w = y
      SUM := Y(I);
      for K in Y'First..I-1 loop
        SUM := SUM - L(L'First(1)-Y'First+I, L'First(2)-Y'First+K) * W(K);
      end loop;
      W(I) := SUM / L(L'First(1)-Y'First+I, L'First(2)-Y'First+I);
    end loop;
                                  --         T
    for I in reverse Y'Range loop -- solve  L  * x = w
      SUM := W(I);
      for K in I+1..Y'Last loop
        SUM := SUM - L(L'First(1)-Y'First+K, L'First(2)-Y'First+I) * X(K);
      end loop;
      X(I) := SUM / L(L'First(1)-Y'First+I, L'First(2)-Y'First+I);
    end loop;
    return X;
  end Cholesky_Solve ;

  procedure LU_Decomposition ( A : Real_Matrix ;
                               L : in out Real_Matrix ;
                               U : in out Real_Matrix ;
                               P : in out Integer_Vector) is

    --      PURPOSE : COMPUTE THE LU DECOMPOSITION OF MATRIX 'A'
    --                LU_SOLVE MAY THEN BE USED TO SOLVE LINEAR EQUATIONS
    --
    --      INPUT  : THE REAL MATRIX  A
    --
    --      OUTPUT : THE REAL MATRIX  L, LOWER TRIANGULAR
    --               THE REAL MATRIX  U, UPPER TRIANGULAR
    --               THE INTEGER VECTOR P, ROW PERMUTATION OF  A
    --
    --      METHOD : LU DECOMPOSITION  P APPLIED TO  A = L * U
    --               DIAGONAL OF L = 1.0
    --
    --      USAGE  : LU_DECOMPOSITION ( A, L, U, P ) ;
    --
    --      EXCEPTION : Constraint_Error  RAISED IF 'A' NOT SQUARE
    --                  MATRIX_DATA_ERROR  RAISED IF 'A' IS SINGULAR
    --

    AA : Real_Matrix(A'Range(1),A'Range(2)) := A;
    ITEMP : Integer;
    KK    : Integer;
    TEMP  : Real;
    BIG   : Real;
    OFAL1 : constant Integer := L'First(1) - A'First(1) ;
    OFAL2 : constant Integer := L'First(2) - A'First(2) ;
    OFAU1 : constant Integer := U'First(1) - A'First(1) ;
    OFAU2 : constant Integer := U'First(2) - A'First(2) ;
    OFAA2 : constant Integer := A'First(2) - A'First(1) ;
    OFAP  : constant Integer := P'First - A'First(1) ;
  begin
    if A'Length ( 1 ) /= A'Length ( 2 ) then
      raise Constraint_Error with "Matrix A is not square";
    end if ;
    if A'Length(1) /= L'Length(1) or
       A'Length(1) /= L'Length(2) or
       A'Length(1) /= U'Length(1) or
       A'Length(1) /= U'Length(2) or
       A'Length(1) /= P'Length
    then
      raise Constraint_Error with "Dimension error with L, U or P";
    end if;

    L := (others=>(others=>0.0));
    U := (others=>(others=>0.0));
    for I in A'Range(1) loop
      P(I+OFAP) := I;
      L(I+OFAL1,I+OFAL2) := 1.0;
    end loop;
    for K in A'First(1)..A'Last(1)-1 loop
      BIG := 0.0;
      for I in K..A'Last(1) loop
        if abs AA(I,K+OFAA2) > BIG then
          BIG := abs AA(I,K+OFAA2);
          KK := I;
        end if;
      end loop;
      if Almost_zero(BIG) then
        raise Matrix_Data_Error with "Matrix is singular";
      end if;
      ITEMP := P(K+OFAP);
      P(K+OFAP) := P(KK+OFAP);
      P(KK+OFAP) := ITEMP;
      for I in A'Range(1) loop
        TEMP := AA(K,I+OFAA2);
        AA(K,I+OFAA2) := AA(KK,I+OFAA2);
        AA(KK,I+OFAA2) := TEMP;
      end loop;
      for I in K+1..A'Last(1) loop
        AA(I,K+OFAA2) := AA(I,K+OFAA2) / AA(K,K+OFAA2);
        for J in K+1..A'Last(1) loop
          AA(I,J+OFAA2) := AA(I,J+OFAA2) - AA(I,K+OFAA2) * AA(K,J+OFAA2);
        end loop;
      end loop;
    end loop;
    for I in A'Range(1) loop  -- copy from A to U
      for J in I..A'Last(1) loop
        U(I+OFAU1,J+OFAU2) := AA(I,J+OFAA2);
      end loop;
    end loop;
    for I in A'First(1)+1..A'Last(1) loop  -- copy from A to L
      for J in 1..I-1 loop
        L(I+OFAL1,J+OFAL2) := AA(I,J+OFAA2);
      end loop;
    end loop;
  end LU_Decomposition;

  function LU_Solve ( L : Real_Matrix ;
                      U : Real_Matrix ;
                      P : Integer_Vector ;
                      Y : Real_Vector ) return Real_Vector is

    --      PURPOSE : SOLVE THE LINEAR SYSTEM OF EQUATIONS WITH REAL
    --                COEFFICIENTS   [A] * |X| = |Y| USING LU DECOMPOSITION
    --
    --      INPUT  : THE REAL MATRIX  L    \_ FROM LU_DECOMPOSITION
    --               THE REAL MATRIX  U     /
    --               THE INTEGER VECTOR P  /
    --               THE REAL VECTOR  Y
    --
    --      OUTPUT : THE REAL VECTOR  X
    --
    --      METHOD : BACK SUBSTITUTION USING LU DECOMPOSITION
    --
    --      USAGE  :  X := LU_SOLVE ( L, U, P, Y ) ;
    --
    --      EXCEPTION : Constraint_Error  RAISED IF 'L' AND 'U' DIFFERENT SIZE
    --                                     Y LENGTH /= ROWS OF 'L'
    --

    X : Real_Vector ( Y'Range ) ;
    B : Real_Vector ( Y'Range ) ;
    SUM : Real ;
    OFYL1 : constant Integer := L'First(1) - Y'First ;
    OFYL2 : constant Integer := L'First(2) - Y'First ;
    OFYU1 : constant Integer := U'First(1) - Y'First ;
    OFYU2 : constant Integer := U'First(2) - Y'First ;
  begin
    if L'Length(1) /= L'Length(2) or
       L'Length(1) /= U'Length(1) or
       L'Length(1) /= U'Length(2) or
       L'Length(1) /= P'Length or
       L'Length(1) /= Y'Length
    then
      raise Constraint_Error with "Dimension error with L, U, P or Y";
    end if;
    B(B'First):= 0.0; -- calm down "may be referenced before..." warning
    X(X'First):= 0.0; -- calm down "may be referenced before..." warning

    for I in Y'Range loop
      SUM := 0.0 ;
      for J in Y'First .. I-1 loop
        SUM := SUM + L(I+OFYL1,J+OFYL2) * B(J) ;
      end loop ;
      B(I) := Y(P(I - Y'First + P'First)) - SUM ;
    end loop ;

    for I in reverse Y'Range loop
      SUM := 0.0 ;
      for J in I+1 .. Y'Last loop
        SUM := SUM + U(I+OFYU1,J+OFYU2) * X(J) ;
      end loop ;
      X(I) := ( B(I) - SUM ) / U(I+OFYU1,I+OFYU2) ;
    end loop ;
    return X;
  end LU_Solve ;

  procedure QR_Decomposition ( A : Real_Matrix ;
                               Q : in out Real_Matrix ;
                               R : in out Real_Matrix ) is

    --      PURPOSE : COMPUTE THE QR DECOMPOSITION OF MATRIX 'A'
    --                QR_SOLVE MAY THEN BE USED TO SOLVE LINEAR EQUATIONS
    --
    --      INPUT  : THE REAL MATRIX  A
    --
    --      OUTPUT : THE REAL MATRIX  Q
    --               THE REAL MATRIX  R
    --
    --      METHOD : QR DECOMPOSITION     A = Q * R    Q * TRANSPOSE(Q) = I
    --                                    R IS UPPER TRIANGULAR
    --                                    Q IS ORTHAGONAL
    --
    --      USAGE  : QR_DECOMPOSITION ( A , Q, R ) ;
    --
    --      EXCEPTION : Constraint_Error  RAISED IF 'A' NOT SQUARE OR
    --                                     'A', 'Q', AND 'R' NOT SAME SIZE
    --                  MATRIX_DATA_ERROR  RAISED IF 'A' IS SINGULAR
    --

    N : constant Integer := A'Length(1) ;     -- SIZE OF MATRIX
    AA : Real_Matrix ( 1 .. N , 1 .. N ) ;    -- WORKING MATRIX
    C  : Real_Vector ( 1 .. N ) ;
    D  : Real_Vector ( 1 .. N ) ;
    SCALE, SIGMA, SUM, TAU : Real ;
    OFAQ1 : constant Integer := Q'First(1) - 1 ;
    OFAQ2 : constant Integer := Q'First(2) - 1 ;
    OFAR1 : constant Integer := R'First(1) - 1 ;
    OFAR2 : constant Integer := R'First(2) - 1 ;
  begin
    if A'Length ( 1 ) /= A'Length ( 2 ) then
      raise Constraint_Error with "Matrix A is not square";
    end if ;
    if N /= R'Length(1) or N /= R'Length(2) or
       N /= Q'Length(1) or N /= Q'Length(2)
    then
      raise Constraint_Error with "Dimension error";
    end if ;

    AA := A;
    for K in 1 .. N-1 loop
      SCALE := 0.0 ;  -- Was before the main loop
      for I in K .. N loop
        if SCALE < abs AA(I,K) then  --  Was ">", then scale stayed always 0
          SCALE := abs AA(I,K) ;
        end if ;
      end loop ;
      if Almost_zero(SCALE) then
        raise Matrix_Data_Error with
          "Matrix is singular (scale = 0 on column" & Integer'Image(K) & ")";
      else
        for I in K .. N loop
          AA(I,K) := AA(I,K) / SCALE ;
        end loop ;
        SUM := 0.0 ;
        for I in K .. N loop
          SUM := SUM + AA(I,K) * AA(I,K) ;
        end loop ;
        if AA(K,K) < 0.0 then
          SIGMA := -Elementary_Functions.Sqrt(SUM) ;
        else
          SIGMA := Elementary_Functions.Sqrt(SUM) ;
        end if ;
        AA(K,K) := AA(K,K) + SIGMA ;
        C(K) := SIGMA * AA(K,K) ;
        D(K) := -SCALE * SIGMA ;
        for J in K+1 .. N loop
          SUM := 0.0 ;
          for I in K .. N loop
            SUM := SUM + AA(I,K) * AA(I,J) ;
          end loop ;
          TAU := SUM / C(K) ;
          for I in K .. N loop
            AA(I,J) := AA(I,J) - TAU * AA(I,K) ;
          end loop ;
        end loop ;
      end if ;
    end loop ;
    D(N) := AA(N,N) ;
    if Almost_zero(D(N)) then
      raise Matrix_Data_Error with
        "Matrix is singular (diag = 0 on position" & Integer'Image(N) & ")";
    end if ;

    Q:= Unit_Matrix(N);
    for K in 1..N-1 loop
      if not Almost_zero(C(K)) then
        for J in 1..N loop
          SUM:= 0.0;
          for I in K..N loop
            SUM:= SUM + AA(I,K) * Q(J+OFAQ1, I+OFAQ2);
          end loop;
          SUM:= SUM / C(K);
          for I in K..N loop
            Q(J+OFAQ1, I+OFAQ2):= Q(J+OFAQ1, I+OFAQ2) - SUM * AA(I,K);
          end loop;
        end loop;
      end if;
    end loop;
    -- R
    for I in 1..N loop
      for J in 1..I-1 loop
        R(I+OFAR1, J+OFAR2):= 0.0;
      end loop;
      R(I+OFAR1, I+OFAR2):= D(I);
      for J in I+1..N loop
        R(I+OFAR1, J+OFAR2):= AA(I,J);
      end loop;
    end loop;

    -- Orig. code, incomplete (e.g. Q was just zeroed !)
    --      making R and Q
    --      for I in 1 .. N loop
    --        for J in 1 .. N loop
    --          Q(I+OFAQ1,J+OFAQ2) := 0.0 ;
    --          R(I+OFAR1,J+OFAR2) := 0.0 ;
    --          if I = J then
    --            R(I+OFAR1,I+OFAR2) := D(I) ;
    --          else
    --            R(I+OFAR1,J+OFAR2) := AA(I,J) ;
    --          end if ;
    --        end loop ;
    --      end loop ;
  end QR_Decomposition;

  function QR_Solve ( Q : Real_Matrix ;
                      R : Real_Matrix ;
                      Y : Real_Vector ) return Real_Vector is

    --      PURPOSE : SOLVE THE LINEAR SYSTEM OF EQUATIONS WITH REAL
    --                COEFFICIENTS   [A] * |X| = |Y| USING QR DECOMPOSITION
    --
    --      INPUT  : THE REAL MATRIX  Q    \_ FROM QR_DECOMPOSITION
    --               THE REAL MATRIX  R     /
    --               THE REAL VECTOR  Y
    --
    --      OUTPUT : THE REAL VECTOR  X
    --
    --      METHOD : BACK SUBSTITUTION USING LU DECOMPOSITION
    --
    --      USAGE  :  X := QR_SOLVE ( Q, R, Y ) ;
    --
    --      EXCEPTION : Constraint_Error  RAISED IF 'Q' AND 'R' DIFFERENT SIZE
    --                                     Y LENGTH /= ROWS OF 'Q'
    --

    X : Real_Vector ( Y'Range ) ;
    SUM : Real ;
    OFYQ1 : constant Integer := Q'First(1) - Y'First ;
    OFYQ2 : constant Integer := Q'First(2) - Y'First ;
    OFYR1 : constant Integer := R'First(1) - Y'First ;
    OFYR2 : constant Integer := R'First(2) - Y'First ;
  begin
    if Y'Length /= R'Length(1) or Y'Length /= R'Length(2) or
       Y'Length /= Q'Length(1) or Y'Length /= Q'Length(2)
    then
      raise Constraint_Error with "Dimension error";
    end if ;
    X(X'First):= 0.0; -- calm down "may be referenced before..." warning

    for I in reverse Y'Range loop
      SUM := 0.0 ;
      for K in Y'Range loop
        SUM := SUM + Q(K+OFYQ1,I+OFYQ2) * Y(K) ; -- TRANSPOSE(Q) * Y
        -- (orig: was Y(I) !)
      end loop ;
      -- sum is (Qt y)_i
      for J in I+1 .. Y'Last loop
        SUM := SUM - X(J) * R(I+OFYR1,J+OFYR2) ;
      end loop ;
      X(I) := SUM / R(I+OFYR1,I+OFYR2) ;
    end loop ;
    return X;
  end QR_Solve ;

  procedure SV_Decomposition ( A : Real_Matrix ;
                               UU : in out Real_Matrix ;
                               VV : in out Real_Matrix ;
                               WW : in out Real_Vector ) is
    M: constant Integer := A'Length(1);
    N: constant Integer := A'Length(2);
    U:Real_Matrix(1..M,1..N) := A;
    V:Real_Matrix(1..N,1..N);
    W:Real_Vector(1..N);
    Rv1:Real_Vector(1..N);
    Its:Integer := 0;
    L:Integer := 0;
    L1:Integer := 0;
    I:Integer := 0;
    K:Integer := 0;
    K1:Integer := 0;
    Mn:Integer := 0;
    C,F,G,H,S,X,Y,Z,Eps,Scale,Machep:Real;
    -- Ierr : Integer := 0;

    function Amax1(A,B:Real) return Real is
    begin
      if A>B then
        return A;
      end if ;
      return B;
    end Amax1;

    function Sign(Val,Sgn:Real) return Real is
    begin
      if Sgn<0.0 then
        return - abs (Val);
      end if ;
      return abs (Val);
    end Sign;
  begin
    if UU'Length(1)/=M or UU'Length(2)/=N or
       VV'Length(1)/=N or VV'Length(2)/=N or
       WW'Length/=N
    then
      raise Constraint_Error with "Dimension error";
    end if ;
    Machep := 2.0**(-23);
    --     HOUSEHOLDER REDUCTION TO BIDIAGONAL FORM
    G := 0.0;
    Scale := 0.0;
    X := 0.0;
    for I in 1..N loop -- 300
      L := I+1;
      Rv1(I) := Scale*G;
      G := 0.0;
      S := 0.0;
      Scale := 0.0;
      if I<=M then  -- 210
        for K in I..M loop  -- 120
          Scale := Scale + abs U(K,I);
        end loop ;  -- 120
        if not Almost_zero (Scale) then  -- 210
          for K in I..M loop  -- 130
            U(K,I) := U(K,I)/Scale;
            S := S+U(K,I)**2;
          end loop ;  -- 130
          F := U(I,I);
          G := -Sign(Elementary_Functions.Sqrt(S),F);
          H := F*G-S;
          U(I,I) := F-G;
          if I/=N then  -- 190
            for J in L..N loop  -- 150
              S := 0.0;
              for K in I..M loop  -- 140
                S := S+U(K,I)*U(K,J);
              end loop ;  -- 140
              F := S/H;
              for K in I..M loop  -- 150
                U(K,J) := U(K,J)+F*U(K,I);
              end loop ;  -- 150
            end loop ;  -- 150
          end if ;  -- 190
          for K in I..M loop  -- 200
            U(K,I) := Scale*U(K,I);
          end loop ;  -- 200
        end if ;  -- 210
      end if ;  -- 210
      W(I) := Scale*G;
      G := 0.0;
      S := 0.0;
      Scale := 0.0;
      -- IF (I > M .OR. I = N) GO TO 290
      if I<=M and I/=N then  -- 290
        for K in L..N loop  -- 220
          Scale := Scale + abs U(I,K);
        end loop ;  -- 220
        if not Almost_zero (Scale) then  -- 290
          for K in L..N loop  -- 230
            U(I,K) := U(I,K)/Scale;
            S := S+U(I,K)**2;
          end loop ;  -- 230
          F := U(I,L);
          G := -Sign(Elementary_Functions.Sqrt(S),F);
          H := F*G-S;
          U(I,L) := F-G;
          for K in L..N loop  -- 240
            Rv1(K) := U(I,K)/H;
          end loop ;  -- 240
          if I/=M then  -- 270
            for J in L..M loop  -- 260
              S := 0.0;
              for K in L..N loop  -- 250
                S := S+U(J,K)*U(I,K);
              end loop ;  -- 250
              for K in L..N loop  -- 260
                U(J,K) := U(J,K)+S*Rv1(K);
              end loop ;  -- 260
            end loop ;  -- 260
          end if ;  -- 270
          for K in L..N loop  -- 280
            U(I,K) := Scale*U(I,K);
          end loop ;  -- 280
        end if ;  -- 290
      end if ;  -- 290
      X := Amax1(X, abs (W(I)) + abs (Rv1(I)));
    end loop ;  -- 300
    -- ACCUMULATION OF RIGHT-HAND TRANSFORMATIONS
    -- FOR I:=N STEP -1 UNTIL 1 DO
    for Ii in 1..N loop  -- 400
      I := N+1-Ii;
      if I/=N then  -- 390
        if not Almost_zero (G) then  -- 360
          for J in L..N loop  -- 320
          -- DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW
            V(J,I) := (U(I,J)/U(I,L))/G;
          end loop ;  -- 320
          for J in L..N loop  -- 350
            S := 0.0;
            for K in L..N loop  -- 340
              S := S+U(I,K)*V(K,J);
            end loop ;  -- 340
            for K in L..N loop  -- 350
              V(K,J) := V(K,J)+S*V(K,I);
            end loop ;  -- 350
          end loop ;  -- 350
        end if ;  -- 360
        for J in L..N loop  -- 380
          V(I,J) := 0.0;
          V(J,I) := 0.0;
        end loop ;  -- 380
      end if ;  -- 390
      V(I,I) := 1.0;
      G := Rv1(I);
      L := I;
    end loop ;  -- 400
    -- ACCUMULATION OF LEFT-HAND TRANSFORMATIONS
    -- FOR I:=MIN(M,N) STEP -1 UNTIL 1 DO
    Mn := N;
    if M<N then
      Mn := M;
    end if ;
    for Ii in 1..Mn loop
      -- 500
      I := Mn+1-Ii;
      L := I+1;
      G := W(I);
      if I/=N then  -- 430
        for J in L..N loop
          -- 420
          U(I,J) := 0.0;
        end loop ;  -- 420
      end if ;  -- 430
      if not Almost_zero (G) then  -- 475
        if I/=Mn then  -- 460
          for J in L..N loop  -- 450
            S := 0.0;
            for K in L..M loop  -- 440
              S := S+U(K,I)*U(K,J);
            end loop ;  -- 440
            -- DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW
            F := (S/U(I,I))/G;
            for K in I..M loop  -- 450
              U(K,J) := U(K,J)+F*U(K,I);
            end loop ;  -- 450
          end loop ;  -- 450
        end if ;  -- 460
        for J in I..M loop
          -- 470
          U(J,I) := U(J,I)/G;
        end loop ;  -- 470
      else  -- 475
        for J in I..M loop  -- 480
          U(J,I) := 0.0;
        end loop ;  -- 480
      end if ;  -- 475
      U(I,I) := U(I,I)+1.0;
    end loop ;  -- 500
    -- DIAGONALIZATION OF THE BIDIAGONAL FORM
    Eps := Machep*X;
    -- FOR K:=N STEP -1 UNTIL 1 DO
    for Kk in 1..N loop  -- 700
      K1 := N-Kk;
      K := K1+1;
      Its := 0;
      -- TEST FOR SPLITTING.
      -- FOR L:=K STEP -1 UNTIL 1 DO
      loop  -- 520
        for Ll in 1..K loop  -- 530
          L1 := K-Ll;
          L := L1+1;
          if abs (Rv1(L))<=Eps then
            goto L565;
          end if ;
          -- RV1(1) IS ALWAYS ZERO, SO THERE IS NO EXIT
          -- THROUGH THE BOTTOM OF THE LOOP
          if (abs W(L1))<=Eps then
            exit ;  -- GO TO 540
          end if ;
        end loop ;
        -- 530
        --  CANCELLATION OF RV1(L) IF L GREATER THAN 1
        -- 540
        C := 0.0;
        S := 1.0;
        for I in L..K loop  -- 560
          F := S*Rv1(I);
          Rv1(I) := C*Rv1(I);
          if abs (F)<=Eps then
            exit ;  -- GO TO 565
          end if ;
          G := W(I);
          H := Elementary_Functions.Sqrt(F*F+G*G);
          W(I) := H;
          C := G/H;
          S := -F/H;
          for J in 1..M loop  -- 550
            Y := U(J,L1);
            Z := U(J,I);
            U(J,L1) := Y*C+Z*S;
            U(J,I) := -Y*S+Z*C;
          end loop ;  -- 550
        end loop ;  -- 560
        -- TEST FOR CONVERGENCE
<<L565>>
        Z := W(K);
        if L=K then
          goto L650;
        end if ;
        -- SHIFT FROM BOTTOM 2 BY 2 MINOR
        if Its=30 then
          -- SET ERROR, NO CONVERGENCE TO A SINGULAR VALUE AFTER 30 ITERATIONS
          -- Ierr := K;
          raise Matrix_Data_Error with "No SV convergence";
        end if ;
        Its := Its+1;
        X := W(L);
        Y := W(K1);
        G := Rv1(K1);
        H := Rv1(K);
        F := ((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.0*H*Y);
        G := Elementary_Functions.Sqrt(F*F+1.0);
        F := ((X-Z)*(X+Z)+H*(Y/(F+Sign(G,F))-H))/X;
        -- NEXT QR TRANSFORMATION
        C := 1.0;
        S := 1.0;
        for I1 in L..K1 loop  -- 600
          I := I1+1;
          G := Rv1(I);
          Y := W(I);
          H := S*G;
          G := C*G;
          Z := Elementary_Functions.Sqrt(F*F+H*H);
          Rv1(I1) := Z;
          C := F/Z;
          S := H/Z;
          F := X*C+G*S;
          G := -X*S+G*C;
          H := Y*S;
          Y := Y*C;
          for J in 1..N loop  -- 570
            X := V(J,I1);
            Z := V(J,I);
            V(J,I1) := X*C+Z*S;
            V(J,I) := -X*S+Z*C;
          end loop ;  -- 570
          Z := Elementary_Functions.Sqrt(F*F+H*H);
          W(I1) := Z;
          -- ROTATION CAN BE ARBITRARY IF Z IS ZERO
          if not Almost_zero (Z) then  -- 580
            C := F/Z;
            S := H/Z;
          end if ;  -- 580
          F := C*G+S*Y;
          X := -S*G+C*Y;
          for J in 1..M loop  -- 590
            Y := U(J,I1);
            Z := U(J,I);
            U(J,I1) := Y*C+Z*S;
            U(J,I) := -Y*S+Z*C;
          end loop ;  -- 590
        end loop ;  -- 600
        Rv1(L) := 0.0;
        Rv1(K) := F;
        W(K) := X;
      end loop ;  -- 520
      -- CONVERGENCE
<<L650>>
      if Z<0.0 then
        -- W(K) IS MADE NON-NEGATIVE
        W(K) := -Z;
        for J in 1..N loop
          V(J,K) := -V(J,K);
        end loop ;
      end if ;
    end loop ;  -- 700
    WW := W;
    UU := U;
    VV := V;
  end SV_Decomposition;

  function SV_Solve ( U : Real_Matrix ;
                      V : Real_Matrix ;
                      W : Real_Vector ;
                      Y : Real_Vector ) return Real_Vector is
    M: constant Integer := U'Length(1);
    N: constant Integer := U'Length(2);
    S:Real;
    X:Real_Vector(1..N);
    Tmp:Real_Vector(1..N);
  begin
    if V'Length(1)/=N or V'Length(2)/=N or W'Length/=N or Y'Length/=N then
      raise Constraint_Error with "Dimension error";
    end if ;
    for J in 1..N loop
      S := 0.0;
      if not Almost_zero (W(J-1+W'First)) then
        for I in 1..M loop
          S := S+U(I-1+U'First(1),J-1+U'First(2))*Y(I-1+Y'First);
        end loop ;
        S := S/W(J-1+W'First);
      end if ;
      Tmp(J) := S;
    end loop ;
    for J in 1..N loop
      S := 0.0;
      for Jj in 1..N loop
        S := S+V(J-1+V'First(1),Jj-1+V'First(2))*Tmp(Jj);
      end loop ;
      X(J) := S;
    end loop ;
    return X;
  end SV_Solve;

end Generic_Real_Linear_Equations ;
