
with Ada.Text_IO; use Ada.Text_IO; -- for debug
--with INTEGER_ARRAYS_IO;  -- for debug
--with REAL_ARRAYS_IO; -- for debug
with Ada.Numerics.GENERIC_ELEMENTARY_FUNCTIONS;
package body GENERIC_REAL_LINEAR_EQUATIONS is

  package ELEMENTARY_FUNCTIONS is new
                       Ada.Numerics.GENERIC_ELEMENTARY_FUNCTIONS ( REAL ) ;

--  package REAL_IO is new REAL_ARRAYS_IO
--                            (REAL, Real_Arrays); -- for debug
--  use REAL_IO; -- for debug
--  package INT_IO is new INTEGER_ARRAYS_IO
--                            (Integer, Integer_Arrays); -- for debug
--  use INT_IO; -- for debug

  function LINEAR_EQUATIONS ( A : REAL_MATRIX ;
                              Y : REAL_VECTOR ) return REAL_VECTOR is

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

    N : constant INTEGER := A'LENGTH(1) ; -- NUMBER OF EQUATIONS
    X : REAL_VECTOR ( 1 .. N ) ;          -- RESULT BEING COMPUTED
    B : REAL_MATRIX ( 1 .. N , 1 .. N + 1 ) ;  -- WORKING MATRIX
    ROW : array ( 1 .. N ) of INTEGER ;   -- ROW INTERCHANGE INDICIES
    HOLD , I_PIVOT : INTEGER ;            -- PIVOT INDICIES
    PIVOT : REAL ;                        -- PIVOT ELEMENT VALUE
    ABS_PIVOT : REAL ;                    -- ABS OF PIVOT ELEMENT
    NORM1 : REAL := 0.0 ;                 -- 1 NORM OF MATRIX
  begin
    if A'LENGTH ( 1 ) /= A'LENGTH ( 2 ) or A'LENGTH ( 1 ) /= Y'LENGTH then
      raise ARRAY_INDEX_ERROR ;
    end if ;

    --                               BUILD WORKING DATA STRUCTURE
    for I in 1 .. N loop
      for J in 1 .. N loop
        B ( I , J ) := A ( I - 1 + A'FIRST( 1 ) , J - 1 + A'FIRST ( 2 )) ;
        if abs B ( I , J ) > NORM1 then
          NORM1 := abs B ( I , J ) ;
        end if;
      end loop ;
      B ( I , N + 1 ) := Y ( I - 1 + Y'FIRST ) ;
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
      if ABS_PIVOT < REAL'EPSILON * NORM1 then
        raise MATRIX_DATA_ERROR;
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
  end LINEAR_EQUATIONS ;

  function LINEAR_EQUATIONS ( A : REAL_MATRIX ;
                              Y : REAL_MATRIX ) return REAL_MATRIX is

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
    --                  ARRAY_INDEX_ERROR IF 'A' IS NOT SQUARE OR
    --                                       ROWS OF 'Y' /= ROWS OF 'A'

    N : constant INTEGER := A'LENGTH(1) ;      -- NUMBER OF EQUATIONS
    M : constant INTEGER := Y'LENGTH(2) ;      -- NUMBER OF SOLUTIONS REQUESTED
    X : REAL_MATRIX ( 1 .. N , 1 .. M ) ;      -- RESULT BEING COMPUTED
    B : REAL_MATRIX ( 1 .. N , 1 .. N + M ) ;  -- WORKING MATRIX
    ROW : array ( 1 .. N ) of INTEGER ;        -- ROW INTERCHANGE INDICIES
    HOLD , I_PIVOT : INTEGER ;                 -- PIVOT INDICIES
    PIVOT : REAL ;                             -- PIVOT ELEMENT VALUE
    ABS_PIVOT : REAL ;                         -- ABS OF PIVOT ELEMENT
    NORM1 : REAL := 0.0 ;                      -- 1 NORM OF MATRIX
  begin
    if A'LENGTH ( 1 ) /= A'LENGTH ( 2 ) or A'LENGTH ( 1 ) /= Y'LENGTH ( 1 ) then
      raise ARRAY_INDEX_ERROR ;
    end if ;

    --                               BUILD WORKING DATA STRUCTURE
    for I in 1 .. N loop
      for J in 1 .. N loop
        B ( I , J ) := A ( I - 1 + A'FIRST( 1 ) , J - 1 + A'FIRST ( 2 )) ;
        if abs B ( I , J ) > NORM1 then
          NORM1 := abs B ( I , J ) ;
        end if ;
      end loop ;
      for J in 1 .. M loop
        B ( I , N + J ) := Y ( I - 1 + Y'FIRST(1), J - 1 + Y'FIRST(2) ) ;
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
      if ABS_PIVOT < REAL'EPSILON * NORM1 then
        raise MATRIX_DATA_ERROR;
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
  end LINEAR_EQUATIONS ;

  function DETERMINANT ( A : REAL_MATRIX ) return REAL is

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
    --      EXCEPTIONS : ARRAY_INDEX_ERROR IF THE MATRIX IS NOT SQUARE
    --
    --      WRITTEN BY : JON SQUIRE , 28 MAY 1983

    N : constant INTEGER := A'LENGTH(1) ;  -- NUMBER OF ROWS
    D : REAL := 1.0 ;                      -- DETERMINANT
    B : REAL_MATRIX ( 1 .. N , 1 .. N ) ;  -- WORKING MATRIX
    ROW : array ( 1 .. N ) of INTEGER ;    -- ROW INTERCHANGE INDICIES
    HOLD , I_PIVOT : INTEGER ;             -- PIVOT INDICIES
    PIVOT : REAL ;                         -- PIVOT ELEMENT VALUE
    ABS_PIVOT : REAL ;                     -- ABS OF PIVOT ELEMENT
  begin
    if A'LENGTH ( 1 ) /= A'LENGTH ( 2 ) then
      raise ARRAY_INDEX_ERROR ;
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
    when CONSTRAINT_ERROR => -- catches overflow
      return 0.0 ;
  end DETERMINANT ;


  function INVERSE ( A : REAL_MATRIX ) return REAL_MATRIX is

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
    --                  ARRAY_INDEX_ERROR IF 'A' IS NOT SQUARE
    --
    --      SAMPLE USE :  NEW_MATRIX := INVERSE ( OLD_MATRIX ) ;
    --                    MATRIX := INVERSE ( MATRIX ) * ANOTHER_MATRIX ;
    --
    --      WRITTEN BY : JON SQUIRE , 3 FEB 1983

    N : constant INTEGER := A'LENGTH ;        -- SIZE OF MATRIX
    AA : REAL_MATRIX ( 1 .. N , 1 .. N ) ;    -- WORKING MATRIX
    TEMP : REAL_VECTOR ( 1 .. N ) ;           -- TEMPORARY FOR UNSCRAMBLING ROWS
    ROW , COL : array ( 1 .. N ) of INTEGER ; -- ROW,COLUMN INTERCHANGE INDICIES
    HOLD , I_PIVOT , J_PIVOT : INTEGER ;      -- PIVOT INDICIES
    PIVOT : REAL ;                            -- PIVOT ELEMENT VALUE
    ABS_PIVOT : REAL ;                        -- ABS OF PIVOT ELEMENT
    NORM1 : REAL := 0.0 ;                     -- 1 NORM OF MATRIX
  begin
    if A'LENGTH ( 1 ) /= A'LENGTH ( 2 ) then
      raise ARRAY_INDEX_ERROR ;
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
      if ABS_PIVOT < REAL'EPSILON * NORM1 then
        raise MATRIX_DATA_ERROR;
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
  end INVERSE ;

  procedure INVERSE ( A : in out REAL_MATRIX ) is

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
    --                  ARRAY_INDEX_ERROR RAISED IF MATRIX IS NOT SQUARE
    --
    --      SAMPLE USE :  INVERSE ( SOME_MATRIX ) ;

    N : constant INTEGER := A'LENGTH ;        -- SIZE OF MATRIX
    AA : REAL_MATRIX ( 1 .. N , 1 .. N ) ;    -- WORKING MATRIX
    ROW , COL : array ( 1 .. N ) of INTEGER ; -- ROW,COLUMN INTERCHANGE INDICIES
    TEMP : REAL_VECTOR ( 1 .. N ) ;           -- TEMP ARRAY FOR UNSCRAMBLING
    HOLD , I_PIVOT , J_PIVOT : INTEGER ;      -- PIVOT INDICIES
    PIVOT : REAL ;  -- PIVOT ELEMENT VALUE    -- PIVOT ELEMENT
    ABS_PIVOT : REAL ;                        -- ABS OF PIVOT ELEMENT
    NORM1 : REAL := 0.0 ;                     -- 1 NORM OF MATRIX
  begin
    if A'LENGTH ( 1 ) /= A'LENGTH ( 2 ) then
      raise ARRAY_INDEX_ERROR ;
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
      if ABS_PIVOT < REAL'EPSILON * NORM1 then
        raise MATRIX_DATA_ERROR;
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
  end INVERSE ;

  function CROUT_SOLVE ( A : REAL_MATRIX ;
                         Y : REAL_VECTOR ) return REAL_VECTOR is

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
    --      EXCEPTION : ARRAY_INDEX_ERROR  RASIED IF 'A' NOT SQUARE OR
    --                                     Y LENGTH /= ROWS OF 'A'
    --                  MATRIX_DATA_ERROR  RAISED IF 'A' SINGULAR
    --
    --      WRITTEN BY : JON SQUIRE , 11/30/88

    N : constant INTEGER := A'LENGTH ;         -- NUMBER OF EQUATIONS
    X : REAL_VECTOR ( 1 .. N ) ;               -- RESULT BEING COMPUTED
    Z : REAL_VECTOR ( 1 .. N ) ;               -- TEMPORARY IN BACK SUB
    YY : REAL_VECTOR ( 1 .. N ) ;              -- TEMPORARY IN BACK SUB
    B : REAL_MATRIX ( 1 .. N , 1 .. N ) ;      -- WORKING MATRIX
    ROW : INTEGER_VECTOR( 1 .. N ) ;           -- ROW INTERCHANGE INDICIES
    HOLD , I_PIVOT : INTEGER ;                 -- PIVOT INDICIES
    PIVOT : REAL ;                             -- PIVOT ELEMENT VALUE
    ABS_PIVOT : REAL ;                         -- ABS OF PIVOT ELEMENT
    NORM1 : REAL := 0.0 ;                      -- 1 NORM OF MATRIX
    SUM : REAL ;                               -- TEMP VARIBLE
  begin
    if A'LENGTH ( 1 ) /= A'LENGTH ( 2 ) or A'LENGTH ( 1 ) /= Y'LENGTH then
      raise ARRAY_INDEX_ERROR ;
    end if ;
    Z(1):= 0.0; -- calm down "may be referenced before it has a value" warning
    X(1):= 0.0; -- calm down "may be referenced before it has a value" warning

    --                        BUILD WORKING DATA STRUCTURE
    for I in 1 .. N loop
      for J in 1 .. N loop
        B ( I , J ) := A ( I - 1 + A'FIRST( 1 ) , J - 1 + A'FIRST ( 2 )) ;
        if abs B ( I , J ) > NORM1 then
          NORM1 := abs B ( I , J ) ;
        end if ;
      end loop ;
      YY ( I ) := Y ( I - 1 + Y'FIRST ) ;
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
      if ABS_PIVOT < REAL'EPSILON * NORM1 then -- degeneration
        raise MATRIX_DATA_ERROR ;
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
      Put_Line("finished col " & integer'image(J));
    end loop ;

    -- back substitute, first part
    for I in 1..N loop
      SUM := 0.0 ;
      for J in 1..I-1 loop
        SUM := SUM + B(ROW(I),J)*Z(J) ;
      end loop ;
      Z(I) := YY(I) - SUM ;
    end loop ;
    Put_Line("Finished back sub, part 1");
    -- back substitute, second part
    for I in reverse 1..N loop
      SUM := 0.0 ;
      for J in I+1..N loop
        SUM := SUM + B(ROW(I),J)*X(J) ;
      end loop;
      X(I) := (Z(I)-SUM)/B(ROW(I),I) ;
    end loop;
    Put_Line("Finished back sub, part 2");
    return X ;
  end CROUT_SOLVE ;


  function CHOLESKY_DECOMPOSITION ( A : REAL_MATRIX) return REAL_MATRIX is

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
    --      EXCEPTION : ARRAY_INDEX_ERROR  RASIED IF 'A' NOT SQUARE
    --                  MATRIX_DATA_ERROR  RAISED IF 'A' NOT SYMMETRIC OR
    --                                               'A' NOT POSITIVE DEFINATE
    --
    --      WRITTEN BY : JON SQUIRE , 11/30/88

    L : REAL_MATRIX ( A'RANGE(1), A'RANGE(2) ) := (others=>(others=>0.0)) ;
    SUM : REAL;
  begin
    if A'LENGTH(1) /= A'LENGTH(2) then
      raise MATRIX_DATA_ERROR ;
    end if;
    for I in A'RANGE(1) loop          -- check A for being symmetric
      for J in A'FIRST(2)-A'FIRST(1)+I+1 .. A'LAST(2) loop
        if A(I,J) /= A(A'FIRST(1)-A'FIRST(2)+J, A'FIRST(2)-A'FIRST(1)+I) then
          if abs(A(I,J)-A(A'FIRST(1)-A'FIRST(2)+J, A'FIRST(2)-A'FIRST(1)+I)) >
             2.0 * REAL'EPSILON * abs(A(I,J)) then
               raise MATRIX_DATA_ERROR with "not symmetric";
          end if;
        end if;
      end loop;
    end loop;

    for J in A'RANGE(2) loop
      for I in A'FIRST(1)-A'FIRST(2)+J..A'LAST(1) loop
        SUM := A(I,J);
        for K in A'FIRST(2)..J-1 loop
          SUM := SUM - L(I,K) * L(A'FIRST(1)-A'FIRST(2)+J,K);
        end loop;
        if I = A'FIRST(1)-A'FIRST(2)+J then
          if SUM <= 0.0 then
            raise MATRIX_DATA_ERROR with "not positive definite";
          end if;
          L(I,J) := ELEMENTARY_FUNCTIONS.SQRT(SUM);
        else
          L(I,J) := SUM / L(A'FIRST(1)-A'FIRST(2)+J,J);
        end if;
      end loop;
    end loop;
    return L;
  end CHOLESKY_DECOMPOSITION;

  function CHOLESKY_SOLVE ( L : REAL_MATRIX ;
                            Y : REAL_VECTOR ) return REAL_VECTOR is

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
    --      EXCEPTION : ARRAY_INDEX_ERROR  RASIED IF 'A' NOT SQUARE OR
    --                                     Y LENGTH /= ROWS OF 'A'
    --                  MATRIX_DATA_ERROR  RAISED IF 'A' SINGULAR
    --
    --      WRITTEN BY : JON SQUIRE , 11/30/88

    X : REAL_VECTOR ( Y'RANGE ) ;
    W : REAL_VECTOR ( Y'RANGE ) ;
    SUM : REAL;
  begin
    X(X'First):= 0.0; -- calm down "may be referenced before..." warning
    W(W'First):= 0.0; -- calm down "may be referenced before..." warning
    --
    for I in Y'RANGE loop -- solve  L * w = y
      SUM := Y(I);
      for K in Y'FIRST..I-1 loop
        SUM := SUM - L(L'FIRST(1)-Y'FIRST+I, L'FIRST(2)-Y'FIRST+K) * W(K);
      end loop;
      W(I) := SUM / L(L'FIRST(1)-Y'FIRST+I, L'FIRST(2)-Y'FIRST+I);
    end loop;
                                  --         T
    for I in reverse Y'RANGE loop -- solve  L  * x = w
      SUM := W(I);
      for K in I+1..Y'LAST loop
        SUM := SUM - L(L'FIRST(1)-Y'FIRST+K, L'FIRST(2)-Y'FIRST+I) * X(K);
      end loop;
      X(I) := SUM / L(L'FIRST(1)-Y'FIRST+I, L'FIRST(2)-Y'FIRST+I);
    end loop;
    return X;
  end CHOLESKY_SOLVE ;

  procedure LU_DECOMPOSITION ( A : REAL_MATRIX ;
                               L : in out REAL_MATRIX ;
                               U : in out REAL_MATRIX ;
                               P : in out INTEGER_VECTOR) is

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
    --      USAGE  : LU_DECOMPOSITION ( A , L, U, P ) ;
    --
    --      EXCEPTION : ARRAY_INDEX_ERROR  RASIED IF 'A' NOT SQUARE
    --                  MATRIX_DATA_ERROR  RAISED IF 'A' IS SINGULAR
    --

    AA : REAL_MATRIX(A'RANGE(1),A'RANGE(2)) := A;
    ITEMP : INTEGER;
    KK    : INTEGER;
    TEMP  : REAL;
    BIG   : REAL;
    OFAL1 : constant INTEGER := L'FIRST(1) - A'FIRST(1) ;
    OFAL2 : constant INTEGER := L'FIRST(2) - A'FIRST(2) ;
    OFAU1 : constant INTEGER := U'FIRST(1) - A'FIRST(1) ;
    OFAU2 : constant INTEGER := U'FIRST(2) - A'FIRST(2) ;
    OFAA2 : constant INTEGER := A'FIRST(2) - A'FIRST(1) ;
    OFAP  : constant INTEGER := P'FIRST - A'FIRST(1) ;
  begin
    if A'Length(1) /= A'Length(2) or
       A'Length(1) /= L'Length(1) or
       A'Length(1) /= L'Length(2) or
       A'Length(1) /= U'Length(1) or
       A'Length(1) /= U'Length(2) or
       A'Length(1) /= P'Length then
      raise Array_Index_Error;
    end if;

    L := (others=>(others=>0.0));
    U := (others=>(others=>0.0));
    for I in A'RANGE(1) loop
      P(I+OFAP) := I;
      L(I+OFAL1,I+OFAL2) := 1.0;
    end loop;
    for K in A'FIRST(1)..A'LAST(1)-1 loop
      BIG := 0.0;
      for I in K..A'LAST(1) loop
        if abs AA(I,K+OFAA2) > BIG then
          BIG := abs AA(I,K+OFAA2);
          KK := I;
        end if;
      end loop;
      if BIG = 0.0 then
        raise MATRIX_DATA_ERROR; -- singular
      end if;
      ITEMP := P(K+OFAP);
      P(K+OFAP) := P(KK+OFAP);
      P(KK+OFAP) := ITEMP;
      for I in A'RANGE(1) loop
        TEMP := AA(K,I+OFAA2);
        AA(K,I+OFAA2) := AA(KK,I+OFAA2);
        AA(KK,I+OFAA2) := TEMP;
      end loop;
      for I in K+1..A'LAST(1) loop
        AA(I,K+OFAA2) := AA(I,K+OFAA2) / AA(K,K+OFAA2);
        for J in K+1..A'LAST(1) loop
          AA(I,J+OFAA2) := AA(I,J+OFAA2) - AA(I,K+OFAA2) * AA(K,J+OFAA2);
        end loop;
      end loop;
    end loop;
    for I in A'RANGE(1) loop  -- copy from A to U
      for J in I..A'LAST(1) loop
        U(I+OFAU1,J+OFAU2) := AA(I,J+OFAA2);
      end loop;
    end loop;
    for I in A'FIRST(1)+1..A'LAST(1) loop  -- copy from A to L
      for J in 1..I-1 loop
        L(I+OFAL1,J+OFAL2) := AA(I,J+OFAA2);
      end loop;
    end loop;
  end LU_DECOMPOSITION;

  function LU_SOLVE ( L : REAL_MATRIX ;
                      U : REAL_MATRIX ;
                      P : INTEGER_VECTOR ;
                      Y : REAL_VECTOR ) return REAL_VECTOR is

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
    --      EXCEPTION : ARRAY_INDEX_ERROR  RASIED IF 'L' AND 'U' DIFFERENT SIZE
    --                                     Y LENGTH /= ROWS OF 'L'
    --

    X : REAL_VECTOR ( Y'RANGE ) ;
    B : REAL_VECTOR ( Y'RANGE ) ;
    SUM : REAL ;
    OFYL1 : constant INTEGER := L'FIRST(1) - Y'FIRST ;
    OFYL2 : constant INTEGER := L'FIRST(2) - Y'FIRST ;
    OFYU1 : constant INTEGER := U'FIRST(1) - Y'FIRST ;
    OFYU2 : constant INTEGER := U'FIRST(2) - Y'FIRST ;
  begin
    if L'Length(1) /= L'Length(2) or
       L'Length(1) /= U'Length(1) or
       L'Length(1) /= U'Length(2) or
       L'Length(1) /= P'Length or
       L'Length(1) /= Y'Length then
      raise Array_Index_Error;
    end if;
    B(B'First):= 0.0; -- calm down "may be referenced before..." warning
    X(X'First):= 0.0; -- calm down "may be referenced before..." warning

    for I in Y'Range loop
      SUM := 0.0 ;
      for J in Y'First .. I-1 loop
        SUM := SUM + L(I+OFYL1,J+OFYL2) * B(J) ;
      end loop ;
      B(I) := Y(P(I)) - SUM ;
    end loop ;

    for I in reverse Y'Range loop
      SUM := 0.0 ;
      for J in I+1 .. Y'Last loop
        SUM := SUM + U(I+OFYU1,J+OFYU2) * X(J) ;
      end loop ;
      X(I) := ( B(I) - SUM ) / U(I+OFYU1,I+OFYU2) ;
    end loop ;
    return X;
  end LU_SOLVE ;

  procedure QR_DECOMPOSITION ( A : REAL_MATRIX ;
                               Q : in out REAL_MATRIX ;
                               R : in out REAL_MATRIX ) is

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
    --      EXCEPTION : ARRAY_INDEX_ERROR  RASIED IF 'A' NOT SQUARE OR
    --                                     'A', 'Q', AND 'R' NOT SAME SIZE
    --                  MATRIX_DATA_ERROR  RAISED IF 'A' IS SINGULAR
    --

    N : constant INTEGER := A'LENGTH(1) ;     -- SIZE OF MATRIX
    AA : REAL_MATRIX ( 1 .. N , 1 .. N ) ;    -- WORKING MATRIX
    C  : REAL_VECTOR ( 1 .. N ) ;
    D  : REAL_VECTOR ( 1 .. N ) ;
    SCALE, SIGMA, SUM, TAU : REAL ;
    OFAQ1 : constant INTEGER := Q'FIRST(1) - 1 ;
    OFAQ2 : constant INTEGER := Q'FIRST(2) - 1 ;
    OFAR1 : constant INTEGER := R'FIRST(1) - 1 ;
    OFAR2 : constant INTEGER := R'FIRST(2) - 1 ;
  begin
    if N /= A'LENGTH(2) or N /= R'LENGTH(1) or N /= R'LENGTH(2) or
       N /= Q'LENGTH(1) or N /= Q'LENGTH(2) then
      raise ARRAY_INDEX_ERROR ;
    end if ;

    AA := A;
    SCALE := 0.0 ;
    for K in 1 .. N-1 loop
      for I in K .. N loop
        if SCALE > abs AA(I,K) then
          SCALE := abs AA(I,K) ;
        end if ;
      end loop ;
      if SCALE = 0.0 then
        raise MATRIX_DATA_ERROR ; -- matrix is singular
      else
        for I in k .. N loop
          AA(I,K) := AA(I,K) / SCALE ;
        end loop ;
        SUM := 0.0 ;
        for I in K .. N loop
          SUM := SUM + AA(I,K) * AA(I,K) ;
        end loop ;
        if AA(K,K) < 0.0 then
          SIGMA := -ELEMENTARY_FUNCTIONS.SQRT(SUM) ;
        else
          SIGMA := ELEMENTARY_FUNCTIONS.SQRT(SUM) ;
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
    if D(N) = 0.0 then
      raise MATRIX_DATA_ERROR ;
    end if ;

    -- making R and Q
    for I in 1 .. N loop
      for J in 1 .. N loop
        Q(I+OFAQ1,J+OFAQ2) := 0.0 ;
        R(I+OFAR1,J+OFAR2) := 0.0 ;
        if I = J then
          R(I+OFAR1,I+OFAR2) := D(I) ;
        else
          R(I+OFAR1,J+OFAR2) := AA(I,J) ;
        end if ;
      end loop ;
    end loop ;
  end QR_DECOMPOSITION;

  function QR_SOLVE ( Q : REAL_MATRIX ;
                      R : REAL_MATRIX ;
                      Y : REAL_VECTOR ) return REAL_VECTOR is

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
    --      USAGE  :  X := LU_SOLVE ( Q, R, Y ) ;
    --
    --      EXCEPTION : ARRAY_INDEX_ERROR  RASIED IF 'Q' AND 'R' DIFFERENT SIZE
    --                                     Y LENGTH /= ROWS OF 'Q'
    --

    X : REAL_VECTOR ( Y'RANGE ) ;
    SUM : REAL ;
    OFYQ1 : constant INTEGER := Q'FIRST(1) - Y'FIRST ;
    OFYQ2 : constant INTEGER := Q'FIRST(2) - Y'FIRST ;
    OFYR1 : constant INTEGER := R'FIRST(1) - Y'FIRST ;
    OFYR2 : constant INTEGER := R'FIRST(2) - Y'FIRST ;
  begin
    if Y'LENGTH /= R'LENGTH(1) or Y'LENGTH /= R'LENGTH(2) or
       Y'LENGTH /= Q'LENGTH(1) or Y'LENGTH /= Q'LENGTH(2) then
      raise ARRAY_INDEX_ERROR ;
    end if ;
    X(X'First):= 0.0; -- calm down "may be referenced before..." warning

    for I in reverse Y'RANGE loop
      SUM := 0.0 ;
      for K in Y'RANGE loop
        SUM := SUM + Q(K+OFYQ1,I+OFYQ2) * Y(I) ; -- TRANSPOSE(Q) * Y
      end loop ;
      for J in I+1 .. Y'LAST loop
        SUM := SUM - X(J) * R(I+OFYR1,J+OFYR2) ;
      end loop ;
      X(I) := SUM / R(I+OFYR1,I+OFYR2) ;
    end loop ;
    return X;
  end QR_SOLVE ;

  procedure SV_DECOMPOSITION ( A : REAL_MATRIX ;
                               UU : in out REAL_MATRIX ;
                               VV : in out REAL_MATRIX ;
                               WW : in out REAL_VECTOR ) is
    M: constant Integer := A'length(1);
    N: constant Integer := A'length(2);
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
    if Uu'length(1)/=M or Uu'length(2)/=N or
       Vv'length(1)/=N or Vv'length(2)/=N or
       Ww'length/=N then
      raise Array_Index_Error;
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
          Scale := Scale+ abs (U(K,I));
        end loop ;  -- 120
        if Scale/=0.0 then  -- 210
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
      --IF (I > M .OR. I = N) GO TO 290
      if I<=M and I/=N then  -- 290
        for K in L..N loop  -- 220
          Scale := Scale+ abs (U(I,K));
        end loop ;  -- 220
        if Scale/=0.0 then  -- 290
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
      X := Amax1(X, abs (W(I))+ abs (Rv1(I)));
    end loop ;  -- 300
    -- ACCUMULATION OF RIGHT-HAND TRANSFORMATIONS
    -- FOR I:=N STEP -1 UNTIL 1 DO
    for Ii in 1..N loop  -- 400
      I := N+1-Ii;
      if I/=N then  -- 390
        if G/=0.0 then  -- 360
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
      if G/=0.0 then  -- 475
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
        --540
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
          if Z/=0.0 then  -- 580
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
    Ww := W;
    Uu := U;
    Vv := V;
  end SV_DECOMPOSITION;

  function SV_SOLVE ( U : REAL_MATRIX ;
                      V : REAL_MATRIX ;
                      W : REAL_VECTOR ;
                      Y : REAL_VECTOR ) return REAL_VECTOR is
    M: constant Integer := U'length(1);
    N: constant Integer := U'length(2);
    S:Real;
    X:Real_Vector(1..N);
    Tmp:Real_Vector(1..N);
  begin
    if V'length(1)/=N or V'length(2)/=N or W'length/=N or Y'length/=N then
      raise Array_Index_Error;
    end if ;
    for J in 1..N loop
      S := 0.0;
      if W(J-1+W'first)/=0.0 then
        for I in 1..M loop
          S := S+U(I-1+U'first(1),J-1+U'first(2))*Y(I-1+Y'first);
        end loop ;
        S := S/W(J-1+W'first);
      end if ;
      Tmp(J) := S;
    end loop ;
    for J in 1..N loop
      S := 0.0;
      for Jj in 1..N loop
        S := S+V(J-1+V'first(1),Jj-1+V'first(2))*Tmp(Jj);
      end loop ;
      X(J) := S;
    end loop ;
    return X;
  end SV_SOLVE;


end GENERIC_REAL_LINEAR_EQUATIONS ;
