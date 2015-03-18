-- generic_integer_arrays.adb    generic package body

package body Generic_Integer_Arrays is

-- SUBPROGRAMS for INTEGER_VECTOR TYPES --

  -- VECTOR arithmetic operations --

  function "+"  (RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR is
  begin
    return RIGHT ;
  end "+";

  function "-"  (RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR is
    C : INTEGER_VECTOR ( RIGHT'RANGE ) ;
  begin
    for I in RIGHT'RANGE loop
      C(I) := -RIGHT(I);
    end loop ;
    return C ;
  end "-";

  function "abs"  (RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR is
    C : INTEGER_VECTOR ( RIGHT'RANGE ) ;
  begin
    for I in RIGHT'RANGE loop
      C(I) := abs RIGHT(I);
    end loop ;
    return C ;
  end "abs";

  function "+"  (LEFT, RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR is
    C : INTEGER_VECTOR ( LEFT'RANGE ) ;
  begin
    if LEFT'LENGTH /= RIGHT'LENGTH then
      raise Constraint_Error with "Dimension error" ;
    end if ;
    for I in LEFT'RANGE loop
      C(I) := LEFT(I) + RIGHT(I-LEFT'FIRST+RIGHT'FIRST) ;
    end loop ;
    return C ;
  end "+";

  function "-"  (LEFT, RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR is
    C : INTEGER_VECTOR ( LEFT'RANGE ) ;
  begin
    if LEFT'LENGTH /= RIGHT'LENGTH then
      raise Constraint_Error with "Dimension error" ;
    end if ;
    for I in LEFT'RANGE loop
      C(I) := LEFT(I) - RIGHT(I-LEFT'FIRST+RIGHT'FIRST) ;
    end loop ;
    return C ;
  end "-";

  function "*"  (LEFT, RIGHT : INTEGER_VECTOR) return INTEGER_TYPE is
    X : INTEGER_TYPE := 0 ;
    TEMP : INTEGER_TYPE ;
  begin
    if LEFT'LENGTH /= RIGHT'LENGTH then
      raise Constraint_Error with "Dimension error" ;
    end if ;
    for I in LEFT'RANGE loop
      TEMP := LEFT(I) * RIGHT(I-LEFT'FIRST+RIGHT'FIRST) ;
      X := X + TEMP * TEMP ;
    end loop ;
    return X;
  end "*" ;

  function "*"  (LEFT, RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR is 
    C : INTEGER_VECTOR ( LEFT'RANGE ) ;
  begin
    if LEFT'LENGTH /= RIGHT'LENGTH then
      raise Constraint_Error with "Dimension error" ;
    end if ;
    for I in LEFT'RANGE loop
      C(I) := LEFT(I) * RIGHT(I-LEFT'FIRST+RIGHT'FIRST) ;
    end loop ;
    return C ;
  end "*";

  function "/"  (LEFT, RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR is
    C : INTEGER_VECTOR ( LEFT'RANGE ) ;
  begin
    if LEFT'LENGTH /= RIGHT'LENGTH then
      raise Constraint_Error with "Dimension error" ;
    end if ;
    for I in LEFT'RANGE loop
      C(I) := LEFT(I) / RIGHT(I-LEFT'FIRST+RIGHT'FIRST) ;
    end loop ;
    return C ;
  end "/";

  function "**"  (LEFT  : INTEGER_VECTOR; 
                  RIGHT : INTEGER) return INTEGER_VECTOR is
    C : INTEGER_VECTOR ( LEFT'RANGE ) ;
  begin
    for I in LEFT'RANGE loop
      C(I) := LEFT(I) ** RIGHT;
    end loop ;
    return C ;
  end "**";

  -- VECTOR scaling operations --

  function "*"  (LEFT  : INTEGER_TYPE;
                 RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR is
    C : INTEGER_VECTOR ( RIGHT'RANGE ) ;
  begin
    for I in RIGHT'RANGE loop
      C(I) := LEFT * RIGHT(I);
    end loop ;
    return C ;
  end "*";

  function "*"  (LEFT : INTEGER_VECTOR;
                 RIGHT : INTEGER_TYPE) return INTEGER_VECTOR is
    C : INTEGER_VECTOR ( LEFT'RANGE ) ;
  begin
    for I in LEFT'RANGE loop
      C(I) := LEFT(I) * RIGHT;
    end loop ;
    return C ;
  end "*";

  function "/"  (LEFT : INTEGER_VECTOR;
                 RIGHT : INTEGER_TYPE) return INTEGER_VECTOR is
    C : INTEGER_VECTOR ( LEFT'RANGE ) ;
  begin
    for I in LEFT'RANGE loop
      C(I) := LEFT(I) / RIGHT;
    end loop ;
    return C ;
  end "/";

  -- other operations --

  function UNIT_VECTOR (INDEX : INTEGER;
                        ORDER : NATURAL;
                        FIRST : INTEGER := 1 ) return INTEGER_VECTOR is
    C : INTEGER_VECTOR ( FIRST..FIRST+ORDER-1) ;
  begin
    if INDEX < FIRST or INDEX > FIRST+ORDER-1 then
      raise Constraint_Error with "Dimension error";
    end if;
    for I in C'RANGE loop
      C(I) := 0;
    end loop ;
    C(INDEX) := 1;
    return C ;
  end UNIT_VECTOR;

-- SUBPROGRAMS for INTEGER_MATRIX TYPES --

  -- MATRIX arithmetic operations --

  function "+"  (RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX is
  begin
    return RIGHT ;
  end "+";

  function "-"  (RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX is
    C : INTEGER_MATRIX ( RIGHT'RANGE(1), RIGHT'RANGE(2)) ;
  begin
    for I in RIGHT'RANGE(1) loop
      for J in RIGHT'RANGE(2) loop
        C(I,J) := -RIGHT(I,J);
      end loop ;
    end loop ;
    return C ;
  end "-";

  function "abs" (RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX is
    C : INTEGER_MATRIX ( RIGHT'RANGE(1), RIGHT'RANGE(2)) ;
  begin
    for I in RIGHT'RANGE(1) loop
      for J in RIGHT'RANGE(2) loop
        C(I,J) := abs RIGHT(I,J);
      end loop ;
    end loop ;
    return C ;
  end "abs";

  function TRANSPOSE (X : INTEGER_MATRIX) return INTEGER_MATRIX is
    C : INTEGER_MATRIX ( X'RANGE(2), X'RANGE(1)) ;
  begin
    for I in X'RANGE(1) loop
      for J in X'RANGE(2) loop
        C(J,I) :=  X(I,J);
      end loop ;
    end loop ;
    return C ;
  end TRANSPOSE;



  function "+"  (LEFT, RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX is
    C : INTEGER_MATRIX ( LEFT'RANGE(1), LEFT'RANGE(2)) ;
  begin
    if LEFT'LENGTH(1) /= RIGHT'LENGTH(1) or
       LEFT'LENGTH(2) /= RIGHT'LENGTH(2) then
      raise Constraint_Error with "Dimension error" ;
    end if ;
    for I in LEFT'RANGE(1) loop
      for J in LEFT'RANGE(2) loop
        C(I,J) := LEFT(I,J) + RIGHT(I-LEFT'FIRST(1)+RIGHT'FIRST(1),
                                    J-LEFT'FIRST(2)+RIGHT'FIRST(2)) ;
      end loop ;
    end loop ;
    return C ;
  end "+";

  function "-"  (LEFT, RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX is
    C : INTEGER_MATRIX ( LEFT'RANGE(1), LEFT'RANGE(2)) ;
  begin
    if LEFT'LENGTH(1) /= RIGHT'LENGTH(1) or
       LEFT'LENGTH(2) /= RIGHT'LENGTH(2) then
      raise Constraint_Error with "Dimension error" ;
    end if ;
    for I in LEFT'RANGE(1) loop
      for J in LEFT'RANGE(2) loop
        C(I,J) := LEFT(I,J) - RIGHT(I-LEFT'FIRST(1)+RIGHT'FIRST(1),
                                    J-LEFT'FIRST(2)+RIGHT'FIRST(2)) ;
      end loop ;
    end loop ;
    return C ;
  end "-";

  function "*"  (LEFT, RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX is
    C : INTEGER_MATRIX ( LEFT'RANGE(1) , RIGHT'RANGE(2)) ;
  begin
    if LEFT'LENGTH(2) /= RIGHT'LENGTH(1) then
      raise Constraint_Error with "Dimension error" ;
    end if ;
    for I in LEFT'RANGE(1) loop
      for J in RIGHT'RANGE(2) loop
        C(I,J) := 0 ;
        for K in LEFT'RANGE(2) loop
          C(I,J) := C(I,J) + LEFT(I,K) * 
                             RIGHT(K-LEFT'FIRST(2)+RIGHT'FIRST(1),J) ;
        end loop ;
      end loop ;
    end loop ;
    return C ;
  end "*" ;

  function "*"  (LEFT  : INTEGER_VECTOR;
                 RIGHT : INTEGER_MATRIX) return INTEGER_VECTOR is
    CV : INTEGER_VECTOR ( RIGHT'RANGE(2)) ;
    SUM : INTEGER_TYPE ;
  begin
    if LEFT'LENGTH /= RIGHT'LENGTH(1) then
      raise Constraint_Error with "Dimension error" ;
    end if ;
    for J in RIGHT'RANGE(2) loop
      SUM := 0 ;
      for I in RIGHT'RANGE(1) loop
        SUM := SUM + LEFT(I-RIGHT'FIRST(1)+LEFT'FIRST) * RIGHT(I,J) ;
      end loop ;
      CV(J) := SUM ;
    end loop ;
    return CV ;
  end "*";

  function "*"  (LEFT  : INTEGER_MATRIX;
                 RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR is
    CV : INTEGER_VECTOR ( LEFT'RANGE(1)) ;
    SUM : INTEGER_TYPE ;
  begin
    if LEFT'LENGTH(2) /= RIGHT'LENGTH then
      raise Constraint_Error with "Dimension error" ;
    end if ;
    for I in LEFT'RANGE(1) loop
      SUM := 0 ;
      for J in LEFT'RANGE(2) loop
        SUM := SUM + RIGHT(J-LEFT'FIRST(2)+RIGHT'FIRST) * LEFT(I,J) ;
      end loop ;
      CV(I) := SUM ;
    end loop ;
    return CV ;
  end "*";

  function "*"  (LEFT, RIGHT : INTEGER_VECTOR) return INTEGER_MATRIX is
    C : INTEGER_MATRIX ( LEFT'RANGE, RIGHT'RANGE) ;
  begin
    for I in LEFT'RANGE loop
      for J in RIGHT'RANGE loop
        C(I,J) :=  LEFT(I) * RIGHT(J);
      end loop ;
    end loop ;
    return C ;
  end "*";

  -- MATRIX scaling operations --

  function "*"  (LEFT  : INTEGER_TYPE;
                 RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX is
    C : INTEGER_MATRIX ( RIGHT'RANGE(1), RIGHT'RANGE(2)) ;
  begin
    for I in RIGHT'RANGE(1) loop
      for J in RIGHT'RANGE(2) loop
        C(I,J) := LEFT * RIGHT(I,J);
      end loop ;
    end loop ;
    return C ;
  end "*";

  function "*"  (LEFT  : INTEGER_MATRIX;
                 RIGHT : INTEGER_TYPE) return INTEGER_MATRIX is
    C : INTEGER_MATRIX ( LEFT'RANGE(1), LEFT'RANGE(2)) ;
  begin
    for I in LEFT'RANGE(1) loop
      for J in LEFT'RANGE(2) loop
        C(I,J) := LEFT(I,J)* RIGHT;
      end loop ;
    end loop ;
    return C ;
  end "*";

  function "/"  (LEFT  : INTEGER_MATRIX;
                 RIGHT : INTEGER_TYPE) return INTEGER_MATRIX is
    C : INTEGER_MATRIX ( LEFT'RANGE(1), LEFT'RANGE(2)) ;
  begin
    for I in LEFT'RANGE(1) loop
      for J in LEFT'RANGE(2) loop
        C(I,J) := LEFT(I,J) / RIGHT;
      end loop ;
    end loop ;
    return C ;
  end "/";

  -- other operations --

  function IDENTITY_MATRIX (ORDER : NATURAL;
                            FIRST_1,FIRST_2 : INTEGER := 1)
                            return INTEGER_MATRIX is
    A : INTEGER_MATRIX ( FIRST_1..FIRST_1+ORDER-1, FIRST_2..FIRST_2+ORDER-1) ;
  begin
    for I in A'RANGE(1) loop
      for J in A'RANGE(2) loop
        A(I,J) := 0 ;
      end loop ;
      A(I,I-A'FIRST(1)+A'FIRST(2)) := 1 ;
    end loop ;
    return A ;
  end IDENTITY_MATRIX ;

end Generic_Integer_Arrays;
