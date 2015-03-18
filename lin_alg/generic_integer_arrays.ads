-- Author: Jon Squire
-- Original code here:
--   http://www.cs.umbc.edu/~squire/adaclass/gnatmath95/
-- Changes:
--   - removed dependency on Array_Exceptions

-- generic_integer_arrays.ads   generic package specification

-- instantiate: package Integer_Arrays is new
--                         Generic_Integer_Arrays(Integer_Type);

generic

  type Integer_Type is range <>;

package Generic_Integer_Arrays is

-- TYPES --

  type Integer_Vector is array (Integer range <>) of Integer_Type;
  type Integer_Matrix is array (Integer range <>, 
                                Integer range <>) of Integer_Type;

-- SUBPROGRAMS for INTEGER_VECTOR TYPES --

  -- VECTOR arithmetic operations --

  function "+" (RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR;
  function "-" (RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR;
  function "abs" (RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR; 

  function "+" (LEFT, RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR;
  function "-" (LEFT, RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR;
  function "*" (LEFT, RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR; 
  function "/" (LEFT, RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR;
  function "**" (LEFT  : INTEGER_VECTOR; 
                 RIGHT : INTEGER) return INTEGER_VECTOR; 

  function "*" (LEFT, RIGHT : INTEGER_VECTOR) return INTEGER_TYPE; 

  -- VECTOR scaling operations --

  function "*" (LEFT  : INTEGER_TYPE;
                RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR;
  function "*" (LEFT  : INTEGER_VECTOR;
                RIGHT : INTEGER_TYPE) return INTEGER_VECTOR;
  function "/" (LEFT  : INTEGER_VECTOR;
                RIGHT : INTEGER_TYPE) return INTEGER_VECTOR;

  -- other operations --

  function UNIT_VECTOR (INDEX : INTEGER;
                        ORDER : NATURAL;
                        FIRST : INTEGER := 1) return INTEGER_VECTOR;  

-- SUBPROGRAMS for INTEGER_MATRIX TYPES --

  -- MATRIX arithmetic operations --

  function "+" (RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX;
  function "-" (RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX;
  function "abs" (RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX; 
  function TRANSPOSE (X : INTEGER_MATRIX) return INTEGER_MATRIX; 

  function "+" (LEFT, RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX;
  function "-" (LEFT, RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX;
  function "*" (LEFT, RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX;

  function "*" (LEFT, RIGHT : INTEGER_VECTOR) return INTEGER_MATRIX; 
  function "*" (LEFT  : INTEGER_VECTOR;
                RIGHT : INTEGER_MATRIX) return INTEGER_VECTOR;
  function "*" (LEFT  : INTEGER_MATRIX;
                RIGHT : INTEGER_VECTOR) return INTEGER_VECTOR;

  -- MATRIX scaling operations --

  function "*" (LEFT  : INTEGER_TYPE;
                RIGHT : INTEGER_MATRIX) return INTEGER_MATRIX;
  function "*" (LEFT  : INTEGER_MATRIX;
                RIGHT : INTEGER_TYPE) return INTEGER_MATRIX;
  function "/" (LEFT  : INTEGER_MATRIX;
                RIGHT : INTEGER_TYPE) return INTEGER_MATRIX;

  -- other operations --

  function IDENTITY_MATRIX (ORDER : NATURAL;
                        FIRST_1, FIRST_2 : INTEGER := 1) return INTEGER_MATRIX;

end Generic_Integer_Arrays;
