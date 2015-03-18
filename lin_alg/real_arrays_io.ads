-- Author: Jon Squire
-- Original code here:
--   http://www.cs.umbc.edu/~squire/adaclass/gnatmath95/
-- Changes:
--   - Generic_Real_Arrays = Ada.Numerics.Generic_Real_Arrays (Ada 2005)

-- real_arrays_io.ads     generic package specification

-- instantiate: package Arrays_IO is new Real_Arrays_IO(Float, Real_Arrays);

with Ada.Numerics.Generic_Real_Arrays;
with Ada.Text_IO;
with Ada.IO_Exceptions;
 
generic 

  type Real is digits <>;
  with package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays(Real);

package Real_Arrays_IO is

  use Real_Arrays;

  package REAL_IO is new Ada.Text_IO.Float_IO(Real);
     -- to get DEFAULT_FORE, DEFAULT_AFT and DEFAULT_EXP for specifications
 
  procedure GET (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 ITEM  : out REAL;
                 WIDTH : in Ada.Text_IO.FIELD := 0) renames REAL_IO.GET;
 
  procedure GET (ITEM : out REAL; 
                 WIDTH : in Ada.Text_IO.FIELD := 0) renames REAL_IO.GET;
 
  procedure PUT (FILE : in  Ada.Text_IO.FILE_TYPE;
                 ITEM : in REAL;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP)
                                                   renames REAL_IO.PUT;
 
  procedure PUT (ITEM : in REAL;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP)
                                                   renames REAL_IO.PUT;
 
  procedure GET (FROM : in STRING; 
                 ITEM : out REAL;
                 LAST : out POSITIVE) renames REAL_IO.GET;
 
  procedure PUT (TO   : out STRING;
                 ITEM : in REAL;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD:= REAL_IO.DEFAULT_EXP)
                                                    renames REAL_IO.PUT;
 
  procedure GET (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 ITEM  : out REAL_VECTOR;
                 WIDTH : in Ada.Text_IO.FIELD := 0);
 
  procedure GET (ITEM  : out REAL_VECTOR ;
                 WIDTH : in Ada.Text_IO.FIELD := 0);

  procedure PUT (FILE : in  Ada.Text_IO.FILE_TYPE;
                 ITEM : in REAL_VECTOR;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP);
 
  procedure PUT (ITEM : in REAL_VECTOR ;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP);

  procedure GET (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 ITEM  : out REAL_MATRIX;
                 WIDTH : in Ada.Text_IO.FIELD := 0);
 
  procedure GET (ITEM  : out REAL_MATRIX ;
                 WIDTH : in Ada.Text_IO.FIELD := 0);

  procedure PUT (FILE : in  Ada.Text_IO.FILE_TYPE;
                 ITEM : in REAL_MATRIX;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP);

  procedure PUT (ITEM : in REAL_MATRIX ;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP);

  -- string intentionally omitted on vector and matrix

  End_Error : exception renames Ada.IO_EXCEPTIONS.END_ERROR ;
 
end Real_Arrays_IO;
