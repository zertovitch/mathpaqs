--  Author: Jon Squire
--  Original code here:
--    http://www.cs.umbc.edu/~squire/adaclass/gnatmath95/
--  Changes:
--    - Generic_Real_Arrays = Ada.Numerics.Generic_Real_Arrays (Ada 2005)

--  real_arrays_io.ads     generic package specification

--  instantiate: package Arrays_IO is new Real_Arrays_IO(Float, Real_Arrays);

with Ada.Numerics.Generic_Real_Arrays;
with Ada.Text_IO;
with Ada.IO_Exceptions;

generic

  type Real is digits <>;
  with package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (Real);

package Real_Arrays_IO is

  package Real_IO is new Ada.Text_IO.Float_IO (Real);
     --  to get DEFAULT_FORE, DEFAULT_AFT and DEFAULT_EXP for specifications

  procedure Get (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 ITEM  : out Real_Arrays.REAL_VECTOR;
                 WIDTH : in Ada.Text_IO.FIELD := 0);
 
  procedure Get (ITEM  : out Real_Arrays.REAL_VECTOR ;
                 WIDTH : in Ada.Text_IO.FIELD := 0);

  procedure Put (FILE : in  Ada.Text_IO.FILE_TYPE;
                 ITEM : in Real_Arrays.REAL_VECTOR;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP);
 
  procedure Put (ITEM : in Real_Arrays.REAL_VECTOR ;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP);

  procedure Get (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 ITEM  : out Real_Arrays.REAL_MATRIX;
                 WIDTH : in Ada.Text_IO.FIELD := 0);
 
  procedure Get (ITEM  : out Real_Arrays.REAL_MATRIX ;
                 WIDTH : in Ada.Text_IO.FIELD := 0);

  procedure Put (FILE : in  Ada.Text_IO.FILE_TYPE;
                 ITEM : in Real_Arrays.REAL_MATRIX;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP);

  procedure Put (ITEM : in Real_Arrays.REAL_MATRIX ;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP);

  -- string intentionally omitted on vector and matrix

  End_Error : exception renames Ada.IO_EXCEPTIONS.END_ERROR ;
 
end Real_Arrays_IO;
