-- Author: Jon Squire
-- Original code here:
--   http://www.cs.umbc.edu/~squire/adaclass/gnatmath95/

-- integer_arrays_io.ads   generic package specification

-- instantiate: package Arrays_IO is new
--                             Integer_Arrays_IO(Integer, Integer_Arrays);

with Ada.Text_IO;
with IO_Exceptions;
with Generic_Integer_Arrays;

generic 
 
  type Integer_Type is range <>;
  with package Integer_Arrays is new Generic_Integer_Arrays(Integer_Type);


package INTEGER_ARRAYS_IO is
 
  use Integer_Arrays;

  package INTEGER_TYPE_IO is new Ada.Text_IO.INTEGER_IO(INTEGER_TYPE);
     -- to get DEFAULT_FORE, DEFAULT_AFT and DEFAULT_EXP for specifications
     -- later used in body for actual GET's and PUT's of array components
 
  procedure GET (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 ITEM  : out INTEGER_TYPE;
                 WIDTH : in Ada.Text_IO.FIELD := 0)
                                                 renames INTEGER_TYPE_IO.GET;
 
  procedure GET (ITEM : out INTEGER_TYPE; 
                 WIDTH : in Ada.Text_IO.FIELD := 0)
                                                 renames INTEGER_TYPE_IO.GET;
 
  procedure PUT (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 ITEM  : in INTEGER_TYPE;
                 WIDTH : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_WIDTH;
                 BASE  : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_BASE)
                                                 renames INTEGER_TYPE_IO.PUT;
 
  procedure PUT (ITEM : in INTEGER_TYPE;
                 WIDTH : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_WIDTH;
                 BASE  : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_BASE)
                                                 renames INTEGER_TYPE_IO.PUT;
 
  procedure GET (FROM : in STRING; 
                 ITEM : out INTEGER_TYPE;
                 LAST : out POSITIVE) renames INTEGER_TYPE_IO.GET;
 
  procedure PUT (TO   : out STRING;
                 ITEM : in INTEGER_TYPE;
                 BASE  : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_BASE)
                                                 renames INTEGER_TYPE_IO.PUT;
 

  procedure PUT (A    : in INTEGER_MATRIX ;
                 WIDTH : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_WIDTH;
                 BASE  : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_BASE);

  procedure PUT (V    : in INTEGER_VECTOR ;
                 WIDTH : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_WIDTH;
                 BASE  : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_BASE);

  procedure GET (A     : out INTEGER_MATRIX ;
                 WIDTH : in Ada.Text_IO.FIELD := 0);

  procedure GET (V     : out INTEGER_VECTOR ;
                 WIDTH : in Ada.Text_IO.FIELD := 0);


  procedure PUT (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 A    : in INTEGER_MATRIX ;
                 WIDTH : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_WIDTH;
                 BASE  : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_BASE);

  procedure PUT (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 V    : in INTEGER_VECTOR ;
                 WIDTH : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_WIDTH;
                 BASE  : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_BASE);

  procedure GET (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 A     : out INTEGER_MATRIX ;
                 WIDTH : in Ada.Text_IO.FIELD := 0);

  procedure GET (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 V     : out INTEGER_VECTOR ;
                 WIDTH : in Ada.Text_IO.FIELD := 0);

  End_Error : exception renames IO_EXCEPTIONS.END_ERROR ;
 
end Integer_Arrays_IO;
