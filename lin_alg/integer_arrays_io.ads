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

package Integer_Arrays_IO is

  use Integer_Arrays;

  package INTEGER_TYPE_IO is new Ada.Text_IO.INTEGER_IO (Integer_Type);
     -- to get DEFAULT_FORE, DEFAULT_AFT and DEFAULT_EXP for specifications
     -- later used in body for actual GET's and PUT's of array components

  procedure Get (FILE  : in  Ada.Text_IO.File_Type;
                 ITEM  : out Integer_Type;
                 WIDTH : in Ada.Text_IO.Field := 0)
                                                 renames INTEGER_TYPE_IO.GET;

  procedure Get (ITEM : out Integer_Type;
                 WIDTH : in Ada.Text_IO.Field := 0)
                                                 renames INTEGER_TYPE_IO.GET;

  procedure Put (FILE  : in  Ada.Text_IO.File_Type;
                 ITEM  : in Integer_Type;
                 WIDTH : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Width;
                 BASE  : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Base)
                                                 renames INTEGER_TYPE_IO.PUT;

  procedure Put (ITEM  : in Integer_Type;
                 WIDTH : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Width;
                 BASE  : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Base)
                                                 renames INTEGER_TYPE_IO.PUT;

  procedure Get (FROM : in String;
                 ITEM : out Integer_Type;
                 LAST : out Positive) renames INTEGER_TYPE_IO.GET;

  procedure Put (TO    : out String;
                 ITEM  : in Integer_Type;
                 BASE  : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Base)
                                                 renames INTEGER_TYPE_IO.PUT;

  procedure Put (A     : in Integer_Matrix ;
                 WIDTH : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Width;
                 BASE  : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Base);

  procedure Put (V     : in Integer_Vector ;
                 WIDTH : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Width;
                 BASE  : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Base);

  procedure Get (A     : out Integer_Matrix ;
                 WIDTH : in Ada.Text_IO.Field := 0);

  procedure Get (V     : out Integer_Vector ;
                 WIDTH : in Ada.Text_IO.Field := 0);

  procedure Put (FILE  : in  Ada.Text_IO.File_Type;
                 A     : in Integer_Matrix ;
                 WIDTH : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Width;
                 BASE  : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Base);

  procedure Put (FILE  : in  Ada.Text_IO.File_Type;
                 V     : in Integer_Vector ;
                 WIDTH : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Width;
                 BASE  : in Ada.Text_IO.Field := INTEGER_TYPE_IO.Default_Base);

  procedure Get (FILE  : in  Ada.Text_IO.File_Type;
                 A     : out Integer_Matrix ;
                 WIDTH : in Ada.Text_IO.Field := 0);

  procedure Get (FILE  : in  Ada.Text_IO.File_Type;
                 V     : out Integer_Vector ;
                 WIDTH : in Ada.Text_IO.Field := 0);

  End_Error : exception renames IO_Exceptions.End_Error ;

end Integer_Arrays_IO;
