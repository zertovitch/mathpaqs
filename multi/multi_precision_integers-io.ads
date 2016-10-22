------------------------------------------------------------------------------
--  File:            Multi_precision_integers-IO.ads
--  Description:     Child of package 'Multi_precision_integers: I/O
--  Date/version:    2006 ; 15-Feb-2002 / 22.XI.1999 / 22.12.1996
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------
with Text_IO;     use Text_IO;

package Multi_precision_integers.IO is

  Default_Base: Number_Base := 10;

  -- Returns the number of digits in the specified base:
  function Number_of_digits(i: Multi_int; base: Number_Base:= 10) return Natural;

  -- Returns the image of i in the specified base:
  function Str(i: Multi_int; base: Number_Base:= 10) return String;

  -- Returns the value of number in string:
  function Val(s: String) return Multi_int;

  -- Output to file, in block format:
  procedure Put_in_blocks(File  : in File_Type;
                          Item  : in Multi_int);

  -- Output to standard input, in block format:
  procedure Put_in_blocks(Item  : in Multi_int);

  ---- The following mimic the Text_IO.Integer_IO

  -- Get from file:
  procedure Get(File  : in  File_Type;
                Item  : out Multi_int;
                Width : in Field := 0);

  -- Get from standard input:
  procedure Get(Item  : out Multi_int;
                Width : in  Field := 0);

  -- Put to file:
  procedure Put(File  : in File_Type;
                Item  : in Multi_int;
                Width : in Field := 0;
                Base  : in Number_Base := Default_Base);
  -- Width=0 : no default formatting, since inpredicatble length

  -- Put to standard output:
  procedure Put(Item  : in Multi_int;
                Width : in Field := 0;
                Base  : in Number_Base := Default_Base);
  -- Width=0 : no default formatting, since inpredicatble length

  -- Get from string:
  procedure Get(From : in  String;
                Item : out Multi_int;
                Last : out Positive);

  -- Put to string:
  procedure Put(To   : out String;
                Item : in Multi_int;
                Base : in Number_Base := Default_Base);

end Multi_precision_integers.IO;
