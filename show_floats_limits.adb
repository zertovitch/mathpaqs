with Ada.Text_IO;
with System;

procedure Show_Floats_Limits is

  generic
    type F is digits <>;
    fp_type_name : String;
  procedure Show_Limits;

  procedure Show_Limits is
    use Ada.Text_IO;
  begin
    Put_Line (fp_type_name);
    Put_Line ("    Largest positive number  . . . . " & F'Image (F'Last));
    Put_Line ("    Largest negative number  . . . . " & F'Image (F'First));
    Put_Line ("    Model_Small  . . . . . . . . . . " & F'Image (F'Model_Small));
    Put_Line ("    Epsilon (Ada 83) . . . . . . . . " & F'Image (F'Epsilon));
    Put_Line ("    Model_Epsilon  . . . . . . . . . " & F'Image (F'Model_Epsilon));
    Put_Line ("    Significant decimal digits . . . " & Integer'Image (F'Digits));
    Put_Line ("    Storage bits . . . . . . . . . . " & Integer'Image (F'Size));
    Put_Line ("    Storage bytes  . . . . . . . . . " & Integer'Image (F'Size / 8));
    New_Line;
  end Show_Limits;

  --  In many cases Float is the IEEE single precision type (32 bits, i.e. 4 bytes).
  --  Single precision accumulates quickly significant rounding errors.
  --  It should to be used only in special cases where memory is scarse.
  --
  procedure S_F          is new Show_Limits (Float,      "Float");

  --  Often, Long_Float is the IEEE double precision type (64 bits, i.e. 8 bytes).
  --
  procedure S_LF         is new Show_Limits (Long_Float, "Long_Float");

  --  Often, the "Max_Digits" type is an internal/proprietary format.
  --
  type Max_Digits is digits System.Max_Digits;
  procedure S_Max_Digits is new Show_Limits (Max_Digits, "Max_Digits");

begin
  S_F;
  S_LF;
  S_Max_Digits;
end Show_Floats_Limits;
