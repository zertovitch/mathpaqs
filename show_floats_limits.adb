with Ada.Text_IO;
with System;

procedure Show_floats_limits is

  type Digits_Max is digits System.Max_Digits;  --  15 or 18 on Intel

  generic
    type F is digits <>;
    name : String;
  procedure Show_limits;

  procedure Show_limits is
    use Ada.Text_IO;
  begin
    Put_Line (name);
    Put_Line ("    Largest positive number  . . . . " & F'Image (F'Last));
    Put_Line ("    Largest negative number  . . . . " & F'Image (F'First));
    Put_Line ("    Model_Small  . . . . . . . . . . " & F'Image (F'Model_Small));
    Put_Line ("    Epsilon (Ada 83 only)  . . . . . " & F'Image (F'Epsilon));
    Put_Line ("    Model_Epsilon  . . . . . . . . . " & F'Image (F'Model_Epsilon));
    Put_Line ("    Significant decimal digits . . . " & Integer'Image (F'Digits));
    Put_Line ("    Storage bits . . . . . . . . . . " & Integer'Image (F'Size));
    Put_Line ("    Storage bytes  . . . . . . . . . " & Integer'Image (F'Size / 8));
    New_Line;
  end Show_limits;

  --  In many cases Float is the IEEE single precision type (32 bits, i.e. 4 bytes).
  --  Single precision accumulates quickly significant rounding errors.
  --  It should to be used only in special cases where memory is scarse.
  --
  procedure SF   is new Show_limits (Float,           "Float");

  --  Often, Long_Float is the IEEE double precision type (64 bits, i.e. 8 bytes).
  --
  procedure SLF  is new Show_limits (Long_Float,      "Long_Float");

  procedure SDMax is new Show_limits (Digits_Max,     "Digits_Max");

begin
  SF;
  SLF;
  SDMax;
end Show_floats_limits;
