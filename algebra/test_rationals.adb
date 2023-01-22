------------------------------------------------------------------------------
--  File:            Test_Rationals.adb
--  Description:     Test for the "Rationals" package
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Text_IO,
     Ada.Integer_Text_IO;

with Rationals;

procedure Test_Rationals is

  use Ada.Text_IO, Ada.Integer_Text_IO;

  type Rational is new Rationals.frac_elt;

  r : Rational;

  procedure Aff (r : Rational) is
  begin
    Put ('('); Put (r.a, 0); Put (','); Put (r.b, 0); Put (") ");
  end Aff;

  procedure Montre_Add (r, s : Rational) is
  begin
    Aff (r); Put ('+'); Aff (s); Put ('='); Aff (r + s);
  end Montre_Add;

  procedure Montre_Add_RNR (r, s : Rational) is
  begin
    Rationals.auto_reduce := True;
    Montre_Add (r, s);
    Rationals.auto_reduce := False;
    Put (" auto_reduce:= false ");
    Montre_Add (r, s);
    New_Line;
  end Montre_Add_RNR;

begin
  r := (0, 1); Aff (r); New_Line;

  Montre_Add_RNR ((-2, 13), (15, 13));
  Montre_Add_RNR ((10, -35), (22, 21));

  --  More to test...
  New_Line;
  Put_Line ("Now, we provoke an exception with (1,0) / (0,1):");
  r := ((1, 0) / (0, 1));
end Test_Rationals;
