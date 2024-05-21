with Ada.Text_IO; use Ada.Text_IO;
--  !! will disappear in favour of Ada.Exceptions

with Multi_Precision_Integers.IO;

pragma Warnings ("C");

package body Multi_Precision_Integers.Check is

  package IOi renames Multi_Precision_Integers.IO;

  --  Don't be afraid by the bug reporting tools,
  --  they are unlikely to pop out of a programme ;-)

  Bug_file : Ada.Text_IO.File_Type;
  Bug_file_name : constant String := "multi_precision_integers.bug";

  --  The flaw detection (DEBUG mode) could suffer from
  --  a chicken-and-egg problem: e.g. "*" works, but the "/"
  --  to verify it doesn't !

  Multiply_flawed_div1_has_rest       : exception;
  Multiply_flawed_div1_wrong_quotient : exception;
  Multiply_flawed_div2_has_rest       : exception;
  Multiply_flawed_div2_wrong_quotient : exception;

  Div_Rem_flawed : exception;

  procedure Open_Bug_Report is
  begin
    Create (Bug_file, Out_File, Bug_file_name);
    Put_Line (Bug_file, "Bug in Multi_precision_integers");
  end Open_Bug_Report;

  procedure Close_Bug_Report is
  begin
    Close (Bug_file);
    --  These console messages can provoke a silent quit in GUI apps,
    --  so we put them after closing the report.
    Put_Line ("Bug in Multi_precision_integers !");
    Put_Line ("For details, read file: " & Bug_file_name);
  end Close_Bug_Report;

  procedure Test (m : Multi_Int; test_last : Boolean := True) is
    last_nz : Index_Int := 0;
    Negative_block,
    Last_index_has_zero,
    Field_last_outside_range, Field_last_is_negative : exception;
  begin
    if m.zero then return; end if; -- 0, nothing to test
    if m.last_used > m.n then raise Field_last_outside_range; end if;
    if m.last_used <   0 then raise Field_last_is_negative; end if;
    for i in 0 .. m.last_used loop
      if m.blk (i) < 0 then
        raise Negative_block;
      end if;
      if m.blk (i) /= 0 then
        last_nz := i;
      end if;
    end loop;
    if test_last and then 0 < last_nz and then last_nz < m.last_used then
      raise Last_index_has_zero;
    end if;
  end Test;

  procedure Check_Multiplication (i1, i2, i3 : in Multi_Int) is
    jeu : constant := 5; -- 0 suffit
    q1 : Multi_Int (i2.last_used + jeu);
    r1 : Multi_Int (i1.last_used + i2.last_used + jeu);
    q2 : Multi_Int (jeu);
    r2 : Multi_Int (i2.last_used + jeu);

    procedure Bug_Report is
    begin
      Open_Bug_Report;
      Put_Line (Bug_file, "Multiply_and_verify");
      Put (Bug_file, "i1 ="); IOi.Put_in_Blocks (Bug_file, i1); New_Line (Bug_file);
      Put (Bug_file, "i2 ="); IOi.Put_in_Blocks (Bug_file, i2); New_Line (Bug_file);
      Put (Bug_file, "i3 ="); IOi.Put_in_Blocks (Bug_file, i3); New_Line (Bug_file);
    end Bug_Report;

  begin
    Test (i1);
    Test (i2);

    if not (i1.zero or i2.zero) then
      --  Now we divide i3 by i1, q1 should be = i2
      Div_Rem_internal_both_export (i3, i1, q1, r1);
      if not r1.zero then
        Bug_Report;
        Close_Bug_Report;
        raise Multiply_flawed_div1_has_rest;
      end if;
      if not Equal (q1, i2) then
        Bug_Report;
        Put (Bug_file, "q1 ="); IOi.Put_in_Blocks (Bug_file, q1); New_Line (Bug_file);
        Close_Bug_Report;
        raise Multiply_flawed_div1_wrong_quotient;
      end if;
      --  Now we divide q1 by i2, should be = 1
      Div_Rem_internal_both_export (q1, i2, q2, r2);
      if not r2.zero then
        Bug_Report;
        Close_Bug_Report;
        raise Multiply_flawed_div2_has_rest;
      end if;
      if not Equal (q2, Multi (1)) then
        Bug_Report;
        Put (Bug_file, "q2 ="); IOi.Put_in_Blocks (Bug_file, q1); New_Line (Bug_file);
        Close_Bug_Report;
        raise Multiply_flawed_div2_wrong_quotient;
      end if;
    end if;

  end Check_Multiplication;

  procedure Check_Div_Rem (i1, i2, q, r : in Multi_Int) is

    procedure Bug_Report is
    begin
      Open_Bug_Report;
      Put_Line (Bug_file, "Div_Rem_and_verify");
      Put (Bug_file, "i1 ="); IOi.Put_in_Blocks (Bug_file, i1); New_Line (Bug_file);
      Put (Bug_file, "i2 ="); IOi.Put_in_Blocks (Bug_file, i2); New_Line (Bug_file);
      Put (Bug_file, "q  ="); IOi.Put_in_Blocks (Bug_file, q); New_Line (Bug_file);
      Put (Bug_file, "r  ="); IOi.Put_in_Blocks (Bug_file, r); New_Line (Bug_file);
    end Bug_Report;

  begin
    Test (i1);
    Test (i2);

    if not Equal (i1, i2 * q + r) then
      Bug_Report;
      Close_Bug_Report;
      raise Div_Rem_flawed;
    end if;

    Test (q);
    Test (r);
  end Check_Div_Rem;

end Multi_Precision_Integers.Check;
