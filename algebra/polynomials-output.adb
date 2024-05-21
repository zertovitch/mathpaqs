-----------------------------------------------------------------------------
--  File: polyoutp.adb; see specification (polyoutp.ads)
-----------------------------------------------------------------------------

with Ada.Integer_Text_IO;

package body Polynomials.Output is

  use Ada.Text_IO;

  procedure Put (File : in Ada.Text_IO.File_Type; Item : in Polynomial; Var : in String) is
    d : constant Integer := Deg (Item);
    iti : Field_elt;
    autres_termes : Boolean := False;
  begin
    if d < 0 then                 --  polynome nul
      Field_Put (File, zero);
    else
      for i in reverse 0 .. d loop
        iti := Item (i);
        if iti /= zero then
          if autres_termes then
            if iti > zero then
              Put (File, " + ");
            else
              Put (File, " - "); iti := -iti;
            end if;
          else
            autres_termes := True;
          end if;
          if iti = -one then
            Put (File, "-");
          elsif i = 0 or iti /= one then
            Field_Put (File, iti);
          end if;
          case i is
            when 0 => null;
            when 1 => Put (File, " " & Var);
            when others =>
              Put (File, " " & Var & "^");
              Ada.Integer_Text_IO.Put (File, i, 0);
          end case;
        end if;
      end loop;
    end if;
  end Put;

  procedure Put (Item : in Polynomial; Var : in String) is
  begin
    Put (Current_Output, Item, Var);
  end Put;

end Polynomials.Output;
