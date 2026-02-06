------------------------------------------------------------------------------
--  File:            ppm2func.adb
--  Description:     .PPM / .PGM -> Ada function
--
--  Transforms a PPM (Portable pixelmap) RGB image or
--  a PGM (Portable greymap) into an Ada 'greyscale' function
--  f(x,y) with values in [0,1] and arguments (x,y) in R x R.
--  The image stands in the [0,1] x [0,1] square. Elsewhere, values are 0.
--
--  Syntax: ppm2func input_file function_name
--
--  Author:          G. de Montmollin
--  Version:         6-Feb-2026; 5-Sep-2007; 25-Apr-2001
------------------------------------------------------------------------------

with Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Float_Text_IO,
     Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Text_IO;

procedure PPM2Func is

  use
    Ada.Characters.Handling, Ada.Command_Line, Ada.Text_IO,
    Ada.Integer_Text_IO, Ada.Float_Text_IO, Ada.Strings, Ada.Strings.Fixed;

  i, o : File_Type;
  mx, my, R, G, B : Integer;

  Unknown_Format : exception;

  s : String (1 .. 100);

  l, max_value : Natural;

  RGB : Boolean;

  function Float_Name return String is
  (if Argument_Count < 3 then "Float" else Argument (3));

begin
  if Argument_Count < 2 then
    Put_Line ("Transforms a PPM (Portable pixelmap) RGB image or");
    Put_Line ("a PGM (Portable greymap) into an Ada 'greyscale' function");
    Put_Line ("f(x,y) with values in [0,1] and arguments (x,y) in R x R.");
    Put_Line ("The image stands in the [0,1] x [0,1] square.");
    Put_Line ("Elsewhere, values are 0.");
    New_Line;
    Put_Line ("Syntax: ppm2func input_file function_name [float_name]");
  else
    Open (i, In_File, Argument (1));
    Get_Line (i, s, l);

    if l /= 2 or else s (1) /= 'P' then
      raise Unknown_Format;
    end if;

    case s (2) is
      when '3' => RGB := True;   --  Pixelmap
      when '2' => RGB := False;  --  Greymap
      when others => raise Unknown_Format;
    end case;

    --  Eventual comment like: # Created by Paint Shop Pro
    Get_Line (i, s, l);
    if l > 0 and then s (1) = '#' then
      null;  --  just go on
    else
      --  We ate a line too much
      Close (i);
      Open (i, In_File, Argument (1));
      Skip_Line (i);
    end if;

    Get (i, mx);
    Get (i, my);
    Get (i, max_value);

    Create (o, Out_File, To_Lower (Argument (2)) & ".adb");

    Put_Line (o, "  function " & Argument (2) &
                 " (x, y : " & Float_Name & ") return " & Float_Name & " is");
    Put_Line (o, "    --  Output of graphic converter ppm2func.");
    Put (o,      "    --  Command line: ppm2func");
    for a in 1 .. Argument_Count loop
      Put (o, ' ' & Argument (a));
    end loop;
    New_Line (o);
    Put (o,      "    --  Image size & maximum value: ");
    Put (o, mx, 0); Put (o, " x ");
    Put (o, my, 0); Put (o, " x ");
    Put (o, max_value, 0);
    Put_Line (o, ".   RGB: " & Boolean'Image (RGB) & '.');
    Put_Line (o, "    --  The image stands in the [0, 1] x [0, 1] square, with");
    Put_Line (o, "    --  values in the [0, 1] range. Elsewhere, values are 0.");

    Put (o, "    a : constant array (0 .. ");
    Put (o, my - 1, 0);
    Put (o, ", 0 .. ");
    Put (o, mx - 1, 0);
    Put_Line (o, ") of " & Float_Name & " :=");

    for y in 1 .. my loop
      if y = 1 then
        Put (o, "    ((");
      else
        Put (o, "     (");
      end if;
      declare
        v : array (1 .. mx) of Float;
        v_min : Float := Float'Last;
        v_max : Float := Float'First;
        identical : Boolean;
      begin
        for x in 1 .. mx loop
          Get (i, R);
          if RGB then
            Get (i, G);
            Get (i, B);
          else
            G := R;
            B := R;
          end if;
          v (x) := (Float (R) + Float (G) + Float (B)) / (Float (max_value) * 3.0);
          v_min := Float'Min (v_min, v (x));
          v_max := Float'Max (v_max, v (x));
        end loop;
        identical := abs (v_min - v_max) < Float'Small;
        if identical then
          Put (o, "others => ");
        end if;
        for x in 1 .. mx loop
          Put (s, v (x), 3, 0);
          Put (o, Trim (s, Left));
          exit when identical;
          if x < mx then Put (o, ','); end if;
          if x mod (75 / 7) = 0 then
            New_Line (o);
            Put (o, "      ");
          elsif x < mx then
            Put (o, ' ');
          end if;
        end loop;
      end;
      Put (o, ')');
      if y < my then Put (o, ','); else Put (o, ");"); end if;
      New_Line (o);
    end loop;
    Close (i);

    New_Line (o);
    Put_Line (o, "  begin");
    Put_Line (o, "    if x < 0.0 or else x > 1.0 or else y < 0.0 or else y > 1.0 then");
    Put_Line (o, "      return 0.0;");
    Put_Line (o, "    else");
    Put (o,     "      return a (Integer ((1.0 - y) * " & Float_Name & " (");
    Put (o, my - 1, 0);
    Put_Line (o, ")),");
    Put (o,     "                Integer        (x  * " & Float_Name & " (");
    Put (o, mx - 1, 0);
    Put_Line (o, ")));");
    Put_Line (o, "    end if;");
    Put_Line (o, "  end " & Argument (2) & ';');
    Close (o);
  end if;
end PPM2Func;
