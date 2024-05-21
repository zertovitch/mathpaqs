-----------------------------------------------------------------------------
--  File: muprinio.adb; see specification (muprinio.ads)
-----------------------------------------------------------------------------

pragma Warnings (".I");

package body Multi_Precision_Integers.IO is

  package IIO is new Integer_IO (Index_Int);

  table : constant array (Basic_Int'(0) .. 15) of Character :=
         ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

  --  15-Feb-2002: Bugfix case i = 0.

  function Chiffres_i_non_nul (i : Multi_Int; base : Number_Base := 10) return Natural is
    nombre : Multi_Int (i.last_used);
    la_base    : constant Basic_Int :=  Basic_Int (base);
    nchiffres : Natural := 1;

    procedure Comptage_rapide (C : Positive) is
      test  : Multi_Int (i.n);
      base_puiss_C : constant Multi_Int := Multi (Basic_Int (base)) ** C;
    begin
      loop
        Fill (test, nombre / base_puiss_C);
        exit when test.zero;
        --  quotient non nul, donc on a au moins C chiffres
        Fill (nombre, test);
        nchiffres := nchiffres + C;
      end loop;
    end Comptage_rapide;

  begin
    Fill (nombre, i);
    Comptage_rapide (400);
    Comptage_rapide (20);
    loop
      Fill (nombre, nombre / la_base);
      exit when nombre.zero;
      nchiffres := nchiffres + 1;
    end loop;
    return nchiffres;
  end Chiffres_i_non_nul;

  function Number_of_Digits (i : Multi_Int; base : Number_Base := 10) return Natural is
  begin
    if i.zero then
      return 1;
    else
      return Chiffres_i_non_nul (i, base);
    end if;
  end Number_of_Digits;

  function Str (i : Multi_Int; base : Number_Base := 10) return String is
    res : String (1 .. 1 + Number_of_Digits (i, base)) := (others => 'x');
    nombre : Multi_Int (i.n) := i;
    chiffre : Basic_Int;
    la_base : constant Basic_Int :=  Basic_Int (base);

  begin
    if nombre.zero or else not nombre.neg then
      res (1) := ' ';
    else
      res (1) := '-';
    end if;
    nombre.neg := False;

    --  maintenant nombre et base sont >=0, MOD=REM
    for k in reverse 2 .. res'Last loop
      Div_Rem (nombre, la_base, nombre, chiffre);
      res (k) := table (chiffre);
      exit when nombre.zero;
    end loop;
    return res;

  end Str;

  --  !!! recursion !!!

  function Val (s : String) return Multi_Int is
    formatting_error : exception;
  begin
    if s = "" then
      return Multi (0);
    elsif s (s'First) = '-' then
      return -Val (s (s'First + 1 .. s'Last));
    elsif s (s'First) = '+' then
      return  Val (s (s'First + 1 .. s'Last));
    elsif s (s'Last) in '0' .. '9' then
      return Basic_Int'Value (s (s'Last .. s'Last)) + 10 *
             Val (s (s'First .. s'Last - 1));
    else
      raise formatting_error;
    end if;
  end Val;

  procedure Put_in_Blocks (File  : in File_Type;
                           Item  : in Multi_Int)
  is
  begin
    if Item.neg then Put (File, '-'); else Put (File, '+'); end if;
    Put (File, " [ ");
    IIO.Put (File, 1 + Item.n, 3);
    Put (File, " blocks ]: ");
    Put (File, '{');
    if Item.n > Item.last_used then
      IIO.Put (File, Item.n - Item.last_used, 3);
      Put (File, " unused |");
    end if;
    for k in reverse 0 .. Item.last_used loop
      Put (File, Block_Type'Image (Item.blk (k)));
      if k > 0 then Put (File, '|'); end if;
    end loop;
    Put (File, '}');
  end Put_in_Blocks;

  procedure Put_in_Blocks (Item  : in Multi_Int) is
  begin
    Put_in_Blocks (Current_Output, Item);
  end Put_in_Blocks;

  procedure Get (File  : in  File_Type;
                 Item  : out Multi_Int;
                 Width : in Field := 0)
  is
  begin
    null; -- !!!
  end Get;

  procedure Get (Item  : out Multi_Int;
                Width : in  Field := 0)
  is
  begin
    Get (Current_Input, Item, Width);
  end Get;

  procedure Put (File  : in File_Type;
                 Item  : in Multi_Int;
                 Width : in Field := 0;
                 Base  : in Number_Base := Default_Base)
  is
  begin
    if Width = 0 then       -- No padding required (default)
      Put (File, Str (Item, Base));
    else                    -- Left padding required -> slow
      declare
        la_chaine : String (1 .. Width);
      begin
        Put (la_chaine, Item, Base);
        Put (File, la_chaine);
      end;
    end if;
  end Put;

  procedure Put (Item  : in Multi_Int;
                 Width : in Field := 0;
                 Base  : in Number_Base := Default_Base)
  is
  begin
    Put (Current_Output, Item, Width, Base);
  end Put;

  procedure Get (From : in  String;
                 Item : out Multi_Int;
                 Last : out Positive)
  is
  pragma Unreferenced (From, Item);
  begin
    Last := 1;
    null; -- !!!
  end Get;

  procedure Put (To   : out String;
                Item : in Multi_Int;
                Base : in Number_Base := Default_Base)
  is
    nchiffres : constant Natural := Number_of_Digits (Item, Base);
    blancs : constant String (To'Range) := (others => ' ');
  begin
    if nchiffres > To'Length then
      raise Layout_Error;
    else
      To := blancs;
      To (To'Last - nchiffres .. To'Last) := Str (Item, Base);
    end if;
  end Put;

end Multi_Precision_Integers.IO;
