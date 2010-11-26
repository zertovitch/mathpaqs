-----------------------------------------------------------------------------
--  File: muprinio.adb; see specification (muprinio.ads)
-----------------------------------------------------------------------------

package body Multi_precision_integers.IO is

  package IIO is new Integer_IO( index_int );

  table: constant array(basic_int'(0)..15) of Character:=
         ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

   -- 15-Feb-2002: Bugfix case i=0. Spotted by Duncan Sands

  function Chiffres_i_non_nul(i: multi_int; base: number_base:= 10) return Natural is
    nombre: multi_int(i.last_used);
    la_base    : constant basic_int :=  basic_int(base);
    nchiffres: Natural:= 1;

    procedure Comptage_rapide( C: Positive ) is
      test  : multi_int(i.n);
      base_puiss_C: constant multi_int:= Multi( basic_int(base) ) ** C;
    begin
      loop
        Fill(test, nombre / base_puiss_C );
        exit when test.zero;
        -- quotient non nul, donc on a au moins C chiffres
        Fill(nombre, test);
        nchiffres:= nchiffres + C;
      end loop;
    end Comptage_rapide;

  begin
    Fill(nombre, i);
    Comptage_rapide( 400 );
    Comptage_rapide( 20 );
    loop
      Fill(nombre, nombre / la_base);
      exit when nombre.zero;
      nchiffres:= nchiffres + 1;
    end loop;
    return nchiffres;
  end Chiffres_i_non_nul;

  function Number_of_digits(i: multi_int; base: number_base:= 10) return Natural is
  begin
    if i.zero then
      return 1;
    else
      return Chiffres_i_non_nul(i,base);
    end if;
  end Number_of_digits;

  function Str(i: multi_int; base: number_base:= 10) return String is
    res: String(1..1 + Number_of_digits(i,base)):= (others=> 'x');
    nombre : multi_int(i.n):= i;
    chiffre: basic_int;
    la_base: constant basic_int :=  basic_int(base);

  begin
    if nombre.zero or else not nombre.neg then
      res(1):= ' ';
    else
      res(1):= '-';
    end if;
    nombre.neg:= False;

    -- maintenant nombre et base sont >=0, MOD=REM
    for k in reverse 2 .. res'Last loop
      Div_Rem( nombre, la_base, nombre, chiffre );
      res(k):= table( chiffre );
      exit when nombre.zero;
    end loop;
    return res;

  end Str;


-- !!! recursion !!!

  function Val(s: String) return multi_int is
    formatting_error: exception;
  begin
    if s="" then
      return Multi(0);
    elsif s(s'First)='-' then
      return -Val(s(s'First+1..s'Last));
    elsif s(s'First)='+' then
      return  Val(s(s'First+1..s'Last));
    elsif s(s'Last) in '0'..'9' then
      return basic_int'Value(s(s'Last..s'Last)) + 10 *
             Val(s(s'First..s'Last-1));
    else
      raise formatting_error;
    end if;
  end Val;

  procedure Put_in_blocks(File  : in File_Type;
                          Item  : in multi_int) is
  begin
    if Item.neg then put(File,'-'); else put(File,'+'); end if;
    Put(File, " [ ");
    IIO.Put(File, 1+Item.n , 3);
    Put(File, " blocks ]: ");
    Put(File,'{');
    if Item.n > Item.last_used then
      IIO.Put(File, Item.n - Item.last_used, 3);
      Put(File, " unused |");
    end if;
    for k in reverse 0 .. Item.last_used loop
      Put(File, Block_type'Image(Item.blk(k)));
      if k>0 then Put(File,'|'); end if;
    end loop;
    Put(File,'}');
  end Put_in_blocks;

  procedure Put_in_blocks(Item  : in multi_int) is
  begin
    Put_in_blocks( Standard_Output, Item );
  end Put_in_blocks;

  procedure Get(File  : in  File_Type;
                Item  : out multi_int;
                Width : in Field := 0) is
  begin
    null; -- !!!
  end Get;

  procedure Get(Item  : out multi_int;
                Width : in  Field := 0) is

  begin
    Get(Standard_Input, Item, Width);
  end Get;


  procedure Put(File  : in File_Type;
                Item  : in multi_int;
                Width : in Field := 0;
                Base  : in Number_Base := Default_Base) is

  begin
    if Width = 0 then       -- No padding required (default)
      Put(File, Str(Item, Base));
    else                    -- Left padding required -> slow
      declare
        la_chaine: String(1..Width);
      begin
        Put(la_chaine, Item, Base);
        Put(File, la_chaine);
      end;
    end if;
  end Put;

  procedure Put(Item  : in multi_int;
                Width : in Field := 0;
                Base  : in Number_Base := Default_Base) is

  begin
    Put(Standard_Output, Item, Width, Base);
  end Put;

  procedure Get(From : in  String;
                Item : out multi_int;
                Last : out Positive) is
  begin
    Last:= 1;
    null; -- !!!
  end Get;


  procedure Put(To   : out String;
                Item : in multi_int;
                Base : in Number_Base := Default_Base) is

    nchiffres: constant Natural:= Number_of_digits(Item, Base);
    blancs: constant String(To'Range):= (others=> ' ');
  begin
    if nchiffres > To'Length then
      raise Layout_Error;
    else
      To:= blancs;
      To( To'Last - nchiffres .. To'Last ):= Str(Item, Base);
    end if;
  end Put;

end Multi_precision_integers.IO;
