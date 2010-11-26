with Ada.Calendar;
with Ada.Unchecked_Conversion, System;

package body U_Rand is
    Init_C : constant := 362436.0/16777216.0 ;
    CD     : constant := 7654321.0/16777216.0 ;
    CM     : constant := 16777213.0/16777216.0 ;

    subtype RANGE_1 is INTEGER range 0..M1-1 ;
    subtype RANGE_2 is INTEGER range 0..M2-1 ;

    procedure Start(Gen   : out Generator;
                    New_I : SEED_RANGE_1 := Default_I ;
                    New_J : SEED_RANGE_1 := Default_J ;
                    New_K : SEED_RANGE_1 := Default_K ;
                    New_L : SEED_RANGE_2 := Default_L
                    )
    is
        S, T : Real ;
        M : RANGE_1 ;
        I, J, K : RANGE_1 ;
        L : RANGE_2 ;
    begin
        I := New_I ; J := New_J ; K := New_K ; L := New_L ;
        Gen.NI := RANGE_3'last ;
        Gen.NJ := (RANGE_3'last/3) + 1 ;
        Gen.C := Init_C ;

        for II in RANGE_3 loop
            S := 0.0 ; T := 0.5 ;
            for JJ in 1..24 loop
                M := (((J * I) mod M1) * K) mod M1 ;
                I := J ; J := K ; K := M ;
                L := (53 * L + 1) mod M2 ;
                if ((L * M) mod 64) >= 32 then
                    S := S + T ;
                end if ;
                T := 0.5 * T ;
            end loop ;
            Gen.U(II) := S ;
        end loop ;
    end Start ;

    procedure Reset (Gen : out Generator; Initiator : Seed_Range_1) is
    begin
      Start(Gen, Initiator);
    end Reset;

    procedure Randomize (Gen : out Generator) is
      use Ada.Calendar;
      Day_Seconds: constant Day_Duration:= Seconds(Clock);
      Hour, Min, Sec, Cent : Integer;
    begin
      Sec := Integer(Day_Seconds);
      Cent:= Integer(100.0 * (Day_Seconds-Duration(Sec)));
      Hour:= Sec/3600 + 1;
      Min := (Sec/60) rem 60 + 1;
      Sec := Sec rem 60 + 1;
      Start (Gen, New_I => Hour, New_J => Min, New_K => Sec, New_L => Cent);
    end Randomize;

    type Pointer is access all Generator;

    function Next(Gen: Generator) return Uniformly_Distributed is
        function Cvt is new Ada.Unchecked_Conversion(System.Address, Pointer);
        -- ^ This method is functionally identical as GNAT's Unrestricted_Access
        -- but has no type safety (cf GNAT Docs)
        Genp : constant Pointer := Cvt(Gen'Address);
        Temp : Real;
    begin
        Temp := Genp.U(Genp.NI) - Genp.U(Genp.NJ) ;
        if Temp < 0.0 then
            Temp := Temp + 1.0;
        end if ;
        Genp.U(Genp.NI) := Temp;
        Genp.NI := Genp.NI - 1;
        if Genp.NI = 0 then
            Genp.NI := RANGE_3'last;
        end if ;
        Genp.NJ := Genp.NJ - 1;
        if Genp.NJ = 0 then
            Genp.NJ := RANGE_3'last;
        end if ;
        Genp.C := Genp.C - CD;
        if Genp.C < 0.0 then
            Genp.C := Genp.C + CM;
        end if ;
        Temp := Temp - Genp.C;
        if Temp <= 0.0 then
            Temp := Temp + 1.0;
        end if ;
        return Temp;
    end Next;

end U_Rand;
------------------------------------------------------------------------
-- test program follows
--
--
--  with Text_IO ; use Text_IO ;
--  with U_Rand ;

--  procedure Test_U_Rand is
--      subtype Real is Float;
--      package Float_IO is new Text_IO.Float_IO(Real) ;
--      use Float_IO ;
--      Test_File : Text_IO.File_Type ;
--      File_Name : constant STRING:= "URand_Test_Output.dat" ;
--      subtype A_RESULT is STRING(1..11) ;
--      type RESULTS_TYPE is array(1..6) of A_RESULT ;
--      Actual_Results : RESULTS_TYPE ;
--      EOS : NATURAL ;
--      Results_OK : BOOLEAN := True ;
--      Expected_Results : constant
--        RESULTS_TYPE := ( 1=>" 6533892.00",
--                          2=>"14220222.00",
--                          3=>" 7275067.00",
--                          4=>" 6172232.00",
--                          5=>" 8354498.00",
--                          6=>"10633180.00") ;

--      Rnum : Real ;
--      package RUR is new U_Rand(Real);
--      Gen: RUR.Generator;

--  begin
--      RUR.Start(Gen);
--
--      for I in 1..20_000 loop
--          Rnum:= RUR.Next(Gen);
--      end loop ;

--      Text_IO.Create(File=>Test_File, Name=>File_Name) ;
--      for I in 1..6 loop
--          Rnum:= RUR.Next(Gen);
--          Put(File=>Test_File,
--              Item=>(2.0**24)*Rnum,
--              Fore=>8, Aft=>2, Exp=>0) ;
--          New_Line(Test_File) ;
--      end loop ;

--      Text_IO.Close(File=>Test_File) ;
--      Text_IO.Open(File=>Test_File, Mode=>In_File, Name=>File_Name) ;
--      Put("Expected Results    ") ; Put_Line("Actual Results") ;

--      for I in 1..6 loop
--          Get_Line(File=>Test_File, Item=>Actual_Results(I), Last=>EOS) ;
--          Skip_Line(Test_File) ;
--          Put(Expected_Results(I)) ; Put("         ") ;
--          Put_Line(Actual_Results(I)) ;
--          if Actual_Results(I) /= Expected_Results(I) then
--              Results_OK := False ;
--          end if ;
--      end loop ;

--      New_Line(2) ;
--      if not Results_OK then
--          Put_Line("!! CAUTION!! Random Number Generator inconsistent on this implementation") ;
--      else
--          Put_Line("Random Number Generator Consistent") ;
--      end if ;
--  end Test_U_Rand ;
