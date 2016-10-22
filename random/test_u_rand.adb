with Text_IO; use Text_IO ;
with U_Rand;

procedure Test_U_Rand is
    subtype Real is Float;
    package Float_IO is new Text_IO.Float_IO (Real) ;
    use Float_IO;
    Test_File : Text_IO.File_Type;
    File_Name : constant  String:= "URand_Test_Output.dat" ;
    subtype A_RESULT is String(1..11) ;
    type RESULTS_TYPE is array(1..6) of A_RESULT ;
    Actual_Results : RESULTS_TYPE;
    EOS : Natural;
    Results_OK : Boolean := True;
    Expected_Results : constant
      RESULTS_TYPE := ( 1=>" 6533892.00",
                        2 =>"14220222.00",
                        3 =>" 7275067.00",
                        4 =>" 6172232.00",
                        5 =>" 8354498.00",
                        6 =>"10633180.00") ;

    Rnum : Real;
    package RUR is new U_Rand (Real);
    Gen : RUR.Generator;

begin
    RUR.Start (Gen);

    for I in 1 .. 20_000 loop
        Rnum := RUR.Next(Gen);
    end loop;

    Text_IO.Create (File=>Test_File, Name=>File_Name) ;
    for I in 1 .. 6 loop
        Rnum := RUR.Next(Gen);
        Put (File=>Test_File,
            Item =>(2.0**24)*Rnum,
            Fore =>8, Aft=>2, Exp=>0) ;
        New_Line (Test_File) ;
    end loop ;

    Text_IO.Close(File=>Test_File) ;
    Text_IO.Open(File=>Test_File, Mode=>In_File, Name=>File_Name) ;
    Put("Expected Results    ") ; Put_Line("Actual Results") ;

    for I in 1 .. 6 loop
        Get_Line (File=>Test_File, Item=>Actual_Results(I), Last=>EOS) ;
        Skip_Line (Test_File) ;
        Put (Expected_Results(I)) ; Put("         ") ;
        Put_Line (Actual_Results(I)) ;
        if Actual_Results (I) /= Expected_Results(I) then
            Results_OK := False;
        end if;
    end loop;

    New_Line (2) ;
    if not Results_OK then
        Put_Line ("!! CAUTION!! Random Number Generator inconsistent on this implementation") ;
    else
        Put_Line ("Random Number Generator Consistent") ;
    end if;
end Test_U_Rand;
