------------------------------------------------------------------------------
--  File:            GNAT_int.adb
--  Description:     Test for Multi_precision_integers;
--                   Wrapper for GNAT 3.13p+
--  Author:          Gautier.deMontmollin@Winterthur.ch
------------------------------------------------------------------------------

with GNAT.Traceback.Symbolic, Ada.Exceptions, Ada.Text_IO;
use Ada.Exceptions, Ada.Text_IO;

with Test_Int;

procedure GNAT_Int is
begin
  Test_Int;
exception
  when E : others =>
    New_Line;
    Put_Line ("------- Patatras! -------");
    Put_Line (" Name of exception     : " & Ada.Exceptions.Exception_Name(E) );
    Put_Line (" Message for exception : " & Ada.Exceptions.Exception_Message(E) );
    Put_Line ( GNAT.Traceback.Symbolic.Symbolic_Traceback(E) );
end GNAT_Int;
