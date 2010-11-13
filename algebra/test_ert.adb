------------------------------------------------------------------------------
--  File:            test_ert.adb
--  Description:     TEST for Euclidean_Ring_Tools
--  Date/Version:    26-Apr-2002 ; 25-May-1997
--  Author:          Gautier.deMontmollin@UniNe.CH
------------------------------------------------------------------------------

With Text_IO, Euclidean_Ring_Tools;
Use  Text_IO;

procedure Test_ERT is 

  subtype My_int is Long_Long_Integer;

  package IIO is new Integer_IO(My_int); use IIO;
  package IEuclid is new Euclidean_Ring_Tools(My_int, 0,1, "-","*","/"); 
      use IEuclid;

  procedure Show_Bezout(a,b: My_int) is
    p, s,t: My_int;
  begin
    Put("("); Put(a,0); Put(","); Put(b,0); Put(") = ");
    p:= GCD(a,b);
    Put(p,6); 
    Put_Line(" = ...");
    Put("     ... ");
    Bezout(a,b, s,t);
    Put(a,3); Put("*");Put(s,3); 
    Put(" + ");
    Put(b,3); Put("*");Put(t,3);
    if P = A*S + B*T then -- Bezout relation
      Put(": equality checked");
    else
      Put(": /=  ---> ERT bug!");
    end if;
    New_Line;
  end;
    
begin
  Put_Line("TEST for Euclidean_Ring_Tools: GCD / Bezout");
  for a in My_Int'(4)..9 loop
    for b in My_Int'(1)..5 loop
      Show_Bezout(a,b);
    end loop;
  end loop;
  Show_Bezout(0,0);
  Show_Bezout(0,1);
  Show_Bezout(1,0);
  Show_Bezout(99991 * 97, 12 * 99991 * 977);
  Show_Bezout(18757 * 13012291, 61 * 13012291 * 46499);
end;
