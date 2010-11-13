------------------------------------------------------------------------------
--  File:            Test_Float_Poly .adb
--  Description:     Test for Float_Polynomials
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Float_Polynomials;                 use Float_Polynomials;

with Polynomials.Output, Euclidean_Ring_Tools;

procedure Test_Float_Poly is 

  --------- Output

  procedure Fl_Put(File: in File_Type; Item: in Float) is
  begin
    Put(File,Item,2,1,0);
  end;

  package PolyOut is new Float_Polynomials.Output(Fl_Put,">"); use PolyOut;

  procedure Show_Bezout(a,b: polynomial) is
    subtype polyfix is polynomial(0..Integer'Max(Deg(a),Deg(b)));
    package PEuclid is new
       Euclidean_Ring_Tools(polyfix, (others=>0.0), (0=>1.0,others=>0.0),
                            "-","*","/"); 
    use PEuclid;
    fixe_a,fixe_b,s,t: polyfix;
  begin
      Put("("); Put(a,"X"); Put(","); Put(b,"X"); Put(") = ");
      Fill(fixe_a,a);
      Fill(fixe_b,b);
      Put(GCD(fixe_a,fixe_b),"X"); 
      Put(" = ("); 
      Bezout(fixe_a,fixe_b, s,t);
      Put(a,"X"); Put(") * (");Put(s,"X"); 
      Put(") + (");
      Put(b,"X"); Put(") * (");Put(t,"X"); Put(")");
      New_Line;
  end;
    

  a: polynomial:= (-1.0,0.0,1.0);  -- x^2 - 1
  b: polynomial:= (-1.0,1.0);      -- x - 1
  c: polynomial:= (1.0,1.0);       -- x + 1
  d: polynomial:= (0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0);

begin
  Put_Line("Test for Polynomials (Real numbers)");
  if Neq(a , b*c) then Put("BUG: a /= b*c"); end if;
  if Neq(a/b , c) then Put("BUG: a/b /= c"); end if;
  if Neq(a/c , b) then Put("BUG: a/c /= b"); end if;
  Put(a,"X"); Put("="); Put(b,"X"); Put("*"); Put(c,"X"); New_Line;
  
  Put("Prim/Der: d(X)="); Put(d,"X"); New_Line;
  Put("Prim d(Y)="); Put( Primitive(d), "Y" ); New_Line;
  Put("    d'(Z)="); Put( Derivate(d), "Z" ); New_Line;
  New_Line; 
  Put("[return]"); Skip_Line;

  Put_Line("GCD/Bezout");
  Show_Bezout( a,b );
  Show_Bezout( (5.0,0.0,-3.75) , 
               (2.6,7.0,9.0,0.0,-2.13,0.0) );

end Test_Float_Poly;
