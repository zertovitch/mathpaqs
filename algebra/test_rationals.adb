------------------------------------------------------------------------------
--  File:            Test_Rationals.adb
--  Description:     Test for the "Rationals" package
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Rationals;                         use Rationals;

procedure Test_Rationals is

  type Rational is new Rationals.frac_elt;

  r: Rational;

  procedure Aff(r:rational) is
  begin
    Put('('); Put(r.a,0); Put(','); Put(r.b,0); Put(") ");
  end;

  procedure Montre_Add(r,s:rational) is
  begin
    Aff(r); Put('+'); Aff(s); Put('='); Aff(r+s);
  end;

  procedure Montre_Add_RNR(r,s:rational) is
  begin
    auto_reduce:= true;
    Montre_Add( r , s ); 
    auto_reduce:= false; Put(" auto_reduce:= false ");
    Montre_Add( r , s ); 
    New_Line;
  end;

begin
  r:= (0,1); Aff(r); New_Line;

  Montre_Add_RNR( (-2,13) , (15,13) ); 
  Montre_Add_RNR( (10,-35) , (22,21) ); 

  -- More to test...
  Put_Line("Now, we provoke an exception with (1,0) / (0,1):");
  r:= ( (1,0) / (0,1) );
end Test_Rationals;