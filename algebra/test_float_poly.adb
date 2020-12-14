------------------------------------------------------------------------------
--  File:            Test_Float_Poly .adb
--  Description:     Test for Float_Polynomials
--                   !! Part with Euclidean_Ring_Tools below
--                      needs further debugging !!
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Float_Polynomials;                 use Float_Polynomials;

with Polynomials.Output;

with Euclidean_Ring_Tools;

procedure Test_Float_Poly is

  --------- Output

  procedure Fl_Put (File : in File_Type; Item : in Float) is
  begin
    Put (File, Item, 2, 1, 0);
  end Fl_Put;

  package PolyOut is new Float_Polynomials.Output (Fl_Put, ">");
  use PolyOut;

  procedure Show_Bezout (a, b : Polynomial) is
    subtype Polyfix is Polynomial (0 .. Integer'Max (Deg (a), Deg (b)));
    type Array_of_Polynomials is array (Integer range <>) of Polyfix;
    package PEuclid is new
       Euclidean_Ring_Tools (
         Polyfix,
         (others => 0.0),            --  "Zero" polynomial (mathematically, the constant 0)
         (0 => 1.0, others => 0.0),  --  "One" polynomial (mathematically, the constant 1)
         "+", "-", "*", "/",
         Eq,
         Fill,
         --  Problem here: source can be an unconstrained Polynomial.
         --  BTW, GNAT obviously accepts "+", "-", etc. for the constrained Polyfix...
         Array_of_Polynomials
       );
    use PEuclid;
    fixe_a, fixe_b, s, t, the_gcd : Polyfix;
  begin
    Put ("GCD ("); Put (a, "X"); Put (","); Put (b, "X"); Put (") = ");
    Fill (fixe_a, a);
    Fill (fixe_b, b);
    GCD (fixe_a, fixe_b, the_gcd);
    Put (the_gcd, "X");
    Put (" = (");
    Bezout (fixe_a, fixe_b, s, t);
    Put (a, "X"); Put (") * ("); Put (s, "X");
    Put (") + (");
    Put (b, "X"); Put (") * ("); Put (t, "X"); Put (")");
    New_Line;
  end Show_Bezout;

  a : constant Polynomial := (-1.0, 0.0, 1.0);  -- x^2 - 1
  b : constant Polynomial := (-1.0, 1.0);       -- x - 1
  c : constant Polynomial := (1.0, 1.0);        -- x + 1
  d : constant Polynomial := (0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0);

begin
  Put_Line ("Test for Polynomials (Real numbers)");
  if Neq (a, b * c) then Put ("BUG: a /= b*c"); end if;
  if Neq (a / b, c) then Put ("BUG: a/b /= c"); end if;
  if Neq (a / c, b) then Put ("BUG: a/c /= b"); end if;
  Put (a, "X"); Put ("="); Put (b, "X"); Put ("*"); Put (c, "X"); New_Line;

  Put ("Prim/Der test: p(x)="); Put (d, "x"); New_Line;
  Put ("Primitive,  p(y)  ="); Put (Primitive (d), "y"); New_Line;
  Put ("Derivative, p'(z) ="); Put (Derivate (d), "z"); New_Line;
  New_Line;
  Put ("[return]"); Skip_Line;

  Put_Line ("GCD/Bezout");
  Show_Bezout (a, b);
  Show_Bezout ((5.0, 0.0, -3.75),
               (2.6, 7.0, 9.0, 0.0, -2.13, 0.0));

end Test_Float_Poly;
