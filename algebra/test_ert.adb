------------------------------------------------------------------------------
--  File:            test_ert.adb
--  Description:     TEST for Euclidean_Ring_Tools
--  Date/Version:    14-Dec-2020; 26-Apr-2002 ; 25-May-1997
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Euclidean_Ring_Tools;

with Ada.Text_IO;

with Interfaces;

procedure Test_ERT is

  type My_Int is new Interfaces.Integer_64;

  use Ada.Text_IO;
  package IIO is new Integer_IO (My_Int); use IIO;

  type My_Int_Array is array (Integer range <>) of My_Int;

  package IEuclid is
    new Euclidean_Ring_Tools (My_Int, 0, 1, "+", "-", "*", "/", "=", My_Int_Array);
  use IEuclid;

  procedure Show_Bezout (a, b : My_Int) is
    p, s, t : My_Int;
  begin
    Put (" GCD ("); Put (a, 0); Put (", "); Put (b, 0); Put (") = ");
    GCD (a, b, p);
    Put (p, 6);
    Put_Line (" = ...");
    Put ("       ... ");
    Bezout (a, b, s, t);
    Put (a, 3); Put (" * "); Put (s, 3);
    Put (" + ");
    Put (b, 3); Put (" * "); Put (t, 3);
    if p = a * s + b * t then  --  Bezout relation
      Put (": p = a * s + b * t equality checked.");
    else
      raise Program_Error with "Euclidean_Ring_Tools: failure with Bezout";
    end if;
    New_Line;
  end Show_Bezout;

  procedure Test_Chinese_Theorem (a, n : My_Int_Array) is
    x, prod : My_Int;
    ok : Boolean := True;
  begin
    Put ("Chinese Theorem solution: ");
    Chinese_Remainder_Theorem (a, n, x, prod);
    Put (x mod prod);
    --  Put ("  (x="); Put (x, 0); Put (";  product ="); Put (prod, 0); Put (")");
    for i in n'Range loop
      ok := ok and x mod n (i) = a (i) mod n (i);
    end loop;
    if ok then
      Put_Line (".   Solution verified.");
    else
      raise Program_Error with "Euclidean_Ring_Tools: failure with Chinese Theorem";
    end if;
  end Test_Chinese_Theorem;

begin
  Put_Line ("TEST for Euclidean_Ring_Tools: GCD / Bezout");
  for a in My_Int'(4) .. 9 loop
    for b in My_Int'(1) .. 5 loop
      Show_Bezout (a, b);
    end loop;
  end loop;
  Show_Bezout (0, 0);
  Show_Bezout (0, 1);
  Show_Bezout (1, 0);
  Show_Bezout (99991 * 97, 12 * 99991 * 977);
  Show_Bezout (18757 * 13012291, 61 * 13012291 * 46499);
  New_Line;
  --
  --  Advent of Code 2020, Day 13, see hac/exm/aoc/2020.
  --
  Test_Chinese_Theorem (
    --  This is the example given on https://adventofcode.com/2020/day/13 .
    a => (0, 12, 55, 25, 12),
    n => (7, 13, 59, 31, 19)
  );
  Test_Chinese_Theorem (
    --  Condensed form of the data: aoc_2020_13.txt
    --  Solution computed in: aoc_2020_13.adb
    a => (0,  14, 380, -29, -34, -29, -29, 293, -60),
    n => (29, 37, 409, 17, 13, 19, 23, 353, 41)
  );
  New_Line;
end Test_ERT;
