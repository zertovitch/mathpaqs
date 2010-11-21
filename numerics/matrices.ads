with G_Matrices;
-- DEC: with Long_float_math_lib;          use Long_float_math_lib;
with Ada.Numerics.Long_Elementary_Functions;
 use Ada.Numerics.Long_Elementary_Functions;

package Matrices is new
  G_Matrices(Long_Float, 0.0,1.0, "-", Sqrt,"+","-","*","/");
