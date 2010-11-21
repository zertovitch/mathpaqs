-- At the moment empty (only shows how to instanciate);
-- will be one day a very nice Finite Elements demo!
-- (I have working code; contact me if in need!).

with Ada.Numerics.Generic_Elementary_Functions;

with G_FEK, G_Matrices;

procedure Demo_FEK is

  subtype F is Long_Float;
  type Vector is array (Integer range <>) of F;
  type Matrix is array (Integer range <>, Integer range <>) of F;

  package FEK is new G_FEK(Long_Float, Vector, Matrix);

  package FEF is new Ada.Numerics.Generic_Elementary_Functions(F);

  package Matrices is new
    G_Matrices(F, 0.0,1.0, "-", FEF.Sqrt,"+","-","*","/", Vector, Matrix);

begin
  null;
end Demo_FEK;
