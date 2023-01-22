--  At the moment empty (only shows how to instanciate);
--  will be one day a very nice Finite Elements demo!
--  (I have working code; contact me if in need!).

with Ada.Numerics.Generic_Elementary_Functions,
     Ada.Numerics.Generic_Real_Arrays;

with G_FEK;

procedure Demo_FEK is

  subtype F is Long_Float;
  type Vector is array (Integer range <>) of F;
  type Matrix is array (Integer range <>, Integer range <>) of F;

  package FEK is new G_FEK (Long_Float, Vector, Matrix);

  package FEF is new Ada.Numerics.Generic_Elementary_Functions (F);

  package Matrices is new Ada.Numerics.Generic_Real_Arrays (F);

begin
  null;
end Demo_FEK;
