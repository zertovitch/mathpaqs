with Formulas;

with Ada.Text_IO; use Ada.Text_IO;

package Test_Formulas_Pkg is

  subtype Real is Long_Float;

  package RIO is new Ada.Text_IO.Float_IO (Real);
  use RIO;

  type Dummy_type is new Integer;
  dummy : constant Dummy_type := 0;

  function Evaluate_variable (name : String; dummy : Dummy_type) return Real;

  package My_Formulas is new Formulas (Real, Dummy_type, Evaluate_variable);

  procedure Show_name (name: String; parameters: Natural);

end Test_Formulas_Pkg;
