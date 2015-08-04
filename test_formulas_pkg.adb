package body Test_Formulas_Pkg is

  function Evaluate_variable (name : String; dummy : Dummy_type) return Real is
  begin
    if name = "x" then
      return 1.234;
    elsif name = "y" then
      return 4.321;
    end if;
    return 0.0;
  end Evaluate_variable;

  procedure Show_name (name: String; parameters: Natural) is
  begin
    Put_Line(
      "Custom variable/function: " & name & 
      "; parameters:" & Integer'Image(parameters)
    );
  end Show_name;

end Test_Formulas_Pkg;
