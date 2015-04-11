with Formulas;

with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Formulas is

  subtype Real is Long_Float;

  package RIO is new Ada.Text_IO.Float_IO(Real);

  function Evaluate_variable(name: String) return Real is
  begin
    if name = "x" then
      return 1.234;
    end if;
    return 0.0;
  end Evaluate_variable;

  package My_Formulas is new Formulas(Real, Evaluate_variable);

  procedure Test_1(expr: String) is
    use My_Formulas;
    f: Form_P:= null;
    ok: Boolean;
    use RIO;
    e0, e: Real;
  begin
    Put_Line("*************** Testing formula: " & expr);
    String_to_Form(expr, f, ok);
    Put("Output...   : ");
    Write_Form(Standard_Output, f);
    New_Line;
    e0:= Eval_form(f);
    for count in 1..4 loop
      Put("Simplify #" & Integer'Image(count) & ": ");
      Simplify(f);
      Write_Form(Standard_Output, f);
      New_Line;
      e:= Eval_form(f);
      if abs(e - e0) > 1.0e-7 then
        Put_Line("!!! Evaluation error !!!");
      end if;
    end loop;
    Put("Eval: "); Put(e, 0,5,0);
    New_Line;
  end;
begin
  Test_1("sin(8.0)");
  Test_1("x");
  Test_1("x*(x*x)");
  Test_1("x*x*x");
  Test_1("x*x*x  +  x*x^2 + x+x");
end Test_Formulas;
