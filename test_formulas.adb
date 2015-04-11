with Formulas;

with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Formulas is

  subtype Real is Long_Float;

  package RIO is new Ada.Text_IO.Float_IO(Real);
  use RIO;

  function Evaluate_variable(name: String) return Real is
  begin
    if name = "x" then
      return 1.234;
    end if;
    return 0.0;
  end Evaluate_variable;

  package My_Formulas is new Formulas(Real, Evaluate_variable);

  procedure Test_1(expr: String; target: String:= "") is
    use My_Formulas;
    f: Formula:= null_formula;
    ok: Boolean;
    e0, e: Real;
  begin
    Put_Line("*************** Testing formula: " & expr);
    Parse(expr, f, ok);
    Put("Output...   : ");
    Put(Standard_Output, f);
    New_Line;
    e0:= Evaluate(f);
    for count in 1..4 loop
      Put("Simplify #" & Integer'Image(count) & ": ");
      Simplify(f);
      Put(Standard_Output, f);
      if target /= "" then
        Put(";  target value is: " & target);
      end if;
      New_Line;
      e:= Evaluate(f);
      if abs(e - e0) > 1.0e-10 then
        Put_Line("!!! Evaluation error !!!");
      end if;
    end loop;
    Put("Eval: "); Put(e, 0,14,0);
    New_Line;
  end;
begin
  Put("x=");
  Put(Evaluate_variable("x"), 0,3,0);
  New_Line;
  Test_1("x * (x*x)");
  Test_1("x*x*x");
  Test_1("x*x*x  +  x*x^2 + x+x");
  Test_1("sin(2*2^(1/2+3/2) + 1*1/2 + 0*7.65) + sin(8.5)", "1.59697422524698 = 2*sin(8.5)");
end Test_Formulas;
