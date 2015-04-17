with Formulas;

with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Formulas is

  subtype Real is Long_Float;

  package RIO is new Ada.Text_IO.Float_IO(Real);
  use RIO;

  type Dummy_type is new Integer;
  dummy: constant Dummy_type:= 0;

  function Evaluate_variable(name: String; dummy: Dummy_type) return Real is
  begin
    if name = "x" then
      return 1.234;
    elsif name = "y" then
      return 4.321;
    end if;
    return 0.0;
  end Evaluate_variable;

  package My_Formulas is new Formulas(Real, Dummy_type, Evaluate_variable);

  procedure Test_1(expr: String; target: String:= "") is
    use My_Formulas;
    f: Formula:= null_formula;
    e0, e: Real;
    style: constant Output_style:= normal;
  begin
    Put_Line("*************** Testing formula: " & expr);
    Parse(expr, f);
    Put("Output...   : ");
    Put(f, style);
    New_Line;
    e0:= Evaluate(f, dummy);
    for count in 1..4 loop
      Put("Simplify #" & Integer'Image(count) & ": ");
      Simplify(f);
      Put(f, style);
      if target /= "" then
        Put(";  target value is: " & target);
      end if;
      New_Line;
      e:= Evaluate(f, dummy);
      if abs(e - e0) > 1.0e-10 then
        Put_Line("!!! Evaluation error !!!");
      end if;
    end loop;
    Put("Final eval: "); Put(e, 0,14,0);
    New_Line;
  end Test_1;

begin
  Put("x=");
  Put(Evaluate_variable("x", dummy), 0,3,0);
  Put(", y=");
  Put(Evaluate_variable("y", dummy), 0,3,0);
  New_Line;
  Test_1("-12345");   --  - {12345}  ->  negative constant -12345
  Test_1("x+x+y");
  Test_1("x+y+x");
  Test_1("6-3+2", "5");
  Test_1("21/3*5/7/5-1", "0");
  Test_1("2 -4 +6 -1 -1- 0 +8", "10");
  Test_1("2*3*4/8 -   5/2*4 +  6 + 0/3", "-1");
  Test_1("(2) + (17*2-30) * (5)+2 - (8/2)*4", "8");
  Test_1("tan(x+1+1)+tan(x+2)+y");
  Test_1("x * (x*x + 0)");
  Test_1("x*x*x");
  Test_1("3 - +2 + -x");
  Test_1("Exp(1) + -1 - sin(-0.5)");
  Test_1("Exp(1) * 0 - 1");
  Test_1("Min(x,y*2) + Exp(1) * 0 - 1 + Min(y+y,x)");
  Test_1("Max(x+x,y) + 5 + Max( x--x , y )");
  Test_1("cos(+x/2)*cos(x/2)*cos(-(x/2))  +  cos(x/2)*cos(x/2)^2 + cos(x/2) + cos(x/2)");
  Test_1("cos(x/2)*cos(x/2)*cos(x*0.5) + cos(x/2) + cos(x/2) +  cos(x/2)*cos(x/2)^2 ");
  Test_1("sin(2*2^(1/2+3/2) + 1*1/2 + 0*7.65) + sin(8.5)", "1.59697422524698 = 2*sin(8.5)");
end Test_Formulas;
