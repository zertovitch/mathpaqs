--  Run "test_formulas >nul" to see eventual errors only

with Test_Formulas_Pkg; use Test_Formulas_Pkg;

with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Formulas is

  use RIO;

  procedure Test_1 (expr : String; target : String := "") is
    use My_Formulas;
    f : Formula;
    e0, e : Real;
    style : constant Output_style := normal;
    --
    function Entropy_test (expr : String; level : Natural) return String is
    begin
      if level = 0 then
        return expr;
      else
        return Image (Parse (Entropy_test (expr, level - 1)));
      end if;
    end Entropy_test;
    --
    procedure Copy_test (f : Formula) is
      g : constant Formula := f;  --  NB: copy is always deep thanks to Adjust.
    begin
      if not Identical (f, g) then
        Put_Line (Standard_Error, "!!! ""Copy / Identical"" test failed !!!");
        raise Program_Error;
      end if;
    end Copy_test;
    --
    function Almost_zero (x : Real) return Boolean is
    begin
      return abs x <= Real'Base'Model_Small;
    end Almost_zero;
    --
  begin
    Put_Line ("*************** Testing formula: " & expr);
    Parse (f, expr);
    --  Enumerate_custom (f, Show_name'Access);
    --  Put_Line("Parsing done.");
    Put_Line ("Image(f) = " & Image (f, bracketed));
    --  Put_Line("(Deep) copy test now.");
    Copy_test (f);
    --  Put (" [Testing Parse / Image entropy] ");
    --  Put_Line("Image(f) = " & Image(f, bracketed));
    declare
      im0 : constant String := Image (f);
      imn : constant String := Entropy_test (im0, 10);
    begin
      if im0 /= imn then
        Put_Line (Standard_Error, "!!! ""Parse / Image"" entropy test failed !!!");
        raise Program_Error;
      end if;
    end;
    Put ("Output...   : ");
    Put (f, style);
    New_Line;
    e0 := Evaluate (f, dummy);
    for count in 1 .. 4 loop
      Put ("Simplify #" & Integer'Image (count) & ": ");
      Simplify (f);
      Put (f, style);
      if target /= "" then
        Put (";  target value is: " & target);
      end if;
      -- Put (" [Testing Deep_copy] ");
      Copy_test (f);
      New_Line;
      -- Put (" [Testing Evaluate] ");
      e := Evaluate (f, dummy);
      if abs (e - e0) > abs (e) * 1.0e-10 and then  --  Relative difference
        not (Almost_zero (e) and Almost_zero (e0))    --  <- filter OA 7.2.2 false positive
      then
        Put_Line (Standard_Error, "!!! Evaluation mismatch !!!");
        Put (Standard_Error, "e0 =");
        Put (Standard_Error, e0);
        New_Line (Standard_Error);
        Put (Standard_Error, "e  =");
        Put (Standard_Error, e);
        New_Line (Standard_Error);
        raise Program_Error;
      end if;
    end loop;
    Put ("Final eval: "); Put (e, 0, 14, 0);
    New_Line;
  end Test_1;

begin
  Put ("Digits: ");
  Put (Integer'Image (Real'Digits));
  New_Line;
  Put ("x=");
  Put (Evaluate_variable ("x", dummy), 0, 3, 0);
  Put (", y=");
  Put (Evaluate_variable ("y", dummy), 0, 3, 0);
  New_Line;
  Test_1 ("-12345");   --  - {12345}  ->  negative constant -12345
  Test_1 ("6-3+2", "5");
  Test_1 ("9-4-3", "2");
  Test_1 ("9-4-3-2", "0");
  Test_1 ("-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1" &
          "-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1-1", "-40");
  Test_1 ("1000000/10/10/10/10/10/10", "1");
  Test_1 ("21/3*5/7/5-1", "0");
  Test_1 ("2 -4 +6 -1 -1- 0 +8", "10");
  Test_1 ("2*3*4/8 -   5/2*4 +  6 + 0/3", "-1");
  Test_1 ("(2) + (17*2-30) * (5)+2 - (8/2)*4", "8");
  Test_1 ("4^3^2");  --  262144 in R, 4096 in Excel. Ada asks for parenthesization.
  Test_1 ("x+x+y");
  Test_1 ("x+y+x");
  Test_1 ("x*y*x");
  Test_1 ("x*y*x*y");
  Test_1 ("[arctan(1+x*4) * sin(5*x)**5 * arctan(2*x+2*x+1)] * 8 * sin(x*5)");
  Test_1 ("tan(x+1+1)+tan(x+2)+y");
  Test_1 ("x * (x*x + 0)");
  Test_1 ("x*x*x");
  Test_1 ("2*PI + 3 + +2 + +x");
  Test_1 ("2*PI + 3 - +2 + -x");
  Test_1 ("Exp(1) + -1 - sin(-0.5)");
  Test_1 ("Exp(1) * 0 - 1");
  Test_1 ("Min(x,y*2) + Exp(1) * 0 - 1 + Min(y+y,x)");
  Test_1 ("Max(x+x,y) + 5 + Max( x--x , y )");
  Test_1 ("cos(+x/2)*cos(x/2)*cos(-(x/2))  +  cos(x/2)*cos(x/2)^2 + cos(x/2) + cos(x/2)");
  Test_1 ("cos(x/2)*cos(x/2)*cos(0.5*x) + cos(x/2) + cos(x/2) +  cos(x/2)*cos(x/2)^2 ");
  Test_1 ("sin(2*2^(1/2+3/2) + 1*1/2 + 0*7.65) + sin(8.5)", "1.59697422524698 = 2*sin(8.5)");
end Test_Formulas;
