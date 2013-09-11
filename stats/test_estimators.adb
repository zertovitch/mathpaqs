with Estimators;

with Ada.Text_IO;                       use Ada.Text_IO;

procedure Test_Estimators is

  type Real is digits 14;
  type Data_vector is array(Positive range <>) of Real;

  package RE is new Estimators(Real, Data_vector);

  data_1: constant Data_vector:= 
    (23.90, 24.00, 24.10, 25.00, 25.20, 25.60, 26.10, 26.20, 26.20, 26.40,
     26.40, 26.80, 26.90, 27.00, 27.20, 27.50, 27.60, 27.60, 27.70, 28.50);

  data_2: constant Data_vector:= 
    (48637.00, 47731.00, 46825.00, 46919.00, 46213.00, 46154.00, 46295.00,
     46389.00, 46530.00, 46671.00, 46859.00, 46953.00, 47000.00, 47112.00,
     47268.00, 46320.00, 46424.00, 45528.00, 45580.00, 45788.00);

  a, b: Real;

begin
  RE.Linear_least_squares(data_1, a, b);
  Put_Line("Data 1 - best fit: " & Real'Image(a) & " + x * " & Real'Image(b));
  RE.Linear_least_squares(data_2, a, b);
  Put_Line("Data 2 - best fit: " & Real'Image(a) & " + x * " & Real'Image(b));
end Test_Estimators;
