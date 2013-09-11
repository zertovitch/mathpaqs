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
    (48637.0, 47731.0, 46825.0, 46919.0, 46213.0, 46154.0, 46295.0,
     46389.0, 46530.0, 46671.0, 46859.0, 46953.0, 47000.0, 47112.0,
     47268.0, 46320.0, 46424.0, 45528.0, 45580.0, 45788.0);

  data_x3: constant Data_vector:=
    (25284.0, 25312.0, 25368.0, 25424.0, 25480.0, 25564.0, 25620.0,
     25676.0, 25732.0, 25788.0, 25816.0, 25928.0, 26012.0, 26068.0,
     26124.0, 26208.0, 26264.0, 26292.0, 26348.0, 26404.0);

  data_y3: constant Data_vector:=
    (269890.0, 270438.0, 270712.0, 271534.0, 271808.0, 272904.0,
     273726.0, 274628.0, 275480.0, 276616.0, 278036.0, 278888.0,
     280024.0, 281160.0, 282012.0, 283148.0, 284004.0, 284886.0,
     286356.0, 286944.0);

  a, b: Real;

begin
  RE.Linear_least_squares(data_1, a, b);
  Put_Line("Data 1 (y only) - best fit: " & Real'Image(a) & " + x * " & Real'Image(b));
  RE.Linear_least_squares(data_2, a, b);
  Put_Line("Data 2 (y only) - best fit: " & Real'Image(a) & " + x * " & Real'Image(b));
  RE.Linear_least_squares(data_x3, data_y3, a, b);
  Put_Line("Data 3 (x,y) - best fit: " & Real'Image(a) & " + x * " & Real'Image(b));
end Test_Estimators;
