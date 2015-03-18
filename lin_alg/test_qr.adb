with Ada.Text_IO;                       use Ada.Text_IO;

with Ada.Numerics.Generic_Real_Arrays;
with Generic_Real_Linear_Equations;

procedure Test_QR is

  subtype Real is Long_Float;
  type Integer_Vector is array (Integer range <>) of Integer;

  package RIO is new Float_IO(Real);

  package GRA is new Ada.Numerics.Generic_Real_Arrays(Real);
  package GRLE is new Generic_Real_Linear_Equations(Real, GRA, Integer_Vector);

  use GRA, GRLE, RIO;

  procedure Put(A: Real_Matrix) is
  begin
    for i in A'Range(1) loop
      for j in A'Range(2) loop
        Put(A(i,j), 4,2,0);
      end loop;
      New_Line;
    end loop;
  end Put;

  procedure Put(x: Real_Vector) is
  begin
    for i in x'Range loop
      Put(x(i), 4,2,0);
    end loop;
    New_Line;
  end Put;

  procedure Test(A: Real_Matrix; x, y: Real_Vector) is
    Q: Real_Matrix(A'Range(1), A'Range(1));
    R: Real_Matrix(A'Range(2), A'Range(2));
    Z: Real_Matrix:= A;
    res: Real:= 0.0;
    x_solved: Real_Vector(x'Range);
  begin
    QR_Decomposition ( A , Q , R ) ;
    Put_Line("**** QR test A -> QR. A is:");
    Put(A);
    Put_Line("Q is:");
    Put(Q);
    Put_Line("R is:");
    Put(R);
    Put_Line("Check Q orthogonal: QtQ should be close to unit matrix");
    Put(Transpose(Q) * Q);
    Put_Line("Check QR - A, should be close to 0 matrix");
    Z:= Q*R-A;
    Put(Z);
    for i in Z'Range(1) loop
      for j in Z'Range(2) loop
        res:= res + abs Z(i,j);
      end loop;
    end loop;
    Put("This sum should be close to 0 :");
    Put(res);
    New_Line;
    Put_Line("**** QR solver");
    x_solved:= QR_SOLVE(Q,R,y);
    Put_Line("  solution x  :");
    Put(x_solved);
    Put_Line("  x should be :");
    Put(x);
    Put("  Ax=");
    Put(A*x_solved);
    Put("   y=");
    Put(y);
    New_Line;
    New_Line;
  end Test;

  A33: constant Real_Matrix(1..3, 1..3):=
        (( 12.0 , -51.0,   4.0),
         (  6.0 , 167.0, -68.0),
         ( -4.0,   24.0, -41.0));

  x_a: constant Real_Vector:= (1.0, 2.0, 3.0);
  y_a: constant Real_Vector:= A33 * x_a;

  A32: constant Real_Matrix(1..3, 1..2):=
        ((  2.0, 1.0),
         (  4.0, 5.0),
         (  3.0, 7.0));

  x_b: constant Real_Vector:= (123.0, 456.0);
  y_b: constant Real_Vector:= A32 * x_b;


begin
  Test( A33, x_a, y_a );
  -- Test( A32, x_b, y_b ); -- !! QR not yet set for non-square matrices
  Put("Finished - press return");
  Skip_Line;
end Test_QR;
