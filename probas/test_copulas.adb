with Ada.Text_IO;                       use Ada.Text_IO;

with Ada.Numerics.Generic_Real_Arrays;
with Generic_Real_Linear_Equations;
with U_Rand;
with Copulas;

procedure Test_Copulas is

  subtype Real is Long_Float;
  type Integer_Vector is array (Integer range <>) of Integer;

  package RIO is new Float_IO (Real);

  package GRA is new Ada.Numerics.Generic_Real_Arrays (Real);
  package GRLE is new Generic_Real_Linear_Equations (Real, GRA, Integer_Vector);

  package Real_U_Rand is new U_Rand (Real);

  -- *** Choice of a random generator: A.N.F_R, or U_Rand (faster), or...:
  package RRand renames
    -- Ada.Numerics.Float_Random;
    Real_U_Rand;

  package RCopulas is new Copulas (
    Real,
    RRand.Uniformly_Distributed,
    RRand.Generator,
    RRand.Random,
    GRA,
    Integer_Vector
  );

  use GRA, GRLE, RIO;

  procedure Put (A : Real_Matrix) is
  begin
    for i in A'Range(1) loop
      for j in A'Range(2) loop
        Put (A(i,j), 4, Real'Digits, 0);
      end loop;
      New_Line;
    end loop;
  end Put;

  procedure Test (A : Real_Matrix; res_name: String) is
    dim_dep : constant Integer := A'Length(1);
    dim_indep : constant := 3;
    U01 : Real_Vector (1..dim_dep + dim_indep);
    gen : RRand.Generator;
    use RCopulas;
    copula : Copula_access;
    f : File_Type;
    L, B : Real_Matrix (A'Range(1), A'Range(2));
  begin
    RRand.Reset (gen, 1);
    Construct_as_Gauss (copula, U01'Length, A);
    Create (f, Out_File, res_name & ".csv");
    for i in 1 .. dim_dep loop
      Put (f, "dep;");
    end loop;
    for i in 1 .. dim_indep loop
      Put (f, "indep;");
    end loop;
    New_Line(f);
    for n in 1 .. 60_000 loop
      U01 := Simulate(copula.all, gen);
      for i in U01'Range loop
        Put (f, U01(i));
        Put (f, ';');
      end loop;
      New_Line(f);
    end loop;
    Put_Line("Results are stored in this file: " & Name(f));
    L:= Get_Cholesky_Matrix(Gauss_Copula(copula.all));
    B:= L * Transpose(L);
    Put_Line("B:= L*Lt.");
    Put_Line("A = B Matrix equality test (bitwise) " & Boolean'Image(A = B));
    Put_Line("A =");
    Put(A);
    New_Line;
    Put_Line("B =");
    Put(B);
    New_Line;
    Put_Line("L =");
    Put(L);
  end Test;

  A33: constant Real_Matrix(1..3, 1..3):=
        ((1.0 , 0.5 , 0.25),
         (0.5 , 1.0 , 0.06),
         (0.25, 0.06, 1.0));

begin
  Test( A33, "Gaussian_3x3" );
--  Skip_Line;
end Test_Copulas;
