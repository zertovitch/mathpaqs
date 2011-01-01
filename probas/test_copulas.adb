with Ada.Text_IO;                       use Ada.Text_IO;

with Ada.Numerics.Generic_Real_Arrays;
with Generic_Real_Linear_Equations;
with U_Rand;
with Copulas;

procedure Test_Copulas is

  subtype Real is Long_Float;

  package RIO is new Float_IO(Real);

  package GRA is new Ada.Numerics.Generic_Real_Arrays(Real);
  package GRLE is new Generic_Real_Linear_Equations(Real, GRA);

  package Real_U_Rand is new U_Rand(Real);

  -- *** Choice of a random generator: A.N.F_R, or U_Rand (faster), or...:
  package RRand renames
    -- Ada.Numerics.Float_Random;
    Real_U_Rand;

  package RCopulas is new Copulas(
    Real,
    RRand.Uniformly_Distributed,
    RRand.Generator,
    RRand.Random,
    GRA
  );

  use GRA, GRLE, RIO;

--  procedure Put(A: Real_Matrix) is
--  begin
--    for i in A'Range(1) loop
--      for j in A'Range(2) loop
--        Put(A(i,j), 4,2,0);
--      end loop;
--      New_Line;
--    end loop;
--  end Put;

  procedure Test(A: Real_Matrix; name: String) is
    dim_dep: constant Integer:= A'Length(1);
    dim_indep: constant:= 3;
    U01: Real_Vector(1..dim_dep + dim_indep);
    gen: RRand.Generator;
    copula: RCopulas.Copula_access;
    f: File_Type;
  begin
    RRand.Reset(gen, 1);
    RCopulas.Construct_as_Gauss(copula, U01'Length, A);
    Create(f, Out_File, name & ".csv");
    for i in 1..dim_dep loop
      Put(f, "dep;");
    end loop;
    for i in 1..dim_indep loop
      Put(f, "indep;");
    end loop;
    New_Line(f);
    for n in 1..60_000 loop
      U01:= RCopulas.Simulate(copula.all, gen);
      for i in U01'Range loop
        Put(f, U01(i));
        Put(f, ';');
      end loop;
      New_Line(f);
    end loop;
  end Test;

  A33: constant Real_Matrix(1..3, 1..3):=
        ((1.0 , 0.5 , 0.25),
         (0.5 , 1.0 , 0.06),
         (0.25, 0.06, 1.0));

begin
  Test( A33, "Gaussian_3x3" );
--  Skip_Line;
end Test_Copulas;