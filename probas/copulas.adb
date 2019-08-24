with Generic_Random_Functions,
     Generic_Real_Linear_Equations;

with Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Copulas is

  package GRF is new Generic_Random_Functions(Real);
  package GRLE is new Generic_Real_Linear_Equations(Real, GRA, Integer_Vector);

  use GRF, GRLE;

  overriding function Simulate(C: Independent_Copula; gen: Generator) return Real_Vector is
    r: Real_Vector(1..C.dim);
  begin
    -- Build an independent U(0,1) vector
    for z in r'Range loop
      r(z):= Real(Random(gen));
    end loop;
    return r;
  end Simulate;

  overriding function Simulate(C: Independent_Copula; gen: Generator_Vector) return Real_Vector is
    r: Real_Vector(1..C.dim);
  begin
    -- Build an independent U(0,1) vector
    for z in r'Range loop
      r(z):= Real(Random(gen(z)));
    end loop;
    return r;
  end Simulate;

  --  Input  : independent U(0,1) vector
  --  Output : U(0,1) vector with C's Gaussian dependency.
  --
  procedure Make_dependent(C: Gauss_Copula; r: in out Real_Vector) is
  begin
    -- Treat the dimensions having dependencies.
    --   1) Uniform -> Normal
    for z in 1..C.dim_dep loop
      r(z):= Normal_inverse_CDF(r(z));
    end loop;
    --      Now r is a draw of an *independant* Normal multivariate random variable.
    --   2) Apply the matrix (Quantitative Risk Management book: Algorithm 3.2)
    if C.dim_dep > 0 then
      r(1..C.dim_dep):= C.Sqrt_Correl_Matrix.all * r(1..C.dim_dep);
    end if;
    --      Now r is distributed as a *dependant* Normal
    --   3) Normal -> Uniform
    for z in 1..C.dim_dep loop
      r(z):= Normal_CDF(r(z));
    end loop;
    --      Now r is U(0,1) with a Gaussian dependence on 1..dim_dep
  end Make_dependent;

  overriding function Simulate(C: Gauss_Copula; gen: Generator) return Real_Vector is
    r: Real_Vector(1..C.dim);
  begin
    -- Start with an independent U(0,1) vector
    for z in r'Range loop
      r(z):= Real(Random(gen));
    end loop;
    Make_dependent(C, r);
    return r;
  end Simulate;

  overriding function Simulate(C: Gauss_Copula; gen: Generator_Vector) return Real_Vector is
    r: Real_Vector(1..C.dim);
  begin
    for z in r'Range loop
      r(z):= Real(Random(gen(z)));
    end loop;
    Make_dependent(C, r);
    return r;
  end Simulate;

  procedure Dump(A: Real_Matrix; name: String) is
    use Ada.Text_IO;
    f: File_Type;
  begin
    Create(f, Out_File, name);
    for i in A'Range(1) loop
      for j in A'Range(2) loop
        Put(f, Real'Image(A(i,j)) & ';');
      end loop;
      New_Line(f);
    end loop;
    Close(f);
  end Dump;

  procedure Construct_as_Gauss(
    C   : out Copula_access;
    dim : Positive;
    corr: Real_Matrix
  )
  is
    g: Gauss_Copula(dim);
  begin
    g.Sqrt_Correl_Matrix:= new Real_Matrix'(Cholesky_Decomposition(corr));
    if trace then
      Dump(corr, "A.csv");
      Dump(g.Sqrt_Correl_Matrix.all, "L.csv");
    end if;
    g.dim_dep:= corr'Last(1);
    --
    C:= new Gauss_Copula'(g);
  exception
    when E : Matrix_Data_Error =>
      if Index (Ada.Exceptions.Exception_Message(E), "not positive definite") > 0 then
        --  We add an explanation about what it means in terms of correlations.
        raise Matrix_Data_Error with
          "Matrix is not positive definite: " &
          "there is a set of three or more correlations in the matrix that is not consistent. " &
          "It is possible to rectify the matrix using the Higham algorithm.";
      else
        raise;  --  Simple re-raise
      end if;
  end Construct_as_Gauss;

  function Get_Cholesky_Matrix(C: Gauss_Copula) return Real_Matrix is
  begin
    return C.Sqrt_Correl_Matrix.all;
  end Get_Cholesky_Matrix;

  procedure Dispose_internal is
    new Ada.Unchecked_Deallocation(Copula'Class, Copula_access);

  procedure Dispose(C: in out Copula_access) is
  begin
    Dispose_internal(C);
  end Dispose;

end Copulas;
