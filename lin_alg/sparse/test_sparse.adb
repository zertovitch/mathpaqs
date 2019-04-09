-- Test of Sparse matrix package.
-- Uses also conjugate gradient methods package.

with ConjGrad, Sparse, Ada.Text_IO; use Ada.Text_IO;

with Ada.Calendar;   use Ada.Calendar;

procedure Test_Sparse is

  -- usually:
  --  float = single
  --  long_float = double

  subtype Real is Long_Float;

  package FIO  is new Float_IO(Real);           use FIO;
  package IIO is new Integer_IO(Integer);       use IIO;
  package DurIO is new Fixed_IO(Duration);

  type Vector is array (Integer range <>) of Real;
  type p_Vector is access Vector;

  -- Matrices creuses, instancie pour Real

  package Spa is new Sparse( Real, Integer, Vector ); -- , false

  -- Methodes du gradient (bi)-conjugue, instanciees pour les
  -- matrices creuses (Spa ci-dessus)

  package Methodes_Gradient is
    new ConjGrad (
           Real              => Real,
           index             => Integer,
           Vector            => Vector,
           Any_matrix        => Spa.CRS_matrix,
           Get               => Spa.Get,
           Rows              => Spa.Rows,
           Defined_symmetric => Spa.Defined_symmetric,
           Copy              => Spa.Copy,
           "*"               => Spa."*",
           Add_scaled        => Spa.Add_scaled,
           Scale             => Spa.Scale,
           Mult              => Spa.Mult
        );

  use Spa, Methodes_Gradient;

  T0, T1, T2: Time;

  n0: constant:= 20;
  n1: constant:= 10 * n0;
  n2: constant:= 11 * n0;
  neq: constant:= n1*n2;
  itmax: constant := 10 * neq;
  nnz_estim: constant:= neq * 5;
  A: constant p_CRS_matrix:= new CRS_matrix( neq+1, nnz_estim );
  x, b: constant p_Vector:= new Vector(1..neq);
  tmp: Real;
  na: Natural;
  nite: Natural;

  function nnz(n1, n2, neq: Natural) return Natural is
    -- determination de la structure de la matrice
    na: Natural:= 0;
  begin
    for m in 1..neq loop
      if  m > n1  then na := na + 1; end if;
      if  m > 1   then na := na + 1; end if;
      na := na + 1;
      if  m + 1 <= neq   then na := na + 1; end if;
      if  m + n1 <= neq  then na := na + 1; end if;
    end loop;
    return na;
  end nnz;

begin
  Put("dim:"); Put(neq); New_Line;
  Put("nnz estim"); Put(nnz_estim); New_Line;
  Put("nnz exact"); Put(nnz(n1, n2, neq)); New_Line;

  T0:= Clock; -- CPU_Clock;

  Put_Line("Remplissage de la matrice A");

  na := 0;
  for m in 1..neq loop
    A.row_ptr(m) := na + 1;

    if  m > n1  then
      na := na + 1;
      A.col_ind(na) := m - n1;
      A.val(na) := -1.0;
    end if;

    if  m > 1  then
      na := na + 1;
      A.col_ind(na) := m - 1;
      A.val(na) := -1.0;
    end if;

    na := na + 1;
    A.col_ind(na) := m;
    A.val(na) := 6.0;

    if  m + 1 <= neq  then
      na := na + 1;
      A.col_ind(na) := m + 1;
      A.val(na) := -1.4;
    end if;

    if  m + n1 <= neq  then
      na := na + 1;
      A.col_ind(na) := m + n1;
      A.val(na) := -1.45;
    end if;
  end loop;

  A.row_ptr(neq+1) := na + 1;
  A.symmetric:= False;

  Put_Line("Construction de b:= Ax avec x connu : x=(1,1,...1)");

  for i in 1..neq loop
    tmp := 0.0;
    for j in A.row_ptr(i)..A.row_ptr(i+1)-1 loop
      tmp := tmp + A.val(j);
    end loop;
    b(i) := tmp;
  end loop;                -- la solution est x=(1,1,...,1)

  Put_Line("Initialisation de x : x:=(0,0,...0)");

  -- x.all:= (others=> 0.0); plante !
  for i in x'Range loop
    x(i):= 0.0;
  end loop;

--      x(1..neq/2):= (others=> -0.09);
--      x(neq/2..neq):= (others=> 1.09);

  Put_Line("Resolution de x : Ax=b");

  T1 := Clock; -- CPU_Clock;

  BiCGStab(A.all,b.all,x.all, 1.0e-20, 1.0e-12, 1.0e-12,
           none,
           itmax, nite
  );

  T2 := Clock; -- CPU_Clock;

  Put("Residu en norme l1:  ||x-(1,1,...)||_1 = ");
  tmp := 0.0;
  for i in x'Range loop
    tmp:= tmp + abs(x(i)-1.0);
  end loop;
  Put(tmp); New_Line;
--      for i in 1..neq loop
--         Put(i); Put(x(i), exp=> 0); new_line;
--      end loop;
  Put("# iterations"); Put(nite); New_Line;

  Put("Temps structure et remplissage"); DurIO.Put(T1 - T0, Aft => 2);
  New_Line;
  Put("Temps resolution              "); DurIO.Put(T2 - T1, Aft => 2);
  New_Line;
end Test_Sparse;
