--
--  Equation de la chaleur resolue par differences finies en espace
--  et schema theta en temps.  GdM v. 31-mar-2000
--

with Ada.Numerics.Generic_Elementary_Functions, Ada.Text_IO;

procedure Heat is

  type Real is digits 15;
  package REF is new Ada.Numerics.Generic_Elementary_Functions (Real);
  use Ada.Numerics, REF;

  use Ada.Text_IO;
  package RIO is new Float_IO ( Real ); use RIO;

  N : constant Integer := 50;
  h : constant Real := 1.0 / Real (N+1);

  N_pas  : constant := 40;
  t_final: constant := 0.05;

  tau : constant := t_final / N_pas;

  theta : constant := 0.501;

  fr : File_Type;

  type Vector is array ( Integer range <> ) of Real;

  uk, ukp1 : Vector ( 0 .. N + 1 );  --  solution d'essai et reste pour Newton

  -- Matrice tridiagonales

  type Tridiag ( nn : Positive ) is record
     d : Vector ( 1 .. nn );  --  diag
     l : Vector ( 2 .. nn );  --  diag inf
     r : Vector ( 2 .. nn );  --  diag sup
  end record;

  A, B: Tridiag ( N );  --  matrices implicite et explicite

  vb: Vector ( 1 .. N );  --  2nd membre

  -- Sous-programmes

  -- Multiplication y:= A x

  procedure Mult ( A: in Tridiag; x: in Vector; y: out Vector ) is
    bad_dimensions: exception;
    n: constant Integer:= A.nn;
  begin
    if x'First /= 1 or y'First /= 1 then raise bad_dimensions; end if;
    if x'Length /= n or y'Length /= n then raise bad_dimensions; end if;
    if n=1 then
      y(1):= A.d(1)*x(1);
    else
      y(1):= A.d(1) * x(1) + A.r(2) * x(2);
      for i in 2 .. n-1 loop
        y(i):= A.l(i) * x(i-1) + A.d(i) * x(i) + A.r(i+1) * x(i+1);
      end loop;
      y(n) := A.l(n) * x(n-1) + A.d(n) * x(n);
    end if;
  end Mult;

  --  Decomposition LR tridiagonale universelle

  procedure LR ( A: in out Tridiag ) is
  begin
    for i in A.l'Range loop
      A.r(i) := A.r(i) / A.d(i-1);
      A.d(i) := A.d(i) - A.r(i) * A.l(i);
    end loop;
  end LR;

  -- Resoudre  A x = d  sur une matrice A tridiagonale apres decomposition LR

  procedure Solve_LR( A: in     Tridiag;
                      d: in     Vector;
                      x: in out Vector ) is

    y : Vector ( d'Range );

  begin
    -- Ly=d :
    y(1) := d(1) / A.d(1);
    for i in 2 .. y'Last loop
      y(i):= ( d(i) - A.l(i) * y(i-1) ) / A.d(i);
    end loop;
    -- Rx=y :
    x(A.nn):= y(A.nn);
    for i in reverse 1 .. A.nn-1 loop
      x(i) := y(i) - A.r(i+1) * x(i+1);
    end loop;
  end Solve_LR;

  procedure Put( f: File_Type; u: Vector ) is
    sep : constant Character := ';';
  begin
    -- Put( f,"{ " );
    for i in u'Range loop
      Put (f, u(i), 3,3,0);
      if i < u'Last then Put (f, sep); end if;
    end loop;
    -- Put_Line( f, " }") ;
    New_Line (f);
  end Put;

begin
  --  Condition initiale, y c. conditions aux bords 0 et N + 1.
  for j in uk'Range loop
    uk(j) := Sin (2.0 * Pi * Real(j) * h);
  end loop;
  ukp1 := uk;

  Create( fr, Name=> "uchal.csv" );
  -- Put( fr,"{ " );
  Put (fr, uk);

  --  Remplir matrice A
  for i in 1 .. N loop A.d(i):= 1.0 + theta * tau *   2.0  / (h*h); end loop;
  for i in 2 .. N loop A.l(i):=     + theta * tau * (-1.0) / (h*h); end loop;
  A.r := A.l;  --  A est symetrique

  --  Remplir matrice B
  for i in 1 .. N loop B.d(i):=  (1.0 + (theta-1.0) * tau *   2.0  / (h*h)); end loop;
  for i in 2 .. N loop B.l(i):=  (      (theta-1.0) * tau * (-1.0) / (h*h)); end loop;
  B.r := B.l;  --  B est symetrique

  LR ( A );  --  Decomposition LR de A

  --  Boucle principale d'evolution en temps
  temps: for k in 1 .. N_pas loop
    Mult ( B, uk (1 .. N), vb );          --  2nd membre
    Solve_LR ( A, vb, ukp1 (1 .. N) );    --  Resolution. on a fait: [A ukp1 = B uk]
    uk := ukp1;
    Put (fr, uk );  --  Ecriture de la solution
    -- if k < N_pas then Put(fr,','); end if;
  end loop temps;

  -- Put_Line( fr, " }" );
  Close (fr);

end Heat;
