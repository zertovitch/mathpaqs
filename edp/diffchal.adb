--
-- Equation de la chaleur resolue par differences finies en espace
-- et schema theta en temps.  GdM v. 31-mar-2000
--

with Text_IO;                            use Text_IO;
with Math_Lib;

procedure Diffchal is

  subtype real is long_float;
  package NEF is new Math_Lib( real ); use NEF;
  package RIO is new Float_IO( real ); use RIO;
  package IIO is new Integer_IO( Integer ); use IIO;

  N: constant integer:= 50;
  h: constant real:= 1.0 / real(N+1);

  N_pas  : constant:= 40;
  t_final: constant:= 0.05;

  tau: constant:= t_final / N_pas;

  theta: constant:= 0.501;

  fr: file_type;

  type vector is array( integer range <> ) of real;

  uk, ukp1: vector( 0..N+1 );  -- solution d'essai et reste pour Newton

  -- Matrice tridiagonales

  type tridiag( nn: positive ) is record
     d: vector( 1..nn ); -- diag
     l: vector( 2..nn ); -- diag inf
     r: vector( 2..nn ); -- diag sup
  end record;

  A, B: tridiag( N ); -- matrices implicite et explicite

  vb: vector( 1..N ); -- 2nd membre

  -- Sous-programmes

  -- Multiplication y:= A x

  procedure Mult( A: in tridiag; x: in vector; y: out vector ) is
    bad_dimensions: exception;
    n: constant integer:= A.nn;
    begin
      if x'first /= 1 or y'first /= 1 then raise bad_dimensions; end if;
      if x'length /= n or y'length /= n then raise bad_dimensions; end if;
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

  -- Decomposition LR tridiagonale universelle

  procedure LR( A: in out tridiag ) is
    begin
      for i in A.l'range loop
        A.r(i):= A.r(i) / A.d(i-1);
        A.d(i):= A.d(i) - A.r(i)*A.l(i);
      end loop;
    end LR;

  -- Resoudre  A x = d  sur une matrice A tridiagonale apres decomposition LR

  procedure Solve_LR( A: in out tridiag;
                      d: in vector;
                      x: in out vector ) is

    y: vector( d'range );

    begin
      -- Ly=d :
      y(1):= d(1) / A.d(1);
      for i in 2 .. y'last loop
        y(i):= ( d(i) - A.l(i) * y(i-1) ) / A.d(i);
      end loop;
      -- Rx=y :
      x(A.nn):= y(A.nn);
      for i in reverse 1 .. A.nn-1 loop
        x(i):= y(i) - A.r(i+1) * x(i+1);
      end loop;
    end Solve_LR;

  procedure Put( f: file_type; u: vector ) is
    begin
      -- Put( f,"{ " );
      for i in u'range loop
        Put(f, u(i), 3,3,0);
        -- if i < u'last then Put(f,','); end if;
      end loop;
      -- Put_Line( f, " }") ;
      New_Line( f );
    end Put;

begin
  -- Condition initiale
  for j in uk'range loop uk(j):= sin(2.0*3.141592653*real(j)*h); end loop;

  Create( fr, name=> "uchal.dat" );
  -- Put( fr,"{ " );
  Put( fr, uk );
  
  -- Remplir matrices A
  for i in 1..N loop A.d(i):= 1.0 + theta * tau *   2.0  / (h*h); end loop;
  for i in 2..N loop A.l(i):=     + theta * tau * (-1.0) / (h*h); end loop;
  A.r:= A.l; -- A est symetrique

  -- Remplir matrice B
  for i in 1..N loop B.d(i):=  (1.0 + (theta-1.0) * tau *   2.0  / (h*h)); end loop;
  for i in 2..N loop B.l(i):=  (      (theta-1.0) * tau * (-1.0) / (h*h)); end loop;
  B.r:= B.l; -- B est symetrique
  
  LR( A );              -- Decomposition LR de A

  -- Boucle principale d'evolution en temps
  temps: for k in 1..N_pas loop
    Mult( B, uk(1..N), vb );          -- 2nd membre
    Solve_LR( A, vb, ukp1(1..N) );    -- Resolution. on a fait: [A ukp1 = B uk] 
    uk:= ukp1;
    Put( fr, uk ); -- Ecriture de la solution
    -- if k < N_pas then Put(fr,','); end if;
  end loop temps;

  -- Put_Line( fr, " }" );
  Close( fr );

end Diffchal;
