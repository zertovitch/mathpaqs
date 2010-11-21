with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; -- for debugging

package body ConjGrad is

  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  use REF;

  -- package rio is new Ada.Text_IO.float_io(real);
  -- package iio is new Ada.Text_IO.integer_io(index);


   procedure CG ( A : in Any_matrix;
                  b : vector;
                  x : in out vector;    -- input:  1st approx;
                                        -- output: solution of Ax=b
                  tol: real;            -- tolerance
                  precond: t_precond;   -- kind of preconditioning
                  precd, precl: vector; -- diag. & lower diag. of precond.
                  itmax: index;         -- maximum number of iterations
                  ite: out index        -- last iteration
                ) is

      neq :   constant index:= Rows(A);

      r,s,q : vector(1 .. neq); -- vecteurs de travail

      r20, rs, rsold, r2, qaq, alpha, beta : real;

      tol2: constant real:= tol * tol;

-------------------------------------------------------------------------
--     resolution d'un systeme lineaire symetrique defini positif
--     par une methode du gradient conjugue preconditionne
--     matrice creuse
-------------------------------------------------------------------------
--     auteur: o. besson, universite de neuchatel
-------------------------------------------------------------------------

--     resolution du systeme lineaire ax=b, a sym. def. pos.

--     methode du gradient conjugue preconditionne


--     precd: diagonale de la matrice de preconditionnement
--     precl: diagonale inferieure de la matrice de preconditionnement
--     b: second membre du systeme lineaire, de long. neq
--     u: en entree:   solution approchee initiale
--        a la sortie: solution du systeme
--     neq: nombre d'equations
--     ite: nombre d'iterations effectuees, si ite >= neq, erreur
--     r,s,q: vecteur de travail de longueur neq

     procedure soltri (d, dl, r: vector; u: in out vector) is
     --     solve the tridiagonal linear system a.u = r
     --     d: main diagonal of matrix l, the cholesky decomposition of a
     --     dl: lower diagonal of matrix l, the cholesky decomposition of a
     --     r: second member
     --     u: solution

     --     this routine uses the cholesky decomposition of the matrix a

     neq: constant index:= d'Last;

     begin
        -- forward substitution

        u(1) := r(1) / d(1);
        for j in u'First+1 .. u'Last loop
           u(j) := (r(j) - dl(j - 1) * u(j - 1)) / d(j);
        end loop;

        -- back substitution

        u(neq) := r(neq) / d(neq);
        for j in reverse 1..neq-1 loop
          u(j) := (u(j) - dl(j) * u(j + 1)) / d(j);
        end loop;

     end soltri;

   begin
--
--     initialisation
--
      for i in 1..neq loop
         s(i) := 0.0;
      end loop;

      Mult( A, x, s);  -- s:= a*x

      for i in 1..neq loop
         r(i) := b(i) - s(i);   -- r:= b - s
      end loop;

      r20 := 0.0;
      for i in 1..neq loop
         r20 := r20 + r(i) * r(i);
      end loop;
--
--     iterations
--
      for ite_cnt in 1..itmax loop
         ite:= ite_cnt;

         case precond is

           when none =>
             s:= r;

           when diag =>
            for i in 1..neq loop
               s(i) := r(i) * precd(i);
            end loop;

           when tridiag =>
            soltri( precd,precl,r,s );

         end case;

         rs := 0.0;
         for i in 1..neq loop
            rs := rs + r(i) * s(i);   -- rs:= (r|s)
         end loop;

         if  ite_cnt = 1  then
            q:= s;
            rsold := rs;
         else
            beta := rs / rsold;
            for i in 1..neq loop
               q(i) := s(i) + beta * q(i);   -- q:= s + beta*q
            end loop;
            rsold := rs;
         end if;

         Mult( A, q, s );   -- s:= a*q

         qaq := 0.0;
         for i in 1..neq loop
            qaq := qaq + q(i) * s(i);   -- qaq:= (q|a*q)
         end loop;

         alpha := rsold / qaq;
         for i in 1..neq loop
            x(i) := x(i) + alpha * q(i);   -- x:= x + alpha*q
            r(i) := r(i) - alpha * s(i);   -- r:= r - alpha*a*q
         end loop;

         r2 := 0.0;
         for i in 1..neq loop
            r2 := r2 + r(i) * r(i);
         end loop;

         if  r2 / r20 < tol2 then return; end if;
         --if  sqrt(r2 / r20) < tol then return; end if;

      end loop;

      raise not_converging;

   end CG;

   procedure BiCGStab ( A : in Any_matrix;
                        b : vector;
                        x : in out vector;    -- input:  1st approx;
                                              -- output: solution of Ax=b
                        eps_rho  : real;      -- minimal step allowed
                        tol_omega: real;      -- tolerance
                        tol      : real;      -- tolerance
                        precond: t_precond;   -- kind of preconditioning
                        precd, precl: vector; -- diag. & lower diag. of precond.
                        itmax: index;         -- maximum number of iterations
                        ite: out index        -- last iteration
                      ) is

--     Resolution du systeme lineaire ax=b
--     methode du gradient bi-conjugue preconditionne
-------------------------------------------------------------------------
--     auteur: o. besson, universite de neuchatel et cray research
-------------------------------------------------------------------------
--
--     precl: matrice de preconditionnement
--     b: second membre du systeme lineaire, de long. neq = A.rows
--     x: en entree:   solution approchee initiale
--        a la sortie: solution du systeme
--     itmax: nombre maximum d'iterations (par ex. itmax = neq)
--     ite: nombre d'iterations effectuees

      rhoold: real:= 1.0;
      rho, alpha, beta, omega, r20: real;

      neq: constant index:= Rows(A);

      p,p_hat,r,r_tild,s,s_hat,t,v : vector(1 .. neq); -- vecteurs de travail

      tol2: constant real:= tol * tol;

      ite_dbg: index:= 0;

   begin
      Mult( A, x, s);  -- s:= A*x

-- put_line("b"&integer'image(b'length));
-- put_line("r"&integer'image(r'length));
-- put_line("x"&integer'image(x'length));
      Copy( b, r );                -- r:= b;
      Add_scaled( -1.0, s, r);     -- r:= b - s

      r20 := r * r;
      Copy( r, r_tild ); -- r_tild:= r;

      alpha:= 0.0;   -- just to calm down GNAT's warnings
      omega:= 0.0;   --  "
      Copy( r, v );  --  "

      for ite_cnt in 1..itmax loop -- Main iteration loop
         ite_dbg:= ite_cnt;
         ite:=     ite_cnt;

         rho := r * r_tild;
         if  abs ( rho ) < eps_rho  then   -- la methode ne marche pas
            raise dot_prod_rho_too_small;
         end if;

         if  ite_cnt = 1  then
            Copy( r, p ); -- p:= r;
         else
            beta := rho * alpha / (rhoold * omega);
            Add_scaled( -omega, v, p );     -- p := p - omega*v
            Scale( beta, p );               -- p := beta*p
            Add_scaled( 1.0, r, p );        -- p := p + r
         end if;

         case precond is

           when none =>
             Copy( p, p_hat ); -- p_hat:= p;

           when diag =>
             for i in 1..neq loop
                p_hat(i) := p(i) * precd(i);
             end loop;
             -- put("diag!");
           when tridiag =>
             raise Constraint_Error;

--        else if (iprec .eq. 2) then ! autre preconditionneur a gauche
--          call solprl(A.nnz,A.col_ind,A.row_ptr,precl,neq,work(ip),work(ip_hat))
--                               ! precl*p_hat = p
         end case;

         Mult(A, p_hat, v);           -- v:= A*p_hat
         alpha := rho / (r_tild * v);

         Copy( v, s );                -- s := v;
         Scale( -alpha, s );          -- s := -alpha*s
         Add_scaled( 1.0, r, s );     -- s := s + r

         if  s*s < tol2  then
            Add_scaled( alpha, p_hat, x);   -- x := x + alpha*p_hat
            return;
         end if;

         case precond is

           when none =>
             Copy( s, s_hat ); -- s_hat:= s;

           when diag =>
             for i in 1..neq loop
               s_hat(i) := s(i) * precd(i);
             end loop;

           when tridiag =>
             raise Constraint_Error;
--        else if (iprec .eq. 2) then ! autre preconditionneur a gauche
--          call solprl(A.nnz,A.col_ind,A.row_ptr,precl,neq,work(is),work(is_hat))
--                               ! precl*p_hat = p
         end case;

         Mult( A, s_hat, t );   --t:= A*s_hat

         omega := (s*t) / (t*t);

         Add_scaled( alpha, p_hat, x); -- x := x + alpha*p_hat
         Add_scaled( omega, s_hat, x); -- x := x + omega*s_hat

         if  abs( omega ) < tol_omega then return; end if;

         Copy( s, r ); -- r:= s;

         Add_scaled( -omega, t, r );   -- r := r - omega*t

         if  sqrt(r*r) < tol then return; end if;

         rhoold := rho;
       end loop;

       raise not_converging;

    exception
      when others =>
        Ada.Text_IO.Put("Exception occured in BiCGStab at iteration " &
            index'Image(ite_dbg) & '/' & index'Image(itmax) );
        raise;
    end BiCGStab;

end ConjGrad;
