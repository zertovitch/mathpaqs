------------------------------------------------------------------------------
--  File:            ConjGrad.ads
--  Description:     Conjugate gradient iterative methods for Ax=b
--                   Generic package (independent of matrix type)
--
--  Date / Version:  21-Nov-2010 ; 8-Jun-2001 ; 29-Mar-1999
--
--  Author:          Olivier Besson, Universite de Neuchatel & Cray research
--                   Olivier.Besson@UniNe.ch
--
--  Ada version:     Gautier de Montmollin
--                   http://gautiersblog.blogspot.com/
--
------------------------------------------------------------------------------

generic

  type Real is digits <>;
  type Index is range <>;
  type Vector is array(Index range <>) of Real;

  type Any_matrix (<>) is private;

  with function Rows( A: Any_matrix ) return Index;

  -----------------------
  -- Vector operations --
  -----------------------

  with procedure Copy( u: in vector; v: out vector );
  with function "*"(u,v: vector) return real;
  with procedure Add_scaled( factor: real; u: in vector; v: in out vector );
  with procedure Scale( factor: real; u: in out vector );

  ----------------------------------
  -- Matrix-vector multiplication --
  ----------------------------------

  with procedure Mult( A: in Any_matrix; u: vector; w: in out vector );

package ConjGrad is

  -- Kind of preconditioner for iterative methods

  type t_precond is ( none, diag, tridiag );

  -----------------------------------------------------------------------------
  -- Iterative solving of Ax=b symmetric system by conjugate gradient method --
  -----------------------------------------------------------------------------

   procedure CG ( A : in Any_matrix;
                  b : vector;
                  x : in out vector;    -- input:  1st approx;
                                        -- output: solution of Ax=b
                  tol: real;            -- tolerance
                  precond: t_precond;   -- kind of preconditioning
                  precd, precl: vector; -- diag. & lower diag. of precond.
                  itmax: index;         -- maximum number of iterations
                  ite: out index        -- last iteration
                );

  --------------------------------------------------------------
  -- Stabilized biconjugate gradient method (A non symmetric) --
  --------------------------------------------------------------

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
                      );

   -- Exceptions raised by CG / BiCG

   non_symmetric, not_converging, dot_prod_rho_too_small: exception;

end ConjGrad;
