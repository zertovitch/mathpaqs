------------------------------------------------------------------------------
--  File:            ConjGrad.ads
--  Description:     Conjugate gradient iterative methods for Ax=b
--                   Generic package (independent of matrix type)
--
--  Date / Version:  21-Nov-2010 ; 30-Nov-2001 ; 8-Jun-2001 ; 29-Mar-1999
--
--  Author:          Olivier Besson, Universite de Neuchatel & Cray research
--                   Olivier.Besson (at) UniNe.ch
--
--  Ada version:     Gautier de Montmollin
--                   http://gautiersblog.blogspot.com/
--
------------------------------------------------------------------------------
-- 01-Jan-2010 : merged with 30-Nov-2001 version
-- 12-Oct-2001 : preconditioner fully managed by CG/BiCGStab algorithms

generic

  type real is digits <>;
  type index is range <>;
  type vector is array(index range <>) of real;

  type Any_matrix (<>) is private;
  -- NB: 2 syntaxes for instanciating that as unconstrained type :
  -- [Ada 95+] type Any_matrix (<>) is private;
  -- [Ada 83]  type Any_matrix is private;

  with function  Get( A: Any_matrix; i,j: index ) return real;
  with function Rows( A: Any_matrix ) return Index;
  with function Defined_symmetric( A: Any_matrix ) return Boolean;

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
                  x : in out vector;    -- * input:  1st approx;
                                        -- * output: solution of Ax=b
                  tol: real;            -- tolerance
                  precond: t_precond;   -- kind of preconditioning
                  itmax: index;         -- maximum number of iterations
                  ite: out index        -- last iteration
                );

  --------------------------------------------------------------
  -- Stabilized biconjugate gradient method (A non symmetric) --
  --------------------------------------------------------------

   procedure BiCGStab ( A : in Any_matrix;
                        b : vector;
                        x : in out vector;    -- * input:  1st approx;
                                              -- * output: solution of Ax=b
                        eps_rho  : real;      -- minimal step allowed
                        tol_omega: real;      -- tolerance
                        tol      : real;      -- tolerance
                        precond: t_precond;   -- kind of preconditioning
                        itmax: index;         -- maximum number of iterations
                        ite: out index        -- last iteration
                      );

   -- Exceptions raised by CG / BiCG
   non_symmetric, not_converging, dot_prod_rho_too_small: exception;

   iteration_at_failure: Index; -- information on iteration at last failure

   -- Preconditionning exceptions
   tridiagonal_singularity: exception;

end ConjGrad;
