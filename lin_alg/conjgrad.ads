------------------------------------------------------------------------------
--  File:            ConjGrad.ads
--  Description:     Conjugate gradient iterative methods for Ax=b
--                   Generic package (independent of matrix type)
--
--  Date / Version:  01-Jan-2011 ; 21-Nov-2010 ;
--                   30-Nov-2001 ; 08-Jun-2001 ; 29-Mar-1999
--
--  Author:          Olivier Besson, Universite de Neuchatel & Cray research
--                   Olivier.Besson (at) UniNe.ch
--
--  Ada version:     Gautier de Montmollin
--                   http://gautiersblog.blogspot.com/
--
------------------------------------------------------------------------------
-- 01-Jan-2011 : merged with 30-Nov-2001 version
-- 12-Oct-2001 : preconditioner fully managed by CG/BiCGStab algorithms

generic

  type Real is digits <>;
  type Index is range <>;
  type Vector is array(Index range <>) of Real;

  type Any_matrix (<>) is private;
  -- NB: 2 syntaxes for instanciating that as unconstrained type :
  -- [Ada 95+] type Any_matrix (<>) is private;
  -- [Ada 83]  type Any_matrix is private;

  with function  Get( A: Any_matrix; i,j: Index ) return Real;
  with function Rows( A: Any_matrix ) return Index;
  with function Defined_symmetric( A: Any_matrix ) return Boolean;

  -----------------------
  -- Vector operations --
  -----------------------

  with procedure Copy( u: in Vector; v: out Vector );
  with function "*"(u,v: Vector) return Real;
  with procedure Add_scaled( factor: Real; u: in Vector; v: in out Vector );
  with procedure Scale( factor: Real; u: in out Vector );

  ------------------------------------
  --  Matrix-vector multiplication  --
  ------------------------------------

  with procedure Mult( A: in Any_matrix; u: Vector; w: in out Vector );

package ConjGrad is

  --  Kind of preconditioner for iterative methods

  type t_precond is ( none, diag, tridiag );

  -----------------------------------------------------------------------------------
  --  Iterative solving of Ax=b symmetric system by the conjugate gradient method  --
  -----------------------------------------------------------------------------------

  procedure CG ( A : in Any_matrix;
                 b : Vector;
                 x : in out Vector;    -- * input:  1st approx;
                                       -- * output: solution of Ax=b
                 tol: Real;            -- tolerance
                 precond: t_precond;   -- kind of preconditioning
                 itmax: Index;         -- maximum number of iterations
                 ite: out Index        -- last iteration
               );

  ----------------------------------------------------------------
  --  Stabilized biconjugate gradient method (A non symmetric)  --
  ----------------------------------------------------------------

  procedure BiCGStab ( A : in Any_matrix;
                       b : Vector;
                       x : in out Vector;    -- * input:  1st approx;
                                             -- * output: solution of Ax=b
                       eps_rho  : Real;      -- minimal step allowed
                       tol_omega: Real;      -- tolerance
                       tol      : Real;      -- tolerance
                       precond: t_precond;   -- kind of preconditioning
                       itmax: Index;         -- maximum number of iterations
                       ite: out Index        -- last iteration
                     );

  --  Exceptions raised by CG / BiCG
  non_symmetric, not_converging, dot_prod_rho_too_small: exception;

  iteration_at_failure: Index;  --  Information on iteration at last failure

  --  Preconditionning exceptions
  tridiagonal_singularity: exception;

end ConjGrad;
