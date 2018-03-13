------------------------------------------------------------------------------
--  File:            sparse.ads
--  Description:     Sparse matrices, generic package
--                     Compressed Row Storage (CRS) format
--                     doc. @ http://www.netlib.org/
--
--  Date / Version:  01-Jan-2011 ; 21-Nov-2010 ;
--                   08-Jun-2001 ; ... ; 29-Mar-1999
--
--  Author:          Olivier Besson, Universite de Neuchatel & Cray research
--                   Olivier.Besson (at) UniNe.ch
--
--  Ada version:     Gautier de Montmollin
--                   http://gautiersblog.blogspot.com/
--
--  NB: From 8-Jun-2001 version, (bi)conjugate gradient algorithms
--      are detached in a package independent of matrix type
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

generic
  type Real is digits <>;
  type Index is range <>;
  type Vector is array (Index range <>) of Real;

package Sparse is

  type Index_array is array(Index range <>) of Index;

  --------------------------------------------------
  -- Define a matrix with Compressed Rows Storage --
  --------------------------------------------------

  type CRS_matrix( rows_max_p1, nnz_max: Index ) is record
    val       : Vector(1..nnz_max);
    col_ind   : Index_array(1..nnz_max);
    row_ptr   : Index_array(1..rows_max_p1); -- p1 means +1 -> extra row ptr
    symmetric : Boolean;
    -- rows and nnz are effective upper bounds for matrix re-use
    rows      : Index:= rows_max_p1-1; -- must be in [ 1..rows_max_p1-1 ]
    nnz       : Index:= nnz_max;       -- must be in [ 1..nnz_max ]
  end record;

  -- Access (pointer) to matrix for dynamic allocation/deallocation:

  type p_CRS_matrix is access CRS_matrix;

  procedure Dispose is new Ada.Unchecked_Deallocation(CRS_matrix,p_CRS_matrix);

  -- Just returns the symmetric indicator, no verification

  function Defined_symmetric( A: CRS_matrix ) return Boolean;

  function Rows( A: CRS_matrix ) return Index;

  ----------------------------------
  -- Matrix-vector multiplication --
  ----------------------------------

  -- w:= A*u

  procedure Mult( A: in CRS_matrix; u: Vector; w: in out Vector );

  -- operator version - warning: uses stack, not for large/fast usage !
  --   function "*"( A: CRS_matrix; u: vector ) return vector;

  ------------------------------------------
  -- Put/Add/Get data into/in/from matrix --
  ------------------------------------------

  procedure Put( A: in out CRS_matrix; i,j: Index; value: Real );
  procedure Add( A: in out CRS_matrix; i,j: Index; value: Real );
  function  Get( A: in     CRS_matrix; i,j: Index ) return Real;

  -- pragma Inline(Put, Add);

  position_not_found_in_sparse_matrix: exception;

  ------------------------------------------------
  -- Diverse - re-export of BLAS-style routines --
  ------------------------------------------------

  procedure Copy ( u: in Vector; v: out Vector );
  function "*"(u, v: Vector) return Real;
  procedure Add_scaled ( factor: Real; u: in Vector; v: in out Vector );
  procedure Scale ( factor: Real; u: in out Vector );

end Sparse;
