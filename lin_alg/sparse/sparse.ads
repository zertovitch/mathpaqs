------------------------------------------------------------------------------
--  File:            sparse.ads
--  Description:     Sparse matrices, generic package
--                     Compressed Row Storage (CRS) format
--                     doc. @ http://www.netlib.org/
--
--  Date / Version:  21-Nov-2010 ; 8-Jun-2001 ; 19-Apr-2001 ; ... ; 29-Mar1999
--
--  Author:          Olivier Besson, Universite de Neuchatel & Cray research
--                   Olivier.Besson@UniNe.ch
--
--  Ada version:     Gautier.deMontmollin@Winterthur.ch
--
--  NB: From 8-Jun-2001 version, (bi)conjugate gradient algorithms
--      are detached in a package independent of matrix type
------------------------------------------------------------------------------

generic
  type real is digits <>;
  type index is range <>;
  type vector is array(index range <>) of real;

package Sparse is

   type index_array is array(index range <>) of index;

  --------------------------------------------------
  -- Define a matrix with Compressed Rows Storage --
  --------------------------------------------------

   type CRS_matrix( rows_max_p1, nnz_max: index ) is
   record
     val       : vector(1..nnz_max);
     col_ind   : index_array(1..nnz_max);
     row_ptr   : index_array(1..rows_max_p1); -- p1 means +1 -> extra row ptr
     symmetric : Boolean;
     -- rows and nnz are effective upper bounds for matrix re-use
     rows      : index:= rows_max_p1-1; -- must be in [ 1..rows_max_p1-1 ]
     nnz       : index:= nnz_max;       -- must be in [ 1..nnz_max ]
   end record;

   -- Access (pointer) type to matrix:

   type p_CRS_matrix is access CRS_matrix;

   -- Just returns the symmetric indicator, no verification

   function Defined_symmetric( A: CRS_matrix ) return Boolean;

   function Rows( A: CRS_matrix ) return Index;

  ----------------------------------
  -- Matrix-vector multiplication --
  ----------------------------------

   -- w:= A*u

   procedure Mult( A: in CRS_matrix; u: vector; w: in out vector );

   -- operator version - warning: uses stack, not for large/fast usage !
--   function "*"( A: CRS_matrix; u: vector ) return vector;


  ------------------------------------------
  -- Put/Add/Get data into/in/from matrix --
  ------------------------------------------

   procedure Put( A: in out CRS_matrix; i,j: index; value: real );
   procedure Add( A: in out CRS_matrix; i,j: index; value: real );
   function  Get( A: in     CRS_matrix; i,j: index ) return real;

   -- pragma Inline(Put, Add);

   position_not_found_in_sparse_matrix: exception;

  ------------------------------------------------
  -- Diverse - re-export of BLAS-style routines --
  ------------------------------------------------

  procedure Copy( u: in vector; v: out vector );
  function "*"(u,v: vector) return real;
  procedure Add_scaled( factor: real; u: in vector; v: in out vector );
  procedure Scale( factor: real; u: in out vector );

end Sparse;
