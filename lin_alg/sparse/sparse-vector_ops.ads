-- Basic Linear Algebra routines for Sparse package

-- Implementations (package bodies) with or without BLAS

generic
  type real is digits <>;
  type index is range <>;
  type vector is array(index range <>) of real;

package Sparse.Vector_Ops is
  procedure Copy( u: in vector; v: out vector );
  function "*"(u,v: vector) return real;
  procedure Add_scaled( factor: real; u: in vector; v: in out vector );
  procedure Scale( factor: real; u: in out vector );

  pragma Inline("*", Add_scaled, Scale);

end Sparse.Vector_Ops;
