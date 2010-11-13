------------------------------------------------------------------------------
--  File:            frac_euclid.ads
--  Description:     Generic package, gives back the field of quotients of an
--                     euclidean ring (i.e. a ring with division; this allows
--                     for *reduction* of fractions)
--  Date/version:    22-Dec-1996
--  Author:          Gautier de Montmollin
--                   http://gautiersblog.blogspot.com/
------------------------------------------------------------------------------

generic                              -- to provide:
          type ring_elt is private;                  -- ring element type
          zero, one: ring_elt;                       -- 0 and 1 elements

          with function "-" (a:ring_elt) return ring_elt;    -- unary oper.
          with function "+" (a,b:ring_elt) return ring_elt;  -- binary oper.
          with function "-" (a,b:ring_elt) return ring_elt;
          with function "*" (a,b:ring_elt) return ring_elt;
          with function "/" (a,b:ring_elt) return ring_elt;
                        -- returns the quotient:  a= b*q + r
                        -- q:quotient, r:rest

package Frac_euclid is
  type frac_elt is record a,b:ring_elt; end record;     -- define fraction

  frac_0: constant frac_elt:= (zero,one);
  frac_1: constant frac_elt:= (one,one);

  function Reduction(f: frac_elt) return frac_elt;
  pragma Inline(Reduction);

  function "+" (f: frac_elt) return frac_elt;                 -- unary oper.
  function "-" (f: frac_elt) return frac_elt;

  function "+" (f1,f2: frac_elt) return frac_elt;             -- binary oper.
  function "+" (a: ring_elt; f: frac_elt) return frac_elt;
  function "+" (f: frac_elt; a: ring_elt) return frac_elt;
  function "-" (f1,f2: frac_elt) return frac_elt;
  function "-" (a: ring_elt; f: frac_elt) return frac_elt;
  function "-" (f: frac_elt; a: ring_elt) return frac_elt;
  function "*" (f1,f2: frac_elt) return frac_elt;
  function "*" (a: ring_elt; f: frac_elt) return frac_elt;
  function "*" (f: frac_elt; a: ring_elt) return frac_elt;
  function "/" (f1,f2: frac_elt) return frac_elt;
  function "/" (a: ring_elt; f: frac_elt) return frac_elt;
  function "/" (f: frac_elt; a: ring_elt) return frac_elt;
  function "/" (a,b: ring_elt) return frac_elt;

  function Eq(f1,f2: frac_elt) return Boolean;               -- returns f1=f2

  auto_reduce: Boolean:= True;
  reduce_in_add: Boolean:= True;

  Zero_denominator:          exception;
  Division_by_null_fraction: exception;

end Frac_Euclid;
