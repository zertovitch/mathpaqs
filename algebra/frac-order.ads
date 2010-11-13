------------------------------------------------------------------------------
--  File:            fracorde.ads
--  Description:     Supplement of generic package 'Frac' to provide an
--                   order relation from one of the ring
--  Date/version:    22.12.1996
--  Author:          Gautier.deMontmollin@Maths.UniNe.CH
------------------------------------------------------------------------------

generic with function "<" (a,b:ring_elt) return boolean;   -- order relation 

package Frac.Order is

  function "ABS" (f: frac_elt) return frac_elt;
  function "<" (f1,f2: frac_elt) return boolean;
  function ">" (f1,f2: frac_elt) return boolean;
  function ">=" (f1,f2: frac_elt) return boolean;
  function "<=" (f1,f2: frac_elt) return boolean;

end;
