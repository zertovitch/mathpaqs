------------------------------------------------------------------------------
--  File:            frac-order.ads
--  Description:     Child of the generic package 'Frac' which provides an
--                    order relation derived from the ring's order relation
--  Date/version:    22-Dec-1996
--  Author:          Gautier de Montmollin
--                   http://gautiersblog.blogspot.com/
------------------------------------------------------------------------------

generic with function "<" (a,b: ring_elt) return boolean;   -- order relation

package Frac.Order is

  function "ABS" (f: frac_elt) return frac_elt;
  function "<" (f1,f2: frac_elt) return boolean;
  function ">" (f1,f2: frac_elt) return boolean;
  function ">=" (f1,f2: frac_elt) return boolean;
  function "<=" (f1,f2: frac_elt) return boolean;

end;
