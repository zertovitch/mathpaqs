------------------------------------------------------------------------------
--  File:            EuRinToo.ads      (possibly extracted from MATHPAQS.ZIP)
--  Description:     Generic package for euclidean rings
--  Date/version:    22.12.1996
--  Author:          Gautier.deMontmollin@Maths.UniNe.CH
------------------------------------------------------------------------------

generic                              -- to provide:
          type ring_elt is private;                  -- ring element type 
          zero, one: ring_elt;                       -- 0 and 1 elements

          with function "-" (a,b:ring_elt) return ring_elt;  -- binary oper.
          with function "*" (a,b:ring_elt) return ring_elt;
          with function "/" (a,b:ring_elt) return ring_elt; 
                        -- returns the quotient:  a= b*q + r
                        -- q:quotient, r:rest

package Euclidean_Ring_Tools is

  function  GCD(a,b:ring_elt) return ring_elt;                -- returns (a,b)
  procedure Bezout(a,b:in ring_elt; s,t:out ring_elt);
          -- gives the s,t for the (a,b)= as+bt factorization (Bezout theorem)

  procedure GCD_and_Bezout(a,b:in ring_elt; s,t,the_gcd:out ring_elt);

end Euclidean_Ring_Tools;
