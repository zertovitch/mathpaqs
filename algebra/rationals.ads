------------------------------------------------------------------------------
--  File:            rationals.ads
--  Description:     Rational numbers, from the 'integer' type
--  Author:          Gautier.deMontmollin@Maths.UniNe.CH
------------------------------------------------------------------------------
with Frac_Euclid; 

package Rationals is new Frac_Euclid(Integer, 0,1, "-","+","-","*","/");

-- Put "type rational is new Rationals.frac_elt;" in your units using
-- this package...
