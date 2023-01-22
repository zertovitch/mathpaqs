------------------------------------------------------------------------------
--  File:            rationals.ads
--  Description:     Rational numbers, using the standard 'Integer' type
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------
with Frac_Euclid;

package Rationals is
  new Frac_Euclid (Integer, 0, 1, "-", "+", "-", "*", "/");

--  Put "type Rational is new Rationals.frac_elt;" in your units using
--  this package...
