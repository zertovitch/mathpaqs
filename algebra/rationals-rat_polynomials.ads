------------------------------------------------------------------------------
--  File:            rationals-rat_polynomials.ads
--  Description:     Polynomials with coefficients of the 'rational' type
--  Author:          Gautier.deMontmollin@Maths.UniNe.CH
------------------------------------------------------------------------------

with Polynomials;

package Rationals.Rat_Polynomials is new
           Polynomials (frac_elt, frac_0, frac_1, "-", "+", "-", "*", "/", "=");
