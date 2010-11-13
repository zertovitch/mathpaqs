------------------------------------------------------------------------------
--  File:            ratipoly.ads
--  Description:     Polynomials with coefficients of 'rational' type
--  Author:          Gautier.deMontmollin@Maths.UniNe.CH
------------------------------------------------------------------------------
With Polynomials;

package Rationals.Polynomials is new
           Polynomials( frac_elt, frac_0, frac_1, "-","+","-","*","/","=" ); 
