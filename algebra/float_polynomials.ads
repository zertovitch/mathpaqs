------------------------------------------------------------------------------
--  File:            floapoly.ads
--  Description:     Polynomials with coefficients of 'float' type
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------
with Polynomials;

package Float_Polynomials is new
           Polynomials (Float, 0.0, 1.0, "-", "+", "-", "*", "/", "=");
