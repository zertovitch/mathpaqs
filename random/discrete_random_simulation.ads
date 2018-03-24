-------------------------------------------------------
--  DISCRETE RANDOM SIMULATION                       --
--  Test procedure: Test_Discrete_Random_Simulation  --
-------------------------------------------------------

----------------------------------
--  Package for simulating empiric discrete random variables, using a standard
--  pseudo-random generator with uniform floating-point values in the [0;1] interval.
----------------------------------
--
--  This is part of the Mathpaqs collection of mathematical packages.
--  Latest version may be available at:
--      home page:     http://mathpaqs.sf.net/
--      project page:  http://sf.net/projects/mathpaqs/
--      mirror:        https://github.com/svn2github/mathpaqs
--
-------------------------
--  Legal licensing note:

--  Copyright (c) 2011 .. 2018 Gautier de Montmollin

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 9-Feb-2011 on the site
--  http://www.opensource.org/licenses/mit-license.php

generic

  --  Probability_value is any floating-point type; can be restricted to the 0.0 .. 1.0 range.
  --
  type Probability_value is digits <>;
  type Probability_array is array(Integer range <>) of Probability_value;

package Discrete_Random_Simulation is

  ----------------------------------------------------------------------------
  --  The function Index gives the appropriate index of an array Fx containing
  --  a Cumulative Distribution Function (CDF), given a value in [0,1].
  --  Index is given in three variants with different usefulness/performance features:
  --
  --     Index_Linear_Search
  --     Index_Dichotomic_Search
  --     Index_Aliases
  --
  --  The Fx array type represents an empiric random variable
  --  with integer values, like 0,1,2,...n, and cumulative probabilities
  --  p0, p0+p1, ... , p0+p1+...+pn = 1
  --
  --  Indeed, Index is an inverse CDF function.
  --
  --  For general discrete random variables with values that are not all integers
  --  you can use an array x(i). Example: x(Index_Linear_Search(Random(gen), Fx)).
  --
  --  Caution:
  ------------
  --
  --    1) The first probability value in the array must be 0.0. The array
  --           represents the function F(x) = P(X<x) (variant of P with strict "<").
  --
  --    2) It is advised to avoid an 1.0 as final value for
  --           the "x=infinite case" even if it looks fine, since
  --           it will be drawn sometimes due to random U01 values
  --           very close to 1 that satisfy, *numerically*, 1.0 <= U01.
  --
  --  Examples with correct data (more in Test_Discrete_Random_Simulation):
  --
  --     Flip-or-coin: (0.0, 0.5)
  --     Dice: (0.0, 1.0/6.0, 2.0/6.0, 3.0/6.0, 4.0/6.0, 5.0/6.0)
  ----------------------------------------------------------------------------

  -------------------------------------------------------------------------
  --  Linear search: we scan the whole array of cumulated probabilities  --
  --  in order to find the most appropriate value.                       --
  --  O(n), best for small arrays.                                       --
  -------------------------------------------------------------------------

  function Index_Linear_Search (
    U01 : Probability_value;  --  Probability value. For simulation: random, uniform in [0,1]
    Fx  : Probability_array   --  Fx is the Cumulative distribution function (CDF), F(x).
  )
  return Integer;
  pragma Inline(Index_Linear_Search);

  ------------------------------------------------------------
  --  Dichotomic search - divide and conquer algorithm.     --
  --  O(Log_2(n))... but slower where Fx is "plateau"-ing.  --
  ------------------------------------------------------------

  function Index_Dichotomic_Search (
    U01 : Probability_value;  --  Probability value. For simulation: random, uniform in [0,1]
    Fx  : Probability_array   --  Fx is the Cumulative distribution function (CDF), F(x).
  )
  return Integer;
  pragma Inline(Index_Dichotomic_Search); --  !! check performance of inlining

  --  Index_Aliases : TBD
  --  A.J. Walker, in (Knuth Volume 2, 3.4.1.A, p.119)

  --------------------------------------------------------------------------
  --  Utility: To_cumulative: conversion of an array with                 --
  --  single probability values to a CDF array.                           --
  --  Note that p(p'Last) is not used in the CDF (it is used implicitly)  --
  --------------------------------------------------------------------------
  --
  --  Example: Dice: To_cumulative((1..6 => 1.0/6.0));
  --
  function To_cumulative (p: Probability_array; check: Boolean:= False) return Probability_array;

end Discrete_Random_Simulation;
