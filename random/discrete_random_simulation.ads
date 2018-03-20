----------------------------------
--  DISCRETE RANDOM SIMULATION  --
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

  type Real is digits <>;
  --  The following array type represents an empiric random variable
  --  with integer values, like 0,1,2,...n and cumulative probabilities
  --  p0, p0+p1, p0+p1+p2, p0+p1+...+pn = 1
  --
  --  Caution:
  --
  --    1) The first probability value in the array must be 0.0 since the array
  --           represents the function F(x) = P(X<x) (variant with strict "<").
  --
  --    2) It is advised to avoid an 1.0 as final value for
  --           the "x=infinite case" even if it looks fine, since
  --           it will be drawn sometimes due to random U01 values
  --           very close to 1 that satisify, numerically, 1.0 <= U01
  --
  --  Examples with correct data:
  --
  --     Flip-or-coin: (0.0, 0.5)
  --     Dice: (0.0, 1.0/6.0, 2.0/6.0, 3.0/6.0, 4.0/6.0, 5.0/6.0)
  --
  --  See Test_Discrete_Random_Simulation for more.

  type Cumulative_distribution_function is array(Integer range <>) of Real;

package Discrete_Random_Simulation is

  type Discrete_random_simulation_mode is
    ( --  Linear: we scan the whole array of cumulated probabilities
      --  in order to find the most appropriate value.
      linear,
      --  Dichotomic search instead of linear.
      dichotomic
      --  There are faster but trickier algorithms
      --  (e.g. A.J. Walker, in (Knuth Volume 2, 3.4.1.A, p.119))
    );

  ----------------------------------------------------------------------------
  --  The function Index gives the appropriate index in an array containing --
  --  a CDF, given a value in [0,1]. Indeed, it is an inverse CDF function. --
  --  With the integer value it is trivial then to obtain the x for a       --
  --  general discrete random variable: just have an array x(i) for that.   --
  ----------------------------------------------------------------------------

  generic
    --  The simulation mode is generic for performance purposes
    --    (and you don't want to use more than one method, do you ?) :
    discrete_random_mode: Discrete_random_simulation_mode;
  function Index(
    U01 : Real;  --  Probability value, assumed to be uniform in [0,1]
    Fx  : Cumulative_distribution_function  --  CDF
  )
  return Integer;

  pragma Inline(Index);  --  For performance

end Discrete_Random_Simulation;
