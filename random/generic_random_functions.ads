--------------------------------
-- Generic_Random_Functions
--
-- Functions facilitating computations with various random distributions.
-- These functions can be called by more elaborate object-oriented
-- random distribution classes with CDF, Inverse_CDF, or Simulate methods.
-- The purpose here is a direct, "ad-hoc" utility.
-------------
--
-- Author: G. de Montmollin, February 2010 and later
--
-- Legal licensing note:

--  Copyright (c) 2010..2013 Gautier de Montmollin

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

-- NB: this is the MIT License, as found 9-Feb-2011 on the site
-- http://www.opensource.org/licenses/mit-license.php

----------------
-- To do list --
----------------
-- * add plenty of random variables!
-- * Poisson simulation with inverse CDF, i.e. with 1 use of random generation

generic

  type Real is digits <> ;

package Generic_Random_Functions is

  -----------------------------------------------------------------------------
  -- ======================================================================= --
  -- 1. Inverse Cumulative Distribution Functions and other simulation tools --
  -- ======================================================================= --
  -----------------------------------------------------------------------------

  -----------------------------------------
  -- 1.1 Continuous random distributions --
  -----------------------------------------

  -------------------------
  -- Normal distribution --
  -------------------------

  function Normal_inverse_CDF(y0: Real) return Real;
  pragma Inline(Normal_inverse_CDF);

  -- Box-Muller: returns a pair of
  -- independent N(0,1) normal variables from
  -- a pair of independent U(0,1) uniform variables
  --
  procedure Box_Muller(u1, u2: in Real; n1, n2: out Real);
  pragma Inline(Box_Muller);

  -------------------------
  -- Pareto distribution --
  -------------------------

  function Pareto_inverse_CDF(q, threshold, minus_inv_alpha: Real) return Real;
  pragma Inline(Pareto_inverse_CDF);

  -- q = 1-p where p is the CDF value.
  -- minus_inv_alpha = -1 / alpha
  -- For simulating Pareto, you can pass a uniformly generated u~U(0,1) as q,
  -- instead of 1-u, since 1-u is U(0,1) too.

  ---------------------------------------
  -- 1.2 Discrete random distributions --
  ---------------------------------------

  --------------------------
  -- Poisson distribution --
  --------------------------

  -- lambda is the frequency. E(N) = lambda when N ~ Poisson(lambda)
  -- U is meant to be an U(0,1) uniform generator
  -- NB: This method is suboptimal, since the random generator
  -- is called one or more times. Ideally U should be called once and passed
  -- as a Real variable to the Poisson function.
  --
  generic
    with function U return Real;
  function Poisson(lambda: Real) return Natural;
  pragma Inline(Poisson);

  ------------------------------------------
  -- ==================================== --
  -- 2. Cumulative Distribution Functions --
  -- ==================================== --
  ------------------------------------------

  -----------------------------------------
  -- 2.1 Continuous random distributions --
  -----------------------------------------

  -------------------------
  -- Normal distribution --
  -------------------------

  function Normal_CDF(x: Real) return Real;
  pragma Inline(Normal_CDF);

  -------------------------
  -- Pareto distribution --
  -------------------------

  function Pareto_CDF(x, threshold, alpha: Real) return Real;

end Generic_Random_Functions;
