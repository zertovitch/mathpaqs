--------------------------------
-- Generic_Random_Functions
--
-- Functions facilitating computations with various random distributions
-------------
--
-- Author: G. de Montmollin, February 2010 and later
--
-- Legal licensing note:

--  Copyright (c) 2010..2011 Gautier de Montmollin

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

  --------------------------------------------------
  -- Inverse CDF's and other simulation functions --
  --------------------------------------------------

  ----------------------
  -- Normal variables --
  ----------------------

  -- Box-Muller: returns a pair of
  -- independent N(0,1) normal variables from
  -- a pair of independent U(0,1) uniform variables
  --
  procedure Box_Muller(u1,u2: in Real; n1,n2: out Real);
  pragma inline(Box_Muller);

  -----------------------
  -- Poisson variables --
  -----------------------

  -- lambda is the frequency. E(N) = lambda when N ~ Poisson(lambda)
  -- U is meant to be an U(0,1) uniform generator
  -- !! This method is suboptimal, since the random generator
  -- is called several times.
  -- Ideally it should be called once and passed as a Real variable.
  --
  generic
    with function U return Real;
  function Poisson(lambda: Real) return Natural;
  pragma inline(Poisson);

  -----------
  -- CDF's --
  -----------

  function Normal_CDF(x: Real) return Real;

end Generic_Random_Functions;
