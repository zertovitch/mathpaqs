---------------------------------
--  BETA SPECIAL FUNCTION      --
--  Test procedure: Test_Beta  --
---------------------------------
--
--  This is part of the Mathpaqs collection of mathematical packages.
--  Latest version may be available at:
--      home page:     http://mathpaqs.sf.net/
--      project page:  http://sf.net/projects/mathpaqs/
--      mirror:        https://github.com/zertovitch/mathpaqs
--
-------------------------
--  Legal licensing note:

--  Copyright (c) 2018 Gautier de Montmollin (Ada translation and maintenance)
--  Originally created by Stephen L. Moshier (see implementation for details)

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

---------------------------------------------------------------------
--  Extreme cases of the Beta function x |-> B(x,a,b):
--
--    - B is at some places very steep and the numerical
--        approximation is inaccurate
--    - B is at some places almost flat and the numerical
--        approximation of the inverse, B^{-1}, is inaccurate
--
--  This occurs on those different combinations of (a,b):
--
--   - B' is  U-shaped (a < 1 and b < 1) with a, b _small_
--   - B' is /\-shaped (a > 1 and b > 1) with a, b large
--   - B' is \_-shaped (a < 1 and b > 1) with b large
--   - B' is _/-shaped (a > 1 and b < 1) with a large
--
--  B' and B can be plotted online on many sites
--  (search "plot beta distribution online"), e.g.:
--    Beta distribution (chart) Calculator
--    http://keisan.casio.com/exec/system/1180573226
--
--  History and further notes about accuracy can be found in the implementation

generic
  type Real is digits <>;

package Beta_function is

  --  Complete Beta integral
  --                  1
  --                  -
  --                 | |  a-1     b-1
  --                 |   t   (1-t)   dt.
  --               | |
  --                -
  --                0
  --
  function Beta (a, b: Real) return Real;

  --  Incomplete Beta integral, with x in [0;1]
  --                  x
  --                  -
  --                 | |  a-1     b-1
  --                 |   t   (1-t)   dt.
  --               | |
  --                -
  --                0
  --
  function Beta (x, a, b: Real) return Real;

  --  Regularized Beta function, sometimes written as I_x(a,b).
  --  It is defined as: I_x(a,b) = Beta(x,a,b) / Beta(a,b).
  --
  --  It is also the cumulative distribution function (CDF) of the Beta probability law.
  --  Excel: BETA.DIST(x,a,b,TRUE)
  --
  function Regularized_Beta (x, a, b: Real) return Real;

  --  Find x such as y = Beta(x, a, b).
  --
  function Inverse_Beta (y, a, b : Real) return Real;

  --  Find x such as y = Regularized_Beta(x, a, b).
  --
  --  It is also the inverse CDF of the Beta probability law.
  --  Excel: BETA.INV(y,a,b)
  function Inverse_Regularized_Beta (y, a, b : Real) return Real;

end Beta_function;
