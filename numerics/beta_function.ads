---------------------------------
--  Beta special function      --
--  Test procedure: Test_Beta  --
---------------------------------
--
-- Legal licensing note:

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

-- NB: this is the MIT License, as found 9-Feb-2011 on the site
-- http://www.opensource.org/licenses/mit-license.php

generic
  type Real is digits <>;

package Beta_function is
  --  History and notes about accuracy can be found in the implementation

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

  -----------------------------------------------------------------------------------------------
  --  !! Inverse beta functions have some suspicious borderline cases (to be investigated) !!  --
  -----------------------------------------------------------------------------------------------

  --  Find x such as y = Beta(x, a, b).
  --
  function Inverse_Beta (y, a, b : Real) return Real;

  --  Find x such as y = Regularized_Beta(x, a, b).
  --
  --  It is also the inverse CDF of the Beta probability law.
  --  Excel: BETA.INV(y,a,b)
  function Inverse_Regularized_Beta (y, a, b : Real) return Real;

end Beta_function;
