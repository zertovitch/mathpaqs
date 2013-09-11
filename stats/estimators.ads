----------------
-- ESTIMATORS --
----------------
-- This package provides various statistical estimators
-------------
-- Author: G. de Montmollin, September 2013 and later
--
-- Legal licensing note:

--  Copyright (c) 2013 Gautier de Montmollin

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

-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php

generic

  type Real is digits <>;
  type Data_vector is array(Positive range <>) of Real;

package Estimators is

  -- Linear regression using the least squares criterion
  -- Compute a and b for the function v(u):= a + b * u such as
  -- sum(( v(x(i)) - y(i) ) ** 2) is minimal.
  procedure Linear_least_squares(x, y: Data_vector; a, b: out Real);

  -- Same as above, but assuming x = (1,2,3,...)
  procedure Linear_least_squares(y: Data_vector; a, b: out Real);

end Estimators;
