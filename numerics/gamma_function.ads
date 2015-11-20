---------------------------------------------
--  Gamma special function and Log(Gamma)
---------------------------------------------
--
-- Legal licensing note:

--  Copyright (c) 2015 Gautier de Montmollin (Ada translation and maintenance)
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

package Gamma_function is

  function Gamma(x : Real) return Real;
  --  Gamma function
  --
  --  Domain:
  --   0 < x < 171.6
  --   -170 < x < 0, x is not an integer.
  --  History and notes about accuracy in the implementation

  function Log_Gamma(x : Real) return Real;
  --  Natural logarithm of gamma function
  --
  --  Domain:
  --   0 < x < 2.55e305
  --   -2.55e305 < x < 0, x is not an integer.
  --  History and notes about accuracy in the implementation

end Gamma_function;
