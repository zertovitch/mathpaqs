-------------
-- SAMPLES --
-------------
-- Package for building samples of random values, then computing
-- statistics from these samples
-------------
-- Author: G. de Montmollin, August 2007 and later
--
-- Legal licensing note:

--  Copyright (c) 2007..2009 Gautier de Montmollin

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
  type Quantile_table is array(Positive range <>) of Real;

package Samples is

  type Histogram_type is array(Natural range <>) of Natural;

  ------------------------
  -- Random sample type --
  ------------------------

  type Sample(bins: Positive) is private;

  ---------------------------------------------------------
  -- (1) Define a sample with minimum and maximum values --
  ---------------------------------------------------------

  procedure Initialize(
    s       : out Sample;
    min, max: in  Real
  );

  Sample_not_initialized: exception;

  --------------------------------------
  -- (2) Add any number of occurences --
  --------------------------------------

  procedure Add_occurence(s: in out Sample; value: Real);
  pragma Inline(Add_occurence);

  Value_out_of_sample_range: exception;

  -- Getting measures from a sample after all occurences are recorded --

  type Measure(quantile_levels: Positive) is record
    -------------
    -- Moments --
    -------------
    mean,     -- = E(X), mathematical expectation
    std_dev,  -- = sqrt(Var(X))
    --
    -- Statistical error
    stat_err: Real;
    --------------------------------------
    -- Information on various quantiles --
    --------------------------------------
    --
    -- level(i): probability level p_i
    level   : Quantile_table(1..quantile_levels);
    -- VaR(i) such that P(X<VaR(i)) = p_i (x-value of the cumulative prob. F)
    VaR     : Quantile_table(1..quantile_levels);
    -- TailVar(i) = E(X|X>VaR(i))
    TailVaR : Quantile_table(1..quantile_levels);
  end record;

  ------------------------------------------------
  -- (3) Get statistical measures of the sample --
  ------------------------------------------------

  procedure Get_measures(
    s: in     Sample;
    m: in out Measure -- "in" are the quantile levels (m.level)
  );

  No_occurence                 : exception;
  Quantile_level_negative      : exception;
  Quantile_levels_not_ascending: exception;
  Unexpected_case              : exception;

private

  type Sample(bins: Positive) is record
    histogram       : Histogram_type(1..bins);
    min, max        : Real;
    width_inv       : Real;
    total_occurences: Natural:= 0;
    bins_r          : Real;
    bins_inv        : Real;
    factor          : Real;
    initialized     : Boolean:= False;
  end record;

end Samples;
