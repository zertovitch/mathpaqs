--
-- COPULAS
--
--   A copula is a multivariate uniform distribution function.
--   Thanks to Sklar's theorem, any multivariate random distribution
--   can be decomposed in a unique copula and marginal distributions.
--   Thus, a copula contains all depedency information and can be used
--   separately from the random distributions themselves.
--
-- Author: G. de Montmollin, January 2009 and later
--         http://gautiersblog.blogspot.com/
--
-- Legal licensing note:
--
--  Copyright (c) 2009..2013 Gautier de Montmollin
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.
--
-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php

------------
-- Changes
--
-- 09-Nov-2010: Fixed a bug in Gaussian copula (the last element of an odd
--                number of dependent variables was wrongly simulated)
-- 17-Jul-2009: U(0,1) Random generator is provided by the user
-- 16-Apr-2009: Evolutive object hierachy implemented
--    Jan-2009: Copula type as rigid record type

with Ada.Numerics.Generic_Real_Arrays;

generic

  -- A floating-point type of any precision
  --
  type Real is digits <> ;

  -- Here, any random generator can be provided: the Ada.Numerics.Float_Random
  -- one, or another one. Rationale: some implementations (GNAT, Aonix) use a
  -- very slow algorithm, the Blum-Blum-Shub 1986 one.
  --
  type RGN_Float is digits <> ; -- is Float for Ada.Numerics.Float_Random
  type Generator is limited private;
  with function Random (Gen : Generator) return RGN_Float;

  with package GRA is new Ada.Numerics.Generic_Real_Arrays(Real);

package Copulas is

  use GRA;

  ----------------------
  -- Root copula type --
  ----------------------

  type Copula(dim : Positive) is abstract tagged private;

  -- Produce a uniform U(0,1) pseudo-random vector generated
  -- by seed in 'gen' and having dependencies from copula 'C'.

  function Simulate(C: Copula; gen: Generator) return Real_Vector
  is abstract;

  type Copula_access is access Copula'Class;
  procedure Dispose(C: in out Copula_access);

  ------------------------
  -- Independent copula --
  ------------------------

  type Independent_Copula is new Copula with private;
  function Simulate(C: Independent_Copula; gen: Generator) return Real_Vector;

  ---------------------
  -- Gaussian copula --
  ---------------------

  type Gauss_Copula is new Copula with private;
  function Simulate(C: Gauss_Copula; gen: Generator) return Real_Vector;

  procedure Construct_as_Gauss(
    C   : out Copula_access;
    dim : Positive;
    corr: Real_Matrix
  );
  -- The correlation matrix 'corr' may have less dimensions than C.dim .
  -- In that case, the other dimensions, from corr'Length(1)+1 up to C.dim,
  -- are considered independent.

  wrong_use, undefined: exception;

  -- Copula_family: enumerated type describing different
  -- basic kind of copulas.
  --
  -- NB: This type was unrelated to the type hierarchy descending from
  -- type Copula, brought confusion, and was of no practical use here:
  -- in practice you could have e.g. several gaussien copulas, several
  -- empirical ones, etc. to be alluded to by an enumerated type.
  -- So we comment out Copula_family.

  --    type Copula_family is
  --      (independent,
  --       -- elliptic - with a correlation matrix
  --       gaussian,
  --       student
  --      );

private

  trace: constant Boolean:= False;

  type p_Real_Matrix is access Real_Matrix;

  type Copula(dim : Positive) is abstract tagged null record;

  type Independent_Copula is new Copula with null record;

  type Gauss_Copula is new Copula with record
    dim_dep: Natural:= 0;
    -- number of dimensions that are actually dependent
    --         1..dim_dep  ----> dependent
    -- dim_dep+1..dim      ----> independent
    Sqrt_Correl_Matrix: p_Real_Matrix:= null;
    -- This the L matrix from a Cholesky decomposition A=LLt
    -- of a correlation matrix A
  end record;

end Copulas;
