-----------------------------------------------------------------------
--  Random generation for enumerated types                           --
-----------------------------------------------------------------------
--  NB: see Discrete_Random_Simulation for more advanced techniques  --
-----------------------------------------------------------------------
--
--  This is part of the Mathpaqs collection of mathematical packages.
--  Latest version may be available at:
--      home page:     http://mathpaqs.sf.net/
--      project page:  http://sf.net/projects/mathpaqs/
--      mirror:        https://github.com/zertovitch/mathpaqs
--

--  17-Feb-2011: moved Uniform_random from generic function
--  to a parameter of Finite_Distributed_Random in order to
--  allow for non independent distributions.

--  From: Reinert Korsnes (reinert.korsnes@chello.no)
--  Subject: Discrete random with given distribution ?
--  Newsgroups: comp.lang.ada
--  View this article only
--  Date: 2002-06-13 05:25:17 PST
--

--  Hi,

--  Is it under Ada any "natural" way to generate random
--  elements of enumeration type (for example (a,b,c))
--  and with a given non-uniform probability distribution ?

--  I.e. so for example "a" is produced by probability 1/2
--  and "b" and "c" both with probability 1/4 ?

--  (Yes, I can do it via Float_Random, but the code looks ugly).

--  reinert

generic
  type A_Float is digits <>;  --  Any floating-point type
  type Thing is (<>);         --  Any discrete type
  type Proba_Array is array (Thing) of A_Float;

function Finite_Distributed_Random
  (proba          : Proba_Array;  --  Contains the probability for each element
   uniform_random : A_Float)      --  Uniformily distributed float in [0,1]
return Thing;
