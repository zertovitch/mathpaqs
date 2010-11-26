--From: Reinert Korsnes (reinert.korsnes@chello.no)
--Subject: Discrete random with given distribution ? 
--Newsgroups: comp.lang.ada
--View this article only 
--Date: 2002-06-13 05:25:17 PST 
-- 

--Hi,

--Is it under Ada any "natural" way to generate random
--elements of enumeration type (for example (a,b,c))
--and with a given non-uniform probability distribution ?

--I.e. so for example "a" is produced by probability 1/2
--and "b" and "c" both with probability 1/4 ?

--(Yes, I can do it via Float_Random, but the code looks ugly). 

--reinert

generic
  type A_float is digits <>;
  type Thing is (<>);
  type Proba_array is array(Thing) of A_float;
  with function Uniform_random return A_float;
function Finite_distributed_random(proba: Proba_array) return Thing;
