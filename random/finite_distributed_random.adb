function Finite_Distributed_Random
  (proba          : Proba_Array;  --  Contains the probability for each element
   uniform_random : A_Float)      --  Uniformily distributed float in [0,1]
return Thing
is
  p : A_Float := 0.0;
  Probabilities_dont_sum_up_to_1 : exception;
begin
  for th in Thing loop
    p := p + proba (th);
    if uniform_random < p then
      return th;
    end if;
  end loop;
  if abs (p - 1.0) > A_Float'Model_Epsilon then
    raise Probabilities_dont_sum_up_to_1;
  end if;
  return Thing'Last;
end Finite_Distributed_Random;
