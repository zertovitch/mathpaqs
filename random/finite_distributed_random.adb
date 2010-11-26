function Finite_distributed_random(proba: Proba_array) return Thing is
  -- subtype AF01 is A_float range 0.0 .. 1.0;
  U: constant A_float:= Uniform_random;
  p: A_float:= 0.0;
  Probabilities_dont_sum_up_to_1: exception;
begin
  for th in Thing loop
    p:= p + A_float(proba(th));
    if U < p then
      return th;
    end if;
  end loop;
  if abs(p-1.0) > A_float'epsilon then
    raise Probabilities_dont_sum_up_to_1;
  end if;
  return Thing'last;
end Finite_distributed_random;
