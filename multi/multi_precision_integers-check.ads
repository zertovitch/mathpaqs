package Multi_precision_integers.Check is

  -- check integrity
  procedure Test( m: multi_int; test_last: Boolean:= True );

  -- i3 must be = i1 * i2
  procedure Check_Multiplication(i1,i2,i3: in multi_int);

  -- i1 must be = i2 * q + r
  procedure Check_Div_Rem(i1,i2,q,r: in multi_int);

end Multi_precision_integers.Check;
