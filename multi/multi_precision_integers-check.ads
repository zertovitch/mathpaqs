package Multi_Precision_Integers.Check is

  --  Check integrity
  procedure Test (m : Multi_Int; test_last : Boolean := True);

  --  i3 must be = i1 * i2
  procedure Check_Multiplication (i1, i2, i3 : in Multi_Int);

  --  i1 must be = i2 * q + r
  procedure Check_Div_Rem (i1, i2, q, r : in Multi_Int);

end Multi_Precision_Integers.Check;
