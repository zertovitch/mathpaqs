--  The Computer Language Shootout
--  http://shootout.alioth.debian.org
--  Calculate digits of pi using the
--  Unbounded Spigot Algorithms
--
--  From Pascal code by Vincent Snijders
--  Translated by (New) P2Ada v. 17-June-2006

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Command_Line;                  use Ada.Command_Line;

with Multi_Precision_Integers;          use Multi_Precision_Integers;

pragma Warnings (".I");

procedure pi_digits is

  procedure Print_Pi_Digits (NumDigits: Integer) is

    subtype mpz_t is Multi_Int (3270);

    q,r,s,t: mpz_t; --  Transformation matrix components.

    i, k, digit, c: Integer;
    text_line: String(1 ..10);

    tmp1, tmp2: mpz_t;

    function Extract(x: Integer) return Integer is
    begin
      Multiply( q, x, tmp1);
      Multiply( s, x, tmp2);
      Add(tmp1, r, tmp1);
      Add(tmp2, t, tmp2);
      Divide(tmp1, tmp2, tmp1);
      return Basic( tmp1 );
      -- ^ equivalent to: return Basic( (q * x + r) / (s * x + t) );
    end Extract;

    function Is_safe return Boolean is
    begin
      return digit = Extract(4);
    end Is_safe;

    procedure Produce is
    begin
      Multiply( r, 10, tmp1);
      Multiply( t, digit*(-10), tmp2);
      Add( tmp1, tmp2, r);
      -- ^ equivalent to: Add( r * 10, t * digit * (-10), r);
      Multiply(q, 10, q);
    end Produce;

    procedure Consume is
      k1, k2: Integer;
    begin
      k:= k + 1;
      k1:= 2*k+1;
      k2:= 4*k+2;
      Multiply(r, k1, tmp1);
      Multiply(q, k2, tmp2);
      Add(tmp1, tmp2, r);
      Multiply(t, k1, tmp1);
      Multiply(s, k2, tmp2);
      Add(tmp1, tmp2, t);
      -- ^ equivalent to:
      --  Add(r * k1 , q * k2, r);
      --  Add(t * k1 , s * k2, t);
      Multiply(s, k, s);
      Multiply(q, k, q);
    end Consume;

  begin
    k:= 0;
    i:= 0;
    c:= 0;
    Fill(q, 1);
    Fill(r, 0);
    Fill(s, 0);
    Fill(t, 1);
    while i < NumDigits loop
      digit:= Extract(3);
      while not Is_safe loop
        Consume;
        digit:= Extract(3);
      end loop;
      Produce;
      c:= c + 1;
      text_line(c) := Character'Val(Character'Pos('0')+digit);
      i:= i + 1;
      if c = 10 then
        Put(text_line & ASCII.HT & ':');
        Put(i,0);
        New_Line;
        c:= 0;
      end if;
    end loop;
    if c /= 0 then
      Put(text_line(1..c));
      for i in c+1..10 loop
        Put(' ');
      end loop;
      Put(ASCII.HT & ':');
      Put(i,0);
      New_Line;
    end if;
  end Print_Pi_Digits;

  n : Integer := 2_500;

begin
  if Argument_Count = 1 then
    n := Integer'Value (Argument (1));
  end if;
  Print_Pi_Digits (n);
end pi_digits;
