-----------------------------------------------------------------------------
--  File: eurintoo.adb; see specification (eurintoo.ads)
-----------------------------------------------------------------------------

package body Euclidean_Ring_Tools is

  function GCD(a,b:ring_elt) return ring_elt is
    dummy, the_gcd: ring_elt;
    begin
      GCD_and_Bezout(a,b, dummy, dummy, the_gcd);
      return the_gcd;
    end GCD;

  procedure Bezout(a,b:in ring_elt; s,t:out ring_elt) is
    dummy: ring_elt;
    begin
      GCD_and_Bezout(a,b, s,t, dummy);
    end Bezout;

  procedure GCD_and_Bezout(a,b:in ring_elt; s,t,the_gcd:out ring_elt) is
    -- Program 1.8, Introduction to number theory, RBJT Allenby & EJ Redfern
    ta,tb: array(1..3) of ring_elt;
    q,r: ring_elt;
    begin
      ta(1):= one;         tb(1):= zero;
      ta(2):= zero;        tb(2):= one;
      ta(3):= a;           tb(3):= b;

      while tb(3)/= zero loop
        q:= ta(3)/tb(3);
        for i in 1..3 loop
          r:= ta(i) - q * tb(i);
          ta(i):= tb(i);
          tb(i):= r;
        end loop;
      end loop;

      s:=       ta(1);
      t:=       ta(2);
      the_gcd:= ta(3);

    end GCD_and_Bezout;

end Euclidean_Ring_Tools;
