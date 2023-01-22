--------------------------------------------------------------------------------
--  File: euclidean_ring_tools.adb; see specification (euclidean_ring_tools.ads)
--------------------------------------------------------------------------------

package body Euclidean_Ring_Tools is

  procedure GCD (a, b : Ring_Element; the_gcd : out Ring_Element) is
    dummy_s, dummy_t : Ring_Element;
  begin
    GCD_and_Bezout (a, b, dummy_s, dummy_t, the_gcd);
  end GCD;

  procedure Bezout (a, b : in Ring_Element; s, t : out Ring_Element) is
    dummy : Ring_Element;
  begin
    GCD_and_Bezout (a, b, s, t, dummy);
  end Bezout;

  procedure Fill (destination : out Ring_Element; source : Ring_Element) is
  pragma Inline (Fill);
  begin
    destination := source;
  end Fill;

  procedure GCD_and_Bezout (a, b : in Ring_Element; s, t, the_gcd : out Ring_Element) is
    --  Program 1.8, Introduction to number theory, RBJT Allenby & EJ Redfern
    ta, tb : array (1 .. 3) of Ring_Element;
    q, r : Ring_Element;
  begin
    Fill (ta (1), one);         Fill (tb (1), zero);
    Fill (ta (2), zero);        Fill (tb (2), one);
    Fill (ta (3), a);           Fill (tb (3), b);

    while not Eq (tb (3), zero) loop
      Fill (q, ta (3) / tb (3));
      for i in 1 .. 3 loop
        Fill (r, ta (i) - q * tb (i));
        Fill (ta (i), tb (i));
        Fill (tb (i), r);
      end loop;
    end loop;

    Fill (s,       ta (1));
    Fill (t,       ta (2));
    Fill (the_gcd, ta (3));

  end GCD_and_Bezout;

  procedure Chinese_Remainder_Theorem (
    a, n      : in     Ring_Element_Array;
    x, prod :    out Ring_Element
  )
  is
    sk, tk, all_but, the_gcd : Ring_Element;
  begin
    if a'First /= n'First then
      raise Constraint_Error with "First indices don't match";
    end if;
    if a'Last /= n'Last then
      raise Constraint_Error with "Last indices don't match";
    end if;
    Fill (prod, one);
    for nn of n loop
      Fill (prod, prod * nn);
    end loop;
    --
    --  Gauss algorithm:
    --
    Fill (x, zero);
    for k in n'Range loop
      Fill (all_but, prod / n (k));
      GCD_and_Bezout (all_but, n (k), sk, tk, the_gcd);
      if not Eq (the_gcd, one) then
        raise Not_Coprime with "Not coprime, Chinese remainder theorem cannot be used.";
      end if;
      Fill (x, x + a (k) * sk * all_but);
    end loop;
  end Chinese_Remainder_Theorem;

end Euclidean_Ring_Tools;
