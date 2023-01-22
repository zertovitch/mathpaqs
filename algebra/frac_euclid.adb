-----------------------------------------------------------------------------
--  File: frac_euclid.adb; see specification (frac_euclid.ads)
-----------------------------------------------------------------------------

with Euclidean_Ring_Tools;

package body Frac_Euclid is

  type Dummy_Array_Type is array (Integer range <>) of ring_elt;

  package ERT is
    new Euclidean_Ring_Tools (
    ring_elt, zero, one, "+", "-", "*", "/",
    "=",
    Dummy_Array_Type);
  use ERT;

  function Reduction (f : frac_elt) return frac_elt is
    p : ring_elt;
  begin
    if f.b = zero then raise Zero_Denominator; end if;
    if f.a = zero then return (zero, one); end if;
    GCD (f.a, f.b, p);
    return (f.a / p, f.b / p);
  end Reduction;

  function Auto_Red (f : frac_elt) return frac_elt is
  pragma Inline (Auto_Red);
  begin
    if auto_reduce then return Reduction (f); else return f; end if;
  end Auto_Red;

  function "+" (f : frac_elt) return frac_elt is begin return f; end "+";

  function "-" (f : frac_elt) return frac_elt is
  begin
    return (-f.a, f.b);
  end "-";

  function "+" (f1, f2 : frac_elt) return frac_elt is
    p, u, v : ring_elt;
  begin
    if (f1.b = zero) or (f2.b = zero) then raise Zero_Denominator; end if;
    if reduce_in_add then
      GCD (f1.b, f2.b, p);
      u := f1.b / p;
      v := f2.b / p;
      return Auto_Red ((f1.a * v + f2.a * u, p * u * v));
    else
      return Auto_Red ((f1.a * f2.b + f2.a * f1.b, f1.b * f2.b));
    end if;
  end "+";

  function "+" (a : ring_elt; f : frac_elt) return frac_elt is
  begin
    return Auto_Red ((a * f.b + f.a, f.b));
  end "+";

  function "+" (f : frac_elt; a : ring_elt) return frac_elt is
  begin
    return Auto_Red ((a * f.b + f.a, f.b));
  end "+";

  function "-" (f1, f2 : frac_elt) return frac_elt is
  begin
    return f1 + (-f2);
  end "-";

  function "-" (a : ring_elt; f : frac_elt) return frac_elt is
  begin
    return Auto_Red ((-f.a + a * f.b, f.b));
  end "-";

  function "-" (f : frac_elt; a : ring_elt) return frac_elt is
  begin
    return Auto_Red (((-a) * f.b + f.a, f.b));
  end "-";

  function "*" (f1, f2 : frac_elt) return frac_elt is
  begin
    return Auto_Red ((f1.a * f2.a, f1.b * f2.b));
  end "*";

  function "*" (a : ring_elt; f : frac_elt) return frac_elt is
  begin
    return Auto_Red ((a * f.a, f.b));
  end "*";

  function "*" (f : frac_elt; a : ring_elt) return frac_elt is
  begin
    return Auto_Red ((a * f.a, f.b));
  end "*";

  function "/" (f1, f2 : frac_elt) return frac_elt is
  begin
    if f2.a = zero then raise Division_by_Null_Fraction; end if;
    return Auto_Red ((f1.a * f2.b, f1.b * f2.a));
  end "/";

  function "/" (a : ring_elt; f : frac_elt) return frac_elt is
  begin
    if f.a = zero then raise Division_by_Null_Fraction; end if;
    return Auto_Red ((a * f.b, f.a));
  end "/";

  function "/" (f : frac_elt; a : ring_elt) return frac_elt is
  begin
    if a = zero then raise Zero_Denominator; end if;
    return Auto_Red ((f.a, f.b * a));
  end "/";

  function "/" (a, b : ring_elt) return frac_elt is
  begin
    if b = zero then raise Zero_Denominator; end if;
    return Auto_Red ((a, b));
  end "/";

  function Eq_Frac (f1, f2 : frac_elt) return Boolean is
  begin
    return f1.a * f2.b = f1.b * f2.a;
  end Eq_Frac;

end Frac_Euclid;
