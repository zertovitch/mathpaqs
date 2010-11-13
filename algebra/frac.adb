-----------------------------------------------------------------------------
--  File: frac.adb; see specification (frac.ads)
-----------------------------------------------------------------------------

package body Frac is

  function "+" (f: frac_elt) return frac_elt is begin return f; end;

  function "-" (f: frac_elt) return frac_elt is
    begin  return ( - f.a, f.b );  end;

  function "+" (f1,f2: frac_elt) return frac_elt is
    begin  return (f1.a * f2.b + f2.a * f1.b, f1.b * f2.b);  end;

  function "+" (a: ring_elt; f: frac_elt) return frac_elt is
    begin  return (a*f.b + f.a, f.b);  end;

  function "+" (f: frac_elt; a: ring_elt) return frac_elt is
    begin  return (a*f.b + f.a, f.b);  end;

  function "-" (f1,f2: frac_elt) return frac_elt is
    begin  return f1+ (-f2);  end;

  function "-" (a: ring_elt; f: frac_elt) return frac_elt is
    begin  return (- f.a + a*f.b, f.b);  end;

  function "-" (f: frac_elt; a: ring_elt) return frac_elt is
    begin  return (-a*f.b + f.a, f.b);  end;

  function "*" (f1,f2: frac_elt) return frac_elt is
    begin  return (f1.a * f2.a, f1.b * f2.b);  end;

  function "*" (a: ring_elt; f: frac_elt) return frac_elt is
    begin  return (a*f.a,f.b);  end;

  function "*" (f: frac_elt; a: ring_elt) return frac_elt is
    begin  return (a*f.a,f.b);  end;

  function "/" (f1,f2: frac_elt) return frac_elt is
    begin
      if f2.a = zero then raise Division_by_null_fraction; end if;
      return (f1.a * f2.b, f1.b * f2.a);
    end;

  function "/" (a: ring_elt; f: frac_elt) return frac_elt is
    begin
      if f.a = zero then raise Division_by_null_fraction; end if;
      return (a * f.b, f.a);
    end;

  function "/" (f: frac_elt; a: ring_elt) return frac_elt is
    begin
      if a = zero then raise Zero_denominator; end if;
      return (f.a, f.b * a);
    end;

  function "/" (a,b:ring_elt) return frac_elt is
    begin
      if b = zero then raise Zero_denominator; end if;
      return (a,b); 
    end;

  function Eq(f1,f2: frac_elt) return boolean is
    begin  return (f1.a * f2.b = f1.b * f2.a);  end;

end Frac;
