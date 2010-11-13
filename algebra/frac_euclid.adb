-----------------------------------------------------------------------------
--  File: fraceucl.adb; see specification (fraceucl.ads)
-----------------------------------------------------------------------------

with Euclidean_Ring_Tools;

package body Frac_Euclid is

  package ERT is new Euclidean_Ring_Tools(ring_elt, zero,one, "-","*","/");
  use ERT;

  function Reduction(f: frac_elt) return frac_elt is
    p: ring_elt;
  begin
    if f.b = zero then raise Zero_denominator; end if;
    if f.a = zero then return (zero,one); end if;
    p:= GCD(f.a,f.b);
    return (f.a/p, f.b/p); 
  end;
  
  function Auto_Red(f: frac_elt) return frac_elt is
  begin
    if auto_reduce then return Reduction(f); else return f; end if;
  end;

  pragma Inline(Reduction, Auto_Red);

  function "+" (f: frac_elt) return frac_elt is begin return f; end;

  function "-" (f: frac_elt) return frac_elt is
  begin  return ( - f.a, f.b );  end;

  function "+" (f1,f2: frac_elt) return frac_elt is
    p,u,v: ring_elt;
  begin
    if (f1.b = zero) or (f2.b = zero) then raise Zero_denominator; end if;
    if reduce_in_add then
      p:= GCD(f1.b,f2.b);
      u:= f1.b/p;
      v:= f2.b/p;
      return Auto_Red((f1.a * v + f2.a * u, p*u*v));
    else
      return Auto_Red((f1.a * f2.b + f2.a * f1.b, f1.b * f2.b));  
    end if;
  end "+";

  function "+" (a: ring_elt; f: frac_elt) return frac_elt is
  begin  return Auto_Red((a*f.b + f.a, f.b));  end;

  function "+" (f: frac_elt; a: ring_elt) return frac_elt is
  begin  return Auto_Red((a*f.b + f.a, f.b));  end;

  function "-" (f1,f2: frac_elt) return frac_elt is
  begin  return f1+ (-f2);  end;

  function "-" (a: ring_elt; f: frac_elt) return frac_elt is
  begin  return Auto_Red((- f.a + a*f.b, f.b));  end;

  function "-" (f: frac_elt; a: ring_elt) return frac_elt is
  begin  return Auto_Red((-a*f.b + f.a, f.b));  end;

  function "*" (f1,f2: frac_elt) return frac_elt is
  begin  return Auto_Red((f1.a * f2.a, f1.b * f2.b));  end;

  function "*" (a: ring_elt; f: frac_elt) return frac_elt is
  begin  return Auto_Red((a*f.a,f.b));  end;

  function "*" (f: frac_elt; a: ring_elt) return frac_elt is
  begin  return Auto_Red((a*f.a,f.b));  end;

  function "/" (f1,f2: frac_elt) return frac_elt is
  begin
    if f2.a = zero then raise Division_by_null_fraction; end if;
    return Auto_Red((f1.a * f2.b, f1.b * f2.a));
  end "/";

  function "/" (a: ring_elt; f: frac_elt) return frac_elt is
  begin
    if f.a = zero then raise Division_by_null_fraction; end if;
    return Auto_Red((a * f.b, f.a));
  end "/";

  function "/" (f: frac_elt; a: ring_elt) return frac_elt is
  begin
    if a = zero then raise Zero_denominator; end if;
    return Auto_Red((f.a, f.b * a));
  end "/";

  function "/" (a,b:ring_elt) return frac_elt is
  begin
    if b = zero then raise Zero_denominator; end if;
    return Auto_Red((a,b)); 
  end "/";

  function Eq(f1,f2: frac_elt) return Boolean is
  begin  return (f1.a * f2.b = f1.b * f2.a);  end;

end Frac_Euclid;
