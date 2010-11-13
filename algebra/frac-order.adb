-----------------------------------------------------------------------------
--  File: fracorde.adb; see specification (fracorde.ads)
-----------------------------------------------------------------------------

package body Frac.Order is

  function "ABS" (f: frac_elt) return frac_elt is
    begin  if f >= (zero,one) then return f; else return -f; end if; end;
    
  function "<" (f1,f2: frac_elt) return boolean is
    begin  return (f1.a * f2.b < f1.b * f2.a);  end;

  function ">" (f1,f2: frac_elt) return boolean is
    begin  return f2<f1;  end;

  function ">=" (f1,f2: frac_elt) return boolean is
    begin  return not (f1<f2); end;
    
  function "<=" (f1,f2: frac_elt) return boolean is
    begin  return not (f2<f1); end;

end Frac.Order;
