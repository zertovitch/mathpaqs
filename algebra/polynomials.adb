-----------------------------------------------------------------------------
--  File: polynomi.adb; see specification (polynomi.ads)
-----------------------------------------------------------------------------

with Add_Inc;

package body Polynomials is

  package Add_Inc_I is new Add_Inc( Integer, 1, "+","-" );     use Add_Inc_I;
  package Add_Inc_c is new Add_Inc( Field_elt, one, "+","-" ); use Add_Inc_c;

  deg_0: constant Integer:= Integer'First;  -- convention: deg(0)= -infinite

----- Informations, conversions, filling

  function Deg(p:polynomial) return Integer is
  begin
    for k in reverse p'Range loop 
      if p(k)/=zero then return k; end if;
    end loop;
    return deg_0;
  end Deg;
    
  function Dom(p:polynomial) return field_elt is 
  begin
    return p(Integer'Max(0,Deg(p))); 
  end;

  procedure Fill(quoi:out polynomial; avec:polynomial) is
    the_last: Natural:= Integer'Max(0,Deg(avec));
  begin
    if the_last>quoi'Last then raise Array_too_small; end if;
    quoi(0..the_last):= avec(0..the_last);
    quoi(the_last+1..quoi'Last):= (others=> zero);
  end Fill;

----- Unary operators

  function "+" (p: polynomial) return polynomial is
  begin return p; end;
    
  function "-" (p: polynomial) return polynomial is
    mp: polynomial(p'Range);
  begin
    for i in p'Range loop mp(i):= -p(i); end loop;
    return mp;
  end "-";

----- Binary operators

  function "+" (p1,p2: polynomial) return polynomial is
    p1_plus_p2: polynomial( 0..Integer'Max(deg(p1),deg(p2)) );
  begin
    for i in p1_plus_p2'Range loop 
      p1_plus_p2(i):= p1(i) + p2(i); 
    end loop;
    return p1_plus_p2;
  end "+";

  function "+" (p: polynomial; const: field_elt) return polynomial is
    p_plus_const: polynomial( p'Range );
  begin
    p_plus_const(0):= p(0) + const;
    p_plus_const(1..p'Last):= p(1..p'Last);
    return p_plus_const;
  end "+";
    
  function "+" (const: field_elt; p: polynomial) return polynomial is
  begin
    return p + const;
  end "+";

  function "-" (p1,p2: polynomial) return polynomial is
    p1_moins_p2: polynomial( 0..Integer'Max(deg(p1),deg(p2)) );
  begin
    for i in p1_moins_p2'Range loop 
      p1_moins_p2(i):= p1(i) - p2(i); 
    end loop;
    return p1_moins_p2;
  end "-";

  function "-" (p: polynomial; const: field_elt) return polynomial is
  begin
    return p + (-const);
  end "-";

  function "-" (const: field_elt; p: polynomial) return polynomial is
  begin
    return -p + const;
  end "-";

  function "*" (p1,p2: polynomial) return polynomial is
    p1_fois_p2: polynomial( 0..deg(p1)+deg(p2) ) := (others=> zero);
  begin 
    for i in 0..deg(p1) loop
      for j in 0..deg(p2) loop
        Add( p1_fois_p2(i+j), p1(i) * p2(j) );
      end loop;
    end loop;
    return p1_fois_p2;
  end "*";

  function "*" (factor: field_elt; p: polynomial) return polynomial is
    p_fois_fact:polynomial(p'Range);
  begin 
    for i in 0..deg(p) loop p_fois_fact(i):= p(i) * factor; end loop;
    return p_fois_fact;
  end "*";

  function "*" (p: polynomial; factor: field_elt) return polynomial is
  begin 
    return factor * p;
  end "*";


  function "/" (p1,p2: polynomial) return polynomial is
    q: polynomial(p1'Range);
    r: polynomial(0..Integer'Max(Deg(p2),Deg(p1)));
  begin
    Div_Rem(p1,p2, q,r);
    return q;
  end "/";

  function "/" (p: polynomial; factor: field_elt) return polynomial is
  begin 
    return (one / factor) * p;
  end "/";

  procedure Div_Rem (a,b: polynomial; q,r:out polynomial) is
    n: Integer:= Deg(b); d: Integer; qdn: Field_elt;
  begin
    q:= (others => zero);
    Fill(r,a);
    loop
      d:= Deg(r);
      exit when d<n;
      qdn:= r(d)/b(n);
      q(d-n):= qdn;
      r(d):= zero;
      for l in 0..n-1 loop Sub( r(l+d-n), b(l)*qdn ); end loop;
    end loop;
  end Div_Rem;

  function "Rem" (p1,p2: polynomial) return polynomial is
    q: polynomial(p1'Range);
    r: polynomial(p2'Range);
  begin
    Div_Rem(p1,p2, q,r);
    return r;
  end "Rem";

  function "Mod" (p1,p2: polynomial) return polynomial is
    q: polynomial(p1'Range);
    r: polynomial(p2'Range);
  begin
    Div_Rem(p1,p2, q,r);
    return r;                    -- CORRIGER !
  end "Mod";

  function Eq(p1,p2: polynomial) return Boolean is
  begin 
    return ( deg(p1)=deg(p2) ) and then ( p1(0..deg(p1)) = p2(0..deg(p2)) );
  end Eq;

  function Neq(p1,p2: polynomial) return Boolean is
  begin return not Eq(p1,p2); end;

------ Evaluation

  function Eval(p:polynomial; x:field_elt) return field_elt is
    u: field_elt; n:constant Integer:= deg(p);
  begin
    if n<=0 then 
      return p(0); 
    else -- Horner
      u:= p(n);
      for j in reverse 0..n-1 loop
        u:= p(j) + x*u;
      end loop;
      return u;
    end if;            
  end Eval;

----- Derivation, Primitive

  function Derivate(p: polynomial) return polynomial is
    dp: polynomial( 0..Integer'Max(0,deg(p)-1) );
  begin
    Derivate(p,dp); return dp;
  end;
    
  procedure Derivate(p: polynomial; dp: out polynomial) is
    ck: field_elt:= one;
  begin
    dp( deg(p)..dp'Last ):= (others=> zero);
    for k in 1..deg(p) loop
      dp(k-1):= p(k) * ck;
      Inc(ck);
    end loop;
  end Derivate;

  function Primitive(p:polynomial) return polynomial is
    pp: polynomial( 0..deg(p)+1 );
  begin
    Primitive(p,pp); return pp;
  end;

  procedure Primitive(p:polynomial; pp: out polynomial) is
    ck: field_elt:= one;
  begin
    pp( deg(p)+2..pp'Last ):= (others=> zero);
    pp(0):= zero;    -- this is ONE possible primitive
    for k in 0..deg(p) loop
      pp(k+1):= p(k) / ck;
      Inc(ck);
    end loop;
  end Primitive;

end Polynomials;
