-----------------------------------------------------------------------------
--  File: polynomials.adb; see specification (polynomials.ads)
-----------------------------------------------------------------------------

package body Polynomials is

  deg_0 : constant Integer := Integer'First;  --  convention: deg (0)= -infinite

----- Informations, conversions, filling

  function Deg (p : Polynomial) return Integer is
  begin
    for k in reverse p'Range loop
      if p (k) /= zero then
        return k;
      end if;
    end loop;
    return deg_0;
  end Deg;

  function Dom (p : Polynomial) return Field_elt is
  begin
    return p (Integer'Max (0, Deg (p)));
  end Dom;

  procedure Fill (quoi : out Polynomial; avec : Polynomial) is
    the_last : constant Natural := Integer'Max (0, Deg (avec));
  begin
    if the_last > quoi'Last then
      raise Array_too_small;
    end if;
    quoi (0 .. the_last) := avec (0 .. the_last);
    quoi (the_last + 1 .. quoi'Last) := (others => zero);
  end Fill;

----- Unary operators

  function "+" (p : Polynomial) return Polynomial is
  begin
    return p;
  end "+";

  function "-" (p : Polynomial) return Polynomial is
    mp : Polynomial (p'Range);
  begin
    for i in p'Range loop mp (i) := -p (i); end loop;
    return mp;
  end "-";

----- Binary operators

  function "+" (p1, p2 : Polynomial) return Polynomial is
    d1 : constant Integer := Deg (p1);
    d2 : constant Integer := Deg (p2);
    p1_plus_p2 : Polynomial (0 .. Integer'Max (d1, d2));
    common : constant Integer := Integer'Min (d1, d2);
    --  ^ could be -infinite.
  begin
    for i in 0 .. common loop
      p1_plus_p2 (i) := p1 (i) + p2 (i);
    end loop;
    if common >= 0 then
      if d1 > common then
        for i in common + 1 .. d1 loop
          p1_plus_p2 (i) := p1 (i);
        end loop;
      elsif d2 > common then
        for i in common + 1 .. d2 loop
          p1_plus_p2 (i) := p2 (i);
        end loop;
      end if;
    end if;
    return p1_plus_p2;
  end "+";

  function "+" (p : Polynomial; const : Field_elt) return Polynomial is
    p_plus_const :  Polynomial (p'Range);
  begin
    p_plus_const (0) := p (0) + const;
    p_plus_const (1 .. p'Last) := p (1 .. p'Last);
    return p_plus_const;
  end "+";

  function "+" (const : Field_elt; p : Polynomial) return Polynomial is
  begin
    return p + const;
  end "+";

  function "-" (p1, p2 : Polynomial) return Polynomial is
  begin
    return p1 + (-p2);
  end "-";

  function "-" (p : Polynomial; const : Field_elt) return Polynomial is
  begin
    return p + (-const);
  end "-";

  function "-" (const : Field_elt; p : Polynomial) return Polynomial is
  begin
    return -p + const;
  end "-";

  function "*" (p1, p2 : Polynomial) return Polynomial is
    p1_fois_p2 : Polynomial (0 .. Deg (p1) + Deg (p2)) := (others => zero);
  begin
    for i in 0 .. Deg (p1) loop
      for j in 0 .. Deg (p2) loop
        p1_fois_p2 (i + j) := p1_fois_p2 (i + j) + p1 (i) * p2 (j);
      end loop;
    end loop;
    return p1_fois_p2;
  end "*";

  function "*" (factor : Field_elt; p : Polynomial) return Polynomial is
    p_fois_fact : Polynomial (p'Range);
  begin
    for i in 0 .. Deg (p) loop p_fois_fact (i) := p (i) * factor; end loop;
    return p_fois_fact;
  end "*";

  function "*" (p : Polynomial; factor : Field_elt) return Polynomial is
  begin
    return factor * p;
  end "*";

  function "/" (p1, p2 : Polynomial) return Polynomial is
    q : Polynomial (p1'Range);
    r : Polynomial (0 .. Integer'Max (Deg (p2), Deg (p1)));
  begin
    Div_Rem (p1, p2, q, r);
    return q;
  end "/";

  function "/" (p : Polynomial; factor : Field_elt) return Polynomial is
  begin
    return (one / factor) * p;
  end "/";

  procedure Div_Rem (a, b : Polynomial; q, r : out Polynomial) is
    n : constant Integer := Deg (b); d : Integer; qdn : Field_elt;
  begin
    q := (others => zero);
    Fill (r, a);
    loop
      d := Deg (r);
      exit when d < n;
      qdn := r (d) / b (n);
      q (d - n) := qdn;
      r (d) := zero;
      for l in 0 .. n - 1 loop
        r (l + d - n) := r (l + d - n) - b (l) * qdn;
      end loop;
    end loop;
  end Div_Rem;

  function "Rem" (p1, p2 : Polynomial) return Polynomial is
    q : Polynomial (p1'Range);
    r : Polynomial (p2'Range);
  begin
    Div_Rem (p1, p2, q, r);
    return r;
  end "Rem";

  function "Mod" (p1, p2 : Polynomial) return Polynomial is
    q : Polynomial (p1'Range);
    r : Polynomial (p2'Range);
  begin
    Div_Rem (p1, p2, q, r);
    return r;                    -- CORRIGER !
  end "Mod";

  function Eq (p1, p2 : Polynomial) return Boolean is
  begin
    return (Deg (p1) = Deg (p2)) and then (p1 (0 .. Deg (p1)) = p2 (0 .. Deg (p2)));
  end Eq;

  function Neq (p1, p2 : Polynomial) return Boolean is
  begin return not Eq (p1, p2); end Neq;

------ Evaluation

  function Eval (p : Polynomial; x : Field_elt) return Field_elt is
    u : Field_elt; n : constant Integer := Deg (p);
  begin
    if n <= 0 then
      return p (0);
    else -- Horner
      u := p (n);
      for j in reverse 0 .. n - 1 loop
        u := p (j) + x * u;
      end loop;
      return u;
    end if;
  end Eval;

----- Derivation, Primitive

  function Derivate (p : Polynomial) return Polynomial is
    dp : Polynomial (0 .. Integer'Max (0, Deg (p) - 1));
  begin
    Derivate (p, dp); return dp;
  end Derivate;

  procedure Derivate (p : Polynomial; dp : out Polynomial) is
    ck : Field_elt := one;
  begin
    dp (Deg (p) .. dp'Last) := (others => zero);
    for k in 1 .. Deg (p) loop
      dp (k - 1) := p (k) * ck;
      ck := ck + one;
    end loop;
  end Derivate;

  function Primitive (p : Polynomial) return Polynomial is
    pp : Polynomial (0 .. Deg (p) + 1);
  begin
    Primitive (p, pp);
    return pp;
  end Primitive;

  procedure Primitive (p : Polynomial; pp : out Polynomial) is
    ck : Field_elt := one;
  begin
    pp (Deg (p) + 2 .. pp'Last) := (others => zero);
    pp (0) := zero;    -- this is ONE possible primitive
    for k in 0 .. Deg (p) loop
      pp (k + 1) := p (k) / ck;
      ck := ck + one;
    end loop;
  end Primitive;

end Polynomials;
