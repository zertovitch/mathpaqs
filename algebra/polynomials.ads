------------------------------------------------------------------------------
--  File:            polynomials.ads
--  Description:     Polynomials package
--  Date/version:    1.5.1997
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------
generic
          type Field_elt is private;
          zero, one : Field_elt;                       --  0 and 1 elements

          with function "-" (a : Field_elt) return Field_elt;   --  unary oper.

          with function "+" (a, b : Field_elt) return Field_elt;  -- binary oper.
          with function "-" (a, b : Field_elt) return Field_elt;
          with function "*" (a, b : Field_elt) return Field_elt;
          with function "/" (a, b : Field_elt) return Field_elt;
          with function "=" (a, b : Field_elt) return Boolean;

package Polynomials is

  type Polynomial is array (Natural range <>) of Field_elt;

----- Informations, conversions, filling

  function Deg (p : Polynomial) return Integer;
  function Dom (p : Polynomial) return Field_elt;  -- coeff. of highest order

  procedure Fill (quoi : out Polynomial; avec : Polynomial);
    -- fill a Polynomial of greater array dimension with a smaller

----- Unary operators

  function "+" (p : Polynomial) return Polynomial;                    -- +p
  function "-" (p : Polynomial) return Polynomial;                    -- -p

----- Binary operators

  function "+" (p1, p2 : Polynomial) return Polynomial;                -- p1+p2
  function "+" (p : Polynomial; const : Field_elt) return Polynomial;  -- p+c
  function "+" (const : Field_elt; p : Polynomial) return Polynomial;  -- c+p

  function "-" (p1, p2 : Polynomial) return Polynomial;                -- p1-p2
  function "-" (p : Polynomial; const : Field_elt) return Polynomial;  -- p-c
  function "-" (const : Field_elt; p : Polynomial) return Polynomial;  -- c-p

  function "*" (p1, p2 : Polynomial) return Polynomial;                -- p1*p2
  function "*" (factor : Field_elt; p : Polynomial) return Polynomial; -- c*p
  function "*" (p : Polynomial; factor : Field_elt) return Polynomial; -- p*c

  function "/" (p1, p2 : Polynomial) return Polynomial;                -- p1/p2
  function "/" (p : Polynomial; factor : Field_elt) return Polynomial; -- p/c
  function "Rem" (p1, p2 : Polynomial) return Polynomial;         -- p1 rem p2
  function "Mod" (p1, p2 : Polynomial) return Polynomial;         -- p1 mod p2
  procedure Div_Rem (a, b : Polynomial; q, r : out Polynomial);     -- div & rem

  function Eq (p1, p2 : Polynomial) return Boolean;                     -- p1=p2
  function Neq (p1, p2 : Polynomial) return Boolean;                    -- p1/=p2

------ Evaluation

  function Eval (p : Polynomial; x : Field_elt) return Field_elt;        -- p(x)

----- Derivation, Primitive

  function Derivate (p : Polynomial) return Polynomial;                -- p'
  procedure Derivate (p : Polynomial; dp : out Polynomial);

  function Primitive (p : Polynomial) return Polynomial;               -- Prim.
  procedure Primitive (p : Polynomial; pp : out Polynomial);

  Array_too_small : exception;

end Polynomials;
