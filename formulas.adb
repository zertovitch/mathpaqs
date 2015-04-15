-- Translated on 9-Apr-2015 by (New) P2Ada v. 28-Oct-2009

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
-- This is for Pi :
-- with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

package body Formulas is

  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  package RIO is new Ada.Text_IO.Float_IO(Real);

  type S_Form_Set is array(S_Form) of Boolean;
  par_or_terminal: constant S_Form_Set:= (par|croch|accol|nb|vr => True, others => False);

  function conv_strg(s: S_Form) return String is
  begin
    case s is
      when plus_una   => return "+";
      when moins_una  => return "-";
      when expn    => return "Exp";
      when logn    => return "Log";
      when sinus   => return "Sin";
      when cosinus => return "Cos";
      when tg      => return "Tan";
      when arctg   => return "Arctan";
      when sh      => return "Sinh";
      when ch      => return "Cosh";
      when th      => return "Tanh";
      when par     => return "(";
      when croch   => return "[";
      when accol   => return "{";
      when fois    => return "*";
      when plus    => return "+";
      when moins   => return "-";
      when sur     => return "/";
      when puiss   => return "^";
      when nb => return "";
      when vr => return "";
    end case;
  end;

  conv_symb_una : constant array (Character) of s_form :=
    ( '+' => plus_una,
      '-' => moins_una,
      others => nb);

  conv_symb : constant array (Character) of s_form :=
    ( '+' => plus,
      '-' => moins,
      '*' => fois,
      '/' => sur,
      '^' => puiss,
      '(' => par,
      '[' => croch,
      '{' => accol,
      others => nb);

  function conv_mstr(s: S_Form) return String is
  begin
    return To_Upper(conv_strg(s));
  end;

  procedure Put (t: in  Ada.Text_IO.File_Type; f: Formula) is
    x : Real;
    use Ada.Text_IO, RIO;
  begin
    if f/= null then
      case f.s is
        when nb =>
          x:= f.n;
          if  x = Real'Floor(x) then
            Put(t, Integer(x), 0);
          else
            Put(t,x,0,5,0);
          end if;
        when vr =>
          Put(t, To_String(f.v));
        when moins_una=>
          Put(t,'-');
          Put(t,f.left);
        when plus_una=>
          Put(t,'+');
          Put(t,f.left);
        when Binary =>
          Put(t,f.left);
          Put(t,conv_strg(f.s));
          Put(t,f.right);
        when logn..th=>
          Put(t,conv_strg(f.s));
          Put(t,'(');
          Put(t,f.left);
          Put(t,')');
        when par=>
          Put(t,'(');
          Put(t,f.left);
          Put(t,')');
        when croch=>
          Put(t,'[');
          Put(t,f.left);
          Put(t,']');
        when accol=>
          Put(t,'{');
          Put(t,f.left);
          Put(t,'}');
        when others=>
          null;  -- [P2Ada]: no otherwise / else in Pascal
      end case;

    end if;
  end Put;

  procedure Deep_delete (f: in out Formula) is
  begin
    if f /= null then
      case f.s is
        when Unary =>
          Deep_delete(f.left);
        when Binary =>
          Deep_delete(f.left);
          Deep_delete(f.right);
        when others=>
          null;
      end case;
      Dispose(f);
      f:= null;
    end if;
  end Deep_delete;

  type Character_Set is array(Character) of Boolean;

  function Almost_zero(x: Real) return Boolean is
  begin
    return abs x <= Real'Base'Model_Small;
  end Almost_zero;

  Closing: constant array(Character) of Character:=
    ('(' => ')',
     '[' => ']',
     '{' => '}',
     others => 'X');

  c_fin: constant Character := Character'Val (0);

  procedure Check_brackets(open, close: Character) is
  begin
    if Closing(open) = close then
      return;
    end if;
    if close = c_fin then
      Raise_Exception(
        Parse_Error'Identity,
        "'" & Closing(open) & "' is missing");
    else
      Raise_Exception(
        Parse_Error'Identity,
        "'" & Closing(open) & "' expected, found '" & close & ''');
    end if;
  end Check_brackets;

  procedure Parse (str_base: String; f: out Formula) is

    function No_Spaces(s: String) return String is
      t: String(s'Range);
      j: Integer:= s'First - 1;
    begin
      for i in s'Range loop
        case s(i) is
          when ' ' | ASCII.HT | ASCII.CR | ASCII.LF =>
            null;
          when others =>
            j:= j + 1;
            t(j):= s(i);
        end case;
      end loop;
      return t(t'First .. j);
    end No_Spaces;

    str: constant String:= No_Spaces(str_base) & c_fin;

    chiffres : constant Character_Set := ('0' .. '9' | '.' => True, others => False);
    lettres  : constant Character_Set := ('a' .. 'z' | 'A' .. 'Z' | '_' | '$' => True, others => False);

    i : Integer;

    function Expression return Formula is
      function Term return Formula is
        function Factor return Formula is
          --
          function Number return Formula is
            n : Formula;
            j : Integer;
          begin
            n:= new Formula_Rec(nb);
            j:=i;
            loop
              i:= i + 1;
              exit when not chiffres(str(i));
            end loop;
            n.n:= Real'Value(str(j..i-1));
            return n;
          end Number;

          function Variable_or_function return Formula is
            n: Formula;
            j: Integer;
          begin
            j:= i;
            loop
              i:= i + 1;
              exit when not lettres(str(i));
            end loop;
            declare
              ch: constant String:= str(j..i-1);
              mch: constant String:= To_Upper(ch);
            begin
              if str(i)='(' then           --  --- (: fonctions std, usr
                for s in  expn .. fois loop
                  if mch = conv_mstr(s) then  -- Found a built-in function
                    n:= new Formula_Rec(s);
                    exit;
                  end if;
                end loop;
                i:= i + 1;
                -- !! fonctions usr TBD !!
                n.left:= Expression;
                Check_brackets('(', str(i));
                i:= i + 1;
              else
                n:= new Formula_Rec(vr);
                n.v:= To_Unbounded_String(ch);
              end if;
            end;
            return n;
          end Variable_or_function;

          --  Factor
          n, n1 : Formula;
          c     : Character;
        begin
          n:= null;
          c:= str(i);
          if chiffres(c) then
            n:= Number;
          elsif lettres(c) then
            n:= Variable_or_function;
          elsif c = '-' or c = '+' then
            n:= new Formula_Rec(conv_symb_una(c));
            i:= i + 1;
            n.left:= Factor;
          end if;
          c:= str(i);
          case c is
            when '^'=>
              n1:= n;
              n:= new Formula_Rec(puiss);
              i:= i + 1;
              n.left:= n1;
              n.right:= Factor;
            when '(' | '[' | '{'=>
              if n = null then
                n:= new Formula_Rec(conv_symb(c));
                i:= i + 1;
                n.left:= Expression;
                Check_brackets(c, str(i));
                i:= i + 1;
              end if;
            when others=>
              null;  -- [P2Ada]: no otherwise / else in Pascal
          end case;
          if n = null then
            Raise_Exception(Parse_Error'Identity, "Unexpected end in factor");
          end if;
          return n;
        end Factor;

        --  Term
        n, left : Formula;
        c : Character;
      begin
        left:= Factor;
        c:= str(i);
        if c = '*' or c = '/' then
          i:= i + 1;
          n:= new Formula_Rec(conv_symb(c));
          n.left:= left;
          n.right:= Term;
          return n;
        else
          return left;
        end if;
      end Term;

      --  Expression
      n, left : Formula;
      c : Character;
    begin
      left:= Term;
      c:= str(i);
      if c = '+' or c = '-' then
        i:= i + 1;
        n:= new Formula_Rec(conv_symb(c));
        n.left:= left;
        n.right:= Expression;
        return n;
      else
        return left;
      end if;
    end Expression;

  begin
    i:= 1;
    f:= Expression;
    if str(i) /= c_fin then
      Deep_delete(f);
      Raise_Exception(Parse_Error'Identity, "Unexpected end in expression");
    end if;
  exception
    when E: Parse_Error =>
      Deep_delete(f);
      Raise_Exception(Parse_Error'Identity, Exception_Message(E));
  end Parse;

  --  -------------------------------- Evaluate ---------------------------------

  function Evaluate (f: Formula; payload: Payload_type) return Real is
    aux: Real;
    use REF;
  begin
    if f = null then
      return 0.0;
    end if;
    case f.s is
      when nb=>
        return f.n;
      when vr=>
        return Evaluate_variable(To_String(f.v), payload);
      when moins_una =>
        return -Evaluate(f.left, payload);
      when plus_una |
           par |
           croch |
           accol =>
        return Evaluate(f.left, payload);
      when plus =>
        return Evaluate(f.left, payload) + Evaluate(f.right, payload);
      when moins =>
        return Evaluate(f.left, payload) - Evaluate(f.right, payload);
      when fois =>
        aux:= Evaluate(f.left, payload);
        if Almost_zero(aux) then
          return 0.0;
        elsif Almost_zero(aux - 1.0) then
          return Evaluate(f.right, payload);
        else
          return aux * Evaluate(f.right, payload);
        end if;
      when sur =>
        aux:= Evaluate(f.right, payload);
        if Almost_zero(aux) then
          raise div_by_0;
        elsif Almost_zero(aux - 1.0) then    --  X/1 -> X
          return Evaluate(f.left, payload);
        else
          return Evaluate(f.left, payload) / aux;
        end if;
      when puiss =>
        aux:= Evaluate(f.left, payload);
        if aux <= 0.0 then
          raise not_pos_power;
        end if;
        return Exp(Evaluate(f.right, payload)*Log(aux));
      when logn=>
        aux:= Evaluate(f.left, payload);
        if aux <= 0.0 then
          raise not_pos_power;
        end if;
        return Log(aux);
      when expn=>
        return Exp(Evaluate(f.left, payload));
      when sinus=>
        return Sin(Evaluate(f.left, payload));
      when cosinus=>
        return Cos(Evaluate(f.left, payload));
      when sh=>
        return Sinh(Evaluate(f.left, payload));
      when ch=>
        return Cosh(Evaluate(f.left, payload));
      when th=>
        return Tanh(Evaluate(f.left, payload));
      when arctg=>
        return ArcTan(Evaluate(f.left, payload));
      when tg=>
        return Tan(Evaluate(f.left, payload));
    end case;
  end Evaluate;

  ------------------------------- Compare -----------------------------------

  function Equivalent(fa, fb : Formula) return Boolean is
    ga, gb : s_form;
  begin
    if fa = null then
      return fb = null;
    end if;
    if fb = null then
      return False;
    end if;
    -- fa and fb are not null, at this point
    ga:= fa.s;
    gb:= fb.s;
    if ga in Neutral then
      return Equivalent(fa.left, fb); -- +A, (A), [A], {A}  -> A
    elsif gb in Neutral then
      return Equivalent(fa, fb.left); -- +B, (B), [B], {B}  -> B
    elsif ga /= gb then
      return False;
    else
      case ga is
        when nb =>
          return Almost_zero(fa.n - fb.n);
        when vr =>
          return fa.v = fb.v;  --  same names
        when Unary =>
          return Equivalent(fa.left, fb.left);
        when Binary =>
          return Equivalent(fa.left, fb.left) and Equivalent(fa.right, fb.right);
      end case;
    end if;
  end Equivalent;

  function Is_constant(a: Formula; cst : Real) return Boolean is
  begin
    return a /= null and then a.s = nb and then a.n = cst;
  end;

  function Is_constant_pair(a: Formula) return Boolean is
  begin
    return
      a.s in Binary and then
      a.left /= null and then
      a.right /= null and then
      a.left.s = nb and then
      a.right.s = nb;
  end;

  ------------------------------- Simplify ----------------------------------

  procedure Simplify (f : in out Formula) is
    aux,
    nexp : Formula;
    x    : Real;

    procedure left_replaces_f is
    begin
      aux:= f.left;
      f.left:= null; --  empˆeche destruction
      Deep_delete(f);
      f:= aux ;
    end left_replaces_f;

    procedure right_replaces_f is
    begin
      aux:= f.right;
      f.right:= null; --  empˆeche destruction
      Deep_delete(f);
      f:= aux ;
    end right_replaces_f;

    procedure cst_replaces_f (c: Real) is
    begin
      Deep_delete(f);
      f:= new Formula_Rec(nb);
      f.n:= c ;
    end cst_replaces_f;

    procedure Simplify_functions is
      use REF;
      --  Assumes: f.s in Built_in_function
    begin
      if f.left /= null then
        if f.left.s = nb then        --  Evaluate "f(cst)" into f(cst)
          x:= f.left.n;
          case f.s is
            when sinus=>
              cst_replaces_f(Sin(x));
            when cosinus=>
              cst_replaces_f(Cos(x));
            when expn=>
              cst_replaces_f(Exp(x));
            when logn=>
              if x > 0.0 then
                cst_replaces_f(Log(x));
              end if;
            when others=>
              null;  -- [P2Ada]: no otherwise / else in Pascal
          end case;
        end if;
      end if;
      if f.s = cosinus and then f.left /= null and then f.left.s = moins_una then
        aux:= f.left.left;    --  Cos(-X) -> Cos(X)
        Dispose(f.left);
        f.left:= aux;
      end if;
    end Simplify_functions;

    use REF;

  begin
    if f = null then
      return;
    end if;
    --  Simplify arguments
    case f.s is
      when Unary =>
        Simplify(f.left);
      when Binary =>
        Simplify(f.left);
        Simplify(f.right);
      when others=>
        null;
    end case;
    if f = null then
      return;
    end if;
    case f.s is
      when moins_una=>
        if f.left /= null and then f.left.s= moins_una then     --    - -X  ->  X
          aux:=f.left.left;
          Dispose(f.left);
          Dispose(f);
          f:= aux;
        end if;
      when plus_una =>
        left_replaces_f;                                   --  +X -> X
      when par | croch | accol=>
        if f.left /= null and then par_or_terminal(f.left.s) then
          left_replaces_f;     --  ((...)) ->(...), (c) -> c, (v) -> v
        end if;
      when plus =>
        if Equivalent(f.left, f.right) then             --  X+X -> 2*X
          aux:= new Formula_Rec(fois);
          aux.left:= new Formula_Rec(nb);
          aux.left.n:= 2.0;
          aux.right:= f.right;
          Deep_delete(f.left);
          Dispose(f);
          f:= aux;
        elsif Is_constant_pair(f) then
          cst_replaces_f( f.left.n + f.right.n );       --  cst+cst  ->  cst
        elsif Is_constant(f.left, 0.0) then
          right_replaces_f;                --  0+X -> X
        elsif Is_constant(f.right, 0.0) then
          left_replaces_f;                 --  X+0 -> X
        end if;
      when moins =>
        if Equivalent(f.left, f.right) then          --  X - X   ->    0
          cst_replaces_f(0.0);
        elsif Is_constant(f.left, 0.0) then              --  0 - X   ->   -X
          aux:= new Formula_Rec(moins_una);
          aux.left:= f.right;
          Deep_delete(f.left);
          Dispose(f);
          f:= aux;
        elsif Is_constant(f.right, 0.0) then
          left_replaces_f;        --  X - 0   ->   X
        end if;
      when fois =>
        if Equivalent(f.left, f.right) then             --  X*X -> X^2
          aux:= new Formula_Rec(puiss);
          aux.left:= f.left;
          aux.right:= new Formula_Rec(nb);
          aux.right.n:= 2.0;
          Deep_delete(f.right);
          Dispose(f);
          f:= aux;
        elsif Is_constant_pair(f) then
          cst_replaces_f( f.left.n * f.right.n );       --  cst+cst  ->  cst
        elsif f.left.s = puiss and then f.right.s = puiss and then Equivalent(f.left.left, f.right.left) then
          aux:= new Formula_Rec(par);        --  X^m * X^n   ->   X^(m+n)
          aux.left:= new Formula_Rec(plus);
          aux.left.left:= f.left.right;
          aux.left.right:= f.right.right;        --  aux= "(m+n)"
          nexp:= new Formula_Rec(puiss);
          nexp.left:= f.left.left;
          nexp.right:= aux;                 --  nexp= "X^(m+n)"
          Deep_delete(f.right.left);
          Dispose(f.left);
          Dispose(f.right);
          Dispose(f);                --  dissoudre ancienne expr
          f:= nexp;
        elsif f.right.s = puiss and then Equivalent(f.left, f.right.left) then  --  X * X^n   ->   X^(n+1)
          aux:= f.right;
          Deep_delete(f.left);
          dispose(f);
          f:= aux;            --  got rid of *
          aux:= new Formula_Rec(par);
          aux.left:= new Formula_Rec(plus);
          aux.left.left:= f.right;
          aux.left.right:= new Formula_Rec(nb);
          aux.left.right.n:= 1.0;     --  (n+1) prepared
          f.right:= aux;
        elsif f.left.s = puiss and then Equivalent(f.left.left, f.right) then  --  X^n * X   ->   X^(n+1)
          aux:= f.left;
          Deep_delete(f.right);
          dispose(f);
          f:= aux;            --  got rid of *
          aux:= new Formula_Rec(par);
          aux.left:= new Formula_Rec(plus);
          aux.left.left:= f.right;
          aux.left.right:= new Formula_Rec(nb);
          aux.left.right.n:= 1.0;     --  (n+1) prepared
          f.right:= aux;
        elsif Is_constant(f.left, 0.0) or else Is_constant(f.right, 0.0) then
          cst_replaces_f(0.0);         --  0*X or X*0  ->  0
        elsif Is_constant(f.left, 1.0) then
          right_replaces_f;            --  1*X  ->  X
        elsif Is_constant(f.right, 1.0) then
          left_replaces_f;             --  X*1  ->  X
        end if;

      when sur =>
        if Is_constant(f.right, 1.0) then
          left_replaces_f;        --  X/1 -> X
        elsif Equivalent(f.left, f.right) then
          cst_replaces_f(1.0);  --  X/X -> 1
        elsif Is_constant_pair(f) and then not Almost_zero(f.right.n) then
          cst_replaces_f( f.left.n / f.right.n );       --  cst/cst -> cst
        end if;

      when puiss =>
        if Is_constant(f.right, 0.0) then
          cst_replaces_f(1.0);        --  X^0  ->  1
        elsif Is_constant(f.right, 1.0) then
          left_replaces_f;              --  X^1  ->  X
        elsif f.left.s= nb and f.right.s= nb then       --  cst^cst  ->  cst
          cst_replaces_f( Exp(Log(f.left.n) * f.right.n) );
        end if;
      when Built_in_function =>
        Simplify_functions;
      when others=>
        null;  -- [P2Ada]: no otherwise / else in Pascal
    end case;
  end Simplify;

end Formulas;
-- Translated on 9-Apr-2015 by (New) P2Ada v. 28-Oct-2009
