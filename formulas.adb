-- Translated on 9-Apr-2015 by (New) P2Ada v. 28-Oct-2009

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
-- This is for Pi :
-- with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

package body Formulas is

  package REF is new Ada.Numerics.Generic_Elementary_Functions(T_Nb);
  package RIO is new Ada.Text_IO.Float_IO(T_Nb);

  type S_Form_Set is array(S_Form) of Boolean;

  Neutres : constant S_Form_Set:= (Plus1 | Par | Croch | Accol => True, others => False);
  -- subtype Oper_2 is S_Form range Fois .. Puiss;

  par_or_terminal: constant S_Form_Set:= (par|croch|accol|nb|vr => True, others => False);

  function conv_strg(s: S_Form) return String is
  begin
    case s is
      when plus1   => return "+";
      when moins1  => return "-";
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

  conv_symb : constant array (Character'(' ') .. '~' ) of s_form :=
    (
    '+' => plus,
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

  procedure Write_form (t: in  Ada.Text_IO.File_Type; f: Form_p) is
    x : t_nb;
    use Ada.Text_IO, RIO;
  begin
    if  f/= null then
      case  f.s  is
        when nb =>
          x:= f.n;
          if  x = T_nb'Floor(x) then
            Put(t, Integer(x), 0);
          else
            Put(t,x,0,5,0);
          end if;
        when vr =>
          Put(t, To_String(f.v));
        when moins1=>
          Put(t,'-');
          Write_form(t,f.f1);
        when plus1=>
          Put(t,'+');
          Write_form(t,f.f1);
        when fois..puiss =>
          Write_form(t,f.f1);
          Put(t,conv_strg(f.s));
          Write_form(t,f.f2);
        when logn..th=>
          Put(t,conv_strg(f.s));
          Put(t,'(');
          Write_form(t,f.f1);
          Put(t,')');
        when par=>
          Put(t,'(');
          Write_form(t,f.f1);
          Put(t,')');
        when croch=>
          Put(t,'[');
          Write_form(t,f.f1);
          Put(t,']');
        when accol=>
          Put(t,'{');
          Write_form(t,f.f1);
          Put(t,'}');
        when others=>
          null;  -- [P2Ada]: no otherwise / else in Pascal
      end case;

    end if;
  end Write_form;

  procedure Del_form (f: in out form_p) is
  begin
    if f/=null then
      case  f.s  is
        when
            moins1 |  plus1 |
            expn |  logn |
            sinus |  cosinus |
            tg |  arctg |
            par |  croch |  accol=>
          Del_form(f.f1);
        when plus |  moins |
            fois |  sur |
            puiss=>
          Del_form(f.f1);
          Del_form(f.f2);
        when others=>
          null;  -- [P2Ada]: no otherwise / else in Pascal
      end case;
      Dispose(f);
      f:= null;
    end if;
  end Del_form;

  type Character_Set is array(Character) of Boolean;

  procedure String_to_form (str_base: String; f: out form_p; OK: out Boolean) is

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

    c_fin    : constant Character     := Character'Val (0);
    str: constant String:= No_Spaces(str_base) & c_fin;

    chiffres : constant Character_Set := ('0' .. '9' | '.' => True, others => False);
    lettres  : constant Character_Set := ('a' .. 'z' | 'A' .. 'Z' | '_' | '$' => True, others => False);

    i : Integer;

    function expression return form_p is
      function terme return form_p is
        function facteur return form_p is
          --
          function nombre return form_p is
            n : form_p;
            j : Integer;
          begin
            n:= new Formula(nb);
            j:=i;
            loop
              i:= i+1;
              exit when not chiffres(str(i));
            end loop;
            n.n:= T_Nb'Value(str(j..i-1));
            return n;
          exception
            when Constraint_Error =>
              n.n:= 0.0;
              OK:= False;
              return n;
          end nombre;

          function variable_fonction return form_p is
            --  variables, fonctions user, fonctions standard
            n                        : form_p;
            j                        : Integer;
          begin
            j:=i;
            loop
              i:= i+1;
              exit when not lettres(str(i));
            end loop;
            declare
              ch: constant String:= str(j..i-1);
              mch: constant String:= To_Upper(ch);
            begin
              if str(i)='(' then           --  --- (: fonctions std, usr
                for s in  expn .. fois loop
                  if mch = conv_mstr(s) then
                    n:= new Formula(s);
                    exit;
                  end if;
                end loop;
                i:= i + 1;
                -- !! fonctions usr !!
                n.f1:= expression;
                if str(i)/=')' then
                  OK:= False ;
                else
                  i:= i + 1;
                end if;
              else
                n:= new Formula(vr);
                n.v:= To_Unbounded_String(ch);
              end if;
            end;
            return n;
          end variable_fonction;

          --  -- Facteur
          n,
          n1 : form_p;
          c  : Character;
        begin
          n:= null;
          if  chiffres(str(i)) then
            n:= nombre;
          else
            if lettres(str(i)) then
              n:= variable_fonction;
            else
              case str(i) is
                when '-' =>
                  n:= new Formula(moins1);
                  i:= i + 1;
                  n.f1:= facteur;
                when '+' =>
                  n:= new Formula(plus1);
                  i:= i + 1;
                  n.f1:= facteur ;
                when others=>
                  null;  -- [P2Ada]: no otherwise / else in Pascal
              end case;
            end if;
          end if;
          c:= str(i);
          case  c  is
            when '^'=>
              n1:=n;
              n:= new Formula(puiss);
              i:= i + 1;
              n.f1:= n1;
              n.f2:= facteur;
            when '(' | '[' | '{'=>
              if  n=null then
                n:= new Formula(conv_symb(c));
                i:= i + 1;
                n.f1:= expression;
                case c is
                  when '('=>
                    OK:= OK  and str(i)=')';
                  when '['=>
                    OK:= OK  and str(i)=']';
                  when '{'=>
                    OK:= OK  and str(i)='}';
                  when others=>
                    null;
                end case;
                i:= i+1;
              end if;
            when others=>
              null;  -- [P2Ada]: no otherwise / else in Pascal
          end case;
          return n;
        end facteur;

        --  -- Terme
        n,
        g,
        d : form_p;
        c : Character;
      begin
        g:= Facteur;
        c:= str(i);
        if c = '*' or c = '/' then
          i:= i + 1;
          d:= Terme;
          n:= new Formula(conv_symb(c));
          n.f1:= g;
          n.f2:= d;
        else
          n:= g;
        end if;
        return n;
      end terme;

      --  -- Expression
      n,
      g,
      d : form_p;
      c : Character;
    begin
      g:= Terme;
      c:= str(i);
      if  c = '+' or c = '-' then
        i:= i+1;
        d:= Expression;
        n:= new Formula(conv_symb(c));
        n.f1:= g;
        n.f2:=  d;
      else
        n:=g;
      end if;
      return n;
    end expression;

  begin
    OK:= True;
    i:= 1;
    f:= Expression;
    OK:= OK and str(i) = c_fin;
    if not ok then
      Del_form(f);
    end if;
  end String_to_form;

  --  -------------------------------- Evaluate ---------------------------------

  function Eval_form (f: Form_p) return T_nb is
    aux: t_nb;
    use REF;
  begin
    if f = null then
      return 0.0;
    else
      case f.s is
        when nb=>
          return f.n;
        when vr=>
          return Evaluate_variable(To_String(f.v));
        when moins1 =>
          return -Eval_form(f.f1);
        when plus1 |
             par |
             croch |
             accol =>
          return Eval_form(f.f1);
        when plus =>
          return Eval_form(f.f1) + Eval_form(f.f2);
        when moins =>
          return Eval_form(f.f1) - Eval_form(f.f2);
        when fois =>
          aux:= Eval_form(f.f1);
          if aux = 0.0 then
            return 0.0;
          else
            if aux = 1.0 then
              return Eval_form(f.f2);
            else
              return aux * Eval_form(f.f2);
            end if;
          end if;
        when sur =>
          aux:= Eval_form(f.f2);
          if aux = 0.0 then
            raise div_by_0;
          else
            if aux = 1.0 then
              return Eval_form(f.f1);
            else
              return Eval_form(f.f1) / aux;
            end if;
          end if;
        when puiss =>
          aux:= Eval_form(f.f1);
          if aux <= 0.0 then
            raise not_pos_power;
          else
            return Exp(Eval_form(f.f2)*Log(aux));
          end if;
        when logn=>
          aux:= Eval_form(f.f1);
          if aux <= 0.0 then
            raise not_pos_power;
          else
            return Log(aux);
          end if;
        when expn=>
          return Exp(Eval_form(f.f1));
        when sinus=>
          return Sin(Eval_form(f.f1));
        when cosinus=>
          return Cos(Eval_form(f.f1));
        when sh=>
          return Sinh(Eval_form(f.f1));
        when ch=>
          return Cosh(Eval_form(f.f1));
        when th=>
          return Tanh(Eval_form(f.f1));
        when arctg=>
          return ArcTan(Eval_form(f.f1));
        when tg=>
          return Tan(Eval_form(f.f1));
      end case;
    end if;
  end Eval_form;

  --  -------------------------------- Compare -----------------------------------


  function EqForms(fa, fb : Form_p) return Boolean is
    ok     : Boolean;
    ga, gb : s_form;
  begin
    if fa = null and fb = null then
      return True;
    end if;
    if fa = null then -- fb is not null, at this point
      return False;
    end if;
    ga:= fa.s;
    gb:= fb.s;
    if neutres(ga) then
      ok:= EqForms(fa.f1,fb);
    else
      if neutres(gb) then
        ok:= EqForms(fa,fb.f1);
      else
        ok:= ga=gb;
        if ok then
          case  ga  is
            when nb =>
              ok:= fa.n = fb.n;
            when vr =>
              ok:= fa.v = fb.v;
            when moins1 =>
              ok:= EqForms(fa.f1, fb.f1);
            when plus | moins | fois | sur | puiss =>
              ok:= EqForms(fa.f1, fb.f1) and EqForms(fa.f2, fb.f2);
            when others =>
              null;  -- [P2Ada]: no otherwise / else in Pascal
          end case;
        end if;
      end if;
    end if;
    return ok;
  end EqForms;

  --  -------------------------------- Simplify -----------------------------------


  procedure Simplify (f : in out Form_p) is
    aux,
    nexp : form_p;
    x    : t_nb;

    function IsConst (a: form_p; cst : t_nb) return Boolean is
    begin
      return a /= null and then a.s = nb and then a.n = cst;
    end IsConst;

    procedure f1_replaces_f is
    begin
      aux:=f.f1;
      f.f1:= null; --  empˆeche destruction
      Del_form(f);
      f:= aux ;
    end f1_replaces_f;

    procedure f2_replaces_f is
    begin
      aux:=f.f2;
      f.f2:= null; --  empˆeche destruction
      Del_form(f);
      f:= aux ;
    end f2_replaces_f;

    procedure cst_replaces_f (c: t_nb) is
    begin
      Del_form(f);
      f:= new Formula(nb);
      f.n:= c ;
    end cst_replaces_f;

    procedure Simplify_functions is
      use REF;
    begin
      if f = null then
        return;
      end if;
      if f.f1 /= null then
        if f.f1.s = nb then        --  Evaluate "f(cst)" into f(cst)
          x:= f.f1.n;
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
      if f.s = cosinus and then f.f1 /= null and then f.f1.s = moins1 then
        aux:=f.f1.f1;    --  Cos(-X) -> Cos(x)
        Dispose(f.f1);
        f.f1:= aux;
      end if;
    end Simplify_functions;

    use REF;

  begin
    if f = null then
      return;
    end if;
    --  Compacter les arguments
    case  f.s  is
      when moins1 |  plus1 |
          expn |  logn |
          sinus |  cosinus |
          tg |  arctg |
          par |  croch |  accol=>
        Simplify(f.f1);
      when plus | moins | fois | sur | puiss =>
        Simplify(f.f1);
        Simplify(f.f2);
      when others=>
        null;  -- [P2Ada]: no otherwise / else in Pascal
    end case;
    if f = null then
      return;
    end if;
    case f.s is
      when moins1=>
        if f.f1 /= null and then f.f1.s= moins1 then     --    - -X  ->  X
          aux:=f.f1.f1;
          Dispose(f.f1);
          Dispose(f);
          f:= aux;
        end if;
      when plus1 =>
        f1_replaces_f;                                   --  +X -> X
      when par | croch | accol=>
        if  f.f1 /= null and then par_or_terminal(f.f1.s) then
          f1_replaces_f;     --  ((...)) ->(...), (c) -> c, (v) -> v
        end if;
      when plus =>
        if EqForms(f.f1, f.f2) then             --  X+X -> 2*X
          aux:= new Formula(fois);
          aux.f1:= new Formula(nb);
          aux.f1.n:= 2.0;
          aux.f2:= f.f2;
          Del_form(f.f1);
          Dispose(f);
          f:= aux;
        elsif f.f1.s = nb and f.f2.s= nb then   --  cst+cst -> cst
          cst_replaces_f( f.f1.n + f.f2.n );
        elsif IsConst(f.f1, 0.0) then
          f2_replaces_f;                --  0+X -> X
        elsif IsConst(f.f2, 0.0) then
          f1_replaces_f;                --  X+0 -> X
        end if;
      when moins =>
        if EqForms(f.f1, f.f2) then                --  X - X   ->    0
          cst_replaces_f(0.0);
        elsif IsConst(f.f1, 0.0) then              --  0 - X   ->   -X
          aux:= new Formula(moins1);
          aux.f1:= f.f2;
          Del_form(f.f1);
          Dispose(f);
          f:= aux;
        elsif IsConst(f.f2, 0.0) then
          f1_replaces_f;        --  X - 0   ->   X
        end if;
      when fois =>
        if EqForms(f.f1, f.f2) then             --  X*X -> X^2
          aux:= new Formula(puiss);
          aux.f1:= f.f1;
          aux.f2:= new Formula(nb);
          aux.f2.n:= 2.0;
          Del_form(f.f2);
          Dispose(f);
          f:= aux;
        elsif f.f1.s = nb and f.f2.s = nb then   --  cst*cst -> cst
          cst_replaces_f( f.f1.n * f.f2.n );
        elsif f.f1.s = puiss and then f.f2.s = puiss and then EqForms(f.f1.f1, f.f2.f1) then
          aux:= new Formula(par);        --  X^m * X^n   ->   X^(m+n)
          aux.f1:= new Formula(plus);
          aux.f1.f1:= f.f1.f2;
          aux.f1.f2:= f.f2.f2;        --  aux= "(m+n)"
          nexp:= new Formula(puiss);
          nexp.f1:= f.f1.f1;
          nexp.f2:= aux;                 --  nexp= "X^(m+n)"
          Del_Form(f.f2.f1);
          Dispose(f.f1);
          Dispose(f.f2);
          Dispose(f);                --  dissoudre ancienne expr
          f:= nexp;
        elsif f.f2.s = puiss and then EqForms(f.f1, f.f2.f1) then  --  X * X^n   ->   X^(n+1)
          aux:= f.f2;
          Del_Form(f.f1);
          dispose(f);
          f:= aux;            --  got rid of *
          aux:= new Formula(par);
          aux.f1:= new Formula(plus);
          aux.f1.f1:= f.f2;
          aux.f1.f2:= new Formula(nb);
          aux.f1.f2.n:= 1.0;     --  (n+1) prepared
          f.f2:= aux;
        elsif f.f1.s = puiss and then EqForms(f.f1.f1, f.f2) then  --  X^n * X   ->   X^(n+1)
          aux:= f.f1;
          Del_Form(f.f2);
          dispose(f);
          f:= aux;            --  got rid of *
          aux:= new Formula(par);
          aux.f1:= new Formula(plus);
          aux.f1.f1:= f.f2;
          aux.f1.f2:= new Formula(nb);
          aux.f1.f2.n:= 1.0;     --  (n+1) prepared
          f.f2:= aux;
        elsif IsConst(f.f1, 0.0) then
          cst_replaces_f(0.0);      --  0*X  ->  0
        elsif IsConst(f.f1, 1.0) then
          f2_replaces_f;            --  1*X  ->  X
        elsif IsConst(f.f2, 0.0) then
          cst_replaces_f(0.0);      --  X*0  ->  0
        elsif IsConst(f.f2, 1.0) then
          f1_replaces_f;            --  X*1  ->  X
        end if;

      when sur =>
        if IsConst(f.f2, 1.0) then
          f1_replaces_f;        --  X/1 -> X
        elsif EqForms(f.f1, f.f2) then
          cst_replaces_f(1.0);  --  X/X -> 1
        elsif f.f1.s= nb and f.f2.s= nb then
          cst_replaces_f( f.f1.n / f.f2.n );  --  cst/cst -> cst
        end if;

      when puiss =>
        if IsConst(f.f2, 0.0) then
          cst_replaces_f(1.0);        --  X^0  ->  1
        elsif IsConst(f.f2, 1.0) then
          f1_replaces_f;              --  X^1  ->  X
        elsif f.f1.s= nb and f.f2.s= nb then           --  cst^cst -> cst
          cst_replaces_f( Exp(Log(f.f1.n) * f.f2.n) );
        end if;
      when sinus |  cosinus | expn |  logn=>
        Simplify_functions;
      when others=>
        null;  -- [P2Ada]: no otherwise / else in Pascal
    end case;
  end Simplify;

end Formulas;
-- Translated on 9-Apr-2015 by (New) P2Ada v. 28-Oct-2009
