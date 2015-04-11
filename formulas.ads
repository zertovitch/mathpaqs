--  ===========================================================================
--  ======|   Formulas                         |===============================
--  ===========================================================================

--  Based on a Pascal university exercise (~1990's)
--  Translated on 9-Apr-2015 by (New) P2Ada v. 28-Oct-2009
--  Reworked

-- Copyright (c) Gautier de Montmollin 2015
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.
--
-- NB: this is the MIT License, as found 2-May-2010 on the site
-- http://www.opensource.org/licenses/mit-license.php

-- TO DO: get rid of the OK out parameter; do all with exceptions

with Ada.Text_IO;

with Ada.Unchecked_Deallocation;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

generic

  type T_Nb is digits <>;

  with function Evaluate_variable(name: String) return T_Nb;

package Formulas is

       type S_Form  is
                (nb, vr,                                        --  0 arg
                 plus1, moins1,                                 --  1 arg
                 expn, logn,
                 sinus, cosinus, tg, arctg,
                 sh, ch, th,
                 par, croch, accol,
                 fois, plus, moins, sur, puiss);                --  2 args

  type Formula;

  type Form_p is access Formula;

  type Formula(S:  S_Form)  is
  record
    case S is
      when
          Nb =>
        N: T_Nb;
      when Vr =>
        V: Unbounded_String;
      when Plus1 |  Moins1 |
          Expn |  Logn |
          Sinus |  Cosinus |  Tg |  Arctg |
          Sh |  Ch |  Th |
          Par |  Croch |  Accol |
          Fois |  Plus |  Moins |  Sur |  Puiss=>
        F1,F2: Form_P;
    end case;
  end record;

  procedure Dispose is new Ada.Unchecked_Deallocation(Formula, Form_P);

  procedure Write_form (t: in  Ada.Text_IO.File_Type; f: Form_p);
  procedure String_to_form (str_base:  String; f: out form_p; OK: out Boolean);
  function Eval_form (f: Form_p) return T_nb;
  function EqForms(fa, fb : Form_p) return Boolean;
  procedure Simplify (f: in out Form_p);
  procedure Del_Form (f: in out Form_p);

  No_Error,
  Div_By_0,
  Not_Pos_Power: exception;

end Formulas;
