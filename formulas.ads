--  ===========================================================================
--  ======|   Formulas: Symbolic computation                 |=================
--  ======|   Parsing - output - evaluation - simplification |=================
--  ===========================================================================

--  Based on a Pascal university exercise (~1990's)
--  Translated on 9-Apr-2015 by (New) P2Ada v. 28-Oct-2009
--  Reworked further (generic, private types, etc.)

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

-- TO DO:
--   - get rid of the OK out parameter; do all with exceptions
--   - implement user functions
--   - for unary operators: "arg" instead of "left" descendant
--   - complete Simplify_functions
--   - improve Simplify (see misses at Test_Formulas)
--   - Deep_copy
--   - Deep_delete internal; Finalization

with Ada.Text_IO;

with Ada.Unchecked_Deallocation;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

generic

  type Real is digits <>;

  with function Evaluate_variable(name: String) return Real;

package Formulas is

  type Formula is private;
  null_formula: constant Formula;

  procedure Put (t: in  Ada.Text_IO.File_Type; f: Formula);
  procedure Parse (str_base:  String; f: out Formula; OK: out Boolean);
  function Evaluate (f: Formula) return Real;
  function Equivalent (fa, fb : Formula) return Boolean;
  procedure Simplify (f: in out Formula);
  procedure Deep_delete (f: in out Formula);

  No_Error,
  Div_By_0,
  Not_Pos_Power: exception;

private

  type S_Form is
                (nb, vr,                                        --  0 arg
                 plus_una, moins_una,                           --  1 arg
                 expn, logn,
                 sinus, cosinus, tg, arctg,
                 sh, ch, th,
                 par, croch, accol,
                 fois, plus, moins, sur, puiss);                --  2 args

  subtype Leaf is S_Form range nb .. vr;
  subtype Unary is S_Form range plus_una .. accol;
  subtype Binary is S_Form range fois .. puiss;

  type Formula_Rec(S:  S_Form);

  type Formula is access Formula_Rec;
  null_formula: constant Formula:= null;

  type Formula_Rec(S:  S_Form) is record
    case S is
      when Nb =>   N: Real;
      when Vr =>   V: Unbounded_String;
      when plus_una |  moins_una |
          Expn |  Logn |
          Sinus |  Cosinus |  Tg |  Arctg |
          Sh |  Ch |  Th |
          Par |  Croch |  Accol |
          Fois |  Plus |  Moins |  Sur |  Puiss
        =>
          left, right: Formula;
    end case;
  end record;

  procedure Dispose is new Ada.Unchecked_Deallocation(Formula_Rec, Formula);

end Formulas;
