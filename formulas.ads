--  ===========================================================================
--  ======|   Formulas: Symbolic computation                    |==============
--  ======|   Parsing - writing - evaluation - simplification   |==============
--  ===========================================================================

--  Based on a Pascal university exercise (~1990's)
--  Translated on 9-Apr-2015 by (New) P2Ada v. 28-Oct-2009
--  Reworked further (generic, private types, exceptions, etc.)

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
--   - implement user functions
--   - complete Simplify_functions
--   - improve Simplify (see misses at Test_Formulas)
--   - Put to String (then, parser entropy test)
--   - Deep_copy
--   - Deep_delete internal; Finalization

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;

generic

  type Real is digits <>;

  type Payload_type is private; -- This can be a container type for user variables

  with function Evaluate_variable (name : String; payload: Payload_type) return Real;

package Formulas is

  type Formula is private;
  null_formula : constant Formula;

  type Output_style is (
    normal,     --  Normal is infix, should be the closest to the parsed formula
    bracketed   --  Like normal, but displays a {} around every parse tree node
  );


  procedure Put (f : Formula; style : Output_style:= normal);
  procedure Put (t : in Ada.Text_IO.File_Type; f : Formula; style : Output_style:= normal);
  procedure Parse (str_base : String; f : out Formula);
  function Evaluate (f : Formula; payload : Payload_type) return Real;
  function Equivalent (fa, fb : Formula) return Boolean;
  procedure Simplify (f : in out Formula);
  procedure Deep_delete (f : in out Formula);

  Parse_Error,
  Div_By_0,
  Not_Pos_Power : exception;

private

  type S_Form is
                (                                    --  0 argument (leaf, terminal nodes):
                 nb, var,
                                                     --  1 argument:
                 moins_una, plus_una,
                 par, croch, accol,
                 -- vvv begin of built-in functions
                 expn, logn,
                 sinus, cosinus, tg, arctg,
                 sh, ch, th,
                                                     --  2 arguments:
                 min, max,
                 -- ^^^ end of built-in functions
                 moins, plus, sur, fois, puiss);

  subtype Unary is S_Form range moins_una .. th;
  subtype Binary is S_Form range min .. puiss;

  type Formula_Rec (s :  S_Form);

  type Formula is access Formula_Rec;
  null_formula : constant Formula := null;

  type Formula_Rec (s :  S_Form) is record
    case s is
      when nb =>   n : Real;
      when var =>  v : Unbounded_String;
      when Unary | Binary => left, right : Formula;
    end case;
  end record;

end Formulas;
