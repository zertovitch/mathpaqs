------------------------------------------------------------------------------
--  File:            multi_precision_integers.ads
--
--  Description:     Multiple precision integers package
--
--  Date/version:       Aug-2007: - No more generics (Long_Block_type,
--                                  Block_type,... always the largest possible
--                                  idea: J.C.)
--                                - Fixed Basic(...) (based on J.C.'s remarks)
--                      Nov-2006: - unsigned types for blocks
--                                - a block uses 2 bits more
--                                - Ada95+ only
--                      Mar-2002: Bugs fixed related to zero field, division
--                      Nov-1999: - procedures (no stack, less copies !)
--                                - new data structure
--                      Dec-1996: First version (operators only)
--
--  Author:          G. de Montmollin, Univ. Neuchatel
--
--  Thanks to:       Duncan Sands, Univ. Paris-Sud, CNRS
--                   Jeffrey R. Carter
--
--  Tested on:       Intel 586 (32 bit) - Windows 98, NT4+ - GNAT 3.13p+
--                   Intel 586 (32 bit) - Windows 98, NT4+ - ObjectAda 7.2.1+
--                   Alpha-AXP (64 bit) - OpenVMS 7.1      - Compaq Ada
--
--  Division algorithm adaptated from BigInt 1.0 library,
--  by Stephen Adams, that refers to
--  D. E. Knuth, the Art of computer programming
--  volume 2, "Seminumerical Algorithms"
--  section 4.3.1, "Multiple-Precision Arithmetic"
--
------------------------------------------------------------------------------

with System;

package Multi_precision_integers is

  -- Integers for array indexing --

  subtype Index_int is Integer;

  -- THE multi-precision integer type --

  type Multi_int(n: Index_int) is private;

  -- Integer type for small values --

  subtype Basic_int is Integer; -- the "normal" signed integer

  ----------------------------------------------------------------
  -- Debug mode: checks results of arithmetic operations and    --
  -- Multi_int variables' integrity.                            --
  -- CAUTION: Debug = True reduces monstruously the performance --
  ----------------------------------------------------------------

  Debug: constant Boolean:= False;

  ---------------------------------------------
  ----- Informations, conversions, filling ----
  ---------------------------------------------

  -- Convert Basic_int to Multi_int
  function Multi(small: Basic_int) return Multi_int;

  -- Convert Multi_int to Basic_int (when possible, else: Cannot_fit raised)
  function Basic(large: Multi_int) return Basic_int;

  -- Fill an Multi_int of greater array dimension with a smaller one
  procedure Fill(what: out Multi_int; with_smaller: Multi_int);
  procedure Fill(what: out Multi_int; with_basic: Basic_int);

  -- Comparisons
  function Equal (i1,i2: Multi_int) return boolean;
  function Equal (i1: Multi_int; i2:Basic_int) return boolean;
  function ">" (i1,i2: Multi_int) return Boolean;
  function ">" (i1: Multi_int; i2:Basic_int) return Boolean;
  function "<" (i1,i2: Multi_int) return Boolean;
  function "<" (i1: Multi_int; i2:Basic_int) return Boolean;
  function ">=" (i1,i2: Multi_int) return Boolean;
  function ">=" (i1: Multi_int; i2:Basic_int) return Boolean;
  function "<=" (i1,i2: Multi_int) return Boolean;
  function "<=" (i1: Multi_int; i2:Basic_int) return Boolean;

  -- Other informations
  function Bits_per_block return Positive;

  ---------------------------------------------------------------------------
  -------- Arithmetic operators.                                   ----------
  -------- For speed, the "procedure" variants should be preferred ----------
  ---------------------------------------------------------------------------

  ---------------------------
  ----- Unary operators -----
  ---------------------------

  procedure Opp(i: in out Multi_int);
  function "+" (i: Multi_int) return Multi_int;
  function "-" (i: Multi_int) return Multi_int;

  procedure Abso(i: in out Multi_int);
  function "ABS" (i: Multi_int) return Multi_int;

  function Sign(i: Multi_int) return Basic_int;
  function Even(i: Multi_int) return Boolean;
  function Odd (i: Multi_int) return Boolean;

  ----------------------------
  ----- Binary operators -----
  ----------------------------

  ---------------------------
  -- Addition, subtraction --
  ---------------------------

  procedure Add(i1,i2: in Multi_int; i3: in out Multi_int);

  function "+" (i1,i2: Multi_int) return Multi_int;
  function "+" (i1: Multi_int; i2: Basic_int) return Multi_int;
  function "+" (i1: Basic_int; i2: Multi_int) return Multi_int;

  procedure Sub     (i1,i2: in Multi_int; i3: in out Multi_int);
  procedure Subtract(i1,i2: in Multi_int; i3: in out Multi_int)
    renames Sub;

  function "-" (i1,i2: Multi_int) return Multi_int;
  function "-" (i1: Multi_int; i2: Basic_int) return Multi_int;
  function "-" (i1: Basic_int; i2: Multi_int) return Multi_int;

  --------------------
  -- Multiplication --
  --------------------

  procedure Multiply(i1,i2: in Multi_int; i3: in out Multi_int);
  procedure Mult    (i1,i2: in Multi_int; i3: in out Multi_int)
    renames Multiply;

  procedure Multiply(i1: in Multi_int; i2: Basic_int; i3: in out Multi_int);
  procedure Mult    (i1: in Multi_int; i2: Basic_int; i3: in out Multi_int)
    renames Multiply;

  function "*" (i1,i2: Multi_int) return Multi_int;
  function "*" (i1: Multi_int; i2: Basic_int) return Multi_int;
  function "*" (i1: Basic_int; i2: Multi_int) return Multi_int;

  -------------------------
  -- Division, Remainder --
  -------------------------

  procedure Div_Rem (i1: in     Multi_int; i2: in     Basic_int;
                     q :    out Multi_int;  r:    out Basic_int);
  procedure Div_Rem (i1,i2: in Multi_int; q,r:    out Multi_int);

  procedure Divide (i1,i2: in Multi_int; q: out Multi_int);

  function "/" (i1,i2: Multi_int) return Multi_int;
  function "/" (i1: Multi_int; i2: Basic_int) return Multi_int;
  function "Rem" (i1,i2: Multi_int) return Multi_int;
  function "Rem" (i1: Multi_int; i2: Basic_int) return Multi_int;
  function "Rem" (i1: Multi_int; i2: Basic_int) return Basic_int;
  function "Mod" (i1,i2: Multi_int) return Multi_int;
  function "Mod" (i1: Multi_int; i2: Basic_int) return Multi_int;
  function "Mod" (i1: Multi_int; i2: Basic_int) return Basic_int;

  -----------
  -- Power --
  -----------

  procedure Power (i: Multi_int; n: Natural; ipn: out Multi_int);
  function "**" (i: Multi_int; n: Natural) return Multi_int;
  -- + 26-Mar-2002 :
  procedure Power (i: Multi_int; n: Multi_int; ipn: out Multi_int;
                   modulo: Multi_int);

  Cannot_fit, Empty_multi_int : exception;

  Array_too_small : exception;

  Result_undersized,
  Quotient_undersized,
  Remainder_undersized: exception;


  Division_by_zero: exception;

  Zero_power_zero, Power_negative: exception;
  Power_modulo_non_positive: exception;

private

  --> Long_Block_type is used for + and * of blocks.
  -- It is by design
  --   a/ the largest possible modular integer, and
  --   b/ twice the size of Block_type defined below.
  -- With the double of bits (2n) one can store m**2 + 2m without overflow
  --   where m = 2**n - 1 is the largest value possible on n bits.
  type Long_Block_type is mod System.Max_Binary_Modulus;

  --> Same size as Long_Block_type, but signed:
  type Long_Block_type_signed is
    range -2**(Long_Block_type'Size-1)..2**(Long_Block_type'Size-1)-1;

  --> Block_type: unsigned integer used to store a chunk of a Multi_int.
  type Block_type is mod 2 ** (Long_Block_type'Size / 2);

  Block_type_bits: constant:= Block_type'Size;

  cardblock: constant:= 2 ** Block_type_bits;
  -- Number of possible values
  maxblock: constant:= Block_type'Last;
  -- NB: GNAT (2006) optimizes out correctly the Block_type(l and maxblock_long)
  --     to Block_type(l), the latter form being caught when range checks are on.

  type Block_array is array( Index_int range <> ) of Block_type;
  -- 2006: was "of Basic_int" for obscure reasons...

  type Multi_int(n: Index_int) is record
    blk:       Block_array( 0..n ); -- the n blocks with ABSOLUTE value
    neg:       Boolean;             -- negative flag
    zero:      Boolean:=True;       -- zero flag (supercedes the other fields)
    last_used: Index_int;           -- the others blocks are supposed 0
  end record;

  -- NB the `zero' field supercedes EVERY other information (last_used, neg)

  ----------------------------------------------------------------------------
  --   Format of type Multi_int.blk: ( i_0, i_1, ..., i_k, *, ..., * )      --
  --   i_0..i_k are >=0 ; others (*) are treated as 0                       --
  ----------------------------------------------------------------------------

  -- Some internal procedures use by check:

  procedure Multiply_internal_copy_export(i1,i2: in Multi_int; i3: in out Multi_int);

  procedure Div_Rem_internal_both_export(i1,i2: in Multi_int; q,r: in out Multi_int);

end Multi_precision_integers;
