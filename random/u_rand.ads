--------------------------------------------------------------------------
-- The following is an implementation of a "universal" random number    --
-- generator algorithm developed by Dr. George Marsaglia of the         --
-- Supercomputer Computations Research Institute (SCRI) at Florida      --
-- State University.  This generator has a period of ~2**144 and has    --
-- been tailored for reproducibility in all CPU's with at least         --
-- 16 bit integer arithmetic and 24 bit floating point.  This algorithm --
-- does not generate random numbers < 2**-24.  At the end of this file  --
-- you will find a self test program that checks generated results      --
-- against known expected results and reports any inaccuracies.         --
--                                                                      --
-- Further references: "Toward a Universal Random Number Generator",    --
-- appearing in the Journal of The American Statistical Association.    --
--                                                                      --
-- This code appeared in the March/April publication of SIGAda's        --
-- Ada Letters and is considered public domain.  PCK                    --
--------------------------------------------------------------------------
--
-- Jul-2009: GdM: - Made generic and task safe: no more globals
--                - Procedure name aliases to make the replacement
--                  of Ada95+'s Ada.Numerics.Float_Random easy.
-- Mar-1988: Published in Ada Letters (Volume VIII, Number 2)

generic
    type Real is digits <>;

package U_Rand is

    subtype Uniformly_Distributed is Real range 0.0 .. 1.0;
    --
    -- ^ also useful to have a type to refer to, when using
    --   this random generator instead of Ada.Numerics.Float_Random

    M1 : constant := 179 ;
    M2 : constant := M1 - 10 ;

    subtype SEED_RANGE_1 is Integer range 1..M1-1 ;
    subtype SEED_RANGE_2 is Integer range 1..M2-1 ;

    Default_I : constant SEED_RANGE_1 := 12 ;
    Default_J : constant SEED_RANGE_1 := 34 ;
    Default_K : constant SEED_RANGE_1 := 56 ;
    Default_L : constant SEED_RANGE_1 := 78 ;

    type Generator is limited private;

    --------------------------------------------------
    -- Initialization of the pseudo-random sequence --
    --------------------------------------------------

    -- Fixed seed initialization
    --
    procedure Start(Gen   : out Generator;
                    New_I : SEED_RANGE_1 := Default_I ;
                    New_J : SEED_RANGE_1 := Default_J ;
                    New_K : SEED_RANGE_1 := Default_K ;
                    New_L : SEED_RANGE_2 := Default_L
                    ) ;
    procedure Reset (Gen : out Generator; Initiator : SEED_RANGE_1);
    -- Ada95

    -- Randomize with real-time clock
    --
    procedure Randomize (Gen : out Generator);
    procedure Reset (Gen : out Generator) renames Randomize; -- Ada 95 style

    ---------------------------------------------
    -- Obtaining the next pseudo-random number --
    ---------------------------------------------
    --
    function Next(Gen: Generator) return Uniformly_Distributed;
    pragma Inline(Next);
    function Random(Gen: Generator) return Uniformly_Distributed renames Next; -- Ada 95 style

private

    M3 : constant := 97 ;

    subtype RANGE_3 is INTEGER range 1..M3 ;
    type U_array is array(RANGE_3) of Real;

    type Generator is limited record
      NI, NJ : INTEGER ;
      C : Real ;
      U : U_array;
    end record;

end U_Rand ;
