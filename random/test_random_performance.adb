with Ada.Calendar;                      use Ada.Calendar;
with Ada.Text_IO;                       use Ada.Text_IO;

with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with U_Rand;

-- PragmARC Random generators provided by J. Carter, http://pragmada.x10hosting.com/pragmarc.htm :
--  with PragmARC.Real_Random_Values,
--       PragmARC.KISS_Random,
--       PragmARC.Threefry_Random;

procedure Test_random_performance is

  generic
    type Real is digits <>;
  procedure Test;

  procedure Test is
    package RIO is new Ada.Text_IO.Float_IO (Real);

    generic
      title : String;
      with procedure Any_Reset;
      with function Any_Random return Real;
    procedure Generator_Test;

    procedure Generator_Test is
      T0, T1 : Time;
      n : constant Integer:= 200_000_000;
      r : Real:= 0.0;
    begin
      New_Line;
      Put_Line ("* Testing generator : " & title & ". Putting a few numbers here:");
      Any_Reset;
      for i in 1 .. 5 loop
        Put ("     ");
        RIO.Put (Any_Random);
        New_Line;
      end loop;
      T0 := Clock;
      for i in 1 .. n loop
        r := r + Any_Random;
      end loop;
      T1 := Clock;
      Put_Line(
        "     -> Time elapsed for" & Integer'Image(n) &
        " iterations:" & Duration'Image(T1-T0) & " seconds."
      );
      Put("   Average: ");
      RIO.Put (r / Real(n));
      New_Line;
    end Generator_Test;

    --------------------------------
    --  Ada.Numerics.Float_Random --
    --------------------------------
    -- Note 14-Sep-2013: GNAT since at least 2008 uses Mersenne Twister
    --
    ANF_gen: Ada.Numerics.Float_Random.Generator;
    procedure ANF_Reset is begin Reset(ANF_gen); end ANF_Reset;
    function ANF_Random return Real is begin return Real(Random(ANF_gen)); end ANF_Random;
    procedure ANF_Test is new Generator_Test("Ada.Numerics.Float_Random", ANF_Reset, ANF_Random);

    ------------
    -- U_Rand --
    ------------
    --
    package R_U_Rand is new U_Rand(Real);
    U_gen: aliased R_U_Rand.Generator;
    procedure U_Reset is begin R_U_Rand.Reset(U_gen); end U_Reset;
    function U_Random return Real is begin return R_U_Rand.Random(U_gen); end U_Random;
    procedure U_Test is new Generator_Test("U_Rand (G. Marsaglia)", U_Reset, U_Random);

    ----------
    -- KISS --
    ----------
    --
    --  package R_K_Rand is new
    --  PragmARC.Real_Random_Values(Real,
    --    PragmARC.KISS_Random.Generator, PragmARC.KISS_Random.Raw);
    --  K_gen: PragmARC.KISS_Random.Generator;
    --  procedure K_Reset is begin PragmARC.KISS_Random.Randomize(K_gen); end;
    --  function K_Random return Real is begin return R_K_Rand.Random(K_gen); end;
    --  procedure K_Test is new Generator_Test("KISS (G. Marsaglia)", K_Reset, K_Random);

    --------------
    -- Threefry --
    --------------
    --
    --  package R_T_Rand is new PragmARC.Real_Random_Values(Real,
    --    PragmARC.Threefry_Random.Generator, PragmARC.Threefry_Random.Random);
    --  T_gen: PragmARC.Threefry_Random.Generator;
    --  procedure T_Reset is begin PragmARC.Threefry_Random.Randomize(T_gen); end;
    --  function T_Random return Real is begin return R_T_Rand.Random(T_gen); end;
    --  procedure T_Test is new Generator_Test("Threefry", T_Reset, T_Random);

  begin
    Put_Line("Precision: " & Integer'Image(Real'Digits) & " digits.");
    U_Test;
    -- K_Test;
    ANF_Test;
    -- T_Test;
  end Test;

  -- procedure Test_Float is new Test(Float);
  procedure Test_Long_Float is new Test(Long_Float);

begin
  -- Test_Float;
  Test_Long_Float;
end Test_random_performance;
