------------------------------------------------------------------------------
--  File:            Test_RSA.adb
--  Description:     Test for Multi_precision_integers : RSA cyphering
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Multi_precision_integers;          use Multi_precision_integers;
with Multi_precision_integers.IO;       use Multi_precision_integers.IO;
with Euclidean_Ring_Tools;

procedure Test_RSA is

  RSA_max: constant:= 10;
  subtype RSA_block is Multi_int(RSA_max);

  --  zero: constant RSA_block:=
  --    (n=> RSA_max, zero=> True, neg=> False, last_used=> 0,
  --     blk=>(others=> 0));

  --  one:  constant RSA_block:=
  --    (n=> RSA_max, zero=> False, neg=> False, last_used=> 0,
  --     blk=>(0=> 1, others=> 0));

  --  package ERT_multi is new
  --    Euclidean_Ring_Tools(RSA_block, zero, one, "-", "*", "/");
  package ERT is new
    Euclidean_Ring_Tools(Integer, 0, 1, "-", "*", "/");

  type RSA_Message is array( Integer range <> ) of RSA_block;

  -- Encode or decode:

  procedure Code( msg: in out RSA_Message; expo, modu: Multi_int ) is
  begin
    for i in msg'Range loop
      Power( msg(i), expo, msg(i), modu );
    end loop;
  end Code;

  procedure Put_in_blocks( msg: RSA_Message ) is
  begin
    for i in msg'Range loop
      Put(i); Put(" ]-> ");
      Put_in_blocks( msg(i) );
      New_Line;
    end loop;
  end Put_in_blocks;

  procedure Codec( msg: in out RSA_Message; e,d,n: Multi_int ) is
    show_contents: constant Boolean:= False;
  begin
    if show_contents then
      Put_Line("    - Original RSA message");
      Put_in_blocks( msg );
    end if;
    -- Encode
    Code( msg, e, n );
    if show_contents then
      Put_Line("    - Encoded RSA message");
      Put_in_blocks( msg );
    end if;
    -- Decode
    Code( msg, d, n );
    if show_contents then
      Put_Line("    - Decoded RSA message");
      Put_in_blocks( msg );
    end if;
  end Codec;

  procedure Test_Allenby_Redfern( s: String; e,d,n: Multi_int ) is
    msg: RSA_Message( 1..s'Last/2 );
    r: String( s'Range );
  begin
    New_Line;
    -- Translate String into RSA blocks
    Put_Line(s);
    for i in msg'Range loop
      Fill( msg(i), (Character'Pos(s(i*2-1))-65) * 100 +
                    (Character'Pos(s(i*2  ))-65) );
    end loop;

    Codec( msg, e,d,n );

    -- Translate RSA blocks into String
    for i in msg'Range loop
      r(i*2-1):= Character'Val( 65 + Basic( msg(i) )  /  100 );
      r(i*2  ):= Character'Val( 65 + Basic( msg(i) ) mod 100 );
    end loop;
    Put_Line(r);
  end Test_Allenby_Redfern;

  procedure Decode_Allenby_Redfern is
    msgint: constant array(1..59) of Integer:=
      (6132, 0615, 5995, 0615, 2368, 1225, 5201, 3198, 0499, 1342, 4718, 0499, 3221,
       2851, 1520, 0001, 2432, 4326, 0472, 4945, 2555, 4718, 2271, 6219, 5941, 3266,
       4713, 1990, 0036, 0096, 0744, 5882, 3998, 1790, 3198, 1541, 5349, 3998, 5941,
       6098, 0048, 0893, 5349, 1835,

          3221, 0615, 0044, 0021, 2915, 1318, 3696,
       0744, 4885, 3608, 4719, 0615, 0832, 3147, 4718);
    msg: RSA_Message(msgint'Range);
    ni:    constant Integer:= 6499; -- 67*97
    phini: constant Integer:= 6636; -- 66*96
    ei:    constant Integer:= 109;
    di: Integer;
    d,n: Multi_int(0);
    r: String( 1..200 ):= (others=> ' ');
    function ASCRF( a:Integer ) return Integer is
    begin
      if a in 0..25 then
        return 65 + a;
      else
        return Character'Pos('*');
      end if;
    end ASCRF;
  begin
    for i in msg'Range loop Fill( msg(i), msgint(i) ); end loop;

    New_Line;

    Fill(n,ni);

    Put( ERT.GCD(ei,phini) ); New_Line;

    for i in 0..ni-1 loop
      if ei*i mod 6636=1 then
        di:= i; Put(i); New_Line;
      end if;
    end loop;
    Fill(d,di);

    Code( msg, d,n );

    -- Translate RSA blocks into String
    for i in msg'Range loop
      r(i*2-1):= Character'Val( ASCRF( Basic( msg(i) )  / 100 ) );
      r(i*2  ):= Character'Val( ASCRF( Basic( msg(i) ) rem 100 ) );
    end loop;
    Put_Line(r);
  end Decode_Allenby_Redfern;

  procedure Test_GM( s: String; e,d,n: Multi_int ) is
    msg: RSA_Message( s'Range );
    r: String( s'Range );
  begin
    New_Line;
    -- Translate String into RSA blocks
    Put_Line(s);
    for i in msg'Range loop
      Fill( msg(i), Character'Pos(s(i)) );
    end loop;

    Codec( msg, e,d,n );

    -- Translate RSA blocks into String
    for i in msg'Range loop
      r(i):= Character'Val( Basic( msg(i) ) );
    end loop;
    Put_Line(r);
  end Test_GM;

begin
  if Debug then
    Put_Line( "NB: the constant DEBUG in Multi_precision_integers");
    Put_Line( "      is set on True, it means a very poor performance!");
    New_Line;
    Put_Line( "    [x] Yes, I am aware of it !");
    New_Line;
  end if;

  Test_Allenby_Redfern(
     "THIS_IS_A_SMALL_TEST_ALLENBY_REDFERN",
     e=> Multi(97), d=> Multi(3073), n=> Multi(4453)
  );

  Decode_Allenby_Redfern;

  Test_GM(
     "Thougher test",
     e=> Multi(3),
     d=> Val("129143530427"),
     n=> Val("440171") * Val("440093")
  );

  Test_GM(
     "Thougher test 2",
     e=> Multi(5),
     d=> Val("719520409731917"),
     n=> Val("17144977") * Val("104917093")
  );

end Test_RSA;
