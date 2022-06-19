------------------------------------------------------------------------------
--  File:            Test_int.adb
--                   !! To do: split into a demo and a (silent) unit test
--                             or make use of the `verbose` flag.
--  Description:     Test for Multi_precision_integers
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;
with Multi_Precision_Integers;          use Multi_Precision_Integers;
with Multi_Precision_Integers.IO;       use Multi_Precision_Integers.IO;

pragma Warnings (".I");
pragma Warnings ("U");

procedure Test_Int is
  use type Basic_Int;
  package BIO is new Ada.Text_IO.Integer_IO (Basic_Int);
  package IIO is new Ada.Text_IO.Integer_IO (Integer);

  verbose : constant Boolean := Ada.Command_Line.Argument_Count > 0;

  i,j,k,l: Multi_Int(20);

  shift: constant Multi_Int:= Val("4294967296"); -- 2**32

  -- Tests de puissance <<manuels>>
  m,n,o,p, m_sur_n, reste_m_sur_n: Multi_Int( 903 );

  s    : String(1..100);
  lstr : Natural;

  procedure Test_aff(s: String; i: Multi_Int;
                     detail: Boolean:= False) is
  begin
    Put_Line("-- [" & s & "]");
    if detail then
      Put("  [aff. blocs]  "); Put_in_blocks(i); New_Line;
      Put("  [aff. decimal]  Chiffres: "); Put(Natural'Image(Number_of_digits(i)));
      Put("  D>" & Str(i) & "<D");
    else
      Put(i);
    end if;
    New_Line;
  end Test_aff;

  procedure miniput( a: Multi_Int ) is
  begin
    if Number_of_digits(a) <= 60 then
      Put_in_blocks(a);
      Put(a);
    else
      Put("(big...)");
    end if;
  end miniput;

  function Test_egalite( a,b: Multi_Int;
                          nom: String ) return Boolean is
  begin
    if not Equal( a , b ) then
      New_Line;
      Put("Erreur [" & nom & ";a="); miniput(a);
      Put(";b=");   miniput(b);
      Put(";a-b="); miniput(a - b);
      Put("] [ret]");
      Skip_Line;
      return False;
    else
      return True;
    end if;
  end Test_egalite;

  procedure Pause is
  begin
    Put("[ Pressez Return SVP ]"); Skip_Line;
  end Pause;

  function Decimales_Pi(N: Natural) return Multi_Int is
    X,Y,S: Multi_Int(Index_Int(N));

    procedure Arctg (m,c,e: Basic_Int) is
      i : Basic_Int := 1;
      moins: Boolean;
    begin
      Fill(X, (c*(Multi(10)**N)) / m);
      Fill(S, S + e*X);
      while not Equal(X, 0) loop
        Fill(X, X / (m*m));
        Fill(Y, X / (2*i + 1));
        if i mod 2 =1 then
          moins:=     (e=1);
        else
          moins:= not (e=1);
        end if;
        if moins then
        Sub(S,Y, S);
        else
        Add(S,Y, S);
        end if;
        i:= i + 1;
      end loop;
    end Arctg;

  begin
    Arctg(18, 48, 1); -- formule de Gauss
    Arctg(57, 32, 1);
    Arctg(239,20,-1);
    return S;
  end Decimales_Pi;

  -- Nombres de Syracuse. Idee transmise par Eric Batard.

  procedure Syracuse(i0: Multi_Int) is
    i: Multi_Int(20);
  begin
    Fill(i, i0);

    loop

      if Even(i) then
        Fill(i, i/2);
      else
        Fill(i, (3*i+1)/2);
      end if;

      Put(i); New_Line;

      exit when Equal(i,1);
    end loop;

  end Syracuse;

  -- 17-Feb-2002: Test procedure operators for length overflows

  procedure Test_Procedure_Operators is

    a,b,c: Multi_Int(20);
    cm1: Multi_Int(c.n-1);
    cmu: Multi_Int(a.n+b.n+1);
    cmu2: Multi_Int(a.n+b.n+3);

    procedure Bourrage( n: in out Multi_Int ) is
    begin
      loop
        begin
          Fill( n, n * 2 );
        exception
          when Array_too_small => exit;
        end;
      end loop;
    end Bourrage;

  begin
    Put("Test limites...");
    Fill( a, Val("1") );
    Fill( b, Val("1") );
    Bourrage(a);
    Bourrage(b);

    -- 1/ Test limites Fill
    begin
      Fill( c, a+b ); Put( " 'Fill': undersize not detected!"); Skip_Line;
    exception
      when Array_too_small => null; -- tout va bien
    end;

    -- 2/ Test limites Add
    begin
      Add( a,b, c ); Put( " 'Add': undersize not detected!"); Skip_Line;
    exception
      when Result_undersized => null; -- tout va bien
    end;

    -- 3/ Test limites Sub
    begin
      Sub( a,b, cm1 ); Put( " 'Sub': undersize not detected!"); Skip_Line;
    exception
      when Result_undersized => null; -- tout va bien
    end;

    -- 4/ Test limites Mult
    begin
      Mult( a,b, cmu ); Put( " 'Mult': undersize not detected!"); Skip_Line;
    exception
      when Result_undersized => null; -- tout va bien
    end;

    -- 5/ Test limites Div_Rem (q)
    begin
      Div_Rem( a, Multi(1), cm1, c ); Put( " 'Div': q undersize not detected!"); Skip_Line;
    exception
      when Quotient_undersized => null; -- tout va bien
    end;

    -- 6/ Test limites Div_Rem (r)
    begin
      Div_Rem( a,b, c, cm1 ); Put( " 'Div': r undersize not detected!"); Skip_Line;
    exception
      when Remainder_undersized => null; -- tout va bien
    end;

    -- 7/ Test limites Power
    Power( a, 2, cmu2 ); -- devrait aller
    begin
      Power( a, 3, cmu ); Put( " 'Power': r undersize not detected!"); Skip_Line;
    exception
      when Result_undersized => null; -- tout va bien
    end;

    Put_Line("fini.");
  end Test_Procedure_Operators;

  procedure Mix_Demo_Test is
  begin
    New_Line;

    if Debug then
      Put_Line( "NB: the parameter DEBUG for Multi_precision_integers");
      Put_Line( "      is set on True, it means a very poor performance!");
      New_Line;
      Put_Line( "    [x] Yes, I am aware of it !");
      New_Line;
    else
      Put_Line( "NB: Multi_precision_integers.Debug = False");
    end if;

    --  -- 2007: Test Basic(j) on a signed 32-bit Basic_int
    --  Fill( j, Val("2147483645") ); -- 2147483648 = 2**31
    --  for count in 5..9 loop
    --    Put_in_blocks(j); New_Line;
    --    Put(j); Put(" = "); Put(Basic(j)); New_Line;
    --    Fill(j, j + 1);
    --  end loop;

    Fill( j, Val("123456789") * Val("1000000001000000001000000001000000001") );

    Put ("Bits per block on this compiler-platform combination: ");
    BIO.Put (Basic_Int (Bits_per_block), 0);
    New_Line (2);

    Test_Procedure_Operators;
    Pause;

    --  -- Simple operations
    --  Put(Multi(1)+Multi(1));
    --  Put(Multi(10)*Multi(10)+Multi(1));
    --  Put(Multi(101) / Multi(10));

    Fill(m,1);
    for i in 1..45 loop
      IIO.Put(Number_of_digits(m)-1); Put(m); New_Line;
      Multiply(m,10,m);
    end loop;
    Pause;

    Fill(m,10);
    for i in 1..7 loop
      IIO.Put(Number_of_digits(m)-1); Put(m); New_Line;
      Fill(m, m*m);
      -- Multiply(m,m,m);
    end loop;
    Pause;

    -- Test carry spread in *
    Fill(i,1);
    Fill(j,0);
    Fill(k,Val("2") ** 32);
    for c in 1..5 loop
      Add(j,i,j);
      Multiply(i,k,i);
    end loop;
    -- j is {1 | 1 | 1 | ...}
    Fill(i,i-1);
    -- i (binary) is {11..111 | 11..111 | 11..111 | ...}
    Put("i=  "); Put_in_blocks(i); New_Line;
    Put("j=  "); Put_in_blocks(j); New_Line;
    Put("i*j="); Put_in_blocks(i*j); New_Line;
    Pause;

    Fill(m,     Val("123456789"));
    Fill(m, m * Val("1000000001000000001000000001000000001000000001000000001"));

    Fill(j,1);
    for i in Basic_Int'(0)..63 loop
      -- Fill(j,10**i);
      Div_Rem( m, j, o, p);
      Multiply(j, 10, j);
      Put(Multi(i), 3);
      if Equal(o,0) then
        Put("  ");
      else
        Put("   ");
      end if;
      -- Put(Multi(10)**i, 25); Put("     ");
      Put( o ); Put(" , ");
      Put( p ); New_Line;
    end loop;

    Pause;
    Put_Line("Pi par formule de Gauss");

    for dec in 1..76 loop
      Put( Decimales_Pi(dec) );
      New_Line;
    end loop;

    Pause;

    loop
      begin
        Put("Nombres de Syracuse, nombre de depart (ch. vide: sortie): ");
        Get_Line(s,lstr);
        exit when lstr=0;
        Syracuse( Val(s(1..lstr)) );
      exception
        when others=> Put_Line("Erreur!");
      end;
    end loop;

    Put_Line("Nombres Phenix");
    for i in Basic_Int'(1)..19 loop
      Put( Val("052631578947368421") * i );
      New_Line;
    end loop;

    Pause;

    Test_aff( "1234567890123456789012345678901234567890",
              Val("1234567890123456789012345678901234567890"), True);

    Test_aff( "1000000 * 1000000", Val("1000000") *  Val("1000000"),True );

    Test_aff( " Mersenne a)  (2^1931-1) MOD 193949641 [=0 car 193949641" &
              " |(2^1931-1) ]",
                (Multi(2)**1931 -1) mod Val("193949641") );

    Test_aff( " Mersenne b)  193949641 = 3863*50207",
                Val("193949641") / 3863 );

    Test_aff( " Calc. 'sur le web' (""*""), q=19382",
                Val("411309159702408")  / Val("21221192844") );

    Test_aff( " Calc. 'sur le web' (""*""), r=0",
                Val("411309159702408") mod Val("21221192844") );
    Pause;

    Test_aff( " 1 mu/mu", Multi(  10000 )  / Multi(  10000 ));
    Test_aff( " 0 mu/mu", Multi(  10000 )  / Multi(  10001 ));
    Test_aff( "-1 mu/mu", Multi( -10000 )  / Multi(  10000 ));
    Test_aff( " 0 mu/mu", Multi(  10000 )  / Multi( -10001 ));

    Test_aff( " 1 mu/si", Multi(  10000 )  /   10000  );
    Test_aff( " 0 mu/si", Multi(  10000 )  /   10001  );
    Test_aff( "-1 mu/si", Multi( -10000 )  /   10000  );
    Test_aff( " 0 mu/si", Multi(  10000 )  / (-10001 ));

    Test_aff( "-1230111", 100*Multi( -123 )*100 - 111 );

    Test_aff( "-23", Multi(  100 )  -         123  );
    Test_aff( "-23",         100    + Multi( -123 ));
    Test_aff( "+23",        -100    + Multi(  123 ));
    Test_aff( "+23", Multi( -100 )  -      ( -123 ));

    Pause;

    Fill( j, ((Multi( 56 ) * 100) * 10  + 88) );

    Test_aff( "j= 56088 = 123 * 456", j );
    Test_aff( "   5608  = j/10     ", j / 10 );
    Test_aff( "   560   = j/100    ", j / 100 );
    Test_aff( "j/1", j / 1 );     -- 56088 = 32768 + 23320
    Test_aff( "0=0/j", (0*j)/1);

    Test_aff( "",  j  /   123  );  --   456
    Test_aff( "",  j  / (-123) );  --   456
    Test_aff( "",(-j) /   123  );
    Test_aff( "",(-j) / (-123) );

    Fill( i, Multi(100)*Multi(100)*10 + Multi(100) ); --   100100

    Test_aff( "i=100100",i );
    Test_aff( "  10010 ",i / 10 );    --   10010
    Test_aff( "  1001  ",i / 100 );   --   1001

    Fill( k, 123 );
    Fill( l, 456 );

    Test_aff( " j^5",             j*j*j*j*j );
    Test_aff( " j^5/k^5",        (j*j*j*j*j) /     (k*k*k*k*k) );
    Test_aff( "-j^5/k^5/l^4", (-(-j*j*j*j*j) /  (-(-k*k*k*k*k))) /
                                (-l*l*l*l) );

    Test_aff( "2^31 (32-bits)", Multi( Basic_Int'Last ) );
    Test_aff( "2^16 (32-bits)", Multi( 32768 ) );

    Test_aff( "-{:|:|:| 100| 0}",
                100 * shift
              - 200 * shift, True );

    Test_aff( "-{:|:|:| 666| 0| 0}",
                322 * (shift*shift)
              - 988 * (shift*shift), True );

    Test_aff( "0=.....", ((j-l*l)*2)-j*2+2*l**2);

    Test_aff( "shift(20)", shift ** 20, True );
    Test_aff( "1 - shift(6)", 1 - shift ** 6, True );
      -- -{:|:|:|:| 32767| 32767| 32767| 32767| 32767| 32767}

    -- Maintenant, les gros moyens...
    New_Line;
    Put_Line("-------- Tests LONGS -> patience! --------");
    New_Line;
    Put_Line("-- Test ""* n fois"" et ""** n""");

    Fill( j, -56088 ); -- =123*456. log_2(56088)=15.775405..
    Fill( k, -123 );
    Fill( l, +456 );

    -- j = k*l

    for puiss in 1..800 loop

      Fill( m, j );
      Fill( n, k );
      Fill( o, l );
      for i in 2 .. puiss loop -- on calcule les ** "betement"
      Mult(m, j, m); -- Version "directe"
      Fill(n, n*k );
      Fill(o, o*l );
      end loop;

      -- On doit avoir: m=j**i, n=k**i, o=l**i, m=n*o

      if puiss mod 10=0 then
        IIO.Put( puiss, 4 );
      end if;

      if not Equal( m, j**puiss ) then Put("err m"); end if;
      if not Equal( n, k**puiss ) then Put("err n"); end if;
      if not Equal( o, l**puiss ) then Put("err o"); end if;
      if not Equal( m, n*o ) then Put("err m=n*o"); end if;
    end loop;

    New_Line;
    Put_Line("-- Test /, **, Power  ");

    for puiss in 1 .. 800 loop
      if puiss mod 50=0 then IIO.Put( puiss, 4 ); end if;

      for ik in Basic_Int'(3)..10 loop
        for il in Basic_Int'(11)..23 loop

          Fill( k, ik );
          Fill( l, il );
          Multiply( k,l, j );   -- Equivalent to: Fill( j, k*l );

          Fill( m, j**puiss );
          Power( k, puiss, n ); -- 17-Feb-2002: test direct forms (procedures) !
          Power( l, puiss, o );

          if not (
            Test_egalite(  m/n, o, " m/n=o ") ) then
            Put("ik="); Put(Basic_Int'Image(ik)); New_Line;
            Put("il="); Put(Basic_Int'Image(il)); New_Line;
            Test_aff("m",m,True);
            Test_aff("n",n,True);
            Put("kl KO"); return;
          end if;
        end loop;
      end loop;

    end loop;

    New_Line;
    Put_Line("-- Test +, Add, *, Multiply, /, rem, Div_Rem, **, Power ");

    Fill( k, 127 );
    Fill( l, 23 );
    Multiply( k, l, j );  -- 17-Feb-2002: more direct than: Fill( j, k*l );
    -- j=2921, log_2(j)=11.512247

    for puiss in 500 .. 800 loop

      Power( j, puiss, m ); -- More direct than: Fill( m, j**puiss );
      Fill( n, k**puiss );
      Fill( o, l**puiss );
      Add( n, o, p);        -- More direct than: Fill( p, n+o );

      Div_Rem( m, n, m_sur_n, reste_m_sur_n);

      if puiss mod 50=0 or not (
        Test_egalite(  m_sur_n, o, " m/n=o ") and
        Test_egalite(  ( m_sur_n ) / (o/(-2)) , Multi( -2), " (m/n)/(o/(-2))=-2 ") and
        Test_egalite(  ( o/2 ) / (o/2) , Multi( 1), " (o/2)/(o/2)=1 ") and
        Test_egalite(  ( m_sur_n ) / o  , Multi( 1), " (m/n)/o=1 ") and
        Test_egalite( m , n*o , " m=n*o ") and
        Test_egalite( ((m-n*n-o)*2)-m*2+2*n**2+o+o , Multi(0) , " err3 ") and
        Test_egalite( m/(-m) , Multi(-1) , " err4 ") and
        Test_egalite( m , (m/p) * p + (m rem p) , " div et rem +/+") and
        Test_egalite( m , (m/(-p)) * (-p) + (m rem (-p)) , " div et rem +/-") and
        Test_egalite( (-m) , ((-m)/p) * p + ((-m) rem p) , " div et rem -/+") and
        Test_egalite( (-m) , ((-m)/(-p)) * (-p) + ((-m) rem (-p)) , " div et rem -/-")
      ) then

        Put( Integer'Image(puiss) );

      end if;
    end loop;
    New_Line;

    Pause;

    Test_aff( "m ( = j^800) vaut...", m );

    Pause;

    Put_Line("m=m  " & Boolean'Image( Equal (m, m) ));
    pragma Assert (Equal (m, m));
    Put_Line("m=j  " & Boolean'Image( Equal (m, j) ));
    pragma Assert (not Equal (m, j));
    Put_Line("j=m  " & Boolean'Image( Equal( j,m ) ));
    Put_Line("m<m  " & Boolean'Image( m<m ));
    Put_Line("j<m  " & Boolean'Image( j<m ));
    Put_Line("m>m  " & Boolean'Image( m>m ));
    Put_Line("j>m  " & Boolean'Image( j>m ));
    Put_Line("m<=m " & Boolean'Image( m<=m ));
    Put_Line("j<=m " & Boolean'Image( j<=m ));
    Put_Line("m<=j " & Boolean'Image( m<=j ));
    Put_Line("m>=m " & Boolean'Image( m>=m ));
    Put_Line("j>=m " & Boolean'Image( j>=m ));
    Put_Line("m>=j " & Boolean'Image( m>=j ));
    Put_Line("Odd(m)  " & Boolean'Image( Odd(m)  ));
    Put_Line("Even(m) " & Boolean'Image( Even(m) ));
    Put_Line("1234567890 < 1234567891  " & Boolean'Image(
             Val("1234567890")<Val("1234567891") ));
    Put_Line("1234567891 < 1234567890  " & Boolean'Image(
             Val("1234567891")<Val("1234567890") ));

  end Mix_Demo_Test;

  procedure Test_Limits is
    procedure Check_Integrity (i : Basic_Int) is
      m : Multi_Int (2);
    begin
      Fill (m, i);
      if verbose then
        Put ("In:  "); BIO.Put (i); New_Line;
        Put ("Out: "); Put (m); New_Line;
      end if;
      pragma Assert (Equal (m, i));
    end Check_Integrity;
  begin
    Check_Integrity (Basic_Int'Last);
    Check_Integrity (Basic_Int'Last - 1);
    Check_Integrity (Basic_Int'First + 1);
    Check_Integrity (Basic_Int'First);
  end Test_Limits;

begin
  Put_Line ("Test_Limits...");
  Test_Limits;
  Mix_Demo_Test;
end Test_Int;
