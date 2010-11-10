-- `Le moment est solennel pour vous; vous participez au c\'el\`ebre jeu`
-- `t\'el\'evis\'e \og Tiroir, mon beau tiroir, qu'est-ce ? \fg.`

-- `L'animateur-vedette Patrick Sandentier vous pr\'esente trois tiroirs`
-- `ferm\'es. L'un d'eux contient le titre de propri\'et\'e de la Porsche 911`
-- `de vos r\^eves, les deux autres sont vides. Au hasard, vous d\'esignez un`
-- `des tiroirs, sans l'ouvrir. L'animateur (qui sait, lui, quel est le bon`
-- `tiroir) vous soumet alors \`a une petite \'epreuve cruelle: il ouvre un`
-- `des deux autres tiroirs, vous montre qu'il est bien vide, et vous demande`
-- `si vous d\'esirez modifier votre choix.`

-- `Que r\'epondez-vous?`

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;

procedure Porscherie is

  G: Generator;

  function Pos_random( p: positive ) return positive is
  begin
    return 1 + Integer( Random(G) * Float(p) ) mod p;
  end;

  function Pile_Face return boolean is
  begin
    return Pos_random( 2 ) = 1;
  end;

  type strategie is ( garder, tirer_au_sort, changer );

  function Gagne_jeu_Porsche( s: strategie ) return boolean is
    choix_1, porsche: positive;
  begin
    porsche:= Pos_random(3);   -- _Le_ tiroir a la Porsche
    choix_1:= Pos_random(3);   --  1er choix du joueur

    if choix_1 = porsche then
    
      -- Il est tombe sur la Porsche (1er choix)
      -- ---> Sandentier retire un des deux tiroirs vides
    
      case s is
        when garder        => return True;      -- Il garde la Porsche
        when tirer_au_sort => return Pile_Face; -- C'est comme pile ou face
        when changer       => return False;     -- Il perd la Porsche
      end case;

    else

      -- Il n'est pas tombe sur la Porsche (1er choix)
      -- ---> Sandentier retire l'autre tiroir vide

      case s is
        when garder        => return False;     -- Il ne gagne pas la Porsche
        when tirer_au_sort => return Pile_Face; -- C'est comme pile ou face
        when changer       => return True;      -- Il gagne la Porsche
      end case;
      
    end if;
  end;

  essais: natural:= 10;
  gagnes: natural;

begin
  Reset(G);
  
  boucle_nb_essais: for log10 in 1..6 loop
  
    boucle_strategies: for s in strategie loop
    
      gagnes:= 0;
  
      for e in 1..essais loop
       
        if Gagne_jeu_Porsche( s ) then
          gagnes:= gagnes + 1;
        end if;
        
      end loop;
      
      Put( gagnes ); Put(" / "); Put( essais );
      Put( 100.0*float(gagnes)/float(essais), 8,2,0 );
      Put_Line( " %    " & strategie'image(s) );
      
    end loop boucle_strategies;
    
    New_Line;

    essais:= essais * 10;

  end loop boucle_nb_essais;

end Porscherie;
