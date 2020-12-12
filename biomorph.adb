---------------------------------------------------------------------------
--  Fractals <<biomorphes>>
---------------------------------------------------------------------------
with Graph,
     Ada.Numerics.Complex_Types,Ada.Numerics.Complex_Elementary_Functions;
use  Graph,
     Ada.Numerics.Complex_Types,Ada.Numerics.Complex_Elementary_Functions;

procedure BioMorph is

  maxx: constant:= 1000; -- proxy for GetMaxX
  maxy: constant:= 1000;

  procedure Graphe_biomorphe_1(xa,ya,xb,yb: Float) is
    z: Complex;
    c: constant Complex:= (0.5,0.0);
    s: constant:= 8.0;
  begin
    Set_math_plane(xa,ya,xb,yb);
    for I in 0..maxx loop
      for J in 0..maxy loop
        Coord(I,J,z.Re,z.Im);
        for K in 1..10 loop
          z:=Sin(z)+Exp(z)+c;
          exit when abs(z) > s or abs(z.Re) > s or abs(z.Im) > s;
        end loop;
        if abs(z.Re) >= s and abs(z.Im) >= s then
          Point(I,J);
        end if;
      end loop;
    end loop;
  end Graphe_biomorphe_1;

  pala: constant:= 20;
  palz: constant:= 255;
  -- !! replace these palette indices with RGB !!

  procedure Graphe_biomorphe_2(xa,ya,xb,yb: Float) is -- <<Tache de vache>>
    z: Complex;
    c: constant Complex:= (0.5,0.0);
    s: constant:= 8.0;
    ko: Natural;
  begin
    Set_math_plane(xa,ya,xb,yb);
    for I in 0..maxx loop
      for J in 0..maxy loop
        Coord(I,J,z.Re,z.Im);
        for K in 1..(palz-pala) loop
          z:=Sin(z)+Exp(z)+c;
          ko:= K;
          exit when abs(z) > s or abs(z.Re) > s or abs(z.Im) > s;
        end loop;
        PutPixel(I,J,ko+pala); -- ! Couleur !
      end loop;
    end loop;
  end Graphe_biomorphe_2;
  pragma Unreferenced (Graphe_biomorphe_2);

begin
  InitGraph(PostScript, "Biomorphe.ps");
  Graphe_biomorphe_1(-1.5,-1.5,-0.7,-0.7);
  CloseGraph;
end BioMorph;
