---------------------------------------------------------------------------
--  Fractals <<biomorphes>>
---------------------------------------------------------------------------
with Graph,
     Ada.Numerics.Complex_types,Ada.Numerics.Complex_elementary_functions;
use  Graph,
     Ada.Numerics.Complex_types,Ada.Numerics.Complex_elementary_functions;

procedure BioMorph is

  maxx: constant:= 1000; -- proxy for GetMaxX
  maxy: constant:= 1000;

  procedure Graphe_biomorphe_1(xa,ya,xb,yb: Float) is
    z: complex;
    c: constant complex:= (0.5,0.0);
    s: constant:= 8.0;
  begin
    Set_math_plane(xa,ya,xb,yb);
    for I in 0..maxx loop
    for J in 0..maxy loop
      Coord(I,J,z.re,z.im);
       for K in 1..10 loop
         Z:=Sin(Z)+Exp(Z)+c;
         exit when abs(Z) > s or abs(z.re) > s or abs(z.im) > s;
       end loop;
       if abs(z.re) >= s and abs(z.im) >= s then
         Point(i,j);
       end if;
    end loop;
    end loop;
  end Graphe_biomorphe_1;

  pala: constant:= 20;
  palz: constant:= 255;
  -- !! replace these palette indices with RGB !!

  procedure Graphe_biomorphe_2(xa,ya,xb,yb: Float) is -- <<Tache de vache>>
    z: complex;
    c: constant complex:= (0.5,0.0);
    s: constant:= 8.0;
    ko: Natural;
  begin
    Set_math_plane(xa,ya,xb,yb);
    for I in 0..maxx loop
    for J in 0..maxy loop
      Coord(I,J,z.re,z.im);
      for K in 1..(palz-pala) loop
        Z:=Sin(Z)+Exp(Z)+c;
        ko:= k;
        exit when abs(Z) > s or abs(z.re) > s or abs(z.im) > s;
      end loop;
      PutPixel(i,j,ko+pala); -- ! Couleur !
    end loop;
    end loop;
  end Graphe_biomorphe_2;

begin
  InitGraph(PostScript, "Biomorphe.ps");
  Graphe_biomorphe_1(-1.5,-1.5,-0.7,-0.7);
  CloseGraph;
end;
