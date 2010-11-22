------------------------------------------------------------------------------
--  File:            GrStafon.ads
--  Description:     Extension of Graph package: standard fonts,
--                       all in one package
--
--  Fonts included:  See below!
--
--  Date/version:    12-Jul-2003
--  Author:          G. de Montmollin
------------------------------------------------------------------------------

with Graph.F.Bold, Graph.F.Complex,
     Graph.F.EuroStyle, Graph.F.Gothic,
     Graph.F.SansSerif, Graph.F.Script, Graph.F.Simple,
     Graph.F.Small, Graph.F.Triplex, Graph.F.TriScript,
     Graph.F.Helv, Graph.F.Helv_I, Graph.F.Helv_B, Graph.F.Helv_BI,
     Graph.F.Noble;

package Graph.Standard_fonts is

  BoldFont:       p_Vector_Font renames Graph.F.Bold.BoldFont;
  ComplexFont:    p_Vector_Font renames Graph.F.Complex.ComplexFont;
  EuroStyleFont:  p_Vector_Font renames Graph.F.EuroStyle.EuroStyleFont;
  GothicFont:     p_Vector_Font renames Graph.F.Gothic.GothicFont;
  SansSerifFont:  p_Vector_Font renames Graph.F.SansSerif.SansSerifFont;
  ScriptFont:     p_Vector_Font renames Graph.F.Script.ScriptFont;
  SimpleFont:     p_Vector_Font renames Graph.F.Simple.SimpleFont;
  SmallFont:      p_Vector_Font renames Graph.F.Small.SmallFont;
  TriplexFont:    p_Vector_Font renames Graph.F.Triplex.TriplexFont;
  TriScriptFont:  p_Vector_Font renames Graph.F.TriScript.TriScriptFont;
  HelvFont:       p_Vector_Font renames Graph.F.Helv.HelvFont;
  Helv_IFont:     p_Vector_Font renames Graph.F.Helv_I.Helv_IFont;
  Helv_BFont:     p_Vector_Font renames Graph.F.Helv_B.Helv_BFont;
  Helv_BIFont:    p_Vector_Font renames Graph.F.Helv_BI.Helv_BIFont;
  NobleFont:      p_Vector_Font renames Graph.F.Noble.NobleFont;

end Graph.Standard_fonts;
