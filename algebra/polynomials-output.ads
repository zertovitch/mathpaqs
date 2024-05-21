------------------------------------------------------------------------------
--  File:            polyoutp.ads
--  Description:     Supplement of package 'Polynomials': outputs
--  Date/version:    1-Feb-2005; 22.12.1996
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Text_IO;

generic

  with procedure Field_Put (File : in Ada.Text_IO.File_Type; Item : in Field_elt);
  with function ">" (a, b : Field_elt) return Boolean;

package Polynomials.Output is

  procedure Put (File : in Ada.Text_IO.File_Type; Item : in Polynomial; Var : in String);
  procedure Put (Item : in Polynomial; Var : in String);

end Polynomials.Output;
