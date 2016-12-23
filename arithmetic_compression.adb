--
--  Hello Thomas,
--
--  On 26.06.94 you wrote in area PASCAL to subject "Arithmetic compression":
--  TW> But where can we get a description of this compression method ??
--  Michael  Barnsley, Lyman Hurd, "Fractal Image Compression", AK Peters,
--  1993
--  Mark Nelson, "The Data Compression Book", M&T Books, 1991
--  Ian  Witten,  Radford  Neal,  John Cleary, "Arithmetic Coding for Data
--  Compression", CACM, Vol. 30, No.6, 1987
--
--  Below  is a small source from the 1st book, translated into Pascal and
--  adopted  to  work  on  the uppercase alphabet to demonstrate the basic
--  principles.
--  For  a  simple  explanation, the program uses the letters of the input
--  String  to "drive" the starting point through the real interval 0.0 ..
--  1.0
--  By  this process, every possible input String stops at a unique point,
--  that  is:  a  point  (better: a small interval section) represents the
--  whole  String.  To  _decode_  it, you have to reverse the process: you
--  start  at  the  given  end point and apply the reverse transformation,
--  noting  which intervals you are touching at your voyage throughout the
--  computation.
--  Due  to the restricted arithmetic resolution of any computer language,
--  the  max.  length of a String will be restricted, too (try it out with
--  TYPE   REAL=EXTENDED,  for  example);  this  happens  when  the  value
--  "underflows" the computers precision.

-- Translated by (New) P2Ada v. 15-Nov-2006

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;

procedure  Arithmetic_Compression is
  type Real is digits 18;
  package RIO is new Float_IO(Real); use RIO;

  char_set: constant String:= "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";

  p: constant array(char_set'Range) of Real:=  --  found empirically
    (
      6.1858296469E-02,
      1.1055412402E-02,
      2.6991022453E-02,
      2.6030374520E-02,
      9.2418577127E-02,
      2.1864028512E-02,
      1.4977615842E-02,
      2.8410764564E-02,
      5.5247871050E-02,
      1.3985123226E-03,
      3.8001321554E-03,
      3.2593032914E-02,
      2.1919756707E-02,
      5.2434924064E-02,
      5.7837905257E-02,
      2.0364674693E-02,
      1.0031075103E-03,
      4.9730779744E-02,
      4.8056280170E-02,
      7.2072478498E-02,
      2.0948493879E-02,
      8.2477728625E-03,
      1.0299101184E-02,
      4.7873173243E-03,
      1.3613601926E-02,
      2.7067980437E-03,
      2.3933136781E-01
    );
  psum: array(char_set'Range) of Real;

  function Encode( s: in String) return Real is
    po: Integer;
    offset,len: Real;
  begin
    offset:= 0.0;
    len:= 1.0;
    for i in s'Range loop
      po:= 0;
      for c in char_set'range loop
        if char_set(c)=s(i) then
          po:= c;
          exit;
        end if;
      end loop;
      if po/=0 then
        offset:= offset+ len * psum(po);
        len:= len * p(po);
      else
        Put("only input chars "); Put(char_set); Put(" allowed!"); New_Line;
        raise Constraint_Error;
      end if;
    end loop;
    return offset + len * 0.5;
  end Encode;

  function Decode(x0:Real; n:Integer) return String is
    j: Integer;
    s: String(1..n);
    x: Real:= x0;
  begin
    if x0 < 0.0 or x0 > 1.0 then
      Put("must lie in the range [0..1]"); New_Line;
      raise Constraint_Error;
    end if;
    for i in 1 .. n loop
      j:= char_set'Last;
      while x < psum(j) loop
        j:= j - 1;
      end loop;
      s(i):= char_set(j);
      x:= x-psum(j);
      x:= x/p(j);
    end loop;
    return s;
  end Decode;

  inp: constant String := "ARITHMETIC CODE";
  r: Real;

begin
  for i in psum'Range loop
    psum(i):=0.0;
    for j in 1 .. i-1 loop
      psum(i):= psum(i)+p(j);
    end loop;
  end loop;

  Put("Digits: "); Put(Real'Digits,0); New_Line;
  Put("Length of message : "); Put(inp'Length,0); New_Line;
  Put("Length of code set: "); Put(char_set'Length,0); New_Line;
  Put_Line("encoding String    : [" & inp & ']');
  r:= Encode(inp);
  Put("String is encoded by "); Put(r); New_Line;
  Put_Line("decoding of r gives: [" & Decode(r, inp'Length) & ']');

end Arithmetic_Compression;
