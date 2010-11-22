--  Returns date & time for a log, e.g. "2001/02/19   18:33:32"

with Calendar; use Calendar; -- Ada83 compatible
 
function Time_log return String is
  T     : Time:= Clock;
  x, sc : Natural;
  
begin
  x := Natural(Seconds(T));
  sc:= x MOD 60;
  x := x  /  60;

  declare
    -- + 100: trick for obtaining 0x
    sM : String:= Integer'image( Month(T) + 100);
    sD : String:= Integer'image(  Day(T)  + 100);
    ssc: String:= Integer'image( sc       + 100);
    smn: String:= Integer'image( x MOD 60 + 100);
    shr: String:= Integer'image( x  /  60 + 100);

  begin
    return
      Integer'image(Year(T))     & '/' &
      sM( sM'last-1 .. sM'last ) & '/' &
      sD( sD'last-1 .. sD'last ) & 
      "   " &
      shr( shr'last-1 .. shr'last ) & ':' &
      smn( smn'last-1 .. smn'last ) & ':' &
      ssc( ssc'last-1 .. ssc'last );
  end;

end Time_Log;