with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Calendar;                      use Ada.Calendar;

with pi_digits, pi_digits_gmp;

procedure pi_digits_timing is
  t0a,t1a,t0g,t1g: Time;
  sa,sg: Float:= 0.0;
  runs: constant:= 100;
begin
  for run in 1..runs loop
    Put_Line("========= Run" & Integer'Image(run));
    t0g:= Clock;
    pi_digits_gmp;
    t1g:= Clock;
    t0a:= Clock;
    pi_digits;
    t1a:= Clock;
    Put_Line("Time in seconds (pure Ada): " & Duration'Image(t1a-t0a)); sa:= sa+Float(t1a-t0a);
    Put_Line("Time in seconds (GMP):      " & Duration'Image(t1g-t0g)); sg:= sg+Float(t1g-t0g);
    Put_Line("\--> factor: " & Duration'Image((t1a-t0a)/(t1g-t0g)));
  end loop;
  sa:=sa/Float(runs);
  sg:=sg/Float(runs);
  Put_Line("Avg time in seconds (pure Ada): " & Duration'Image(Duration(sa)));
  Put_Line("Avg time in seconds (GMP):      " & Duration'Image(Duration(sg)));
  Put_Line("\--> factor: " & Duration'Image(Duration(sa/sg)));
  Put("[press return]");
  Skip_Line;
end;
