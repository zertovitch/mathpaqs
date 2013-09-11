package body Estimators is

  --------------------------
  -- Linear_least_squares --
  --------------------------

  procedure Linear_least_squares (x, y: Data_vector; a, b: out Real) is
    sx, sy, sxy, sxx: Real:= 0.0;
    n, j: Natural;
    rn: Real;
  begin
    if x'Length /= y'Length then
      raise Constraint_Error;
    end if;
    n:= x'Length;
    rn:= Real(n);
    if n = 0 then
      a:= 0.0;
      b:= 0.0;
      return;
    end if;
    for i in x'Range loop
      j:= i - x'First + y'First;
      sx:= sx + x(i);
      sy:= sy + y(j);
      sxx:= sxx + x(i) * x(i);
      sxy:= sxy + x(i) * y(j);
    end loop;
    b:= (rn * sxy - sx * sy) / (rn * sxx - sx * sx);
    a:= (sy - b * sx) / rn;
  end Linear_least_squares;

  ------------------------------------------
  -- Linear_least_squares x = (1,2,3,...) --
  ------------------------------------------

  procedure Linear_least_squares (y: Data_vector; a, b: out Real) is
    x: Data_vector(y'Range);
    c: Positive:= 1;
  begin
    for i in x'Range loop
      x(i):= Real(c);
      c:= c + 1;
    end loop;
    Linear_least_squares(x,y,a,b);
  end Linear_least_squares;

end Estimators;
