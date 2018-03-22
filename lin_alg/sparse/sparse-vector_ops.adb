-- Linear Algebra routines for Sparse package
--
--   This version of Ada body is standalone, NOT mapped to BLAS library

-- Compile with an "unroll-loops" optimisation !

package body Sparse.Vector_Ops is

  -- Internal vector routines, interfaced with BLAS when use_BLAS=true

  procedure Copy( u: in vector; v: out vector ) is
  begin
    -- v:= u(v'range);      -- Variante (A) < 1-May-2001.

    for i in v'Range loop   -- Variante (B)
      v(i):= u(i);
    end loop;

    -- Hansbo problem 100x100x800, IBM Thinkpad 500 Mhz, GNAT 3.13p

    -- (A) >>>> STILS_TMQ1 : 5394 sec. =~  89 min.
    -- (B) >>>> STILS_TMQ1 : 2000 sec. =~  33 min.  --> 37%
  end Copy;

  function "*"(u,v: vector) return real is
    uv: real:= 0.0;
  begin
    for i in u'Range loop
      uv:= uv + u(i)*v(i);
    end loop;
    return uv;
  end "*";

  procedure Add_scaled( factor: real; u: in vector; v: in out vector ) is
  begin
    for i in u'Range loop
      v(i):= v(i) + factor * u(i);
    end loop;
  end Add_scaled;

  procedure Scale( factor: real; u: in out vector ) is
  begin
    for i in u'Range loop
      u(i):= factor * u(i);
    end loop;
  end Scale;

  pragma Inline("*", Add_scaled, Scale);

end Sparse.Vector_Ops;
