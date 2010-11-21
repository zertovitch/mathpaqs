package body G_FEK is

  procedure l2shap (x         :        l2_vector;  -- x(i) = coordinate of node i
                    s         :        FEK_Float;  -- s = some local point
                    shape     :    out l2_vector;  -- shape fcn value at s
                    dadl,dadg : in out l2_grad;    -- local and global gradients
                    det       :    out FEK_Float;  -- jacobian of transformation
                    eps       :        FEK_Float) is -- 0 det checking criterion

  -- NOTE, GdM IV 2000: The original FEK handles the element
  -- in 2 dimensions with wrong determinant (a SQRT too much) and
  -- global derivatives!

  dxds: FEK_Float;

  begin
      shape(1):=  0.5 * (1.0 - s); -- shape functions
      shape(2):=  0.5 * (1.0 + s);
      dadl(1,1) := -0.5; -- local derivatives of shape functions
      dadl(2,1) :=  0.5;

      --  1x1 Jacobian = determinant = d/ds (x(1) + 0.5 * (1+s) * (x(2)-x(1))
      dxds:= 0.5 * (x(2)-x(1));
      det:= dxds;

      if abs(dxds) < eps then raise zero_jacobian; end if;
--
--    find global derivatives
--
      dadg(1,1):=  -0.5 / dxds; -- = -1/(x(2)-x(1))  OK
      dadg(2,1):=   0.5 / dxds; -- = +1/(x(2)-x(1))  OK

      if dxds < 0.0 then raise negative_jacobian; end if;

  end l2shap;

  procedure l2shap (s: FEK_Float; shape: out l2_vector) is
  begin
      shape(1):=  0.5 * (1.0 - s); -- shape functions
      shape(2):=  0.5 * (1.0 + s);
  end l2shap;

  procedure q4shap (x1,x2: q4_vector; s,t:FEK_Float;
                    shape: out q4_vector; dadl,dadg: in out q4_grad;
                    det: out FEK_Float;
                    calc_shape: Boolean; opt: shape_opt; eps: FEK_Float) is

--    usage
--    =====

--    quadrilateral element, in two dimensions.
--    linear shape functions.

--    purpose
--    -------

--    finds shape function local and global derivatives and jacobian
--    of transformation, at a point specified by the coordinates (s,t)
--    in the reference element.

--    input arguments
--    ---------------

--    x1       = vector of length 9 containing first coordinates
--               of element nodal points.
--    x2       = vector of length 9 containing second coordinates
--               of element nodal points.
--    s        = user specified first coordinate.
--    t        = user specified second coordinate.
--    eps      = criterion for checking zero determinant.

--    output arguments
--    ----------------

--    shape    = vector of length 4 containing shape functions.
--    dadl     = matrix of order (#free. deg.)x2 containing local derivatives.
--    dadg     = matrix of order (#free. deg.)x2 containing global derivatives.
--    det      = jacobian of transformation.

------------------------------------------------------------------------

  c,sp,sm,tp,tm,
  dxds,dxdt,dyds,dydt, dsdx,dsdy,dtdx,dtdy, determ: FEK_Float;
  ielt: constant:= 4;

  begin
      sp:= 1.0 + s;
      sm:= 1.0 - s;
      tp:= 1.0 + t;
      tm:= 1.0 - t;

      if calc_shape then
        shape(1):= 0.25*sm*tm;
        shape(2):= 0.25*sp*tm;
        shape(3):= 0.25*sp*tp;
        shape(4):= 0.25*sm*tp;
      end if;
--
--    local derivatives
--
      dadl(1,1):= -0.25*tm;
      dadl(2,1):= -dadl(1,1);
      dadl(3,1):=  0.25*tp;
      dadl(4,1):= -dadl(3,1);
--
      dadl(1,2):= -0.25*sm;
      dadl(2,2):= -0.25*sp;
      dadl(3,2):= -dadl(2,2);
      dadl(4,2):= -dadl(1,2);

      if opt < loc_der_jac then return; end if;

--
--       evaluate  the jacobian  matrix
--
      dxds:= 0.0;
      dxdt:= 0.0;
      dyds:= 0.0;
      dydt:= 0.0;
      for i in 1..ielt loop
        dxds:= dxds + dadl(i,1)*x1(i);
        dxdt:= dxdt + dadl(i,2)*x1(i);
        dyds:= dyds + dadl(i,1)*x2(i);
        dydt:= dydt + dadl(i,2)*x2(i);
      end loop;
--
--    evaluate determinant for element
--
      determ:= dxds*dydt - dxdt*dyds;
      det:= determ;
--
--      check  the determinant
--
      if abs(determ) < eps then raise zero_jacobian; end if;
--
--    find global derivatives
--
      if opt = loc_der_jac_glob_der then

        c:= 1.0/ determ;     --  invert the jacobian
        dsdx:= c*dydt;
        dsdy:=-c*dxdt;
        dtdx:=-c*dyds;
        dtdy:= c*dxds;
--
--    global derivatives
--
        for i in 1..ielt loop
           dadg(i,1):= dsdx*dadl(i,1) + dtdx*dadl(i,2);
           dadg(i,2):= dsdy*dadl(i,1) + dtdy*dadl(i,2);
        end loop;
      end if;
      if determ < 0.0 then raise negative_jacobian; end if;

  end q4shap;

  procedure q4shap (s,t: FEK_Float; shape: out q4_vector) is
    sp,sm,tp,tm: FEK_Float;
  begin
      sp:= 1.0 + s;
      sm:= 1.0 - s;
      tp:= 1.0 + t;
      tm:= 1.0 - t;
      shape(1):= 0.25*sm*tm;
      shape(2):= 0.25*sp*tm;
      shape(3):= 0.25*sp*tp;
      shape(4):= 0.25*sm*tp;
  end q4shap;

------------------------------------------------------------------------

  procedure q9shap (x1,x2: q9_vector; s,t:FEK_Float;
                    shape: out q9_vector; dadl,dadg: in out q9_grad;
                    det: out FEK_Float;
                    calc_shape: Boolean; opt: shape_opt; eps: FEK_Float) is

--    usage
--    =====

--    quadrilateral element, in two dimensions.
--    parabolic shape functions.

--    purpose
--    -------

--    finds shape function local and global derivatives and jacobian
--    of transformation, at a point specified by the coordinates (s,t)
--    in the reference element.

--    input arguments
--    ---------------

--    x1       = vector of length 9 containing first coordinates
--               of element nodal points.
--    x2       = vector of length 9 containing second coordinates
--               of element nodal points.
--    s        = user specified first coordinate.
--    t        = user specified second coordinate.
--    eps      = criterion for checking zero determinant.

--    output arguments
--    ----------------

--    shape    = vector of length 9 containing shape functions.
--    dadl     = matrix of order (#free. deg.)x2 containing local derivatives.
--    dadg     = matrix of order (#free. deg.)x2 containing global derivatives.
--    det      = jacobian of transformation.

------------------------------------------------------------------------

    c,sp,sm,tp,tm,st,ss,tt,ssp,ssm,ttp,ttm,
    dxds,dxdt,dyds,dydt, dsdx,dsdy,dtdx,dtdy, determ: FEK_Float;
    ielt: constant:= 9;

  begin
      sp:= 1.0 + s;
      sm:= 1.0 - s;
      tp:= 1.0 + t;
      tm:= 1.0 - t;
      st:= s*t;
      ss:= 1.0 - s*s;
      tt:= 1.0 - t*t;

      if calc_shape then
        shape(1):= 0.25*sm*tm*st;
        shape(2):=-0.25*sp*tm*st;
        shape(3):= 0.25*sp*tp*st;
        shape(4):=-0.25*sm*tp*st;
        shape(5):=-0.50*ss*tm*t;
        shape(6):= 0.50*tt*sp*s;
        shape(7):= 0.50*ss*tp*t;
        shape(8):=-0.50*tt*sm*s;
        shape(9):=     ss*tt;
      end if;
--
--    local derivatives
--
      ssp:= 1.0 + 2.0*s;
      ssm:= 1.0 - 2.0*s;
      ttp:= 1.0 + 2.0*t;
      ttm:= 1.0 - 2.0*t;
      dadl(1,1):= 0.25*ssm*tm*t;
      dadl(2,1):=-0.25*ssp*tm*t;
      dadl(3,1):= 0.25*ssp*tp*t;
      dadl(4,1):=-0.25*ssm*tp*t;
      dadl(5,1):=  st*tm;
      dadl(6,1):= 0.50*ssp*tt;
      dadl(7,1):=- st*tp;
      dadl(8,1):=-0.50*ssm*tt;
      dadl(9,1):=- 2.0*tt *s;
--
      dadl(1,2):= 0.25*s*sm*ttm;
      dadl(2,2):=-0.25*s*sp*ttm;
      dadl(3,2):= 0.25*s*sp*ttp;
      dadl(4,2):=-0.25*s*sm*ttp;
      dadl(5,2):=-0.50*ss*ttm;
      dadl(6,2):=- st*sp;
      dadl(7,2):= 0.50*ss*ttp;
      dadl(8,2):=  st*sm;
      dadl(9,2):=- 2.0*ss*t;
      if opt < loc_der_jac then return; end if;
--
--       evaluate  the jacobian  matrix
--
      dxds:= 0.0;
      dxdt:= 0.0;
      dyds:= 0.0;
      dydt:= 0.0;
      for i in 1..ielt loop
        dxds:= dxds + dadl(i,1)*x1(i);
        dxdt:= dxdt + dadl(i,2)*x1(i);
        dyds:= dyds + dadl(i,1)*x2(i);
        dydt:= dydt + dadl(i,2)*x2(i);
      end loop;
--
--    evaluate determinant for element
--
      determ:= dxds*dydt - dxdt*dyds;
      det:= determ;
--
--      check  the determinant
--
      if abs(determ) < eps then raise zero_jacobian; end if;
--
--    find global derivatives
--
      if opt = loc_der_jac_glob_der then

        c:= 1.0/ determ;     --  invert the jacobian
        dsdx:= c*dydt;
        dsdy:=-c*dxdt;
        dtdx:=-c*dyds;
        dtdy:= c*dxds;
--
--    global derivatives
--
        for i in 1..ielt loop
           dadg(i,1):= dsdx*dadl(i,1) + dtdx*dadl(i,2);
           dadg(i,2):= dsdy*dadl(i,1) + dtdy*dadl(i,2);
        end loop;
      end if;
      if determ < 0.0 then raise negative_jacobian; end if;
    end q9shap;

  procedure q9shap (s,t:FEK_Float; shape: out q9_vector) is
    sp,sm,tp,tm,st,ss,tt: FEK_Float;
  begin
      sp:= 1.0 + s;
      sm:= 1.0 - s;
      tp:= 1.0 + t;
      tm:= 1.0 - t;
      st:= s*t;
      ss:= 1.0 - s*s;
      tt:= 1.0 - t*t;

      shape(1):=  0.25*sm*tm*st;
      shape(2):= -0.25*sp*tm*st;
      shape(3):=  0.25*sp*tp*st;
      shape(4):= -0.25*sm*tp*st;
      shape(5):= -0.50*ss*tm*t;
      shape(6):=  0.50*tt*sp*s;
      shape(7):=  0.50*ss*tp*t;
      shape(8):= -0.50*tt*sm*s;
      shape(9):=       ss*tt;

  end q9shap;

------------------------------------------------------------------------

  procedure b27shp (x1,x2,x3: b27_vector; r,s,t: FEK_Float;
                    shape: out b27_vector; dadl,dadg: in out b27_grad;
                    det: out FEK_Float;
                    calc_shape: Boolean; opt: shape_opt; eps: FEK_Float) is

--    usage
--    =====

--    quadrilateral element, in three dimensions.
--    parabolic shape functions.

--    purpose
--    -------

--    finds shape function local and global derivatives and jacobian
--    of transformation, at a point specified by the coordinates (r,s,t)
--    in the reference element.

--    input arguments
--    ---------------

--    x1       = vector of length 27 containing first coordinates
--               of element nodal points.
--    x2       = vector of length 27 containing second coordinates
--               of element nodal points.
--    x3       = vector of length 27 containing third coordinates
--               of element nodal points.
--    r        = user specified first coordinate.
--    s        = user specified second coordinate.
--    t        = user specified third coordinate.
--    iopt     = option paramater (see note 1).
--    eps      = criterion for checking zero determinant.

--    output arguments
--    ----------------

--    shape    = vector of length 27 containing shape functions.
--    dadl     = matrix of order (#free. deg.)x3 containing local derivatives.
--    dadg     = matrix of order (#free. deg.)x3 containing global derivatives.
--    det      = jacobian of transformation.

------------------------------------------------------------------------

    u,v,w,du,dv,dw: vector(1..3); a,b: matrix(1..3,1..3);
    ip,j,k: Positive; c,c1,c2,c3, determ: FEK_Float;
    ielt: constant:= 27;

  begin
      u:= ( (r - 1.0)*r*0.5, 1.0 - r*r, (1.0 + r)*r*0.5 );
      v:= ( (s - 1.0)*s*0.5, 1.0 - s*s, (1.0 + s)*s*0.5 );
      w:= ( (t - 1.0)*t*0.5, 1.0 - t*t, (1.0 + t)*t*0.5 );

      if calc_shape then
        for k in 1..3 loop
          for j in 1..3 loop
            for i in 1..3 loop
               shape( permu27(i,j,k) ) := u(i)*v(j)*w(k);
            end loop;
          end loop;
        end loop;
      end if;
--
--    local derivatives
--
      du:= (r -0.5, -r*2.0, r +0.5);
      dv:= (s -0.5, -s*2.0, s +0.5);
      dw:= (t -0.5, -t*2.0, t +0.5);

      for i in 1..3 loop
        for j in 1..3 loop
          for k in 1..3 loop
             ip:= permu27(i,j,k);
             dadl(ip,1):= du(i)  *   v(j)  *  w(k);
             dadl(ip,2):=  u(i)  *  dv(j)  *  w(k);
             dadl(ip,3):=  u(i)  *   v(j)  * dw(k);
          end loop;
        end loop;
      end loop;
      if opt < loc_der_jac then return; end if;
--
--    jacobian matrix a
--
      for i in 1..3 loop
        c1:= 0.0;
        c2:= 0.0;
        c3:= 0.0;
        for j in 1..ielt loop
          c1:= c1 + dadl(j,i)*x1(j);
          c2:= c2 + dadl(j,i)*x2(j);
          c3:= c3 + dadl(j,i)*x3(j);
        end loop;
        a(i,1):= c1;
        a(i,2):= c2;
        a(i,3):= c3;
      end loop;
--
--    invert jacobian
--
      for i in 1..3 loop
        j:= i + 1; if j=4 then j:= 1; end if;
        k:= j + 1; if k=4 then k:= 1; end if;
        b(i,i):= a(j,j)*a(k,k) - a(k,j)*a(j,k);
        b(i,j):= a(k,j)*a(i,k) - a(i,j)*a(k,k);
        b(j,i):= a(j,k)*a(k,i) - a(j,i)*a(k,k);
      end loop;
--
--    find determinant of jacobian matrix.
--
      determ:= a(1,1)*b(1,1) + a(1,2)*b(2,1) + a(1,3)*b(3,1);
      det:= determ;
--
--    check determinant of jacobian
--
      if abs(determ) < eps then raise zero_jacobian; end if;
--
--    find global derivatives
--
      if opt = loc_der_jac_glob_der then
        for i in 1..3 loop
          for j in 1..ielt loop
            c:= b(i,1)*dadl(j,1) + b(i,2)*dadl(j,2) + b(i,3)*dadl(j,3);
            dadg(j,i):= c/determ;
          end loop;
        end loop;
      end if;
      if determ < 0.0 then raise negative_jacobian; end if;
    end b27shp;

-- 31.X.1998: shape functions only

  procedure b27shp (r,s,t: FEK_Float; shape: out b27_vector) is
    u,v,w: vector(1..3);
  begin
      u:= ( (r - 1.0)*r*0.5, 1.0 - r*r, (1.0 + r)*r*0.5 );
      v:= ( (s - 1.0)*s*0.5, 1.0 - s*s, (1.0 + s)*s*0.5 );
      w:= ( (t - 1.0)*t*0.5, 1.0 - t*t, (1.0 + t)*t*0.5 );

      for k in 1..3 loop
        for j in 1..3 loop
          for i in 1..3 loop
            shape( permu27(i,j,k) ) := u(i)*v(j)*w(k);
          end loop;
        end loop;
      end loop;

  end b27shp;

end G_FEK;

-- pragma Suppress_All;
