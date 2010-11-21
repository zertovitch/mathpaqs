-- Linear Algebra routines for Sparse package
--
--   This version of Ada body IS mapped to BLAS library

package body SparseB is

  -- identify floating point type

  is_single: constant boolean:= 
                real'digits = float'digits;

  is_double: constant boolean:=
                real'digits = long_float'digits;


  procedure scopy(n: natural; x: in  vector; incx: integer;
                              y: out vector; incy: integer);
  procedure dcopy(n: natural; x: in vector; incx: integer;
                              y: out vector; incy: integer);
  pragma Interface(FORTRAN, scopy);
  pragma Interface(FORTRAN, dcopy);

  function sdot(n: natural; x: in vector; incx: integer;
                            y: in vector; incy: integer) return real;
  function ddot(n: natural; x: in vector; incx: integer;
                            y: in vector; incy: integer) return real;
  pragma Interface(FORTRAN, sdot); 
  pragma Interface(FORTRAN, ddot);

  procedure saxpy(n: natural; a: real; x: in vector; incx: integer;
                  y: in out vector; incy: integer);
  procedure daxpy(n: natural; a: real; x: in vector; incx: integer;
                  y: in out vector; incy: integer);
  pragma Interface(FORTRAN, daxpy);
  pragma Interface(FORTRAN, saxpy);

  procedure sscal(n: natural; a: real; x: in out vector; incx: integer);
  procedure dscal(n: natural; a: real; x: in out vector; incx: integer);
  pragma Interface(FORTRAN, sscal);
  pragma Interface(FORTRAN, dscal);


  procedure Copy( u: in vector; v: out vector ) is
     begin
       if is_single then
         scopy(u'length, u,1,v,1);
       elsif is_double then
         dcopy(u'length, u,1,v,1);
       else
         v:= u;
       end if;
     end;

   function "*"(u,v: vector) return real is
     begin
       if is_single then
--put("sdot!");
         return sdot(u'length, u,1,v,1);
       elsif is_double then
--put("ddot!");
         return ddot(u'length, u,1,v,1); 
       else
         declare uv: real:= 0.0;
         begin
           for i in u'range loop
             uv:= uv + u(i)*v(i);
           end loop;
           return uv;
         end;
       end if;
     end;
                                             
  procedure Add_scaled( factor: real; u: in vector; v: in out vector ) is
     begin
       if is_single then
         saxpy(u'length, factor, u,1,v,1);
       elsif is_double then
         daxpy(u'length, factor, u,1,v,1);
       else
         for i in u'range loop
           v(i):= v(i) + factor * u(i);
         end loop;
       end if;
     end;
 
  procedure Scale( factor: real; u: in out vector ) is
     begin
       if is_single then
         sscal(u'length, factor, u,1);
       elsif is_double then
         dscal(u'length, factor, u,1);
       else
         for i in u'range loop
           u(i):= factor * u(i);
         end loop;
       end if;
     end;

  pragma inline("*", Add_scaled, Scale);

end SparseB;
