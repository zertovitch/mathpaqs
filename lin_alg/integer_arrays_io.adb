-- integer_arrays_io.adb    generic package body 

package body Integer_Arrays_IO is

  procedure PUT (A     : in INTEGER_MATRIX ;
                 WIDTH : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_WIDTH;
                 BASE  : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_BASE) is
  begin
    for I in A'RANGE ( 1 ) loop
      for J in A'RANGE ( 2 ) loop
        Ada.Text_IO.PUT ( "INTEGER_MATRIX( " ) ;
        Ada.Text_IO.PUT ( INTEGER'IMAGE ( I ) ) ;
        Ada.Text_IO.PUT ( " , " ) ;
        Ada.Text_IO.PUT ( INTEGER'IMAGE ( J ) ) ;
        Ada.Text_IO.PUT ( " ) = " ) ;
        INTEGER_TYPE_IO.PUT ( A( I , J ), WIDTH, BASE) ;
        Ada.Text_IO.NEW_LINE ;
      end loop ;
    end loop ;
  end PUT ;

  procedure PUT (V    : in INTEGER_VECTOR ;
                 WIDTH : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_WIDTH;
                 BASE  : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_BASE) is
  begin
    for I in V'RANGE loop
      Ada.Text_IO.PUT ( " INTEGER_VECTOR( " ) ;
      Ada.Text_IO.PUT ( INTEGER'IMAGE ( I ) ) ;
      Ada.Text_IO.PUT ( " ) = " ) ;
      INTEGER_TYPE_IO.PUT ( V( I ), WIDTH, BASE) ;
      Ada.Text_IO.NEW_LINE ;
    end loop ;
  end PUT ;

  procedure GET (A     : out INTEGER_MATRIX ;
                 WIDTH : in Ada.Text_IO.FIELD := 0) is
  begin
    for I in A'RANGE ( 1 ) loop
      for J in A'RANGE ( 2 ) loop
        INTEGER_TYPE_IO.GET ( A( I , J ), WIDTH) ;
      end loop ;
    end loop ;
  end GET ;


  procedure GET (V     : out INTEGER_VECTOR ;
                 WIDTH : in Ada.Text_IO.FIELD := 0) is
  begin
    for I in V'RANGE loop
      INTEGER_TYPE_IO.GET ( V( I ), WIDTH) ;
    end loop ;
  end GET ;




  procedure PUT (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 A     : in INTEGER_MATRIX ;
                 WIDTH : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_WIDTH;
                 BASE  : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_BASE) is
  begin
    for I in A'RANGE ( 1 ) loop
      for J in A'RANGE ( 2 ) loop
        INTEGER_TYPE_IO.PUT ( FILE, A( I , J ), WIDTH, BASE) ;
      end loop ;
    end loop ;
  end PUT ;

  procedure PUT (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 V    : in INTEGER_VECTOR ;
                 WIDTH : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_WIDTH;
                 BASE  : in Ada.Text_IO.FIELD := INTEGER_TYPE_IO.DEFAULT_BASE) is
  begin
    for I in V'RANGE loop
      INTEGER_TYPE_IO.PUT ( FILE, V( I ), WIDTH, BASE) ;
    end loop ;
  end PUT ;

  procedure GET (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 A     : out INTEGER_MATRIX ;
                 WIDTH : in Ada.Text_IO.FIELD := 0) is
  begin
    for I in A'RANGE ( 1 ) loop
      for J in A'RANGE ( 2 ) loop
        INTEGER_TYPE_IO.GET ( FILE, A( I , J ), WIDTH) ;
      end loop ;
    end loop ;
  end GET ;


  procedure GET (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 V     : out INTEGER_VECTOR ;
                 WIDTH : in Ada.Text_IO.FIELD := 0) is
  begin
    for I in V'RANGE loop
      INTEGER_TYPE_IO.GET ( FILE, V( I ), WIDTH) ;
    end loop ;
  end GET ;

end Integer_Arrays_IO;




