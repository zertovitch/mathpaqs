-- real_arrays_io.adb    generic package body 

package body Real_Arrays_IO is

  procedure GET (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 ITEM  : out Real_Arrays.REAL_VECTOR;
                 WIDTH : in Ada.Text_IO.FIELD := 0) is
  begin
    for I in ITEM'RANGE loop
      REAL_IO.GET ( FILE, ITEM( I ), WIDTH) ;
    end loop ;
  end GET ;
 
  procedure GET (ITEM  : out Real_Arrays.REAL_VECTOR ;
                 WIDTH : in Ada.Text_IO.FIELD := 0) is
  begin
    for I in ITEM'RANGE loop
      REAL_IO.GET ( ITEM( I ), WIDTH) ;
    end loop ;
  end GET ;

  procedure PUT (FILE : in  Ada.Text_IO.FILE_TYPE;
                 ITEM : in Real_Arrays.REAL_VECTOR;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP) is
  begin
    for I in ITEM'RANGE loop
      --Ada.Text_IO.PUT ( FILE, "REAL_VECTOR( " ) ;
      --Ada.Text_IO.PUT ( FILE, INTEGER'IMAGE ( I ) ) ;
      --Ada.Text_IO.PUT ( FILE, " ) = " ) ;
      REAL_IO.PUT ( FILE, ITEM( I ), FORE, AFT, EXP) ;
      --Ada.Text_IO.NEW_LINE(FILE) ;
    end loop ;
  end PUT ;

  procedure PUT (ITEM : in Real_Arrays.Real_Vector ;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP) is
  begin
    for I in ITEM'RANGE loop
      Ada.Text_IO.PUT ( "    Real_Vector (" & I'Image & " ) = " ) ;
      REAL_IO.PUT ( ITEM( I ), FORE, AFT, EXP) ;
      Ada.Text_IO.NEW_LINE ;
    end loop ;
  end PUT ;

  procedure GET (FILE  : in  Ada.Text_IO.FILE_TYPE;
                 ITEM  : out Real_Arrays.REAL_MATRIX;
                 WIDTH : in Ada.Text_IO.FIELD := 0) is
  begin
    for I in ITEM'RANGE ( 1 ) loop
      for J in ITEM'RANGE ( 2 ) loop
        REAL_IO.GET ( FILE, ITEM( I , J ), WIDTH) ;
      end loop ;
    end loop ;
  end GET ;
 
  procedure GET (ITEM  : out Real_Arrays.REAL_MATRIX ;
                 WIDTH : in Ada.Text_IO.FIELD := 0) is
  begin
    for I in ITEM'RANGE ( 1 ) loop
      for J in ITEM'RANGE ( 2 ) loop
        REAL_IO.GET ( ITEM( I , J ), WIDTH) ;
      end loop ;
    end loop ;
  end GET ;

  procedure PUT (FILE : in  Ada.Text_IO.FILE_TYPE;
                 ITEM : in Real_Arrays.REAL_MATRIX;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP) is
  begin
    for I in ITEM'RANGE ( 1 ) loop
      for J in ITEM'RANGE ( 2 ) loop
        --Ada.Text_IO.PUT ( FILE, "REAL_MATRIX( " ) ;
        --Ada.Text_IO.PUT ( FILE, INTEGER'IMAGE ( I ) ) ;
        --Ada.Text_IO.PUT ( FILE, " , " ) ;
        --Ada.Text_IO.PUT ( FILE, INTEGER'IMAGE ( J ) ) ;
        --Ada.Text_IO.PUT ( FILE, " ) = " ) ;
        REAL_IO.PUT ( FILE, ITEM( I , J ), FORE, AFT, EXP) ;
        --Ada.Text_IO.NEW_LINE(FILE) ;
      end loop ;
    end loop ;
  end PUT ;


  procedure PUT (ITEM : in Real_Arrays.REAL_MATRIX ;
                 FORE : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_FORE;
                 AFT  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_AFT;
                 EXP  : in Ada.Text_IO.FIELD := REAL_IO.DEFAULT_EXP) is
  begin
    for I in ITEM'RANGE ( 1 ) loop
      for J in ITEM'RANGE ( 2 ) loop
        Ada.Text_IO.PUT ( "REAL_MATRIX( " ) ;
        Ada.Text_IO.PUT ( INTEGER'IMAGE ( I ) ) ;
        Ada.Text_IO.PUT ( " , " ) ;
        Ada.Text_IO.PUT ( INTEGER'IMAGE ( J ) ) ;
        Ada.Text_IO.PUT ( " ) = " ) ;
        REAL_IO.PUT ( ITEM( I , J ), FORE, AFT, EXP) ;
        Ada.Text_IO.NEW_LINE ;
      end loop ;
    end loop ;
  end PUT ;

end Real_Arrays_IO;
