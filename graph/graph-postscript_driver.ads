package Graph.PostScript_driver is

  -- Create / Close PS file stream
  procedure PS_Prolog;
  procedure PS_Close;

  -- Page feed
  procedure PS_Page;

  -- Graphics operations
  procedure PS_Dot (x, y: Integer);
  procedure PS_Line (x1, y1, x2,y2: Integer);
  procedure PS_Write (x, y: Integer; s:String);

  -- Set colour on PS (was named: Translate_RGB_PS)
  procedure Set_current_PS_Colour (c: gr_colour );

  procedure PS_Set_bitmap_pattern (p: FillPatternType; c: gr_colour );

  procedure PS_rectfill (x1, y1, w,h: Integer);

end Graph.PostScript_driver;
