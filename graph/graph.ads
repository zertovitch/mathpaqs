------------------------------------------------------------------------------
--  File: Graph.ads         (possibly from DOSGraph, WinGraph or OSIGraph.zip)
------------------------------------------------------------------------------
--   Description:
--
--   Graphics package for PC - like Turbo/Borland Pascal Graph unit,
--     + math plane coordinates for scientific drawing





--     + integrated file/printer device drivers:
--         PostScript (PS), Encapsulated PostScript (EPSF), AutoCAD (DXF)

--     + integrated vectorial fonts which can be displayed in any direction
--     + simultaneous multiple device drawing
--     + exceptions handling in place of tracking GraphResult variable
--     + no need of external files for drivers and fonts
--





-- Note: this is the *OS-independent* version

--       this file generated from Graph.prs by "gnatprep", see prep_src.bat
--
--  Date/version:    16-Feb-2007
--  Author:          Gautier de Montmollin
--
--  Copyright (c) Gautier de Montmollin 1999-2007
--  CH-8810 Horgen
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
--
--  Revisions:  Robert B. Sutton
--  V1.01   11/24/99    Reformatted file for 80 column printing.
--                      Tab value set to 4
--  V1.02   11/25/99    Added references to Turbo / Borland Pascal
--                      and comments.
--  V1.03   11/25/99    Updated by edit, to GDM vers. 31.I.1999
--                      Minor corrections
--
--
--  N.B.: The drawing routines use
--          > in the math plane coordinates, the type *float*
--          > in pixel coordinates, the type *integer*
--        so, no confusion is possible - and one can mix both methods
--
--  Reference Books:    Page references are supplied to the appropriate
--                      reference manuals for Turbo Pascal 5.0 (TP5),
--                      and Borland Pascal 7.0 (BP7).
------------------------------------------------------------------------------
with Interfaces;          use Interfaces;
with Ada.Text_IO;

package Graph is

-- * Devices
  type device_type is (





    PostScript, AutoCAD_DXF);
     --, Bitmap_BMP, IBM_graphics_printer);
     
  current_device: device_type:=  -- this var. is changed *only* by user





    PostScript;


-- * Mode constants for devices (mode=1 is the default mode)
  default_mode:constant:= 1;
  key_for_current_mode:constant:= 0; -- just for calling InitGraph














-- Postscript modes
  PS_10_pt :            constant:= 1; 
  PS_12_pt :            constant:= 2; 
  PS_half  :            constant:= 3;
  PS_landscape_10_pt :  constant:= 4;
  PS_landscape_12_pt :  constant:= 5;
  PS_landscape_half:    constant:= 6;
  subtype Postscript_modes is natural range 1..6;

  EPSF : Boolean := FALSE;
--  True:   encapsulated Postscript, 1 page mode, for document inclusion
--  False:  normal Postscript, multi-page mode, for sending to printer

-- * Coordinates of points, arcs and polygons in pixel
  type PointType is record X,Y:integer; end record;
  type ArcCoordsType is record X,Y,Xstart,Ystart,Xend,Yend:integer; end record;
  type polygon_type is array(integer range<>) of PointType;
  type ViewPortType is record x1, y1, x2, y2:integer; clip:boolean; end record;

-- * Coordinates of points, polygons in math plane
  type Math_PointType is record X,Y:float; end record;
  type vector2 is new math_PointType;
  type math_polygon_type is array(integer range<>) of Math_PointType;

-- * Colours
  subtype gr_colour is natural;
  
  Black    : constant gr_colour:= 0; DarkGray    : constant gr_colour:=  8;
  Blue     : constant gr_colour:= 1; LightBlue   : constant gr_colour:=  9;
  Green    : constant gr_colour:= 2; LightGreen  : constant gr_colour:= 10;
  Cyan     : constant gr_colour:= 3; LightCyan   : constant gr_colour:= 11;
  Red      : constant gr_colour:= 4; LightRed    : constant gr_colour:= 12;
  Magenta  : constant gr_colour:= 5; LightMagenta: constant gr_colour:= 13;
  Brown    : constant gr_colour:= 6; Yellow      : constant gr_colour:= 14;
  LightGray: constant gr_colour:= 7; White       : constant gr_colour:= 15;

  -- It is a good idea to keep the 16 first colours as above in a palette

  -- For PostScript, we code 0..15 : standard,
  -- or 100 + R*1_00_00 + G*1_00 + B : RGB in 0..99 scale (3-Feb-2001)

  -- Variant: directly use Red-Green-Blue values with SetColor (28-Apr-2002):

  subtype Float_0_1 is Float range 0.0..1.0;

-- * Text display settings & vectorial fonts (22.I.1998):

  -- See routines:
  --   OutText, OutTextXY, TextHeight, TextWidth
  --   SetTextJustify, SetTextStyle, GetTextSettings, SetTextSettings,
  --   SetUserCharSize, SetUserCharDirection

  -- Vectorial font stuff made private (26-Apr-2002)
  type Vector_font(first_char,last_char: Character; n_vertices: Natural) is private;
  type p_Vector_font is access all Vector_font;
  DefaultFont: constant p_Vector_font:= null;  -- the default, bitmap font

  function Name (font : Vector_font) return String;

  type t_direction is (HorizDir, VertDir, AnyDir);
  -- HorizDir: left to right (angle 0)
  -- VertDir: bottom to top  (angle pi/2)
  -- AnyDir: user-set angle

  UserCharSize: constant:= 0; -- key for user defined char size

  type t_hori_justify is (LeftText, CenterText, RightText);
  type t_vert_justify is (BottomText, CenterText, TopText);

  type TextSettingsType is record
    Font      : p_vector_font := DefaultFont;
    Direction : t_direction   := HorizDir;
    CharSize  : Natural       := 1;         -- 0: user defined; >0: standard
    Horiz     : t_hori_justify:= LeftText;
    Vert      : t_vert_justify:= TopText;
    -- -- -- -- -- Added to TP's Graph settings (26-Apr-2002):
    Bold      : Boolean:= False;
    Slanted   : Boolean:= False;
    Underlined: Boolean:= False;
  end record;

  -- 26-Apr-2002:

  type FillStyle is (
     EmptyFill      ,   --  0   | Background colour
     SolidFill      ,   --  1   | Line colour
     LineFill       ,   --  2   | ---
     LtSlashFill    ,   --  3   | ///
     SlashFill      ,   --  4   | /// bold
     BkSlashFill    ,   --  5   | \\\ bold
     LtBkSlashFill  ,   --  6   | \\\
     HatchFill      ,   --  7   | Fine Hatch
     XHatchFill     ,   --  8   | Bold Hatch
     InterleaveFill ,   --  9   | Cross
     WideDotFill    ,   -- 10   | Wide dots
     CloseDotFill   ,   -- 11   | Close dots
     UserFill           -- 12   | User-defined
  );

  type FillSettingsType is record
    style: FillStyle;
    color: Gr_Colour;
  end record;

  type FillPatternType is array(0..7) of Unsigned_8;

------------ Operations on vectors and polygons objects

  function "+" (v1,v2:vector2) return vector2;
  function "*" (f:float; v:vector2) return vector2;
  function "*" (v:vector2; f:float) return vector2;
  function Norm2(v:vector2) return float;

  function "+" (pl:polygon_type; pt:PointType) return polygon_type;
  function "+" (pl:math_polygon_type; pt:math_PointType) 
      return math_polygon_type;
  function Dilatation(pl:math_polygon_type; fx,fy:float) 
      return math_polygon_type;
  function "*" (pl:math_polygon_type; f:float) return math_polygon_type;
  function "*" (f:float; pl:math_polygon_type) return math_polygon_type;
  
-----------------------------------------------------------------------
--      Notation explanation
--
--      '+'  denotes a function / procedure added, which is not present
--           within Turbo / Borland Graphics.
--      GDM denotes source is Gautier de Montmollin
--      RBS denotes source is Robert B. Sutton
--
-----------------------------------------------------------------------

----------------------------------------------------------------------
--  +  Adapt_for_view_port          GDM routine             
--
--     Adapt pixel coordinate for the view port window
----------------------------------------------------------------------
  procedure Adapt_for_view_port(x,y: in out integer; 
                                d:device_type:=current_device);

----------------------------------------------------------------------
--  Arc                             TP5, page 238   BP7, page 10            
--
--  Arc of circle.
----------------------------------------------------------------------
  procedure Arc(x,y,a1,a2,r:integer; round_aspect:boolean:= true; 
                d:device_type:=current_device);
  procedure Arc(x,y,a1,a2,r:float;   round_aspect:boolean:= true; 
                d:device_type:=current_device);

----------------------------------------------------------------------
--  Bar                             TP5, page 241   BP7, page 16            
--
--  Draws a bar, using the current fill style and color.
----------------------------------------------------------------------
  procedure Bar(x1, y1, x2, y2: Integer; d: Device_type:= current_device);
  procedure Bar(x1, y1, x2, y2: Float;   d: Device_type:= current_device);

----------------------------------------------------------------------
--  Bar3D                           TP5, page 242   BP7, page 16            
--
--  Draws a 3-D bar, using the current fill style and color.
----------------------------------------------------------------------
  procedure Bar3D(x1, y1, x2, y2: Integer; Depth: Natural; Top: Boolean;
                  d: Device_type:= current_device);

  procedure Bar3D(x1, y1, x2, y2: Float;   Depth: Natural; Top: Boolean;
                  d: Device_type:= current_device);

----------------------------------------------------------------------
--  Circle                          TP5, page 246   BP7, page 24            
--
--  Draws a circle of R: radius, at X, Y.
----------------------------------------------------------------------
  procedure Circle(x,y,r:integer; round_aspect:boolean:= true; 
                   d:device_type:=current_device);
  procedure Circle(x,y,r:float;  round_aspect:boolean:= false; 
                   d:device_type:=current_device);

----------------------------------------------------------------------
--  ClearDevice                     TP5, page 247   BP7, page 24            
--
--  Clears the graphics screen to backgound color.
----------------------------------------------------------------------
  procedure ClearDevice(d:device_type:=current_device);

----------------------------------------------------------------------
--  ClearViewPort                   TP5, page 248   BP7, page 25            
--
--  Clears the current viewport to background color.
----------------------------------------------------------------------
  procedure ClearViewPort(d:device_type:=current_device);

----------------------------------------------------------------------
--  CloseGraph                      TP5, page 249   BP7, page 27            
--
--  Shuts down the graphics system.
----------------------------------------------------------------------
  procedure CloseGraph(d:device_type:=current_device);

----------------------------------------------------------------------
--  +  Coord                        GDM Routines            
--
--     1. Conversion of math coordinates to pixel coordinates.
--     2. Conversion of pixel coordinates to math coordinates.
----------------------------------------------------------------------
  procedure Coord(x,y:float; i,j:out integer; d:device_type:=current_device);
  procedure Coord(i,j:integer; x,y:out float; d:device_type:=current_device);

----------------------------------------------------------------------
--  +  Draw_axes                    GDM rountine            
--
--     Draw axes of the math plane.
----------------------------------------------------------------------
  procedure Draw_axes(axes:boolean:= true; units,grid_lines,grid_points,
            border,scale: boolean:= false; scalex,scaley:positive:= 1; 
            d:device_type:=current_device);

---------------------------------------------------------------------------
--  DrawPoly                        TP5, page 258   BP7, page 42            
--
--  Draws the outline of a polygon, using the current line style and color.
--  The number of the points is implicit: p'range
---------------------------------------------------------------------------
  procedure DrawPoly(p:polygon_type; d:device_type:=current_device);
  procedure DrawPoly(p:math_polygon_type; d:device_type:=current_device);
  
----------------------------------------------------------------------
--  + DrawVector                    GDM routines            
--
--  Three procedures to draw a vector
----------------------------------------------------------------------
  procedure DrawVector(x,y: float; u,v:float; d:device_type:=current_device);
  procedure DrawVector(x,y: float; v:vector2; d:device_type:=current_device);
  procedure DrawVector(p:math_PointType; v:vector2; 
                       d:device_type:=current_device);

----------------------------------------------------------------------
--  Ellipse                         TP5, page 260   BP7, page 45            
--
--  Draws an elliptical arc from A1: start angle to A2: end angle, with 
--  RX and RY as horizontal and verticle axis, center located at X, Y.  
--  Uses current line style and color.
----------------------------------------------------------------------
  procedure Ellipse(x,y,a1,a2,rx,ry:integer; d:device_type:=current_device);
  procedure Ellipse(x,y,a1,a2,rx,ry:float;   d:device_type:=current_device);

----------------------------------------------------------------------
--  FillEllipse                     TP5, page 270   BP7, page 60            
--
--  Fills the ellipse with fill style, fill color
----------------------------------------------------------------------
  procedure FillEllipse(x,y,rx,ry:integer; d:device_type:=current_device);
  procedure FillEllipse(x,y,rx,ry:float;   d:device_type:=current_device);

----------------------------------------------------------------------
--  FillPoly                        TP5, page 271   BP7, page 61            
--
--  Fills the ellipse with fill style, fill color
--  The number of the points is implicit: p'range
----------------------------------------------------------------------
  procedure FillPoly(p:polygon_type; d:device_type:=current_device);
  procedure FillPoly(p:math_polygon_type; d:device_type:=current_device);

----------------------------------------------------------------------
--  FloodFill                       TP5, page 273   BP7, page 64            
--
--  Fills the area within a border color, border, pointed by X,Y
--  The fill is with current fill pattern.
----------------------------------------------------------------------
  procedure FloodFill(x,y:integer; border:gr_colour; 
                      d:device_type:=current_device);
  procedure FloodFill(x,y:float;   border:gr_colour; 
                      d:device_type:=current_device);

---------------------------------------------------------------------------
--  GetArcCoords                    TP5, page 278   BP7, page 71            
--
--  Permits user to acquire coordinates of the last Ellipse / Arc command.
---------------------------------------------------------------------------
  procedure GetArcCoords(c:out ArcCoordsType);

----------------------------------------------------------------------
--  GetAspectRatio                  TP5, page 279   BP7, page 72            
--
--  Returns the effective resolution of the graphics screen from which
--  The aspect ratio Xasp : Yasp can be computed.
----------------------------------------------------------------------
  procedure GetAspectRatio(x,y:out integer; d:device_type:=current_device);

----------------------------------------------------------------------
--  GetBKColor                      TP5, page 280   BP7, page 73            
--
--  Returns the index (color number) into the palette, for the current
--  background color.
----------------------------------------------------------------------
  function GetBkColor(d:device_type:=current_device) return gr_colour;

----------------------------------------------------------------------
--  GetColor                        TP5, page 281   BP7, page 74            
--
--  Returns the index (color number) into the palette for the current
--  color
----------------------------------------------------------------------
  function GetColor(d:device_type:=current_device) return gr_colour;

----------------------------------------------------------------------
--  GetDefaultPalette               TP5, page 283   BP7, page 76            
--
--  Returns a palette definition record, for the palette as defined
--  at graphics initiation.
----------------------------------------------------------------------
--  (not yet) procedure GetDefaultPalette

----------------------------------------------------------------------
--  GetDriverName                   TP5, page 284   BP7, page 78            
--
--  Returns a string containing the name of the current driver
----------------------------------------------------------------------
  function GetDriverName(d:device_type:=current_device) return string;

---------------------------------------------------------------------------
--  GetFillPattern                  TP5, page 287   BP7, page 81            
--
--  Returns the last fill pattern set by a previous call to SetFillPattern
---------------------------------------------------------------------------
  procedure GetFillPattern(FillPattern: out FillPatternType);

----------------------------------------------------------------------
--  GetFillSettings                 TP5, page 288   BP7, page 81            
--
--  Returns the last fill pattern and color set by a previous call to
--  SetFillPattern or SetFillStyle (color is device-dependent)
----------------------------------------------------------------------
  procedure GetFillSettings(FillInfo: out FillSettingsType;
                            d: Device_type:= current_device);

----------------------------------------------------------------------
--  GetGraphMode                    TP5, page 289   BP7, page 82            
--
--  Returns the current graphics mode
----------------------------------------------------------------------
  function GetGraphMode(d:device_type:=current_device) return positive;

----------------------------------------------------------------------
--  GetImage                        TP5, page 291   BP7, page 84            
--
--  Saves a bit image of the specified region into a buffer.
----------------------------------------------------------------------
--  (not yet) procedure GetImage            

----------------------------------------------------------------------
--  GetLineSettings                 TP5, page 293   BP7, page 85            
--
--  Returns the current line style, line pattern, and line thickness
--  as set by SetLineStyle
----------------------------------------------------------------------
--  (not yet) procedure GetLineSettings

----------------------------------------------------------------------
--  GetMaxColor                     TP5, page 294   BP7, page 86            
--
--  Returns the highest color number that can be passed to SetColor
----------------------------------------------------------------------
  function GetMaxColor(d:device_type:=current_device) return gr_colour;

----------------------------------------------------------------------
--  GetMaxMode                      TP5, page 294   BP7, page 86            
--
--  Returns the maximum mode number for the current driver
----------------------------------------------------------------------  
  function GetMaxMode(d:device_type:=current_device) return positive;

----------------------------------------------------------------------
--  GetMaxX                         TP5, page 295   BP7, page 87            
--
--  Get the maximum X (pixel value) for the right-most column of the 
--  current graphics driver and mode
----------------------------------------------------------------------
  function GetMaxX(d:device_type:=current_device) return integer;

----------------------------------------------------------------------
--  GetMaxY                         TP5, page 296   BP7, page 88            
--
--  Get the maximum Y (pixel value) for the bottom-most row of the 
--  current graphics driver and mode
----------------------------------------------------------------------
  function GetMaxY(d:device_type:=current_device) return integer;

----------------------------------------------------------------------
--  GetModeName                     TP5, page 297   BP7, page 89            
--
--  Returns as a string, the name of the specified graphics mode
----------------------------------------------------------------------
  function GetModeName(mode:positive; d:device_type:=current_device)
    return string;

-------------------------------------------------------------------------
--  GetModeRange                    TP5, page 298   BP7, page 90            
--
--  Returns the lowest and highest valid graphics mode for a given driver
-------------------------------------------------------------------------
  Procedure GetModeRange(d : device_type; 
                         ModeInf : out positive;
                         ModeSup : out positive);

----------------------------------------------------------------------
--  GetPalette                      TP5, page 299   BP7, page 91            
--
--  Returns the current palette and its size
----------------------------------------------------------------------
--  (not yet) procedure GetPalette(p: out PaletteType; 
--                                 d:device_type:=current_device);

---------------------------------------------------------------------------
--  GetPaletteSize                  TP5, page 300   BP7, page 92            
--
--  Returns the size (number of colors) of the palette number lookup table
---------------------------------------------------------------------------
--  (not yet) function GetPaletteSize(d:device_type:=current_device) 
--                return integer;

----------------------------------------------------------------------
--  GetPixel                        TP5, page 300   BP7, page 92            
--
--  Gets the pixel value (color number) at X, Y
----------------------------------------------------------------------
  function GetPixel( x,y: Integer;
                     d: Device_type:= current_device) return Gr_Colour;
  function GetPixel( x,y: Float;
                     d: Device_type:= current_device) return Gr_Colour;
----------------------------------------------------------------------
--  GetTextSettings                 TP5, page 301   BP7, page 95            
--
--  Returns the current text font, direction, size, and justification
--  as set by SetTextStyle, SetTextJustify, and SetTextSettings
----------------------------------------------------------------------
  procedure GetTextSettings(TextInfo: out TextSettingsType);

----------------------------------------------------------------------
--  GetViewSettings                 TP5, page 303   BP7, page 97            
--
--  Returns the current viewport (drawing window) and clipping 
--  parameters, as set by SetViewPort
----------------------------------------------------------------------
  procedure GetViewSettings(ViewPort: out ViewPortType; 
                            d:device_type:=current_device);

----------------------------------------------------------------------
--  GetX                            TP5, page 304   BP7, page 98            
--
--  Returns the X coordinate of the current position (CP)
----------------------------------------------------------------------
  function GetX(d:device_type:=current_device) return integer;

----------------------------------------------------------------------
--  GetY                            TP5, page 305   BP7, page 99            
--
--  Returns the Y coordinate of the current position (CP)
----------------------------------------------------------------------
  function GetY(d:device_type:=current_device) return integer;

----------------------------------------------------------------------
--  GraphDefaults                   TP5, page 307   BP7, page 114           
--
--  Resets the graphics settings
----------------------------------------------------------------------
  procedure GraphDefaults(d:device_type:=current_device);

----------------------------------------------------------------------
--  ImageSize                       TP5, page 312   BP7, page 123           
--
--  Returns the number of bytes required to store a rectangular region
--  of the screen
----------------------------------------------------------------------
--  (not yet) function ImageSize           

------------------------------------------------------------------------------
--  InitGraph                       TP5, page 313   BP7, page 126           
--
--  Initializes the graphics system and puts the hardware into graphics mode
--
--
--  Expands capability in original Pascal, and no driver file puzzles like TP
--
--  +  versions where one can specify a file name for output & dimensions
--
------------------------------------------------------------------------------
  procedure InitGraph(d:device_type:=current_device;
                      mode:natural:= key_for_current_mode);
                      
  -- Required file name (AutoCAD or PostScript)

  procedure InitGraph( d:device_type:=current_device; 
                       mode:natural;
                       file_name:string );

  procedure InitGraph( d:device_type:=current_device;
                       file_name:string );

  -- New dimensions (only for Windows_GDI)

  procedure InitGraph( d:device_type:=current_device;
                       mode:natural;
                       max_x, max_y:positive );

  procedure InitGraph( d:device_type:=current_device;
                       max_x, max_y:positive );

  -- New dimensions and required file name (only for AutoCAD)
  
  procedure InitGraph( d:device_type:=current_device;
                       mode:natural;
                       file_name:string;
                       max_x, max_y:positive );

  procedure InitGraph( d:device_type:=current_device;
                       file_name:string;
                       max_x, max_y:positive );

----------------------------------------------------------------------
--  Line                            TP5, page 325   BP7, page 137           
--
--  Draws a line from (X1, Y1) to (X2, Y2), with 
--  Uses current style, thickness and color used.
----------------------------------------------------------------------
  procedure Line(x1,y1,x2,y2:integer; d:device_type:=current_device);
  procedure Line(x1,y1,x2,y2:float;   d:device_type:=current_device);

-------------------------------------------------------------------------
--  LineRel                         TP5, page 326   BP7, page 139           
--
--  Draws a line from the current position, to X, Y from current location
--  Current style, thickness and color used.
-------------------------------------------------------------------------
  procedure LineRel(dx,dy:integer; d:device_type:=current_device);
  procedure LineRel(dx,dy:float;   d:device_type:=current_device);

----------------------------------------------------------------------
--  LineTo                          TP5, page 327   BP7, page 140           
--
--  Draws a line from the current pointer to X, Y.
--  Current style, thickness and color used.
----------------------------------------------------------------------
  procedure LineTo(x,y:integer; d:device_type:=current_device);
  procedure LineTo(x,y:float;   d:device_type:=current_device);
  
----------------------------------------------------------------------
--  MoveRel                         TP5, page 332   BP7, page 153           
--
--  Moves the current pointer (CP) X,Y, distance from current location
----------------------------------------------------------------------  
  procedure MoveRel(dx,dy:integer; d:device_type:=current_device);
  procedure MoveRel(dx,dy:float;   d:device_type:=current_device);

----------------------------------------------------------------------
--  MoveTo                          TP5, page 333   BP7, page 153           
--
--  Moves the current pointer (CP) to X, Y.
----------------------------------------------------------------------
  procedure MoveTo(x,y:integer; d:device_type:=current_device);
  procedure MoveTo(x,y:float;   d:device_type:=current_device);

----------------------------------------------------------------------
--  +  Nowhere                      GDM routine             
--
--     Set cursor to "nowhere" position (to use for a seq. of LineTo)
----------------------------------------------------------------------
  procedure Nowhere(d:device_type:=current_device);

----------------------------------------------------------------------------
--  OutText                         TP5, page 337   BP7, page 157           
--
--  Sends a character / string to the output device, at cuurent pointer (CP)
----------------------------------------------------------------------------
  procedure OutText(c:character; d:device_type:=current_device);
  procedure OutText(s:string; d:device_type:=current_device);
  
--------------------------------------------------------------------
--  OutTextXY                       TP5, page 339   BP7, page 159           
--
--  Sends a character / string to the output device, at X, Y
----------------------------------------------------------------------
  procedure OutTextXY(x,y:integer; c:character; d:device_type:=current_device);
  procedure OutTextXY(x,y:float;   c:character; d:device_type:=current_device);
  procedure OutTextXY(x,y:integer; s:string; d:device_type:=current_device);
  procedure OutTextXY(x,y:float;   s:string; d:device_type:=current_device);

----------------------------------------------------------------------------
--  PieSlice                        TP5, page 346   BP7, page 173           
--
--  Draws & fills a pie slice at origin X, Y, from start angle to end angle.
----------------------------------------------------------------------------
--  (not yet) procedure PieSlice  

----------------------------------------------------------------------
--  +  Point                        GDM routine             
--
--     Plots a point (pixel?) at X, Y, in the current color
--     See: PutPixel
----------------------------------------------------------------------
  procedure Point(x,y:integer; d:device_type:=current_device);
  procedure Point(x,y:float;   d:device_type:=current_device);

----------------------------------------------------------------------
--  PutImage                        TP5, page 349   BP7, page 175           
--
--  Puts a bit image on the screen
----------------------------------------------------------------------
--  (not yet) procedure PutImage            

----------------------------------------------------------------------
--  PutPixel                        TP5, page 351   BP7, page 177           
--
--  Plots a pixel at X, Y, in the defined color
----------------------------------------------------------------------
  procedure PutPixel(x,y:integer; colour:gr_colour; 
            d:device_type:=current_device);
            
----------------------------------------------------------------------
--  Rectangle                       TP5, page 356   BP7, page 183           
--
--  Draws a rectangle using the current line style and color
----------------------------------------------------------------------          
  procedure Rectangle(x1,y1,x2,y2:integer; d:device_type:=current_device);
  procedure Rectangle(x1,y1,x2,y2:float;   d:device_type:=current_device);

--------------------------------------------------------------------------
--  RestoreCrtMode                  TP5, page 364   BP7, page 190           
--
--  Restores the screen mode to its mode prior to graphics initialization
--------------------------------------------------------------------------
  procedure RestoreCrtMode(d:device_type:=current_device);

----------------------------------------------------------------------
--  Sector                          TP5, page 368   BP7, page 196           
--
--  Draws and fills an elliptical sector
----------------------------------------------------------------------
  procedure Sector(x,y,a1,a2,rx,ry:integer; d:device_type:=current_device);
  procedure Sector(x,y,a1,a2,rx,ry:float;   d:device_type:=current_device);

----------------------------------------------------------------------
--   Virtual screens (SetActivePage, SetVisualPage) 28.XII.1997
--
--   Active and visual page is #0 at startup; the virtual screens are
--   reserved in memory only for the 1st time usage
----------------------------------------------------------------------
  max_virtual_pages: constant positive:= 1;  -- Modify it if neccessary
  subtype page_range is integer range 0..max_virtual_pages;  

----------------------------------------------------------------------
--  SetActivePage                   TP5, page 371   BP7, page 200           
--
--  Set the active page for graphics output
----------------------------------------------------------------------    
  procedure SetActivePage(page_num: page_range; d:device_type:=current_device);

----------------------------------------------------------------------
--  SetAllPalette                   TP5, page 372   BP7, page 201
--
--  Changes all palette colors, as specified
----------------------------------------------------------------------
--  (not yet) procedure SetAllPalette(p: Palette; 
--                                    d:device_type:=current_device);

----------------------------------------------------------------------
--  SetAspectRatio                  TP5, page 373   BP7, page 202           
--
--  Changes the default aspect-ratio correction factor
----------------------------------------------------------------------
  procedure SetAspectRatio(x,y: integer; d:device_type:=current_device);

-------------------------------------------------------------------------
--  SetBkColor                      TP5, page 375   BP7, page 203
--
--  Changes the background color - index (Gr_Colour) or R-G-B value
-------------------------------------------------------------------------
  procedure SetBkColor( colour: Gr_Colour; d: Device_type:= current_device );
  procedure SetBkColor( R,G,B: Float_0_1; d: Device_type:= current_device );

-------------------------------------------------------------------------
--  SetColor                        TP5, page 376   BP7, page 204
--
--  Changes the current drawing color - index (Gr_Colour) or R-G-B value
-------------------------------------------------------------------------
  procedure SetColor( colour: Gr_Colour; d: Device_type:= current_device );
  procedure SetColor( R,G,B: Float_0_1; d: Device_type:= current_device );

--------------------------------------------------------------------------
--  +  Set_Math_Plane               GDM routine             
--
--    Set the math plane.  To be used *after* an eventual SetViewPort !)
--    (x1,y1): lower left corner
--    (x2,y2): upper right corner
--------------------------------------------------------------------------
  procedure Set_math_plane(x1,y1,x2,y2:float; d:device_type:=current_device);  

----------------------------------------------------------------------
--  SetFillPattern                  TP5, page 378   BP7, page 206           
--
--  Selects a user defined fill pattern and color (color is device-dependent)
----------------------------------------------------------------------
  procedure SetFillPattern(Pattern: FillPatternType; Colour: Gr_Colour;
                           d: Device_type:= current_device);

----------------------------------------------------------------------
--  SetFillStyle                    TP5, page 380   BP7, page 208           
--
--  Sets the fill style and color (color is device-dependent)
----------------------------------------------------------------------
  procedure SetFillStyle(Pattern: FillStyle; Colour: Gr_Colour;
                         d: Device_type:= current_device);

--------------------------------------------------------------------------
--  SetGraphBufSize                 TP5, page 381   BP7, page 209           
--
--  Permits user to change the size of buffer used for scan & flood fills.
--------------------------------------------------------------------------
--  (not yet) procedure SetGraphBufSize

----------------------------------------------------------------------
--  SetGraphMode                    TP5, page 381   BP7, page 209           
--
--  Sets the system to graphics mode and clears the screen.
----------------------------------------------------------------------  
  procedure SetGraphMode(mode:positive:= default_mode; 
                         d:device_type:=current_device); 
  
----------------------------------------------------------------------
--  SetLineStyle                    TP5, page 385   BP7, page 211           
--
--  Sets the current line width and style
----------------------------------------------------------------------
--  (not yet) procedure SetLineStyle

------------------------------------------------------------------
--  SetPalette                      TP5, page 386   BP7, page 212
--
--  Procedure to change the color number within the palette
------------------------------------------------------------------
--  (not yet) procedure SetPalette(NumColour, Colour: integer; 
--                                   d:device_type:=current_device););

------------------------------------------------------------------
--  SetAllPalette                   TP5, Pg 372     BP7, page201
--
--  Procedure to change all color numbers within the palette
------------------------------------------------------------------
--  (not yet) procedure SetAllPalette(NumColour, Colour: integer; 
--                                   d:device_type:=current_device););

------------------------------------------------------------------
--  SetAllRGB                       RBS routine
--
--  Procedure to change all RGB color definitions within the palette
--  Used to call SVGA.Set_Palette
------------------------------------------------------------------
--  (not yet) procedure SetAllRGB(NumColour, Colour: integer; 
--                                   d:device_type:=current_device););

------------------------------------------------------------------
--  SetRGBPalette                   TP5, page 388   BP7, page 214 
--
--  Procedure to change the RGB values for a palette entry
------------------------------------------------------------------
  procedure SetRGBPalette(col, r,g,b: Integer; d:device_type:=current_device);

----------------------------------------------------------------------
--  SetTextJustify                  TP5, page 391   BP7, page 219           
--
--  Sets text justification values used by OutText, and OutTextXY.
----------------------------------------------------------------------
  procedure SetTextJustify(Horiz: t_hori_justify; Vert: t_vert_justify);

--------------------------------------------------------------------------
--  SetTextStyle                    TP5, page 392   BP7, page 220           
--
--  1.  Sets current text font, style, and character magnification factor.
--  2.  Sets current text font, style
--  + optional: Bold, Slanted, Underlined (GdM 26-Apr-2002)
--  Applicable to text output by OutText, and OutTextXY.
--------------------------------------------------------------------------
  procedure SetTextStyle(
              Font      : p_Vector_font;
              Direction : T_Direction;
              Size      : Natural;
              Bold      : Boolean:= False;
              Slanted   : Boolean:= False;
              Underlined: Boolean:= False);

  procedure SetTextStyle(
              Font      : p_Vector_font;
              Direction : T_Direction;
              Bold      : Boolean:= False;
              Slanted   : Boolean:= False;
              Underlined: Boolean:= False);

----------------------------------------------------------------------
--  +  SetTextSettings              GDM routine             
--
--     Combined SetTextJustify and SetTextStyle:
----------------------------------------------------------------------
  procedure SetTextSettings(TextInfo: TextSettingsType);

----------------------------------------------------------------------
--  SetUserCharSize                 TP5, page 394   BP7, page 221           
--
--  Permits user to alter character width / heigth for stroked fonts.
----------------------------------------------------------------------
  procedure SetUserCharSize(MultX, DivX, MultY, DivY: positive);

----------------------------------------------------------------------
--  SetUserCharDirection            TP5, N/A        BP7, N/A            
--
--  Permits the user alter the angle of orientation for stroked fonts.
--  Angle is expressed in radians.
----------------------------------------------------------------------
  procedure SetUserCharDirection(angle:float);

----------------------------------------------------------------------------
--  SetViewPort                     TP5, page 395   BP7, page 222           
--
--  Set a view port (i.e. a drawing window in the main screen of device d)
--  (x1,y1): upper left corner
--  (x2,y2): lower right cornerFills the ellipse with fill style, fill color
----------------------------------------------------------------------------
  procedure SetViewPort(x1,y1,x2,y2:natural; clip:boolean:= true; 
                        d:device_type:=current_device);

----------------------------------------------------------------------
--  SetVisualPage                   TP5, page 398   BP7, page 224           
--
--  Sets the visual graphics page number
--  Virtual screens (see SetActivePage)
----------------------------------------------------------------------
  procedure SetVisualPage(page_num: page_range; save_old: boolean:= true; 
                          d:device_type:=current_device);

----------------------------------------------------------------------
--  SetWriteMode                    TP5, page 399   BP7, page 225           
--
--  Sets the writing mode for line drawing
----------------------------------------------------------------------
--  (not yet) procedure SetWriteMode        

----------------------------------------------------------------------
--  + Size_of_math_unit             GDM routines            
--  + Size_ofpixel
--
--    Conversion of sizes: pixel <--> math unit
----------------------------------------------------------------------
  procedure Size_of_pixel(x,y:out float; d:device_type:=current_device);
  procedure Size_of_math_unit(x,y:out float; d:device_type:=current_device);

----------------------------------------------------------------------
--  TextHeigth                      TP5, page 407   BP7, page 246           
--
--  Returns the pixel heigth of a character / string
----------------------------------------------------------------------
  function TextHeight(c: Character;
                      d: Device_type:= current_device) return Natural;
  function TextHeight(TextString: String;
                      d: Device_type:= current_device) return Natural;

----------------------------------------------------------------------
--  TextWidth                       TP5, page 409   BP7, page 249           
--
--  Returns the pixel width of a character / string
----------------------------------------------------------------------
  function TextWidth(c: Character;
                     d: Device_type:= current_device) return Natural;
  function TextWidth(TextString: String;
                     d: Device_type:= current_device) return Natural;

--------------------- Exceptions --------------------------------------------

  Device_not_opened,
  Device_not_closed,
  Non_sizeable_device,

  Invalid_math_plane, Undefined_math_plane, 
  Invalid_view_port, Invalid_mode,
  Zero_aspect_ratio, No_file_name, EPSF_is_for_1_page: exception;

  Non_readable_device,
  Non_RGB_device: exception;

  pragma Inline(Norm2);
  pragma Inline(Nowhere);
  
private
  type p_string is access String;

  dev_file: array(device_type) of Ada.Text_IO.File_Type;
  dev_mode: array(device_type) of positive:= (others=>default_mode);
  device_file_name: array(device_type) of p_string:= (others=> null);

  -- Vectorial font stuff made private (26-Apr-2002)

  type vf_width is array(character range <>) of unsigned_8;
  type vf_index is array(character range <>) of unsigned_16;
  type vf_vertex is array(natural range <>)  of integer_8;

  type vector_font( first_char, last_char: character;
                    n_vertices: natural) is record
    name: string(1..12);
    height, downline: integer;
    width: vf_width(first_char..last_char);
    index: vf_index(first_char..last_char);
    vertex_x, vertex_y: vf_vertex(1..n_vertices);
  end record;

end Graph;
