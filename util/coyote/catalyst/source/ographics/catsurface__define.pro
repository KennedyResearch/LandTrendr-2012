;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement an object graphics surface
;       command for display in an ODrawWidget object. To create a rotating surface
;       in the draw widget, button events MUST be turned on for the draw widget.
;
; AUTHORS:
;
;        FANNING SOFTWARE CONSULTING   BURRIDGE COMPUTING
;        1645 Sheely Drive             18 The Green South
;        Fort Collins                  Warborough, Oxon
;        CO 80526 USA                  OX10 7DN, ENGLAND
;        Phone: 970-221-0438           Phone: +44 (0)1865 858279
;        E-mail: davidf@dfanning.com   E-mail: davidb@burridgecomputing.co.uk
;
; CATEGORY:
;
;       Objects.
;
; SYNTAX:
;
;       theSurface = Obj_New("CATSURFACE")
;       oDrawWidget -> SetProperty, Button_Events=1
;       oDrawWidget -> Add, theSurface
;
; SUPERCLASSES:
;
;       CATOBJECTVIEW
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { CATSURFACE, $
;             INHERITS CATOBJECTVIEW, $      ; A subclass of the CatObjectView object.
;             _origTransform: FltArr(4,4), $ ; The original transformation matrix.
;             _trackball: Obj_New(), $       ; The trackball object.
;             _rotatingModel: Obj_New(), $   ; The rotating model of the surface.
;             _theSurface: Obj_New(), $      ; The surface object.
;             _xAxis:Obj_New(), $            ; The X Axis object.
;             _yAxis:Obj_New(), $            ; The Y Axis object.
;             _zAxis:Obj_New(), $            ; The Z Axis object.
;             _nonRotatingLight:Obj_New(), $ ; The non-rotating light object.
;             _rotatingLight: Obj_New(), $   ; The rotating light object.
;             _fillLight: Obj_New(), $       ; The fill light object.
;             _ambientLight: Obj_New(), $    ; The ambient light object.
;             _thePalette: Obj_New(), $      ; The surface color palette.
;             _colortable: 0L, $             ; The current color table (for elevation colors).
;             _surfaceColor:BytArr(3), $     ; The current color of the surface.
;             _r: BytArr(256), $             ; The R values of the current color table.
;             _g: BytArr(256), $             ; The G values of the current color table.
;             _b: BytArr(256), $             ; The B values of the current color table.
;             _data: Ptr_New(), $            ; The original Z data (2D) for the surface.
;             _x: Ptr_New(), $               ; The original X data for the surface.
;             _y: Ptr_New(), $               ; The original Y data for the surface.
;             _plottitle: Obj_New(), $       ; The plot title object.
;             _colortool: Obj_New() $        ; A color tool.
;           }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 1 July, 2003.
;-
;******************************************************************************************;
;  Copyright (c) 2008, jointly by Fanning Software Consulting, Inc.                        ;
;  and Burridge Computing. All rights reserved.                                            ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. or Burridge Computing       ;
;        nor the names of its contributors may be used to endorse or promote products      ;
;        derived from this software without specific prior written permission.             ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. AND BURRIDGE COMPUTING   ;
;  ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE     ;
;  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE          ;
;  DISCLAIMED. IN NO EVENT SHALL FANNING SOFTWARE CONSULTING, INC. OR BURRIDGE COMPUTING   ;
;  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL    ;
;  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;    ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
;+
; NAME:
;       CATSURFACE::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the CATSURFACE object.
;
; SYNTAX:
;
;       theObject -> ControlPanel, baseObject
;
; ARGUMENTS:
;
;       baseObject:    The object reference of a base widget for this control to
;                      be added to. If not supplied, the control panel will be in a
;                      self contained window (i.e., a TOPLEVELBASE object).
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the CatControlPanel::INIT method.
;
;-
;*****************************************************************************************************
PRO CatSurface::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Object Control Panel', _EXTRA=extraKeywords)

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.
   base = Obj_New('BASEWIDGET', cp, Column=1, Frame=1, Event_Object=self)


;   ; Create OUTPUT menu buttons for formatted output files.
;   output = Obj_New('ButtonWidget', base, Value='Save As...', /Menu)
;   button = Obj_New('ButtonWidget', output, Value='BMP File', $
;      Name='BMP', Event_Method='SurfaceOutput')
;   button = Obj_New('ButtonWidget', output, Value='EPS File', $
;      Name='EPS', Event_Method='SurfaceOutput')
;   button = Obj_New('ButtonWidget', output, Value='JPEG File', $
;      Name='JPEG', Event_Method='SurfaceOutput')
;   button = Obj_New('ButtonWidget', output, Value='PNG File', $
;      Name='PNG', Event_Method='SurfaceOutput')
;   button = Obj_New('ButtonWidget', output, Value='TIFF File', $
;      Name='TIFF', Event_Method='SurfaceOutput')
;
;   printer = Obj_New('ButtonWidget', base, Value='Print', /Separator, $
;      Event_Method='Surface_Printing', /Menu)
;   dummy = Obj_New('ButtonWidget', printer, Value='Vector Output (faster BW)', Name='VECTOR')
;   dummy = Obj_New('ButtonWidget', printer, Value='Bitmap Output (slower BW)', Name='BITMAP')
;   dummy = Obj_New('ButtonWidget', printer, Value='Full Color Printing (slower)', Name='COLOR')

   ; Create STYLE menu buttons for surface style.
   style = Obj_New('ButtonWidget', base, Value='Style', /Menu)
   dummy = Obj_New('ButtonWidget', style, Value='Dot Surface', $
      Event_Method='SurfaceStyle', Name='DOTS')
   dummy = Obj_New('ButtonWidget', style, Value='Wire Mesh', $
      Event_Method='SurfaceStyle', Name='MESH')
   dummy = Obj_New('ButtonWidget', style, Value='Solid', $
      Event_Method='SurfaceStyle', Name='SOLID')
   dummy = Obj_New('ButtonWidget', style, Value='Parallel X Lines', $
      Event_Method='SurfaceStyle', Name='XPARALLEL')
   dummy = Obj_New('ButtonWidget', style, Value='Parallel Y Lines', $
      Event_Method='SurfaceStyle', Name='YPARALLEL')
   dummy = Obj_New('ButtonWidget', style, Value='Hidden Lines ON', $
      Event_Method='SurfaceStyle', Name='HIDDEN', /Separator)

   elevationID = Obj_New('ButtonWidget', style, Value='Elevation Shading ON', $
         /Separator, UValue='Elevation Shading OFF', Name='ELEVATION SHADING', $
         Event_Method='SurfaceElevation')
   colorsID = Obj_New('ButtonWidget', style, Value='Elevation Colors...', $
      Event_Method='SurfaceElevation', Name='ELEVATION COLORS')

   ; Create PROPERTIES menu buttons for surface properties.
   properties = Obj_New('ButtonWidget', base, Value='Properties', /Menu, $
      Event_Method='SurfaceProperties')

   ; Surface Color
   scolorID = Obj_New('ButtonWidget', properties, Value='Surface Color...', $
      Name='SURFACE_COLOR', Event_Method='SurfaceProperties')

   colorsID -> SetProperty, Sensitive = 0
   scolorID -> SetProperty, Sensitive = 1
   self._colorsID = colorsID
   self._scolorID = scolorID

    ; Background Color
   bcolor = Obj_New('ButtonWidget', properties, Value='Background Color', /Menu)
   dummy = Obj_New('ButtonWidget', bcolor, Value='Black', $
      Event_Method='SurfaceProperties', Name='BBLACK')
   dummy = Obj_New('ButtonWidget', bcolor, Value='White', $
      Event_Method='SurfaceProperties', Name='BWHITE')
   dummy = Obj_New('ButtonWidget', bcolor, Value='Charcoal', $
      Event_Method='SurfaceProperties', Name='BCHARCOAL')
   dummy = Obj_New('ButtonWidget', bcolor, Value='Gray', $
      Event_Method='SurfaceProperties', Name='BGRAY')

   ; Axes Color
   acolor = Obj_New('ButtonWidget', properties, Value='Axes Color', /Menu)
   dummy = Obj_New('ButtonWidget', acolor, Value='Black', $
      Event_Method='SurfaceProperties', Name='ABLACK')
   dummy = Obj_New('ButtonWidget', acolor, Value='White', $
      Event_Method='SurfaceProperties', Name='AWHITE')
   dummy = Obj_New('ButtonWidget', acolor, Value='Yellow', $
      Event_Method='SurfaceProperties', Name='AYELLOW')
   dummy = Obj_New('ButtonWidget', acolor, Value='Green', $
      Event_Method='SurfaceProperties', Name='AGREEN')
   dummy = Obj_New('ButtonWidget', acolor, Value='Navy Blue', $
      Event_Method='SurfaceProperties', Name='ANAVY')

   ; Title Color
   tcolor = Obj_New('ButtonWidget', properties, Value='Title Color', /Menu)
   dummy = Obj_New('ButtonWidget', tcolor, Value='Black', $
      Event_Method='SurfaceProperties', Name='TBLACK')
   dummy = Obj_New('ButtonWidget', tcolor, Value='White', $
      Event_Method='SurfaceProperties', Name='TWHITE')
   dummy = Obj_New('ButtonWidget', tcolor, Value='Yellow', $
      Event_Method='SurfaceProperties', Name='TYELLOW')
   dummy = Obj_New('ButtonWidget', tcolor, Value='Green', $
      Event_Method='SurfaceProperties', Name='TGREEN')
   dummy = Obj_New('ButtonWidget', tcolor, Value='Navy Blue', $
      Event_Method='SurfaceProperties', Name='TNAVY')

   ; Color Schemes.
   dummy = Obj_New('ButtonWidget', properties, Value='Black on White', /Separator, $
      Event_Method='SurfaceProperties', Name='B/W')
   dummy = Obj_New('ButtonWidget', properties, Value='White on Black', $
      Event_Method='SurfaceProperties', Name='W/B')
   dummy = Obj_New('ButtonWidget', properties, Value='Original Colors', $
      Event_Method='SurfaceProperties', Name='ORIGINAL_COLORS')

   ; Original Axis rotation.
   dummy = Obj_New('ButtonWidget', properties, Value='Original Rotation', /Separator, $
      Event_Method='SurfaceProperties', Name='ORIGINAL_T3D')

   ; Drag Quality.
   dragID = Obj_New('ButtonWidget', properties, Value='Drag Quality', /Separator, /Menu)
      self._dragLowID = Obj_New('ButtonWidget', dragID, Value='Low', $
         Event_Method='SurfaceProperties', Name='DRAG_LOW')
      self._dragMedID = Obj_New('ButtonWidget', dragID, Value='Medium', $
         Event_Method='SurfaceProperties', Name='DRAG_MEDIUM')
      self._dragHighID = Obj_New('ButtonWidget', dragID, Value='High', $
         Event_Method='SurfaceProperties', Name='DRAG_HIGH')
   self._dragHighID -> SetProperty, Sensitive=0
   self._dragQuality = 2

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::CREATE_VEIW
;
; PURPOSE:
;
;       This method creates the IDLgrView object that will be displayed in the oDrawGraphics window.
;
; SYNTAX:
;
;       theView = self -> Create_View()
;
; ARGUMENTS:
;
;       data:               A 2D array of surface data.
;
;       x:                  A vector of X data values.
;
;       y:                  A vector of Y data values.
;
; KEYWORDS:
;
;       COLORTABLE:         Set this keyword to a number between 0 and 40 to select one
;                           of the pre-selected IDL color tables for elevation shading.
;
;       ELEVATION_SHADING: Set this keyword to put elevation shading into effect.
;
;       EXACT:             Set this keyword to a one-, two-,or three-element array to set exact axis
;                          scaling for the X, Y, and Z axes, respectively. If Exact is a one-element array,
;                          all three axes are set to the same value. For example, to set the X axis to
;                          exact scaling and the Y and Z axes to normal scaling, type:
;
;                             IDL> FSC_Surface, Exact=[1,0,0]
;
;       HIDDEN_LINES:      Set this keyword to draw the surface with hidden lines removed. The default
;                          is to show hidden lines.
;
;       POSITION:          A two-, four- or six-element array of normalized (0 to 1) coordinates
;                          used to position the X, Y, and Z axis in the coordinate space. Uses the form
;                          [x0, x1, y0, y1, z0, z1]. In the absence of POSITION information, the Z position
;                          is always [0,1] and the X and Y positions are calculated in a manner that
;                          preserves the aspect ratio of the surface data.
;
;       SHADED_SURFACE:    Set this keyword to set up a shaded surface plot rather than a wire
;                          mesh surface, which is the default.
;
;       TITLE:             A string used as the title of the plot.
;
;       XTITLE:            A string used as the X title of the plot.
;
;       YTITLE:            A string used as the Y title of the plot.
;
;       ZTITLE:            A string used as the Z title of the plot.
;
;       _EXTRA:            This keyword collects otherwise undefined keywords that are passed to the
;                          IDLgrSURFACE initialization routine.
;-
;*****************************************************************************************************
FUNCTION CatSurface::Create_View, data, x, y, $
   Colortable=colortable, $
   Elevation_Shading=elevation, $
   Exact=exact, $
   Hidden_Lines=hidden_lines, $
   Position=position, $
   Shaded_Surface=shaded, $
   Title=plotTitle, $
   XTitle=xtitle, $
   YTitle=ytitle, $
   ZTitle=ztitle, $
   _Extra=extraKeywords

   @cat_func_error_handler

   ; This method can only be run once. After that, all changes should be made
   ; via the SetProperty method.

   IF Obj_Valid(self._theView) THEN Message, 'Set CATSurface properties with the SetProperty method.'

      ; Initialize the view. Use RGB color. Charcoal background.
      ; The coodinate system is chosen so that (0,0,0) is in the
      ; center of the window. This will make rotations easier.

   theView = Obj_New('IDLgrView', Color=[80,80,80], Viewplane_Rect=[-1.2,-1.1,2.3,2.2])

       ; Check for keywords.

   IF N_Elements(xtitle) EQ 0 THEN xtitle='X Axis'
   IF N_Elements(ytitle) EQ 0 THEN ytitle='Y Axis'
   IF N_Elements(ztitle) EQ 0 THEN ztitle='Z Axis'
   IF N_Elements(plotTitle) EQ 0 THEN plotTitle=''
   IF N_Elements(colortable) EQ 0 THEN colortable = 4 ELSE colortable = 0 > colortable < 40
   hidden_lines = Keyword_Set(hidden_lines)
   elevation = Keyword_Set(elevation)
   landscape = Keyword_Set(landscape)
   IF Keyword_Set(shaded) THEN BEGIN
      shading = 1
      style = 2
   ENDIF ELSE BEGIN
      shading = 0
      style = 1
   ENDELSE
   CASE N_Elements(exact) OF
      0: exact = [0,0,0]
      1: exact = Replicate(exact, 3)
      2: exact = [exact, 0]
      3:
      ELSE: BEGIN
         Message, 'Exact keyword contains too many elements. Returning...'
         ENDCASE
   ENDCASE

       ; Need some data.

   IF N_Elements(data) EQ 0 THEN BEGIN
      data = dist(41, 41)
   ENDIF

   s = Size(data)
   IF s[0] NE 2 THEN Message,'Must pass 2D argument. Using fake data.'
   IF N_Elements(x) EQ 0 THEN x = Findgen(s[1])
   IF N_Elements(y) EQ 0 THEN y = Findgen(s[2])


   Catch, /Cancel

      ; Calculate or use the position coordinates.

   IF N_Elements(position) EQ 0 THEN BEGIN

         ; I want the surface data to have the same aspect ratio as the data itself
         ; in the X and Y directions.

      surfaceAspect = Float(s[2]) / s[1]
      windowAspect = 1.0
      position = Aspect(surfaceAspect, WindowAspect=windowAspect, Margin=0)
      position = [position[0], position[2], position[1], position[3], 0.0, 1.0] - 0.5

   ENDIF ELSE BEGIN

      CASE N_Elements(position) OF

         2: BEGIN
            position = [position, 0.0, 1.0, 0.0, 1.0]
            position[0] = 0.0 > position[0]
            position[1] = position[1] < 1.0
            END

         4: BEGIN
            position = [position, 0.0, 1.0]
            position[0] = 0.0 > position[0]
            position[1] = position[1] < 1.0
            position[2] = 0.0 > position[2]
            position[3] = position[3] > 1.0
            END

         6: BEGIN
            position[0] = 0.0 > position[0]
            position[1] = position[1] < 1.0
            position[2] = 0.0 > position[2]
            position[3] = position[3] > 1.0
            position[4] = 0.0 > position[4]
            position[5] = position[5] > 1.0
            END

         ELSE: BEGIN
            Message, 'POSITION keyword must be a 2, 4, or 6 element array. Returning...'
            END

      ENDCASE

      pos = pos - 0.5

   ENDELSE

       ; Create a model for the surface and axes and add it to the view.
       ; This model will rotate under the direction of the trackball object.

   rotatingModel = OBJ_NEW('IDLgrModel', Name='ROTATING MODEL')
   theView -> Add, rotatingModel

       ; Create a separate model for the title that doesn't rotate.

   textModel = Obj_New('IDLgrModel', Name='TITLE MODEL')
   theView -> Add, textModel

       ; Create helper objects. First, create title objects
       ; for the axes and plot. Color them green.

   xTitle = Obj_New('IDLgrText', xtitle, Color=[0,255,0])
   yTitle = Obj_New('IDLgrText', ytitle, Color=[0,255,0])
   zTitle = Obj_New('IDLgrText', ztitle, Color=[0,255,0])

       ; Create font objects.

   helvetica10pt = Obj_New('IDLgrFont', 'Helvetica', Size=10)
   helvetica14pt = Obj_New('IDLgrFont', 'Helvetica', Size=14)

       ; Create a plot title object. I am going to place the title
       ; centered in X and towards the top of the viewplane rectangle.

   plotTitle = Obj_New('IDLgrText', plotTitle, Color=[0,255,0], $
      Alignment=0.5, Location=[0.0, 0.9, 0.0], Font=helvetica14pt)
   textModel -> Add, plotTitle

       ; Create a trackball for surface rotations. Center it in
       ; the 400-by-400 window. Give it a 200 pixel diameter.

   thisTrackball = OBJ_NEW('Cat_Trackball', [200, 200], 200)

      ; Create a palette for the surface.

   thePalette = Obj_New("IDLgrPalette")
   thePalette -> LoadCT, colortable
   thePalette -> GetProperty, Red=r, Green=g, Blue=b

       ; Create a surface object. Make it white.

   self._surfColor = [255, 255, 255]
   IF elevation THEN BEGIN
      theSurface = OBJ_NEW('IDLgrSurface', data, x, y, $
         Color=self._surfColor, _Extra=extra, Style=style, $
         Shading=shading, Hidden_Lines=hidden_lines)
      s = Size(data, /Dimensions)
      theSurface -> SetProperty, Vert_Colors=Reform(BytScl(data), s[0]*s[1], /NAN), Palette=thePalette
      self._elevshading = 1
   ENDIF ELSE BEGIN
      theSurface = OBJ_NEW('IDLgrSurface', data, x, y, $
         Color=self._surfColor, _Extra=extra, Style=style, $
         Shading=shading, Hidden_Lines=hidden_lines)
      self._elevshading = 0
   ENDELSE

       ; Get the data ranges of the surface.

   theSurface -> GetProperty, XRange=xrange, YRange=yrange, ZRange=zrange

       ; Create axes objects for the surface. Color them green.
       ; Axes are created after the surface so the range can be
       ; set correctly. Note how I set the font to 10 pt helvetica.

   xAxis = Obj_New("IDLgrAxis", 0, Color=[0,255,0], Ticklen=0.1, $
      Minor=4, Title=xtitle, Range=xrange, Exact=exact[0])
   xAxis -> GetProperty, Ticktext=xAxisText
   xAxisText -> SetProperty, Font=helvetica10pt

   yAxis = Obj_New("IDLgrAxis", 1, Color=[0,255,0], Ticklen=0.1, $
      Minor=4, Title=ytitle, Range=yrange, Exact=exact[1])
   yAxis -> GetProperty, Ticktext=yAxisText
   yAxisText -> SetProperty, Font=helvetica10pt

   zAxis = Obj_New("IDLgrAxis", 2, Color=[0,255,0], Ticklen=0.1, $
      Minor=4, Title=ztitle, Range=zrange, Exact=exact[2])
   zAxis -> GetProperty, Ticktext=zAxisText
   zAxisText -> SetProperty, Font=helvetica10pt

       ; The axes may not use exact axis scaling, so the ranges may
       ; have changed from what they were originally set to. Get
       ; and update the range variables.

   xAxis -> GetProperty, CRange=xrange
   yAxis -> GetProperty, CRange=yrange
   zAxis -> GetProperty, CRange=zrange

       ; Set scaling parameters for the surface and axes so that everything
       ; is scaled into the range -0.5 to 0.5. We do this so that when the
       ; surface is rotated we don't have to worry about translations. In
       ; other words, the rotations occur about the point (0,0,0).

   xs = FSC_Normalize(xrange, Position=[position[0], position[1]])
   ys = FSC_Normalize(yrange, Position=[position[2], position[3]])
   zs = FSC_Normalize(zrange, Position=[position[4], position[5]])

       ; Scale the axes and place them in the coordinate space.
       ; Note that not all values in the Location keyword are
       ; used. (I've put really large values into the positions
       ; that are not being used to demonstate this.) For
       ; example, with the X axis only the Y and Z locations are used.

   xAxis -> SetProperty, Location=[9999.0, position[2], position[4]], XCoord_Conv=xs
   yAxis -> SetProperty, Location=[position[0], 9999.0, position[4]], YCoord_Conv=ys
   zAxis -> SetProperty, Location=[position[0],  position[3], 9999.0], ZCoord_Conv=zs

       ; Scale the surface.

   theSurface -> SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

       ; Add the surface and axes objects to the model.

   rotatingModel -> Add, theSurface
   rotatingModel -> Add, xAxis
   rotatingModel -> Add, yAxis
   rotatingModel -> Add, zAxis

       ; Rotate the surface model to the standard surface view.

   rotatingModel -> Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
   rotatingModel -> Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
   rotatingModel -> Rotate,[1,0,0],  30  ; Rotate it down slightly.

   ; Create some lights to view the surface. Surfaces will look
   ; best if there is some ambient lighting to illuminate them
   ; uniformly, and some positional lights to give the surface
   ; definition. We will create three positional lights: one,
   ; non-rotating light will provide overhead definition. Two
   ; rotating lights will provide specific surface definition.
   ; Lights should be turned off or hidden if elevation shading
   ; is in effect.

       ; First create the ambient light. Don't turn it on too much,
       ; or the surface will appear washed out.

   ambientLight = Obj_New('IDLgrLight', Type=0, Intensity=0.2, Hide=Keyword_Set(elevation))
   rotatingModel -> Add, ambientLight

       ; Shaded surfaces will not look shaded unless there is a
       ; positional light source to give the surface edges definition.
       ; This light will rotate with the surface.

   rotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.60, $
       Location=[xrange[1], yrange[1], 4*zrange[1]], $
       Direction=[xrange[0], yrange[0], zrange[0]], Hide=Keyword_Set(elevation))
   rotatingModel -> Add, rotatingLight

       ; Create a fill light source so you can see the underside
       ; of the surface. Otherwise, just the top surface will be visible.
       ; This light will also rotate with the surface.

   fillLight = Obj_New('IDLgrLight', Type=1, Intensity=0.4, $
      Location=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, -2*Abs(zrange[0])], $
      Direction=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, zrange[1]], $
      Hide=Keyword_Set(elevation))
   rotatingModel -> Add, fillLight

       ; Create a non-rotating overhead side light.

   nonrotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.8, $
       Location=[-xrange[1], (yrange[1]-yrange[0])/2.0, 4*zrange[1]], $
       Direction=[xrange[1], (yrange[1]-yrange[0])/2.0, zrange[0]], $
       Hide=Keyword_Set(elevation))
   nonrotatingModel = Obj_New('IDLgrModel')
   nonrotatingModel -> Add, nonrotatingLight

      ; Be sure to add the non-rotating model to the view, or it won't be visualized.

   theView -> Add, nonrotatingModel

       ; Scale the light sources.

   rotatingLight -> SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
   fillLight -> SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
   nonrotatingLight -> SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

       ; Rotate the non-rotating model to the standard surface view.

   nonrotatingModel -> Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
   nonrotatingModel -> Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
   nonrotatingModel -> Rotate,[1,0,0],  30  ; Rotate it down slightly.

   self._trackball = thisTrackball

      ; Get the current transformation matrix, so it can be restored.

   rotatingModel -> GetProperty, Transform=origTransform
   self._origTransform = origTransform

      ; Create a colortool for changing the elevation colors and register
      ; for color table messages.

   self._colortool = Obj_New('ColorTool', colortable)
   self._colortool -> RegisterForMessage, self, 'COLORTOOL_TABLECHANGE'

      ; Add created objects to the trash container.
   self -> AddToTrash, thisTrackball
   self -> AddToTrash, xTitle
   self -> AddToTrash, yTitle
   self -> AddToTrash, zTitle
   self -> AddToTrash, xAxis
   self -> AddToTrash, yAxis
   self -> AddToTrash, zAxis
   self -> AddToTrash, theSurface
   self -> AddToTrash, nonRotatingModel
   self -> AddToTrash, rotatingModel
   self -> AddToTrash, plotTitle
   self -> AddToTrash, helvetica10pt
   self -> AddToTrash, helvetica14pt
   self -> AddToTrash, thePalette


   self._surfaceColor = [255B, 255B, 255B]
   self._thePalette = thePalette
   self._theSurface = theSurface
   self._rotatingModel = rotatingModel
   self._rotatingLight = rotatingLight
   self._theSurface = theSurface
   self._xaxis = xaxis
   self._yaxis = yaxis
   self._zaxis = zaxis
   self._nonRotatingLight = nonRotatingLight
   self._fillLight = fillLight
   self._ambientLight = ambientLight
   self._thePalette = thePalette
   self._r = r
   self._g = g
   self._b = b
   IF Ptr_Valid(self._data) THEN *self._data = data ELSE self._data = Ptr_New(data)
   IF Ptr_Valid(self._x) THEN *self._x = x ELSE self._x = Ptr_New(x)
   IF Ptr_Valid(self._y) THEN *self._y = y ELSE self._y = Ptr_New(y)
   self._plotTitle = plotTitle

   RETURN, theView
END


;*****************************************************************************************************
;+
; NAME:
;       CATSUFACE::EVENTHANDLER
;
; PURPOSE:
;
;       This method is the event handler for the CatSurface object.
;
; SYNTAX:
;
;       The method is called automatically when a widget generate an event..
;
; ARGUMENTS:
;
;     event:  The event structure from the object that caused the event.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO CatSurface::EventHandler, event

            ; What kind of event is this? It must be from the draw widget.

         drawTypes = Replicate('NONE', 16)
         drawTypes[0:6] = ['PRESS', 'RELEASE', 'MOTION', 'SCROLL', 'EXPOSE', 'ASCII KEY', 'NON_ASCII KEY']
         thisEvent = drawTypes(event.type)

            ; Get the view object, the graphic tree, and the window object.
            ; Note that the event.id should be an oDrawWidget object. This event
            ; handler is used if the HANDLE_EVENTS keyword is used when this object
            ; is ADDed to the the oDrawWidget object.

         event.id -> GetProperty, ViewObject=viewObject, WindowObject=thisWindow, Graphics_Tree=thisView

            ; Get the trackball object.
         viewObject -> GetProperty, Trackball=thisTrackball

            ; Get the rotating model object.
         thisModel = thisView -> Get(ISA='IDLgrModel')

         CASE thisEvent OF

            'EXPOSE':  ; Nothing required except to draw the view.
            'PRESS': BEGIN

                  ; Zoom out on middle, zoom in on right, rotate on left.

                possibleButtons = Replicate('NONE', 32)
                possibleButtons[0:6] = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT', 'COMBO', 'COMBO']
                thisButton = possibleButtons(event.press)

                CASE thisButton OF

                   ; Zoom in.
                  'RIGHT': BEGIN
                           thisView -> GetProperty, Viewplane_Rect=thisRect
                           thisRect(0) = (thisRect(0) + 0.05) < thisRect(2)
                           thisRect(1) = (thisRect(1) + 0.05) < thisRect(3)
                           thisRect(2) = (thisRect(2) - 0.1) > thisRect(0)
                           thisRect(3) = (thisRect(3) - 0.1) > thisRect(1)
                           thisView -> SetProperty, Viewplane_Rect=thisRect
                           END

                   ; Zoom out.
                  'MIDDLE': BEGIN
                           thisView -> GetProperty, Viewplane_Rect=thisRect
                           thisRect(0) = thisRect(0) - 0.05
                           thisRect(1) = thisRect(1) - 0.05
                           thisRect(2) = thisRect(2) + 0.1
                           thisRect(3) = thisRect(3) + 0.1
                           thisView -> SetProperty, Viewplane_Rect=thisRect
                           END

                   ; Rotate
                  'LEFT':  BEGIN
                              IF event.modifiers NE 0 THEN BEGIN
                              thisView -> GetProperty, Viewplane_Rect=thisRect
                              thisRect(0) = thisRect(0) - 0.05
                              thisRect(1) = thisRect(1) - 0.05
                              thisRect(2) = thisRect(2) + 0.1
                              thisRect(3) = thisRect(3) + 0.1
                              thisView -> SetProperty, Viewplane_Rect=thisRect
                           ENDIF ELSE BEGIN
                              event.id -> SetProperty, Motion_Events=1 ; Motion events ON.
                              thisWindow -> SetProperty, Quality=self._dragQuality
                           ENDELSE
                           END

                  ELSE:

                ENDCASE

                END
            'RELEASE': BEGIN
                event.id -> SetProperty, Motion_Events=0 ; Motion events OFF.
                thisWindow -> SetProperty, Quality=2 ; Drag Quality to High.
                END
            'MOTION':
            ELSE:

         ENDCASE

               ; Does the trackball need updating? If so, update. Updating MUST
               ; occur here, NOT in MOTION case above.

         needUpdate = thisTrackball -> Update(event, Transform=thisTransform)
         IF needUpdate THEN BEGIN
            thisModel -> GetProperty, Transform=modelTransform
            thisModel -> SetProperty, Transform=modelTransform # thisTransform
         ENDIF

             ; Draw the view.

         thisWindow -> Draw, thisView

END



;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::GETPROPERTY
;
; PURPOSE:
;
;       This method enables the getting of the CATSURFACE object properties.
;
; SYNTAX:
;
;       aCATSURFACE -> GetProperty, ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     TRACKBALL:  The trackball object contained within the IDLgrView object.
;
;     _EXTRA:     Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO CatSurface::GetProperty, Trackball=trackball, _Ref_Extra=extraKeywords

   IF Arg_Present(trackball) THEN trackball = self._trackball
   IF N_Elements(extraKeywords) GT 0 THEN self -> CatObjectView::GetProperty, _Extra=extraKeywords
END


;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::MESSAGEHANDLER
;
; PURPOSE:
;
;       This method responds to "messages" sent from other objects. It is called
;       automatically by other objects. To receive messages, it is necessary to
;       "register" with the messaging object.
;
; SYNTAX:
;
;       None. Called by other objects.
;
; ARGUMENTS:
;
;       TITLE:  The message title. This is the "name" of the message indicated when
;               the object registered for messages with the messaging object.
;
; KEYWORDS:
;
;       DATA:   Information from the SENDER that may be relevant in processing the message.
;               Typically, an anonymous structure variable, although it could be anything at all.
;
;       SENDER: An output keyword. This is the object reference of the object that is sending
;               the message.
;-
;*****************************************************************************************************
PRO CatSurface::MessageHandler, title, SENDER=sender, DATA=data

      ; Initialise the error handler
   @cat_pro_error_handler

   CASE title OF

      'COLORTOOL_TABLECHANGE': BEGIN

         ; Update the surface elevation colors.
         self._colortool -> SetProperty, Red=data.r, Green=data.g, Blue=data.b, Bottom=data.bottom
         self._thePalette -> SetProperty, Red=data.r, Green=data.g, Blue=data.b
         self -> SendMessage, 'SETPROPERTY_CHANGE'
         END

      ELSE:

   ENDCASE

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::RESIZE
;
; PURPOSE:
;
;       This method resizes the trackball for the object
;
; SYNTAX:
;
;       self -> Resize, xsize, ysize
;
; ARGUMENTS:
;
;       xsize:   The new X size.
;
;       ysize:   The new Y size.
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
PRO CatSurface::Resize, xsize, ysize

   @cat_pro_error_handler

   self._trackball -> Reset, [xsize/2, ysize/2], (ysize/2) < (xsize/2)

END



;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::SELECTCOLORS
;
; PURPOSE:
;
;       This method is used to allow user-selection of various surface plot colors.
;       If no keywords are used, the color of the surface is changed.
;
; SYNTAX:
;
;       aCatSurface -> SelectColors
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     AXIS:          Set this keyword to allow the user to select a color for all three axes.
;
;     BACKGROUND     Set this keyword to allow the user to select a color for the background color.
;
;     ELEVATION:     Set this keyword to allow the user to select elevation shading colors
;                           from a COLORTOOL object.
;
;     SURFACE        Set this keyword to allow the user to select a color for the surface color.
;
;     TITLE:         Set this keyword to allow the user to select a color for the title color.
;
;     XAXIS:         Set this keyword to allow the user to select a color for the X axis color.
;
;     YAXIS:         Set this keyword to allow the user to select a color for the Y axis color.
;
;     ZAXIS:         Set this keyword to allow the user to select a color for the Z axis color.
;
;-
;*****************************************************************************************************
PRO CatSurface::SelectColors, $
   AXIS=axis, $
   BACKGROUND=background, $
   ELEVATION=elevation, $
   SURFACE=surface, $
   TITLE=title, $
   XAXIS=xaxis, $
   YAXIS=yaxis, $
   ZAXIS=zaxis

   @cat_pro_error_handler

   IF Total( Keyword_Set(axis) + Keyword_Set(background) + Keyword_Set(elevation) + Keyword_Set(surface) + $
             Keyword_Set(title) + Keyword_Set(xaxis) + Keyword_Set(yaxis) + Keyword_Set(zaxis) ) EQ 0 THEN $
             surface = 1

   IF Keyword_Set(axis) THEN BEGIN
      self._xaxis -> GetProperty, Color=theColor

         ; Can you find a widget to be the group leader for PICKCOLOR?

         parents = self._parents -> Get(/All)
         FOR j=0,N_Elements(parents)-1 DO $
            IF Obj_IsA_Valid(parents[j], 'WIDGETATOM') THEN $
            BEGIN
               group_leader = parents[j]
               group_leader -> GetProperty, ID=thisGroupLeader
               BREAK
            ENDIF

      thisColor = PickColor(Group_Leader=thisGroupLeader, CurrentColor=theColor, $
         Cancel=cancelled, Title='Pick AXIS Color...')
      IF NOT cancelled THEN self -> SetProperty, Axis_Color=Reform(thisColor)
   ENDIF

   IF Keyword_Set(background) THEN BEGIN
      self._theView -> GetProperty, Color=theColor

         ; Can you find a widget to be the group leader for PICKCOLOR?

         parents = self._parents -> Get(/All)
         FOR j=0,N_Elements(parents)-1 DO $
            IF Obj_IsA_Valid(parents[j], 'WIDGETATOM') THEN $
            BEGIN
               group_leader = parents[j]
               group_leader -> GetProperty, ID=thisGroupLeader
               BREAK
            ENDIF

      thisColor = PickColor(Group_Leader=thisGroupLeader, CurrentColor=theColor, $
         Cancel=cancelled, Title='Pick BACKGROUND Color...')
      IF NOT cancelled THEN self -> SetProperty, Background_Color=Reform(thisColor)
   ENDIF

   IF Keyword_Set(surface) THEN BEGIN
      self._theSurface -> GetProperty, Color=theColor

         ; Can you find a widget to be the group leader for PICKCOLOR?

         parents = self._parents -> Get(/All)
         FOR j=0,N_Elements(parents)-1 DO $
            IF Obj_IsA_Valid(parents[j], 'WIDGETATOM') THEN $
            BEGIN
               group_leader = parents[j]
               group_leader -> GetProperty, ID=thisGroupLeader
               BREAK
            ENDIF

      thisColor = PickColor(Group_Leader=thisGroupLeader, CurrentColor=theColor, $
         Cancel=cancelled, Title='Pick SURFACE Color...')
      IF NOT cancelled THEN self -> SetProperty, Surface_Color=Reform(thisColor)
   ENDIF

   IF Keyword_Set(title) THEN BEGIN
      self._plotTitle -> GetProperty, Color=theColor

         ; Can you find a widget to be the group leader for PICKCOLOR?

         parents = self._parents -> Get(/All)
         FOR j=0,N_Elements(parents)-1 DO $
            IF Obj_IsA_Valid(parents[j], 'WIDGETATOM') THEN $
            BEGIN
               group_leader = parents[j]
               group_leader -> GetProperty, ID=thisGroupLeader
               BREAK
            ENDIF

      thisColor = PickColor(Group_Leader=thisGroupLeader, CurrentColor=theColor, $
         Cancel=cancelled, Title='Pick PLOT TITLE Color...')
      IF NOT cancelled THEN self -> SetProperty, Title_Color=Reform(thisColor)
   ENDIF

   IF Keyword_Set(xaxis) THEN BEGIN
      self._xaxis -> GetProperty, Color=theColor

         ; Can you find a widget to be the group leader for PICKCOLOR?

         parents = self._parents -> Get(/All)
         FOR j=0,N_Elements(parents)-1 DO $
            IF Obj_IsA_Valid(parents[j], 'WIDGETATOM') THEN $
            BEGIN
               group_leader = parents[j]
               group_leader -> GetProperty, ID=thisGroupLeader
               BREAK
            ENDIF

      thisColor = PickColor(Group_Leader=thisGroupLeader, CurrentColor=theColor, $
         Cancel=cancelled, Title='Pick X AXIS Color...')
      IF NOT cancelled THEN self -> SetProperty, XAxis_Color=Reform(thisColor)
   ENDIF

   IF Keyword_Set(yaxis) THEN BEGIN
      self._yaxis -> GetProperty, Color=theColor

         ; Can you find a widget to be the group leader for PICKCOLOR?

         parents = self._parents -> Get(/All)
         FOR j=0,N_Elements(parents)-1 DO $
            IF Obj_IsA_Valid(parents[j], 'WIDGETATOM') THEN $
            BEGIN
               group_leader = parents[j]
               group_leader -> GetProperty, ID=thisGroupLeader
               BREAK
            ENDIF

      thisColor = PickColor(Group_Leader=thisGroupLeader, CurrentColor=theColor, $
         Cancel=cancelled, Title='Pick Y AXIS Color...')
      IF NOT cancelled THEN self -> SetProperty, YAxis_Color=Reform(thisColor)
   ENDIF

      IF Keyword_Set(zaxis) THEN BEGIN
      self._zaxis -> GetProperty, Color=theColor

         ; Can you find a widget to be the group leader for PICKCOLOR?

         parents = self._parents -> Get(/All)
         FOR j=0,N_Elements(parents)-1 DO $
            IF Obj_IsA_Valid(parents[j], 'WIDGETATOM') THEN $
            BEGIN
               group_leader = parents[j]
               group_leader -> GetProperty, ID=thisGroupLeader
               BREAK
            ENDIF

      thisColor = PickColor(Group_Leader=thisGroupLeader, CurrentColor=theColor, $
         Cancel=cancelled, Title='Pick Z AXIS Color...')
      IF NOT cancelled THEN self -> SetProperty, ZAxis_Color=Reform(thisColor)
   ENDIF

self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the CATSURFACE object properties.
;
; SYNTAX:
;
;       aCatSurface -> SetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     AXIS_COLOR:           A three-element color triple array, specifying the new axes color.
;
;     BACKGROUND_COLOR:     A three-element color triple array, specifying the new background color.
;
;     ELEVATION_COLORS:     Set this keyword to allow the user to select elevation shading colors
;                           from a COLORTOOL object.
;
;     ELEVATION_OFF:        Set this keyword to turn elevation shading off for the surface.
;
;     ELEVATION_ON:         Set this keyword to turn elevation shading on for the surface.
;
;     HIDDEN_LINES:         Set this keyword to 1 to display with surface with hidden lines removed.
;
;     ORIGINAL_ROTATION:    Set this keyword to return the surface to its original rotation.
;
;     SURFACE_COLOR:        A three-element color triple array, specifying the new surface color.
;
;     SURFACE_STYLE:        Use this keyword to select the surface style. Values are:
;                              0: Dot surface
;                              1: Mesh surface
;                              2: Solid surface
;                              3: Horizontal line surface
;                              4: Vertical line surface
;                              5: Lego surface
;                              6: Filled lego surface
;
;     TITLE_COLOR:          A three-element color triple array, specifying the new title color.
;
;     XAXIS_COLOR:          A three-element color triple array, specifying the new X axis color.
;
;     YAXIS_COLOR:          A three-element color triple array, specifying the new Y axis color.
;
;     ZAXIS_COLOR:          A three-element color triple array, specifying the new Z axis color.
;
;-
;*****************************************************************************************************
PRO CatSurface::SetProperty, $
   AXIS_COLOR=axis_color, $
   BACKGROUND_COLOR=background_color, $
   ELEVATION_COLORS=elevation_colors, $
   ELEVATION_OFF=elevation_off, $
   ELEVATION_ON=elevation_on, $
   HIDDEN_LINES=hidden_lines, $
   ORIGINAL_ROTATION=original_rotation, $
   SURFACE_COLOR=surface_color, $
   SURFACE_STYLE=surface_style, $
   TITLE_COLOR=title_color, $
   XAXIS_COLOR=xaxis_color, $
   YAXIS_COLOR=yaxis_color, $
   ZAXIS_COLOR=zaxis_color, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler


   IF N_Elements(axis_color) NE 0 THEN BEGIN
      self._xAxis -> SetProperty, Color=axis_color
      self._yAxis -> SetProperty, Color=axis_color
      self._zAxis -> SetProperty, Color=axis_color
   ENDIF
   IF N_Elements(background_color) NE 0 THEN self._theView -> SetProperty, Color=background_color
   IF N_Elements(elevation_colors) NE 0 THEN BEGIN
      IF Obj_Valid(self._colortool) THEN BEGIN

         ; Can you find a widget to be the group leader for XCOLORS?

         parents = self._parents -> Get(/All)
         FOR j=0,N_Elements(parents)-1 DO $
            IF Obj_IsA_Valid(parents[j], 'WIDGETATOM') THEN $
            BEGIN
               group_leader = parents[j]
               BREAK
            ENDIF
         IF self._elevshading EQ 0 THEN self -> SetProperty, /Elevation_ON
         self._colortool -> XColors, Group_Leader=group_leader
      ENDIF
   ENDIF
   IF N_Elements(elevation_off) THEN BEGIN
      self._theSurface -> SetProperty, Palette=Obj_New(), Vert_Colors=0
      self._nonRotatingLight -> SetProperty, Hide=0
      self._rotatingLight -> SetProperty, Hide=0
      self._fillLight -> SetProperty, Hide=0
      self._ambientLight -> SetProperty, Hide=0
      self._elevshading = 0
   ENDIF
   IF N_Elements(elevation_on) THEN BEGIN
      s = Size(*self._data, /Dimensions)
      self._theSurface -> SetProperty, Palette=self._thePalette, $
         Vert_Colors=Reform(BytScl(*self._data, /NAN), s[0]*s[1])
      self._nonRotatingLight -> SetProperty, Hide=1
      self._rotatingLight -> SetProperty, Hide=1
      self._fillLight -> SetProperty, Hide=1
      self._ambientLight -> SetProperty, Hide=1
      self._elevshading = 1
   ENDIF
   IF N_Elements(hidden_lines) NE 0 THEN self._theSurface -> SetProperty, HIDDEN_LINES=Keyword_Set(hidden_lines)
   IF N_Elements(original_rotation) NE 0 THEN self._rotatingModel -> SetProperty, Transform=self._origTransform
   IF N_Elements(surface_color) NE 0 THEN BEGIN
      self._theSurface -> SetProperty, Color=surface_color
      self._surfaceColor = surface_color
   ENDIF

      ; Select the surface style.

   IF N_Elements(surface_style) NE 0 THEN BEGIN

      IF self._elevshading THEN BEGIN
         self._nonRotatingLight -> SetProperty, Hide=1
         self._rotatingLight -> SetProperty, Hide=1
         self._fillLight -> SetProperty, Hide=1
         self._ambientLight -> SetProperty, Hide=1
      ENDIF ELSE BEGIN
         self._nonRotatingLight -> SetProperty, Hide=0
         self._rotatingLight -> SetProperty, Hide=0
         self._fillLight -> SetProperty, Hide=0
         self._ambientLight -> SetProperty, Hide=0
      ENDELSE

      theStyles = ['DOTS', 'MESH', 'SOLID', 'HORIZONTAL LINES', 'VERTICAL LINES', 'LEGO', 'FILLED LEGO']
      theStyle = theStyles[0 > surface_style < 6]
      CASE theStyle OF
         'DOTS': self._theSurface -> SetProperty, Style=0
         'MESH': self._theSurface -> SetProperty, Style=1
         'SOLID': self._theSurface -> SetProperty, Style=2, Shading=1
         'HORIZONTAL LINES': self._theSurface -> SetProperty, Style=3
         'VERTICAL LINES': self._theSurface -> SetProperty, Style=4
         'LEGO': self._theSurface -> SetProperty, Style=5
         'FILLED LEGO': self._theSurface -> SetProperty, Style=6, Shading=1
      ENDCASE

   ENDIF

   IF N_Elements(title_color) NE 0 THEN self._plotTitle -> SetProperty, Color=title_color
   IF N_Elements(xaxis_color) NE 0 THEN self._xAxis -> SetProperty, Color=xaxis_color
   IF N_Elements(yaxis_color) NE 0 THEN self._yAxis -> SetProperty, Color=yaxis_color
   IF N_Elements(zaxis_color) NE 0 THEN self._zAxis -> SetProperty, Color=zaxis_color


   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CatObjectView::SetProperty, _EXTRA=extraKeywords

   ; Send a SETPROPERTY_CHANGE message.

   self -> SendMessage, 'SETPROPERTY_CHANGE'

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::SURFACELEVATION
;
; PURPOSE:
;
;       This method is the event handler for surface elevation parameters.
;
; SYNTAX:
;
;       theObject -> SurfaceElevation, event
;
; ARGUMENTS:
;
;     event:  The event structure from the object that caused the event.

;
; KEYWORDS:
;
;      None.
;
;-
;*****************************************************************************************************
PRO CatSurface::SurfaceElevation, event

   @cat_pro_error_handler

   CASE event.name OF

      'ELEVATION SHADING': BEGIN

         event.id -> GetProperty, Value=buttonValue, UValue=newValue
         CASE buttonValue OF
            'Elevation Shading ON': BEGIN
               s = Size(*self._data, /Dimensions)

               self._theSurface->SetProperty, Palette=self._thePalette, $
                  Vert_Colors=Reform(BytScl(*self._data, /NAN), s[0]*s[1])
               self._colorsID -> SetProperty, Sensitive = 1

                  ; Make sure lights are turned off.

               self._nonRotatingLight->SetProperty, Hide=1
               self._rotatingLight->SetProperty, Hide=1
               self._fillLight->SetProperty, Hide=1
               self._ambientLight->SetProperty, Hide=1
               self._scolorID -> SetProperty, Sensitive=0
               event.id -> SetProperty, Value=newValue, UValue=buttonValue

               ENDCASE

            'Elevation Shading OFF': BEGIN
               self._theSurface->SetProperty, Palette=Obj_New(), Vert_Colors=0
               self._colorsID -> SetProperty, Sensitive = 0

                  ; Make sure lights are turned on.

               self._nonRotatingLight->SetProperty, Hide=0
               self._rotatingLight->SetProperty, Hide=0
               self._fillLight->SetProperty, Hide=0
               self._ambientLight->SetProperty, Hide=0
               self._scolorID -> SetProperty, Sensitive=1
               event.id -> SetProperty, Value=newValue, UValue=buttonValue

               ENDCASE
         ENDCASE
         END

      'ELEVATION COLORS': self -> SetProperty, /Elevation_Colors

   ENDCASE

   ; Find the draw widget window so you can get the view and window objects.
   self -> GetProperty, First_Parent=drawWidget
   drawWidget -> GetProperty, Graphics_Tree=thisView, WindowObject=thisWindow
   thisWindow -> Draw, thisView

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::SURFACEOUTPUT
;
; PURPOSE:
;
;       This method is the event handler for surface output.
;
; SYNTAX:
;
;       theObject -> SurfaceOutput, event
;
; ARGUMENTS:
;
;     event:  The event structure from the object that caused the event.

;
; KEYWORDS:
;
;      None.
;
;-
;*****************************************************************************************************
PRO CatSurface::SurfaceOutput, event

   @cat_pro_error_handler

   ; Find the draw widget window so you can get a snapshot of the window.
   self -> GetProperty, First_Parent=drawWidget
   drawWidget -> GetProperty, WindowObject=thisWindow, Graphics_Tree=thisView
   thisWindow -> GetProperty, Image_Data=snapshot

   ; What kind of file is wanted?
   CASE event.name OF

      'JPEG': BEGIN

         filename = Dialog_Pickfile(/Write, File='testcatalyst.jpg')
         IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1, Quality=100
         END

      'TIFF': BEGIN

         filename = Dialog_Pickfile(/Write, File='testcatalyst.tif')
         IF filename NE '' THEN BEGIN

            ; TIFF files should have their Y direction reversed for
            ; compatibility with most other software.

            Write_TIFF, filename, Reverse(snapshot,3)
         ENDIF
         END

      'BMP': BEGIN
         filename = Dialog_Pickfile(/Write, File='testcatalyst.bmp')
         IF filename NE '' THEN Write_BMP, filename, snapshot
         END

      'PNG': BEGIN
         filename = Dialog_Pickfile(/Write, File='testcatalyst.png')
         IF filename NE '' THEN Write_PNG, filename, snapshot
         END

      'EPS': BEGIN
         filename = Dialog_Pickfile(/Write, File='testcatalyst.eps')
         IF filename NE '' THEN BEGIN
            thisWindow -> GetProperty, Dimensions=viewDimensions, Units=viewUnits
            clipboard = Obj_New('IDLgrClipboard', Dimensions=viewDimensions, Unit=viewUnits)
            clipboard -> Draw, thisView, /Postscript, Filename=filename
            Obj_Destroy, clipboard
         ENDIF
         END

   ENDCASE


   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::SURFACEPRINTING
;
; PURPOSE:
;
;       This method is the event handler for surface printing.
;
; SYNTAX:
;
;       theObject -> SurfacePrinting, event
;
; ARGUMENTS:
;
;     event:  The event structure from the object that caused the event.

;
; KEYWORDS:
;
;      None.
;
;-
;*****************************************************************************************************
PRO CatSurface::SurfacePrinting, event

   @cat_pro_error_handler

   ; Does the user really want to print?
   thisPrinter = Obj_New('IDLgrPrinter')
   print_it = Dialog_PrinterSetup(thisPrinter)
   IF NOT print_it THEN BEGIN
      Obj_Destroy, thisPrinter
      RETURN
   ENDIF

   ; Find the draw widget window so you can get the view object.
   self -> GetProperty, First_Parent=drawWidget
   drawWidget -> GetProperty, ViewObject=thisView

   ; Get the current viewport parameters.
   thisView->GetProperty, Dimensions=viewDimensions, Location=viewLocation, Units=viewUnits

   IF event.name NE 'COLOR' THEN BEGIN

      ; Find out the current colors of all the objects.
      thisView->GetProperty, Color=backgroundColor
      self._theSurface->GetProperty, Color=surfaceColor
      self._xAxis->GetProperty, Color=axisColor
      self._yAxis->GetProperty, Color=axisColor
      self._zAxis->GetProperty, Color=axisColor
      self._thePalette->GetProperty, Red=r, Green=g, Blue=b

      ; Change colors to black and white for printing.
      thisView->SetProperty, Color=[255,255,255]
      self._theSurface->SetProperty, Color=[0,0,0]
      self._xAxis->SetProperty, Color=[0,0,0]
      self._yAxis->SetProperty, Color=[0,0,0]
      self._zAxis->SetProperty, Color=[0,0,0]
      self._plotTitle->SetProperty, Color=[0,0,0]
      self._thePalette->LoadCT, 0

   ENDIF

   ; I want the output on the page to have the same aspect ratio
   ; (ratio of height to width) as I see in the display window.
   ; I use the FSC_Surface_Aspect function to calculate the
   ; correct viewport position in normalized coordinates. The
   ; return value of FSC_Surface_Aspect is the position of the
   ; viewport on the output page.
   self._thisWindow->GetProperty, Dimensions=wdims
   self._thisPrinter->GetProperty, Dimensions=pdims
   plotAspect = Float(wdims[1]) / wdims[0]
   printerAspect = Float(pdims[1]) / pdims[0]
   position = Aspect(plotAspect, WindowAspect=printerAspect, Margin=0)

   ; Change the dimensions and postion of the viewport on the output page.
   ; Be sure to use normalized coordinates (units=3).
   thisView->SetProperty, Dimensions=[position[2]-position[0], position[3]-position[1]], $
      Location=[position[0], position[1]], Units=3

   ; Print it. May take a little time. Alert the user.
   Widget_Control, Hourglass=1
   IF event.name EQ 'VECTOR' THEN BEGIN
      thisPrinter->Draw, thisView, Vector=1
   ENDIF ELSE BEGIN
      thisPrinter->Draw, thisView
   ENDELSE
   thisPrinter->NewDocument
   Widget_Control, Hourglass=0

   IF event.name NE 'COLOR' THEN BEGIN

      ; Put everything back the way it was.
      thisView->SetProperty, Color=backgroundColor, Dimensions=[0,0], Location=[0,0]
      self._theSurface->SetProperty, Color=surfaceColor
      self._xAxis->SetProperty, Color=axisColor
      self._yAxis->SetProperty, Color=axisColor
      self._zAxis->SetProperty, Color=axisColor
      self._plotTitle->SetProperty, Color=axisColor
      self._thePalette->SetProperty, Red=r, Green=g, Blue=b

   ENDIF

   ; Restore the viewport parameters.
   thisView->SetProperty, Dimensions=viewDimensions, $
      Location=viewLocation, Units=viewUnits

   ; Destroy the printer object.
   Obj_Destroy, thisPrinter

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::SURFACEPROPERTIES
;
; PURPOSE:
;
;       This method is the event handler for surface properties.
;
; SYNTAX:
;
;       theObject -> SurfaceProperties, event
;
; ARGUMENTS:
;
;     event:  The event structure from the object that caused the event.

;
; KEYWORDS:
;
;      None.
;
;-
;*****************************************************************************************************
PRO CatSurface::SurfaceProperties, event

   @cat_pro_error_handler

   ; Find the draw widget window so you can get the view and window objects.
   self -> GetProperty, First_Parent=drawWidget
   drawWidget -> GetProperty, Graphics_Tree=thisView, WindowObject=thisWindow

   ; What property is wanted?
   CASE event.name OF

      'ORIGINAL_T3D': self._rotatingModel->SetProperty, Transform=self._origTransform

          ; Surface color.

      'SURFACE_COLOR': BEGIN
         topObject = CatGetTopObject(event.id)
         id = topObject -> GetID()
         thisColor = PickColor(Group_Leader=id, $
            Cancel=cancelled, Title='Pick Surface Color...')
         IF NOT cancelled THEN BEGIN
            thisColor = Reform(thisColor, 3, 1)
            self._theSurface->SetProperty, Color=thisColor
            self._surfColor = thisColor
         ENDIF
         END

          ; Background color.

      'BBLACK': thisView->SetProperty, Color=[0,0,0]
      'BWHITE': thisView->SetProperty, Color=[255,255,255]
      'BCHARCOAL': thisView->SetProperty, Color=[80,80,80]
      'BGRAY': thisView->SetProperty, Color=[135, 135, 135]

          ; Axes colors.

      'ABLACK': BEGIN
         self._xAxis->SetProperty, Color=[0,0,0]
         self._yAxis->SetProperty, Color=[0,0,0]
         self._zAxis->SetProperty, Color=[0,0,0]
         END
      'AWHITE': BEGIN
         self._xAxis->SetProperty, Color=[255,255,255]
         self._yAxis->SetProperty, Color=[255,255,255]
         self._zAxis->SetProperty, Color=[255,255,255]
         END
      'AGREEN': BEGIN
         self._xAxis->SetProperty, Color=[0,255,0]
         self._yAxis->SetProperty, Color=[0,255,0]
         self._zAxis->SetProperty, Color=[0,255,0]
         END
      'AYELLOW': BEGIN
         self._xAxis->SetProperty, Color=[255,255,0]
         self._yAxis->SetProperty, Color=[255,255,0]
         self._zAxis->SetProperty, Color=[255,255,0]
         END
      'ANAVY': BEGIN
         self._xAxis->SetProperty, Color=[0, 0, 115]
         self._yAxis->SetProperty, Color=[0, 0, 115]
         self._zAxis->SetProperty, Color=[0, 0, 115]
         END

          ; Title colors.

      'TBLACK': self._plotTitle->SetProperty, Color=[0,0,0]
      'TWHITE': self._plotTitle->SetProperty, Color=[255,255,255]
      'TGREEN': self._plotTitle->SetProperty, Color=[0,255,0]
      'TYELLOW': self._plotTitle->SetProperty, Color=[255,255,0]
      'TNAVY': self._plotTitle->SetProperty, Color=[0,0,115]

         ; Color schemes.

      'B/W': BEGIN
         thisView->SetProperty, Color=[255,255,255]
         self._theSurface->SetProperty, Color=[0,0,0]
         self._surfColor = [0,0,0]
         self._xAxis->SetProperty, Color=[0,0,0]
         self._yAxis->SetProperty, Color=[0,0,0]
         self._zAxis->SetProperty, Color=[0,0,0]
         self._plotTitle->SetProperty, Color=[0,0,0]
         END
      'W/B': BEGIN
         thisView->SetProperty, Color=[0,0,0]
         self._theSurface->SetProperty, Color=[255,255,255]
         self._surfColor = [255,255,255]
         self._xAxis->SetProperty, Color=[255,255,255]
         self._yAxis->SetProperty, Color=[255,255,255]
         self._zAxis->SetProperty, Color=[255,255,255]
         self._plotTitle->SetProperty, Color=[255,255,255]
         END
      'ORIGINAL_COLORS': BEGIN
         thisView->SetProperty, Color=[80,80,80]
         self._theSurface->SetProperty, Color=[255,255,255]
         self._surfColor = [255,255,0]
         self._xAxis->SetProperty, Color=[0,255,0]
         self._yAxis->SetProperty, Color=[0,255,0]
         self._zAxis->SetProperty, Color=[0,255,0]
         self._plotTitle->SetProperty, Color=[0,255,0]
         END

      'DRAG_LOW': BEGIN
         self._dragQuality = 0
         self._dragLowID -> SetProperty, Sensitive=0
         self._dragMedID -> SetProperty, Sensitive=1
         self._dragHighID -> SetProperty, Sensitive=1
         END

      'DRAG_MEDIUM': BEGIN
         self._dragQuality = 1
         self._dragMedID -> SetProperty, Sensitive=0
         self._dragLowID -> SetProperty, Sensitive=1
         self._dragHighID -> SetProperty, Sensitive=1
         END

      'DRAG_HIGH': BEGIN
         self._dragQuality = 2
         self._dragMedID -> SetProperty, Sensitive=1
         self._dragLowID -> SetProperty, Sensitive=1
         self._dragHighID -> SetProperty, Sensitive=0
         END

   ENDCASE

   ;Draw the view
   thisWindow -> Draw, thisView

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::SURFACESTYLE
;
; PURPOSE:
;
;       This method is the event handler for surface style parameters.
;
; SYNTAX:
;
;       theObject -> SurfaceStyle, event
;
; ARGUMENTS:
;
;     event:  The event structure from the object that caused the event.

;
; KEYWORDS:
;
;      None.
;
;-
;*****************************************************************************************************
PRO CatSurface::SurfaceStyle, event

   @cat_pro_error_handler

      ; Make sure lights are turned on.

   self._nonRotatingLight->SetProperty, Hide=0
   self._rotatingLight->SetProperty, Hide=0
   self._fillLight->SetProperty, Hide=0
   self._ambientLight->SetProperty, Hide=0
   self._theSurface->SetProperty, Color=self._surfColor

       ; What style is wanted?

   CASE event.name OF

      'DOTS': self._theSurface->SetProperty, Style=0
      'MESH': self._theSurface->SetProperty, Style=1
      'SOLID': self._theSurface->SetProperty, Style=2, Shading=1
      'XPARALLEL': self._theSurface->SetProperty, Style=3
      'YPARALLEL': self._theSurface->SetProperty, Style=4
      'HIDDEN': BEGIN
          event.id -> GetProperty, Value=buttonValue
          IF buttonValue EQ 'Hidden Lines OFF' THEN BEGIN
             setting = 0
             hlvalue = 'Hidden Lines ON'
          ENDIF ELSE BEGIN
             setting = 1
             hlvalue = 'Hidden Lines OFF'
          ENDELSE
          event.id -> SetProperty, Value=hlvalue
          self._theSurface->SetProperty, Hidden_Lines=setting
          ENDCASE

   ENDCASE

   ; Find the draw widget window so you can get the view and window objects.
   self -> GetProperty, First_Parent=drawWidget
   drawWidget -> GetProperty, Graphics_Tree=thisView, WindowObject=thisWindow
   thisWindow -> Draw, thisView

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::CLEANUP
;
; PURPOSE:
;
;       This is the CATSURFACE object class destructor method.
;
; SYNTAX:
;
;       Called automatically when the object is destroyed.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
PRO CatSurface::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self._trackball
   Obj_Destroy, self._rotatingModel
   Obj_Destroy, self._theSurface
   Obj_Destroy, self._xAxis
   Obj_Destroy, self._yAxis
   Obj_Destroy, self._zAxis
   Obj_Destroy, self._nonRotatingLight
   Obj_Destroy, self._ambientLight
   Obj_Destroy, self._thePalette
   Obj_Destroy, self._plottitle
   Obj_Destroy, self._fillLight
   Obj_Destroy, self._colortool
   Obj_Destroy, self._rotatingLight
   Ptr_Free, self._x
   Ptr_Free, self._y
   Ptr_Free, self._data

   self -> CatObjectView::CLEANUP

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATSURFACE::INIT
;
; PURPOSE:
;
;       This is the CATSURFACE object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       data:               A 2D array of surface data.
;
;       x:                  A vector of X data values.
;
;       y:                  A vector of Y data values.
;
; KEYWORDS:
;
;       COLORTABLE:         Set this keyword to a number between 0 and 40 to select one
;                           of the pre-selected IDL color tables for elevation shading.
;
;       ELEVATION_SHADING: Set this keyword to put elevation shading into effect.
;
;       EXACT:             Set this keyword to a one-, two-,or three-element array to set exact axis
;                          scaling for the X, Y, and Z axes, respectively. If Exact is a one-element array,
;                          all three axes are set to the same value. For example, to set the X axis to
;                          exact scaling and the Y and Z axes to normal scaling, type:
;
;                             IDL> FSC_Surface, Exact=[1,0,0]
;
;       HIDDEN_LINES:      Set this keyword to draw the surface with hidden lines removed. The default
;                          is to show hidden lines.
;
;       LANDSCAPE:         Set this keyword if you want printing of the surface to be in landscape mode.
;
;       POSITION:          A two-, four- or six-element array of normalized (0 to 1) coordinates
;                          used to position the X, Y, and Z axis in the coordinate space. Uses the form
;                          [x0, x1, y0, y1, z0, z1]. In the absence of POSITION information, the Z position
;                          is always [0,1] and the X and Y positions are calculated in a manner that
;                          preserves the aspect ratio of the surface data.
;
;       SHADED_SURFACE:    Set this keyword to set up a shaded surface plot rather than a wire
;                          mesh surface, which is the default.
;
;       TITLE:             A string used as the title of the plot.
;
;       XTITLE:            A string used as the X title of the plot.
;
;       YTITLE:            A string used as the Y title of the plot.
;
;       ZTITLE:            A string used as the Z title of the plot.
;
;       _EXTRA:            This keyword collects otherwise undefined keywords that are passed to the
;                          IDLgrSURFACE initialization routine or for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION CatSurface::INIT, data, x, y, $
   Colortable=colortable, $
   Elevation_Shading=elevation, $
   Exact=exact, $
   Hidden_Lines=hidden_lines, $
   Landscape=landscape, $
   Position=position, $
   Shaded_Surface=shaded, $
   Title=plotTitle, $
   XTitle=xtitle, $
   YTitle=ytitle, $
   ZTitle=ztitle, $
   _Extra=extraKeywords

   @cat_func_error_handler

   ok = self -> CatObjectView::INIT()
   IF NOT ok THEN Message, 'Unable to create CatObjectView superclass object'


   theView = self -> Create_View(data, x, y, $
      Colortable=colortable, $
      Elevation_Shading=elevation, $
      Exact=exact, $
      Hidden_Lines=hidden_lines, $
      Position=position, $
      Shaded_Surface=shaded, $
      Title=plotTitle, $
      XTitle=xtitle, $
      YTitle=ytitle, $
      ZTitle=ztitle, $
      _Extra=extraKeywords)

   IF Obj_Valid(theView) EQ 0 THEN Message, 'Unable to create the VIEW object'
   self -> SetProperty, View=theView

   self -> Report, /Completed

   RETURN, 1

END



;*****************************************************************************************************
;
; NAME:
;       CATSURFACE CLASSNAME DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the CatSurface object.
;
;*****************************************************************************************************
PRO CatSurface__DEFINE, class

   class = { CATSURFACE, $
             _origTransform: FltArr(4,4), $ ; The original transformation matrix.
             _trackball: Obj_New(), $       ; The trackball object.
             _rotatingModel: Obj_New(), $   ; The rotating model of the surface.
             _theSurface: Obj_New(), $      ; The surface object.
             _xAxis:Obj_New(), $            ; The X Axis object.
             _yAxis:Obj_New(), $            ; The Y Axis object.
             _zAxis:Obj_New(), $            ; The Z Axis object.
             _nonRotatingLight:Obj_New(), $ ; The non-rotating light object.
             _rotatingLight: Obj_New(), $   ; The rotating light object.
             _fillLight: Obj_New(), $       ; The fill light object.
             _ambientLight: Obj_New(), $    ; The ambient light object.
             _thePalette: Obj_New(), $      ; The surface color palette.
             _colortable: 0L, $             ; The current color table (for elevation colors).
             _surfaceColor:BytArr(3), $     ; The current color of the surface.
             _r: BytArr(256), $             ; The R values of the current color table.
             _g: BytArr(256), $             ; The G values of the current color table.
             _b: BytArr(256), $             ; The B values of the current color table.
             _data: Ptr_New(), $            ; The original Z data (2D) for the surface.
             _x: Ptr_New(), $               ; The original X data for the surface.
             _y: Ptr_New(), $               ; The original Y data for the surface.
             _plottitle: Obj_New(), $       ; The plot title object.
             _elevshading: 0L, $            ; A flag that indicates whether elevation shading is on or off.
             _colortool: Obj_New(), $       ; A color tool.
             _surfcolor: BytArr(3), $       ; The RGB value of the surface color.
             _colorsID: Obj_New(), $        ; The Load Colors button widget.
             _scolorID: Obj_New(), $        ; The Surface Color button widget.
             _dragQuality: 0, $             ; The draw quality.
             _dragLowID: Obj_New(), $       ; The low draw quality button widget.
             _dragMedID: Obj_New(), $       ; The medium draw quality button widget.
             _dragHighID: Obj_New(), $      ; The high draw quality button widget.
             INHERITS CATOBJECTVIEW $       ; A subclass of the CatObjectView object.
           }

END

