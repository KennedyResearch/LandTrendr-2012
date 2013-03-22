;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEWINDOW
;
; PURPOSE:
;
;       The purpose of this routine is to allow the user to annotate or make
;       measurements on a graphics window. A copy of the graphics window is
;       created and placed into an annotation window. The user can create
;       various kinds of file output from within the annotation window.
;
;       Select the annotation tool from the tool bar at the top of the annotate
;       window. Click and drag in the window to draw the annotation. (The text
;       tool requires you to click in the window to start typing. Be sure to
;       hit a carriage return at the end of typing to "set" the text in the
;       window.) Click anywhere inside the tool to "select" it. Most tools
;       allow you to move and edit the shape of the annotation after selection.
;       Right click inside a selected annotation to access properties of that
;       annotation, such as grouping, moving an annotation forward to backward,
;       deleting the object, etc. Click the "Other Properties" button to access
;       individual properties for the annotations. For example, if you mis-spelled
;       some text, you would click on the text to select it. Right click to access
;       it's select menu, select the Other Properties button, and find the text
;       field in this pop-up dialog to change the spelling of the text.
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
;       Catalyst Applications.
;
; SYNTAX:
;
;       AnnotateWindow, theBackground
;
; ARGUMENTS:
;
;       theBackground:  This can either be the window index number of a graphics window,
;                       or it can be an 8-bit or 24-bit image variable. If this argument
;                       is undefined, the program tries to obtain a background image
;                       from the current graphics window. Note that if the graphics window
;                       contains a black background (the normal situation for many IDL
;                       graphics windows), this will be reproduced *exactly* in PostScript
;                       output. This is NOT the same as most PostScript output. If you
;                       intend to create PostScript output of your annotated window, it
;                       may be better to create graphics windows with white backgrounds.
;
;                       Device, Decomposed=0, Get_Decomposed=currentState
;                       TVLCT, [0, 255], [0,255], [0, 255], !D.Table_Size-3
;                       Plot, findgen(11), Color=!D.Table_Size-3, Background=!D.Table_Size-2
;                       Device, Decomposed=currentState
;                       AnnotateWindow
;
; KEYWORDS:
; 
;      ADD_OBJECTS:     An object, or an array of objects to add to the Annotation Window.
;                       Objects added in this fashion will be destroyed when the Annotate window
;                       is destroyed.
;
;      BG_COLOR:        The name of a background color. This is used only if there is a margin
;                       around the background image. By default, "white". The background color
;                       will always be "white" if the output is sent to a PostScript file. (See list
;                       of color names below.)
;
;      COLOR:           The name of the foreground color for the annotations. By default "Light Coral"
;                       as something that might contrast enough with any background. (See list
;                       of color names below.)
;
;      NOMARGIN:        There is a 10% margin added to the background image, unless this keyword
;                       is set, in which case the background image will fill the annotation window.
;                       
;      OUTPUT_FILENAME: The base name of the output filename used in SAVE AS operations. By default, "annotation".
;
;      XRANGE:          The TapeMeasure tool will report the distance in normalized coordinate space
;                       by default. Use this keyword to specify a two-element array that represents
;                       the minimum and maximum X range of the background image.
;
;      YRANGE:          The TapeMeasure tool will report the distance in normalized coordinate space
;                       by default. Use this keyword to specify a two-element array that represents
;                       the minimum and maximum Y range of the background image.
;
;     _EXTRA:           Any keywords appropriate for the ANNOTATEINTERACTION object.
;
; RESTRICTIONS:
;
;       Requires the Catalyst Library from Fanning Software Consulting, Inc., or a catalyst.sav file
;       to be restored prior to use. A Catalyst save file may be download from Coyote's Guide to IDL
;       Programming. Be sure to download a version compatible with your IDL license.
;
;           http://www.dfanning.com/programs/catalyst.sav (for IDL 6.1 or higher)
;
;       The following color names can be used for the BG_COLOR and COLOR keywords:
;
;           Active            Almond     Antique White        Aquamarine             Beige            Bisque
;             Black              Blue       Blue Violet             Brown         Burlywood        Cadet Blue
;          Charcoal        Chartreuse         Chocolate             Coral   Cornflower Blue          Cornsilk
;           Crimson              Cyan    Dark Goldenrod         Dark Gray        Dark Green        Dark Khaki
;       Dark Orchid          Dark Red       Dark Salmon   Dark Slate Blue         Deep Pink       Dodger Blue
;              Edge              Face         Firebrick      Forest Green             Frame              Gold
;         Goldenrod              Gray             Green      Green Yellow         Highlight          Honeydew
;          Hot Pink        Indian Red             Ivory             Khaki          Lavender        Lawn Green
;       Light Coral        Light Cyan        Light Gray      Light Salmon   Light Sea Green      Light Yellow
;        Lime Green             Linen           Magenta            Maroon       Medium Gray     Medium Orchid
;          Moccasin              Navy             Olive        Olive Drab            Orange        Orange Red
;            Orchid    Pale Goldenrod        Pale Green            Papaya              Peru              Pink
;              Plum       Powder Blue            Purple               Red              Rose        Rosy Brown
;        Royal Blue      Saddle Brown            Salmon       Sandy Brown         Sea Green          Seashell
;          Selected            Shadow            Sienna          Sky Blue        Slate Blue        Slate Gray
;              Snow      Spring Green        Steel Blue               Tan              Teal              Text
;           Thistle            Tomato         Turquoise            Violet        Violet Red             Wheat
;             White            Yellow
;
; EXAMPLE:
;
;       To annotate a medical image:
;
;         filename = Filepath(Subdirectory=['examples','data'], 'mr_knee.dcm')
;         image = Read_DICOM(filename)
;         AnnotateWindow, Rebin(image, 512, 512), XRange=[0,5], YRange=[0,5]
;
;      Remember to right-click on any annotation object to access that objects properties.
;      This is the way you would change a mis-spelled text label, add units to a measurement,
;      change the color of an annotation object, etc.
;
;      Also remember that you MUST type a carriage return when using the Text Tool to
;      "set" the text into the annotation window.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 25 July 2005.
;       Improved documentation header. 11 August 2005. DWF.
;       Added the ability to add annotation objects on initialization. 1 October 2008. DWF.
;       Added the ability to make this a resizeable graphics window. 1 October 2008. DWF.
;-
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright 2005-2008 Fanning Software Consulting
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################
PRO AnnotateWindow, theBackground, $
   ADD_OBJECTS=add_objects, $
   BG_COLOR=bg_Color, $
   COLOR=color, $
   GROUP_LEADER=group_leader, $
   NOMARGIN=nomargin, $
   OUTPUT_FILENAME=output_filename, $
   WINSIZE=winsize, $
   XRANGE=xrange, $
   YRANGE=yrange, $
   _EXTRA=extra

   ; Return to caller upon error.
   ON_ERROR, 2

   ; Check keywords.
   IF N_Elements(bg_color) EQ 0 THEN bg_color = 'White'
   IF N_Elements(color) EQ 0 THEN color = 'Light Coral'
   nomargin = Keyword_Set(nomargin)
   IF N_Elements(xrange) EQ 0 THEN xrange = [0,1]
   IF N_Elements(yrange) EQ 0 THEN yrange = [0,1]

   ; Make sure you have some kind of background to annotate.
   IF N_Elements(theBackground) EQ 0 THEN BEGIN
      theBackground = !D.Window
      IF theBackground LT 0 THEN Message, 'There is no current graphics window to annotate.'
   ENDIF

   ; Is theBackground a window index number or an image?
   ndim = Size(theBackground, /N_Dimensions)
   CASE ndim OF

      0: BEGIN
         IF WindowAvailable(theBackground) EQ 0 THEN $
            Message, 'The specified graphics window is not available to annotate.'
         currentWindow = !D.Window
         WSet, theBackground
         image = TVREAD()
         IF currentWindow GE 0 THEN WSet, currentWindow
         END

      2: BEGIN
         minVal = Min(theBackground, Max=maxval)
         IF (minVal LT 0) OR (maxVal GT 255) THEN image = BytScl(theBackground) ELSE $
            image = theBackground
         END

      3: BEGIN
         image = theBackground
         END

      ELSE: Message, 'The argument to AnnotateWindow has an unexpected dimension. Returning...'

   ENDCASE

   ; Capture the current color table vectors and store in ColorTool.
   TVLCT, r, g, b, /Get
   colors = Obj_New('Colortool')
   colors -> SetProperty, RED=r, GREEN=g, BLUE=b

   ; Create a CATIMAGE out of the background image.
   catimage = Obj_New('CatImage', image, Color_Object=colors, /Keep_Aspect, /NoInterp)

   ; Calculate the size of the window. If NOMARGIN is set, or if this is a graphics
   ; window, then the size of the window is the size of the background. Otherwise,
   ; add a 10% margin around the image.
   catimage -> GetProperty, XSize=xsize, YSize=ysize
   IF nomargin THEN BEGIN
      position = [0.0, 0.0, 1.0, 1.0]
   ENDIF ELSE BEGIN
      xsize = xsize * 1.2
      ysize = ysize * 1.2
      position = [0.1, 0.1, 0.9, 0.9]
   ENDELSE
   catimage -> SetProperty, Position=position

   ; Find the size of the display.
   s = Get_Screen_Size()
   IF s[0] GT 1600 THEN s[0] = s[0] / 2
   
   ; Don't want to exceed the screen size.
   aspect = float(ysize)/xsize
   IF xsize GT s[0] OR ysize GT s[1] THEN BEGIN
      IF aspect GE 1 THEN BEGIN
        ysize = s[1] - 50
        xsize = Round(ysize / aspect)
      ENDIF ELSE BEGIN
        xsize = s[0] - 50
        ysize = Round(xsize * aspect)
      ENDELSE
   ENDIF

   ; Do we need scroll bars on the TLB?
   IF (xsize GE s[0]) OR (ysize GE s[1]) THEN BEGIN
      tlb = Obj_New('TopLevelBase', X_Scroll_Size=s[0]-50, Y_Scroll_Size=s[1]-25, $
         Title='Annotation Window', MBAR=menuID, XPad=0, YPad=0, Space=0, Column=1, $
         GROUP_LEADER=group_leader, /SIZE_EVENTS)
   ENDIF ELSE BEGIN
      tlb = Obj_New('TopLevelBase', Title='Annotation Window', MBAR=menuID, XPad=0, YPad=0, $
         Space=0, Column=1, GROUP_LEADER=group_leader, /SIZE_EVENTS)
   ENDELSE

   ; Create a toolbar base.
   toolbar = Obj_New('BaseWidget', tlb, Row=1)

   ; Create a draw widget.
   drawID = Obj_New('SelectableDrawWidget', tlb, XSize=xsize, YSize=ysize, $
      /Select_Events, Initial_Color=bg_color, /Notify_Realize)

   ; Add the image to the draw widget.
   drawID -> Add, catimage

   ; Create an annotation layer.
   layer = Obj_New('CatLayer')
   
   ; If there are any objects to add, do it here.
   IF N_Elements(add_objects) NE 0 THEN BEGIN
      FOR j=0,N_Elements(add_objects)-1 DO layer -> Add, add_objects[j]
   ENDIF

   ; Create a coordinate object for the annotate interaction.
   IF N_Elements(xrange) EQ 0 THEN xrange = [0,1]
   IF N_Elements(yrange) EQ 0 THEN yrange = [0,1]
   coords = Obj_New('CatCoord', XRange=xrange, YRange=yrange, Position=position)

   ; Create the annotation interaction.
   annotate = Obj_New('AnnotateInteraction', drawID, Layer=layer, Name='Annotation Window', $
      Coord_Object=coords, Color=color, OUTPUT_FILENAME=output_filename, _EXTRA=extra)

   ; Put the annotation control panel in the toolbar.
   annotate -> ControlPanel, toolbar, ROW=1

   ; Put in a SAVE File button.
   saveasID = Obj_New('ButtonWidget', toolbar, Value='Save Window As...', /MENU)
   button = Obj_New('ButtonWidget', saveasID, Value='BMP File', Name='BMP', Event_Object=annotate)
   button = Obj_New('ButtonWidget', saveasID, Value='JPEG File', Name='JPEG', Event_Object=annotate)
   button = Obj_New('ButtonWidget', saveasID, Value='PNG File', Name='PNG', Event_Object=annotate)
   button = Obj_New('ButtonWidget', saveasID, Value='PostScript File', Name='POSTSCRIPT', Event_Object=annotate)
   button = Obj_New('ButtonWidget', saveasID, Value='TIFF File', Name='TIFF', Event_Object=annotate)

   ; Realize everything and get it started.
   tlb -> Draw, /CENTER
   
   ; Add some information to the UVALUE of the tlb.
   tlb -> SetProperty, EVENT_OBJECT=annotate

   ; Start the annotation.
   annotate -> SetDisplay

   ; Add the annotation to the container of an object that will be destroyed,
   ; so it will also be destroyed.
   tlb -> AddToTrash, annotate

END