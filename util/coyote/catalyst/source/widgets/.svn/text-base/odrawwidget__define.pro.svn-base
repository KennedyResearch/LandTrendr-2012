;*****************************************************************************************************
;+
; NAME:
;       ODRAWWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to implement an object graphics draw widget as an object.
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
;       Object widgets.
;
; SYNTAX:
;
;       oDrawWidget = Obj_New("ODrawWidget", theParent)
;
; SUPERCLASSES:
;
;       DRAWWIDGET
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { ODRAWWIDGET, $
;             INHERITS DrawWidget }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W.Fanning, 28 June 2003.
;       Fixed a problem involving getting the graphics tree in GetProperty method. 19 July 2004. DWF.
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
;       ODRAWWIDGET::ADD
;
; PURPOSE:
;
;       This method adds an IDLgrView or IDLgrScene object to the ODrawWidget container.
;       Unlike direct graphics DrawWidget objects, ODrawWidgets do NOT draw all of the
;       objects in their container. In fact, only ONE object can be added to the container.
;       Subsequent ADD method calls will replace the IDLgrView object with another.
;
;       While the draw widget is adding the object, it will also regester for any
;       SETPROPERTY_CHANGE messages.
;
; SYNTAX:
;
;       thisDrawObj -> Add, viewObject
;
; ARGUMENTS:
;
;       viewObject:    The IDLgrView or IDLgrScene object to add to the draw widget container.
;
; KEYWORDS:
;
;       EXCLUSIVE:     This keyword is used only if HANDLE_EVENTS is also set. If EXCLUSIVE
;                      is set the EXCLUSIVE_EVENT_OBJECT property is set instead of the
;                      EVENT_OBJECT property.
;
;       HANDLE_EVENTS: If this keyword is set, the object that is being added will set the
;                      EVENT_OBJECT property of the oDrawWidget, and all subsequent
;                      draw widget events will be sent to the EVENT_HANDLER method of the
;                      object being added.
;
;-
;*****************************************************************************************************
PRO ODrawWidget::Add, viewObject, EXCLUSIVE=exclusive, HANDLE_EVENTS=handle_events

   @cat_pro_error_handler


      ; If this is not a CATOBJECTVIEW object, don't add it. (DWF: May want to change this later.)

   IF Obj_Isa_Valid(viewObject, 'CATOBJECTVIEW') EQ 0 THEN $
      Message, 'Only CATOBJECTVIEW objects can be added to ODrawWidget objects.'

      ; Add the object to this container. Only one object in the container.
      ; (DWF: May want to change this later.)

   oldObject = self -> Get(/All, Count=count)
   IF count GT 0 THEN self -> WIDGETATOM::Remove, oldObject
   self -> WIDGETATOM::Add, viewObject

      ; Do you want the object you are adding to handle draw widget events?

   IF Keyword_Set(handle_events) THEN BEGIN
      IF Keyword_Set(exclusive) THEN self -> SetProperty, EXCLUSIVE_EVENT_OBJECT=viewObject ELSE $
         self -> SetProperty, EVENT_OBJECT=viewObject
   ENDIF

      ; Register for SETPROPERTY_CHANGE messages.

   viewObject -> RegisterForMessage, self, 'SETPROPERTY_CHANGE'

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ODRAWWIDGET::COPY
;
; PURPOSE:
;
;       This method copies the contents of the draw widget to an image variable.
;
; RETURN_VALUE:
;
;       image:     A (3, winXsize, winYsize) or (4, winXsize, winYsize) array (depending
;                  upon whether an alpha channel is present or not) image.
;
; SYNTAX:
;
;       image = oDrawWidget -> Copy()
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
FUNCTION ODrawWidget::Copy

   @cat_func_error_handler

      ; Check we have a valid window to copy from.

   WIDGET_CONTROL, self._id, GET_VALUE=windowObject
   IF Obj_Valid(windowObject) EQ 0 THEN Message, 'Draw widget has not been realized yet. Cannot copy.'

   ; Capture the image and return it.

   windowObject -> GetProperty, Image_Data=snapshot

   self -> Report, /Completed

   RETURN, snapshot

END


;*****************************************************************************************************
;+
; NAME:
;       ODRAWWIDGET::DRAW
;
; PURPOSE:
;
;       This method draws the contents of the draw widget object's container in the draw widget window.
;       It does this by calling the DRAW methods of any objects found in its container object.
;;
; SYNTAX:
;
;       thisDrawObj -> Draw
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       HOURGLASS:        Set this keyword to enable the hourglass cursor for the draw operation.
;
;       _EXTRA:           Any extra keywords appropriate for superclass DRAW methods.
;-
;*****************************************************************************************************
PRO ODrawWidget::Draw, $
   HOURGLASS=hourglass, $
   _EXTRA=extraKeywords

      ; Set up the error handler
   @cat_pro_error_handler

      ; If drawing of contents has been inhibited, return immediately.
   IF (self._noDraw) THEN RETURN

      ; If we haven't got a valid window ID yet, issue error.

   Widget_Control, self._ID, Get_Value=windowObject
   IF Obj_Valid(windowObject) EQ 0 THEN Message, 'Draw widget does not have a valid window object.'

      ; Check keywords.
   hourglass = Keyword_Set(hourglass)

      ; Enable the hourglass mouse cursor, if needed.
   IF hourglass THEN WIDGET_CONTROL, /HOURGLASS

      ; Draw the contents of this window

   viewObject = self -> Get(Position=0)
   viewObject -> GetProperty, View=theView
   IF Obj_Valid(theView) THEN windowObject -> Draw, theView

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       ODRAWWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to obtain the DrawWidget object's properties
;
; SYNTAX:
;
;       aDrawWidget -> GetProperty, RESOLUTION=resolution
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       GRAPHICS_TREE:     Set this keyword to an named variable to retrieve the IDLgrView or
;                          IDLgrScene object contained in the draw widget.
;
;       RESOLUTION:        The pixel resolution [xres, yres] of the window measured in centimeters per pixel.
;
;       SCREEN_DIMENSIONS: A two-element array of the form [height, width] specifying the dimensions
;                          of the display device in pixel or device coordinates.
;
;       WINDOWOBJECT:      The window object associated with the the draw widget.
;
;       VIEWOBJECT:        Set this keyword to return the CATOBJECTVIEW object contained in the
;                          oDrawWidget container.
;
;       ZBUFFER_DATA:      Set this keyword to a named variable that will contain a floating array
;                          representing the z-buffer currently in the window. The returned array
;                          will have dimensions of [winXsize, winYsize].
;
;       _REF_EXTRA:        Any keywords appropriate for the superclass GetProperty methods.
;-
;*****************************************************************************************************
PRO ODrawWidget::GetProperty, $
   GRAPHICS_TREE=graphics_tree, $
   RESOLUTION=resolution, $
   SCREEN_DIMENSIONS=screen_dimensions, $
   VIEWOBJECT = viewObject, $
   WINDOWOBJECT=windowObject, $
   ZBUFFER_DATA=zbuffer_data, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (WIDGET_INFO (self._id, /VALID_ID)) THEN $
   BEGIN
      WIDGET_CONTROL, self._id, GET_VALUE=windowObject
      IF Obj_Valid(windowObject) THEN BEGIN
         IF Arg_Present(resolution) THEN windowObject -> GetProperty, RESOLUTION=resolution
         IF Arg_Present(screen_dimensions) THEN windowObject -> GetProperty, SCREEN_DIMENSIONS=screen_dimensions
         IF Arg_Present(zbuffer_data) THEN windowObject -> GetProperty, ZBUFFER_DATA=zbuffer_data
      ENDIF
   ENDIF

   IF Arg_Present(viewObject) THEN viewObject = self -> Get(Position=0)

   IF Arg_Present(graphics_tree) THEN BEGIN
      viewObject = self -> Get(Position=0)
      viewObject -> GetProperty, View=graphics_tree
   ENDIF

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> DRAWWIDGET::GetProperty, _EXTRA=extraKeywords

END



;*****************************************************************************************************
;+
; NAME:
;       ODRAWWIDGET::MESSAGEHANDLER
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
PRO ODrawWidget::MessageHandler, title, SENDER=sender, DATA=data

      ; Initialise the error handler
   @cat_pro_error_handler
   CASE title OF

      ; Redraw the window if anything changes.
      'SETPROPERTY_CHANGE': self -> Draw

      ELSE:

   ENDCASE

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       ODRAWWIDGET::Output
;
; PURPOSE:
;
;       This method will create graphic output files of the draw widget contents. The default
;       is to create JPEG output files unless other output is selected via keywords.
;
; SYNTAX:
;
;       drawWidget -> Output
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;      BMP:        Set this keyword to create BMP output file.
;
;      JPEG:       Set this keyword to create JPEG output file.
;
;      FILENAME:   Set this keyword to the name of the output file.
;
;      NODIALOG:   Set this keyword if you want to write the file directly,
;                  without giving the user a chance to change the filename.
;
;      PNG:        Set this keyword to create PNG output file.
;
;      TIFF:       Set this keyword to create TIFF output file.
;
;      POSTSCRIPT: Set this keyword to create encapsulated PostScript output file.
;
;      _EXTRA:     Any keywords appropriate for the WRITE_*** routines.
;
;-
;*****************************************************************************************************
PRO ODrawWidget::Output, $
   BMP=bmp, $
   FILENAME=filename, $
   NODIALOG=nodialog, $
   JPEG=jpeg, $
   PNG=png, $
   POSTSCRIPT=postscript, $
   TIFF=tiff, $
   _Extra=extraKeywords

   @cat_pro_error_handler

   output = 'JPEG'
   IF Keyword_Set(jpeg) THEN output = 'JPEG'
   IF Keyword_Set(tiff) THEN output = 'TIFF'
   IF Keyword_Set(bmp) THEN output = 'BMP'
   IF Keyword_Set(png) THEN output = 'PNG'
   IF Keyword_Set(postscript) THEN output = 'POSTSCRIPT'

   nodialog = Keyword_Set(nodialog)

      ; Get a snapshop of window contents. (TVRD equivalent.)

   Widget_Control, self._ID, Get_Value=windowObject
   IF Obj_Valid(windowObject) EQ 0 THEN Message, 'Draw widget does not have a valid window object.'
   windowObject -> GetProperty, Image_Data=snapshot

      ; What kind of file is wanted?

   CASE output OF

      'JPEG': BEGIN

         IF N_Elements(filename) EQ 0 THEN filename='output.jpg'
         IF nodialog EQ 0 THEN filename = Dialog_Pickfile(/Write, File=filename)
         IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1, Quality=100, _Extra=extraKeywords
         END


      'TIFF': BEGIN

         IF N_Elements(filename) EQ 0 THEN filename='output.tif'
         IF nodialog EQ 0 THEN filename = Dialog_Pickfile(/Write, File=filename)
         IF filename NE '' THEN BEGIN

            ; TIFF files should have their Y direction reversed for
            ; compatibility with most other software.

            Write_TIFF, filename, Reverse(snapshot,3), _Extra=extraKeywords
         ENDIF
         END

      'BMP': BEGIN
         IF N_Elements(filename) EQ 0 THEN filename='output.bmp'
         IF nodialog EQ 0 THEN filename = Dialog_Pickfile(/Write, File=filename)
         IF filename NE '' THEN Write_BMP, filename, snapshot, _Extra=extraKeywords
         END

      'PNG': BEGIN
         IF N_Elements(filename) EQ 0 THEN filename='output.png'
         IF nodialog EQ 0 THEN filename = Dialog_Pickfile(/Write, File=filename)
         IF filename NE '' THEN Write_PNG, filename, snapshot, _Extra=extraKeywords
         END

      'POSTSCRIPT': BEGIN
         IF N_Elements(filename) EQ 0 THEN filename='output.eps'
         IF nodialog EQ 0 THEN filename = Dialog_Pickfile(/Write, File=filename)
         IF filename NE '' THEN BEGIN
            viewObject = self -> Get(Position=0)
            viewObject -> GetProperty, View=theView
            windowObject -> GetProperty, Dimensions=viewDimensions, Units=viewUnits
            clipboard = Obj_New('IDLgrClipboard', Dimensions=viewDimensions, Unit=viewUnits)
            clipboard->Draw, theView, /Postscript, Filename=filename
            Obj_Destroy, clipboard
         ENDIF
         END

   ENDCASE

 END



;*****************************************************************************************************
;+
; NAME:
;       ODRAWWIDGET::PRINT
;
; PURPOSE:
;
;       This method will send the contents of the draw widget to the default printer.
;
; SYNTAX:
;
;       drawWidget -> Print
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;      LANDSCAPE:  Set this keyword to 1 to select landscape output. The default in portrait output.
;
;      VECTOR:     Set this keyword to draw using vector output. The output is not guaranteed to be
;                  correct, but this may be a faster option.
;
;      _EXTRA:     Any keywords appropriate for the IDLgrPrinter object.
;
;-
;*****************************************************************************************************
PRO ODrawWidget::Print, $
   LANDSCAPE=landscape, $
   VECTOR=vector, $
   _Extra=extraKeywords

   @cat_pro_error_handler

      ;Create a printer object.

   IF Obj_Valid(self._thePrinter) EQ 0 THEN self._thePrinter = Obj_New('IDLgrPrinter')
   IF N_Elements(landscape) NE 0 THEN self._thePrinter -> SetProperty, Landscape=Keyword_Set(landscape)

      ; Does the user really want to print?

   IF Dialog_PrinterSetup(self._thePrinter) EQ 0 THEN RETURN

      ; Get the current viewport parameters.

   Widget_Control, self._ID, Get_Value=windowObject
   IF Obj_Valid(windowObject) EQ 0 THEN Message, 'Draw widget does not have a valid window object.'
   viewObject = self -> Get(Position=0)
   viewObject -> GetProperty, View=theView
   theView -> GetProperty, Dimensions=viewDimensions, Location=viewLocation, Units=viewUnits

      ; I want the output on the page to have the same aspect ratio
      ; (ratio of height to width) as I see in the display window.
      ; I use the Aspect function to calculate the correct viewport
      ; position in normalized coordinates. The return value of Aspect
      ; is the position of the viewport on the output page.

   windowObject -> GetProperty, Dimensions=wdims
   self._thePrinter -> GetProperty, Dimensions=pdims
   plotAspect = Float(wdims[1]) / wdims[0]
   printerAspect = Float(pdims[1]) / pdims[0]
   position = Aspect(plotAspect, WindowAspect=printerAspect, Margin=0)

      ; Change the dimensions and postion of the viewport on the output page.
      ; Be sure to use normalized coordinates (units=3).

   theView -> SetProperty, Dimensions=[position[2]-position[0], position[3]-position[1]], $
      Location=[position[0], position[1]], Units=3

      ; Print it. May take a little time. Alert the user.

   Widget_Control, Hourglass=1
   IF Keyword_Set(vector) THEN BEGIN
      self._thePrinter -> Draw, theView, Vector=1
   ENDIF ELSE BEGIN
      self._thePrinter -> Draw, theView
   ENDELSE
   self._thePrinter -> NewDocument
   Widget_Control, Hourglass=0

      ; Restore the viewport parameters.

   theView -> SetProperty, Dimensions=viewDimensions, Location=viewLocation, Units=viewUnits

   self -> Report, /Completed

 END



;*****************************************************************************************************
;+
; NAME:
;       ODRAWWIDGET::RESIZE
;
; PURPOSE:
;
;       This method resizes the canvas area of the draw widget.
;;
; SYNTAX:
;
;       thisDrawObj -> Resize, xsize, ysize
;
; ARGUMENTS:
;
;       XSIZE:  The new X size of the canvas area of the draw widget, in pixels.
;
;       YSIZE:  The new Y size of the canvas area of the draw widget, in pixels.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO ODrawWidget::Resize, xsize, ysize

   @cat_pro_error_handler

   IF N_Elements(xsize) EQ 0 THEN Message, "XSIZE parameter is missing."
   IF N_Elements(ysize) EQ 0 THEN Message, "YSIZE parameter is missing."

   ; Resize the window.
   WIDGET_CONTROL, self._id, GET_VALUE=windowObject
   IF Obj_Valid(windowObject) THEN windowObject -> SetProperty, Dimensions=[xsize, ysize]


   ; Get the CatObjectView object from the container.
   viewObject = self -> Get(Position=0)
   IF Obj_Valid(viewObject) THEN BEGIN
      viewObject -> GetProperty, View=theView
      viewObject -> Resize, xsize, ysize
   ENDIF
   IF Obj_Valid(theView) THEN BEGIN
      WIDGET_CONTROL, self._id, GET_VALUE=windowObject
      windowObject -> Draw, theView
   ENDIF

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::SET_WINDOW
;
; PURPOSE:
;
;       This method simply overwrites the DRAWWIDGET Set_Window method to do nothing.
;
; SYNTAX:
;
;       thisDrawObj -> Set_Window
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None
;-
;*****************************************************************************************************
PRO ODrawWidget::Set_Window
END



;*****************************************************************************************************
;+
; NAME:
;       ODRAWWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the DRAWWIDGET object class destructor method.
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
;       None.
;-
;*****************************************************************************************************
PRO ODrawWidget::CLEANUP

   @cat_pro_error_handler

   ; Is there a graphics tree object in the self container. If so, destroy it.

   theView = self -> Get(Position=0)
   IF Obj_Valid(theView) THEN Obj_Destroy, theView
   Obj_Destroy, self._thePrinter

   self -> DRAWWIDGET::CLEANUP

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       ODRAWWIDGET::INIT
;
; PURPOSE:
;
;       This is the ODRAWWIDGET object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       theParent:      An object reference to a WIDGETATOM-subclassed object.
;
; KEYWORDS:
;
;       APP_SCROLL:      The "application scroll" keyword. See Widget_Draw documentation for details.
;
;       BUTTON_EVENTS:   Set this keyword to enable widget button events in the draw widget.
;
;       COLOR_MODEL:     Set this keyword to 1 to use INDEXED color for associated IDLgrWindow.
;                        Set to 0 (the default) for RBG color model.
;
;       EXPOSE_EVENTS:   Set this keyword to enable widget expose events in the draw widget.
;
;       FRAME:           Create a frame this many pixels wide around the widget.
;
;       GRAPHICS_TREE:   Set this keyword to an IDLgrView or IDLgrScene object to be displayed
;                        in the draw widget.
;
;       KEYBOARD_EVENTS: Set this keyword to enable keyboard events for this widget. Setting
;                        the value to 1 will enable "normal" keys. Setting the value to 2 will enable
;                        modifier keys. See the IDL documenation for WIDGET_DRAW for details.
;
;       MOTION_EVENTS:   Set this keyword to enable widget motion events in the draw widget.
;
;       NODRAW:          Set this keyword to prohibit re-drawing via the DRAW method.
;
;       PARENT:          An object reference to a WIDGETATOM-subclassed object.
;
;       RENDERER:        Set this keyword to 1 to use IDL software OpenGL library. Set to 0
;                        (the default) to use hardware OpenGL rendering (if available).
;
;       RETAIN:          Set this keyword to determine how backing store is handled. See IDL WIDGET_DRAW
;                        documentation for details. Set to 1 by default for Windows machines and to 2 otherwise.
;
;       SCR_XSIZE:       Set the screen X size of the base to this many pixels. (Use discouraged.)
;
;       SCR_YSIZE:       Set the screen Y size of the base to this many pixels. (Use discouraged.)
;
;       SCROLL:          Set this keyword to add scroll bars to the draw widget.
;
;       UNITS:           The units for measurments. The default is 0 for pixels. Other values are
;                        1 for inches, and 2 for centimeters.
;
;       VIEWPORT_EVENTS: Set this keyword to enable widget viewport scroll events in the draw widget
;
;       X_SCROLL_SIZE:   The X size (pixels) of the scrollable window area.
;
;       XOFFSET:         The horizontal space (pixels) from upper left corner of the display.
;
;       XSIZE:           The X size of the widget. (300 pixels by default.)
;
;       Y_SCROLL_SIZE:   The Y size (pixels) of the scrollable window area
;
;       YOFFSET:         The vertical space (pixels) from upper left corner of the display.
;
;       YSIZE:           The Y size of the widget. (300 pixels by default.)

;       _EXTRA:          Any keyword appropriate for the superclass INIT methods.
;-
;*****************************************************************************************************
FUNCTION ODrawWidget::INIT, parent, $
   APP_SCROLL=app_scroll, $
   BUTTON_EVENTS=button_events, $
   COLOR_MODEL=color_model, $
   EXPOSE_EVENTS=expose_events, $
   FRAME=frame, $
   GRAPHICS_TREE=graphics_tree, $
   KEYBOARD_EVENTS=keyboard_events, $
   MOTION_EVENTS=motion_events, $
   NODRAW=nodraw, $
   RENDERER=renderer, $
   RETAIN=retain, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   SCROLL=scroll, $
   UNITS=units, $
   VIEWPORT_EVENTS=viewport_events, $
   X_SCROLL_SIZE=x_scroll_size, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   Y_SCROLL_SIZE=y_scroll_size, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   @cat_func_error_handler

      ; Set defaults.

   color_model = Keyword_Set(color_model)
   renderer = Keyword_Set(renderer)
   graphics_level = 2 ; Object graphics draw widget window.
   IF N_Elements(xsize) EQ 0 THEN xsize = 400
   IF N_Elements(ysize) EQ 0 THEN ysize = 400
   IF N_Elements(retain) EQ 0 THEN retain=(!Version.OS_Family EQ 'Windows')?1:2

      ; If the parent is not specified, the create a TLB to use as a parent.

   IF N_ELEMENTS (parent) EQ 0 THEN $
   BEGIN
      self._drawBase = OBJ_NEW ('TopLevelBase', NAME='Auto Top-Level Base')
      parent = self._drawBase
   ENDIF

      ; If the parent is specified but invalid, warn and exit.

   IF (NOT obj_isa_valid (parent, 'BaseWidget')) THEN $
      Message, 'Parent object is not a BASEWIDGET type object.

      ; Otherwise, get the parents widget ID.

   parent -> WidgetAtom::GetProperty, ID=parentID

      ; Create the draw widget, adding it to the widget hierarchy.

   id = WIDGET_DRAW (parentID,   $
      APP_SCROLL=app_scroll, $
      BUTTON_EVENTS=button_events, $
      COLOR_MODEL=color_model, $
      EXPOSE_EVENTS=expose_events, $
      FRAME=frame, $
      GRAPHICS_LEVEL=graphics_level, $
      KEYBOARD_EVENTS=keyboard_events, $
      MOTION_EVENTS=motion_events, $
      RENDERER=renderer, $
      RETAIN=retain, $
      SCR_XSIZE=scr_xsize, $
      SCR_YSIZE=scr_ysize, $
      SCROLL=scroll, $
      UNITS=units, $
      VIEWPORT_EVENTS=viewport_events, $
      X_SCROLL_SIZE=x_scroll_size, $
      XOFFSET=xoffset, $
      XSIZE=xsize, $
      Y_SCROLL_SIZE=y_scroll_size, $
      YOFFSET=yoffset, $
      YSIZE=ysize, $
     _EXTRA=extraKeywords)

      ; Call superclass INIT method, report and return success.

   IF  NOT self -> WidgetAtom::INIT (parent, id, _EXTRA=extraKeywords) THEN $
     Message, 'WidgetAtom::INIT method failed to complete.'

      ; Add a graphics tree object to the container.

   IF N_Elements(graphics_tree) NE 0 THEN self -> Add, graphics_tree

      ; If we created a new tlb, realize it now.

   IF OBJ_VALID (self._drawBase) THEN BEGIN
      self._drawBase -> Draw
   ENDIF

      ; Decrement the amount of video ram that is available.

;   self._videoRAM = xsize * ysize
;   videoMemory = CatGetDefault ('VideoMemory', Success=ok)
;   IF (ok) THEN CatSetDefault, 'VideoMemory',  videoMemory - (self._videoRAM / 1024)

   self -> Report, /Completed

   RETURN, 1
END



;*****************************************************************************************************
;
; NAME:
;       ODRAWWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the )DRAWWIDGET object class definition.
;       The ODRAWWIDGET object is subclassed from the DRAWWIDGET object.
;
;*****************************************************************************************************
PRO ODrawWidget__DEFINE, class

   class = { ODRAWWIDGET, $
             _thePrinter: Obj_New(), $    ; A printer object for printing draw widget contents.
             INHERITS DrawWidget }

END