;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to implement a draw widget as an object.
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
;       drawWidget = Obj_New("DrawWidget", theParent)
;
; SUPERCLASSES:
;
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; MESSAGES:
;
;       RESIZEDDRAWWIDGET:  If the XSIZE or YSIZE keywords are used with the SETPROPERTY method, a
;                           RESIZEDRAWWIDGET message is sent to registered users (often a pixmap).
;                           The DATA passed with the message is a two-element array containing the
;                           new X and Y size of the draw widget canvas.
;
;       DRAWWIDGETREALIZED: When the Draw Widget is realized a DRAWWIDGETREALIZED message is sent to
;                           registered users.
;
;       DRAWWIDGET_DRAW:    When the Draw Widget draws itself a DRAWWIDGET_DRAW message is sent to
;                           registered users. If it is available, the REQUESTER of the DRAW is sent
;                           with the message as the DATA of the message.
;
; CLASS_STRUCTURE:
;
;   class = { DrawWidget,          $
;             _coords    : OBJ_NEW(), $   ; A CATCOORD object of some type.
;             _colors    : OBJ_NEW(), $   ; A COLORTOOL object for setting up color tables.
;             _drawBase   : OBJ_NEW (), $ ; A top-level base widget object, if required.
;             _noDraw     : 0B, $         ; Flag that, if set, inhibits draw method calls.
;             _videoRAM   : 0L, $         ; amount of video memory consumed by the widget
;             _psconfig   : OBJ_NEW(), $  ; A PostScript configuration object.
;             _eraseWindow: 0B, $         ; A flag that indicates the window should be erased before drawing.
;             _refreshbuffer: OBJ_NEW(), $; The identifier of a PIXMAPWIDGET that can carry out window refresh.
;             INHERITS WidgetAtom $       ; INHERITS WIDGETATOM capabilities.
;             }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W.Fanning, 28 June 2002.
;       Added a RESIZEDRAWWIDGET message in SetProperty method. 7 December 2004. DWF.
;       I made the default background for draw widget's white to facilitate PostScript
;         output. Default annotation colors are not black. 11 Dec 2004. DWF.
;       Added refresh buffer capability along with REFRESH method. 23 January 2005. DWF.
;       Changed the INITIALCOLOR back to "black". Tired of fighting it... 5 July 2005. DWF.
;       Fixed a problem that occurred when setting XSIZE, but not YSIZE. 29 Aug 2005. DWF.
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
;       DRAWWIDGET::ADD
;
; PURPOSE:
;
;       This method adds an object to the DrawWidget object container. The DrawWidget
;       object's DRAW method is called automatically, unless adviced otherwise.
;
; SYNTAX:
;
;       thisDrawObj -> Add, object
;
; ARGUMENTS:
;
;       object:      The object to add to the draw widget container.
;
; KEYWORDS:
;
;       DRAW:        If this keyword is set, the DRAW method is called as soon as the object has
;                    been added to the DrawWidget object container.
;
;       SETWINDOW:   Set this keyword, if you wish to add this draw widget as the display window
;                    for the object being added.
;
;       USE_COLORS:  If this keyword is set, replace the COLORTOOL object (if any) in the object
;                    being added, with the one associated with this DrawWidget object.
;
;       USE_COORDS:  If this keyword is set, replace the CATCOORD object (if any) in the object
;                    being added, with the one associated with this DrawWidget object.
;
;       _EXTRA:      Any keyword appropriate for the superclass ADD methods.
;
;-
;*****************************************************************************************************
PRO DrawWidget::Add, object, $
   DRAW=draw , $
   SETWINDOW=setwindow, $
   USE_COLORS=use_colors, $
   USE_COORDS=use_coords, $
   _Extra=extraKeywords

   @cat_pro_error_handler

   ; Add the object to this container.
   self -> WidgetAtom::Add, object, _Extra=extraKeywords

   ; Use the ColorTool object of the draw widget?
   IF Keyword_Set(use_colors) THEN BEGIN
      IF Obj_Isa_Valid(object, 'CATDATAATOM') THEN BEGIN
         IF Obj_Valid(self._colors) THEN object -> SetProperty, Color_Object=self._colors
      ENDIF
   ENDIF

   ; Use the CatCoord object of the draw widget?
   IF Keyword_Set(use_coords) THEN BEGIN
      IF Obj_Isa_Valid(object, 'CATDATAATOM') THEN BEGIN
         IF Obj_Valid(self._coords) THEN object -> SetProperty, Coord_Object=self._coords
      ENDIF
   ENDIF

   ; Are you the display window of this object?
   IF Keyword_Set(setWindow) THEN object -> SetProperty, WID=self

   ; Call the draw method if requested.
   IF KEYWORD_SET (draw) THEN self -> Draw

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::APPLYCOLORS
;
; PURPOSE:
;
;       This method sets up the colors for the data object if they exist. For this
;       reason, sub-classes *MUST* call this method at the *START* of their draw methods.
;
; SYNTAX:
;
;       self -> ApplyColors
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
PRO DrawWidget::ApplyColors

   ; Set up an error handler
   @cat_pro_error_handler

   ; Set up the colors
   IF (Obj_IsA_Valid (self._colors, 'CATATOM')) THEN self._colors -> Draw

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::APPLYCOORDS
;
; PURPOSE:
;
;       This method sets up the coordinates for the data object if they exist. For this
;       reason, sub-classes *MUST* call this method at the *START* of their draw methods.
;
; SYNTAX:
;
;       self -> ApplyCoords
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
PRO DrawWidget::ApplyCoords

   ; Set up an error handler
   @cat_pro_error_handler

   ; Set up the colors
   IF (Obj_IsA_Valid (self._coords, 'CATCOORD')) THEN BEGIN
      theWindow = !D.Window
      self -> SetWindow
      self._coords -> Draw
      IF theWindow NE -1 THEN WSet, theWindow
   ENDIF
   ; Report completion
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CatImage::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the CATIMAGE object. A
;       control panel is a graphical user interface for setting object
;       properties. If you create a control panel, the events are typically
;       sent to the EVENTHANDLER method.
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
PRO DrawWidget::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.
   className = Obj_Class(self)

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE=classname + ' Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)
   self -> SetProperty, Description=className + ' Properties'

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.
   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, Event_Method='ControlPanelEvents', Event_Object=self, $
      Name=className + ' PROPERTYSHEET', YSize=4, Description=className + ' Properties')

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;        DrawWidget::CONTROLPANELEVENTS
;
; PURPOSE:
;
;        This method is the event handler for the DrawWidget object's ControlPanel.
;
; SYNTAX:
;
;        This method is called automatically by the event handling mechanism.
;
; ARGUMENTS:
;
;       event: The event structure as described in the IDL help files, except
;              that the ID, TOP and HANDLER tags will be object references.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO DrawWidget::ControlPanelEvents, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   className = Obj_Class(self)

   CASE eventName OF

      ClassName + ' PROPERTYSHEET': BEGIN

         IF event.type EQ 0 THEN BEGIN

            CASE StrUpCase(event.identifier) OF

               'INITIAL_COLOR': BEGIN

                  event.component -> GetProperty, Initial_Color=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, Initial_Color=color

                  ; Update changes
                  self -> Draw
               END

               ELSE: BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value

                  ; Update changes
                  self -> Draw
               END

            ENDCASE

         ENDIF

      END

      ELSE: Message, 'Unexpected event in ControlPanelEvents event handler.'
   ENDCASE

END


;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::COPY
;
; PURPOSE:
;
;       This method copies the contents of the draw widget to the current graphics window using
;       the DEVICE COPY method. The DEVICE COPY command looks like this:
;
;       DEVICE, COPY=[origin[0], origin[1], extent[0], extent[1], destination[0], destination[1], drawWindowID]
;
;       If the IMAGE keyword is used, the window contents are stored in an image variable
;       and the window contents are *not* copied into the current graphics window.
;
; SYNTAX:
;
;       thisDrawObj -> Copy
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DESINATION: A two-element array specifying the device coordinates of the lower-left
;                   corner of the copied region in the destination window. By default: [0,0].
;
;       EXTENT:     A two-element array specifying the number of columns and rows to copy.
;                   If missing, the entire draw widget window is copied.
;
;       IMAGE:      Set this keyword to a named IDL variable that returns a copy of the draw
;                   widget contents as an image. (Output). If this keyword is used, the draw
;                   widget contents are stored here, rather than copied to the current graphics
;                   window.
;
;       ORIGIN:     A two-element array specifying the device coordinates of the lower-left
;                   corner of region in the draw widget window to be copied. By default: [0,0].
;-
;*****************************************************************************************************
PRO DrawWidget::Copy, $
   DESTINATION=destination, $
   EXTENT=extent, $
   IMAGE=image, $
   ORIGIN=origin

   @cat_pro_error_handler

      ; Check we have a valid window to copy from.

   WIDGET_CONTROL, self._id, GET_VALUE=windowID
   IF (windowID EQ -1) THEN Message, 'Draw widget has not been realized yet. Cannot copy.'

    ; Check the keyword parameters.

   IF (N_ELEMENTS (origin) NE 2) THEN origin = [0, 0]
   IF (N_ELEMENTS (extent) NE 2) THEN BEGIN
      thisWindow = !D.Window
      WSet, windowID
      extent = [!D.X_Size, !D.Y_Size]
      IF thisWindow GT 0 THEN WSet, thisWindow
   ENDIF
   IF (N_ELEMENTS (destination) NE 2) THEN destination = [0, 0]

      ; If we're using an output variable, capture the current window
      ; and return it in the IMAGE keyword. Otherwise, copy the window
      ; contents into the current graphics window.

   IF (ARG_PRESENT (image)) THEN $
   BEGIN
      thisWindow = !D.Window
      WSet, windowID
      image = TVRead()
      IF thisWindow GT 0 THEN WSet, thisWindow
   ENDIF $
   ELSE DEVICE, COPY=[origin[0], origin[1], extent[0], extent[1], destination[0], destination[1], windowID]

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::DRAW
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
;       BACKGROUND_COLOR: Set this keyword to the name of the background color. Used
;                         only when erasing window contents. On 8-bit displays, this
;                         will load a color in !P.BACKGROUND. The color "BLACK" by default.
;
;       ERASE_WINDOW:     Set this keyword to erase the window before drawing contents.
;
;       HOURGLASS:        Set this keyword to enable the hourglass cursor for the draw operation.
;
;       REQUESTER:        This optional keyword is set to the object that requests a DRAW of the
;                         DrawWidget. This is helpful sometimes when DRAWWIDGET_DRAW messages
;                         are received by other objects. The object reference is passed on as
;                         the DATA in the DRAWWIDGET_DRAW message.
;
;       TARGET_WINDOW:    Normally the draw widget draws into its own window. But, sometimes you
;                         want the draw widget to draw somewhere else. Setting this keyword to
;                         another DRAWWIDGET or PIXMAPWIDGET object reference allows graphics
;                         to be drawn there.
;
;       TARGETS:          Typically, calling the DRAW method of a DrawWidget will call the DRAW
;                         method of any objects in its container. However, if the TARGETS keyword
;                         is set to an object reference (or array of object references), only these
;                         objects will be drawn. This would allow you, for example, to re-draw only
;                         a single image object in a window with several image objects.
;
;       _EXTRA:           Any extra keywords appropriate for superclass DRAW methods.
;-
;*****************************************************************************************************
PRO DrawWidget::Draw, $
   BACKGROUND_COLOR=background_color, $
   ERASE_WINDOW=erase_window, $
   HOURGLASS=hourglass, $
   REQUESTER=requester, $
   TARGET_WINDOW=target_window, $
   TARGETS=targets, $
   _EXTRA=extraKeywords

      ; Set up the error handler
   @cat_pro_error_handler

      ; If required, call the superclass draw method to realize the widget
      ; NB: Call it with NO_CHILDREN set so it does not draw its contents yet
   WIDGET_CONTROL, self._id, GET_VALUE=windowID
   IF (windowID EQ -1) THEN $
   BEGIN
      self -> WidgetAtom::Draw, /NO_CHILDREN, _EXTRA=extraKeywords
      WIDGET_CONTROL, self._id, GET_VALUE=windowID
   ENDIF

      ; If drawing of contents has been inhibited, return immediately.
   IF (self._noDraw) THEN RETURN

      ; If we haven't got a valid window ID yet, issue error.
   IF (windowID EQ -1) THEN Message, 'Draw widget has not got a valid window ID.'

      ; Check keywords.
   hourglass = Keyword_Set(hourglass)
   erase_window = Keyword_Set(erase_window)
   IF N_Elements(background_color) EQ 0 THEN background_color = self._initialcolor

      ; Enable the hourglass mouse cursor, if needed.
   IF hourglass THEN WIDGET_CONTROL, /HOURGLASS

   IF (!D.FLAGS AND 256) NE 0 THEN BEGIN

      ; Switch to this window ready to draw
      IF Obj_Valid(target_window) THEN target_window -> SetWindow ELSE WSet, windowID

         ; Need the window erased?
      IF (erase_window OR self._eraseWindow) THEN Erase, Color = FSC_Color(background_color, !P.BACKGROUND)

   ENDIF

   ; If you have a colors object, draw it now.
   self -> ApplyColors

   ; If you have a coordinates object, draw it now.
   self -> ApplyCoords

   ; If there are targets, draw just the targets, not everything.
   IF N_Elements(targets) NE 0 THEN BEGIN

      contents = self -> Get(/All, /Recursive_Search)

      FOR j=0,N_Elements(targets)-1 DO BEGIN

         ; If you can find the target in the list of objects, then draw it.
         i = Where(contents EQ targets[j], count)
         IF count GT 0 THEN targets[j] -> Draw

      ENDFOR

   ENDIF ELSE BEGIN

      ; Call the superclass draw method which will draw all of the object's contents.
      self -> WidgetAtom::Draw, _EXTRA=extraKeywords

   ENDELSE

   ; Send a message that you have drawn something. Include the requester of the DRAW, if available.
   self -> SendMessage, 'DRAWWIDGET_DRAW', DATA=requester

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to obtain the DrawWidget object's properties
;
; SYNTAX:
;
;       aDrawWidget -> GetProperty, WINDOW_ID=wid
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       BUTTON_EVENTS:    Returns a 1 if button events are on for the draw widget. Returns 0 otherwise.
;
;       COLOR_OBJECT:     The COLORTOOL object associated with this draw widget.
;
;       CONTEXT_EVENTS:   Returns a 1 if context events are on, a 0 otherwise.
;
;       COORD_OBJECT:     The CATCOORD object associated with this draw widget.
;
;       ERASE_WINDOW:     Returns a 1 if the ERASE_WINDOW keyword is set. Returns 0 otherwise.
;
;       EXPOSE_EVENTS:    Returns a 1 if expose events are on for the draw widget. Returns 0 otherwise.
;
;       INITIAL_COLOR:    The name of the initial color for the draw widget.
;
;       KEYBOARD_EVENTS:  Returns a 1 if keyboard events are on for the draw widget. Returns 0 otherwise.
;
;       MOTION_EVENTS:    Returns a 1 if motion events are on for the draw widget. Returns 0 otherwise.
;
;       NO_DRAW:          Returns a 1 if drawing is turned off for the draw widget. Returns 0 otherwise.
;
;       REFRESHBUFFER:    An object reference to a PIXMAPWIDGET object that can serve to refresh the
;                         draw widget display.
;
;       TOOLTIP:          The tooltip associated with this widget object. Null string if no tooltip.
;
;       VIEWPORT_EVENTS:  Returns a 1 if scroll events are on for the draw widget. Returns 0 otherwise.
;
;       WHEEL_EVENTS:     Returns a 1 if wheel events are on for the draw widget. Returns 0 otherwise.
;
;       WINDOWID:         The window index number of the draw widget.
;
;       _REF_EXTRA:       Any keywords appropriate for the superclass GetProperty methods.
;-
;*****************************************************************************************************
PRO DrawWidget::GetProperty, $
   BUTTON_EVENTS=button_events, $
   COLOR_OBJECT=color_object, $
   CONTEXT_EVENTS=context_events, $
   COORD_OBJECT=coord_object, $
   ERASE_WINDOW=erase_window, $
   EXPOSE_EVENTS=expose_events, $
   INITIAL_COLOR=initialcolor, $
   KEYBOARD_EVENTS=keyboard_events, $
   MOTION_EVENTS=motion_events, $
   NO_DRAW=no_draw, $
   REFRESHBUFFER=refreshbuffer, $
   TOOLTIP=tooltip, $
   VIEWPORT_EVENTS=viewport_events, $
  ; WHEEL_EVENTS=wheel_events, $
   WINDOWID=windowID, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   no_draw  = self._noDraw
   IF (WIDGET_INFO (self._id, /VALID_ID)) THEN $
   BEGIN
      WIDGET_CONTROL, self._id, GET_VALUE=windowID
      button_events   = Widget_Info(self._id, /Draw_Button_Events)
      expose_events   = Widget_Info(self._id, /Draw_Expose_Events)
      motion_events   = Widget_Info(self._id, /Draw_Motion_Events)
      viewport_events = Widget_Info(self._id, /Draw_Viewport_Events)
      keyboard_events = Widget_Info(self._id, /Draw_Keyboard_Events)
      tooltip         = Widget_Info(self._id, /Tooltip)
     ; wheel_events    = Widget_Info(self._id, /Draw_Wheel_Events)
   ENDIF

   IF Arg_Present(color_object) THEN color_object = self._colors
   IF Arg_Present(context_events) THEN context_events = Widget_Info(self._ID, /Context_Events)
   IF Arg_Present(coord_object) THEN coord_object = self._coords
   IF Arg_Present(erase_window) THEN erase_window = self._eraseWindow
   IF Arg_Present(initialcolor) THEN initialcolor = self._initialcolor
   IF Arg_Present(refreshbuffer) THEN refreshbuffer = self._refreshbuffer

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> WidgetAtom::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::MESSAGEHANDLER
;
; PURPOSE:
;
;       This method receives notification of a message call from another object's SENDMESSAGE
;       method.
;
; SYNTAX:
;
;       thisObject -> MessageHandler, title, SENDER=sender, MESSAGE=message
;
; ARGUMENTS:
;
;       TITLE:   The title of the message.
;
; KEYWORDS:
;
;       DATA:    A keyword that contains any information the sender wishes to pass
;                with the message. It can be empty.
;
;       SENDER:  The object that generated the message
;
;-
;*****************************************************************************************************
PRO DrawWidget::MessageHandler, title, SENDER=sender, DATA=data

   @cat_pro_error_handler

   IF N_Elements(title) EQ 0 THEN Message, 'Ill-formed message received. No title.'

   ; Handle various kinds of messages.
   CASE title OF

      'COLORTOOL_TABLECHANGE': BEGIN
         IF Obj_Valid(self._colors) THEN self._colors -> SetProperty, Red=data.r, $
            Green=data.g, Blue=data.b
         self -> Draw
      END

      'COLORTOOL_SETPROPERTY': BEGIN
         IF self._colors NE sender THEN BEGIN
            sender -> GetProperty, Red=r, Green=g, Blue=b
            IF Obj_Valid(self._colors) THEN self._colors -> SetProperty, Red=r, Green=g, Blue=b
         ENDIF
         self -> Draw
      END

      'NEED_UPDATE': self -> Draw

      'OBJECT_DELETED': BEGIN
         children = self -> Get(/ALL)
         index = Where(children EQ data, count)
         IF count GT 0 THEN self -> Remove, data
         self -> Refresh
         END

      'SETWINDOW': self -> SetWindow

      ELSE: self -> WIDGETATOM::MessageHandler, title, SENDER=sender, DATA=data

   ENDCASE

   ; It is possible that the DRAW widget might be in the control of an INTERACTION. If so,
   ; the interaction will need to refresh it's pixmap so that the change will be observed
   ; there. This code checks to see. First, check to see if there is an Exclusive Event Object.
   ; If so, is it an INTERACTION object?
   self -> GetProperty, EXCLUSIVE_EVENT_OBJECT=exclusive_event_object
   IF Obj_Valid(exclusive_event_object) THEN BEGIN
      IF Obj_Isa_Valid(exclusive_event_object, 'INTERACTION') THEN BEGIN
         exclusive_event_object -> RefreshPixmap
      ENDIF
   ENDIF


   ; Report success
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::NOTIFY_REALIZE
;
; PURPOSE:
;
;       At realization, the draw widget draws it's contents.
;
; SYNTAX:
;
;       self -> Notify_Realize, object
;
; ARGUMENTS:
;
;       OBJECT: The object that was realized (often the self object).
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO DrawWidget::Notify_Realize, theObject

   @cat_pro_error_handler

   currentWindow = !D.Window

   self -> GetProperty, RefreshBuffer=refreshBuffer
   IF Obj_Valid(refreshBuffer) THEN BEGIN
      refreshBuffer -> SetWindow
      Erase, Color=FSC_Color(self._initialcolor)
      self -> SetWindow
      refreshBuffer -> Copy
   ENDIF ELSE BEGIN
      self -> SetWindow
      Erase, Color=FSC_Color(self._initialcolor)
   ENDELSE

   IF currentWindow GE 0 THEN WSet, currentWindow

   ; Send a DRAWWIDGETREALIZED message.
   self -> SendMessage, 'DRAWWIDGETREALIZED'


   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::Output
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
;      BMP:        Set this keyword to create a BMP output file.
;
;      JPEG:       Set this keyword to create a JPEG output file.
;
;      FILENAME:   Set this keyword to the name of the output file. If the filename
;                  is specified without a file extension, the appropriate file
;                  extension will be added to the filename, based on the type of
;                  the file.
;
;      NODIALOG:   Set this keyword if you want to write the file directly,
;                  without giving the user a chance to change the filename.
;
;      PNG:        Set this keyword to create a PNG output file.
;
;      POSTSCRIPT: Set this keyword to create a PostScript output file.
;
;      TIFF:       Set this keyword to create a TIFF output file.
;
;      TYPE        Can be set to the type of file to write. Use this instead of
;                  setting BMP, JPEG,PNG, or TIFF keywords: TYPE='JPEG'. The
;                  primary purpose of this is to make event handlers easier to write.
;
;      _EXTRA:     Any keywords appropriate for the WRITE_*** routines.
;
;-
;*****************************************************************************************************
PRO DrawWidget::Output, $
   BMP=bmp, $
   FILENAME=filename, $
   NODIALOG=nodialog, $
   JPEG=jpeg, $
   PNG=png, $
   POSTSCRIPT=postscript, $
   TIFF=tiff, $
   TYPE=type, $
   _Extra=extraKeywords

   @cat_pro_error_handler

   ; Do you want to write an image file instead of
   ; capturing an image?
   IF N_Elements(type) NE 0 THEN BEGIN
      CASE StrUpCase(type) OF
         'BMP': bmp = 1
         'JPEG': jpeg = 1
         'JPG': jpeg = 1
         'PNG': png = 1
         'TIFF': tiff = 1
         'TIF': tiff = 1
         'POSTSCRIPT': postscript = 1
         ELSE: Message, 'Cannot write a file of type: ' + StrUpCase(type) + '.'
      ENDCASE
   ENDIF

   output = 'JPEG'
   IF Keyword_Set(jpeg) THEN output = 'JPEG'
   IF Keyword_Set(tiff) THEN output = 'TIFF'
   IF Keyword_Set(bmp) THEN output = 'BMP'
   IF Keyword_Set(png) THEN output = 'PNG'
   IF Keyword_Set(postscript) THEN output = 'POSTSCRIPT'


   nodialog = Keyword_Set(nodialog)

      ; Get a snapshop of window contents.
   self -> SetWindow
   snapshot = TVRead()
   position = PSWindow()

      ; What kind of file is wanted?

   CASE output OF

      'JPEG': BEGIN

         IF N_Elements(filename) EQ 0 THEN filename='output.jpg' 
         basename = FSC_Base_Filename(filename, EXTENSION=ext)
         IF ext EQ "" THEN filename = filename + '.jpg'
         IF nodialog EQ 0 THEN filename = Dialog_Pickfile(/Write, File=filename)
         IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1, Quality=100, _Extra=extraKeywords
         END


      'TIFF': BEGIN

         IF N_Elements(filename) EQ 0 THEN filename='output.tif'
         basename = FSC_Base_Filename(filename, EXTENSION=ext)
         IF ext EQ "" THEN filename = filename + '.tif'
         IF nodialog EQ 0 THEN filename = Dialog_Pickfile(/Write, File=filename)
         IF filename NE '' THEN BEGIN

            ; TIFF files should have their Y direction reversed for
            ; compatibility with most other software.

            Write_TIFF, filename, Reverse(snapshot,3), _Extra=extraKeywords
         ENDIF
         END

      'BMP': BEGIN
         IF N_Elements(filename) EQ 0 THEN filename='output.bmp'
         basename = FSC_Base_Filename(filename, EXTENSION=ext)
         IF ext EQ "" THEN filename = filename + '.bmp'
         IF nodialog EQ 0 THEN filename = Dialog_Pickfile(/Write, File=filename)
         IF filename NE '' THEN Write_BMP, filename, snapshot, _Extra=extraKeywords, /RGB
         END

      'PNG': BEGIN
         IF N_Elements(filename) EQ 0 THEN filename='output.png'
         basename = FSC_Base_Filename(filename, EXTENSION=ext)
         IF ext EQ "" THEN filename = filename + '.png'
         IF nodialog EQ 0 THEN filename = Dialog_Pickfile(/Write, File=filename)
         IF filename NE '' THEN Write_PNG, filename, snapshot, _Extra=extraKeywords
         END

      'POSTSCRIPT': BEGIN
         position = PSWindow()
         IF N_Elements(filename) EQ 0 THEN filename='output.ps'
         basename = FSC_Base_Filename(filename, EXTENSION=ext)
         IF ext EQ "" THEN filename = filename + '.ps'
         IF nodialog EQ 1 THEN BEGIN
            keywords = PSConfig(Filename=filename, /NoGUI, _Extra=position)
            cancelled = 0
         ENDIF ELSE keywords = PSConfig(Filename=filename, Cancel=cancelled, _Extra=position)
         IF cancelled THEN RETURN

         thisDevice = !D.Name
         Set_Plot, 'PS'
         Device, _Extra=keywords
         self -> Draw
         Device, /Close_File
         Set_Plot, thisDevice

         END

   ENDCASE

 END



;;*****************************************************************************************************
;;+
;; NAME:
;;       DRAWWIDGET::PRINT
;;
;; PURPOSE:
;;
;;       This method will send the contents of the draw widget to the default printer. There
;;       is no guarantee the contents will print correctly. This will depend entirely upon
;;       how the underlying DRAW methods are written. If they are not written in a device-independent
;;       fashion, they are unlikely to work correctly.
;;
;; SYNTAX:
;;
;;       drawWidget -> Print
;;
;; ARGUMENTS:
;;
;;       None.
;;
;; KEYWORDS:
;;
;;      LANDSCAPE:  Set this keyword to 1 to select landscape output. The default in portrait output.
;;
;;      _EXTRA:     Any keywords appropriate for the PRINTER Device command.
;;
;;-
;;*****************************************************************************************************
;PRO DrawWidget::Print, $
;   LANDSCAPE=landscape, $
;   _Extra=extraKeywords
;
;   @cat_pro_error_handler
;
;      ; Set up the printer.
;
;   ok = Dialog_PrinterSetup()
;   IF NOT ok THEN RETURN
;
;   thisDevice = !D.Name
;
;   IF Keyword_Set(landscape) THEN BEGIN
;      Set_Plot, 'PRINTER'
;      Device, Landscape=1, _Extra=extra
;   ENDIF ELSE BEGIN
;      Set_Plot, 'PRINTER'
;      Device, Portrait=1, _Extra=extra
;   ENDELSE
;
;      ; Print it. May take a little time. Alert the user.
;
;   Widget_Control, Hourglass=1
;   self -> Draw
;   Device, /Close_Document
;   Set_Plot, thisDevice
;   Widget_Control, Hourglass=0
;
;   self -> Report, /Completed
;
; END
;
;
;
;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::REFRESH
;
; PURPOSE:
;
;       This method refreshs the draw widget from a refresh buffer, if available.
;       If not available, the method simply calls the DRAW method of the object.
;;
; SYNTAX:
;
;       thisDrawObj -> Refresh
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       REQUESTER:        This optional keyword is set to the object that requests a DRAW of the
;                         DrawWidget. This is helpful sometimes when DRAWWIDGET_DRAW messages
;                         are received by other objects. The object reference is passed on as
;                         the DATA in the DRAWWIDGET_DRAW message.
;
;       TARGET_WINDOW:    Normally the draw widget draws into its own window. But, sometimes you
;                         want the draw widget to draw somewhere else. Setting this keyword to
;                         another DRAWWIDGET or PIXMAPWIDGET object reference allows graphics
;                         to be drawn there.
;
;       _EXTRA:           Any extra keywords appropriate for the DRAW method.
;-
;*****************************************************************************************************
PRO DrawWidget::Refresh, REQUESTER=requester, TARGET_WINDOW=target_window, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Obj_ISA_VALID(self._refreshbuffer, 'PIXMAPWIDGET') THEN BEGIN

      self._refreshbuffer -> Refresh, REQUESTER=requester
      self -> SetWindow
      self._refreshBuffer -> Copy

   ENDIF ELSE self -> Draw, REQUESTER=requester, TARGET_WINDOW=target_window, _Extra=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::RESIZE
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
;       XSIZE:    The new X size of the canvas area of the draw widget, in pixels.
;
;       YSIZE:    The new Y size of the canvas area of the draw widget, in pixels.
;
; KEYWORDS:
;
;       DRAW:     Set this keyword to call the draw method when the draw widget
;                 resizing is completed.
;
;       SCREEN:   Normally, the XSIZE and YSIZE keywords apply to the draw widget canvas.
;                 If the SCREEN keyword is set, the keywords apply to the screen coordinates
;                 of the draw widget. (It's actual size on the display. Usually about 6 pixels
;                 larger than the canvas.)
;
;       VIEWPORT: Normally, the XSIZE and YSIZE keywords apply to the draw widget canvas.
;                 If the VIEWPORT keyword is set, the keywords apply to the viewport size
;                 of the draw widget.
;
;       _EXTRA:   Any extra keywords appropriate for the DRAW method.
;-
;*****************************************************************************************************
PRO DrawWidget::Resize, xsize, ysize, DRAW=draw, SCREEN=screen, VIEWPORT=viewport, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(xsize) EQ 0 THEN Message, "XSIZE parameter is missing."
   IF N_Elements(ysize) EQ 0 THEN Message, "YSIZE parameter is missing."

   CASE 1 OF

      KEYWORD_SET(screen): Widget_Control, self._id, Scr_XSize=xsize, Scr_YSize=ysize

      KEYWORD_SET(viewport): Widget_Control, self._id, XSize=xsize, YSize=ysize
 
      ELSE: Widget_Control, self._id, Draw_XSize=xsize, Draw_YSize=ysize

   ENDCASE

   ; Send a message that the draw widget was resized.
   self -> SendMessage, 'RESIZEDRAWWIDGET', Data=[xsize, ysize]
   
   IF Keyword_Set(draw) THEN self -> Draw, _Extra=extraKeywords
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::SELECTOBJECTS
;
; PURPOSE:
;
;       This method searches through the object hierarchy, rooted at the draw widget,
;       for selectable objects. Valid objects are returned in an object array.
;
; SYNTAX:
;
;       selectableObjects = drawObj->SelectObjects()
;
; ARGUMENTS:
;
;       X:      The X location of the selection in the draw widget window.
;       Y:      The Y location of the selection in the draw widget window.
;
; KEYWORDS:
;
;       COUNT:  Returns the number of valid objects found.
;-
;*****************************************************************************************************
FUNCTION DrawWidget::SelectObjects, x, y, Count=count

   @cat_func_error_handler

   retVal = Obj_New()
   count = 0

   ; Get the children of the draw widget.
   childObjs = self -> Get(/All, Count=childNo, /Recursive_Search)
   FOR j=0,childNo-1 DO BEGIN
      IF Obj_Valid(childObjs[j]) THEN BEGIN
         anObject = childObjs[j] -> Select(x, y, Success=success)
      ENDIF ELSE BEGIN
         success = 0
         self -> Remove, childObjs[j] ; Remove NULL objects if you find any.
      ENDELSE
      IF success THEN BEGIN
         IF N_Elements(selectableObjects) EQ 0 THEN selectableObjects = [anObject] ELSE $
            selectableObjects = [selectableObjects, anObject]
      ENDIF

   ENDFOR

   ; Did you find any objects?
   IF N_Elements(selectableObjects) NE 0 THEN BEGIN
      retVal = selectableObjects
      count = N_Elements(retval)
   ENDIF

   self -> Report, /Completed
   RETURN, retVal

END


;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the DrawWidget object's properties
;
; SYNTAX:
;
;       aDrawWidget -> SetProperty, BUTTON_EVENTS=1
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;
;       BUTTON_EVENTS:    Turns draw widget button events on if set to 1. Events off if set to 0.
;
;       COLOR_OBJECT:     Use this keyword to load a COLORTOOL object for setting up colors
;                         for data display.
;
;       CONTEXT_EVENTS:   Set to 1 to turn context events on for the base widget.
;
;       COORD_OBJECT:     Use this keyword to load a CATCOORD object for setting up
;                         the data coordinate system for data display.
;
;       ERASE_WINDOW:     Set this keyword to cause the draw widget to execute an ERASE before drawing.
;
;       EXPOSE_EVENTS:    Turns draw widget expose events on if set to 1. Events off if set to 0.
;
;       IGNORE_ACCELERATORS: Set this keyword to specify what WIDGET_BUTTON accelerators are to be
;                        ignored when this draw widget has keyboard focus. Setting IGNORE_ACCELERATORS
;                        allows a defined list of accelerators to be processed by the draw widget instead
;                        of by the conflicting accelerated button. Valid values are:
;
;                             A string or string array containing any value that is legal for the
;                             ACCELERATOR keyword for BUTTONWIDGET, or
;
;                             1 -- Indicating that all accelerators should be ignored.
;
;                        Ordinarily, accelerators are processed before keyboard events reach the
;                        widget that has the keyboard focus. Setting IGNORE_ACCELERATORS allows a
;                        defined list of accelerators to be processed by the draw widget instead of
;                        by associated buttons or menu items. Once the draw widget loses focus, all
;                        specified accelerators are automatically re-enabled.
;       INITIAL_COLOR:    The name of the initial color for the draw widget. Used when realized and
;                         if the draw widget is set up to erase before display (i.e., ERASE_WINDOW=1).
;                         By default, set to "WHITE" to facilitate output to PostScript.
;
;       INPUT_FOCUS:      Set this keyword to configure the draw widget to receive keyboard focus.
;
;       KEYBOARD_EVENTS:  Turns draw widget keyboard events on.
;
;       MOTION_EVENTS:    Turns draw widget motion events on if set to 1. Events off if set to 0.
;
;       NO_DRAW:          Prevents drawing of contents if set to 1. Allows drawing if set to 0.
;
;       REFRESHBUFFER:    An object reference to a PIXMAPWIDGET object that can serve to refresh the
;                         draw widget display. The draw widget takes no parental interest in this object
;                         and doesn't destroy it when the draw widget is destroyed.
;
;       SCREEN:           Normally, the XSIZE and YSIZE keywords apply to the draw widget canvas.
;                         If the SCREEN keyword is set, the keywords apply to the screen coordinates
;                         of the draw widget. (It's actual size on the display. Usually about 6 pixels
;                         larger than the canvas.)
;
;       SET_DRAW_VIEW:    A two-element array that defines the current position of the viewport in UNITS
;                         relative to the lower-left corner of the draw widget.
;
;       TOOLTIP:          A short string that will be displayed if the cursor hovers over this widget.
;
;       VIEWPORT_EVENTS:  Turns draw widget viewport events on if set to 1. Events off if set to 0.
;
;       VIEWPORT_SIZE:    Normally, the XSIZE and YSIZE keywords apply to the draw widget canvas.
;                         If the VIEWPORT_SIZE keyword is set, the keywords apply to the viewport size
;                         of the draw widget.

;       XSIZE:            The new X size of the canvas area of the draw widget, in pixels.
;
;       YSIZE:            The new Y size of the canvas area of the draw widget, in pixels.
;
;
;       _EXTRA:           Any keywords appropriate for the superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO DrawWidget::SetProperty, $
   BUTTON_EVENTS=button_events, $
   COLOR_OBJECT=color_object, $
   CONTEXT_EVENTS=context_events, $
   COORD_OBJECT=coord_object, $
   ERASE_WINDOW=erase_window, $
   EXPOSE_EVENTS=expose_events, $
   IGNORE_ACCELERATORS=ignore_accelerators, $
   INITIAL_COLOR=initialcolor, $
   INPUT_FOCUS=input_focus, $
   KEYBOARD_EVENTS=keyboard_events, $
   MOTION_EVENTS=motion_events, $
   NO_DRAW=no_draw, $
   REFRESHBUFFER=refreshbuffer, $
   SCREEN=screen, $
   SET_DRAW_VIEW=set_draw_view, $
   TOOLTIP=tooltip, $
   VIEWPORT_EVENTS=viewport_events, $
   VIEWPORT_SIZE=viewport_size, $
  ; WHEEL_EVENTS=wheel_events, $
   XSIZE=xsize, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

;         ; Modify the amount of Video RAM free
;      videoMemory = CatGetDefault ('VideoMemory', Success=ok)
;      IF (ok) THEN $
;      BEGIN
;         videoMemory = videoMemory + (self._videoRAM / 1024)
;         self._videoRAM = xsize * ysize
;         CatSetDefault, 'VideoMemory',  videoMemory - (self._videoRAM / 1024)
;      ENDIF

      ; Call the superclass SetProperty methods.

   self -> WidgetAtom::SetProperty, _EXTRA=extraKeywords


      ; Make sure you have a valid widget here.

   IF Widget_Info(self._id, /Valid_ID) NE 1 THEN BEGIN
      self -> Report, /Completed
      RETURN
   ENDIF

      ; Set properties with Widget_Control.

   IF (N_ELEMENTS (button_events) GT 0) THEN WIDGET_CONTROL, self._id, DRAW_BUTTON_EVENTS=Keyword_Set(button_events)
   IF N_Elements(context_events) NE 0 THEN $
      WIDGET_CONTROL, self._id, CONTEXT_EVENTS=Keyword_Set(context_events)
   IF (N_ELEMENTS (erase_window) GT 0) THEN self._eraseWindow = Keyword_Set(erase_window)
   IF (N_ELEMENTS (expose_events) GT 0) THEN WIDGET_CONTROL, self._id, DRAW_EXPOSE_EVENTS=Keyword_Set(expose_events)
   IF (N_ELEMENTS (ignore_accelerators) GT 0) THEN WIDGET_CONTROL, self._id, IGNORE_ACCELERATORS=Keyword_Set(ignore_accelerators)
   IF (N_ELEMENTS (initialcolor) GT 0) THEN self._initialcolor = initialcolor
   IF (N_ELEMENTS (keyboard_events) GT 0) THEN WIDGET_CONTROL, self._id, DRAW_KEYBOARD_EVENTS=keyboard_events
   IF (N_ELEMENTS (motion_events) GT 0) THEN WIDGET_CONTROL, self._id, DRAW_MOTION_EVENTS=Keyword_Set(motion_events)
   IF N_Elements(refreshbuffer) NE 0 THEN self._refreshBuffer = refreshbuffer
   IF (N_ELEMENTS (set_draw_view) GT 0) THEN WIDGET_CONTROL, self._id, SET_DRAW_VIEW=set_draw_view
   IF (N_ELEMENTS (viewport_events) GT 0) THEN WIDGET_CONTROL, self._id, DRAW_VIEWPORT_EVENTS=Keyword_Set(viewport_events)
   IF (N_ELEMENTS (input_focus) GT 0) THEN WIDGET_CONTROL, self._id, INPUT_FOCUS=Keyword_Set(input_focus)
   IF N_ELEMENTS (tooltip) NE 0 THEN  Widget_Control, self._ID, TOOLTIP=tooltip
   ;IF N_ELEMENTS (wheel_events) NE 0 THEN  Widget_Control, self._ID, DRAW_WHEEL_EVENTS=Keyword_Set(wheel_events)
   IF N_ELEMENTS (xsize) NE 0 THEN  resizeFlag = 1
   IF N_ELEMENTS (ysize) NE 0 THEN resizeFlag = 1

   ; If the resize flag is set, resize the draw widget and issue a RESIZEDRAWWIDGET message.
   IF Keyword_Set(resizeFlag) THEN BEGIN

      IF N_Elements(xsize) EQ 0 THEN self -> GetProperty, XSize=xsize
      IF N_Elements(ysize) EQ 0 THEN self -> GetProperty, YSize=ysize
      IF Obj_Isa_Valid(self, 'ODRAWWIDGET') THEN $
         self -> Resize, xsize, ysize ELSE $
         self -> Resize, xsize, ysize, SCREEN=screen, VIEWPORT=viewport_size
      self -> GetProperty, XSIZE=xs, YSIZE=ys
   ENDIF

      ; Set other properties.

   IF OBJ_ISA_VALID (color_object, 'COLORTOOL')  OR OBJ_ISA_VALID (color_object, 'CATCOLORS') THEN $
   BEGIN
      Obj_Destroy, self._colors
      self._colors = color_object
      color_object -> AddParent, self
      color_object -> RegisterForMessage, self, 'COLORTOOL_SETPROPERTY'
   END

   IF OBJ_ISA_VALID (coord_object, 'CATCOORD')  THEN $
   BEGIN
      Obj_Destroy, self._coords
      self._coords = coord_object
      coord_object -> AddParent, self
   END

   IF N_ELEMENTS (no_draw) NE 0 THEN self._noDraw  = KEYWORD_SET (no_draw)

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::SETWINDOW
;
; PURPOSE:
;
;       This method sets the window index number of the draw widget to be the
;       current graphics window.
;
; SYNTAX:
;
;       thisDrawObj -> SetWindow
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
PRO DrawWidget::SetWindow

   ; If WSET can't be supported, then return.
   IF (!D.FLAGS AND 256) EQ 0 THEN RETURN
   WIDGET_CONTROL, self._id, GET_VALUE=windowID
   IF windowID GE 0 THEN WSet, windowID
END


;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::CLEANUP
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
;       _EXTRA:  Any keyword appropriate for the  superclass CLEANUP methods.
;-
;*****************************************************************************************************
PRO DrawWidget::CLEANUP

   @cat_pro_error_handler

         ; Increment the amount of video ram that is available
   videoMemory = CatGetDefault ('VideoMemory', Success=ok)
   IF (ok) THEN CatSetDefault, 'VideoMemory',  videoMemory + (self._videoRAM / 1024)
   Obj_Destroy, self._psconfig
;   Obj_Destroy, self._coords
;   Obj_Destroy, self._colors
   IF Obj_Valid(self._coords) THEN self._coords -> RemoveParent, self
   IF Obj_Valid(self._colors) THEN self._colors -> RemoveParent, self

   self -> WidgetAtom::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       DRAWWIDGET::INIT
;
; PURPOSE:
;
;       This is the DRAWWIDGET object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       theParent - An object reference to a WIDGETATOM-subclassed object.
;           This is the same as using the PARENT keyword.
;
; KEYWORDS:
;
;       APP_SCROLL:      A memory-saving way of scrolling draw widgets. See WIDGET_DRAW documentation.
;
;       BUTTON_EVENTS:   Set this keyword to enable widget button events in the draw widget.
;
;       COLOR_OBJECT:    Use this keyword to load a COLORTOOL object for setting up colors
;                        for data display.
;
;       CONTEXT_EVENTS:  Set this keyword to turn context events on for this widget object..
;
;       COORD_OBJECT:    Use this keyword to load a CATCOORD object for setting up
;                        the data coordinate system for data display.
;
;       ERASE_WINDOW:    Set this keyword to cause the draw widget to execute an ERASE before drawing.
;
;       EXPOSE_EVENTS:   Set this keyword to enable widget expose events in the draw widget.
;
;       FRAME:           Create a frame this many pixels wide around the widget.
;
;       IGNORE_ACCELERATORS: Set this keyword to specify what WIDGET_BUTTON accelerators are to be
;                        ignored when this draw widget has keyboard focus. Setting IGNORE_ACCELERATORS
;                        allows a defined list of accelerators to be processed by the draw widget instead
;                        of by the conflicting accelerated button. Valid values are:
;
;                             A string or string array containing any value that is legal for the
;                             ACCELERATOR keyword for BUTTONWIDGET, or
;
;                             1 -- Indicating that all accelerators should be ignored.
;
;                        Ordinarily, accelerators are processed before keyboard events reach the
;                        widget that has the keyboard focus. Setting IGNORE_ACCELERATORS allows a
;                        defined list of accelerators to be processed by the draw widget instead of
;                        by associated buttons or menu items. Once the draw widget loses focus, all
;                        specified accelerators are automatically re-enabled.
;
;       INITIAL_COLOR:   The name of the initial color for the draw widget. Used when realized and
;                        if the draw widget is set up to erase before display (i.e., ERASE_WINDOW=1).
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
;       REFRESHBUFFER:   An object reference to a PIXMAPWIDGET object that can serve to refresh the
;                        draw widget display. The draw widget takes no parental interest in this object
;                        and doesn't destroy it when the draw widget is destroyed. The refresh buffer is
;                        accessed via the REFRESH method. If no buffer is present, the REFRESH method
;                        simply calls the DRAW method for the object.
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
;       TOOLTIP:         Set this keyword to a string that will be displayed if the cursor hovers
;                        over the draw widget.
;
;       UNITS:           The units for measurments. The default is 0 for pixels. Other values are
;                        1 for inches, and 2 for centimeters.
;
;       VIEWPORT_EVENTS: Set this keyword to enable widget viewport scroll events in the draw widget.
;
;       WHEEL_EVENTS:    Set this keyword to enable wheel events in the draw widget.
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
;
;-
;*****************************************************************************************************
FUNCTION DrawWidget::INIT, parent, $
   APP_SCROLL=app_scroll, $
   BUTTON_EVENTS=button_events, $
   COLOR_OBJECT=color_object, $
   CONTEXT_EVENTS=context_events, $
   COORD_OBJECT=coord_object, $
   ERASE_WINDOW=erase_window, $
   EXPOSE_EVENTS=expose_events, $
   FRAME=frame, $
   IGNORE_ACCELERATORS=ignore_accelerators, $
   INITIAL_COLOR=initialcolor, $
   KEYBOARD_EVENTS=keyboard_events, $
   MOTION_EVENTS=motion_events, $
   NODRAW=nodraw, $
   REFRESHBUFFER=refreshbuffer, $
   RETAIN=retain, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   SCROLL=scroll, $
   TOOLTIP=tooltip, $
   UNITS=units, $
   VIEWPORT_EVENTS=viewport_events, $
   WHEEL_EVENTS=wheel_events, $
   X_SCROLL_SIZE=x_scroll_size, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   Y_SCROLL_SIZE=y_scroll_size, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   @cat_func_error_handler

      ; Set the defaults.

   self._noDraw   = Keyword_Set(nodraw)
   IF N_Elements(xsize) EQ 0 THEN xsize = 300
   IF N_Elements(ysize) EQ 0 THEN ysize = 300
   IF N_Elements(initialcolor) EQ 0 THEN self._initialcolor = 'Black' ELSE self._initialcolor = initialcolor
   IF N_Elements(retain) EQ 0 THEN retain=(!Version.OS_Family EQ 'Windows') ? 1 : 2

      ; If the parent is not specified, then create a TLB to use as a parent.

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
      EXPOSE_EVENTS=expose_events, $
      FRAME=frame, $
      IGNORE_ACCELERATORS=ignore_accelerators, $
      KEYBOARD_EVENTS=keyboard_events, $
      MOTION_EVENTS=motion_events, $
      RETAIN=retain, $
      SCR_XSIZE=scr_xsize, $
      SCR_YSIZE=scr_ysize, $
      SCROLL=scroll, $
      TOOLTIP=tooltip, $
      UNITS=units, $
      VIEWPORT_EVENTS=viewport_events, $
      WHEEL_EVENTS=wheel_events, $
      X_SCROLL_SIZE=x_scroll_size, $
      XOFFSET=xoffset, $
      XSIZE=xsize, $
      Y_SCROLL_SIZE=y_scroll_size, $
      YOFFSET=yoffset, $
      YSIZE=ysize,$
     _EXTRA=extraKeywords)

      ; Call superclass INIT method, report and return success.

   IF  NOT self -> WidgetAtom::INIT (parent, id, _EXTRA=extraKeywords) THEN $
     Message, 'WidgetAtom::INIT method failed to complete.'

   ; Load the color object
   IF OBJ_ISA_VALID (color_object, 'COLORTOOL')  THEN $
   BEGIN
      self._colors = color_object
      color_object -> AddParent, self
      color_object -> RegisterForMessage, self, 'COLORTOOL_SETPROPERTY'
   END

   ; Load the coordinates object
   IF OBJ_ISA_VALID (coord_object, 'CATCOORD')  THEN $
   BEGIN
      self._coords = coord_object
      coord_object -> AddParent, self
   END

   ; If we created a new tlb, realize it now.
   IF OBJ_VALID (self._drawBase) THEN BEGIN
      self._drawBase -> Draw
   ENDIF

   ; Decrement the amount of video ram that is available
;   self._videoRAM = xsize * ysize
;   videoMemory = CatGetDefault ('VideoMemory', Success=ok)
;   IF (ok) THEN CatSetDefault, 'VideoMemory',  videoMemory - (self._videoRAM / 1024)

   ; Need a window erase?
   self._eraseWindow = Keyword_Set(erase_window)

   ; Refresh buffer.
   IF Obj_ISA_VALID(refreshbuffer) THEN self._refreshbuffer = refreshbuffer

   ; Set up PostScript configuration object.
   self._psconfig = Obj_New('FSC_PSCONFIG')

   ; Register properties for the property sheet.
   self->RegisterProperty, 'Erase_Window', 1, NAME="Erase Before Drawing"
   self -> SetPropertyByIdentifier, 'Erase_Window', self._eraseWindow
   self -> RegisterProperty, 'INITIAL_COLOR', 0, USERDEF=CapFirstLetter(self._initialcolor), NAME="Color (Background)"

   self -> Report, /Completed

   RETURN, 1
END



;*****************************************************************************************************
;
; NAME:
;       DRAWWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the DRAWWIDGET object class definition.
;       The DRAWWIDGET object is subclassed from the WIDGETATOM object.
;
;*****************************************************************************************************
PRO DrawWidget__DEFINE, class

   class = { DrawWidget,          $
             _coords     : OBJ_NEW(), $  ; A CATCOORD object of some type.
             _colors     : OBJ_NEW(), $  ; A COLORTOOL object for setting up color tables.
             _drawBase   : OBJ_NEW (), $ ; A top-level base widget object, if required.
             _noDraw     : 0B, $         ; Flag that, if set, inhibits draw method calls.
             _videoRAM   : 0L, $         ; amount of video memory consumed by the widget
             _psconfig   : OBJ_NEW(), $  ; A PostScript configuration object.
             _eraseWindow: 0B, $         ; A flag that indicates the window should be erased before drawing.
             _initialcolor: "", $        ; The name of the initial window color.
             _refreshbuffer: OBJ_NEW(), $; The identifier of a PIXMAPWIDGET that can carry out window refresh.
             INHERITS WidgetAtom $       ; INHERITS WIDGETATOM capabilities.
             }
END