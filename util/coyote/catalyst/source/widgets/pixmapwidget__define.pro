;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to implement a pixmap window as a
;       widget object.
;
;       Putting this widget into a hierarchy will cause the display generated
;       by drawing the child widgets to be buffered through the pixmap to the
;       target window. For this reason, invoking the DRAW method on the pixmap
;       simply does a copy to the target window (or current window, if the target
;       window is undefined). If you wish to simple draw the child widgets on the
;       pixmap without affecting the target window, use the REFRESH method.
;       
;       The pixmap actually exists in an unmapped TopLevelBaseWidget. This is
;       super convenient, because if you need to see the pixmap for debugging
;       purposes, all you need to is set the MAP=1 keyword on its SetProperty
;       method. To make the pixmap disappear again, set MAP=0.
;
; AUTHOR:
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
;       pixmapWidget = Obj_New("PixmapWidget", theParent)
;
; SUPERCLASSES:
;
;       TOPLEVELBASE
;       BASEWIDGET
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { PixmapWidget, $
;             _backgroundColor: "", $     ; The name of the background color.
;             _drawWidget: 0L, $          ; The Widget ID of the draw widget.
;             _keep: 0B, $                ; To conserve pixmap memory, KEEP=1 retains the pixmap, KEEP=0 does not.
;             _isReady: 0B, $             ; A flag to show if the window needs updating before copy.
;             _noDraw: 0B, $              ; A flag to inhibit drawing operation.
;             _snapshot: OBJ_NEW(), $     ; For KEEP=0, a snapshot of the required pixmap
;             _targetWindow: OBJ_NEW(), $ ; A window object that is to be the target of the pixmap.
;             _xsize: 0L,  $              ; The X size of the pixmap window.
;             _ysize: 0L,  $              ; The Y size of the pixmap window.
;             INHERITS TOPLEVELBASE }
;
;
; NOTES:
;
;       Note that unless the PIXMAPWIDGET is a child of another widget, that
;       the user will be responsible for destroying the PIXMAPWIDGET. Failure to
;       do this will result in memory leakage. In other words, if the PIXMAPWIDGET
;       exists in its own unmapped TOPLEVELBASE, it will NOT be automatically destroyed
;       when the program that uses it is destroyed. This must be done *explicitly*.
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 13th July 2002.
;       Small change to SetProperty and GetProperty methods to get READY status. 28 Oct 2004. DWF.
;       Added response to RESIZEDRAWWIDGET message in MessageHandler method. 7 Dec 2004. DWF.
;       I made the default BACKGROUND_COLOR for pixmaps white to facilitate PostScript
;          output and conform to draw widgets. 12 Dec 2004. DWF
;       Removed the "overlay" capability, which we were not using. 23 January 2005. DWF.
;       Changed the BACKGROUND_COLOR back to "black". Tired of fighting it... 5 July 2005. DWF.
;       Renamed the former RETAIN keyword to KEEP, so that RETAIN can be used in its normal
;          IDL window definition. 21 August 2006. DWF.
;       Changed BACKGROUND_COLOR to BACKGROUNDCOLOR to remain consistent with GET/SET methods. And
;          removed DRAW_COLOR keyword, which was used for overlays. 21 August 2006. DWF.
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
;       PIXMAPWIDGET::ADD
;
; PURPOSE:
;
;       This method adds an object to the PixmapWidget object container.
;
; SYNTAX:
;
;       pixmapObj -> Add, object
;
; ARGUMENTS:
;
;       object:  The object to add to the container.
;
; KEYWORDS:
;
;      DRAW:     Set this keyword to cause the pixmap to be re-drawn as soon as the
;                object has been added.
;
;      _EXTRA:   Any keyword appropriate for the superclass ADD methods.
;
;-
;*****************************************************************************************************
PRO PixmapWidget::Add, object, _EXTRA=extraKeywords, Draw=draw

   @cat_pro_error_handler

   ; Add the object to the pixmap window.
   self -> WIDGETATOM::Add, object, _EXTRA=extraKeywords
   self._isReady = 0B

   ; Need to draw now?
   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the PIXMAPWIDGET object. A
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
PRO PixmapWidget::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Pixmap Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)
   self -> SetProperty, Description='Pixmap Properties'

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.
   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, $
      Name='PIXMAP PROPERTYSHEET', Description='Pixmap Properties', YSIZE=3)
   aproperties -> SetProperty, Event_Object=self

   ; Is the base object from a browser? If so, then size the property sheet
   ; according to the size of the base widget.
   IF Obj_Valid(baseObject) THEN BEGIN
      IF StrUpCase(StrMid(baseObject->GetName(), 0, 7)) EQ 'BROWSER' THEN BEGIN
         baseObject -> GetProperty, Geometry=geo
         aproperties -> SetProperty, Scr_XSize=geo.xsize, Scr_YSize=geo.ysize
      ENDIF

   ENDIF

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END

;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::COPY
;
; PURPOSE:
;
;       This method copies the contents of the draw widget to the current graphics window using
;       the DEVICE COPY method. The DEVICE COPY command looks like this:
;
;       DEVICE, COPY=[origin[0], origin[1], extent[0], extent[1], destination[0], destination[1], self._windowID]
;
;       If the IMAGE keyword is used, the window contents are stored in an image variable
;       and the window contents are *not* copied into the current graphics window.
;
; SYNTAX:
;
;       pixmapObj -> Copy
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       ERASE_WINDOW: Set this keyword to erase the window before drawing contents.
;
;       DESINATION:   A two-element array specifying the device coordinates of the lower-left
;                     corner of the copied region in the destination window. By default: [0,0].
;
;       EXTENT:       A two-element array specifying the number of columns and rows to copy.
;                     If missing, the entire draw widget window is copied.
;
;       IMAGE:        Set this keyword to a named IDL variable that returns a copy of the draw
;                     widget contents as an image. (Output). If this keyword is used, the draw
;                     widget contents are stored here, rather than copied to the current graphics
;                     window.
;
;       ORDER:        Set this keyword to affect the order of image transfer when the COPY is
;                     to an image rather than to a display. Applies only if the IMAGE keyword is used.
;
;       ORIGIN:       A two-element array specifying the device coordinates of the lower-left
;                     corner of region in the draw widget window to be copied. By default: [0,0].
;
;       HOURGLASS:    Turn the cursor into an hourglass for the processing.
;-
;*****************************************************************************************************
PRO PixmapWidget::Copy, ERASE_WINDOW=erase_window, $
                        IMAGE=image,               $
                        ORDER=order,               $
                        ORIGIN=origin,             $
                        EXTENT=extent,             $
                        DESTINATION=dest,          $
                        HOURGLASS=hourglass

   @cat_pro_error_handler

   ; If drawing's been inhibited or the window ID is invalid return immediately
   IF (self._noDraw) THEN RETURN

   ; Ensure that the pixmap exists and is up-to-date
   IF (NOT self._isReady) THEN self -> Refresh, HOURGLASS=hourglass, /NO_DELETE
   IF (self._keep EQ 0) THEN BEGIN
      oldWin = !D.Window
      self -> SetWindow ; ensures the draw widget exists even if KEEP=0
      WSET, oldWin
   ENDIF

   ; Check the input parameters
   IF (N_ELEMENTS (origin) NE 2) THEN origin = [0, 0]
   IF (N_ELEMENTS (extent) NE 2) THEN extent = [self._xsize, self._ysize]
   IF (N_ELEMENTS (dest  ) NE 2) THEN dest   = [0, 0]

   ; Get the window ID of the draw widget
   WIDGET_CONTROL, self._drawWidget, GET_VALUE=windowID

   ; If this is a copy to screen and erase_window is set, erase the target window
   IF (NOT ARG_PRESENT (image) AND KEYWORD_SET (erase_window)) THEN ERASE

   ; If we're using an output variable, capture the current window
   IF ARG_PRESENT(image) THEN BEGIN
      currWin = !D.WINDOW
      WSET, windowID
      image = TVRead (origin[0], origin[1], extent[0], extent[1], ORDER=Keyword_Set(order))
      IF (currWin NE -1) THEN WSET, currWin
   ENDIF ELSE BEGIN
      DEVICE, COPY=[origin[0], origin[1], extent[0], extent[1], dest[0], dest[1], windowID]
   ENDELSE

   ; If keep = 0, destroy the temporary pixmap draw widget.
   self -> UnSetWindow

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::DRAW
;
; PURPOSE:
;
;       This method copies the contents of the pixmap widget object into the current window.
;       IF the REFRESH keyword is specified or the contents have changed, the contents are
;       regenerated before the copy.
;
; SYNTAX:
;
;       pixmapObj -> Draw
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       REFRESH:        Set this keyword to force the pixmap to refresh it's contents.
;
;       ERASE_WINDOW:   Set this keyword to erase the target window before drawing contents.
;
;       HOURGLASS:      Set this keyword to enable the hourglass cursor for the draw operation.
;
;       _EXTRA:         Any extra keywords appropriate for the "WidgetAtom::Draw" method.
;-
;*****************************************************************************************************
PRO PixmapWidget::Draw, REFRESH=refresh, $
                        ERASE_WINDOW=erase_window, $
                        HOURGLASS=hourglass, $
                        _EXTRA=extraKeywords

   ; Set up the error handler
   @cat_pro_error_handler

   ; Capture the current window ID
   IF Obj_Valid(self._targetWindow) THEN $
      self._targetWindow -> GetProperty, WindowID=targetWindow ELSE $
      targetWindow = !D.Window

   ; Enable the hourglass mouse pointer (in case this takes a while)
   IF (KEYWORD_SET (hourglass)) THEN WIDGET_CONTROL, /HOURGLASS

   ; If necessary, redraw the pixmap.
   ; Set the NO_DELETE flag so the draw widget is not deleted - we need it for the copy below
   IF ((NOT self._isReady) OR KEYWORD_SET (refresh)) THEN $
      self -> Refresh, /NO_DELETE, HOURGLASS=hourglass, _EXTRA=extraKeywords

   ; Copy the contents of the pixmap into the current window
   WSET, targetWindow
   IF (NOT self._noDraw) THEN self -> Copy, ERASE_WINDOW=erase_window

   ; Ensure the draw widget is destroyed (if KEEP=0)
   self -> UnSetWindow

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;        PIXMAPWIDGET::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the PIXMAPWIDGET object. It will typically
;        be used to respond to events from widget objects created in the CONTROLPANEL
;        method.
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
PRO PixmapWidget::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF

      'PIXMAP PROPERTYSHEET': BEGIN

         IF event.type EQ 0 THEN BEGIN

            CASE StrUpCase(event.identifier) OF

               ELSE: BEGIN
                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value
               END

            ENDCASE

         ENDIF

         END

   ENDCASE

   ; Report completion. Object may have been deleted.
   IF Obj_Valid(self) THEN self -> Report, /Completed
END

;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to obtain the PixmapWidget object's properties
;
; SYNTAX:
;
;       pixmapObject -> GetProperty, XSIZE=xsize, YSIZE=ysize
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;     BACKGROUNDCOLOR: The background color of the pixmap window.
;
;     KEEP:            The "keep" status of the pixmap.
;
;     NO_DRAW:         The flag to determine if the pixmap ignores draw invocations.
;
;     READY:           A flag to show if the window needs updating before copy.
;
;     RETAIN:          The RETAIN status of the window.
;
;     WINDOWID:        The window index number of the draw widget used as the pixmap. If
;                      the draw widget doesn't exist when the keyword is invoked (e.g, KEEP=0)
;                      then a -1 is returned.
;
;     XSIZE:           The width of the pixmap window canvas.
;
;     YSIZE:           The height of the pixmap window canvas.
;
;     _REF_EXTRA:      Any keywords appropriate for the  superclass GetProperty methods.
;-
;*****************************************************************************************************
PRO PixmapWidget::GetProperty, $
     BACKGROUNDCOLOR=backgroundColor, $
     KEEP=keep, $
     NO_DRAW=no_draw, $
     READY=ready, $
     RETAIN=retain, $
     WINDOWID=windowID, $
     XSIZE=xsize, $
     YSIZE=ysize, $
     _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   backgroundColor = self._backgroundColor
   keep            = self._keep
   ready           = self._isReady
   retain          = self._retain
   no_draw         = self._noDraw
   xsize           = self._xsize
   ysize           = self._ysize

   IF Arg_Present(windowID) THEN BEGIN
      IF Widget_Info(self._drawWidget,/Valid_ID) THEN BEGIN
         Widget_Control, self._drawWidget, Get_Value=windowID
      ENDIF ELSE windowID = -1L
   ENDIF

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> TOPLEVELBASE::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::MESSAGEHANDLER
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
PRO PixmapWidget::MessageHandler, title, SENDER=sender, DATA=data

   @cat_pro_error_handler

   IF N_Elements(title) EQ 0 THEN Message, 'Ill-formed message received. No title.'

   ; Handle various kinds of messages.
   CASE title OF

      'NEED_UPDATE': self -> Refresh

      'OBJECT_DELETED': BEGIN
         children = self -> Get(/ALL)
         index = Where(children EQ data, count)
         IF count GT 0 THEN self -> Remove, data
         self -> Refresh
         END

      'RESIZEDRAWWIDGET': self -> SetProperty, XSize=data[0], YSize=data[1]

      'SETWINDOW': self -> SetWindow

      ELSE: self -> TOPLEVELBASE::MessageHandler, title, SENDER=sender, DATA=data

   ENDCASE

   ; Report success
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::NOTIFY_REALIZE
;
; PURPOSE:
;
;       At realization, the pixmap's draw widget is initialized
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
PRO PixmapWidget::Notify_Realize, theObject

   @cat_pro_error_handler

   currentWindow = !D.Window

   self -> Refresh

   IF (currentWindow GE 0) AND WindowAvailable(currentWindow) THEN WSet, currentWindow

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::RESIZE
;
; PURPOSE:
;
;       This method resizes the canvas area of the pixmap widget.
;;
; SYNTAX:
;
;       pixmapObj -> Resize, xsize, ysize
;
; ARGUMENTS:
;
;       XSIZE:     The new X size of the canvas area of the draw widget, in pixels.
;
;       YSIZE:     The new Y size of the canvas area of the draw widget, in pixels.
;
; KEYWORDS:
;
;       NOREFRESH: Set this keyword to disable the default call to the REFRESH method when the draw widget
;                  resizing is completed. If you use this keyword, you will be responsible for setting
;                  the pixmap READY flag when the pixmap is ready for display.
;
;-
;*****************************************************************************************************
PRO PixmapWidget::Resize, xsize, ysize, NOREFRESH=norefresh, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(xsize) EQ 0 THEN Message, "XSIZE parameter is missing."
   IF N_Elements(ysize) EQ 0 THEN Message, "YSIZE parameter is missing."
   If Widget_Info(self._drawWidget, /Valid_ID) THEN $
      Widget_Control, self._drawWidget, Draw_XSize=xsize, Draw_YSize=ysize
   self._xsize = xsize
   self._ysize = ysize

  IF ~Keyword_Set(norefresh) THEN self -> Refresh

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::REFRESH
;
; PURPOSE:
;
;       This method regenerates the contents of the pixmap window.
;
; SYNTAX:
;
;       pixmapObj -> Refresh
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       HOURGLASS:      Set this keyword to enable the hourglass cursor for the draw operation.
;
;       NO_DELETE:      Set this keyword to prevent the method from destroying the draw widget
;                       when KEEP=0. Primarily when the refresh is called from the COPY method.
;
;       REQUESTER:      This optional keyword is set to the object that requests a DRAW of the
;                       DrawWidget. This is helpful sometimes when DRAWWIDGET_DRAW messages
;                       are received by other objects. The object reference is passed on as
;                       the DATA in the DRAWWIDGET_DRAW message.
;
;       TARGETS:        Typically, calling the REFRESH method of a PixmapWidget will call the DRAW
;                       method of any objects in its container. However, if the TARGETS keyword
;                       is set to an object reference (or array of object references), only these
;                       objects will be drawn. This would allow you, for example, to re-draw only
;                       a single image object in a window with several image objects.
;
;       _EXTRA:         Any extra keywords appropriate for the "WidgetAtom::Draw" method.
;-
;*****************************************************************************************************
PRO PixmapWidget::Refresh, HOURGLASS=hourglass, $
                           NO_DELETE=no_delete, $
                           REQUESTER=requester, $
                           TARGETS=targets, $
                           _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Process the input keywords
   IF (KEYWORD_SET (hourglass)) THEN WIDGET_CONTROL, /HOURGLASS

   ; Switch the window, erase, redraw and switch back to the original window
   oldwin = !D.Window
   self -> SetWindow

   ; Erase the pixmap window
   ERASE, Color=FSC_Color(self._backgroundColor)

   IF N_Elements(targets) NE 0 THEN BEGIN

      contents = self -> Get(/All, /Recursive_Search)

      FOR j=0,N_Elements(targets)-1 DO BEGIN

         ; If you can find the target in the list of objects, then draw it.
         i = Where(contents EQ targets[j], count)
         IF count GT 0 THEN targets[j] -> Draw

      ENDFOR

   ENDIF ELSE BEGIN

      ; Call the superclass draw method to draw children of pixmap.
      self -> TOPLEVELBASE::Draw, REQUESTER=requester, _EXTRA=extrakeywords

   ENDELSE

   ; Switch back to the original window.
   self -> UnSetWindow, NO_DELETE=no_delete
   IF oldwin NE -1 AND WindowAvailable(oldwin) THEN WSET, oldwin
   self._isReady = 1B

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the PixmapWidget object's properties
;
; SYNTAX:
;
;       PixmapWidget -> SetProperty, XSIZE=300, YSIZE=500
;
; ARGUMENTS:
;
;       None.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;     BACKGROUNDCOLOR: The background color of the pixmap window.
;
;     DRAW_COLOR:      The name of the drawing color to use for overlays.
;
;     KEEP:            Keep the pixmap in memory, rather than re-create it when needed.
;
;     NO_DRAW:         Set this keyword to 1 to make pixmap ignore all DRAW method calls.
;
;     READY:           A flag to show if the window needs updating before copy.
;
;     XSIZE:           The width of the pixmap window canvas.
;
;     YSIZE:           The height of the pixmap window canvas.
;
;     _EXTRA:          Any keywords appropriate for the  superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO PixmapWidget::SetProperty, $
   BACKGROUNDCOLOR=backgroundColor, $
   DRAW_COLOR=overlayColor, $
   KEEP=keep, $
   NO_DRAW=no_draw, $
   READY=ready, $
   XSIZE=xsize, $
   YSIZE=ysize, $
    _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS(xsize) NE 0) OR (N_ELEMENTS (ysize) NE 0) THEN $
   BEGIN
      self._isReady = 0B
      IF N_Elements(xsize) EQ 0 THEN self -> GetProperty, XSize=xsize
      IF N_Elements(ysize) EQ 0 THEN self -> GetProperty, YSize=ysize
      self -> Resize, xsize, ysize, REFRESH=1-Keyword_Set(no_draw)
   ENDIF

   IF (N_ELEMENTS (backgroundColor) NE 0) THEN self._backgroundColor = backgroundColor

   IF (N_ELEMENTS (keep ) NE 0) THEN BEGIN
      self._keep = KEYWORD_SET(keep)
      IF self._keep EQ 0 THEN BEGIN
         Obj_Destroy, self._snapshot
         self -> UnSetWindow
      ENDIF
   ENDIF
   IF (N_ELEMENTS (no_draw) NE 0) THEN self._noDraw = KEYWORD_SET (no_draw)

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> TOPLEVELBASE::SetProperty, _EXTRA=extraKeywords

   IF N_Elements(ready) NE 0 THEN self._isReady = Keyword_Set(ready)
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::SETWINDOW
;
; PURPOSE:
;
;       This method sets the window pixmap widget to be the current graphics window.
;       Note that when the pixmap is no longer required to be the current window, the
;       UnSetWindow method below should be called which will delete the draw widget
;       (and free the memory resources) if required.
;
; SYNTAX:
;
;       thisPixmapObj -> SetWindow
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
PRO PixmapWidget::SetWindow

   @cat_pro_error_handler

   ; If the draw widget is invalid (e.g. KEEP=0), [re]create it
   IF (WIDGET_INFO (self._drawWidget, /VALID_ID) EQ 0) THEN $
      self._drawWidget = WIDGET_DRAW (self._ID, XSIZE=self._xsize, YSIZE=self._ysize, RETAIN=self._retain)

   WIDGET_CONTROL, self._drawWidget, GET_VALUE=windowID
   WSet, windowID

   ; If we have a valid snapshot (e.g. KEEP=0), paste it into the window
   IF ((self._keep EQ 0) AND OBJ_VALID (self._snapshot)) THEN self._snapshot -> Draw

END



;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::UnSetWindow
;
; PURPOSE:
;
;       This method is used to tell the pixmap that it is no longer required
;       as the current window. This is so that if KEEP=0, the draw widget
;       can be destroyed.
;
; SYNTAX:
;
;       thisPixmapObj -> UnSetWindow
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       NO_DELETE:      If this keyword is set, the draw widget is not destroyed.
;
;       NO_SNAPSHOT:    If this keyword is set, a snapshop of the pixmap is not created.
;-
;*****************************************************************************************************
PRO PixmapWidget::UnSetWindow, NO_DELETE=no_delete, NO_SNAPSHOT=no_snapshot

   @cat_pro_error_handler

   ; If KEEP=0 and the draw widget is valid, capture a snapshot and destroy the draw widget.
   IF ((self._keep EQ 0) AND WIDGET_INFO (self._drawWidget, /VALID_ID)) THEN $
   BEGIN
      IF (NOT KEYWORD_SET (no_snapshot)) THEN $
      BEGIN
         ; If you haven't taken a snapshot of the pixmap window yet, do it now.
         WIDGET_CONTROL, self._drawWidget, GET_VALUE=windowID
         WSet, windowID
         IF (OBJ_VALID (self._snapshot) EQ 0) THEN BEGIN
            self._snapshot = OBJ_NEW ('CatImage', TVRead(TRUE=3), DISPLAY_MODE=1)
         ENDIF ELSE BEGIN
            self._snapshop -> SetProperty, Image=TVRead(TRUE=3)
         ENDELSE
      ENDIF
      IF ((self._keep EQ 0) AND NOT KEYWORD_SET (no_delete)) THEN WIDGET_CONTROL, self._drawWidget, /DESTROY
   ENDIF
END


;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the PIXMAPWIDGET object class destructor method.
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
PRO PixmapWidget::CLEANUP

   @cat_pro_error_handler

   IF (OBJ_VALID (self._snapshot)) THEN OBJ_DESTROY, self._snapshot
   IF (WIDGET_INFO (self._ID, /VALID_ID)) THEN WIDGET_CONTROL, self._ID, /DESTROY

   self -> TOPLEVELBASE::CLEANUP

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       PIXMAPWIDGET::INIT
;
; PURPOSE:
;
;       This is the PIXMAPWIDGET object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     theParent: An object reference to a WIDGETATOM-subclassed object.
;                This is the same as using the PARENT keyword.
;
; KEYWORDS:
;
;     BACKGROUNDCOLOR:  The name of the background color of the pixmap window. "Black" by default.
;
;     KEEP:             Keep the pixmap in memory, rather than re-create it when needed. Set to 1 by default.
;
;     MAP:              Set this keyword to make the pixmap visible (for testing purposes).
;
;     NO_DRAW:          The flag to determine if the pixmap ignores draw invocations.
;
;     PARENT:           The object to be parent of this one in a hierarchy.
;
;     RETAIN:           Set this keyword to determine how backing store is handled. See IDL WIDGET_DRAW
;                       documentation for details. Set to 1 by default for Windows machines and to 2 otherwise.
;
;     TARGETWINDOW:     Set this to a window object reference that will be the target window for the pixmap.
;                       If this property is undefined, the current graphics window is used as the target window.
;
;     UNITS:            The units for measurments. The default is 0 for pixels. Other values are
;                       1 for inches, and 2 for centimeters.
;
;     XSIZE:            The width of the pixmap window canvas.
;
;     YSIZE:            The height of the pixmap window canvas.
;
;     _EXTRA:           Any keywords appropriate for the "WidgetAtom::Init" method.
;-
;*****************************************************************************************************
FUNCTION PixmapWidget::INIT, theParent,             $
                             BACKGROUNDCOLOR=backgroundColor, $
                             KEEP=keep, $
                             MAP=map, $
                             NO_DRAW=no_draw,       $
                             PARENT=parent,         $
                             RETAIN=retain,         $
                             TARGETWINDOW=targetwindow, $
                             UNITS=units,           $
                             XSIZE=xsize,           $
                             YSIZE=ysize,           $
                             _EXTRA=extraKeywords

   @cat_func_error_handler

   ; Set default values.
   IF N_Elements(backgroundColor) EQ 0 THEN backgroundColor = 'Black'
   IF N_Elements(keep) EQ 0 THEN keep = 1
   keep = Keyword_Set(keep)
   no_draw = Keyword_Set(no_draw)
   map = Keyword_Set(map)
   IF N_Elements(retain) EQ 0 THEN retain=(!Version.OS_Family EQ 'Windows')?1:2
   IF N_Elements(xsize) EQ 0 THEN xsize = 400
   IF N_Elements(ysize) EQ 0 THEN ysize = 400

      ; The parent can be passed in as an argument, or as a keyword. Resolve
      ; the two here.

   IF (N_ELEMENTS (theParent) EQ 1) THEN parent = theParent

      ; Create an (unmapped) TLB to use as a parent, then add a draw widget and realize.

   ok = self -> TOPLEVELBASE::INIT (Parent=parent, $
                                    TITLE='Pixmap Object', $
                                    Group_Leader=parent, $
                                    /NO_MBAR, $
                                    MAP=map, $
                                    ;TLB_FRAME_ATTR=11, $
                                    _Extra=extrakeywords)
   IF NOT ok THEN Message, 'TopLevelBase initialization failed.'

   self._keep              = keep
   self._retain            = retain
   self._xsize             = xsize
   self._ysize             = ysize
   self._isReady           = 1B
   self._backgroundColor   = backgroundColor
   self._noDraw            = no_draw
   self._map = map
   IF Obj_Valid(targetwindow) THEN self._targetWindow = targetWindow
   IF (self._keep) THEN BEGIN
      self._drawWidget = WIDGET_DRAW (self._ID, XSIZE=self._xsize, YSIZE=self._ysize, $
         NOTIFY_REALIZE='CatRealizeNotify', UNITS=units, UVALUE=self, RETAIN=self._retain)
   ENDIF

      ; Realize the pixmap object. Nothing shows up on display, since
      ; the draw widget is in an unmapped top-level base.
   winID = !D.Window
   self -> TOPLEVELBASE::Draw, /Just_Register
   IF (winID NE -1) THEN WSET, winID

   ; Register properties
   self->RegisterProperty, 'Map', 1, NAME="Map"
   self -> SetPropertyByIdentifier, 'Map', self._map
   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       PIXMAPWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the PIXMAPWIDGET object class definition.
;       The PIXMAPWIDGET object is subclassed from the WIDGETATOM object.
;
;*****************************************************************************************************
PRO PixmapWidget__DEFINE, class

   class = { PixmapWidget, $
             _backgroundColor: "", $     ; The name of the background color.
             _drawWidget: 0L, $          ; The Widget ID of the draw widget.
             _keep: 0B, $                ; To conserve pixmap memory, KEEP=1 retains the pixmap, KEEP=0 does not.
             _isReady: 0B, $             ; A flag to show if the window needs updating before copy.
             _noDraw: 0B, $              ; A flag to inhibit drawing operation.
             _retain: 0B, $              ; A flag for the RETAIN status of the draw widget.
             _snapshot: OBJ_NEW(), $     ; For KEEP=0, a snapshot of the required pixmap
             _targetWindow: OBJ_NEW(), $ ; A window object that is to be the target of the pixmap.
             _xsize: 0L,  $              ; The X size of the pixmap window.
             _ysize: 0L,  $              ; The Y size of the pixmap window.
             INHERITS TOPLEVELBASE }

END


