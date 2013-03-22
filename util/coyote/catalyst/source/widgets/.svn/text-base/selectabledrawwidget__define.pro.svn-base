;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEDRAWWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to subclass a draw widget so that items in the
;       draw widget can be "selectable" and interactive. The type of interaction available
;       is limited to moving objects (with the left mouse button) or calling their
;       control panel (with the right mouse button).
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
;       selectableDrawWidget = Obj_New("SelectableDrawWidget", theParent)
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
;   class = { SelectableDrawWidget,          $
;             _event_handler: Obj_New(), $ ; The real event handler object(s) for the DROPLISTWIDGET.
;             _event_method_real: "", $    ; The event method assigned by the user to this object widget.
;             _select_events: 0L, $        ; A flag that indicates SELECT_EVENTS are turned on.
;             INHERITS DRAWWIDGET $        ; INHERITS DRAWWIDGET capabilities.
;             }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W.Fanning, 31 March 2005.
;       Added ability to double-buffer output through BUFFER_OUTPUT keyword. 8 July 2005. DWF.
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
;       SELECTABLEDRAWWIDGET::EVENTHANDLER
;
; PURPOSE:
;
;       This is the event handler method of the object. The purpose is to check
;       for SELECTABLEOBJECTS if SELECT_EVENTS is set. Otherwise, the object acts
;       identically to a draw widget.
;
; SYNTAX:
;
;       widgetObject -> EventHandler, event
;
; ARGUMENTS:
;
;       event:  The event created by the draw widget.
;-
;*****************************************************************************************************
PRO SelectableDrawWidget::EventHandler, event

   @cat_pro_error_handler

   ; Get the event objects.
   eventObjs = self._event_handler -> Get(/All)

   ; Make yourself the current graphics window.
   self -> SetWindow

   ; Are select events turned on for the draw widget? If not, immediately dispatch
   ; the event to the intended recepient.
   IF self._select_events AND (event.event_name EQ 'WIDGET_DRAW') THEN $
   BEGIN

      ; What kind of event is this? Only draw widget events should get this far.
      eventTypes = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSED', 'CHARACTER', 'KEYPRESS_CH', 'KEYPRESS_KEY']
      thisEvent = eventTypes[event.type]

      CASE thisEvent OF

         'DOWN': BEGIN

           ; Did you click in a selectable object?
           objects = self -> SelectObjects(event.x, event.y, Count=count)

           ; Did you find a selectable object?
            IF count GT 0 THEN BEGIN

              ; Return the first SELECTABLEOBJECT object you find.
              theObject = (Reverse(objects))[0]
              ;IF Obj_Isa_Valid(theObject, 'SELECTABLEOBJECT') THEN BEGIN
              IF Obj_Valid(theObject) THEN BEGIN

                 ; Did you click the LEFT mouse button? Then we are moving the object.
                 CASE event.press OF
                     1: BEGIN

                          self._selectableObject = theObject
                          self._sx = event.x
                          self._sy = event.y

                          ; Motion events on. Create pixmap.
                          self -> GetProperty, MOTION_EVENTS=mevent
                          self._save_on_event = mevent
                          self -> SetProperty, MOTION_EVENTS=1
                          self -> GetProperty, XSize=xsize, YSize=ysize, Initial_Color=icolor
                          self._select_pixmap = Obj_New('PixmapWidget', XSize=xsize,  YSize=ysize, BackgroundColor=icolor)
                          IF self._buffer_output THEN $
                           self._buffer_pixmap = Obj_New('PixmapWidget', XSize=xsize,  YSize=ysize, BackgroundColor=icolor)

                          ; Slightly different procedure if you have buffered output.
                          IF self._buffer_output THEN BEGIN

                             ; Quick draw of pixmapwindow with this selected objects turned off. Want
                             ; this for quick re-draw during movement.
                             self._selectableObject -> SetProperty, Visible=0, /NoMessage, /NoRefresh
                             self -> Draw, Target_Window=self._buffer_pixmap
                             self._select_pixmap -> SetWindow
                             self._buffer_pixmap -> Copy

                             ; Draw the selected object in the window.
                             self._selectableObject -> SetProperty, Visible=1, /NoMessage, /NoRefresh
                             self._buffer_pixmap -> SetWindow
                             self._selectableObject -> Draw
                             self -> SetWindow
                             self._buffer_pixmap -> Copy

                          ENDIF ELSE BEGIN

                             ; Quick draw of pixmapwindow with this selected objects turned off. Want
                             ; this for quick re-draw during movement.
                             self._selectableObject -> SetProperty, Visible=0, /NoMessage, /NoRefresh
                             self -> Draw, Target_Window=self._select_pixmap
                             self._select_pixmap -> SetWindow
                             self._selectableObject -> SetProperty, Visible=1, /NoMessage, /NoRefresh
                             self._selectableObject -> Draw
                             self -> SetWindow
                             self._select_pixmap ->  Copy

                          ENDELSE

                           RETURN
                        END
                     4: BEGIN
                          ; Did you click in a selectable object?
                          objects = self -> SelectObjects(event.x, event.y, Count=count)

                          ; Did you find a selectable object?
                           IF count GT 0 THEN BEGIN

                             ; Return the first SELECTABLEOBJECT object you find.
                             theObject = (Reverse(objects))[0]
                             ;IF Obj_Isa_Valid(theObject, 'SELECTABLEOBJECT') THEN BEGIN
                             IF Obj_Valid(theObject) THEN BEGIN
                                 theObject -> SelectPanel, event.x, event.y, event.id;
                                 ;theObject -> ControlPanel, Group_Leader=event.id
                             ENDIF

                           RETURN
                           ENDIF

                        END
                     ELSE:
                 ENDCASE

              ENDIF ; of found a SELECTABLEOBJECT.

            ENDIF ; of COUNT GT 0.

            END

         'MOTION': BEGIN

               IF Obj_Valid(self._selectableObject) THEN BEGIN
                    deltaX = event.x - self._sx
                    deltaY = event.y - self._sy

                    ; Slightly different procedure if you have buffered output
                    IF self._buffer_output THEN BEGIN

                       self._buffer_pixmap -> SetWindow
                       self._select_pixmap -> Copy
                       self._selectableObject -> Move, deltaX, deltaY, /NoDraw
                       self._selectableObject -> Draw
                       self -> SetWindow
                       self._buffer_pixmap -> Copy

                    ENDIF ELSE BEGIN

                       self._selectableObject -> SetProperty, Visible=0, /NoMessage, /NoRefresh
                       self -> Draw, Target_Window=self._select_pixmap
                       self._selectableObject -> Move, deltaX, deltaY, /NoDraw
                       self._select_pixmap -> SetWindow
                       self._selectableObject -> SetProperty, Visible=1, /NoMessage, /NoRefresh
                       self._selectableObject -> Draw
                       self -> SetWindow
                       self._select_pixmap -> Copy

                    ENDELSE

                    self._sx = event.x
                    self._sy = event.y
                    RETURN
                 ENDIF
            END

         'UP': BEGIN
               IF Obj_Valid(self._selectableObject) THEN BEGIN

                    IF self._buffer_output THEN BEGIN
                       self -> Draw, Target_Window=self._buffer_pixmap
                       self -> SetWindow
                       self._buffer_pixmap -> Copy
                    ENDIF ELSE BEGIN
                       self -> Draw, Target_Window=self._select_pixmap
                       self -> SetWindow
                       self._select_pixmap -> Copy
                    ENDELSE

                    ; Prepare an event for sending to the draw widget event handler.
                    event.ID = self._selectableObject
                    event.name = self._selectableObject -> GetName()
                    event.event_name = 'SELECTABLE_OBJECT_MOVED'

                    ; Clean up.
                    Obj_Destroy, self._select_pixmap
                    Obj_Destroy, self._buffer_pixmap
                    self._selectableObject = Obj_New()
                    self -> SetProperty, MOTION_EVENTS=self._save_on_event
               ENDIF

            END

         ELSE:  ; Not DOWN, MOTION, OR UP.

      ENDCASE

   ENDIF ; SELECT_EVENTS = 1

   ; Find out the target object for the event, get the event method from
   ; the target object, and send the event to this method, UNLESS this
   ; object has its own object method.
   FOR e = 0, N_ELEMENTS (eventObjs) - 1 DO $
   BEGIN
      IF OBJ_VALID (eventObjs [e]) THEN $
      BEGIN
         event.handler = eventObjs[e]
         eventObjs[e] -> CatAtom::GetProperty, Event_Method=event_method
         IF (self._event_method_real NE "") AND (e EQ 0) THEN $
            thisMethod = self._event_method_real ELSE thisMethod = event_method
         Call_Method, thisMethod, eventObjs[e], event
      ENDIF
   ENDFOR

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEDRAWWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to obtain the SelectableDrawWidget object's properties
;
; SYNTAX:
;
;       aSelectableDrawWidget -> GetProperty, WINDOW_ID=wid
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;      BUFFER_OUTPUT:     Set to 1 if buffer output is turned on, to 0 otherwise.
;    
;      BUFFER_PIXMAP:     The buffer pixmap object, if one is available.
;
;      SELECT_EVENTS:     Set to 1 if SELECT_EVENTS is turned on, to 0 otherwise.
;
;       _REF_EXTRA:       Any keywords appropriate for the superclass GetProperty methods.
;-
;*****************************************************************************************************
PRO SelectableDrawWidget::GetProperty, $
   BUFFER_OUTPUT=buffer_output, $
   BUFFER_PIXMAP=buffer_pixmap, $
   SELECT_EVENTS=select_events, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(buffer_pixmap) THEN buffer_pixmap = self._buffer_pixmap
   IF Arg_Present(buffer_output) THEN buffer_output = self._buffer_output
   IF Arg_Present(select_events) THEN select_events = self._select_events

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> DrawWidget::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEDRAWWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the SelectableDrawWidget object's properties
;
; SYNTAX:
;
;       aSelectableDrawWidget -> SetProperty, BUTTON_EVENTS=1
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;
;       BUFFER_OUTPUT:   Normally, output to the display window is not buffered.
;                        Depending upon what is in the window, this may result in
;                        distracting flashing. If this is undesireable, set this keyword
;                        and the output will be double-buffered to the display window.
;                        
;       INITIAL_COLOR:   The name of the initial color for the draw widget. Used when realized and
;                        if the draw widget is set up to erase before display (i.e., ERASE_WINDOW=1).    
;
;       SELECT_EVENTS:   Turns select events on (1) or off (0).
;
;       _EXTRA:          Any keywords appropriate for the superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO SelectableDrawWidget::SetProperty, $
   BUFFER_OUTPUT=buffer_output, $
   INITIAL_COLOR=initial_color, $
   SELECT_EVENTS=select_events, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_ELEMENTS(buffer_output) GT 0 THEN self._buffer_output = Keyword_Set(buffer_output)
   IF N_ELEMENTS (initial_color) GT 0 THEN BEGIN
      self._initialcolor = initial_color
      IF Obj_Valid(self._buffer_pixmap) THEN self._buffer_pixmap -> SetProperty, BACKGROUNDCOLOR=initial_color
   ENDIF
   IF N_ELEMENTS(select_events) GT 0 THEN self._select_events = Keyword_Set(select_events)

   ; Call the superclass SetProperty methods.
   self -> DrawWidget::SetProperty, _EXTRA=extraKeywords

   ; Make sure you have a valid widget here.
   IF Obj_Valid(self) THEN BEGIN
      self -> Report, /Completed
      RETURN
   ENDIF

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEDRAWWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the SELECTABLEDRAWWIDGET object class destructor method.
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
;       _EXTRA:  Any keyword appropriate for the superclass CLEANUP methods.
;-
;*****************************************************************************************************
PRO SelectableDrawWidget::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self._buffer_pixmap
   Obj_Destroy, self._select_pixmap

   ; Remove all the children in self._event_handler and destroy the container.
   self._event_handler -> Remove, /All
   Obj_Destroy, self._event_handler

   self ->DRAWWIDGET::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEDRAWWIDGET::INIT
;
; PURPOSE:
;
;       This is the SELECTABLEDRAWWIDGET object class initialization method
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
;       BUFFER_OUTPUT:   Output to the display window is typically buffered to some extent,
;                        but still requires drawing to a pixmap window, which can be slower
;                        than you like under some circumstances. Setting this keyword creates
;                        a second pixmap so that output can be double-buffered. This results
;                        in faster display, but at the cost of additional window memory.
;
;       SELECT_EVENTS:   Set this keyword to turn selection events (the ability to sense SELECTABLEOBJECTS)
;                        on for this draw widget. The keyword is turned OFF by default to conform with
;                        the expectations of normal draw widgets.
;
;       _EXTRA:          Any keyword appropriate for the superclass INIT methods.
;
;-
;*****************************************************************************************************
FUNCTION SelectableDrawWidget::INIT, parent, $
   BUFFER_OUTPUT=buffer_output, $
   EVENT_METHOD=event_method, $   ; Required to intercept event method intended for CATATOM.
   EVENT_OBJECTS=event_objects, $ ; Required to intercept event objects intended for CATATOM.
   SELECT_EVENTS=select_events, $
   _EXTRA=extraKeywords

   @cat_func_error_handler

   ; Call superclass INIT method, report and return success.
   IF NOT self -> DrawWidget::INIT (parent,_EXTRA=extraKeywords) THEN $
     Message, 'DrawWidget::INIT method failed to complete.'

   self._select_events = Keyword_Set(select_events)

  ; The following is required because the SELECTABLEDRAWWIDGET is a compound object.
  ; Get the assigned event object. Replace this with the self object. This
  ; assures you that the events will go through the SELECTABLEDRAWWIDGET's EventHandler
  ; method first. Save the assigned event object so it can be used at the end
  ; of the SELECTABLEDRAWWIDGET's EventHandler method.
   IF N_Elements(event_objects) EQ 0 THEN $
   BEGIN
      parent -> CATATOM::GetProperty, Event_Objects=event_objects
      self._event_handler = OBJ_NEW ('CatContainer', Memory_Management=0)
      FOR j=0,N_Elements(event_objects) -1 DO BEGIN
         IF Obj_Valid(event_objects[j]) THEN self._event_handler -> Add, event_objects[j]
      ENDFOR
   ENDIF ELSE BEGIN
      self._event_handler = OBJ_NEW ('CatContainer', Memory_Management=0)
      FOR j=0,N_Elements(event_objects) -1 DO BEGIN
         IF Obj_Valid(event_objects[j]) THEN self._event_handler -> Add, event_objects[j]
      ENDFOR
   ENDELSE

  ; All events MUST come to the EventHandler method before they are dispatched
  ; elsewhere. This is typical of compound objects.
   IF N_Elements(event_method) NE 0 THEN self._event_method_real = event_method
   self._event_method = 'EventHandler'

   self._buffer_output = Keyword_Set(buffer_output)

   self -> Report, /Completed

   RETURN, 1
END



;*****************************************************************************************************
;
; NAME:
;       SELECTABLEDRAWWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the SELECTABLEDRAWWIDGET object class definition.
;       The SELECTABLEDRAWWIDGET object is subclassed from the WIDGETATOM object.
;
;*****************************************************************************************************
PRO SelectableDrawWidget__DEFINE, class

   class = { SelectableDrawWidget,          $
             _buffer_output: 0L, $           ; Flag indicating buffered output.
             _buffer_pixmap: Obj_New(), $    ; A buffer pixmap for double-buffering display.
             _event_handler: Obj_New(), $    ; The real event handler object(s) for the DROPLISTWIDGET.
             _event_method_real: "", $       ; The event method assigned by the user to this object widget.
             _save_on_event: 0L, $           ; A flag set to the condition of the original MOTION_ON event state.
             _select_events: 0L, $           ; A flag that indicates SELECT_EVENTS are turned on.
             _selectableObject: Obj_New(), $ ; The selectable object found in the window.
             _select_pixmap: Obj_New(), $    ; A pixmap for dragging selectable objects.
             _sx: 0L, $                      ; The X static corner of the drag box.
             _sy: 0L, $                      ; The Y static corner of the drag box.
             INHERITS DRAWWIDGET $           ; INHERITS DRAWWIDGET capabilities.
             }
END