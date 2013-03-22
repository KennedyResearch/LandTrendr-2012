;*****************************************************************************************************
;+
; NAME:
;       INTERACTION__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement the most basic of interactions.
;       An "interaction" is an operation that takes over control of a draw widget and
;       allows the user to interact with the draw widget in some way. For example,
;       drawing ROI's on a draw widget is a good example of an interaction. Most interactions
;       will be subclassed from this INTERACTION object. We have spent an inordinate amount
;       of time on interactions, but in the end they are *extremely* complicated to program.
;       I am of the mind now that users should program their own functionality and not
;       rely on interactions, since the programming effort to create interactions seems
;       disproportional to the results. I'm saying that most of the time, interactive functionality
;       can be programmed as normal event handler procedures, rather than relying on interactions.
;       That said, the interactions we have programmed do work, and they have been instrumental
;       in producing some very nice functionality in Catalyst applications. Ideas for simplifying
;       interactions are especially needed.
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
;       theObject = Obj_New("INTERACTION")
;
; SUPERCLASSES:
;
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { INTERACTION, $
;             _ask_on_up: 0B, $                     ; Flag for UP button dialog widget.
;             _drawID: Obj_New(), $                 ; The draw widget whose events are being hijacked.
;             _drawID_events: IntArr(7), $          ; Storage for the draw widget event types.
;             _drawID_excl_event_obj: Obj_New(), $  ; The old exclusive event (if there is one).
;             _drawID_pixmap: Obj_New(), $          ; A pixmap for storing the drawID picture.
;             _drawID_event_objects: Ptr_New(), $   ; The event objects for the draw widget.
;             _click_x: 0L, $                       ; The X location of button down event.
;             _click_y: 0L, $                       ; The Y location of button down event.
;             _contextmenu: Obj_New(), $            ; The context menu.
;             _mode: "", $                          ; The "mode" of the interaction: eg., MOVE or DRAW.
;             _noPicture: 0L, $                     ; A flag: Should picture be restored at end of interaction?
;             _roi_color: "", $                     ; The color of the roi.
;             _selectedObject: Obj_New(), $         ; A selectable interaction object that can be moved.
;             _statusbar: Obj_New(), $              ; A statusbar object. Can be passed messages, etc.
;             INHERITS CATATOM $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 10 February 2004.
;       Added DRAW_REALIZE keyword. 14 February 2005.
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
;       INTERACTION::ADD
;
; PURPOSE:
;
;       This method is where you can screen what kinds of objects are
;       added to this object's hierarchy. The method is not always needed.
;       If you do create it, be CERTAIN to call the superclass ADD method
;       or your program will not work correctly.
;
; SYNTAX:
;
;       theObject -> Add, object
;
; ARGUMENTS:
;
;     object:     The object to be added to this one.
;
; KEYWORDS:
;
;     _EXTRA:     Any keywords appropriate for the superclass Add method.
;-
;*****************************************************************************************************
PRO Interaction::Add, object, _EXTRA=extraKeywords

   @cat_pro_error_handler

   Message, 'An object cannot be added to an INTERACTION object.'

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       INTERACTION::CLEARABANDONEDOBJECTS
;
; PURPOSE:
;
;       Many of the interaction objects create an object and add it to the draw widget.
;       For example, the RUBBERBANDBOX interaction adds a BOX object to the draw widget.
;       If the ASK_ON_UP keyword is set, this object may be abandoned by the user not
;       selecting either the Accept or Cancel button. Since I cannot anticipate when this
;       will happen, this method is about recovering from those unfortunate times when I
;       cannot tell what has happened previously. I might callt his method when the user
;       switches interactions, for example. The method will simple look for an object
;       with the CLASSNAME prepended to the word "SELECTABLE_". If it finds it, it will
;       destroy it.
;
; SYNTAX:
;
;       theObject -> ClearAbandonedObjects
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO Interaction::ClearAbandonedObjects

   @cat_pro_error_handler
   abandonedObject = self._drawID -> Get('SELECTABLE_' + Obj_Class(self), Count=foundit)
   IF foundit THEN BEGIN
      Obj_Destroy, abandonedObject
      self._drawID -> SetWindow
      self._drawID_pixmap -> Copy
   ENDIF

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       INTERACTION::DRAW
;
; PURPOSE:
;
;       This method draws the "interaction" in the draw widget.
;
; SYNTAX:
;
;       theObject -> Draw
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO Interaction::Draw, _EXTRA=extra

   @cat_pro_error_handler

   self._drawID ->SetWindow
   PLOTS, self._click_x, self._click_y, /Device, PSYM=2, SymSize=2, Color=FSC_Color(self._roi_color)
   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;        INTERACTION::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the INTERACTION object. It will be used
;        to respond to event generated in the draw widget.
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
PRO Interaction::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Is this a tracking event? If so, handle it here and RETURN.
   IF event.event_name EQ 'WIDGET_TRACKING' THEN BEGIN

      IF Obj_Valid(self._statusbar) THEN BEGIN
         IF event.enter THEN BEGIN
            event.id -> GetProperty, Helpline=theText
            IF N_Elements(theText) NE 0 THEN self._statusbar -> SetProperty, Text=theText
         ENDIF ELSE self._statusbar -> SetProperty, Text='Interaction: Waiting...'
      ENDIF
      RETURN
   ENDIF

   ; Otherwise, branch on event name.
   CASE event.name OF

      'ACCEPT': BEGIN

                  ; Clean up.
                  self -> RestoreDisplay

                  ; Send an event to the real draw widget event handlers.
                   thisEvent = event
                  thisEvent.ID = self._drawID
                  thisEvent.HANDLER = Obj_New()
                  thisEvent.EVENT_NAME='INTERACTION_EVENT'
                  thisEvent.NAME = self._name

                  ; Convert the return coordinates, if you can.
                  self._drawID -> GetProperty, Coord_Object=coords
                  IF Obj_Valid(coords) THEN BEGIN
                     coords -> Draw
                     c = Convert_Coord(self.click_x, self.click_y, /Device, /To_Data)
                     x = c[0,0]
                     y = c[1,0]
                  ENDIF ELSE BEGIN
                     x = self.click_x
                     y = self.click_y
                  ENDELSE

                  ; Add coordinates to the event structure and send the event.
                  thisEvent = Create_Struct(thisEvent, 'x', x, 'y', y)
                  self -> SendEvent, thisEvent

                  RETURN

                END

      'CANCEL': BEGIN

                  ; Restore the picture in the draw window.
                  self._drawID -> SetWindow
                  self._drawID_pixmap -> Copy
                  RETURN

                END

      ELSE: $ ; Draw widget events handled there
         BEGIN

            ; Down events
            IF event.type EQ 0 THEN BEGIN

               ;All but RIGHT button.
               IF event.press NE 4 THEN BEGIN
                  self._click_x = event.x
                  self._click_y = event.y
                  self -> Draw
                  Empty ; Must flush graphics buffer here!
                  IF self._ask_on_up THEN BEGIN
                     Widget_DisplayContextMenu, event.id -> GetID(), event.x+10, event.y-10, self._contextMenu->GetID()
                  ENDIF ELSE BEGIN

                     ; Clean up.
                     self -> RestoreDisplay

                     ; Create an event structure for this interaction..
                     thisEvent = event
                     thisEvent.ID = self._drawID
                     thisEvent.HANDLER = Obj_New()
                     thisEvent.EVENT_NAME='INTERACTION_EVENT'
                     thisEvent.NAME = self._name

                     ; Convert the return coordinates, if you can.
                     self._drawID -> GetProperty, Coord_Object=coords
                     IF Obj_Valid(coords) THEN BEGIN
                        coords -> Draw
                        c = Convert_Coord(self._click_x, self._click_y, /Device, /To_Data)
                        x = c[0,0]
                        y = c[1,0]
                     ENDIF ELSE BEGIN
                        x = self._click_x
                        y = self._click_y
                     ENDELSE

                     ; Add coordinates to the event structure and send the event.
                     thisEvent = Create_Struct(thisEvent, 'x_loc', x, 'y_loc', y)
                     self -> SendEvent, thisEvent

                  ENDELSE
               ENDIF

            ENDIF

            ; Release events with RIGHT button.
            IF (event.type EQ 1) AND (event.release EQ 4) THEN BEGIN
               Widget_DisplayContextMenu, event.id -> GetID(), event.x+10, event.y-1, self._contextMenu->GetID()
            ENDIF
         END

   ENDCASE

   ; Report completion
   IF Obj_Valid(self) THEN self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       INTERACTION::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain INTERACTION properties. Be sure
;       you ALWAYS call the superclass GETPROPERTY method if you have extra
;       keywords!
;
; SYNTAX:
;
;       theObject -> GetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     COLOR:          The name of the color of the region of interest.
;
;     DRAWIMAGE:      Set this keyword to a named variable to receive a copy of the
;                     draw widget's current contents as an image. May by undefined
;                     if SetDisplay hasn't been called, or if RestoreDisplay has been called.
;
;     DRAWWIDGET:     The draw widget object for whom you are taking over events.
;
;     PIXMAPIMAGE:    Set this keyword to a named variable to receive a copy of the
;                     draw widget's original contents (at the moment SetDisplay was called)
;                     as an image. May by undefined if SetDisplay hasn't been called, or if
;                     RestoreDisplay has been called.
;
;     MODE:           The current "mode" of the interaction.
;
;     SELECTEDOBJECT: The currenly selected object.
;
;     STATUSBAR:      A reference to the current statusbar object.
;
;     XPT:            The X location of the click interaction.
;
;     YPT:            The Y location of the click interaction.
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO Interaction::GetProperty, $
   COLOR=color, $
   DRAWIMAGE=drawimage, $
   DRAWWIDGET=drawObject, $
   PIXMAPIMAGE=pixmapimage, $
   MODE=mode, $
   SELECTEDOBJECT=selectedObject, $
   STATUSBAR=statusbar, $
   XPT=xpt, $
   YPT=ypt, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(color) THEN color = self._roi_color
   IF Arg_Present(drawobject) THEN drawObject = self._drawID

   IF Arg_Present(drawimage) THEN BEGIN
      IF Obj_Valid(self._drawID) THEN BEGIN
         self._drawID -> SetWindow
         drawimage = TVRead()
      ENDIF
   ENDIF

   IF Arg_Present(pixmapimage) THEN BEGIN
      IF Obj_Valid(self._drawID_pixmap) THEN BEGIN
         self._drawID_pixmap -> Copy, Image=pixmapimage
      ENDIF
   ENDIF

   IF Arg_Present(mode) THEN mode = self._mode
   IF Arg_Present(selectedObject) THEN selectedObject = self._selectedObject
   IF Arg_Present(statusbar) THEN statusbar = self._statusbar
   IF Arg_Present(xpt) THEN xpt = self._click_x
   IF Arg_Present(ypt) THEN ypt = self._click_y

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATATOM::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       INTERACTION::NEEDDIALOG
;
; PURPOSE:
;
;       This method is a utility routine to simply determine if a dialog is required on not.
;       If not, an event is generated and sent to the user of the interaction.
;
; SYNTAX:
;
;       theObject -> NeedDialog, event
;
; ARGUMENTS:
;
;       event:      The event structure currently in effect.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO Interaction::NeedDialog, event

   @cat_pro_error_handler

  ; Need a dialog?
  IF (self._ask_on_up) THEN BEGIN

      ; Call context display menu.
      Widget_DisplayContextMenu, event.id -> GetID(), event.x+20 , event.y+20, self._contextMenu->GetID()

  ENDIF ELSE BEGIN

     ; Send an event to the real draw widget event handlers.
     ; Create an event structure for this interaction..
     event.ID = self._drawID
     event.HANDLER = Obj_New()
     event.EVENT_NAME='SELECTINTERACTION_EVENT'
     event.NAME = self._name

     ; Let the selectableObject add whatever is appropriate to the event structure.
     thisEvent = self._selectedObject -> AddToEventStructure(event)

     ; Send the event.
     self -> SendEvent, thisEvent

  ENDELSE

END


;*****************************************************************************************************
;+
; NAME:
;       INTERACTION::REFRESHPIXMAP
;
; PURPOSE:
;
;       This method refreshes the pixmap (the original display might have changed in some way) and
;       (optionally) calls the DRAW method.
;
; SYNTAX:
;
;       theObject -> RefreshPixmap
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DRAW:       If this keyword is set, the DRAW method is called..
;-
;*****************************************************************************************************
PRO Interaction::RefreshPixmap, DRAW=draw

   @cat_pro_error_handler

   ; Make sure you can do something here.
   IF Obj_Valid(self._drawID) EQ 0 THEN RETURN

   ; Make sure there is a pixmap to refresh.
   IF Obj_Valid(self._drawID_pixmap) EQ 0 THEN RETURN

   self._drawID_pixmap -> SetWindow
   self._drawID -> Copy

   IF Keyword_Set(draw) THEN self -> Draw

END



;*****************************************************************************************************
;+
; NAME:
;       INTERACTION::RESTOREDISPLAY
;
; PURPOSE:
;
;       This method restores the draw widget to its former state.
;
; SYNTAX:
;
;       theObject -> RestoreDisplay
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
PRO Interaction::RestoreDisplay

   @cat_pro_error_handler

   ; Make sure you can do something here.
   IF Obj_Valid(self._drawID) EQ 0 THEN RETURN

   ; Restore the draw widget to its previous condition.
   tevents = self._drawID_events[0]
   bevents = self._drawID_events[1]
   eevents = self._drawID_events[2]
   kevents = self._drawID_events[3]
   mevents = self._drawID_events[4]
   vevents = self._drawID_events[5]
   cevents = self._drawID_events[6]
   self._drawID -> SetProperty, Tracking_Events=tevents, Button_Events=bevents, $
      Expose_Events=eevents, Keyboard_Events=kevents, Motion_Events=mevents, $
      Viewport_Events=vevents, Context_Events=cevents, $
      Exclusive_Event_Object=self._drawID_excl_event_obj

   ; Restore the draw widgets appearance?
   CASE self._nopicture OF
      0: BEGIN
            self._drawID -> SetWindow
            IF Obj_Valid(self._drawID_pixmap) THEN self._drawID_pixmap -> Copy
         END
      1:
   ENDCASE

   ; Clean up the pixmap.
   Obj_Destroy, self._drawID_pixmap

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       Interaction::MESSAGEHANDLER
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
PRO Interaction::MessageHandler, title, SENDER=sender, DATA=data

   ; Initialise the error handler
   @cat_pro_error_handler

   CASE title OF

      'DRAWWIDGETREALIZED': self -> SetDisplay

      ELSE: self -> CATATOM::MessageHandler, title, SENDER=sender, DATA=data


   ENDCASE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       INTERACTION::SENDEVENT
;
; PURPOSE:
;
;       This method allows the user to send an event to the real draw widget event handler.
;
;
; SYNTAX:
;
;       theObject -> SetEvent
;
; ARGUMENTS:
;
;     event          The original event.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO Interaction::SendEvent, thisEvent

   @cat_pro_error_handler

   ; Check that there is at least one valid event object, otherwise swallow the event.
   IF Ptr_Valid(self._drawID_event_objects) THEN BEGIN

      ; The event objects to send the event to.
      eventObjs = *self._drawID_event_objects

      ; Are there any valid objects among the event objects?
      IF (MAX (OBJ_VALID (eventObjs)) NE 0) THEN BEGIN ; There are valid event object.

         ; Find out the target object for the event, get the event method from
         ; the target object, and send the event to this method, UNLESS this
         ; object has its own object method.
         FOR e = 0, N_ELEMENTS (eventObjs) - 1 DO $
         BEGIN
            IF OBJ_VALID (eventObjs [e]) THEN $
            BEGIN
               thisEvent.handler = eventObjs [e]
               self._drawID -> GetProperty, Event_Method=drawID_method
               eventObjs[e] -> CatAtom::GetProperty, Event_Method=event_method
               IF (drawID_method NE "") AND (e EQ 0) THEN $
                  thisMethod = drawID_method ELSE thisMethod = event_method
               Call_Method, thisMethod, eventObjs[e], thisEvent
             ENDIF
         ENDFOR

      ENDIF

   ENDIF

   IF Obj_Valid(self) THEN self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       INTERACTION::SETDISPLAY
;
; PURPOSE:
;
;       This method takes over event handling from the draw widget and
;       sets up the object so that everything can be restored to the way
;       it was when the interaction is finished.
;
; SYNTAX:
;
;       theObject -> SetDisplay
;
; ARGUMENTS:
;
;       drawWidget  ; The draw widget used in the interaction.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO Interaction::SetDisplay, drawWidget

   @cat_pro_error_handler

   IF N_Elements(drawWidget) NE 0 THEN self -> SetProperty, DrawWidget=drawWidget

   ; Is the draw widget realized? If not, register for a DRAWWIDGETREALIZED message.
   self._drawID -> GetProperty, Realized=realized
   IF realized EQ 0 THEN BEGIN
      self._drawID -> SetProperty, Notify_Realize=1
      self._drawID -> RegisterForMessage, self, 'DRAWWIDGETREALIZED'
      RETURN
   ENDIF

   ; Determine how the draw widget is set up:
   self._drawID -> GetProperty, Tracking_Events=tevents, Button_Events=bevents, $
      Expose_Events=eevents, Keyboard_Events=kevents, Motion_Events=mevents, $
      Viewport_Events=vevents, Context_Events=cevents, Exclusive_Event_Object=excl_event_obj, $
      XSize=xsize, YSize=ysize, Event_Objects=event_objects

   ; Store draw window information.
   self._drawID_events = [tevents, bevents, eevents, kevents, mevents, vevents, cevents]
   self._drawID_excl_event_obj = excl_event_obj

   ; Get the event objects.
   IF N_Elements(event_objects) NE 0 THEN BEGIN
      IF Ptr_Valid(self._drawID_event_objects) EQ 0 THEN self._drawID_event_objects = Ptr_New(event_objects) ELSE $
         *self._drawID_event_objects = event_objects
   ENDIF

   ; Copy the display window into the pixmap
   IF Obj_Valid(self._drawID_Pixmap) EQ 0 THEN BEGIN
      self._drawID -> GetProperty, XSize=xsize, YSize=ysize
      self._drawID_Pixmap = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)
   ENDIF
   self._drawID_Pixmap -> SetWindow
   self._drawID -> Copy

   ; Set up the window for your own purposes.
   self._drawID -> SetProperty, Tracking_Events=0, Button_Events=1, $
      Expose_Events=0, Keyboard_Events=0, Motion_Events=0, $
      Viewport_Events=0, Exclusive_Event_Object=self

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       INTERACTION::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the INTERACTION object's properties. Be sure
;       you ALWAYS call the superclass SETPROPERTY method if you have extra keywords!
;
;
; SYNTAX:
;
;       theObject -> SetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     DRAWWIDGET:     The draw widget object for whom you are taking over events.
;
;     NOPICRESTORE:   Set this keyword to not update the draw widget with original image
;                     when RestoreDisplay method is called.
;
;     COLOR:          The name of the color of the region of interest.
;
;     MODE:           The current "mode" of the interaction. By default, "DRAW".
;
;     SELECTEDOBJECT: The currenly selected object. Normally, this object will be moved or dragged
;                     if the mode is MOVE or DRAG, etc.
;
;     START_NOW:      If this keyword is set, the SetDisplay method is called immediately.
;
;     STATUSBAR:      A reference to a statusbar object. Messages can be sent from the interaction
;                     to the statusbar object, if supplied. The statusbar object is NOT destroyed by
;                     the interaction when the interaction is destroyed.
;
;     _EXTRA:         Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO Interaction::SetProperty, $
   DRAWWIDGET=drawObject, $
   NOPICRESTORE=nopicrestore, $
   COLOR=roi_color, $
   MODE=mode, $
   SELECTEDOBJECT=selectedObject, $
   START_NOW=start_now, $
   STATUSBAR=statusbar, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(drawObject) NE 0 THEN BEGIN
      self._drawID = drawobject
      Obj_Destroy, self._drawID_pixmap

      ; Create a copy of the draw widget window.
      self._drawID -> GetProperty, XSize=xsize, YSize=ysize
      self._drawID_Pixmap = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)
      self._drawID_Pixmap -> SetWindow
      self._drawID -> Copy
   ENDIF

   IF N_Elements(nopicrestore) NE 0 THEN self._noPicture = Keyword_Set(nopicrestore)
   IF N_Elements(roi_color) NE 0 THEN self._roi_color = roi_color

   IF N_Elements(mode) NE 0 THEN self._mode = mode
   IF N_Elements(selectedObject) NE 0 THEN self._selectedObject = selectedObject
   IF Keyword_Set(start_new) THEN self -> SetDisplay
   IF N_Elements(statusbar) NE 0 THEN self._statusbar = statusbar

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATATOM::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       INTERACTION::CLEANUP
;
; PURPOSE:
;
;       This is the INTERACTION object class destructor method.
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
PRO Interaction::CLEANUP

   @cat_pro_error_handler

   ; Destroy the pixmap.
   Obj_Destroy, self._drawID_pixmap
   Obj_Destroy, self._contextMenu

   ; Free the event object pointer. No need to destroy the objects, since we
   ; don't own them.
   Ptr_Free, self._drawID_event_objects

   ; Remove parent status from the draw widget.
   IF Obj_Valid(self._drawID) THEN self._drawID -> RemoveParent, self

   self -> CATATOM::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       INTERACTION::INIT
;
; PURPOSE:
;
;       This is the INTERACTION object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     drawObject:   The draw widget object that you will be taking over events from.
;
; KEYWORDS:
;
;     ASK_ON_UP:      Set this keyword if you wish the user to respond to a ACCEPT/CANCEL
;                     dialog when the box is finished drawing. If there is no dialog, it is
;                     as if an ACCEPT were recorded and a RUBBERBANDBOX_EVENT is sent to the
;                     appropriate event handler method.
;
;     COLOR:          The name of the color of the region of interest. By default, "Yellow".
;
;     NOPICRESTORE:   Normally, when the display is restored, the contents of the draw widget
;                     are restored to the state they were in before the interaction took place.
;                     However, if this keyword is set, the contents of the display window are
;                     left as they are at the end of the interaction.
;
;     MODE:           The current "mode" of the interaction. By default, "DRAW".
;
;     SELECTEDOBJECT: The currenly selected object. Normally, this object will be moved or dragged
;                     if the mode is MOVE or DRAG, etc.
;
;     START_NOW:      If this keyword is set, the SetDisplay method is called from within the INIT method.
;                     If the draw widget is unrealized at the time of the call, the interaction object
;                     will register for a DRAWWIDGETREALIZED message.
;
;     STATUSBAR:      A reference to a statusbar object. Messages can be sent from the interaction
;                     to the statusbar object, if supplied. The statusbar object is NOT destroyed by
;                     the interaction when the interaction is destroyed.
;
;     _EXTRA:         Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION Interaction::INIT, drawObject, $
   ASK_ON_UP=ask_on_up, $
   COLOR=roi_color, $
   DRAW_REALIZE=draw_realize, $
   NOPICRESTORE=nopicrestore, $
   MODE=mode, $
   SELECTEDOBJECT=selectedObject, $
   START_NOW=start_now, $
   STATUSBAR=statusbar, $
   _EXTRA=extraKeywords, $

   ; It is not possible to redirect event handling on an interaction. So attempts
   ; to do so must be caught and prevented.
   EVENT_METHOD=event_method, $
   EVENT_OBJECT=event_object

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   IF N_Elements(event_method) NE 0 THEN $
      Message, 'Interactions cannot be assigned an Event Method. Assign the method to the Draw Widget'
   IF N_Elements(event_object) NE 0 THEN $
      Message, 'Interactions cannot be assigned an Event Object. Assign the object to the Draw Widget'

   ok = self -> CATATOM::INIT (_EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0


   IF N_Elements(drawObject) EQ 0 THEN Message, 'A draw object reference is required as argument.'
   self._ask_on_up = Keyword_Set(ask_on_up)
   self._drawID = drawObject
   self -> AddParent, self._drawID
   self._click_x = -1
   self._click_y = -1

   self._contextMenu = Obj_New('ContextMenuBase', self._drawID, Column=1)
   button = Obj_New('ButtonWidget', self._contextMenu, Value='Accept', Name='ACCEPT', Event_Object=self)
   button = Obj_New('ButtonWidget', self._contextMenu, Value='Cancel', Name='CANCEL', Event_Object=self)

   ; Set up color
   IF N_Elements(roi_color) EQ 0 THEN roi_color = 'Yellow'
   self._roi_color = roi_color
   IF N_Elements(mode) EQ 0 THEN mode = 'SELECT'
   IF Obj_Valid(statusBar) THEN self._statusBar = statusBar

   self._noPicture = Keyword_Set(nopicrestore)
   self._mode = mode
   IF N_Elements(selectedObject) NE 0 THEN self._selectedObject = selectedObject

   ; Start this now?
   IF Keyword_Set(start_now) THEN self -> SetDisplay

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       INTERACTION CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the INTERACTION object.
;
;*****************************************************************************************************
PRO Interaction__DEFINE, class


   class = { INTERACTION, $
             _ask_on_up: 0B, $                     ; Flag for UP button dialog widget.
             _drawID: Obj_New(), $                 ; The draw widget whose events are being hijacked.
             _drawID_events: IntArr(7), $          ; Storage for the draw widget event types.
             _drawID_excl_event_obj: Obj_New(), $  ; The old exclusive event (if there is one).
             _drawID_pixmap: Obj_New(), $          ; A pixmap for storing the drawID picture.
             _drawID_event_objects: Ptr_New(), $   ; The event objects for the draw widget.
             _click_x: 0L, $                       ; The X location of button down event.
             _click_y: 0L, $                       ; The Y location of button down event.
             _contextmenu: Obj_New(), $            ; The context menu.
             _mode: "", $                          ; The "mode" of the interaction: eg., MOVE or DRAW.
             _noPicture: 0L, $                     ; A flag: Should picture be restored at end of interaction?
             _roi_color: "", $                     ; The color of the roi.
             _selectedObject: Obj_New(), $         ; A selectable interaction object that can be moved.
             _statusbar: Obj_New(), $              ; A statusbar object. Can be passed messages, etc.
             INHERITS CATATOM $
           }

END

