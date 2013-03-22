;*****************************************************************************************************
;+
; NAME:
;       SELECTINTERACTION__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement the most basic of interactions.
;       An "interaction" is an operation that takes over control of a draw widget and
;       allows the user to interact with the draw widget in some way. For example,
;       drawing ROI's on a draw widget is a good example of an interaction. This will
;       be the basic interaction object many other interactions will inherit.
;
;       The philosophy of an interaction is that there is an object to "draw" in the
;       window. Once drawn, the object can be selected, moved, rearranged, resized, etc.
;       Then, when finished the interaction can report results to the real event handler
;       for the draw widget.
;
;       Much of the event handling for an interaction occurs in the INTERACTION_EVENTS
;       method of the object being drawn or manipulated. These objects are SELECTABLEOBJECTS
;       (e.g. BOX, ARROW, TEXTLINE, etc.) and must be written in a particular way. This 
;       approach is, in my opinion, overly complicated at the moment, and new interaction
;       objects are difficult to write. I have given this subject a great deal of thought,
;       and have still not hit on the one simple, elegant idea. So, I leave this to you.
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
;       theObject = Obj_New("SELECTINTERACTION")
;
; SUPERCLASSES:
;
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; EVENT_STRUCTURE:
;
;       event = { ID:theObject, TOP:topObject, HANDLER:Obj_New(), EVENT_NAME='SELECTINTERACTION_EVENT', $
;                  NAME: self._name, ACTION:"", CURRENTMODE:"", ... }
;
;       If the interaction is in DRAW mode, an event is generated immediately upon an UP event.
;       If the interaction is in INSERT mode, the action is similar, unless the ASK_ON_UP keyword
;       is set. Then, the event is sent only when the user responds to the CANCEL or ACCEPT buttons.
;       In any case, the ACTION field is always set to "ACCEPT" unless the CANCEL button is selected,
;       and then it is set to "CANCEL". 
;
; CLASS_STRUCTURE:
;
;   class = { SELECTINTERACTION, $
;             _ask_on_up: 0B, $                     ; Flag for UP button dialog widget.
;             _color: "", $                         ; The color of the object drawn.
;             _coord_object: Obj_New(), $           ; A coordinate object.
;             _drawID: Obj_New(), $                 ; The draw widget whose events are being hijacked.
;             _drawID_events: IntArr(7), $          ; Storage for the draw widget event types.
;             _drawID_excl_event_obj: Obj_New(), $  ; The old exclusive event (if there is one).
;             _drawID_pixmap: Obj_New(), $          ; A pixmap for storing the drawID picture.
;             _drawID_event_objects: Ptr_New(), $   ; The event objects for the draw widget.
;             _contextmenu: Obj_New(), $            ; The context menu.
;             _linestyle: 0L,  $                    ; The linestyle of the object drawn.
;             _mode: "", $                          ; The "mode" of the interaction: eg., INSERT or DRAW.
;             _noPicture: 0L, $                     ; A flag: Should picture be restored at end of interaction?
;             _selectedObject: Obj_New(), $         ; A selectable interaction object that can be moved.
;             _statusbar: Obj_New(), $              ; A statusbar object. Can be passed messages, etc.
;             _sx: 0L, $                            ; The static X location of a selected item.
;             _sy: 0L, $                            ; The static Y location of a selected item.
;             _thick: 0L, $                         ; The thickness of the line of the object drawn.
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
;       SELECTINTERACTION::ADD
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
PRO SelectInteraction::Add, object, _EXTRA=extraKeywords

   @cat_pro_error_handler

   Message, 'An object cannot be added to an SELECTINTERACTION object.'

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       SELECTINTERACTION::DRAW
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
PRO SelectInteraction::Draw, _EXTRA=extra

   @cat_pro_error_handler

   self._drawID ->SetWindow
   self._selectedObject -> Draw
   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;        SELECTINTERACTION::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the SELECTINTERACTION object. It will be used
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
PRO SelectInteraction::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Is this a tracking event? If so, handle it here and RETURN.
   IF event.event_name EQ 'WIDGET_TRACKING' THEN BEGIN

      IF Obj_Valid(self._statusbar) THEN BEGIN
         IF event.enter THEN BEGIN
            event.id -> GetProperty, Helpline=theText
            IF N_Elements(theText) NE 0 THEN self._statusbar -> SetProperty, Text=theText
         ENDIF ELSE self._statusbar -> SetProperty, Text='SelectInteraction: Waiting...'
      ENDIF
      RETURN

   ENDIF ; of TRACKING event.

   ; Is this an ACCEPT event?
   IF StrUpCase(event.name) EQ 'ACCEPT' THEN BEGIN

        ; Send an event to the real draw widget event handlers.
        ; Create an event structure for this interaction.
        thisEvent = event
        thisEvent.ID = self._drawID
        thisEvent.HANDLER = Obj_New()
        thisEvent.EVENT_NAME='SELECTINTERACTION_EVENT'
        thisEvent.NAME = self._name

        ; Add the ACTION and CURRENTMODE fields to the event structure.
        thisEvent = Create_Struct(thisEvent, 'ACTION', 'ACCEPT', 'CURRENTMODE', 'INSERT')

        ; Let the selectableObject add whatever is appropriate to the event structure.
        thisEvent = self._selectedObject -> AddToEventStructure(thisEvent)

        ; Make a copy of the current interaction, so you can start over.
        self._selectedObject -> CreateNewObject, NewObject=newBox

        ; Destroy the old object.
        Obj_Destroy, self._selectedObject
        self._drawID_pixmap -> Refresh
        self._drawID -> SetWindow
        self._drawID_pixmap -> Copy
        self._selectedObject = newBox
        self -> SetProperty, Mode='INSERT'

        ; Send the event.
        self -> SendEvent, thisEvent

        ; Return from event handler.
        RETURN

    ENDIF ; of ACCEPT event.

   ; Is this an CANCEL event?
   IF StrUpCase(event.name) EQ 'CANCEL' THEN BEGIN

      ; Make a copy of the current interaction, so you can start over.
      self._selectedObject -> CreateNewObject, NewObject=newBox

      ; Destroy the old object.
      Obj_Destroy, self._selectedObject
      self._drawID_pixmap -> Refresh
      self._drawID -> SetWindow
      self._drawID_pixmap -> Copy
      self._selectedObject = newBox
      self -> SetProperty, Mode='INSERT'

      ; Send an event to the real draw widget event handlers.
      ; Create an event structure for this interaction.
      thisEvent = event
      thisEvent.ID = self._drawID
      thisEvent.HANDLER = Obj_New()
      thisEvent.EVENT_NAME='SELECTINTERACTION_EVENT'
      thisEvent.NAME = self._name

      ; Add the ACTION and CURRENTMODE fields to the event structure.
      thisEvent = Create_Struct(thisEvent, 'ACTION', 'CANCEL', 'CURRENTMODE', 'INSERT')

      ; Send the event.
      self -> SendEvent, thisEvent

      RETURN

   ENDIF ; OF CANCEL event.

   ; This must be some kind of widget draw event. What kind?
   eventTypes = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSED', 'CHARACTER', 'KEYPRESS']
   thisEvent = eventTypes[event.type]

   ; Everything depends on mode.
   CASE self._mode OF

      'SELECT': BEGIN

          CASE thisEvent OF

            'DOWN': BEGIN

               ; Did you click in a selectable object?
               objects = self._drawID -> SelectObjects(event.x, event.y, Count=count)

               ; Did you find a selectable object?
                IF count GT 0 THEN BEGIN

                   theObject = (Reverse(objects))[0]

                   ; Search for the first SELECTABLEOBJECT object you find.
                   IF Obj_Isa_Valid(theObject, 'SELECTABLEOBJECT') THEN BEGIN

                      ; Did you click the LEFT mouse button? Then we are moving the object.
                      IF event.press EQ 1 THEN BEGIN

                         ; Is this object the same as the selectedObject? If not, destroy
                         ; the selected object.
                         IF theObject NE self._selectedObject THEN Obj_Destroy, self._selectedObject

                         ; Pass the SELECT event on to its InteractionEvents handler.
                         theObject -> InteractionEvents, event, Interaction=self
                         self._selectedObject = theObject

                         ; New mode. Save the cursor location.
                         IF self._mode EQ 'SELECT' THEN self -> SetProperty, MODE = 'MOVE'
                         self._sx = event.x
                         self._sy = event.y

                        ; Motion events on. Make sure the pixmap is the same size
                         ; as the window. Could *create* pixmap here, but I think
                         ; this is faster.
                         self._drawID -> SetProperty, MOTION_EVENTS=1
                         self._drawID -> GetProperty, XSize=xsize, YSize=ysize
                         self._drawID_pixmap -> GetProperty, XSize=pxsize, YSize=pysize
                         IF (xsize NE pxsize) OR (ysize NE pysize) THEN BEGIN
                             self._drawID_pixmap -> SetProperty, XSize=xsize, YSize=ysize
                         ENDIF

                         ; Quick draw of pixmapwindow with this selected objects turned off. Want
                         ; this for quick re-draw during movement.
                         self._selectedObject -> SetProperty, Visible=0, /NoMessage, /NoRefresh
                         self._drawID_pixmap -> Refresh

                         ; Copy pixmap contents to window.
                         self._drawID -> SetWindow
                         self._drawID_pixmap -> Copy

                         ; Draw the selected object in the window.
                         self._drawID -> SetWindow
                         self._selectedObject -> SetProperty, Visible=1, /NoMessage, /NoRefresh
                         self._selectedObject -> Draw
                         self._selectedObject -> DrawSelectionBox

                      ENDIF

                      ; User clicked the RIGHT button in selectable object. Access the
                      ; selectable object's select panel.
                      IF event.press EQ 4 THEN BEGIN
                         self._selectedObject -> ControlPanel, Group_Leader=self._drawID
                      ENDIF

                    ENDIF ;


               ENDIF ELSE BEGIN ; Did NOT find a selectable object.

                  ; Clicked RIGHT BUTTTON outside a selectable object. Get the SelectInteraction
                  ; ControlPanel. Nothing will appear if the control panel already exists. For
                  ; example, it might already be embedded somewhere, as in the Catalyst application.
                  ; Otherwise, the SelectInteraction ControlPanel will pop up next to where the user has clicked.
                  IF event.press EQ 4 THEN BEGIN
                      tlb = CatGetTopObject(self._drawID)
                      tlb -> GetProperty, XOffset=xoff, YOffset=yoff
                      self -> ControlPanel, Group_Leader=self._drawID_pixmap, XOffset=xoff+event.x, YOffset=yoff+event.y
                  ENDIF

                  ; If there is a valid current selectabled object,
                  ; destroy it after making a substitute.
                  IF Obj_Valid(self._selectedObject) THEN BEGIN

                     ; Destroy the selected object you just inserted. But first, make
                     ; a copy.
                     self._selectedObject -> CreateNewObject, NewObject=newObject
                     Obj_Destroy, self._selectedObject
                     self._selectedObject = newObject

                     ; Redraw the window.
                     self._drawID_pixmap->Refresh

                     ; Copy window contents from pixmap to window.
                     self._drawID -> SetWindow
                     self._drawID_pixmap -> Copy

                     self._drawID -> SetProperty, Motion_Events=0, /Clear_Events
                     self -> SetProperty, Mode='INSERT'
                     self._selectedObject -> InteractionEvents, event, Interaction=self

                  ENDIF



               ENDELSE

               END

            ELSE:

          ENDCASE ; of SELECT DOWN

          END ; of Case SELECT

         'MOVE': BEGIN

            CASE thisEvent OF

              'DOWN': BEGIN

                    IF Obj_Valid(self._selectedObject) THEN BEGIN
                       self._sx = event.x
                       self._sy = event.y
                       self._drawID -> SetProperty, MOTION_EVENTS=1
                    ENDIF

                 END

              'UP': BEGIN

                 self._drawID -> SetProperty, Motion_Events=0, /Clear_Events
                 self -> SetProperty, Mode='FINISHED_MOVE'
                 self._drawID_pixmap -> Refresh
                 self._drawID -> SetWindow
                 self._selectedObject -> CopyParameters, self._drawID, Destination=d, Extent=e
                 self._drawID_pixmap -> Copy, Destination=d, Extent=e, Origin=d
                 self._selectedObject -> DrawSelectionBox
                 END

              'MOTION': BEGIN

                    IF Obj_Valid(self._selectedObject) THEN BEGIN
                       deltaX = event.x - self._sx
                       deltaY = event.y - self._sy
                       self._drawID -> SetWindow
                       self._selectedObject -> CopyParameters, self._drawID, Destination=d, Extent=e
                       self._drawID_pixmap -> Copy, Destination=d, Extent=e, Origin=d
                       self._selectedObject -> Move, deltaX, deltaY, /NoDraw
                       self._selectedObject -> Draw
                       self._selectedObject -> DrawSelectionBox
                       self._sx = event.x
                       self._sy = event.y
                    ENDIF

                 END

              ELSE:

            ENDCASE ; of thisEvent in MOVE

         END ; of case MOVE

      ELSE: self._selectedObject -> InteractionEvents, event, Interaction=self

   ENDCASE


   ; At this point, the particular interaction event has been processed. Either by
   ; the interaction, or by the selectable object. What we are concerned with now is
   ; whether the interaction has "finished", which normally would indicate an UP event
   ; has occurred. The convention is to indicate an UP event with the mode "FINISHED_MODE",
   ; where "MODE" is the mode name, for example, "FINISHED_INSERT".

   ; Find out the current mode and see if you can find the letters "FINISHED" in the first
   ; eight characters.
   self -> GetProperty, Mode=currentMode
   IF StrMid(StrUpCase(currentMode), 0, 8) EQ 'FINISHED' THEN BEGIN

      ; There are a variety of MODES that can be finished. We handle them here.

      CASE StrUpCase(currentMode) OF

         'FINISHED_INSERT': BEGIN

            ; Find the newly inserted object and make it the selected object.
            ; Turn the mode to SELECT so that interations with the object can take place.
            self._selectedObject -> GetProperty, InsertedObject=insertedObject
            IF Obj_Valid(insertedObject) THEN BEGIN
               self -> SetProperty, SelectedObject=insertedObject
               self._selectedObject -> DrawSelectionBox
               self -> NeedDialog, event
            ENDIF
            self -> SetProperty, Mode='SELECT'
            END ; of FINISHED_INSERT ;------------------------------------------

         'FINISHED_DRAW': BEGIN


            ; Send an event to the real draw widget event handlers.
            ; Create an event structure for this interaction..
            event.ID = self._drawID
            event.HANDLER = Obj_New()
            event.EVENT_NAME='SELECTINTERACTION_EVENT'
            event.NAME = self._name

            ; Let the selectableObject add whatever is appropriate to the event structure.
            thisEvent = self._selectedObject -> AddToEventStructure(event)

            ; Add the ACTION and CURRENTMODE fields to the event structure.
            thisEvent = Create_Struct(thisEvent, 'ACTION', 'ACCEPT', 'CURRENTMODE', 'DRAW')

            ; Set the mode back to DRAW for another go.
            self -> SetProperty, Mode='DRAW'

            ; Send the event.
            self -> SendEvent, thisEvent

            ; Re-initialize the object.
            IF Obj_Valid(self._selectedObject) THEN self._selectedObject -> Initialize


            END ; of FINISHED_DRAW ---------------------------------------------

         'FINISHED_MOVE': BEGIN

            self -> NeedDialog, event
            self -> SetProperty, Mode='SELECT'

            END ; of FINISHED_MOVE ---------------------------------------------

         ELSE: BEGIN

            self -> NeedDialog, event
            self -> SetProperty, Mode='SELECT'

            END ; of ELSE ------------------------------------------------------


      ENDCASE


   ENDIF

   ; Report completion
   IF Obj_Valid(self) THEN self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       SELECTINTERACTION::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain SELECTINTERACTION properties. Be sure
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
;     COORD_OBJECT:   A coordinate object for establishing a data coordinate system.
;
;     DRAWIMAGE:      Set this keyword to a named variable to receive a copy of the
;                     draw widget's current contents as an image. May by undefined
;                     if SetDisplay hasn't been called, or if RestoreDisplay has been called.
;
;     DRAWWIDGET:     The draw widget object for whom you are taking over events.
;
;     LINESTYLE:      The linestyle index.
;
;     PIXMAPOBJECT:   Set this keyword to a named variable to receive the pixmap object associated with the draw widget.
;
;     MODE:           The current "mode" of the interaction.
;
;     SELECTEDOBJECT: The currenly selected object.
;
;     STATUSBAR:      A reference to the current statusbar object.
;
;     THICK:          The line thickness index.
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO SelectInteraction::GetProperty, $
   COLOR=color, $
   COORD_OBJECT=coord_object, $
   DRAWIMAGE=drawimage, $
   DRAWWIDGET=drawObject, $
   LINESTYLE=linestyle, $
   MODE=mode, $
   PIXMAP=pixmapObject, $
   SELECTEDOBJECT=selectedObject, $
   STATUSBAR=statusbar, $
   THICK=thick, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(color) THEN color = self._color
   IF Arg_Present(coord_object) THEN coord_object = self._coord_object
   IF Arg_Present(drawobject) THEN drawObject = self._drawID
   IF Arg_Present(pixmapObject) THEN pixmapObject = self._drawID_pixmap
   IF Arg_Present(drawimage) THEN BEGIN
      IF Obj_Valid(self._drawID) THEN BEGIN
         self._drawID -> SetWindow
         drawimage = TVRead()
      ENDIF
   ENDIF

   IF Arg_Present(linestyle) THEN linestyle = self._linestyle
   IF Arg_Present(mode) THEN mode = self._mode
   IF Arg_Present(selectedObject) THEN selectedObject = self._selectedObject
   IF Arg_Present(statusbar) THEN statusbar = self._statusbar
   IF Arg_Present(thick) THEN thick = self._thick

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATATOM::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTINTERACTION::NEEDDIALOG
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
PRO SelectInteraction::NeedDialog, event

   @cat_pro_error_handler

  ; Need a dialog?
  IF (self._ask_on_up) THEN BEGIN

      ; Call context display menu.
      self._selectedObject -> GetProperty, BOUNDARY_BOX=box
      c = Convert_Coord(box, /NORMAL, /TO_DEVICE)
      max_x = Max(c[0,*])
      max_y = Max(c[1,*], Min=min_y)

      Empty ; Context menu can appear before graphics pipeline is completely drawn.
      Widget_DisplayContextMenu, event.id -> GetID(), max_x+20 , min_y + ((max_y-min_y)/2) +20, self._contextMenu->GetID()

  ENDIF ELSE BEGIN

     ; Send an event to the real draw widget event handlers.
     ; Create an event structure for this interaction..
     event.ID = self._drawID
     event.HANDLER = Obj_New()
     event.EVENT_NAME='SELECTINTERACTION_EVENT'
     event.NAME = self._name

     ; Add the ACTION and CURRENTMODE fields to the event structure.
     thisEvent = Create_Struct(event, 'ACTION', 'ACCEPT', 'CURRENTMODE', 'DRAW')

     ; Let the selectableObject add whatever is appropriate to the event structure.
     thisEvent = self._selectedObject -> AddToEventStructure(thisEvent)

     ; Send the event.
     self -> SendEvent, thisEvent

  ENDELSE

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTINTERACTION::REFRESHPIXMAP
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
PRO SelectInteraction::RefreshPixmap, DRAW=draw

   @cat_pro_error_handler

   ; Make sure you can do something here.
   IF Obj_Valid(self._drawID) EQ 0 THEN RETURN

   ; Make sure there is a pixmap to refresh.
   IF Obj_Valid(self._drawID_pixmap) EQ 0 THEN RETURN

   self._drawID_pixmap -> SetWindow
   self._drawID -> Copy
   self._drawID_Pixmap -> SetProperty, Ready=1

   IF Keyword_Set(draw) THEN self -> Draw

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTINTERACTION::RESTOREDISPLAY
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
PRO SelectInteraction::RestoreDisplay

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

   ; Unregister for DRAWWIDGET_DRAW messages.
   self._drawID -> RegisterForMessage, self, 'DRAWWIDGET_DRAW', /UNREGISTER

   ; Clean up the pixmap.
   Obj_Destroy, self._drawID_pixmap

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       SelectInteraction::MESSAGEHANDLER
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
PRO SelectInteraction::MessageHandler, title, SENDER=sender, DATA=data

   ; Initialise the error handler
   @cat_pro_error_handler

   CASE title OF

      ; It might be the interaction was initialized before the draw widget was on
      ; the display. If this is the case, and the START_NOW keyword was used, the
      ; interaction must wait for the draw widget to be realized before it can take
      ; over event handling. It does that here.
      'DRAWWIDGETREALIZED': self -> SetDisplay

      ; If the draw widget associated with this pixmap gets drawn by someone else
      ; (perhaps a colortool object), then we need to grab another copy of the background
      ; and refresh ourselves for re-drawing.
      'DRAWWIDGET_DRAW': BEGIN

          IF N_Elements(data) NE 0 THEN BEGIN
            requester = data
            IF Obj_Class(requester) EQ 'SELECTABLEOBJECT' THEN BEGIN

            ENDIF

          ENDIF ELSE BEGIN

            self._drawID -> Copy, Image=newImage
            image = self._drawID_Pixmap -> Get('Background_Image')
            image -> SetProperty, Image=newImage

          ENDELSE
          END

      ELSE: self -> CATATOM::MessageHandler, title, SENDER=sender, DATA=data


   ENDCASE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTINTERACTION::SENDEVENT
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
PRO SelectInteraction::SendEvent, thisEvent

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
;       SELECTINTERACTION::SETDISPLAY
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
PRO SelectInteraction::SetDisplay, drawWidget

   @cat_pro_error_handler

   IF N_Elements(drawWidget) NE 0 THEN self -> SetProperty, DrawWidget=drawWidget

   ; If the draw widget isn't here, issue an error message.
   IF ~Obj_Valid(self._drawID) THEN Message, 'No valid draw widget object. Please establish before setting display.'

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
      IF Ptr_Valid(self._drawID_event_objects) EQ 0 THEN $
         self._drawID_event_objects = Ptr_New(event_objects) ELSE $
         *self._drawID_event_objects = event_objects
   ENDIF

   ; Copy the display window into the pixmap
   IF Obj_Valid(self._drawID_Pixmap) EQ 0 THEN BEGIN
      self._drawID -> GetProperty, XSize=xsize, YSize=ysize
      self._drawID_Pixmap = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)
   ENDIF
   self._drawID_Pixmap -> SetWindow

   ; Make the pixmap the refresh buffer for the draw widget.
   self._drawID -> SetProperty, RefreshBuffer=self._drawID_Pixmap

   ; Register interest in getting DRAWWIDGET_DRAW messages.
   self._drawID -> RegisterForMessage, self, 'DRAWWIDGET_DRAW'

   ; Copy the current background into the pixmap.
   self._drawID -> Copy, Image=drawWindowImage
   self._drawID_Pixmap -> Add, Obj_New('CatImage', drawWindowImage, Name='Background_Image')
   self._drawID_Pixmap -> Refresh

   ; Set up the window for your own purposes.
   self._drawID -> SetProperty, Tracking_Events=0, Button_Events=1, $
      Expose_Events=0, Keyboard_Events=0, Motion_Events=0, $
      Viewport_Events=0, Exclusive_Event_Object=self

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       SELECTINTERACTION::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the SELECTINTERACTION object's properties. Be sure
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
;     NOPICRESTORE:   Set this keyword to not update the draw widget with the original background image
;                     when RestoreDisplay method is called.
;
;     COLOR:          The name of the color of the region of interest. By default, "NONE". If
;                     this keyword is set to "NONE", the color of the SELECTEDOBJECT will be
;                     used instead.
;
;     COORD_OBJECT:   A coordinate object for establishing a data coordinate system. Note that
;                     changing the coordinate object here will not have any effect on the current
;                     selectedObject. The selectedObject will have to be replace in a second call,
;                     for this coordinate system to be assigned.
;
;     LINESTYLE:      The linestyle index of the region of interest. By default, "-1". If
;                     this keyword is set to "-1", the linestyle of the SELECTEDOBJECT will be
;                     used instead.
;
;     MODE:           The current "mode" of the interaction. By default, "INSERT".
;
;     SELECTEDOBJECT: The currenly selected object. In other words, the object the interaction
;                     manipulates. This is a BOX object by default.
;
;     START_NOW:      If this keyword is set, the SetDisplay method is called immediately.
;
;     STATUSBAR:      A reference to a statusbar object. Messages can be sent from the interaction
;                     to the statusbar object, if supplied. The statusbar object is NOT destroyed by
;                     the interaction when the interaction is destroyed.
;
;     THICK:          The thickness index of the region of interest. By default, "-1". If
;                     this keyword is set to "-1", the thickness index of the SELECTEDOBJECT will be
;                     used instead.
;
;     _EXTRA:         Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO SelectInteraction::SetProperty, $
   COLOR=color, $
   COORD_OBJECT=coord_object, $
   DRAWWIDGET=drawObject, $
   LINESTYLE=linestyle, $
   MODE=mode, $
   NOPICRESTORE=nopicrestore, $
   SELECTEDOBJECT=selectedObject, $
   START_NOW=start_now, $
   STATUSBAR=statusbar, $
   THICK=thick, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(drawObject) NE 0 THEN BEGIN
      IF Obj_Valid(self._drawID) THEN self -> RemoveParent, self._drawID
      self._drawID = drawobject
      Obj_Destroy, self._drawID_pixmap

      IF ~Obj_Valid(self._contextMenu) THEN BEGIN
         self._contextMenu = Obj_New('ContextMenuBase', self._drawID, Column=1)
         button = Obj_New('ButtonWidget', self._contextMenu, Value='Accept', Name='ACCEPT', Event_Object=self)
         button = Obj_New('ButtonWidget', self._contextMenu, Value='Cancel', Name='CANCEL', Event_Object=self)
      ENDIF

      self -> AddParent, self._drawID

      ; Create a copy of the draw widget window.
      self._drawID -> GetProperty, XSize=xsize, YSize=ysize
      self._drawID_Pixmap = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)
      self._drawID_Pixmap -> SetWindow
      self._drawID -> Copy
      self._drawID_Pixmap -> SetProperty, Ready=1

   ENDIF

   IF N_Elements(nopicrestore) NE 0 THEN self._noPicture = Keyword_Set(nopicrestore)
   IF N_Elements(color) NE 0 THEN self._color = color
   IF N_Elements(coord_object) NE 0 THEN BEGIN
      self._coord_object  -> RemoveParent, self
      self._coord_object = coord_object
   ENDIF
   IF N_Elements(linestyle) NE 0 THEN self._linestyle = linestyle
   IF N_Elements(mode) NE 0 THEN self._mode = mode
   IF N_Elements(selectedObject) NE 0 THEN BEGIN

      ; Remove interest in current selectedObject.
      IF Obj_Valid(self._selectedObject) THEN Obj_Destroy, self._selectedObject

      ; Register interest in this selectedObject.
      selectedObject -> AddParent, self
      self._selectedObject = selectedObject

      ; Are there global properties set?
      IF self._color NE "NONE" THEN self._selectedObject -> SetProperty, Color=self._color
      IF self._linestyle NE -1 THEN self._selectedObject -> SetProperty, Linestyle=self._linestyle
      IF self._thick NE -1 THEN self._selectedObject -> SetProperty, Thick=self._thick

   ENDIF
   IF N_Elements(statusbar) NE 0 THEN self._statusbar = statusbar
   IF N_Elements(thick) NE 0 THEN self._thick = thick

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATATOM::SetProperty, _EXTRA=extraKeywords


   ; Does the selectedObject have a coordinate system? If not, pass along
   ; this one from the INTERACTION.
   self._selectedObject -> GetProperty, Coord_Object=so_coord_object
   IF N_Elements(so_coord_object) EQ 0 THEN self._selectedObject -> SetProperty, Coord_Object=self._coord_object


   IF Keyword_Set(start_now) THEN self -> SetDisplay

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTINTERACTION::CLEANUP
;
; PURPOSE:
;
;       This is the SELECTINTERACTION object class destructor method.
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
PRO SelectInteraction::CLEANUP

   @cat_pro_error_handler

   ; Destroy the pixmap.
   Obj_Destroy, self._drawID_pixmap
   Obj_Destroy, self._contextMenu

   ; Free the event object pointer. No need to destroy the objects, since we
   ; don't own them.
   Ptr_Free, self._drawID_event_objects

   ; Destroy the selected object.
    IF Obj_Valid(self._selectedObject) THEN Obj_Destroy, self._selectedObject

   ; Remove parent status from the draw widget.
   IF Obj_Valid(self._drawID) THEN self._drawID -> RemoveParent, self

   ; Remove parent status from the coordinate object.
    IF Obj_Valid(self._coord_object) THEN self._coord_object -> RemoveParent, self

   self -> CATATOM::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTINTERACTION::INIT
;
; PURPOSE:
;
;       This is the SELECTINTERACTION object class initialization method
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
;                     dialog when the selectable object reports an UP event.
;
;     COLOR:          The name of the color of the selectable object. By default, "NONE". If
;                     this keyword is set to "NONE", the color of the SELECTEDOBJECT will be
;                     used instead.
;
;     COORD_OBJECT:   A coordinate object for establishing a data coordinate system. If not
;                     provided, the coordinate object of the draw widget is obtained. If this
;                     is unavailable, a normalized coordinate system is used. The COORD_OBJECT
;                     is passed to the SelectableObject as its coordinate system.
;
;     LINESTYLE:      The linestyle index of the region of interest. By default, "-1". If
;                     this keyword is set to "-1", the linestyle of the SELECTEDOBJECT will be
;                     used instead.
;
;     NOPICRESTORE:   Normally, when the display is restored, the contents of the draw widget
;                     are restored to the state they were in before the interaction took place.
;                     However, if this keyword is set, the contents of the display window are
;                     left as they are at the end of the interaction.
;
;     MODE:           The current "mode" of the interaction. By default, "INSERT". All draw widget
;                     events are passed along to the selected object to process in its INTERACTION_EVENTS
;                     method.
;
;     SELECTEDOBJECT: The currenly selected or "default" object. It must be a subclassed SELECTABLEOBJECT,
;                     such as a BOX or ELLIPSE object. This object will be destroyed when the interaction
;                     is destroyed.
;
;     START_NOW:      If this keyword is set, the SetDisplay method is called from within the INIT method.
;                     If the draw widget is unrealized at the time of the call, the interaction object
;                     will register for a DRAWWIDGETREALIZED message.
;
;     STATUSBAR:      A reference to a statusbar object. Messages can be sent from the interaction
;                     to the statusbar object, if supplied. The statusbar object is NOT destroyed by
;                     the interaction when the interaction is destroyed.
;
;     THICK:          The thickness index of the region of interest. By default, "-1". If
;                     this keyword is set to "-1", the thickness index of the SELECTEDOBJECT will be
;                     used instead.
;
;     _EXTRA:         Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION SelectInteraction::INIT, drawObject, $
   ASK_ON_UP=ask_on_up, $
   COLOR=color, $
   COORD_OBJECT=coord_object, $
   DRAW_REALIZE=draw_realize, $
   LINESTYLE=linestyle, $
   MODE=mode, $
   NOPICRESTORE=nopicrestore, $
   SELECTEDOBJECT=selectedObject, $
   START_NOW=start_now, $
   STATUSBAR=statusbar, $
   THICK=thick, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   ok = self -> CATATOM::INIT (_EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; If you have been provided a drawObject, then register interest and create a context menu (which
   ; may or may not be used).
   IF N_Elements(drawObject) NE 0 THEN BEGIN
      self._drawID = drawObject
      self._contextMenu = Obj_New('ContextMenuBase', self._drawID, Column=1)
      button = Obj_New('ButtonWidget', self._contextMenu, Value='Accept', Name='ACCEPT', Event_Object=self)
      button = Obj_New('ButtonWidget', self._contextMenu, Value='Cancel', Name='CANCEL', Event_Object=self)
      self -> AddParent, self._drawID
   ENDIF

   ; Set flags.
   self._ask_on_up = Keyword_Set(ask_on_up)

   ; Find a coordinate object for this interaction. If you can't find one, create a normalized one.
   IF Obj_Valid(coord_object) EQ 0 THEN BEGIN
      drawObject -> GetProperty, Coord_Object=coord_object
      IF Obj_Valid(coord_object) EQ 0 THEN BEGIN
         coord_object = Obj_New('CatCoord', Position=[0,0,1,1], XRange=[0,1], YRange=[0,1])
      ENDIF
   ENDIF
   coord_object -> AddParent, self
   self._coord_object = coord_object

   ; Check other keyword parameters. Define defaults.
   IF N_Elements(color) EQ 0 THEN color = 'NONE'
   IF N_Elements(linestyle) EQ 0 THEN linestyle = -1
   IF N_Elements(thick) EQ 0 THEN thick = -1
   IF N_Elements(mode) EQ 0 THEN mode = 'INSERT'
   IF Obj_Valid(statusBar) THEN self._statusBar = statusBar

   ; Populate the object.
   self._color = color
   self._linestyle = linestyle
   self._thick = thick
   self._noPicture = Keyword_Set(nopicrestore)
   self._mode = mode
   IF N_Elements(selectedObject) EQ 0 THEN BEGIN
      self._selectedObject = Obj_New('Box')
   ENDIF ELSE BEGIN
      self._selectedObject = selectedObject
   ENDELSE
   self._selectedObject -> SetProperty, Coord_Object=self._coord_object

   ; Register interest in selectedObject.
   self._selectedObject -> AddParent, self

   ; Are there global properties set?
   IF self._color NE "NONE" THEN self._selectedObject -> SetProperty, Color=self._color
   IF self._linestyle NE -1 THEN self._selectedObject -> SetProperty, Linestyle=self._linestyle
   IF self._thick NE -1 THEN self._selectedObject -> SetProperty, Thick=self._thick

   ; Start this now?
   IF Keyword_Set(start_now) THEN self -> SetDisplay

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       SELECTINTERACTION CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the SELECTINTERACTION object.
;
;*****************************************************************************************************
PRO SelectInteraction__DEFINE, class


   class = { SELECTINTERACTION, $
             _ask_on_up: 0B, $                     ; Flag for UP button dialog widget.
             _color: "", $                         ; The color of the object drawn.
             _coord_object: Obj_New(), $           ; A coordinate object.
             _drawID: Obj_New(), $                 ; The draw widget whose events are being hijacked.
             _drawID_events: IntArr(7), $          ; Storage for the draw widget event types.
             _drawID_excl_event_obj: Obj_New(), $  ; The old exclusive event (if there is one).
             _drawID_pixmap: Obj_New(), $          ; A pixmap for storing the drawID picture.
             _drawID_event_objects: Ptr_New(), $   ; The event objects for the draw widget.
             _contextmenu: Obj_New(), $            ; The context menu.
             _linestyle: 0L,  $                    ; The linestyle of the object drawn.
             _mode: "", $                          ; The "mode" of the interaction: eg., INSERT or DRAW.
             _noPicture: 0L, $                     ; A flag: Should picture be restored at end of interaction?
             _selectedObject: Obj_New(), $         ; A selectable interaction object that can be moved.
             _statusbar: Obj_New(), $              ; A statusbar object. Can be passed messages, etc.
             _sx: 0L, $                            ; The static X location of a selected item.
             _sy: 0L, $                            ; The static Y location of a selected item.
             _thick: 0L, $                         ; The thickness of the line of the object drawn.
             INHERITS CATATOM $
           }

END

