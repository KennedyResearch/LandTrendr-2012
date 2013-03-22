;*****************************************************************************************************
;+
; NAME:
;       MEASUREINTERACTION__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide an interaction for creating
;       and manipulating TapeMeasure objects.
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
;       theObject = Obj_New("MEASUREINTERACTION")
;
; SUPERCLASSES:
;
;       INTERACTION
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { MEASUREINTERACTION, $
;             pixmap: Obj_New(), $             ; A pixmap for fast redraw.
;             mode: "", $                      ; The current mode. "SELECT", "WRITE", "MOVE", etc.
;             selectedObjects: Ptr_New(), $    ; The currently selected objects.
;             sx: 0L, $                        ; The static X location.
;             sy: 0L, $                        ; The static Y location.
;             tapeObject: Obj_New(), $         ; The default TapeMeasure object.
;             layerObject: Obj_New(), $        ; A layer object for holding selectable annotation objects.
;             INHERITS INTERACTION $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 9 August 2004.
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
;       MEASUREINTERACTION::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the object
;
; SYNTAX:
;
;       imageObject -> ControlPanel, baseObj
;
; ARGUMENTS:
;
;       baseObject:    The object reference of a base widget for this control to
;                      be added to. If not supplied, the control panel will be a
;                      self contained window.
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the "CONTROLPANEL::INIT" method.
;
;
;-
;*****************************************************************************************************
PRO MeasureInteraction::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel
   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, Name='MeasureInteraction::ControlPanel', $
      TITLE='Annotate', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK, XPad=0, YPad=0, /Exclusive)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN BEGIN
      tlb -> Draw
   ENDIF

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       MEASUREINTERACTION::DRAW
;
; PURPOSE:
;
;       This method draws the interaction in the display window.
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
PRO MeasureInteraction::Draw

   @cat_pro_error_handler

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;        MEASUREINTERACTION::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the MEASUREINTERACTION object.
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
PRO MeasureInteraction::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Is this a tracking event? If so, handle it here and RETURN.
   IF event.event_name EQ 'WIDGET_TRACKING' THEN BEGIN
      IF Obj_Valid(self._statusbar) THEN BEGIN
         IF event.enter THEN BEGIN
            event.id -> GetProperty, Helpline=theText
            self._statusbar -> SetProperty, Text=theText
         ENDIF ELSE self._statusbar -> SetProperty, $
            Text='[SHIFT-] LEFT click to SELECT/MOVE, RIGHT click for PROPERTIES/GROUP.'
      ENDIF
      RETURN
   ENDIF

   self._drawID -> SetWindow

   IF event.event_name EQ 'WIDGET_BUTTON' THEN BEGIN

      CASE event.name OF

         'CANCEL':

          'INSERT MODE': self -> SetProperty, Mode='INSERT'

          'SELECT MODE': self -> SetProperty, Mode='SELECT'

          'SELECT_OBJECTS': BEGIN
            self -> SetProperty, Mode='SELECT'
            END

          ELSE: Print, 'Unknown button event in MeasureInteraction::EventHandler.'

      ENDCASE

      RETURN ; from WIDGET_BUTTON events.

   ENDIF

   ; What kind of event is this? Only draw widget events should get this far.
   eventTypes = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSED', 'CHARACTER', 'KEYPRESS_CH', 'KEYPRESS_KEY']
   thisEvent = eventTypes[event.type]

   ; Everything depends on the current MODE.
   CASE self._mode OF

      'SELECT': BEGIN

          CASE thisEvent OF

            'DOWN': BEGIN

               ; Did you click in a selectable object?
               objects = self._drawID -> SelectObjects(event.x, event.y, Count=count)

               ; Did you find a selectable object?
                IF count GT 0 THEN BEGIN

                  ; Found a selectable object.
                  FOR j=0,count-1 DO BEGIN

                    theObject = (Reverse(objects))[j]

                    ; Search for the first SELECTABLEOBJECT object you find.
                    IF Obj_Isa_Valid(theObject, 'SELECTABLEOBJECT') THEN BEGIN

                       ; Did you click the LEFT mouse button? Then we are moving the object.
                       IF event.press EQ 1 THEN BEGIN

                           ; If we didn't use a modifier key, then find out if we have
                           ; a single object or whether this object is already in a group
                           ; of objects.
                          IF event.modifiers EQ 0 THEN BEGIN
                             IF Ptr_Valid(self.selectedObjects) THEN BEGIN
                                selectedObjects = *self.selectedObjects
                                i = Where(selectedObjects EQ theObject, cnt)
                                IF cnt EQ 0 THEN BEGIN
                                    *self.selectedObjects = theObject
                                    singleObjectFlag = 1
                                ENDIF ELSE BEGIN
                                    IF N_Elements(selectedObjects) GT 1 THEN BEGIN
                                       singleObjectFlag = 0
                                    ENDIF ELSE BEGIN
                                       singleObjectFlag = 1
                                    ENDELSE
                                ENDELSE
                             ENDIF ELSE BEGIN
                                    self.selectedObjects = Ptr_New(theObject)
                                    singleObjectFlag = 1
                             ENDELSE

                          ENDIF

                         ; If we used a SHIFT key, then we are adding to the selection.
                         IF event.modifiers EQ 1 THEN BEGIN
                            IF Ptr_Valid(self.selectedObjects) THEN BEGIN
                               index = Where(*self.selectedObjects EQ theObject, count)
                               IF count EQ 0 THEN (*self.selectedObjects) = [*self.selectedObjects,theObject]
                            ENDIF ELSE BEGIN
                               self.selectedObjects = Ptr_New(theObject)
                            ENDELSE
                         ENDIF

                          ; New mode. Save the cursor location.
                          IF self._mode EQ 'SELECT' THEN self._mode = 'MOVE'
                          self.sx = event.x
                          self.sy = event.y

                         ; Motion events on. Make sure the pixmap is the same size
                          ; as the window. Could *create* pixmap here, but I think
                          ; this is faster.
                          self._drawID -> SetProperty, MOTION_EVENTS=1
                          self._drawID -> GetProperty, XSize=xsize, YSize=ysize
                          self.pixmap -> GetProperty, XSize=pxsize, YSize=pysize
                          IF (xsize NE pxsize) OR (ysize NE pysize) THEN BEGIN
                              self.pixmap -> SetProperty, XSize=xsize, YSize=ysize
                          ENDIF

                          ; Quick draw of pixmapwindow with this selected objects turned off. Want
                          ; this for quick re-draw during movement.
                           FOR j=0,N_Elements(*self.selectedObjects)-1 DO $
                                 (*self.selectedObjects)[j] -> SetProperty, Visible=0, /NoMessage, /NoRefresh
                          self.pixmap -> Refresh

                          ; Copy pixmap contents to window.
                          self._drawID -> SetWindow
                          self.pixmap -> Copy

                          ; Draw the selected object in the window.
                          self._drawID -> SetWindow
                          FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                             (*self.selectedObjects)[j] -> SetProperty, Visible=1, /NoMessage, /NoRefresh
                             (*self.selectedObjects)[j] -> Draw
                             (*self.selectedObjects)[j] -> DrawSelectionBox
                          ENDFOR

                          ; Can only select one at a time.
                          BREAK

                       ENDIF

                       ; User clicked the RIGHT button in selectable object. Access the
                       ; selectable object's select panel.
                       IF event.press EQ 4 THEN BEGIN
                          IF Ptr_Valid(self.selectedObjects) THEN BEGIN
                             IF N_Elements(*self.selectedObjects) GT 1 THEN BEGIN
                                  self -> BuildMultiSelectMenu
                                  Widget_DisplayContextMenu, event.id -> GetID(), event.x+10, event.y-1, $
                                    self._contextMenu->GetID()
                             ENDIF ELSE BEGIN
                               IF Obj_Isa_Valid(theObject, 'SELECTABLEGROUP') THEN BEGIN
                                  Obj_Destroy, self._contextMenu
                                   self._contextMenu = Obj_New('ContextMenuBase', self._drawID, Column=1, Event_Object=self)
                                  button = Obj_New('ButtonWidget', self._contextMenu, Value='Ungroup', Name='UNGROUP')
                                  Widget_DisplayContextMenu, event.id -> GetID(), event.x+10, event.y-1, $
                                    self._contextMenu->GetID()
                               ENDIF ELSE BEGIN
                                    theObject -> SelectPanel, event.x, event.y, self._drawID
                                  BREAK ; Only one control panel.
                               ENDELSE
                             ENDELSE
                          ENDIF
                       ENDIF

                    ENDIF

                  ENDFOR

               ENDIF ELSE BEGIN

                  ; Clicked RIGHT BUTTTON outside a selectable object. Get the Annotation
                  ; ControlPanel. Nothing will appear if the control panel already exists. For
                  ; example, it might already be embedded somewhere, as in the Catalyst application.
                  ; Otherwise, the Annotation ControlPanel will pop up next to where the user has clicked.
                  IF event.press EQ 4 THEN BEGIN
                      tlb = CatGetTopObject(self._drawID)
                      tlb -> GetProperty, XOffset=xoff, YOffset=yoff
                      self -> ControlPanel, Group_Leader=self.pixmap, XOffset=xoff+event.x, YOffset=yoff+event.y, Row=1
                  ENDIF

                  ; If there is a valid current selectable object,
                  ; redraw to remove selection and remove selected object.
                  IF Ptr_Valid(self.selectedObjects) THEN BEGIN

                     ; Redraw the window.
                     self.pixmap->Refresh

                     ; Copy window contents from pixmap to window.
                     self._drawID -> SetWindow
                     self.pixmap -> Copy

                  ENDIF
                  Ptr_Free, self.selectedObjects

               ENDELSE

               END

            ELSE:

          ENDCASE ; of SELECT DOWN

          END ; of Case SELECT

      'MOVE': BEGIN

         CASE thisEvent OF

           'DOWN': BEGIN

                 IF Ptr_Valid(self.selectedObjects) THEN BEGIN
                    self.sx = event.x
                    self.sy = event.y
                    self._drawID -> SetProperty, MOTION_EVENTS=1
                 ENDIF

              END

           'UP': BEGIN

              self._drawID -> SetProperty, Motion_Events=self._drawID_Events[4], /Clear_Events
              self -> SetProperty, Mode='SELECT'
              self.pixmap -> Refresh
              self._drawID -> SetWindow
              ;self.pixmap -> Copy
              IF Ptr_Valid(self.selectedObjects) THEN BEGIN
                 FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                    (*self.selectedObjects)[j] -> CopyParameters, self._drawID, Destination=d, Extent=e
                    self.pixmap -> Copy, Destination=d, Extent=e, Origin=d
                    (*self.selectedObjects)[j] -> DrawSelectionBox
                 ENDFOR
              ENDIF
              END



           'MOTION': BEGIN

                 IF Ptr_Valid(self.selectedObjects) THEN BEGIN
                    deltaX = event.x - self.sx
                    deltaY = event.y - self.sy
                    self._drawID -> SetWindow
                    ;self.pixmap -> Copy
                    FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                       (*self.selectedObjects)[j] -> CopyParameters, self._drawID, Destination=d, Extent=e
                       self.pixmap -> Copy, Destination=d, Extent=e, Origin=d
                       (*self.selectedObjects)[j] -> Move, deltaX, deltaY, /NoDraw
                       (*self.selectedObjects)[j] -> Draw
                       (*self.selectedObjects)[j] -> DrawSelectionBox
                    ENDFOR
                    self.sx = event.x
                    self.sy = event.y
                 ENDIF

              END

           ELSE:

         ENDCASE ; of thisEvent in MOVE

         END ; of case MOVE

      ; If some mode comes in that is not handled here, then the event
      ; is passed on to the INTERACTIONEVENTS method of the current interaction
      ; object.
      ELSE: BEGIN
         IF Obj_Valid(self.tapeObject) THEN BEGIN
            self.tapeObject -> InteractionEvents, event, Interaction=self
         ENDIF ELSE BEGIN
            IF Ptr_Valid(self.selectedObjects) THEN BEGIN
               IF Obj_Valid((*self.selectedObjects)[0]) THEN $
                  (*self.selectedObjects)[0] -> InteractionEvents, event, Interaction=self
            ENDIF
         ENDELSE

         END

   ENDCASE ; of self._mode


   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       MEASUREINTERACTION::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain MEASUREINTERACTION properties. Be sure
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
;     TAPE_OBJECT:        The current TapeMeasure object.
;
;     LAYER:              The current annotation CATLAYER object.
;
;     MODE:               The current "mode" of the object.
;
;     PIXMAP:             The object reference to any pixmap widget that is created.
;
;     SELECTEDOBJECTS:    Any objects currently selected.
;
;     _REF_EXTRA:         Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO MeasureInteraction::GetProperty, $
    TAPE_OBJECT=tape_object, $
    LAYER=layer, $
    MODE=mode, $
    PIXMAP=pixmap, $
    SELECTEDOBJECTS=selectedObjects, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(mode) THEN mode = self._mode
   IF Arg_Present(pixmap) THEN pixmap = self.pixmap
   IF Arg_Present(tape_object) THEN tape_object = self.tapeObject
   IF Arg_Present(layer) THEN layer = self.layerObject
   IF Arg_Present(selectedObjects) THEN selectedObjects = self.selectedObjects
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> INTERACTION::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       MEASUREINTERACTION::RESTOREDISPLAY
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
PRO MeasureInteraction::RestoreDisplay

   @cat_pro_error_handler

   ; Call superclass method.
   self -> INTERACTION::RestoreDisplay

   ; Set the cursor back to its original shape.
   Device, /Cursor_Original

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       MEASUREINTERACTION::SETDISPLAY
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
;       None.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO MeasureInteraction::SetDisplay

   @cat_pro_error_handler

   self -> INTERACTION::SetDisplay

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       MEASUREINTERACTION::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the MEASUREINTERACTION object's properties. Be sure
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
;     The following keywords set default properties for new objects created in this interaction.
;
;     TAPE_OBJECT:        Set this keyword to an object reference to a TAPEMEASURE object.
;
;     LAYER:              A CATLAYER object for holding the annotation objects. The old annotation layer
;                         and everything it contains will be destroyed.
;
;     MODE:               The current object mode.
;
;     _EXTRA:             Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO MeasureInteraction::SetProperty, $
   DEF_TEXT_OBJECT=def_text_object, $
   TAPE_OBJECT=tape_object, $
   LAYER=layer, $
   MODE=mode, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(tape_object) NE 0 THEN BEGIN
      Obj_Destroy, self.tapeObject
      self.tapeObject = tape_object
   ENDIF

   IF N_Elements(layer) NE 0 THEN BEGIN


      IF Obj_Valid(self.layerObject) THEN BEGIN

         ; Remove the layer object from the draw widget and pixmap.
         self._drawID -> Remove, self.layerObject
         self.pixmap -> Remove, self.layerObject

         ; Removing all the parents of the layer object (probably destroying it, unless
         ; someone has registered specific interest by setting AUTO_DESTROY=0).
         self.layerObject -> GetProperty, Parents=parents
         FOR j=0,N_Elements(parents)-1 DO self.layerObject -> RemoveParent, parents[j]

      ENDIF

      ; Add the new layer. Register your interest in it.
      self.layerObject = layer
      self.layerObject -> AddParent, self

      ; Add the new layer to each of the default objects.
      self.tapeObject -> SetProperty, Layer=layer

      ; Add the layer object to the draw widget and pixmap.
      self._drawID -> Add, self.layerObject
      self.pixmap -> Add, self.layerObject

   ENDIF

   IF N_Elements(mode) NE 0 THEN self._mode = StrUpCase(mode)
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> INTERACTION::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       MEASUREINTERACTION::CLEANUP
;
; PURPOSE:
;
;       This is the MEASUREINTERACTION object class destructor method.
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
PRO MeasureInteraction::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self.selectedObjects
   Obj_Destroy, self.pixmap

   ; Remove interest in these objects.
   IF Obj_Valid(self.tapeObject) THEN self.tapeObject -> RemoveParent, self
   IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self
   self -> INTERACTION::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       MEASUREINTERACTION::INIT
;
; PURPOSE:
;
;       This is the MEASUREINTERACTION object class initialization method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     drawObject:          The draw widget object that you will be taking over events from.
;
; KEYWORDS:
;
;     COORD_OBJECT:        A coordinate object (CATCOORD) for establishing a coordinate system for the
;                          interaction objects. If a coordinate object isn't provided, an attempt is made
;                          to use the one provided by the drawObject. If still undefined, a default
;                          coordinate object with XRange=[0,1] and YRange=[0,1] is created.
;
;     TAPE_OBJECT:         Set this keyword to an object reference to a TAPEMEASURE object.
;
;     LAYER:               An optional CATLAYER object for holding the annotation objects. If a layer object is
;                          not provided, the annotations are placed directly in the drawObject container. Otherwise,
;                          the annotations are placed in the CATLAYER object, and the CATLAYER object is placed in
;                          the drawObject.
;
;     _EXTRA:              Any keywords appropriate for the superclass INIT method or TAPE_OBJECT INIT method.
;-
;*****************************************************************************************************
FUNCTION MeasureInteraction::INIT, drawObject, $
   COORD_OBJECT=coord_object, $
   TAPE_OBJECT=tape_object, $
   LAYER=layer, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   ok = self -> INTERACTION::INIT (drawObject, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; Create a pixmap for fast re-drawing. Set it as the RefreshBuffer of the drawObject.
   drawObject -> GetProperty, XSize=xsize, YSize=ysize, Initial_Color=theColor
   self.pixmap = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize, Background_Color=theColor, map=1)
   self.pixmap -> SetWindow
   drawObject -> Copy
   ;self.pixmap -> SetProperty, Ready=1
   ;drawObject -> SetProperty, RefreshBuffer=self.pixmap

   ; Copy all the objects in the draw widget into the pixmap.
   objects = drawObject -> Get(/All)
   FOR j=0,N_Elements(objects)-1 DO self.pixmap -> Add, objects[j]

   ; If a coordinate object wasn't provided, try to copy one from the
   ; draw widget. If this isn't available, create a normalized one.
   IF Obj_Valid(coord_object) EQ 0 THEN BEGIN
      IF Obj_Valid(drawObject) THEN drawObject -> GetProperty, Coord_Object=coord_object
      IF Obj_Valid(coord_object) EQ 0 THEN $
         coord_object = Obj_New('CatCoord', XRange=[0,1], YRange=[0,1], Position=[0,0,1,1], Name='Default Coords')
   ENDIF

   ; If you received an annotation layer object, regester your interest
   ; and add it to the drawObject and the pixmap.
   IF Obj_Isa_Valid(layer, "CatLayer") NE 0 THEN BEGIN
      self.layerObject = layer
      self.layerObject -> AddParent, self
      drawObject -> Add, layer
      self.pixmap -> Add, layer
   ENDIF

   ; Current mode.
   IF self._mode EQ "" THEN self._mode = 'SELECT'

   ; Check keywords.
   IF N_Elements(tape_object) EQ 0 THEN $
      tape_object = Obj_New('TAPEMEASURE', Coord_Object=coord_object, Visible=0) ELSE $
      tape_object -> SetProperty, Visible=0

   ; Register for RESIZE messages from the draw widget.
   drawObject -> RegisterForMessage, self.pixmap, 'RESIZEDRAWWIDGET'


   ; Load the object.
   self.tapeObject = tape_object

   ; Is there a layer object?
   IF Obj_Valid(self.layerObject) THEN self.tapeObject -> SetProperty, Layer=layer

   ; Register your interest in these objects.
   self.tapeObject -> AddParent, self

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       MEASUREINTERACTION CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the MEASUREINTERACTION object.
;
;*****************************************************************************************************
PRO MeasureInteraction__DEFINE, class

   class = { MEASUREINTERACTION, $
             pixmap: Obj_New(), $             ; A pixmap for fast redraw.
             selectedObjects: Ptr_New(), $    ; The currently selected objects.
             sx: 0L, $                        ; The static X location.
             sy: 0L, $                        ; The static Y location.
             tapeObject: Obj_New(), $         ; The default TapeMeasure object.
             layerObject: Obj_New(), $        ; A layer object for holding selectable annotation objects.
             INHERITS INTERACTION $
           }

END


