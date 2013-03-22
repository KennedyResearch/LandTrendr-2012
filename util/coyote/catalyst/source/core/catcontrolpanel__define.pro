;*****************************************************************************************************
;+
; NAME:
;       CATCONTROLPANEL
;
; PURPOSE:
;
;       The purpose of this routine is to implement a control panel as an object. It
;       automatically deals with whether the CP is embedded or standalone and adds
;       OK, APPLY and CANCEL buttons if required. Control panels are most often
;       represented by propertysheet widgets, but don't have to be. Our original 
;       notion was that every object in the Catalyst system could have its own control
;       panel to allow the user to manipulate the properties of that object. See the
;       ControlPanel method of the IMGAXES object as an example of how to create control 
;       panels for objects.
;
;       Objects can (and should) customise the control panel by adding other widget
;       objects. Note that the events for these will be passed to the caller object
;       event handler.
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
;       aCatControlPanel = Obj_New("CatControlPanel", callerObject)
;
; SUPERCLASSES:
;
;       WIDGETATOM
;       CATATOM
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { CatControlPanel, $
;             _caller : OBJ_NEW (), $           ; The object that calls the Control Panel.
;             _tlb    : OBJ_NEW (), $           ; The pseudo-TLB of the Control Panel widgets.
;             _event_destination: OBJ_NEW(), $  ; The object that will receive events.
;            INHERITS BaseWidget $
;            }
;
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 19th August 2002.
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
;       CATCONTROLPANEL::DRAW
;
; PURPOSE:
;
;       This method draws the control panel.
;
; SYNTAX:
;
;       aCatControlPanel -> Draw
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       _EXTRA:   Any keywords accepted by the "BASEWIDGET::DRAW" method.
;
;-
;*****************************************************************************************************
PRO CatControlPanel::Draw, _EXTRA=extraKeywords

   ; To display the control panel use the SHOW method
   ; This is to avoid problems that occur when adding children to
   ; an already realized base.
   ; Change this mechanism AT YOUR PERIL!!!
END



;*****************************************************************************************************
;+
; NAME:
;       CATCONTROLPANEL::EVENTHANDLER
;
; PURPOSE:
;
;       This method serves as an event handler for the "OK", "APPLY" and "CANCEL" buttons
;       (if they exist for this control panel). When an event is received, and new CATCONTROL_PANEL
;       event structure is created with the name of the button as the VALUE field. This event
;       structure is then sent to the real object handling the events for this control panel.
;
; SYNTAX:
;
;       Called automatically by the event dispatcher.
;
; ARGUMENTS:
;
;       event:    A structure describing the event.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO CatControlPanel::EventHandler, event

   ; Initialise the error handler
   @cat_pro_error_handler

   ; Create an event structure to pass on
   newEvent = {CATCONTROL_PANEL, ID:event.id, Handler:self._event_destination, Name:event.name, Value:''}

   ; This *must* be an OK, APPLY or CANCEL button event.
   CASE event.name OF
      'OK'     : newEvent.value = 'OK'
      'APPLY'  : newEvent.value = 'APPLY'
      'CANCEL' : newEvent.value = 'CANCEL'
      'CONTROLPANEL_TLB_RESIZE': BEGIN

         ; Get the Control Panel base. Look inside it for PROPERTYSHEET widgets.
         ; If you find any (on just the highest level), resize them, too, in the
         ; X direction. This won't work for PROPERTYSHEETS inside of base widgets,
         ; which are themselves inside the Control Panel base.
         base = event.id -> Get(ISA='CATCONTROLPANEL', /ALL)
         propertywidgets = base -> Get(ISA='PROPERTYSHEETWIDGET', /All, Count=num)
         FOR j=0,num-1 DO BEGIN
            propertywidgets[j] -> SetProperty, Scr_XSize=event.x
            propertywidgets[j] -> SetProperty, /Refresh_Property
         ENDFOR
         self -> Report, /Completed
         RETURN
      ENDCASE
      ELSE    : Message, 'Unexpected event in the CATCONTROLPANEL EventHandler.
   ENDCASE

   ; Recurse the event to the tree of control panels, if one exists
   children = self -> Get (ISA='CATCONTROLPANEL', COUNT=noChildren)
   FOR c = 0, noChildren - 1 DO children [c] -> EventHandler, newEvent

   ; Send the event on to the real event destination.
   self._event_destination -> EventHandler, newEvent

   ; Report completion
   IF (OBJ_VALID (self)) THEN self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CATCONTROLPANEL::CREATED_OWN_TLB
;
; PURPOSE:
;
;       This method returns a 1 if the control panel object created it's own TLB, and a 0 otherwise.
;
; SYNTAX:
;
;        IF aCatControlPanel -> CREATED_OWN_TLB(tlb) THEN tlb -> Draw ELSE cp -> Show
;
; ARGUMENTS:
;
;       tlb   : The object reference stored in the self._tlb field of the object. (Output)
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
FUNCTION CatControlPanel::Created_Own_TLB, tlb

   @cat_func_error_handler

   tlb = self._tlb

   self -> Report, /Completed

   RETURN, Obj_Valid(self._tlb)
END



;*****************************************************************************************************
;+
; NAME:
;       CATCONTROLPANEL::REFRESH_PROPERTIES
;
; PURPOSE:
;
;       This method searches in the ControlPanel base widget for a child
;       that is a PropertySheet widget. If it finds one, it sets the
;       REFRESH_PROPERTY keyword to 1 so that all the properties in the
;       control panel are updated at once.
;
; SYNTAX:
;
;       aCatControlPanel -> Refresh_Properties
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       PROPERTIES:   The string or string array listing the properties that should be refreshed.
;                     If not provided, all properties are refreshed.
;
;-
;*****************************************************************************************************
PRO CatControlPanel::Refresh_Properties, Properties=properties

   @cat_pro_error_handler

   ; Look for property sheet widgets amoung the children.
   objects = self -> Get(/All, ISA='PROPERTYSHEETWIDGET', Count=count)

   FOR j=0,count-1 DO BEGIN
      IF N_Elements(properties) EQ 0 THEN BEGIN
         IF Obj_Valid(objects[j]) THEN  objects[j] -> SetProperty, Refresh_Property=1
      ENDIF ELSE BEGIN
         IF Obj_Valid(objects[j]) THEN  objects[j] -> SetProperty, Refresh_Property=properties
      ENDELSE
   ENDFOR

   ; Report completion
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATCONTROLPANEL::SHOW
;
; PURPOSE:
;
;       This method shows the control panel on the screen. If the control panel
;       already exists, it gets popped to the front.
;
; SYNTAX:
;
;       aCatControlPanel -> Show
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
PRO CatControlPanel::Show

   @cat_pro_error_handler

   ; Recurse the event to the tree of control panels
   IF (OBJ_VALID (self._tlb)) THEN $
   BEGIN
      self._tlb -> Draw
      self._tlb -> SetProperty, ICONIFY=0
      WIDGET_CONTROL, self._ID, /Show
   ENDIF $
   ELSE BEGIN
      self -> GetProperty, First_Parent=parent
      parent -> Draw
   ENDELSE

   ; Report completion
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATCONTROLPANEL::CLEANUP
;
; PURPOSE:
;
;       This method cleans up a control panel.
;
; SYNTAX:
;
;       Called automatically on destruction, thus:
;
;       OBJ_DESTROY, aCatControlPanel
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       _EXTRA: Any keywords valid for the "BASEWIDGET::CLEANUP" method.
;
;-
;*****************************************************************************************************
PRO CatControlPanel::CLEANUP

   ; Initialise error handler
   @cat_pro_error_handler

   ; If a top-level base was created, destroy it
   IF (Obj_Valid (self._tlb)) THEN Obj_Destroy, self._tlb

   ; Call the superclass cleanup method
   self -> BaseWidget::Cleanup
END



;*****************************************************************************************************
;+
; NAME:
;       CATCONTROLPANEL::INIT
;
; PURPOSE:
;
;       This method creates a default control panel.
;
; SYNTAX:
;
;       Called automatically by initialisation, thus:

;       aCatControlPanel = OBJ_NEW ('CatControlPanel', caller)
;
; ARGUMENTS:
;
;       CALLER:         The object reference of the object the control panel controls.
;
; KEYWORDS:
;
;       BASEWIDGET:     A parent object for the control panel. If this is a base widget object
;                       the control panel will be mounted upon it. Otherwise, the control panel
;                       will be in its own window (see the TITLE and TLB_FRAME_ATTR keywords below).
;
;       EVENT_OBJECT:   The object that should receive the events generated by this control panel.
;                       If not provided, this will be the same as the CALLER.
;
;       NAME:           The name of the control panel. By default, it is set to the name of the
;                       caller object with " Control Panel" appended to it.
;
;       NO_APPLY:       Set this keyword to specify no "Apply" button on the interface. This keyword
;                       only applies if the CatControlPanel creates its own top-level base.
;
;       NO_CANCEL:      Set this keyword to specify no "Cancel" button on the interface. This keyword
;                       only applies if the CatControlPanel creates its own top-level base.
;
;       NO_OK:          Set this keyword to specify no "OK" button on the interface. This keyword
;                       only applies if the CatControlPanel creates its own top-level base.
;
;       PARENT:         A parent object for the control panel. If this is a base widget object
;                       the control panel will be mounted upon it. Otherwise, the control panel
;                       will be in its own window (see the TITLE and TLB_FRAME_ATTR keywords below).
;
;       TITLE:          The title attributes of the top-level base of the Control Panel. Only
;                       used if baseObj is not provided.
;
;       TLB_FRAME_ATTR: The frame attributes of the top-level base of the Control Panel. Only
;                       used if BASEWIDGET keyword is not provided.
;
;       _EXTRA:         Any keywords valid for the "BASEWIDGET::INIT" method.
;
;-
;*****************************************************************************************************
FUNCTION CatControlPanel::INIT, caller, $
                             EVENT_OBJECT=event_object, $
                             FLOATING=floating, $
                             GROUP_LEADER=group_leader, $
                             NAME=name, $
                             MODAL=modal, $
                             NO_APPLY=no_apply, $
                             NO_CANCEL=no_cancel, $
                             NO_OK=no_ok, $
                             PARENT=parent, $
                             TITLE=title, $
                             TLB_FRAME_ATTR=tlb_frame_attr, $
                             XOFFSET=xoffset, $
                             YOFFSET=yoffset, $
                             _EXTRA=extraKeywords

   ; Set up an error handler
   @cat_func_error_handler

   ; Check that a caller object exists
   IF (NOT Obj_IsA_Valid (caller, 'CatAtom')) THEN MESSAGE, 'CatAtom object required for control panel.'
   self._caller = caller

   ; If we haven't got a name, create a default one
   IF (N_ELEMENTS (name) EQ 0) THEN BEGIN
      caller -> GetProperty, Name=theName
      name = theName + ' Control Panel'
   ENDIF

   ; Check that the caller object has no control panel already. If it does,
   ; show the control panel.
   caller -> GetProperty, ControlPanel=cp
   IF (Obj_Valid (cp)) THEN $
   BEGIN
      topObject = CatGetTopObject(cp)
      IF OBJ_ISA_VALID(topObject, 'WIDGETATOM') THEN topObject -> SetProperty, Show=1
      ;Message, /Informational, name + ' already exists. Exiting...'
      RETURN, 0
   ENDIF

   ; If the EVENT_OBJECT keyword is absent, pass events directly to the caller
   IF (NOT OBJ_VALID (event_object)) THEN event_object = caller

   ; If no parent base is specified, create a top-level base with the buttons.
   IF (N_ELEMENTS (parent) EQ 0) THEN $
   BEGIN
      IF (N_ELEMENTS (tlb_frame_attr) EQ 0) THEN tlb_frame_attr = 0
      IF (N_ELEMENTS (title         ) EQ 0) THEN title = name
      parent = OBJ_NEW ('TOPLEVELBASE', Column=1, $
                                        Title=title, $
                                        /Align_Center, $
                                        Event_Object=self, $
                                        FLOATING=floating, $
                                        GROUP_LEADER=group_leader, $
                                        MODAL=modal, $
                                        NAME='CONTROLPANEL_TLB_RESIZE', $
                                        SIZE_EVENTS=1, $
                                        TLB_Frame_Attr=tlb_frame_attr, $
                                        XOFFSET=xoffset, YOFFSET=yoffset, $
                                        YPAD=0, XPAD=0)
      self._tlb = parent
   ENDIF

   ; Create the control panel as a sub base
   ok = self -> BaseWidget::Init (parent, Name=name, Event_Object=event_object, $
      GROUP_LEADER=group_leader, XOFFSET=xoffset, YOFFSET=yoffset, _EXTRA=extraKeywords)
   IF (NOT ok) THEN Message, 'Base widget failed to initialise'

   ; Save the event destination object.
   self._event_destination = event_object

   ; If we created the TLB, add the control buttons to it
   ; If this is a top-level control panel, create the OK, Apply and Cancel buttons
   noControls = 3 - KEYWORD_SET (no_ok) - KEYWORD_SET (no_apply) - KEYWORD_SET (no_cancel)
   IF (Obj_Valid (self._tlb) AND (noControls GT 0)) THEN $
   BEGIN
      bttns = OBJ_NEW ('BaseWidget',  self._tlb, COLUMN=3, FRAME=1, /Align_Center)
      IF (NOT KEYWORD_SET (no_cancel)) THEN $
         cancelBttn = OBJ_NEW ('ButtonWidget', bttns, VALUE='Cancel', NAME='CANCEL')
      IF (NOT KEYWORD_SET (no_apply )) THEN $
         applyBttn  = OBJ_NEW ('ButtonWidget', bttns, VALUE='Apply', NAME='APPLY')
      IF (NOT KEYWORD_SET (no_ok    )) THEN $
         okBttn     = OBJ_NEW ('ButtonWidget', bttns, VALUE='OK', NAME='OK')
   ENDIF

   ; Inform the caller object of the Control Panel reference
   caller -> CatAtom::SetProperty, ControlPanel=self

   ; Report completion and return success flag
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       CATCONTROLPANEL CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the CATCONTROLPANEL object class definition.
;       The CATCONTROLPANEL object is subclassed from the BASEWIDGET object.
;
;*****************************************************************************************************

PRO CatControlPanel__DEFINE, class

   class = { CatControlPanel, $
             _caller : OBJ_NEW (), $           ; The object that calls the Control Panel.
             _tlb    : OBJ_NEW (), $           ; The pseudo-TLB of the Control Panel widgets.
             _event_destination: OBJ_NEW(), $  ; The object that will receive events.
            INHERITS BaseWidget $
            }
END
