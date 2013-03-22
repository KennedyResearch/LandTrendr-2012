;*****************************************************************************************************
;+
; NAME:
;      SPINNER__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a spinner widget, in which
;       arrow buttons affect the value of the spinner. Leaving the cursor down
;       on an arrow button will cause the value of the spinner to "spin" or
;       change rapidly.
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
;       theObject = Obj_New("SPINNER")
;
; SUPERCLASSES:
;
;       BASEWIDGET
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = {SPINNER, $
;             INHERITS BASEWIDGET $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 26 September 2005.
;       Fixed bug that allows spinner value to be set manually. 25 July 2007. DWF.
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
;      SPINNER::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the SPINNER object. A
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
PRO Spinner::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.
   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Spinner Control Panel', _EXTRA=extraKeywords)

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.
   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, $
      Name='SPINNER PROPERTYSHEET', YSize=6)
   aproperties -> SetProperty, Event_Object=self, Event_Method='ControlPanel_Events'

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;        Spinner::CONTROLPANELEVENTS
;
; PURPOSE:
;
;        This method is the event handler for the Spinner object's ControlPanel.
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
PRO Spinner::ControlPanel_Events, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName

   CASE eventName OF

      'SPINNER PROPERTYSHEET': BEGIN
             component = event.component
             identifier = event.identifier
             event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
             event.component -> SetPropertyByIdentifier, identifier, value
             END
      ELSE: Message, 'Unexpected event in ControlPanelEvents event handler.'
   ENDCASE

END




;*****************************************************************************************************
;+
; NAME:
;       SPINNER::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the SPINNER object. It will typically
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
PRO Spinner::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   CASE event.name OF

      'SPINNER_TEXTWIDGET': BEGIN

         ; Save the current value of the spinner.
         oldValue = self._value

         ; Can the current value be converted to a double?
         ; If so, do it. Otherwise, restore the old value and RETURN.
         ; Conversion errors are IOERRORs.
         ON_IOERROR, skip
         self._textID -> GetProperty, Value=newValue
         newValue = Double(newValue[0])
         self -> SetProperty, VALUE=newValue
         self -> SendEvent
         RETURN

         skip: ; Conversion unsuccessful.
         self._textID -> SetProperty, Value=oldValue
         RETURN
         END

      ELSE : BEGIN ; One of the spin buttons

         ; Store the select state. It is used at the end of the loop below.
         event.id -> SetProperty, UValue=event.select

         ; Are we finished?
         IF event.select EQ 0 THEN BEGIN
            event.id -> SetProperty, /Clear_Events
            RETURN
         ENDIF

         direction = event.name EQ 'SPINNER_UP' ? 1L : -1L

         ; We don't want to start the spinner too soon. Give the user
         ; a bit of time.
         spinwait = 10

         ; We are going to exit if the mouse is released from the button.
         WHILE 1 DO BEGIN

            ; Get the current time.
            t1 = Systime(1)

            ; Value depends on which direction you are incrementing.
            self._multiple = self._multiple + direction
            CASE direction OF
               -1L: self._value = (self._increment * self._multiple)
                1L: self._value = (self._increment * self._multiple)
            ENDCASE

            ; Update display.
            self._textID -> SetProperty, Value=String(self._value, Format='(G0)')

            ; Send an event.
            self -> SendEvent

            checkit:

            ; Has the mouse UP event arrived yet?
            newEvent = Widget_Event(event.id->GetID(), BAD_ID=dead, /NOWAIT)
            IF dead NE 0L THEN RETURN ELSE BEGIN
               IF Tag_Names(newEvent, /Structure_Name) NE 'WIDGET_NOEVENT' THEN $
                   event.id -> SetProperty, UValue=newEvent.select
            ENDELSE

            ; Check the select state of the button. If the button is UP, RETURN.
            event.id -> GetProperty, UValue=selectState
            IF selectState EQ 0 THEN RETURN ELSE BEGIN
               self -> SendEvent
            ENDELSE

            ; Loop spinwait times before spinning.
            IF spinwait NE 0 THEN BEGIN
               Wait, 0.05D
               spinwait--
               GOTO, checkit
            ENDIF

            ; Now spin it.
            time1 = Systime(1) - t1
            IF time1 LT self._spindelay THEN Wait, self._spindelay - time1

         ENDWHILE
         END
   ENDCASE

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;      SPINNER::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain SPINNER properties. Be sure
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
;     INCREMENT:      The increment by which the spinner value changes.
;
;     LABELWIDGET:    The label widget holding the title (if there is one).
;
;     SPINDELAY:      The time delay before the spinner kicks into gear. Allows the user
;                     to depress the arrow buttons without activating the spinner. By default 0.05 seconds.
;
;     TEXTWIDGET:     The text widget holding the value of the spinner.
;
;     VALUE:          The current value of the spinner.
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO Spinner::GetProperty,  $
   INCREMENT=increment, $
   LABELWIDGET=labelwidget, $
   SPINDELAY=spindelay, $
   TEXTWIDGET=textwidget, $
   VALUE=value, $
  _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(increment) THEN increment = self._increment
   IF Arg_Present(spindelay) THEN spindelay = self._spindelay
   IF Arg_Present(value) THEN value = self._value
   IF Arg_Present(labelWidget) THEN labelWidget = self._labelID
   IF Arg_Present(textWidget) THEN textWidget = self._textID

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> BASEWIDGET::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;      SPINNER::SENDEVENT
;
; PURPOSE:
;
;       This method sends an event from the compound widget to the actual event handler.
;
;
; SYNTAX:
;
;       theObject -> SendEvent
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     None
;-
;*****************************************************************************************************
PRO Spinner::SendEvent

   @cat_pro_error_handler

   ; Check that there is at least one valid event object, otherwise swallow the event.
   eventObjs = self._event_handler -> Get(/All)
   thisEvent = {SPINNER_EVENT, self, Obj_New(), 'SPINNER_EVENT', self._name, self._value}
   IF (MAX (OBJ_VALID (eventObjs)) EQ 0) THEN RETURN ; There is no valid event object.

   ; Find out the target object for the event, get the event method from
   ; the target object, and send the event to this method, UNLESS this
   ; object has its own object method.
   FOR e = 0, N_ELEMENTS (eventObjs) - 1 DO $
   BEGIN
      IF OBJ_VALID (eventObjs [e]) THEN $
      BEGIN
         thisEvent.handler = eventObjs [e]
         eventObjs[e] -> CatAtom::GetProperty, Event_Method=event_method
         IF (self._event_method_real NE "") AND (e EQ 0) THEN $
            thisMethod = self._event_method_real ELSE thisMethod = event_method
         Call_Method, thisMethod, eventObjs[e], thisEvent
      ENDIF
   ENDFOR

END


;*****************************************************************************************************
;+
; NAME:
;      SPINNER::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the SPINNER object's properties. Be sure
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
;     INCREMENT:    The increment by which the spinner value changes. By default, 1.0D.
;
;     SPINDELAY:    The time delay before the spinner kicks into gear. Allows the user
;                   to depress the arrow buttons without activating the spinner. By default 0.05 seconds.
;
;     VALUE:        The current value of the spinner. By default, 0.0D.
;
;     _EXTRA:       Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO Spinner::SetProperty, $
   INCREMENT=increment, $
   SPINDELAY=spindelay, $
   VALUE=value, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(increment) NE 0 THEN self._increment = Double(increment)
   IF N_Elements(spindelay) NE 0 THEN self._spindelay = Double(spindelay)
   IF N_Elements(value) NE 0 THEN BEGIN
      self._value = Double(value)
      self._textID -> SetProperty, Value=String(self._value, Format='(G0)')
   ENDIF
   self._multiple = Round(self._value / self._increment)

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> BASEWIDGET::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;      SPINNER::CLEANUP
;
; PURPOSE:
;
;       This is the SPINNER object class destructor method.
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
PRO Spinner::CLEANUP

   @cat_pro_error_handler

   ; Remove all the children in self._event_handler and destroy the container.
   self._event_handler -> Remove, /All
   Obj_Destroy, self._event_handler

   self -> BASEWIDGET::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;      SPINNER::INIT
;
; PURPOSE:
;
;       This is the SPINNER object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     parent:     The parent object referece. This object will be added to the parent's container.
;
; KEYWORDS:
;
;     INCREMENT:    The increment by which the spinner value changes. By default, 1.0D.
;
;     SPINDELAY:    The time delay before the spinner kicks into gear. Allows the user
;                   to depress the arrow buttons without activating the spinner. By default 0.05 seconds.
;
;     TITLE:        A string label that goes next to the spinner to identify it. By default, "".
;
;     VALUE:        The current value of the spinner. By default, 0.0D.
;
;     VERTICAL:     Set this keyword to put the spinner title (if there is one) above the spinner, rather
;                   than beside it.
;
;     XTITLESIZE:   Passed on the the SCR_XSIZE of the label widget that holds the title. No default value.
;
;     XSPINSIZE:    Passed on to the XSIZE of the text widget used to hold the value of the spinner. By default, 8.
;
;     _EXTRA:       Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION Spinner::INIT, parent, $
   EVENT_METHOD=event_method, $   ; Required to intercept event method intended for CATATOM.
   EVENT_OBJECTS=event_objects, $ ; Required to intercept event objects intended for CATATOM.
   INCREMENT=increment, $
   SPINDELAY=spindelay, $
   TITLE=title, $
   VALUE=value, $
   VERTICAL=vertical, $
   XTITLESIZE=xtitlesize, $
   XSPINSIZE=xspinsize, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   IF Keyword_Set(vertical) THEN $
      ok = self -> BASEWIDGET::INIT (parent, _EXTRA=extraKeywords, XPAD=0, YPAD=0, SPACE=2, COLUMN=1, /BASE_ALIGN_CENTER) ELSE $
      ok = self -> BASEWIDGET::INIT (parent, _EXTRA=extraKeywords, XPAD=0, YPAD=0, SPACE=2, ROW=1, /BASE_ALIGN_CENTER)
   IF ~ok THEN RETURN, 0

   ; The following is required because the SPINNER is a compound object.
   ; Get the assigned event object. Replace this with the self object. This
   ; assures you that the events will go through the SPINNER's EventHandler
   ; method first. Save the assigned event object so it can be used at the end
   ; of the compound object's EventHandler method.
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

   ; Define parameters.
   IF N_Elements(increment) EQ 0 THEN increment = 1.00D ELSE increment = Double(increment)
   IF N_Elements(spindelay) EQ 0 THEN spindelay = 0.05D ELSE spindelay = Double(spindelay)
   IF N_Elements(title) EQ 0 THEN title = ""
   IF N_Elements(xspinsize) EQ 0 THEN xspinsize = 8
   IF N_Elements(value) EQ 0 THEN value = 0.0D

   ; Need a label?
   IF title NE "" THEN BEGIN
      IF N_Elements(xtitlesize) EQ 0 THEN $
         self._labelID = Obj_New('LabelWidget', self, Value=title, Name='SPINNER_LABEL') ELSE $
         self._labelID = Obj_New('LabelWidget', self, Value=title, SCR_XSIZE=xtitlesize, Name='SPINNER_LABEL')
   ENDIF

   ; Create the rest of the widgets.
   spinbase = Obj_New('BaseWidget', self, XPAD=0, YPAD=0, SPACE=0, ROW=1)
   self._textID = Obj_New('TextWidget', spinbase, $
      /EDITABLE, $
      NAME='SPINNER_TEXTWIDGET', $
      XSIZE=xspinsize, $
      VALUE=STRING(value, Format='(G0)'))

   ; Up/down buttons.
   buttonBase = Obj_New('BaseWidget', spinbase, $
        /ALIGN_CENTER, $
        /COLUMN, $
        /TOOLBAR, $
        XPAD=0, YPAD=0, SPACE=0)

   ; Motif needs an extra 1 pixel padding around bitmaps.
   isMotif = !Version.OS_Family NE 'Windows'

   bitmap = Filepath('spinup.bmp', SUBDIR=['resource','bitmaps'])
   upID = Obj_New('ButtonWidget', buttonBase, $
        /BITMAP, VALUE=bitmap, $
        /PUSHBUTTON_EVENTS, $
        NAME='SPINNER_UP', $
        UVALUE=0, $    ; Mouse is up.
        XSIZE=16+isMotif, YSIZE=10+isMotif)

   bitmap = FILEPATH('spindown.bmp', SUBDIR=['resource','bitmaps'])
   downID = Obj_New('ButtonWidget', buttonBase, $
        /BITMAP, VALUE=bitmap, $
        /PUSHBUTTON_EVENTS, $
        NAME='SPINNER_DOWN', $
        UVALUE=0, $    ; Mouse is up.
        XSIZE=16+isMotif, YSIZE=10+isMotif)

   ; Load the object.
   self._value = value
   self._increment = increment
   self._multiple = Round(self._value / self._increment)
   self._spindelay = spindelay

   ; Register properties for the control panel.
   self -> RegisterProperty, 'INCREMENT', 3, NAME="Increment"
   self -> RegisterProperty, 'SPINDELAY', 3, NAME="Spin Delay"
   self -> RegisterProperty, 'VALUE', 3, NAME="Value"

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;      SPINNER CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the SPINNER object.
;
;*****************************************************************************************************
PRO Spinner__DEFINE, class

   event  = { SPINNER_EVENT, $
                ID:Obj_New(), $            ; The widget object that caused the event.
                HANDLER:Obj_New(), $       ; The event handler object.
                EVENT_NAME: "", $          ; The name of the event, "SPINNER_EVENT".
                NAME: "", $                ; The name of the object.
                VALUE:0.0D $               ; The value of the spinner.
      }

   class = { SPINNER, $                       ; The SPINNER object class definition.
                _event_handler:Obj_New(), $   ; The real event handler object(s) for the SPINNER.
                _event_method_real: "", $     ; The event method assigned by the user to this object widget.
                _increment: 0.0D, $           ; The increment of the spinner.
                _labelID: Obj_New(), $        ; The title label in the spinner.
                _spindelay: 0.0D, $           ; The delay time of the spinner.
                _textID: Obj_New(), $         ; The text widget in the spinner.
                _multiple: 0L, $              ; The multiple of the increment. Used to avoid problems in conversion.
                _value: 0.0D, $               ; The current value of the spinner.
             INHERITS BASEWIDGET $
           }

END


;*****************************************************************************************************
;
; NAME:
;      SPINNER TEST PROGRAM
;
; PURPOSE:
;
;       This is the test program for the SPINNER object. It may not
;       be required in your object.
;
;*****************************************************************************************************
PRO Spinner_Test, tlb
   tlb = Obj_New('ToplevelBase', /Column)
   spinner = Obj_New('spinner', tlb, Title='Power', xtitlesize=30, value=0.25, increment=0.01)
   spinner = Obj_New('spinner', tlb, Title='Spin', xtitlesize=30, value=20, increment=2)
   tlb -> Draw
END