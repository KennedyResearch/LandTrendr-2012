;*****************************************************************************************************
;+
; NAME:
;       LISTWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to implement a list widget in an object
;       structure. This routine is a compound widget in order to give the list
;       more functionality than the current IDL list widget.
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
;       Compound object widgets.
;
; SYNTAX:
;
;       aList = Obj_New("LISTWIDGET", theParent, VALUE=['pig', 'cow', coyote'], INDEX=2)
;
; SUPERCLASSES:
;
;       BASEWIDGET
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;      class = { LISTWIDGET, $                ; The LISTWIDGET object class definition.
;                _clicks: 0L, $               ; The number of clicks required to generate events.
;                _index: Ptr_New(), $         ; A pointer to the index or indices of the selections.
;                _listID:0L, $                ; The list widget identifier.
;                _listtitle: Obj_New(), $     ; The title object.
;                _listbutton: Obj_New(), $    ; The button object.
;                _multiple: 0L, $             ; A flag that indicates this list widget generates multiple selections.
;                _selection: Ptr_New(), $     ; A pointer to the selections.
;                _value: Ptr_New(), $         ; The values or selections on the list.
;                _event_handler: Obj_New(), $ ; The real (assigned) event handler object for this compound object.
;                _event_method_real: "", $    ; The event method assigned by the user to this object widget.
;                INHERITS BASEWIDGET $
;              }
;
; EVENT_STRUCTURE:
;
;   This is the event structure sent from the LISTWIDGET to the event handler object.
;
;      event = { LISTWIDGET_EVENT, $
;                ID:Obj_New(), $            ; The widget object that caused the event.
;                TOP: Obj_New(), $          ; The object at the top of the object hierarchy.
;                HANDLER:Obj_New(), $       ; The event handler object.
;                EVENT_NAME: "", $          ; The name of the event, LISTWIDGET_EVENT.
;                NAME: "", $                ; The name of the object.
;                CLICKS: 0L, $              ; The number of clicks for selection. 1 or 2.
;                INDEX: Ptr_New(), $        ; The index (or indices) of the current selection.
;                NUM_SELECTED: 0L, $        ; The number of elements in the selection pointer.
;                SELECTION:Ptr_New() $      ; The current list selection (i.e., value[index]).
;              }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W.Fanning, 25 March 2003.
;       Fixed a problem with the EventHandler when using context sensitive menus with ListWidget. DWF. 15 May 2004.
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
;       LISTWIDGET::EVENTHANDLER
;
; PURPOSE:
;
;       This is the event handler method of the object. The purpose is to set the
;       object's current selection and index number. A LISTWIDGET event is
;       sent to the parent object's event handler.
;
; SYNTAX:
;
;       widgetObject ->EventHandler, event
;
; ARGUMENTS:
;
;       EVENT:  The event created by the list widget.
;-
;*****************************************************************************************************
PRO ListWidget::EventHandler, event

   @cat_pro_error_handler

      ; Get the current selections.

   self -> GetProperty, INDEX=index, NAME=name, NUM_SELECTED=num_selected,  SELECTION=selection

      ; If there is nothing selected then return.

   IF num_selected EQ 0 THEN BEGIN
      self -> Report, /Completed
      RETURN
   ENDIF

   IF Ptr_Valid(self._selection) THEN *self._selection = selection ELSE self._selection = Ptr_New(selection)
   IF Ptr_Valid(self._index) THEN *self._index = index ELSE self._index = Ptr_New(index)

      ; Whether you send an event or not, depends upon if you have a button
      ; or not, and whether this is a button event.

   IF Obj_Valid(self._listbutton) THEN BEGIN
      event.ID -> GetProperty, Name=eventName;, Top_Object=top_object

      IF eventName EQ 'LISTBUTTON' THEN BEGIN

      ; Check that there is at least one valid event object, otherwise complain
      eventObjs = self._event_handler -> Get(/All)
      thisEvent = {LISTWIDGET_EVENT, self, eventObjs[0], 'LISTWIDGET_EVENT', name, $
         1, self._index, num_selected, self._selection}
      IF (MAX (OBJ_VALID (eventObjs)) EQ 0) THEN $
         MESSAGE, 'No event handler for object ' + OBJ_CLASS (object) + ' named ' + objName

      ; Find out the target object for the event, get the event method from
      ; the target object, and send the event to this method.
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

      ENDIF ELSE self._listButton -> SetProperty, Input_Focus=1
   ENDIF ELSE BEGIN

      ; We have to be careful about getting events for context-sensitive menus.
      ; If they come into this event handler, they are button events that should
      ; be sent onto the the target object.
      eventObjs = self._event_handler -> Get(/All)
      IF event.event_name EQ 'WIDGET_BUTTON' THEN BEGIN
        thisEvent = event
      ENDIF ELSE BEGIN

         ; Check that there is at least one valid event object, otherwise complain
         thisEvent = {LISTWIDGET_EVENT, self, eventObjs[0], 'LISTWIDGET_EVENT', name, $
            1, self._index, num_selected, self._selection}
      ENDELSE

      IF (MAX (OBJ_VALID (eventObjs)) EQ 0) THEN $
         MESSAGE, 'No event handler for object ' + OBJ_CLASS (object) + ' named ' + objName

      ; Find out the target object for the event, get the event method from
      ; the target object, and send the event to this method.
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
   ENDELSE
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       LISTWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to obtain the ListWidget object's properties
;
; SYNTAX:
;
;       aListWidget -> GetProperty, Selection=currentSelection
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       CONTEXT_EVENTS:  Returns a 1 if context events are on, a 0 otherwise.
;
;       EVENT_OBJECT:    Normally, the EVENT_OBJECT is obtained in the ATOM object, but the
;                        LISTWIDGET is compound object, so we have to intercept the process
;                        and get the event object that will *eventually* receive the LISTWIDGET event.
;
;       INDEX:           The index number or numbers of the current selection.
;
;       MULTIPLE:        Returns a 1 is this is a multiple-selection list, 0 otherwise.
;
;       NUMBER:          Returns the number of selections available in the list.
;
;       NUM_SELECTED:    Returns the number of selections in the SELECTION.
;
;       SELECTION:       Returns the current selection or selections in the list values.
;
;       TOP:             Returns the index number of the list item current at the top
;                        of the visible portion of the list.
;
;       VALUE:           Returns the current possible values or selections in the list.
;
;       VISIBLE:         Returns the number of list items visible at the current time.
;
;       XSIZE:           The size of the list widget
;       _REF_EXTRA:      Any keywords appropriate for the superclass GetProperty methods.
;-
;*****************************************************************************************************
PRO ListWidget::GetProperty, $
   CONTEXT_EVENTS=context_events, $
   EVENT_OBJECT=event_object, $
   INDEX=index, $
   MULTIPLE=multiple, $
   NUM_SELECTED=num_selected, $
   NUMBER=number, $
   SELECTION=selection, $
   TOP=top, $
   VALUE=value, $
   VISIBLE=visible, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Call the superclass getproperty method if needed.

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> WidgetAtom::GetProperty, _EXTRA=extraKeywords

      ; Get the properties. Make sure you have a valid widget ID.

   IF Widget_Info(self._listid, /Valid_ID) EQ 0 THEN BEGIN
      self -> Report, /Completed
      RETURN
   ENDIF

   IF Arg_Present(context_events) THEN context_events = Widget_Info(self._ID, /Context_Events)

      ; Get the current selection.

   IF Ptr_Valid(self._value) THEN BEGIN
      value = *self._value
      index = Widget_Info(self._listID, /LIST_SELECT)
      IF N_Elements(index) EQ 1 THEN BEGIN
         IF index EQ -1 THEN num_selected = 0 ELSE BEGIN
            num_selected = 1
            selection = (*self._value)[index]
         ENDELSE
      ENDIF ELSE BEGIN
         selection = (*self._value)[index]
         num_selected = N_Elements(selection)
      ENDELSE
   ENDIF ELSE num_selected = 0

      ; Other parameters.

   multiple = Widget_Info(self._listID, /LIST_MULTIPLE)
   number = Widget_Info(self._listID, /LIST_NUMBER)
   top = Widget_Info(self._listID, /LIST_TOP)
   visible = Widget_Info(self._listID, /LIST_NUM_VISIBLE)
   event_object = self._event_handler

      ; Report completion.

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       LISTWIDGET::NOTIFY_REALIZE
;
; PURPOSE:
;
;       This method is used to make sure all the components of the list compound widget object
;       are the same size when they are realized
;
; SYNTAX:
;
;       Called automatically when widget hierarchy is realized.
;-
;*****************************************************************************************************
PRO ListWidget::Notify_Realize, id, theObject
   theObject -> Resize
END


;*****************************************************************************************************
;+
; NAME:
;       LISTWIDGET::RESIZE
;
; PURPOSE:
;
;       This method is used to dynamically resize the list.
;
; SYNTAX:
;
;       aListWidget -> Resize, newsize
;
; ARGUMENTS:
;
;       newSize:      The new size of the list. If the variable newSize is
;                     not provided, the new size is set by the parent widget's
;                     X screen size. This makes it possible to have the list
;                     sized to fit it's parent base widget.
;
; KEYWORDS:
;
;       PARENT_SIZE:  Returns the SCR_XSIZE of the parent widget. (Output keyword)
;-
;*****************************************************************************************************
PRO ListWidget::Resize, newSize,  ParentSize=parentSize

   @cat_pro_error_handler

      ; Get the parent widget's geometry and X screen size.

   self._parent -> GetProperty, ID=parentID
   parentGeometry = Widget_Info(parentID, /Geometry)
   parentSize = parentGeometry.scr_xsize

      ; Has a size been provided? In not, use the parent's X screen size.

   IF N_Elements(newSize) EQ 0 THEN BEGIN
      newSize = parentGeometry.scr_xsize
   ENDIF

      ; Resize the list widget.

   IF Obj_Valid(self._listtitle) THEN self._listtitle -> SetProperty, SCR_XSIZE=newSize
   Widget_Control, self._listID, Scr_XSize=newSize
   IF Obj_Valid(self._listbutton) THEN self._listbutton -> SetProperty, SCR_XSIZE=newSize

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       LISTWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the ListWidget object's properties
;
; SYNTAX:
;
;       aListWidget -> SetProperty, Map=1
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       CONTEXT_EVENTS:    Set to 1 to turn context events on for the base widget.
;
;       EVENT_OBJECTS: Normally, the EVENT_OBJECTS keyword is set by the ATOM object, but the
;                 LISTWIDGET is compound object, so we have to intercept the process
;                 and set the event object(s) that will *eventually* receive the LISTWIDGET event.
;
;       FORMAT:   Set this keyword to a format for processing the "values" of the
;                 list. For example, VALUES=[4, 5], FORMAT='(F4.2)'. Only used if VALUE is set.
;
;       SELECT:   A scalar or vector of indices into the list items that will highlight or
;                 select these items in the list.
;
;       SPACES:   A one- or two-element array that indicates how many blank spaces to
;                 insert around the "values" of the list. The first element is the
;                 before-value spacing, the secound element is the after-value spacing.
;                 A single value adds the same spacing both before and after the value.
;                 Only used if VALUE is set.
;
;       TOPVALUE: Set this keyword to an integer that specified the index of the item
;                 at the top of the list in the list widget.
;
;       VALUE:   A vector of possible selections for the list. May be any data type.
;
;       XSIZE:   The size of the list widget in character units.
;
;       YSIZE:   The size of the list widget in terms of items in the list.
;
;
;       _EXTRA:  Any keywords appropriate for the superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO ListWidget::SetProperty, $
   CONTEXT_EVENTS=context_events, $
   EVENT_METHOD=event_method, $     ; Can't change EVENT_METHOD for compound objects.
   EVENT_OBJECTS=event_objects, $   ; Required to intercept keyword intended for CATATOM.
   FORMAT=format, $
   SELECT=select, $
   SPACES=spaces, $
   TOPVALUE=topvalue, $
   VALUE=value, $
   XSIZE=xsize, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

      ; Error handling.

   @cat_pro_error_handler

      ; Value?

   IF N_Elements(context_events) NE 0 THEN $
      WIDGET_CONTROL, self._id, CONTEXT_EVENTS=Keyword_Set(context_events)

   IF N_Elements(value) NE 0 THEN BEGIN

      IF N_Elements(spaces) EQ 0 THEN BEGIN
         forwardSpace = ""
         trailingSpace = ""
      ENDIF ELSE BEGIN
         IF N_Elements(spaces) EQ 2 THEN BEGIN
            IF spaces[0] LE 0 THEN forwardSpace = "" ELSE forwardSpace = String(Replicate(32B,spaces[0]))
            IF spaces[1] LE 0 THEN trailingSpace = "" ELSE trailingSpace = String(Replicate(32B,spaces[1]))
         ENDIF ELSE BEGIN
            IF spaces[0] LE 0 THEN forwardSpace = "" ELSE forwardSpace = String(Replicate(32B,spaces[0]))
            IF spaces[0] LE 0 THEN trailingSpace = "" ELSE trailingSpace = String(Replicate(32B,spaces[0]))
         ENDELSE
      ENDELSE
      IF Keyword_Set(format) THEN theValue = String(value, Format=format) ELSE theValue = StrTrim(value,2)

      Widget_Control, self._listID, Set_Value=theValue
      IF Ptr_Valid(self._value) THEN *self._value = value ELSE self._value = Ptr_New(value)

   ENDIF

      ; Selection?

   IF N_Elements(select) NE 0 THEN Widget_Control, self._listID, Set_List_Select=select

      ; Top?

   IF N_Elements(topvalue) NE 0 THEN Widget_Control, self._listID, Set_List_Top=topvalue

      ; The following is required because the LISTWIDGET is a compound object.
      ; Get the assigned event object. Replace this with the self object. This
      ; assures you that the events will go through the LISTWIDGET's EventHandler
      ; method first. Save the assigned event object so it can be used at the end
      ; of the compound object's EventHandler method.

  IF N_Elements(event_objects) NE 0 THEN BEGIN
      first = self._event_handler->Get(Position=0)
      self._event_handler -> Remove, /All
      IF Obj_Valid(first) THEN self._event_handler -> Add, first
      FOR j=0,N_Elements(event_objects) -1 DO BEGIN
         IF Obj_Valid(event_objects[j]) THEN self._event_handler -> Add, event_objects[j]
      ENDFOR
  ENDIF

  IF N_Elements(xsize) NE 0 THEN Widget_Control, self._listID, XSize=xsize
  IF N_Elements(ysize) NE 0 THEN Widget_Control, self._listID, YSize=ysize

      ; If there are more keywords, call the superclass SetProperty method.

   self -> WIDGETATOM::SetProperty, _Extra=extrakeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       LISTWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the LISTWIDGET object class destructor method.
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
PRO ListWidget::CLEANUP
   Ptr_Free, self._index
   Ptr_Free, self._selection
   Ptr_Free, self._value

   ; Remove all the children in self._event_handler and destroy the container.
   self._event_handler -> Remove, /All
   Obj_Destroy, self._event_handler

   ; Call superclass Cleanup.
   self->WIDGETATOM::CLEANUP
END



;*****************************************************************************************************
;+
; NAME:
;       LISTWIDGET::INIT
;
; PURPOSE:
;
;       This is the LISTWIDGET object class initialization method
;
; SYNTAX:
;
;       alist = Obj_New('LISTWIDGET')
;
; ARGUMENTS:
;
;       theParent:     An object reference to a WIDGETATOM-subclassed object.
;                      This is the same as using the PARENT keyword.
;
; KEYWORDS:
;
;       CONTEXT_EVENTS:    Set this keyword to turn context events on for this widget object..
;
;       FONT:           Set this keyword to the name of a font to use on the list.
;
;       FORMAT:         Set this keyword to a format for processing the "values" of the
;                       list. For example, VALUES=[4, 5], FORMAT='(F4.2)'.
;
;       FRAME:          Set this keyword to create a frame this many pixels wide around the list.
;
;       MULTIPLE:       Set this keyword if you want to make multiple selections in the list. If
;                       this keyword is set, a button is placed at the bottom of the list widget
;
;       MULTI_TITLE:    The text on the MULTI_BUTTON widget. It will be "Apply" by default.
;
;       NO_MULTI_BUTTON: Set this keyword if you are making a MULTIPLE selection and do not want
;                       an APPLY button at the bottom of the list.
;
;       PARENT:         An object reference to a WIDGETATOM-subclassed object.
;
;       SCR_XSIZE:      Set the screen X size of the base to this many pixels. (Use discouraged.)
;
;       SCR_YSIZE:      Set the screen Y size of the base to this many pixels. (Use discouraged.)
;
;       SPACES:         A one- or two-element array that indicates how many blank spaces to
;                       insert around the "values" of the list. The first element is the
;                       before-value spacing, the secound element is the after-value spacing.
;                       A single value adds the same spacing both before and after the value.
;
;       TITLE:          A string that is the "title" of the list. If absent, no Title Label
;                       appears above the list.
;
;       UNITS:          The units for measurments. The default is 0 for pixels. Other values are
;                       1 for inches, and 2 for centimeters.
;
;       VALUE:          A vector of possible selections for the list. May be any data type.
;
;       XSIZE:          The X size of the widget.
;
;       YSIZE:          The Y size of the widget.
;
;       _EXTRA:         Any keyword appropriate for the superclass INIT methods.
;-
;*****************************************************************************************************
FUNCTION ListWidget::INIT, parent, $
   CONTEXT_EVENTS=context_events, $
   EVENT_OBJECTS=event_objects, $ ; Required to intercept event objects intended for CATATOM.
   FONT=font, $
   FORMAT=format, $
   FRAME=frame, $
   MULTIPLE=multiple, $
   MULTI_TITLE=multi_title, $
   NO_MULTI_BUTTON=no_multi_button, $
   INDEX=index, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   SPACES=spaces, $
   TITLE=title, $
   UNITS=units, $
   VALUE=value, $
   XSIZE=xsize, $
   YSIZE=ysize, $
   _EXTRA=extrakeywords

   @cat_func_error_handler

      ; If there is a parent, make sure it is a valid BASEWIDGET class object.

   IF OBJ_ISA_VALID(parent, "BASEWIDGET") EQ 0 THEN Message, 'Parent object invalid or not of type BASEWIDGET.'

      ; Check for presence of keywords.

   IF N_Elements(clicks) EQ 0 THEN clicks = 0
   IF N_Elements(index) EQ 0 THEN index = 0
   multiple = Keyword_Set(multiple)
   IF N_Elements(value) EQ 0 THEN value = ['Dog', 'Cat', 'Coyote']
   IF N_Elements(spaces) EQ 0 THEN BEGIN
      forwardSpace = ""
      trailingSpace = ""
   ENDIF ELSE BEGIN
      IF N_Elements(spaces) EQ 2 THEN BEGIN
         IF spaces[0] LE 0 THEN forwardSpace = "" ELSE forwardSpace = String(Replicate(32B,spaces[0]))
         IF spaces[1] LE 0 THEN trailingSpace = "" ELSE trailingSpace = String(Replicate(32B,spaces[1]))
      ENDIF ELSE BEGIN
         IF spaces[0] LE 0 THEN forwardSpace = "" ELSE forwardSpace = String(Replicate(32B,spaces[0]))
         IF spaces[0] LE 0 THEN trailingSpace = "" ELSE trailingSpace = String(Replicate(32B,spaces[0]))
      ENDELSE
   ENDELSE
   index = 0 > index < (N_Elements(value) - 1)
   IF Keyword_Set(format) THEN theValue = String(value, Format=format) ELSE theValue = StrTrim(value,2)

      ; Initialize the base widget for the list widget.

   ok = self -> BASEWIDGET::INIT (parent, Column=1, Base_Align_Center=1, Frame=frame, _Extra=extrakeywords)
   IF NOT ok THEN Message, 'BaseWidget initialization failed.'

      ; The following is required because the LISTWIDGET is a compound object.
      ; Get the assigned event object(s). Replace this with the self object. This
      ; assures you that the events will go through the LISTWIDGET's EventHandler
      ; method first. Save the assigned event object(s) so it can be used at the end
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

      ; Do you need a title?

   IF N_Elements(title) NE 0 THEN BEGIN
      self._listtitle = Obj_New('LABELWIDGET', self, SCR_XSIZE=scr_xsize, $
         Value=title, /Dynamic_Resize, /ALIGN_CENTER, FONT=font)
   ENDIF

   IF N_Elements(ysize) EQ 0 THEN ysize = N_Elements(value)

      ; Create the list widget.

   self._listID = Widget_List(self._id, $
      CONTEXT_EVENTS=context_events, $
      MULTIPLE=multiple, $
      FONT=font, $
      VALUE=forwardSpace + theValue + trailingSpace, $
      SCR_XSIZE=scr_xsize, $
      SCR_YSIZE=scr_ysize, $
      UNITS=units, $
      XSIZE=xsize, $
      YSIZE=ysize)

      ; If this is a multiple widget, we may need an Apply button.

   IF multiple THEN BEGIN
      IF Keyword_Set(no_multi_button) EQ 0 THEN BEGIN
         IF N_Elements(multi_title) EQ 0 THEN multi_title = 'Apply'
         self._listbutton = Obj_New('BUTTONWIDGET', self, Value=multi_title, $
            /Dynamic_Resize, /ALIGN_CENTER, FONT=font, NAME='LISTBUTTON')
      ENDIF
   ENDIF

      ; Make sure the self object reference is stored in the UValue of the list widget.

   Widget_Control, self._listID, Set_UValue=self

      ; Set the current selection on the droplist.

   selection = Value[index]
   Widget_Control, self._listID, Set_list_Select=index

      ; Populate the object.
   self._clicks = clicks
   self._index = PTR_NEW(index)
   self._selection = Ptr_New(selection)
   self._value = Ptr_New(value)

   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       LISTWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the LISTWIDGET object class definition.
;       The LISTWIDGET object is subclassed from the BASEWIDGET object.
;
;*****************************************************************************************************
PRO ListWidget__DEFINE, class

      event = { LISTWIDGET_EVENT, $
                ID:Obj_New(), $            ; The widget object that caused the event.
                HANDLER:Obj_New(), $       ; The event handler object.
                EVENT_NAME: "", $          ; The name of the event, LISTWIDGET_EVENT.
                NAME: "", $                ; The name of the object.
                CLICKS: 0L, $              ; The number of clicks for selection. 1 or 2.
                INDEX: Ptr_New(), $        ; The index (or indices) of the current selection.
                NUM_SELECTED: 0L, $        ; The number of elements in the selection pointer.
                SELECTION:Ptr_New() $      ; The current list selection (i.e., value[index]).
              }

     class =  { LISTWIDGET, $              ; The LISTWIDGET object class definition.
                _clicks: 0L, $             ; The number of clicks required to generate events.
                _index: Ptr_New(), $       ; A pointer to the index or indices of the selections.
                _listID:0L, $              ; The list widget identifier.
                _listtitle: Obj_New(), $   ; The title object.
                _listbutton: Obj_New(), $  ; The button object.
                _multiple: 0L, $           ; A flag that indicates this list widget generates multiple selections.
                _selection: Ptr_New(), $   ; A pointer to the selections.
                _value: Ptr_New(), $       ; The values or selections on the list.
                _event_handler: Obj_New(), $ ; The real event handler object or objects for this compound object.
                _event_method_real: "", $  ; The event method assigned by the user to this object widget.
                INHERITS BASEWIDGET $
              }

END


