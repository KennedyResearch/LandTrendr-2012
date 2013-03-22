;*****************************************************************************************************
;+
; NAME:
;       DROPLISTWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to implement a droplist widget in an object
;       structure. This routine is a compound widget in order to give the droplist
;       more functionality than the current IDL droplist widget. In particular, it
;       is much easier to keep track of the current value of the droplist in this
;       implementation.
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
;       aDroplist = Obj_New("DROPLISTWIDGET", parent, VALUE=['pig', 'cow', coyote'], INDEX=2)
;
; SUPERCLASSES:
;
;       BASEWIDGET
;       WIDGETATOM
;       ATOM
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { DROPLISTWIDGET, $                ; The DROPLISTWIDGET object class definition.
;                _droplistID:0L, $             ; The droplist widget identifier.
;                _event_handler:Obj_New(), $   ; The assigned event handler object for the DROPLISTWIDGET.
;                _event_method_real: "", $     ; The event method assigned by the user to this object widget.
;                _index:0L, $                  ; The index number of the current selecton.
;                _title: "", $                 ; The droplist title.
;                _selection:Ptr_New(), $       ; The current droplist selection.
;                _value: Ptr_New(), $          ; The values or selections on the droplist.
;                INHERITS BASEWIDGET $
;            }
;
; EVENT_STRUCTURE:
;
;       This is the event structure sent to the parent widget object.
;
;   event  = { DROPLISTWIDGET_EVENT, $
;                ID:Obj_New(), $            ; The widget object that caused the event.
;                TOP: Obj_New(), $          ; The object at the top of the object hierarchy.
;                HANDLER:Obj_New(), $       ; The event handler object.
;                EVENT_NAME: "", $          ; The name of the event, DROPLISTWIDGET_EVENT.
;                NAME: "", $                ; The name of the object.
;                INDEX:0L, $                ; The index number of the current selection.
;                SELECTION:Ptr_New() $      ; The current droplist selection (i.e., value[index]).
;      }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W.Fanning, 23 July 2002.
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
;       DROPLISTWIDGET::EVENTHANDLER
;
; PURPOSE:
;
;       This is the event handler method of the object. The purpose is to set the
;       object's current selection and index number. A DROPLISTWIDGET event is
;       sent to the parent object's event handler.
;
; SYNTAX:
;
;       widgetObject -> EventHandler, event
;
; ARGUMENTS:
;
;       EVENT:  The event created by the droplist widget.
;-
;*****************************************************************************************************
PRO DroplistWidget::EventHandler, event

   @cat_pro_error_handler

      ; Set the current selection and index number.

   *self._selection = (*self._value)[event.index]
   self._index = event.index

      ; If we need to send an event, package the event up. Include both
      ; the index number (what normal droplist events produce), the current
      ; selection, and the self object reference.

   ; Check that there is at least one valid event object, otherwise swallow the event.
   eventObjs = self._event_handler -> Get(/All)
   thisEvent = {DROPLISTWIDGET_EVENT, self, Obj_New(), $
      'DROPLISTWIDGET_EVENT', self._name, self._index, self._selection}
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

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       DROPLISTWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to obtain the DroplistWidget object's properties
;
; SYNTAX:
;
;       aDroplistWidget -> GetProperty, Selection=currentSelection
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       EVENT_OBJECTS:   Normally, the EVENT_OBJECTS are obtained in the CATATOM object, but the
;                        DROPLISTWIDGET is compound object, so we have to intercept the process
;                        and get the event object that will *eventually* receive the DROPLISTWIDGET event.
;
;       DYNAMIC_RESIZE : Returns a 1 if the droplist is configued for dynamic resizing. Otherwise, 0.
;
;       INDEX:           Returns the index number of the current selection.
;
;       NUMBER:          Returns the number of selections available in the droplist.
;
;       SELECTION:       Returns the current selection in the droplist values.
;
;       VALUES:          Returns the current possible values or selections in the droplist.
;
;       _REF_EXTRA:      Any keywords appropriate for the superclass GetProperty methods.
;-
;*****************************************************************************************************
PRO DroplistWidget::GetProperty, $
   EVENT_OBJECTS=event_objects, $
   DYNAMIC_RESIZE=dynamic_resize, $
   INDEX=index, $
   NUMBER=number, $
   SELECTION=selection, $
   VALUES=values, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Call the superclass getproperty method if needed.

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> WidgetAtom::GetProperty, _EXTRA=extraKeywords

      ; Get the properties. If widget identifiers are going to be
      ; used, be sure to make sure they are valid IDs. Sometimes
      ; the GetProperty method can be called from within INIT methods
      ; and these widget indentifiers will not be property defined yet.

   IF Widget_Info(self._droplistID, /Valid_ID) THEN $
      dynamic_resize = Widget_Info(self._droplistID, /Dynamic_Resize)
   index = self._index
   IF Widget_Info(self._droplistID, /Valid_ID) THEN $
      number = Widget_Info(self._droplistID, /Droplist_Number)
   selection = self._selection
   values = self._value
   event_objects = self._event_handler

      ; Report completion.

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       DROPLISTWIDGET::RESIZE
;
; PURPOSE:
;
;       This method is used to dynamically resize the droplist.
;
; SYNTAX:
;
;       aDroplistWidget -> Resize, newsize
;
; ARGUMENTS:
;
;       newSize:      The new size of the droplist. If the variable newSize is
;                     not provided, the new size is set by the parent widget's
;                     X screen size. This makes it possible to have the droplist
;                     sized to fit it's parent base widget.
;
; KEYWORDS:
;
;       PARENT_SIZE:  Returns the SCR_XSIZE of the parent widget. (Output keyword)
;-
;*****************************************************************************************************
PRO DroplistWidget::Resize, newSize,  ParentSize=parentSize

   @cat_pro_error_handler

      ; Get the parent widget's geometry and X screen size.

   self._parent -> GetProperty, ID=parentID
   parentGeometry = Widget_Info(parentID, /Geometry)
   parentSize = parentGeometry.scr_xsize

      ; Has a size been provided? In not, use the parent's X screen size.

   IF N_Elements(newSize) EQ 0 THEN BEGIN
      newSize = parentGeometry.scr_xsize
   ENDIF

      ; Resize the droplist widget.

   Widget_Control, self._droplistID, Scr_XSize=newSize

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       DROPLISTWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the DroplistWidget object's properties
;
; SYNTAX:
;
;       aDroplistWidget -> SetProperty, Map=1
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DYNAMIC_RESIZE : Set this keyword to 1 to turn dynamic resizing on. To 0 to turn it off..
;
;       EVENT_OBJECTS:   Normally, the EVENT_OBJECTS are set by the CATATOM object, but the
;                        LISTWIDGET is compound object, so we have to intercept the process
;                        and set the event object that will *eventually* receive the LISTWIDGET event.
;
;       INDEX:           Set the current droplist selection to this index number.
;
;       SELECTION:       Set the current droplist selection to this selection. (An alternative
;                        to using the INDEX keyword.)
;
;       SPACES:          A one- or two-element array that indicates how many blank spaces to
;                        insert around the "values" of the droplist. The first element is the
;                        before-value spacing, the secound element is the after-value spacing.
;                        A single value adds the same spacing both before and after the value.
;
;       VALUE:           A vector of possible selections for the droplist. May be any data type.
;
;       _EXTRA:          Any keywords appropriate for the superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO DroplistWidget::SetProperty, $
   DYNAMIC_RESIZE=dynamic_resize, $
   EVENT_METHOD=event_method, $     ; Can't change EVENT_METHOD for compound objects.
   EVENT_OBJECTS=event_objects, $   ; Required to intercept keyword intended for CATATOM.
   INDEX=index, $
   SELECTION=selection, $
   SPACES=spaces, $
   VALUE=value, $
   _EXTRA=extraKeywords

   ; Error handling.
   @cat_pro_error_handler

   ; Spaces.
   IF N_Elements(spaces) EQ 0 THEN spaces = self._spaces
   IF N_Elements(spaces) EQ 2 THEN BEGIN
      IF spaces[0] LE 0 THEN forwardSpace = "" ELSE forwardSpace = String(Replicate(32B,spaces[0]))
      IF spaces[1] LE 0 THEN trailingSpace = "" ELSE trailingSpace = String(Replicate(32B,spaces[1]))
   ENDIF ELSE BEGIN
      IF spaces[0] LE 0 THEN forwardSpace = "" ELSE forwardSpace = String(Replicate(32B,spaces[0]))
      IF spaces[0] LE 0 THEN trailingSpace = "" ELSE trailingSpace = String(Replicate(32B,spaces[0]))
   ENDELSE

   IF N_Elements(value) NE 0 THEN BEGIN
      Ptr_Free, self.value
      self._value = Ptr_New(value)
      Widget_Control, self._droplistID, Set_Value=forwardSpace + value + trailingSpace
   ENDIF

   ; Dynamic Resizing?
   IF N_Elements(dynamic_resize) NE 0 THEN $
      Widget_Control, self._droplistID, Dynamic_Resize=Keyword_Set(dynamic_resize)

   ; Index?
   IF N_Elements(index) NE 0 THEN BEGIN

      self._index = 0 > index < (N_Elements(*self._value) - 1)

      ; Set the current selection.
      *self._selection = (*self._value)[index]

      ; Set the selection on the droplist widget.
      Widget_Control, self._droplistID, Set_Droplist_Select=self._index

   ENDIF

   ; Selection?
   IF N_Elements(selection) NE 0 THEN BEGIN

      index = Where(StrUpCase( Strtrim(*self._value,2) ) EQ StrUpCase( StrTrim(selection,2) ), count)

      ; If you can't find the selection, print an error message.
      IF count EQ 0 THEN Message, 'Cannot find the requested selection in the droplist.'

      ; Set the current index and selection for the object.
      self._index = 0 > index < (N_Elements(*self._value) - 1)
      *self._selection = selection

      ; Set the selection on the droplist widget.
      Widget_Control, self._droplistID, Set_Droplist_Select=self._index
   ENDIF

      ; The following is required because the DROPLISTWIDGET is a compound object.
      ; Get the assigned event object. Replace this with the self object. This
      ; assures you that the events will go through the DROPLISTWIDGET's EventHandler
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

  ; If there are more keywords, call the superclass SetProperty method.
   self -> WIDGETATOM::SetProperty, _Extra=extrakeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       DROPLISTWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the DROPLISTWIDGET object class destructor method.
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
PRO DroplistWidget::CLEANUP
   Ptr_Free, self._value
   Ptr_Free, self._selection

   ; Remove all the children in self._event_handler and destroy the container.
   self._event_handler -> Remove, /All
   Obj_Destroy, self._event_handler

   self->WIDGETATOM::Cleanup
END



;*****************************************************************************************************
;+
; NAME:
;       DROPLISTWIDGET::INIT
;
; PURPOSE:
;
;       This is the DROPLISTWIDGET object class initialization method
;
; SYNTAX:
;
;       aDropList = Obj_New('DROPLISTWIDGET')
;
; ARGUMENTS:
;
;       theParent:     An object reference to a WIDGETATOM-subclassed object.
;                      This is the same as using the PARENT keyword.
;
; KEYWORDS:
;
;       DYNAMIC_RESIZE: Set this keyword to enable dynamic resizing of the droplist.
;
;       FONT:           Set this keyword to the name of a font to use on the droplist.
;
;       FORMAT:         Set this keyword to a format for processing the "values" of the
;                       droplist. For example, VALUES=[4, 5], FORMAT='(F4.2)'.
;
;       FRAME:          Set this keyword to create a frame this many pixels wide around the droplist.
;
;       INDEX:          The index of the value that should be the current droplist selection.
;
;       PARENT:         An object reference to a WIDGETATOM-subclassed object.
;
;       LABEL_SIZE:     The scr_xsize of the "label" to the left of the droplist.
;
;       SCR_XSIZE:      Set the screen X size of the base to this many pixels. (Use discouraged.)
;
;       SCR_YSIZE:      Set the screen Y size of the base to this many pixels. (Use discouraged.)
;
;       SPACES:         A one- or two-element array that indicates how many blank spaces to
;                       insert around the "values" of the droplist. The first element is the
;                       before-value spacing, the secound element is the after-value spacing.
;                       A single value adds the same spacing both before and after the value.
;
;       TITLE:          A string that is the "title" of the droplist.
;
;       UNITS:          The units for measurments. The default is 0 for pixels. Other values are
;                       1 for inches, and 2 for centimeters.
;
;       VALUE:          A vector of possible selections for the droplist. May be any data type.
;
;       XSIZE:          The X size of the widget.
;
;       YSIZE:          The Y size of the widget.
;
;       _EXTRA:         Any keyword appropriate for the superclass INIT methods.
;-
;*****************************************************************************************************
FUNCTION DroplistWidget::INIT, parent, $
   DYNAMIC_RESIZE=dynamic_resize, $
   EVENT_METHOD=event_method, $   ; Required to intercept event method intended for CATATOM.
   EVENT_OBJECTS=event_objects, $ ; Required to intercept event objects intended for CATATOM.
   FONT=font, $
   FORMAT=format, $
   FRAME=frame, $
   INDEX=index, $
   LABEL_SIZE=label_size, $
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

   IF N_Elements(title) EQ 0 THEN title = 'Selection: '
   IF N_Elements(value) EQ 0 THEN value = ['Dog', 'Cat', 'Coyote']
   IF N_Elements(index) EQ 0 THEN index = 0
   IF N_Elements(spaces) EQ 0 THEN BEGIN
      spaces = [0,0]
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

      ; Initialize the base widget for the droplist widget.

   ok = self -> BASEWIDGET::INIT (parent, SCR_XSIZE=scr_xsize, SCR_YSIZE=scr_ysize, $
      Frame=frame, _Extra=extrakeywords, UNITS=units, Row=1)
   IF NOT ok THEN Message, 'BaseWidget initialization failed.'

      ; The following is required because the DROPLISTWIDGET is a compound object.
      ; Get the assigned event object. Replace this with the self object. This
      ; assures you that the events will go through the DROPLISTWIDGET's EventHandler
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

      ; Create a "label" for the droplist. I'm using a LABELWIDGET here
      ; because this facilitates sizing of droplists among other widgets.
      ; I subtract 4 from the input label size to account for widget margins.

   IF N_Elements(label_size) NE 0 THEN $
      label = Obj_New('LABELWIDGET', self, Value=title, Scr_XSize=label_size-4) ELSE $
      label = Obj_New('LABELWIDGET', self, Value=title)

      ; Create the droplist widget.

   self._droplistID = Widget_Droplist(self._id, $
      DYNAMIC_RESIZE=dynamic_resize, $
      FONT=font, $
      TITLE="", $
      VALUE=forwardSpace + theValue + trailingSpace, $
      XSIZE=xsize, $
      YSIZE=ysize, $
      UNITS=units, $
      _Extra=extrakeywords)

      ; Make sure the self object reference is stored in the UValue of the widget.

   Widget_Control, self._droplistID, Set_UValue=self

      ; Set the current selection on the droplist.

   selection = Value[index]
   Widget_Control, self._droplistID, Set_Droplist_Select=index

      ; Populate the object.

   self._index = index
   self._spaces = spaces
   self._title = title
   self._selection = Ptr_New(selection)
   self._value = Ptr_New(value)

   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       DROPLISTWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the DROPLISTWIDGET object class definition.
;       The DROPLISTWIDGET object is subclassed from the BASEWIDGET object.
;
;*****************************************************************************************************
PRO DroplistWidget__DEFINE, class

   event  = { DROPLISTWIDGET_EVENT, $
                ID:Obj_New(), $            ; The widget object that caused the event.
                HANDLER:Obj_New(), $       ; The event handler object.
                EVENT_NAME: "", $          ; The name of the event, DROPLISTWIDGET_EVENT.
                NAME: "", $                ; The name of the object.
                INDEX:0L, $                ; The index number of the current selection.
                SELECTION:Ptr_New() $      ; The current droplist selection (i.e., value[index]).
      }

   class = { DROPLISTWIDGET, $                ; The DROPLISTWIDGET object class definition.
                _droplistID:0L, $             ; The droplist widget identifier.
                _event_handler:Obj_New(), $   ; The real event handler object(s) for the DROPLISTWIDGET.
                _event_method_real: "", $     ; The event method assigned by the user to this object widget.
                _index:0L, $                  ; The index number of the current selecton.
                _title: "", $                 ; The droplist title.
                _selection:Ptr_New(), $       ; The current droplist selection.
                _value: Ptr_New(), $          ; The values or selections on the droplist.
                _spaces: IntArr(2), $         ; The spaces around the droplist selections.
                INHERITS BASEWIDGET $
            }
END

