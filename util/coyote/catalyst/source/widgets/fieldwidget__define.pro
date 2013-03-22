;*****************************************************************************************************
;+
; NAME:
;       FIELDWIDGET__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to create an object widget similar to FSC_FIELD
;       (which is itself similar to CW_FIELD).
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
;       theObject = Obj_New("FIELDWIDGET")
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
;   class = { FIELDWIDGET, $
;             _event_handler: Obj_New(), $ ; The real event handler object or objects for this compound object.
;             _event_method_real: "", $    ; The event method assigned by the user to this object widget.
;             cr_events: 0L, $             ; A flag meaning send carriage return events.
;             datatype: "" , $             ; The type of data to be returned from the text widget.
;             decimal: 0, $                ; The number of decimals points in FLOAT and DOUBLE numbers.
;             digits: 0, $                 ; The number of digits in INT and LONG numbers.
;             focus: 0L, $                 ; A flag to indicate focus events should be returned.
;             gentype: "", $               ; The "general" type of data: INTEGER, UNSIGNED, FLOAT, or STRING.
;             labelID: Obj_New(), $        ; The label widget ID.
;             tabnext: Obj_New(), $        ; The identifier of a widget to receive the cursor focus if a TAB character is detected.
;             textID: Obj_New(), $         ; The text widget ID.
;             theText: "", $               ; The actual text in the text widget.
;             theValue: Ptr_New(), $       ; The actual "value" of the text in the text widget. :-)
;             positive: 0, $               ; A flag meaning only positive numbers allowed.
;             undefined: Ptr_New(), $      ; The undefined data type.
;             INHERITS BASEWIDGET $
;           }
;
; MESSAGES:
;
;   None.
;
; EVENTS:
;
;   All events are handled internally unless the keywords CR_EVENTS or FOCUS_EVENTS are used.
;   If the CR_EVENTS keyword is used, the event structure returned to your Event Handler routine
;   is defined like this:
;
;       {ID:Obj_New(), TOP:Obj_New(), HANDLER:Obj_New(), NAME:self._name, $
;         EVENT_NAME:'FIELD_CARRIAGE_RETURN_EVENT', VALUE:Ptr_New(), TYPE:""}
;
;   If the FOCUS_EVENTS keyword is use, the event structure returned to your Event Handler routine
;   is defined like this, where ENTER is 0 if focus leaves the text widget and is 1 if focus enters
;   the text widget:
;
;       {ID:Obj_New(), TOP:Obj_New(), HANDLER:Obj_New(), NAME:self._name, $
;         EVENT_NAME:'FIELD_CARRIAGE_RETURN_EVENT', VALUE:Ptr_New(), TYPE:"", ENTER:0}
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 18 July 2004.
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
;        FIELDWIDGET::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the FIELDWIDGET object.
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
PRO FieldWidget::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Get the previous text, the current cursor location in the text widget,
   ; and indicate this is not a Carriage Return event.
   previousText = self.theText
   event.id -> GetProperty, TEXT_SELECT=textLocation
   cr_event = 0

   ; Is this a keyboard focus event? Then process it differently.
   IF self.focus AND (StrUpCase(event.event_name) EQ 'WIDGET_KBRD_FOCUS') THEN BEGIN

       ; Get the current contents of text widget. Validate it.
       self.textID -> GetProperty, Value=newText
       newText = newText[0]
       validText = self -> Validate(newText)

      ; Load the valid text.
      self.theText = validText
      testValue  = self -> ReturnValue(validText)
      IF String(testValue) EQ "NULLVALUE" THEN BEGIN
          Ptr_Free, self.theValue
          self.theValue = Ptr_New(/Allocate_Heap)
      ENDIF ELSE *self.theValue = testValue

      ; Get the top object.
      event.ID -> GetProperty, Top_Object=top_object

      ; Check that there is at least one valid event object, otherwise complain
      eventObjs = self._event_handler -> Get(/All)
      event_name = Tag_Names(event, /Structure_Name)
      thisEvent = {ID:self, TOP:top_object, HANDLER:eventObjs[0], NAME:self->GetName(), $
         EVENT_NAME:'FIELD_FOCUS_EVENT', VALUE:self.theValue, TYPE:self.dataType, ENTER:event.enter}
      IF (MAX (OBJ_VALID (eventObjs)) EQ 0) THEN $
         MESSAGE, 'No event handler for object ' + OBJ_CLASS (event.id) + ' named ' + self -> GetName()

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

      RETURN

   ENDIF

   ; What kind of event is this?
   possibleTypes = ['INSERT SINGLE CHARACTER', 'INSERT MULTIPLE CHARACTERS', 'DELETE TEXT', 'SELECT TEXT']
   thisType = possibleTypes[event.type]
   cr_event = 0 ; Will only return CR events from INSERT SINGLE CHARACTER events.

   ; Branch on event type.
   CASE thisType OF

      'INSERT SINGLE CHARACTER': BEGIN

            ; If the character is a TAB see if there is something to do.
            IF event.ch EQ 9B THEN BEGIN
               self -> MoveTab
               RETURN
            ENDIF

            ; Get the current contents of text widget. Validate it.
            self.textID -> GetProperty, Value=newText
            newText = newText[0]
            validText = self -> Validate(newText)

            ; If it is valid, leave it alone. If not, go back to previous text.
            IF validText NE newText THEN BEGIN

               self.textID -> SetProperty, Value=previousText, TEXT_SELECT=[textLocation[0]-1,0]

            ENDIF ELSE BEGIN

               self.theText = validText
               testValue  = self -> ReturnValue(validText)
               IF String(testValue) EQ "NULLVALUE" THEN BEGIN
                  Ptr_Free, self.theValue
                  self.theValue = Ptr_New(/Allocate_Heap)
               ENDIF ELSE *self.theValue = testValue

            ENDELSE

            ; Is this a Carriage Return event?
            IF event.ch EQ 10B then cr_event = 1

         ENDCASE

      'INSERT MULTIPLE CHARACTERS': BEGIN

               ; Same as above, but for all the characters you are inserting.

            self.textID -> GetProperty, Value=newText
            newText = newText[0]
            validText = self -> Validate(newText)
            IF validText NE newText THEN BEGIN
               self.textID -> SetProperty, Value=previousText, TEXT_SELECT=[textLocation[0]-1,0]
            ENDIF ELSE BEGIN
               self.theText = validText
               testValue  = self -> ReturnValue(validText)
               IF String(testValue) EQ "NULLVALUE" THEN BEGIN
                  Ptr_Free, self.theValue
                  self.theValue = Ptr_New(/Allocate_Heap)
               ENDIF ELSE *self.theValue = testValue
            ENDELSE
         ENDCASE

      'DELETE TEXT': BEGIN

               ; Get the current contents of text widget. Validate it.


            self.textID -> GetProperty, Value=newText
            newText = newText[0]
            validText = self -> Validate(newText)

               ; Load the valid text.

           self.textID -> SetProperty, Value=validText, TEXT_SELECT=[textLocation[0],0]
           self.theText = validText
           testValue  = self -> ReturnValue(validText)
           IF String(testValue) EQ "NULLVALUE" THEN BEGIN
               Ptr_Free, self.theValue
               self.theValue = Ptr_New(/Allocate_Heap)
           ENDIF ELSE *self.theValue = testValue

         ENDCASE

      'SELECT TEXT': ; Nothing to do.

   ENDCASE

   ; Report the event, if this is a Carriage Return.
   IF cr_event and self.cr_events THEN BEGIN

      ; Check that there is at least one valid event object, otherwise complain
      eventObjs = self._event_handler -> Get(/All)
      event_name = Tag_Names(event, /Structure_Name)
      thisEvent = {ID:self, HANDLER:eventObjs[0], NAME:self->GetName(), $
         EVENT_NAME:'FIELD_CARRAGE_RETURN_EVENT', VALUE:self.theValue, TYPE:self.dataType}
      IF (MAX (OBJ_VALID (eventObjs)) EQ 0) THEN $
         MESSAGE, 'No event handler for object ' + OBJ_CLASS (event.id) + ' named ' + self -> GetName()

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

   ENDIF

   ; Report completion
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       FIELDWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain FIELDWIDGET properties. Be sure
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
;   CR_EVENTS:          Indicates if carriage return events are passed to the parent.
;
;   DATATYPE:           The type of data displayed in the widget.
;
;   DECIMAL:            The number of digits to the right of the decimal in floating number.
;
;   DIGITS:             The number of digits permitted in integer numbers.
;
;   FOCUS_EVENTS:       Indicates if keyboard focus events are passed to the parent.

;   LABELSIZE:          The X screen size of the label widget.
;
;   TEXTSIZE:           The X screen size of the text widget.
;
;   VALUE:              The current value of the compound widget.
;
;   XSIZE:              The X size of the Text Widget.
;
;     _REF_EXTRA:       Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO FieldWidget::GetProperty, $
   CR_Events=cr_events, $
   DataType=datatype, $
   Decimal=decimal, $
   Digits=digits, $
   Focus_Events=focus_event, $
   LabelSize=labelsize, $
   TextID=textID, $
   TextSize=textsize, $
   Value=value, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Get the properties.
   cr_events = self.cr_events
   datatype = self.datatype
   decimal = self.decimal
   digits = self.digits
   IF Ptr_Valid(self.theValue) THEN value = *self.thevalue ELSE value = ""
   focus_event = self.focus

   IF Arg_Present(labelsize) THEN self.labelID -> GetProperty, SCR_XSIZE=labelSize
   IF Arg_Present(textID) THEN textID = self.textID
   IF Arg_Present(textsize) THEN self.textID -> GetProperty, SCR_XSIZE=textsize

   IF (N_Elements(extraKeywords) NE 0) THEN self -> BASEWIDGET::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       FIELDWIDGET::MOVETAB
;
; PURPOSE:
;
;       This method moves the focus to the next Inputwidget.
;
; SYNTAX:
;
;       theObject -> MoveTab
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
PRO FieldWidget::MoveTab

   IF Obj_Valid(self.nextTab) EQ 0 THEN RETURN
   self.nextTab -> GetProperty, TextID=textObj
   self.nextTab -> SetProperty, /Input_Focus
   self.nextTab -> GetProperty, Value=theText
   theText = theText[0]
   textObj -> SetProperty, TEXT_SELECT=[0,StrLen(theText)]

END


;*****************************************************************************************************
;+
; NAME:
;       FIELDWIDGET::RETURNVALUE
;
; PURPOSE:
;
;       This method takes a string and turns it into a number,depending upon the current data
;       type of the compound widget. For numbers, if the input value is a null string, then
;       an undefined variable is returned.
;
; SYNTAX:
;
;       A private method.
;
; ARGUMENTS:
;
;       inputValue:       A string value that is to be turned into a number.
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
FUNCTION FieldWidget::ReturnValue, inputValue

   ; Error handling to catch reading undefined pointer variables.
   ON_IOERROR, CatchIt

   CASE self.datatype OF
      'BYTE': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
         retValue = 'NULLVALUE' ELSE retValue = Fix(inputValue)
      'INT': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
         retValue = 'NULLVALUE' ELSE retValue = Fix(inputValue)
      'LONG': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
         retValue = 'NULLVALUE' ELSE retValue = Long(inputValue)
      'LONG64': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
         retValue = 'NULLVALUE' ELSE retValue = Long64(inputValue)
      'UINT': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
         retValue = 'NULLVALUE' ELSE retValue = UInt(inputValue)
      'ULONG': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
         retValue = 'NULLVALUE' ELSE retValue = ULong(inputValue)
      'ULONG64': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
         retValue = 'NULLVALUE' ELSE retValue = ULong64(inputValue)
      'FLOAT' : IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
         retValue = 'NULLVALUE' ELSE retValue = Float(inputValue)
      'DOUBLE': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
         retValue = 'NULLVALUE' ELSE retValue = Double(inputValue)
      'STRING' : retValue = inputValue
   ENDCASE

   RETURN, retValue

   ; If you got here, you must have a undefined pointer value.
   CatchIt:
      retValue = 'NULLVALUE'
      RETURN, retValue
END


;*****************************************************************************************************
;+
; NAME:
;       FIELDWIDGET::RESIZE
;
; PURPOSE:
;
;       This method resizes the widget by making the text widget fit the new size.
;
; SYNTAX:
;
;       theobject -> Resize, newsize
;
; ARGUMENTS:
;
;       newsize:          The new size of the compound text widget.
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
PRO InputWidge::Resize, newsize

   @cat_pro_error_handler

   self.labelID -> GetProperty, Scr_XSize=labelxsize
   self.textID -> SetProperty, Scr_XSize=(newsize - labelxsize)

END

;*****************************************************************************************************
;+
; NAME:
;       FIELDWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the FIELDWIDGET object's properties. Be sure
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
;   CR_EVENTS:          Set this keyword if you want Carriage Return events to be returned.
;
;   DECIMAL:            Set this keyword to the number of digits to the right of the decimal
;                       point in FLOATVALUE and DOUBLEVALUE numbers.
;
;   DIGITS:             Set this keyword to the number of digits permitted in INTERGERVALUE and LONGVALUE numbers.
;
;   FIELDFONT:          The font name for the text in the Text Widget.
;
;   FOCUS_EVENTS:       Set this keyword if you only want text events when the keyboard focus is
;                       moved in or out of the text widget.
;
;   INPUT_FOCUS:        Set this keyword to set input focus to/from the text widget.
;
;   LABELSIZE:          The X screen size of the Label Widget.
;
;   POSITIVE:           Set this keyword if you want only positive numbers allowed.
;
;   SCR_XSIZE:          The X screen size of the compound widget.
;
;   SCR_YSIZE:          The Y screen size of the compound widget.
;
;   SELECT:             Set this keyword to select the text in the Text Widget.
;
;   TABNEXT:            The inputwidget object to receive focus upon tabbing.
;
;   TEXTSIZE:           The X screen size of the Text Widget.
;
;   TITLE:              The text to go on the Label Widget.
;
;   VALUE:              The "value" of the compound widget.
;
;   XSIZE:              The X size of the Text Widget.
;
;     _EXTRA:           Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO FieldWidget::SetProperty, $
   CR_Events=cr_events, $
   Decimal=decimal, $
   Digits=digits, $
   Focus_Events=focus_events, $
   Input_Focus=input_focus, $
   LabelSize=labelsize, $
   Scr_XSize=scr_xsize, $
   Scr_YSize=scr_ysize, $
   Select=select, $
   Tabnext=tabnext, $
   TextSize=textsize, $
   Title=title, $
   Value=value, $
   XSize=xsize, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Set the properties, if needed.
   IF N_Elements(cr_events) NE 0 THEN self.cr_events = Keyword_Set(cr_events)
   IF Keyword_Set(decimal)THEN self.decimal = decimal
   IF Keyword_Set(digits)THEN self.digits = digits
   IF N_Elements(focus_events) NE 0 THEN self.focus = Keyword_Set(focus_events)
   IF N_Elements(input_focus) NE 0 THEN BEGIN
      self.textID -> SetProperty, Input_Focus=Keyword_Set(input_focus)
   ENDIF
   IF N_Elements(labelsize) NE 0 THEN BEGIN
      self.labelID -> SetProperty, XSize=labelsize
   ENDIF
   IF N_Elements(scr_xsize) NE 0 THEN BEGIN
      self.scr_xsize = scr_xsize
      self.textID -> SetProperty, Scr_XSize=scr_xsize
   ENDIF
   IF N_Elements(scr_ysize) NE 0 THEN BEGIN
      self.scr_ysize = scr_ysize
      self.textID -> SetProperty, Scr_YSize=scr_ysize
   ENDIF
   IF N_Elements(tabnext) NE 0 THEN self.nexttab = tabnext
   IF N_Elements(textsize) NE 0 THEN BEGIN
      self.textID -> SetProperty, XSize=textsize
   ENDIF
   IF N_Elements(title) NE 0 THEN self.labelID -> SetProperty, Set_Value=title
   IF N_Elements(xsize) NE 0 THEN BEGIN
      self.xsize = xsize
      self.textID -> SetProperty, Scr_XSize=xsize
   ENDIF

   IF N_Elements(value) NE 0 THEN BEGIN
      theText = StrTrim(value, 2)
      theText = self -> Validate(theText)
      self.theText = theText
      *self.theValue = self -> ReturnValue(self.theText)
      self.textID -> SetProperty, Value=self.theText
      self.textID -> SetProperty, TEXT_SELECT=[StrLen(theText),0]
   ENDIF

   IF N_Elements(select) NE 0 THEN BEGIN
      IF Keyword_Set(select) THEN self.textID -> SetProperty, Text_Select=[0,Strlen(self.theText)] ELSE $
         self.textID -> SetProperty, Text_Select=[0,0]
   ENDIF

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> BASEWIDGET::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       FIELDWIDGET::VALIDATE
;
; PURPOSE:
;
;       This function eliminates illegal characters from a string that represents
;       a number. The return value is a properly formatted string that can be turned into
;       an INT, LONG, FLOAT, or DOUBLE value.
;
;
; SYNTAX:
;
;       This is a private method.
;
; NOTES:
;
;      + 43B
;      - 45B
;      . 46B
;      0 - 9 48B -57B
;      'eEdD' [101B, 69B, 100B, 68B]
;
; ARGUMENTS:
;
;       value:          The input value to validate.
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
FUNCTION FieldWidget::Validate, value

  ; Error handling.
   @cat_func_error_handler

      ; A null string should be returned at once.

   IF N_Elements(value) EQ 0 THEN value = ""
   value = value[0]
   IF value EQ "" THEN RETURN, String(value)

      ; No leading or trailing blank characters to evaluate.

   value = StrTrim(value, 2)

      ; A string value should be returned at once. Nothing to check.

   IF StrUpCase(self.gentype) EQ 'STRING' THEN RETURN, String(value)

      ; Check integers and longs. A "-" or "+" in the first character is allowed. Otherwise,
      ; only number between 0 and 9, or 43B to 57B.

   IF self.gentype EQ 'INTEGER' THEN BEGIN

      returnValue = Ptr_New(/Allocate_Heap)
      asBytes = Byte(value)

      IF self.positive THEN BEGIN
         IF (asBytes[0] EQ 43B) OR $
            (asBytes[0] GE 48B AND asBytes[0] LE 57B) THEN *returnValue = [asBytes[0]]
      ENDIF ELSE BEGIN
         IF (asBytes[0] EQ 45B) OR (asBytes[0] EQ 43B) OR $
            (asBytes[0] GE 48B AND asBytes[0] LE 57B) THEN *returnValue = [asBytes[0]]
      ENDELSE
      length = StrLen(asBytes)
      IF length EQ 1 THEN BEGIN
         IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [32B] ELSE $
               *returnValue = [asBytes[0]]
      ENDIF ELSE BEGIN
         FOR j=1,length-1 DO BEGIN
            IF (asBytes[j] GE 48B AND asBytes[j] LE 57B) THEN BEGIN
               IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [asBytes[j]] ELSE $
                  *returnValue = [*returnValue, asBytes[j]]
            ENDIF
         ENDFOR
     ENDELSE
     IF N_Elements(*returnValue) NE 0 THEN retValue = String(*returnValue) ELSE retValue = ""
     Ptr_Free, returnValue

         ; Check for digit restrictions.

     IF self.digits GT 0 THEN BEGIN
         retValue = StrTrim(retValue, 2)
         IF StrMid(retValue, 0, 1) EQ "-" THEN digits = self.digits + 1 ELSE digits = self.digits
         retValue = StrMid(retValue, 0, digits)
     ENDIF

     RETURN, retValue

   ENDIF

      ; Check unsigned data types.

   IF self.gentype EQ 'UNSIGNED' THEN BEGIN

      returnValue = Ptr_New(/Allocate_Heap)
      asBytes = Byte(value)

      IF self.positive THEN BEGIN
         IF (asBytes[0] GE 48B AND asBytes[0] LE 57B) THEN *returnValue = [asBytes[0]]
      ENDIF ELSE BEGIN
         IF (asBytes[0] GE 48B AND asBytes[0] LE 57B) THEN *returnValue = [asBytes[0]]
      ENDELSE
      length = StrLen(asBytes)
      IF length EQ 1 THEN BEGIN
         IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [32B] ELSE $
               *returnValue = [asBytes[0]]
      ENDIF ELSE BEGIN
         FOR j=1,length-1 DO BEGIN
            IF (asBytes[j] GE 48B AND asBytes[j] LE 57B) THEN BEGIN
               IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [asBytes[j]] ELSE $
                  *returnValue = [*returnValue, asBytes[j]]
            ENDIF
         ENDFOR
     ENDELSE
     IF N_Elements(*returnValue) NE 0 THEN retValue = String(*returnValue) ELSE retValue = ""
     Ptr_Free, returnValue

         ; Check for digit restrictions.

     IF self.digits GT 0 THEN BEGIN
         retValue = StrTrim(retValue, 2)
         digits = self.digits
         retValue = StrMid(retValue, 0, digits)
     ENDIF

     RETURN, retValue

   ENDIF

      ; Check floating and double values. (+,-) in first character or after 'eEdD'.
      ; Only numbers, signs, decimal points, and 'eEdD' allowed.

   IF self.gentype EQ 'FLOAT' THEN BEGIN
      returnValue = Ptr_New(/Allocate_Heap)
      asBytes = Byte(value)
      IF self.positive THEN BEGIN
         IF (asBytes[0] EQ 43B) OR $
            (asBytes[0] GE 48B AND asBytes[0] LE 57B) OR $
            (asBytes[0] EQ 46B) THEN *returnValue = [asBytes[0]]
         IF (asBytes[0] EQ 46B) THEN haveDecimal = 1 ELSE haveDecimal = 0
      ENDIF ELSE BEGIN
         IF (asBytes[0] EQ 45B) OR (asBytes[0] EQ 43B) OR $
            (asBytes[0] GE 48B AND asBytes[0] LE 57B) OR $
            (asBytes[0] EQ 46B) THEN *returnValue = [asBytes[0]]
         IF (asBytes[0] EQ 46B) THEN haveDecimal = 1 ELSE haveDecimal = 0
      ENDELSE
      haveExponent = 0
      length = StrLen(asBytes)
      prevByte = asBytes[0]
      exponents = Byte('eEdD')
      IF length EQ 1 THEN BEGIN
         IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [32B] ELSE $
               *returnValue = [asBytes[0]]
      ENDIF ELSE BEGIN
         FOR j=1,length-1 DO BEGIN
            IF (asBytes[j] GE 48B AND asBytes[j] LE 57B) THEN BEGIN
               IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [asBytes[j]] ELSE $
                  *returnValue = [*returnValue, asBytes[j]]
               prevByte = asBytes[j]
            ENDIF ELSE BEGIN

               ; What kind of thing is it?

               IF (asBytes[j] EQ 46B) THEN BEGIN ; A decimal point.
                  IF haveDecimal EQ 0 THEN BEGIN
                     *returnValue = [*returnValue, asBytes[j]]
                     haveDecimal = 1
                     prevByte = asBytes[j]
                  ENDIF
               ENDIF

               IF (asBytes[j] EQ 45B) OR (asBytes[j] EQ 43B) THEN BEGIN ; A + or - sign.
                  index = Where(exponents EQ prevByte, count)
                  IF count EQ 1 AND haveExponent THEN BEGIN
                     *returnValue = [*returnValue, asBytes[j]]
                     haveDecimal = 1
                     prevByte = asBytes[j]
                  ENDIF
               ENDIF

               index = Where(exponents EQ asBytes[j], count)
               IF count EQ 1 AND haveExponent EQ 0 THEN BEGIN ; An exponent
                  *returnValue = [*returnValue, asBytes[j]]
                  haveExponent = 1
                  prevByte = asBytes[j]
               ENDIF
            ENDELSE
         ENDFOR
      ENDELSE
         IF N_Elements(*returnValue) NE 0 THEN BEGIN

         retValue = String(*returnValue)
         retValue = StrTrim(retValue, 2)

                  ; Check for decimal restrictions

         IF self.decimal GE 0 THEN BEGIN
            theDecimalPt = StrPos(retValue, '.')
            IF theDecimalPt NE -1 THEN retValue = StrMid(retValue, 0, theDecimalPt + self.decimal + 1)
         ENDIF

      ENDIF ELSE retValue = ""
      Ptr_Free, returnValue

         ; Is this a representable number?

      testValue = self->ReturnValue(retValue)
      IF String(testValue) NE 'NULLVALUE' THEN numCheck = Finite(testValue) ELSE numCheck = 1
      IF numCheck THEN BEGIN
         RETURN, retValue
      ENDIF ELSE BEGIN
         ok = Dialog_Message('The requested number is not representable.')
         RETURN, ""
      ENDELSE
   ENDIF
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       FIELDWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the FIELDWIDGET object class destructor method.
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
PRO FieldWidget::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self.theValue
   Ptr_Free, self.undefined
   self._event_handler -> Remove, /All
   Obj_Destroy, self._event_handler
   self -> BASEWIDGET::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       FIELDWIDGET::INIT
;
; PURPOSE:
;
;       This is the FIELDWIDGET object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;   COLUMN:             Set this keyword to have the Label Widget above the Text Widget.
;
;   CR_EVENTS:          Set this keyword if you only want Carriage Return events to be returned.
;
;   DECIMAL:            Set this keyword to the number of digits to the right of the decimal
;                       point in FLOATVALUE and DOUBLEVALUE numbers.
;
;   DIGITS:             Set this keyword to the number of digits permitted in INTERGERVALUE and LONGVALUE numbers.
;
;   FIELDFONT:          The font name for the text in the Text Widget.
;
;   FOCUS_EVENTS:       Set this keyword if you want text events when the keyboard focus is
;                       moved out of the text widget.
;
;   FRAME:              Set this keyword to put a frame around the compound widget.
;
;   LABEL_LEFT:         Set this keyword to align the label to the left of the label.
;
;   LABEL_RIGHT:        Set this keyword to align the label to the right of the label.
;
;   LABELFONT:          The font name for the text in the Label Widget.
;
;   LABELSIZE:          The X screen size of the Label Widget.
;
;   NOEDIT:             Set this keyword to make the text widget non-editable.
;
;   POSITIVE:           Set this keyword if you want only positive numbers allowed.
;
;   ROW:                Set this keyword to have the Label beside the Text Widget. (The default.)
;
;   SCR_XSIZE:          The X screen size of the compound widget.
;
;   SCR_YSIZE:          The Y screen size of the compound widget.
;
;   TITLE:              The text to go on the Label Widget.
;
;   VALUE:              The "value" of the compound widget.
;
;   XSIZE:              The X size of the text widget in the usual character units
;
;    _EXTRA:            Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION FieldWidget::INIT, parent, $
   Column=column, $
   CR_Events=cr_events, $
   Decimal=decimal, $
   Digits=digits, $
   FieldFont=fieldfont, $
   Focus_Events=focus_events, $
   Frame=frame, $
   IntegerValue=integervalue, $
   Label_Left=label_left, $
   Label_Right=label_right, $
   LabelFont=labelfont, $
   LabelSize=labelsize, $
   LongValue=longvalue, $
   NoEdit=noedit, $
   Positive=positive, $
   Row=row, $
   Scr_XSize=scr_xsize, $
   Scr_YSize=scr_ysize, $
   Title=title, $
   Value=value, $
   XSize=xsize, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

  ; If there is a parent, make sure it is a valid BASEWIDGET class object.

   IF OBJ_ISA_VALID(parent, "BASEWIDGET") EQ 0 THEN Message, 'Parent object invalid or not of type BASEWIDGET.'

   ; Check for presence of keywords.
   IF N_Elements(column) EQ 0 THEN column = 0
   IF N_Elements(digits) EQ 0 THEN digits = 0 ELSE digits = Fix(digits)
   IF N_Elements(decimal) EQ 0 THEN decimal = -1 ELSE decimal = Fix(decimal)
   IF N_Elements(fieldfont) EQ 0 THEN fieldfont = ""
   IF N_Elements(frame) EQ 0 THEN frame = 0
   IF N_Elements(labelfont) EQ 0 THEN labelfont = ""
   IF N_Elements(labelsize) EQ 0 THEN labelsize = 0
   IF N_Elements(scr_xsize) EQ 0 THEN scr_xsize = 0
   IF N_Elements(scr_ysize) EQ 0 THEN scr_ysize = 0
   IF N_Elements(title) EQ 0 THEN title = "Input Value: "
   IF N_Elements(uvalue) EQ 0 THEN uvalue = ""
   IF N_Elements(value) EQ 0 THEN value = ""
   IF N_Elements(xsize) EQ 0 THEN xsize = 0
   IF N_Elements(row) EQ 0 AND column EQ 0 THEN row = 1

   ; What data type are we looking for?

   dataType = Size(value, /TNAME)
   CASE dataType OF
      'BYTE'   : BEGIN
         genType = 'INTEGER'
         dataType = 'INT'
         positive = 1
         value = Fix(value)
         Message, 'BYTE data not supported. Value will be converted to INT.', /Informational
         END
      'INT'    : genType = 'INTEGER'
      'LONG'   : genType = 'INTEGER'
      'LONG64' : genType = 'INTEGER'
      'UINT'   : genType = 'UNSIGNED'
      'ULONG'  : genType = 'UNSIGNED'
      'ULONG64': genType = 'UNSIGNED'
      'FLOAT'  : genType = 'FLOAT'
      'DOUBLE' : genType = 'FLOAT'
      'STRING' : genType = 'STRING'
      ELSE     : genType = 'UNDEFINED'
   ENDCASE

   IF N_Elements(undefined) EQ 0 THEN BEGIN
      IF genType EQ 'STRING' THEN undefined = "" ELSE undefined = !VALUES.F_NAN
   ENDIF
   self.undefined = Ptr_New(undefined)

   ; Initialize the base widget for the list widget.
   ok = self -> BASEWIDGET::INIT (parent, $
      Frame=frame, $
      Row=row, $
      Column=column, $
      Base_Align_Center=1, $
      _Extra=extrakeywords)
   IF NOT ok THEN Message, 'BaseWidget initialization failed.'

   ; Populate the object.
   self.cr_events = Keyword_Set(cr_events)
   self.datatype = datatype
   self.decimal = decimal
   self.digits = digits
   self.focus = Keyword_Set(focus_events)
   self.positive = Keyword_Set(positive)
   self.gentype = gentype

  ; The following is required because the FIELDWIDGET is a compound object.
  ; Get the assigned event object(s). Replace this with the self object. This
  ; assures you that the events will go through the FIELDWIDGET's EventHandler
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

   ; Validate the input value.
   theText = StrTrim(value, 2)
   theText = self -> Validate(theText)
   self.theText = theText

   ; Create the input widget.
   self.labelID = Obj_New('LabelWidget', self, Value=title, Font=labelfont, $
      Align_Left=label_left, Align_Right=label_right, Scr_XSize=labelsize)

   self.textID = Obj_New('TextWidget', self, $  ; The Text Widget.
      Value=theText, $
      XSize=xsize, $
      YSize=1, $
      Scr_XSize=scr_xsize, $
      Scr_YSize=scr_ysize, $
      Font=fieldfont, $
      All_Events=1, $
      KBRD_FOCUS_EVENTS=Keyword_Set(focus_events), $
      Editable=1-Keyword_Set(noedit) )

   ; Set the actual return value of the compound widget.
   self.theValue = Ptr_New(self -> ReturnValue(theText))

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       FIELDWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the FIELDWIDGET object.
;
;*****************************************************************************************************
PRO FieldWidget__DEFINE, class

   class = { FIELDWIDGET, $
             _event_handler: Obj_New(), $ ; The real event handler object or objects for this compound object.
             _event_method_real: "", $    ; The event method assigned by the user to this object widget.
             cr_events: 0L, $             ; A flag meaning to send carriage return events to the real event handler.
             datatype: "" , $             ; The type of data to be returned from the text widget.
             decimal: 0, $                ; The number of decimals points in FLOAT and DOUBLE numbers.
             digits: 0, $                 ; The number of digits in INT and LONG numbers.
             focus: 0L, $                 ; A flag to indicate focus events should be returned.
             gentype: "", $               ; The "general" type of data: INTEGER, UNSIGNED, FLOAT, or STRING.
             labelID: Obj_New(), $        ; The label widget ID.
             nextTab: Obj_New(), $        ; The identifier of a widget to receive the cursor focus if a TAB character is detected.
             textID: Obj_New(), $         ; The text widget ID.
             theText: "", $               ; The actual text in the text widget.
             theValue: Ptr_New(), $       ; The actual "value" of the text in the text widget. :-)
             positive: 0, $               ; A flag meaning only positive numbers allowed.
             undefined: Ptr_New(), $      ; The undefined data type.
             INHERITS BASEWIDGET $
           }

END


