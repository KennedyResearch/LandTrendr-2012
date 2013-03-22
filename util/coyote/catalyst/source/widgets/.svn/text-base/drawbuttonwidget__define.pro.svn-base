;*****************************************************************************************************
;+
; NAME:
;       DRAWBUTTONWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to construct a button widget from a draw widget.
;       The button is constucted using the system colors of the user's computer, so it
;       could look differently on each individual's computer.
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
;       Object compound widgets.
;
; SYNTAX:
;
;       buttonWidget = Obj_New("DrawButtonWidget", theParent)
;
; SUPERCLASSES:
;
;       DRAWWIDGET
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; EVENT_STURCTURE:
;
;   event  = { DRAWBUTTONWIDGET_EVENT, $
;                ID:Obj_New(), $            ; The widget object that caused the event.
;                TOP: Obj_New(), $          ; The object at the top of the object hierarchy.
;                HANDLER:Obj_New(), $       ; The event handler object.
;                EVENT_NAME: "", $          ; The name of the event. (DRAWBUTTONWIDGET_EVENT)
;                NAME: "", $                ; The name of the DrawButtonWidget object.
;                SELECT:0L $                ; Always set to 1 for button events.
;            }
;
; CLASS_STRUCTURE:
;
;   class = { DRAWBUTTONWIDGET, $
;             colors: Obj_New(), $         ; A SystemColors object for drawing colors.
;             event_handler:Obj_New(), $   ; The real event handler object(s) for the DROPLISTWIDGET.
;             event_method_real: "", $     ; The event method assigned by the user to this object widget.
;             fontname: "", $              ; The name of a true-type font to use for button text.
;             fontsize: 0.0, $             ; The font size of button text.
;             fonttype: 0L, $              ; The type of font you want (e.g, !P.Font).
;             offcenter: 0L, $             ; An offset from center of widget.
;             pixmap: Obj_New(), $         ; The pixmap for buffering output.
;             selected: 0L, $              ; A flag that indicates if the button has a selected (1) or unselected (0) appearance.
;             sensitive: 0L, $             ; A flag that indicates if the button is sensitive (1) or insensitive (0).
;             xsize: 0L, $                 ; The X size of the widget.
;             ysize: 0L, $                 ; The Y size of the widget.
;             value: "", $                 ; The button value (text).
;             INHERITS DrawWidget $
;           }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W.Fanning, 21 January 2004.
;       Added NORELEASE keyword to INIT and SETPROPERTY methods. 10 May 2004. DWF.
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
;       DRAWBUTTONWIDGET::ADD
;
; PURPOSE:
;
;       This method is a dummy ADD method that ensures nothing can be added to this object.
;       This object is meant to be a terminal object.
;
; SYNTAX:
;
;       Not used.
;
; ARGUMENTS:
;
;       object: The object to add to the container.
;
; KEYWORDS:
;
;       _EXTRA: Any keyword appropriate for the superclass ADD methods.
;
;-
;*****************************************************************************************************
PRO DrawButtonWidget::Add, object, _Extra=extraKeywords

   @cat_pro_error_handler

   IF Obj_Valid(object) EQ 0 THEN RETURN
   Message, 'Objects cannot be added to the DrawButtonWidget container.

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       DRAWBUTTONWIDGET::DRAW
;
; PURPOSE:
;
;       Draws the button appearance in the draw widget. Superclass DRAW methods are NOT called,
;       since this is a terminal draw event.
;
; SYNTAX:
;
;       buttonObject -> Draw
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       _EXTRA: Any keyword appropriate for the WIDGETATOM::DRAW method.
;-
;*****************************************************************************************************
PRO DrawButtonWidget::Draw

   @cat_pro_error_handler

   ; Get your colors organized.
   c = self.colors -> GetColors()

   ; Do the drawing in the pixmap.
   self.pixmap -> SetWindow

   ; Set up paramters.
   x1 = 5
   x2 = self.xsize - 5
   y1 = 5
   y2 = self.ysize - 5
   xs = self.xsize
   ys = self.ysize
   buttonx = [x1, x1, x2, x2, x1]
   buttony = [y1, y2, y2, y1, y1]
   lightx = [0, 0, xs, x2, x1, x1, 0]
   lighty = [0, ys, ys, y2, y2, y1,0]
   darkx = [0, x1, x2, x2, xs, xs, 0]
   darky = [0, y1, y1, y2, ys,  0, 0]

   ; Drawing on 24-bit devices.
   Device, Decomposed=1, Get_Decomposed=theState

   ; Are you in selected or de-selected state?
   CASE self.selected OF

      0: BEGIN
         PLOTS, [0, xs, xs, 0, 0], [0, 0, ys, ys, 0], /Device, Color=c.shadow
         POLYFILL, buttonx, buttony, Color=c.active, /Device
         POLYFILL, darkx, darky, Color=c.shadow, /Device
         POLYFILL, lightx, lighty, Color=c.edge, /Device
         PLOTS, buttonx, buttony, Color=c.shadow, /Device
         PLOTS, [0, x1], [0, y1], Color=c.shadow, /Device
         PLOTS, [x1, 0], [y2, ys], Color=c.shadow, /Device
         PLOTS, [x2, xs], [y2, ys], Color=c.shadow, /Device
         PLOTS, [x2, xs], [y1, 0], Color=c.shadow, /Device
         END

      1: BEGIN
         PLOTS, [0, xs, xs, 0, 0], [0, 0, ys, ys, 0], /Device, Color=c.edge
         POLYFILL, buttonx, buttony, Color=c.edge, /Device
         POLYFILL, darkx, darky, Color=c.edge, /Device
         POLYFILL, lightx, lighty, Color=c.shadow, /Device
         PLOTS, buttonx, buttony, Color=c.shadow, /Device
         PLOTS, [0, x1], [0, y1], Color=c.shadow, /Device
         PLOTS, [x1, 0], [y2, ys], Color=c.edge, /Device
         PLOTS, [x2, xs], [y2, ys], Color=c.shadow, /Device
         PLOTS, [x2, xs], [y1, 0], Color=c.shadow, /Device
         END

   ENDCASE

   ; Select the font color based on the current state of the button.
   IF self.selected THEN fontcolor = c.selected ELSE fontcolor = c.text
   IF self.sensitive EQ 0 THEN fontcolor = c.shadow

   ; Are we using true-type fonts?
   IF (self.fontname NE '') AND (self.fonttype EQ 2)THEN Device, Set_Font=self.fontname, /TT_Font

   PLOTS, [ 0.0,  0.0, 0.99], [0.0, 0.99, 0.99], /Normal, Color=c.edge

   ; Add the text to this button.
   XYOUTS, xs/2.0, ys/2.0 - self.offcenter, /Device, self.value, Alignment=0.5, Color=fontColor, Font=self.fonttype

   ; Clean up.
   Device, Decomposed=theState

   ; Copy button from from the pixmap to the display window.
   self -> SetWindow
   self.pixmap -> Draw

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       DRAWBUTTONWIDGET::EVENTHANDLER
;
; PURPOSE:
;
;       This is the event handler method of the object. The purpose is to send an event to the
;       real event handler on a button UP event that is within the confines of the button.
;       A DRAWBUTTONWIDGET event is sent to the parent object's event handler.
;
; SYNTAX:
;
;       widgetObject -> EventHandler, event, object
;
; ARGUMENTS:
;
;       EVENT:  The event created by the droplist widget.
;
;       OBJECT: The object reference of the widget object causing the event.
;-
;*****************************************************************************************************
PRO DrawButtonWidget::EventHandler, event, object

   @cat_pro_error_handler

   ; No events if button is insensitive.
   IF self.sensitive EQ 0 THEN RETURN

   ; Is this a widget tracking event?
   tags = Tag_Names(event)
   index = Where(tags EQ 'ENTER', count)
   IF count GT 0 THEN BEGIN

      ; Check that there is at least one valid event object, otherwise swallow the event.
      eventObjs = self.event_handler -> Get(/All)
      IF (MAX (OBJ_VALID (eventObjs)) EQ 0) THEN RETURN ; No valid event object.

      ; Find out the target object for the event, get the event method from
      ; the target object, and send the event to this method, UNLESS this
      ; object has its own object method.
      FOR e = 0, N_ELEMENTS (eventObjs) - 1 DO $
      BEGIN
         IF OBJ_VALID (eventObjs [e]) THEN $
         BEGIN
            event.handler = eventObjs [e]
            eventObjs[e] -> CatAtom::GetProperty, Event_Method=event_method
            IF (self.event_method_real NE "") AND (e EQ 0) THEN $
               thisMethod = self.event_method_real ELSE thisMethod = event_method
            Call_Method, thisMethod, eventObjs[e], event
         ENDIF
      ENDFOR
      RETURN
   ENDIF

   ; If this is a button UP event and you are inside the draw widget, it
   ; is a valid button event. If this is a button DOWN event, change the
   ; appearance of the button to "selected"

   CASE event.type OF
      0: BEGIN ; button DOWN event

            ; Button should appear selected.
            self.currentSelect = self.selected
            self.selected = 1

            ; Draw the button.
            self -> Draw

         END

      1: BEGIN ; button UP event

            ; Are you inside the extent of the draw widget?
            IF (event.x GT self.xsize) OR (event.x LT 0) THEN BEGIN
               self.selected = self.currentSelect
               self -> Draw
               RETURN
            ENDIF
            IF (event.y GT self.ysize) OR (event.y LT 0) THEN BEGIN
               self.selected = self.currentSelect
               self -> Draw
               RETURN
            ENDIF

            ; Button should NOT appear selected unless it was originally selected.
            IF self.norelease EQ 0 THEN self.selected = 0
            IF self.currentSelect EQ 1 THEN self.selected = 1

            ; Draw the button.
            self -> Draw

            ; If the button was originally selected, there is no need to send a button
            ; event for another selection event. You can simply RETURN.
            IF self.currentSelect EQ 1 THEN RETURN

            ; If we need to send an event, package the event up. Include both
            ; the index number (what normal droplist events produce), the current
            ; selection, and the self object reference.

            self -> GetProperty, Name=thisName

            ; Check that there is at least one valid event object, otherwise swallow the event.
            eventObjs = self.event_handler -> Get(/All)
            thisEvent = {DRAWBUTTONWIDGET_EVENT, self, Obj_New(), 'DRAWBUTTONWIDGET_EVENT', thisName, 1}
            IF (MAX (OBJ_VALID (eventObjs)) EQ 0) THEN RETURN ; No valid event object.

            ; Find out the target object for the event, get the event method from
            ; the target object, and send the event to this method, UNLESS this
            ; object has its own object method.
            FOR e = 0, N_ELEMENTS (eventObjs) - 1 DO $
            BEGIN
               IF OBJ_VALID (eventObjs [e]) THEN $
               BEGIN
                  thisEvent.handler = eventObjs [e]
                  eventObjs[e] -> CatAtom::GetProperty, Event_Method=event_method
                  IF (self.event_method_real NE "") AND (e EQ 0) THEN $
                     thisMethod = self.event_method_real ELSE thisMethod = event_method
                  Call_Method, thisMethod, eventObjs[e], thisEvent
               ENDIF
            ENDFOR

            END

      ELSE:
   ENDCASE

   ; Done.
IF Obj_Valid(self) THEN  self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       DRAWBUTTONWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to get the object's properties.
;
; SYNTAX:
;
;       buttonObject -> GetProperty, VALUE=buttonValue
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       FONTNAME:       The name of the font to used for button text.
;
;       FONTSIZE:       The size of the font. .
;
;       FONTTYPE:       The type of font used for button text. (-1 Hershey, 0 Hardware, 1 True-Type)..
;
;       OFFCENTER:      The number of pixels in Y direction that text is offset.
;
;       SELECTED:       Set if the button has a selected appearance.
;
;       SENSITIVE:      Set if the button is sensitive.
;
;       VALUE:          The text value of the button.
;
;       _REF_EXTRA:     Any keyword appropriate for the GETPROPERTY method of superclass objects.
;-
;*****************************************************************************************************
PRO DrawButtonWidget::GetProperty, $
   FONTNAME=fontname, $
   FONTSIZE=fontsize, $
   FONTTYPE=fonttype, $
   OFFCENTER=offcenter, $
   SELECTED=selected, $
   SENSITIVE=sensitive, $
   VALUE=value, $
   _REF_EXTRA=extrakeywords

   @cat_pro_error_handler

   IF Arg_Present(fontname) THEN fontname = self.fontname
   IF Arg_Present(fontsize) THEN fontsize = self.fontsize
   IF Arg_Present(fonttype) THEN fonttype = self.fonttype
   IF Arg_Present(offcenter) THEN offcenter = self.offcenter
   IF Arg_Present(selected) THEN selected = self.selected
   IF Arg_Present(sensitive) THEN sensitive = self.sensitive
   IF Arg_Present(value) THEN value = self.value

   IF N_Elements(extraKeywords) GT 0 THEN self -> DrawWidget::GetProperty, _Extra=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       DRAWBUTTONWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the object's properties.
;
; SYNTAX:
;
;       buttonObject -> SetProperty, VALUE=buttonValue
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DRAW:           Set this keyword if you wish to draw the button after properties have been set.
;
;       FONTNAME:       The name of the font to use for button text. Only valid if FONTTYPE=1.
;
;       FONTSIZE:       The size of the font. Not valid with FONTTYPE=0.
;
;       FONTTYPE:       The type of font to use. (-1 Hershey, 0 Hardware, 1 True-Type).
;
;       NORELEASE:      If this keyword is set, the button will assume a SELECTED appearance upon
;                       selection.
;
;       OFFCENTER:      The number of pixels in Y direction that text should be offset.
;
;       SELECTED:       Set this keyword to give the button a selected appearance.
;
;       SENSITIVE:      Set this keyword to make the button sensitive (1) or insensitive (0).
;
;       VALUE:          The initial text value of the button.
;
;       _EXTRA:         Any keyword appropriate for the SETPROPERTY method of superclass objects.
;-
;*****************************************************************************************************
PRO DrawButtonWidget::SetProperty, $
   DRAW=draw, $
   FONTNAME=fontname, $
   FONTSIZE=fontsize, $
   FONTTYPE=fonttype, $
   NORELEASE=norelease, $
   OFFCENTER=offcenter, $
   SELECTED=selected, $
   SENSITIVE=sensitive, $
   VALUE=value, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Call the superclass SetProperty methods.
   self -> DrawWidget::SetProperty, _EXTRA=extraKeywords

   ; Set any of your own keywords here.
   IF N_Elements(fontname) NE 0 THEN self.fontname = fontname
   IF N_Elements(fontsize) NE 0 THEN self.fontsize = fontsize
   IF N_Elements(fonttype) NE 0 THEN self.fonttype = fonttype
   IF N_Elements(norelease) NE 0 THEN self.norelease = Keyword_Set(norelease)
   IF N_Elements(offcenter) NE 0 THEN self.offcenter = offcenter
   IF N_Elements(selected) NE 0 THEN self.selected = selected
   IF N_Elements(sensitive) NE 0 THEN self.sensitive = sensitive
   IF N_Elements(value) NE 0 THEN self.value = value

   ; Need to draw?
   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       DRAWBUTTONWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the DRAWBUTTONWIDGET object class destructor method.
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
PRO DrawButtonWidget::Cleanup

   @cat_pro_error_handler

   ; Local objects and pointers
   Obj_Destroy, self.colors
   Obj_Destroy, self.pixmap

   ; Remove all the children in self.event_handler and destroy the container.
   self.event_handler -> Remove, /All
   Obj_Destroy, self.event_handler

   ; Call Superclass destructor.
   self -> DrawWidget::CLEANUP

   self -> Report, /Completed

END

;*****************************************************************************************************
;+
; NAME:
;       DRAWBUTTONWIDGET::INIT
;
; PURPOSE:
;
;       This is the BUTTONWIDGET object class initialization method.

; ARGUMENTS:
;
;       parent:        The parent object reference.
;
; KEYWORDS:
;
;       FONTNAME:       The name of the font to use for button text. Default is "". Only valid if FONTTYPE=1.
;
;       FONTSIZE:       The size of the font. Not valid with FONTTYPE=0. Default is 1.0.
;
;       FONTTYPE:       The type of font to use. (-1 Hershey, 0 Hardware, 1 True-Type). Default is 0.
;
;       NORELEASE:      If this keyword is set, the button will assume a SELECTED appearance upon
;                       selection.
;
;       OFFCENTER:      The number of pixels in Y direction that text should be offset. Default is 5.
;
;       SELECTED:       Set this keyword to give the button a selected appearance.
;
;       SENSITIVE:      Set this keyword to make the button sensitive (1) or insensitive (0). Default is 1.
;
;       UNITS:          The units for measurments. The default is 0 for pixels. Other values are
;                       1 for inches, and 2 for centimeters.
;
;       VALUE:          The initial text value of the button.
;
;       XOFFSET:        The horizontal space (pixels) from upper left corner of the parent bullitin-board base.
;
;       XSIZE:          The X size of the button.
;
;       YOFFSET:        The vertical space (pixels) from upper left corner of the parent bullitin-board base.
;
;       YSIZE:          The Y size of the button.
;
;       _EXTRA:         Any extra keywords are passed along to the DRAWWIDGET superclass object.
;-
;*****************************************************************************************************
FUNCTION DrawButtonWidget::INIT, parent, $
   EVENT_METHOD=event_method, $   ; Required to intercept event method intended for CATATOM.
   EVENT_OBJECTS=event_objects, $ ; Required to intercept event objects intended for CATATOM.
   FONTNAME=fontname, $
   FONTSIZE=fontsize, $
   FONTTYPE=fonttype, $
   NORELEASE=norelease, $
   OFFCENTER=offcenter, $
   SELECTED=selected, $
   SENSITIVE=sensitive, $
   UNITS=units, $
   VALUE=value, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   @cat_func_error_handler

   ; The parent must be a base widget object and it must be present.
   IF Obj_Valid(parent) EQ 0 THEN Message, 'A parent parameter is required.'
   IF (Obj_IsA_Valid (parent, 'BaseWidget')EQ 0) THEN $
      Message, 'The parent parameter must be a base widget.'

   IF N_Elements(xsize) EQ 0 THEN xsize = 100
   xsize = 50 > xsize

   ; YSIZE depends on XSIZE unless specified.
   IF N_Elements(ysize) EQ 0 THEN ysize = (xsize/3.0) > 25

      ; Create the new drawbutton widget.
   ok = self -> DrawWidget::INIT(parent, XOffset=xoffset, YOffset=yoffset, $
      XSize=xsize, YSize=ysize, UNITS=units, _Extra=extraKeywords, Button_Events=1)
   IF ~ok THEN RETURN, 0

   ; Check keywords.
   IF N_Elements(fontname) EQ 0 THEN fontname = ""
   IF N_Elements(fontsize) EQ 0 THEN fontsize = 1.0
   IF N_Elements(fonttype) EQ 0 THEN fonttype = 0
   IF N_Elements(offcenter) EQ 0 THEN offcenter = 5
   IF N_Elements(selected) EQ 0 THEN selected = 0
   IF N_Elements(sensitive) EQ 0 THEN sensitive = 1
   IF N_Elements(value) EQ 0 THEN value = "Button"

   ; Load object.
   self.colors = Obj_New('SystemColors')
   self.fontname = fontname
   self.fontsize = fontsize
   self.fonttype = fonttype
   self.norelease = Keyword_Set(norelease)
   self.pixmap = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)
   self.selected = selected
   self.sensitive = sensitive
   self.xsize = xsize
   self.offcenter = offcenter
   self.ysize = ysize
   self.value = value

      ; The following is required because the DRAWBUTTONWIDGET is a compound widget object.
      ; Get the assigned event object. Replace this with the self object. This
      ; assures you that the events will go through the DRAWBUTTONWIDGET's EventHandler
      ; method first. Save the assigned event object so it can be used at the end
      ; of the compound object's EventHandler method.

   IF N_Elements(event_objects) EQ 0 THEN $
   BEGIN
      parent -> CATATOM::GetProperty, Event_Objects=event_objects
      self.event_handler = OBJ_NEW ('CatContainer', Memory_Management=0)
      FOR j=0,N_Elements(event_objects) -1 DO BEGIN
         IF Obj_Valid(event_objects[j]) THEN self.event_handler -> Add, event_objects[j]
      ENDFOR
   ENDIF ELSE BEGIN
      self.event_handler = OBJ_NEW ('CatContainer', Memory_Management=0)
      FOR j=0,N_Elements(event_objects) -1 DO BEGIN
         IF Obj_Valid(event_objects[j]) THEN self.event_handler -> Add, event_objects[j]
      ENDFOR
   ENDELSE

      ; All events MUST come to the EventHandler method before they are dispatched
      ; elsewhere. This is typical of compound objects.

   IF N_Elements(event_method) NE 0 THEN self.event_method_real = event_method
   self._event_method = 'EventHandler'

   self -> Report, /Completed

   RETURN, 1

END



;*****************************************************************************************************
;
; NAME:
;       DRAWBUTTONWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This is the DRAWBUTTONWIDGET object's class definition code. The DRAWBUTTONWIDGET's event
;       structure is also defined here.
;
;*****************************************************************************************************
PRO DrawButtonWidget__Define, class

   event  = { DRAWBUTTONWIDGET_EVENT, $
                ID:Obj_New(), $            ; The widget object that caused the event.
                HANDLER:Obj_New(), $       ; The event handler object.
                EVENT_NAME: "", $          ; The name of the event. (DRAWBUTTONWIDGET_EVENT)
                NAME: "", $                ; The name of the DrawButtonWidget object.
                SELECT:0L $                ; Always set to 1.
            }

   class = { DRAWBUTTONWIDGET, $
             colors: Obj_New(), $         ; A SystemColors object for default drawing colors.
             currentSelect: 0L, $         ; The current self.selected state when button is pressed DOWN.
             event_handler:Obj_New(), $   ; The real event handler object(s) for the DROPLISTWIDGET.
             event_method_real: "", $     ; The event method assigned by the user to this object widget.
             fontname: "", $              ; The name of a true-type font to use for button text.
             fontsize: 0.0, $             ; The font size of button text.
             fonttype: 0L, $              ; The type of font you want (e.g, !P.Font).
             norelease: 0L, $             ; A flag that indicates the Sensitive=1 should not be set on button release.
             offcenter: 0L, $             ; An offset from center of widget.
             pixmap: Obj_New(), $         ; The pixmap for buffering output.
             selected: 0L, $              ; A flag that indicates if the button has a selected (1) or unselected (0) appearance.
             sensitive: 0L, $             ; A flag that indicates if the button is sensitive (1) or insensitive (0).
             xsize: 0L, $                 ; The X size of the widget.
             ysize: 0L, $                 ; The Y size of the widget.
             value: "", $                 ; The button value (text).
             INHERITS DrawWidget $
           }

END


