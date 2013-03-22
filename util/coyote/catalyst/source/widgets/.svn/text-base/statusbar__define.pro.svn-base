;*****************************************************************************************************
;+
; NAME:
;       STATUSBAR
;
; PURPOSE:
;
;       The purpose of this routine is to implement a status bar that can be used
;       to alert the user to the current status of the system.
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
;       aStatusBar = Obj_New("STATUSBAR", theParent, Status='Ready')
;
; SUPERCLASSES:
;
;       LABELWIDGET
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { STATUSBAR, $
;             INHERITS LABELWIDGET, $ ; Inherits LABELWIDGET.
;             _percent: 0L $          ; Percentage of size of parent widget.
;           }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W.Fanning, 25 August 2002.
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
;       STATUSBAR::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the STATUSBAR object. A
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
PRO StatusBar::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='StatusBar Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)
   self -> SetProperty, Description='Pixmap Properties'

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.
   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, $
      Name='STATUSBAR PROPERTYSHEET', Description='StatusBar Properties', YSIZE=3)
   aproperties -> SetProperty, Event_Object=self

   ; Is the base object from a browser? If so, then size the property sheet
   ; according to the size of the base widget.
   IF Obj_Valid(baseObject) THEN BEGIN
      IF StrUpCase(StrMid(baseObject->GetName(), 0, 7)) EQ 'BROWSER' THEN BEGIN
         baseObject -> GetProperty, Geometry=geo
         aproperties -> SetProperty, Scr_XSize=geo.xsize, Scr_YSize=geo.ysize
      ENDIF

   ENDIF

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END

;*****************************************************************************************************
;+
; NAME:
;        STATUSBAR::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the STATUSBAR object. It will typically
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
PRO StatusBar::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF

      'STATUSBAR PROPERTYSHEET': BEGIN

         IF event.type EQ 0 THEN BEGIN

            CASE StrUpCase(event.identifier) OF

               ELSE: BEGIN
                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value
               END

            ENDCASE

         ENDIF

         END

   ENDCASE

   ; Report completion. Object may have been deleted.
   IF Obj_Valid(self) THEN self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       STATUSBAR::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to obtain the STATUSBAR object's properties
;
; SYNTAX:
;
;       aStatusBar -> GetProperty, Status=theStatus
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       TEXT:          The current status text.
;
;       _REF_EXTRA:    Any keywords appropriate for the superclass GetProperty methods.
;-
;*****************************************************************************************************
PRO StatusBar::GetProperty, Text=text, _REF_EXTRA=extraKeywords

    @cat_pro_error_handler

   ; Get the properties. Make sure you have a valid widget ID.
   IF Arg_Present(text) THEN Widget_Control, self._ID, Get_Value=text

   ; Call the superclass GetProperty method if needed.
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> LabelWidget::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       STATUSBAR::NOTIFY_REALIZE
;
; PURPOSE:
;
;       This method is used to set the initial size of the status bar when
;       it is realized.
;
; SYNTAX:
;
;       It is called automatically by the Catalyst Library system.
;
; ARGUMENTS:
;
;       object:   The widget object reference of the object being realized.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO StatusBar::Notify_Realize, object

   @cat_pro_error_handler

   ; Get the geometries of the filename widgets.

   object -> GetProperty, First_Parent=theParent
   IF Obj_ISA_Valid(theParent, 'WIDGETATOM') THEN BEGIN

      ; Assemble all the parts to calculate new size.
      theParent -> GetProperty, Geometry=pgeo
      spacing = 2*pgeo.space
      padding = 4*pgeo.xpad
      IF StrUpCase(!VERSION.os_family) EQ 'WINDOWS' THEN BEGIN
            newsize = pgeo.scr_xsize - padding - spacing - 4
      ENDIF ELSE BEGIN
            newsize = pgeo.scr_xsize - 8
      ENDELSE

      object -> SetProperty, Scr_XSize = newsize * (self._percent / 100.0)
   ENDIF

   self -> Report, /Completed

 END



;*****************************************************************************************************
;+
; NAME:
;       STATUSBAR::RESIZE
;
; PURPOSE:
;
;       This method is used to resize the status bar at the end of a resize event.
;
; SYNTAX:
;
;       statusBar -> Resize
;
; ARGUMENTS:
;
;       objectToMatch:   When resizing, the status bar will normally size itself according
;                        to its parent base object. If this parameter is passed, it will size
;                        itself to the dimensions of this object.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO StatusBar::Resize, objectToMatch

   @cat_pro_error_handler

   ; Get the geometries of the filename widgets.

   IF N_Elements(objectToMatch) EQ 0 THEN $
      self -> GetProperty, First_Parent=objectToMatch
   IF Obj_ISA_Valid(objectToMatch, 'WIDGETATOM') THEN BEGIN

      ; Assemble all the parts to calculate new size.
      objectToMatch -> GetProperty, Geometry=pgeo
      spacing = 2*pgeo.space
      padding = 4*pgeo.xpad
      IF StrUpCase(!VERSION.os_family) EQ 'WINDOWS' THEN BEGIN
            newsize = pgeo.scr_xsize - padding - spacing - 4
      ENDIF ELSE BEGIN
            newsize = pgeo.scr_xsize - 2
      ENDELSE
      self -> SetProperty, Scr_XSize = newsize * (self._percent / 100.0)
   ENDIF

   self -> Report, /Completed

 END


;*****************************************************************************************************
;+
; NAME:
;       STATUSBAR::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the STATUSBAR object's properties
;
; SYNTAX:
;
;       aStatusBar -> SetProperty, Status=theStatus
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       TEXT:            The status bar text.
;
;       _EXTRA:          Any keywords appropriate for the superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO StatusBar::SetProperty, TEXT=text, SCR_XSIZE=scr_xsize, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(scr_xsize) NE 0 THEN BEGIN
      Widget_Control, self._id, Scr_XSize=scr_xsize

   ENDIF

      ; Call the superclass GetProperty method if needed.

   IF N_Elements(extraKeywords) NE 0 THEN self -> LabelWidget::SetProperty, _EXTRA=extraKeywords

   IF N_Elements(text) NE 0 THEN Widget_Control, self._ID, Set_Value=text

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;        STATUSBAR::EVENTTRACKING
;
; PURPOSE:
;
;        This a genertic method for handling widget tracking events. The HELPLINE
;        string is removed from the event.ID object and used in the status bar as
;        the one-line widget tracking value.
;
; SYNTAX:
;
;        This method is called (typically) from an EVENTHANDLER method.
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
PRO StatusBar::TrackingEvents, event

   @cat_pro_error_handler

      CASE event.name OF

        ELSE: BEGIN
                        event.ID -> GetProperty, Helpline=text
                        IF event.enter EQ 1 THEN BEGIN
                           self -> SetProperty, Text=text
                        ENDIF ELSE BEGIN
                           self -> SetProperty, Text=''
                        ENDELSE
                        END
      ENDCASE

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       STATUSBAR::CLEANUP
;
; PURPOSE:
;
;       This is the STATUSBAR object class destructor method.
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
PRO StatusBar::CLEANUP
   self->LabelWidget::Cleanup
END



;*****************************************************************************************************
;+
; NAME:
;       STATUSBAR::INIT
;
; PURPOSE:
;
;       This is the STATUSBAR object class initialization method
;
; SYNTAX:
;
;       statusbar = Obj_New('STATUSBAR')
;
; ARGUMENTS:
;
;       theParent:     An object reference to a WIDGETATOM-subclassed object.
;                      This is the same as using the PARENT keyword.
;
; KEYWORDS:
;
;       ALIGN_BOTTOM:   Set this keyword to align the statusbar widget with the bottom of the parent base.
;
;       ALIGN_CENTER:   Set this keyword to align the statusbar widget with the center of the parent base.
;
;       ALIGN_LEFT:     Set this keyword to align the statusbar widget with the left of the parent base.
;
;       ALIGN_RIGHT:    Set this keyword to align the statusbar widget with the right of the parent base.
;
;       ALIGN_TOP:      Set this keyword to align the statusbar widget with the top of the parent base.
;
;       FONT:           Set this keyword to the name of a font to use on the status bar.
;
;       FRAME:          Set this keyword to create a frame this many pixels wide around the
;                       status bar. Set to 1 by default.
;
;       PARENT:         An object reference to a WIDGETATOM-subclassed object.
;
;       PERCENT:        A number between 10 and 100 indicating the size of the status bar
;                       with respect to the size of its parent widget object.
;
;       SCR_XSIZE:      Set the screen X size of the widget to this many pixels. (Use discouraged.)
;
;       SCR_YSIZE:      Set the screen Y size of the widget to this many pixels. (Use discouraged.)
;
;       TEXT:           A text string for display in the status bar. Set to "Ready" by default.
;
;       UNITS:          The units for measurments. The default is 0 for pixels. Other values are
;                       1 for inches, and 2 for centimeters.
;
;       XOFFSET:        The horizontal space (pixels) from upper left corner of the parent.
;
;       XSIZE:          The X size of the widget.
;
;       YOFFSET:        The vertical space (pixels) from upper left corner of the parent.
;
;       YSIZE:          The Y size of the widget.
;
;       _EXTRA:         Any keyword appropriate for the superclass INIT methods.
;-
;*****************************************************************************************************
FUNCTION StatusBar::INIT, $
   theParent, $
   ALIGN_BOTTOM=align_bottom,$
   ALIGN_CENTER=align_center, $
   ALIGN_LEFT=align_left, $
   ALIGN_RIGHT=align_right, $
   ALIGN_TOP=align_top, $
   FONT=font, $
   FRAME=frame, $
   PARENT=parent, $
   PERCENT=percent, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   TEXT=text, $
   UNITS=units, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
   _Extra=extraKeywords

   @cat_func_error_handler

      ; The parent can be passed in as an argument, or as a keyword. Resolve
      ; the two here.

   CASE 1 OF
      N_Elements(theParent) EQ 0 AND N_Elements(parent) EQ 0: Message, 'A parent widget object is required.'
      N_Elements(theParent) EQ 0 AND N_Elements(parent) NE 0: parent = parent
      N_Elements(theParent) NE 0 AND N_Elements(parent) EQ 0: parent = theParent
      N_Elements(theParent) NE 0 AND N_Elements(parent) NE 0: parent = theParent
   ENDCASE

      ; If there is a parent, make sure it is a valid BASEWIDGET class object.

   IF OBJ_ISA_VALID(parent, "BASEWIDGET") EQ 0 THEN Message, 'Parent object invalid or not of type BASEWIDGET.'

      ; Check for presence of keywords.

   IF (N_Elements(align_center) + N_Elements(align_left) + N_Elements(align_right) EQ 0) THEN align_center = 1
   IF N_Elements(frame) EQ 0 THEN frame = 0
   IF N_Elements(title) EQ 0 THEN title = 'Status: '
   IF N_Elements(text) EQ 0 THEN text = 'Ready'
   IF N_Elements(percent) EQ 0 THEN self._percent = 100L ELSE self._percent = 10L > percent < 100L

      ; Create the widget.

   ok = self->LABELWIDGET::INIT(parent, $
        ALIGN_BOTTOM=align_bottom,$
        ALIGN_CENTER=align_center, $
        ALIGN_LEFT=align_left, $
        ALIGN_RIGHT=align_right, $
        ALIGN_TOP=align_top, $
        DYNAMIC_RESIZE=1, $
        FONT=font, $
        FRAME=frame, $
        SCR_XSIZE=scr_xsize, $
        SCR_YSIZE=scr_ysize, $
        SUNKEN_FRAME=1, $
        UNITS=units, $
        XOFFSET=xoffset, $
        XSIZE=xsize, $
        YOFFSET=yoffset, $
        YSIZE=ysize, $
        VALUE=text, $
        Notify_Realize=1,$
         _Extra=extraKeywords)

   IF NOT ok THEN BEGIN
      self -> Report, /Failed
      RETURN, 0
   ENDIF

   ; Register properties
   self->RegisterProperty, 'TEXT', 4, NAME="Text"

   RETURN, 1
END



;*****************************************************************************************************
;
; NAME:
;       STATUSBAR CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the STATUSBAR object class definition.
;       The STATUSBAR object is subclassed from the BASEWIDGET object.
;
;*****************************************************************************************************
PRO StatusBar__DEFINE, class


   class = { STATUSBAR, $
             INHERITS LABELWIDGET, $ ; Inherits LABELWIDGET.
             _percent: 0L $          ; Percentage of size of parent widget.
           }
END