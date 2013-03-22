;*****************************************************************************************************
;+
; NAME:
;       PROPERTYSHEETWIDGET__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to create a propertysheet widget as an object.
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
;       theObject = Obj_New("PROPERTYSHEETWIDGET")
;
; SUPERCLASSES:
;
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { PROPERTYSHEETWIDGET, $
;             INHERITS WIDGETATOM $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 21 April 2004.
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
;       PROPERTYSHEETWIDGET::ADD
;
; PURPOSE:
;
;       This method is where you can screen what kinds of objects are
;       added to this object's hierarchy. The method is not always needed.
;       If you do create it, be CERTAIN to call the WIDGETATOM ADD method
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
;     _EXTRA:     Any keywords appropriate for the WIDGETATOM Add method.
;-
;*****************************************************************************************************
PRO PROPERTYSHEETWIDGET::Add, object, _EXTRA=extraKeywords

   @cat_pro_error_handler

   self -> WIDGETATOM::Add, object, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       PROPERTYSHEETWIDGET::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the PROPERTYSHEETWIDGET object. A
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
PRO PROPERTYSHEETWIDGET::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Object Control Panel', _EXTRA=extraKeywords)

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.

   base = Obj_New('BASEWIDGET', cp, Column=1, Frame=1)

   ; Display the control panel if it created its own TLB.

   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       PROPERTYSHEETWIDGET::DRAW
;
; PURPOSE:
;
;       This method may or may not be needed by your object, depending
;       upon whether a graphical representation of the object is required.
;       If you wish the DRAW method to automatically propogates down to any
;       objects contained in this object's container, call the WIDGETATOM DRAW
;       method.
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
PRO PROPERTYSHEETWIDGET::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; Draw any objects contained within this object.
   self -> WIDGETATOM::Draw, _Extra=extrakeywords

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;        PROPERTYSHEETWIDGET::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the PROPERTYSHEETWIDGET object. It will typically
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
PRO PROPERTYSHEETWIDGET::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       PROPERTYSHEETWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain PROPERTYSHEETWIDGET properties. Be sure
;       you ALWAYS call the WIDGETATOM GETPROPERTY method if you have extra
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
;     COMPONENT:      Set this to the component (object) for which information is requested.
;
;     PROPERTY_VALID: Set this to a string to see if this string is a valid property of the widget.
;                     The return value of this variable will be a 1 if the string is a valid property,
;                     or a 0 if the string is not a valid property.
;
;     PROPERTY_VALUE: Set this to the particular property identifier you which to know the
;                     value of.
;
;     VALUE:          The value of a particular COMPONENT and PROPERTY_VALUE.
;
;     _REF_EXTRA:     Any keywords appropriate for the WIDGETATOM GetProperty method.
;-
;*****************************************************************************************************
PRO PROPERTYSHEETWIDGET::GetProperty, $
   COMPONENT=component, $
   PROPERTY_VALID=property_valid, $
   PROPERTY_VALUE=property_value, $
   VALUE=value, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(value) THEN BEGIN
      self -> GetProperty, ID=id
      IF N_Elements(component) EQ 0 THEN Message, 'Must also specify COMPONENT to obtain value.'
      IF N_Elements(property_value) EQ 0 THEN Message, 'Must also specify PROPERTY_VALUE to obtain value.'
      value = Widget_Info(id, Component=component, Property_Value=property_value)
   ENDIF

   IF Arg_Present(property_valid) THEN property_valid = Widget_Info(id, Property_Valid=property_valid)

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> WIDGETATOM::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       PROPERTYSHEETWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the PROPERTYSHEETWIDGET object's properties. Be sure
;       you ALWAYS call the WIDGETATOM SETPROPERTY method if you have extra keywords!
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
;    REFRESH_PROPERTY: Set this keyword to refresh all the properties in the property sheet widget.
;
;     _EXTRA:          Any keywords appropriate for the WIDGETATOM SetProperty method.
;-
;*****************************************************************************************************
PRO PROPERTYSHEETWIDGET::SetProperty, REFRESH_PROPERTY=refresh_property, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(refresh_property) NE 0 THEN BEGIN
      self -> GetProperty, ID=id
      IF Size(refresh_property, /TNAME) NE 'STRING' THEN BEGIN
         Widget_Control, id, Refresh_Property=Keyword_Set(refresh_property)
      ENDIF ELSE BEGIN
         Widget_Control, id, Refresh_Property=refresh_property
      ENDELSE
   ENDIF

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> WIDGETATOM::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       PROPERTYSHEETWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the PROPERTYSHEETWIDGET object class destructor method.
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
PRO PROPERTYSHEETWIDGET::CLEANUP

   @cat_pro_error_handler

   self -> WIDGETATOM::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       PROPERTYSHEETWIDGET::INIT
;
; PURPOSE:
;
;       This is the PROPERTYSHEETWIDGET object class initialization method
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
;     ALIGN_BOTTOM: Set this keyword to align the base widget with the bottom of the parent base.
;
;     ALIGN_CENTER: Set this keyword to align the base widget with the center of the parent base.
;
;     ALIGN_LEFT:   Set this keyword to align the base widget with the left of the parent base.
;
;     ALIGN_RIGHT:  Set this keyword to align the base widget with the right of the parent base.
;
;     ALIGN_TOP:    Set this keyword to align the base widget with the top of the parent base.
;
;     CONTEXT_EVENTS: Set this keyword to make the base widget return WIDGET_CONTEXT events when
;                   the right mouse button is clicked over this base widget.
;
;     SCR_XSIZE:    Set the screen X size of the base to this many pixels. (Use discouraged.)
;
;     SCR_YSIZE:    Set the screen Y size of the base to this many pixels. (Use discouraged.)
;
;     UNITS:        The units for measurments. The default is 0 for pixels. Other values are
;                   1 for inches, and 2 for centimeters.
;
;     VALUE:        The object associated with the property sheet.
;
;     XOFFSET:      The horizontal space (pixels) from upper left corner of the display.
;
;     XSIZE:        The X size of the widget.
;
;     YOFFSET:      The vertical space (pixels) from upper left corner of the display.
;
;     YSIZE:        The Y size of the widget.
;
;     _EXTRA:       Any keywords appropriate for the WIDGETATOM INIT method.
;-
;*****************************************************************************************************
FUNCTION PROPERTYSHEETWIDGET::INIT, parent, $
   ALIGN_BOTTOM=align_bottom,$
   ALIGN_CENTER=align_center, $
   ALIGN_LEFT=align_left, $
   ALIGN_RIGHT=align_right, $
   ALIGN_TOP=align_top, $
   CONTEXT_EVENTS=context_events, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   UNITS=units, $
   VALUE=value, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   ; Set up error handler and call WIDGETATOM init method
   @cat_func_error_handler

   parent -> GetProperty, ID=parentID
   id = Widget_PropertySheet(parentID, $
      ALIGN_BOTTOM=align_bottom,$
      ALIGN_CENTER=align_center, $
      ALIGN_LEFT=align_left, $
      ALIGN_RIGHT=align_right, $
      ALIGN_TOP=align_top, $
      CONTEXT_EVENTS=context_events, $
      SCR_XSIZE=scr_xsize, $
      SCR_YSIZE=scr_ysize, $
      UNITS=units, $
      VALUE=value, $
      XOFFSET=xoffset, $
      XSIZE=xsize, $
      YOFFSET=yoffset, $
      YSIZE=ysize)

   ok = self -> WIDGETATOM::INIT (parent, id, _EXTRA=extraKeywords)

   IF (ok) THEN self -> Report, /Completed $
   ELSE self -> Report, /Failed
   RETURN, ok
END


;*****************************************************************************************************
;
; NAME:
;       PROPERTYSHEETWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the PROPERTYSHEETWIDGET object.
;
;*****************************************************************************************************
PRO PROPERTYSHEETWIDGET__DEFINE, class

   class = { PROPERTYSHEETWIDGET, $
             INHERITS WIDGETATOM $
           }

END

