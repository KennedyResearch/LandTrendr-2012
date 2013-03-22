;*****************************************************************************************************
;+
; NAME:
;       LABELWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to implement a label widget as an object.
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
;       labelWidget = Obj_New("LabelWidget", parent)
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
;   class = {LABELWIDGET, INHERITS WidgetAtom }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 15th July 2002.
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
;
; NAME:
;       LABELWIDGET::ADD
;
; PURPOSE:
;
;       This method overrides the superclass Add method to prevent addition of
;       other widget objects to label objects.
;
; SYNTAX:
;
;       labelObject -> Add, object
;
; ARGUMENTS:
;
;       object: The object to add to the container. Only objects that are NOT subclassed
;               from WIDGETATOM objects can be added.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO LabelWidget::Add, object

   @cat_pro_error_handler
   IF OBJ_ISA_VALID(object, 'WIDGETATOM') THEN Message, 'Cannot add widget objects to LabelWidgets.'
   self -> Add::WIDGETATOM, object
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       LABELWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to get the object's properties.
;
; SYNTAX:
;
;       labelObject -> GetProperty, VALUE=labelValue
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       VALUE:       The current value of the slider widget.
;
;       _REF_EXTRA:  Any keyword appropriate for the "WidgetAtom::GetProperty" object method.
;
;-
;*****************************************************************************************************
PRO LabelWidget::GetProperty, VALUE=value, _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (WIDGET_INFO (self._id, /VALID_ID)) THEN WIDGET_CONTROL, self._id, GET_VALUE=value
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> WidgetAtom::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       LABELWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the object's properties.
;
; SYNTAX:
;
;       labelObject -> SetProperty, VALUE='Test'
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       VALUE:          The value of the label widget.
;
;       DYNAMIC_RESIZE: Set this keyword to make the label size itself for its contents
;
;       _EXTRA:         Any keyword appropriate for the "WidgetAtom::SetProperty" object method.
;
;-
;*****************************************************************************************************
PRO LabelWidget::SetProperty, VALUE=value, DYNAMIC_RESIZE=dynamic_Resize, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (value) GT 0) THEN WIDGET_CONTROL, self._id, SET_VALUE=value
   IF (N_ELEMENTS (dynamic_resize) GT 0) THEN WIDGET_CONTROL, self._id, DYNAMIC_RESIZE=dynamic_resize
   self -> widgetAtom::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       LABELWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the LABELWIDGET object class destructor method.
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
;       _EXTRA:  Any keyword appropriate for the "WidgetAtom::Cleanup" method.
;-
;*****************************************************************************************************
PRO LabelWidget::CLEANUP

   @cat_pro_error_handler
   self -> WidgetAtom::CLEANUP
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       LABELWIDGET::INIT
;
; PURPOSE:
;
;       This is the LABELWIDGET object class initialization method.

; ARGUMENTS:
;
;       theParent - The parent object reference. This is the same as using the PARENT keyword.
;
; KEYWORDS:
;
;       ALIGN_CENTER:    Set this keyword to center-align text on the widget.
;
;       ALIGN_LEFT:      Set this keyword to left-align text on the widget.
;
;       ALIGN_RIGHT:     Set this keyword to right-align text on the widget.
;
;       DYNAMIC_RESIZE:  Set this keyword to enable dynamic resizing of the widget.
;
;       FONT:            The name of a font to be used by the widget.
;
;       FRAME:           Create a frame this many pixels wide around base.
;
;       PARENT:          An object reference to a container object.
;
;       SCR_XSIZE:       Set the screen X size of the widget to this many pixels. (Use discouraged.)
;
;       SCR_YSIZE:       Set the screen Y size of the widget to this many pixels. (Use discouraged.)
;
;       SUNKEN_FRAME:    Set this keyword to obtain a 3D look for the lable.
;
;       UNITS:           The units for measurments. The default is 0 for pixels. Other values are
;                        1 for inches, and 2 for centimeters.
;
;       VALUE:           The text to be placed on the label widget.
;
;       XOFFSET:         The horizontal space (pixels) from upper left corner of the parent.
;
;       XSIZE:           The X size of the widget.
;
;       YOFFSET:         The vertical space (pixels) from upper left corner of the parent.
;
;       YSIZE:           The Y size of the widget.
;
;       _EXTRA:          Any keyword appropriate for the "WidgetAtom::Init" method.
;-
;*****************************************************************************************************
FUNCTION LabelWidget::INIT, parent, $
   ALIGN_CENTER=align_center, $
   ALIGN_LEFT=align_left, $
   ALIGN_RIGHT=align_right, $
   DYNAMIC_RESIZE=dynamic_resize, $
   FONT=font, $
   FRAME=frame, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   SUNKEN_FRAME=sunken_frame, $
   UNITS=units, $
   VALUE=value, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   @cat_func_error_handler

      ; The parent can be passed in as an argument, or as a keyword. Resolve
      ; the two here.

   ; Make sure there is a valid BASEWIDGET class object as a parent.

   IF OBJ_ISA_VALID(parent, "BASEWIDGET") EQ 0 THEN Message, 'Parent object invalid or not of type BASEWIDGET.'

   ; Create the new label
   parent -> GetProperty, ID=parentID
   id = WIDGET_LABEL (parentID, $
      ALIGN_CENTER=align_center, $
      ALIGN_LEFT=align_left, $
      ALIGN_RIGHT=align_right, $
      DYNAMIC_RESIZE=dynamic_resize, $
      FONT=font, $
      FRAME=frame, $
      SCR_XSIZE=scr_xsize, $
      SCR_YSIZE=scr_ysize, $
      SUNKEN_FRAME=sunken_frame, $
      UNITS=units, $
      VALUE=value, $
      XOFFSET=xoffset, $
      XSIZE=xsize, $
      YOFFSET=yoffset, $
      YSIZE=ysize)

   ; Call the superclass INIT method
   ok = self -> WidgetAtom::INIT (parent, id, _EXTRA=extraKeywords)
   IF (NOT ok) THEN self -> Message, 'Superclass failed to initialize properly.'

   ; Print and return status report
   IF (ok) THEN self -> Report, /Completed $
   ELSE self -> Report, /Failed
   RETURN, ok
END


;*****************************************************************************************************
;
; NAME:
;       LABELWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This is the LABELWIDGET object's structure definition code.
;
;*****************************************************************************************************
PRO LabelWidget__DEFINE, class
   class = {LabelWidget, INHERITS WidgetAtom}
END