;*****************************************************************************************************
;+
; NAME:
;       SLIDERWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to implement a slider widget as an object.
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
;       sliderWidget = Obj_New("sliderWidget", parent)
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
;    class = { sliderWidget, INHERITS widgetAtom }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 24 March 2003.
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
;       SLIDERWIDGET::ADD
;
; PURPOSE:
;
;       This method overrides the superclass Add method to prevent addition of
;       other widget objects to slider objects.
;
; SYNTAX:
;
;       sliderObject -> Add, object
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
PRO SliderWidget::Add, object

   @cat_pro_error_handler
   IF OBJ_ISA_VALID(object, 'WIDGETATOM') THEN Message, 'Cannot add widget objects to SliderWidgets.'
   self -> Add::WIDGETATOM, object
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       SLIDERWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to get the object's properties.
;
; SYNTAX:
;
;       sliderObject -> GetProperty, VALUE=sliderValue
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       VALUE:       The current value of the slider widget.
;
;       MINIMUM:     The minimum value for the slider.
;
;;      MAXIMUM:     The maximum value for the slider.
;
;       _REF_EXTRA:  Any keyword appropriate for the "WidgetAtom::GetProperty" object method.
;-
;*****************************************************************************************************
PRO SliderWidget::GetProperty, VALUE=value, $
                               MINIMUM=minimum, $
                               MAXIMUM=maximum, $
                               _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   Widget_Control, self._id, Get_Value=value
   minmax = Widget_Info(self._id, SLIDER_MIN_MAX=1)
   minimum = minmax[0]
   maximum = minmax[1]

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> widgetAtom::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed
END

;*****************************************************************************************************
;+
; NAME:
;       SLIDERWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the object's properties.
;
; SYNTAX:
;
;       sliderObject -> SetProperty, VALUE=sliderValue
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       VALUE:       The value of the slider widget.
;
;       MINIMUM:     The minimum value for the slider.
;
;;      MAXIMUM:     The maximum value for the slider.
;
;       _EXTRA:      Any keyword appropriate for the "WidgetAtom::SetProperty" object method.
;
;-
;*****************************************************************************************************
PRO SliderWidget::SetProperty, VALUE=value, MINIMUM=minimum, MAXIMUM=maximum, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (value  ) NE 0) THEN BEGIN
      Widget_Control, self._id, SET_VALUE=value
   ENDIF
   IF (N_ELEMENTS (minimum) NE 0) THEN BEGIN
      Widget_Control, self._id, SET_SLIDER_MIN=minimum
   ENDIF
   IF (N_ELEMENTS (maximum) NE 0) THEN BEGIN
      Widget_Control, self._id, SET_SLIDER_MAX=maximum
   ENDIF

   self -> widgetAtom::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       SLIDERWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the SLIDERWIDGET object class destructor method.
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
PRO SliderWidget::CLEANUP

   @cat_pro_error_handler
   self -> widgetAtom::CLEANUP
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       SLIDERWIDGET::INIT
;
; PURPOSE:
;
;       This is the SLIDERWIDGET object class initialization method.

; ARGUMENTS:
;
;       theParent - The parent object reference. This is the same as using the PARENT keyword.
;
; KEYWORDS:
;
;       DRAG:        Set this keyword to create events as the slider is dragged. Drag events
;                    are not possible on WINDOWS operating systems. It is not possible to turn
;                    continuous drag events on using SETPROPETY.
;
;       FONT:        The name of a font to be used by the widget.
;
;       FRAME:       Create a frame this many pixels wide around base.
;
;       MINIMUM:     The minimum value for the slider.
;
;       MAXIMUM:     The maximum value for the slider.
;
;       PARENT:      An object reference to a container object.
;
;       SCR_XSIZE:   Set the screen X size of the widget to this many pixels. (Use discouraged.)
;
;       SCR_YSIZE:   Set the screen Y size of the widget to this many pixels. (Use discouraged.)
;
;       TITLE:       The text title on the slider.
;
;       UNITS:       The units for measurments. The default is 0 for pixels. Other values are
;                    1 for inches, and 2 for centimeters.
;
;       VALUE:       The value of the slider widget.
;
;       VERTICAL:    Set this keyword to create a vertical slider as opposed to the default
;                    horizontal slider.
;
;       XOFFSET:     The horizontal space (pixels) from upper left corner of the parent.
;
;       XSIZE:       The X size of the widget.
;
;       YOFFSET:     The vertical space (pixels) from upper left corner of the parent.
;
;       YSIZE:       The Y size of the widget.
;
;       _EXTRA:      Any keyword appropriate for the "WidgetAtom::Init" method.
;-
;*****************************************************************************************************
FUNCTION SliderWidget::INIT, parent, $
   DRAG=drag, $
   FONT=font, $
   FRAME=frame, $
   MAXIMUM=maximum, $
   MINIMUM=minimum, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   SUPPRESS_VALUE=suppress_value, $
   TITLE=title, $
   UNITS=units, $
   VALUE=value, $
   VERTICAL=vertical, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   @cat_func_error_handler

      ; If there is a parent, make sure it is a valid BASEWIDGET class object.

   IF OBJ_ISA_VALID(parent, "BASEWIDGET") EQ 0 THEN Message, 'Parent object invalid or not of type BASEWIDGET.'

   ; Check input parameters
   IF (N_ELEMENTS (value) EQ 0) THEN value = 50

   IF (N_ELEMENTS (minimum) EQ 0) THEN minimum = 0

   IF (N_ELEMENTS (maximum) EQ 0) THEN maximum = 100

   ; Create the new slider
   parent -> getProperty, ID=parentID
   id = WIDGET_SLIDER (parentID, $
      DRAG=drag, $
      FONT=font, $
      FRAME=frame, $
      MAXIMUM=maximum, $
      MINIMUM=minimum, $
      SCR_XSIZE=scr_xsize, $
      SCR_YSIZE=scr_ysize, $
      SUPPRESS_VALUE=suppress_value, $
      TITLE=title, $
      UNITS=units, $
      VALUE=value, $
      VERTICAL=vertical, $
      XOFFSET=xoffset, $
      XSIZE=xsize, $
      YOFFSET=yoffset, $
      YSIZE=ysize)

   ; Call the superclass init method
   ok = self -> WidgetAtom::INIT (parent, id, _EXTRA=extraKeywords)
   IF NOT ok THEN Message, 'Superclass failed to initialize properly.'

   ; Print and return status report
   self -> Report, /Completed
   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       SLIDERWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This is the SLIDERWIDGET object's structure definition code.
;
;*****************************************************************************************************
PRO SliderWidget__DEFINE, class

    class = { SliderWidget, $
              INHERITS widgetAtom $
            }
END