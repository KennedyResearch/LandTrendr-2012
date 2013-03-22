;*****************************************************************************************************
;+
; NAME:
;       BUTTONWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to implement a button widget as an object.
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
;       buttonWidget = Obj_New("buttonWidget", theParent)
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
;    class = { ButtonWidget, $        ; The BUTTONWIDGET object class definition.
;              _menu:0B, $            ; A flag that indicates this is a menu button.
;              INHERITS WidgetAtom $  ; Inherits WIDGETATOM properties.
;             }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W.Fanning, 28 June 2002.
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
;       BUTTONWIDGET::ADD
;
; PURPOSE:
;
;       This method overrides the superclass Add methods to make sure only button widgets are
;       added to menu buttons.
;
; SYNTAX:
;
;       thisButtonObj -> Add, thatButtonObj
;
; ARGUMENTS:
;
;       object: The object to add to the container. Only objects subclassed from BUTTONWIDGET
;               can be added, and only if the current object has a menu flag set for it.
;
; KEYWORDS:
;
;       _EXTRA: Any keyword appropriate for the superclass ADD methods.
;
;-
;*****************************************************************************************************
PRO ButtonWidget::Add, object, _Extra=extraKeywords

   @cat_pro_error_handler

   IF Obj_Valid(object) EQ 0 THEN Message, 'Only valid objects can be added to this container.'
   IF (OBJ_ISA (object, 'buttonWidget'))  EQ 0 THEN $
      Message, 'Only sub-classed BUTTONWIDGET objects can be added to ButtonWidget objects.'
   IF (self._menu) THEN self -> WidgetAtom::Add, object, _Extra=extraKeywords $
      ELSE Message, 'ButtonObjects can only be added to ButtonObjects when the MENU flag is set.'

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       BUTTONWIDGET::DRAW
;
; PURPOSE:
;
;       A dummy DRAW method. All keywords are passed to the WIDGETATOM::DRAW superclass.
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
PRO ButtonWidget::Draw, _EXTRA=extraKeywords
   @cat_pro_error_handler
   self -> WidgetAtom::Draw, _EXTRA=extraKeywords
END


;*****************************************************************************************************
;+
; NAME:
;       BUTTONWIDGET::GETPROPERTY
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
;       BUTTON_SET:     This keyword will return 1 if the button is set, or 0 otherwise.
;
;       DYNAMIC_RESIZE: This keyword will be set to 1 if the button currently will resize dynamically.
;                       It is set to 0 otherwise.
;
;       TOOLTIP:        Returns the current tooltip associated with this button.
;
;       VALUE:          The current value of the button widget.
;
;       _REF_EXTRA:     Any keyword appropriate for the GETPROPERTY method of superclass objects.
;-
;*****************************************************************************************************
PRO ButtonWidget::GetProperty, $
   BUTTON_SET=button_set, $
   DYNAMIC_RESIZE=dynamic_resize, $
   TOOLTIP=tooltip, $
   VALUE=value, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (WIDGET_INFO (self._id, /VALID_ID)) THEN BEGIN
      IF Arg_Present(value) AND (self._bitmap EQ 0) THEN WIDGET_CONTROL, self._id, GET_VALUE=value
   ENDIF
   IF Arg_Present(button_set) THEN button_set = Widget_Info(self._id, /Button_Set)
   IF Arg_Present(tooltip) THEN button_set = Widget_Info(self._id, /Tooltip)
   IF (WIDGET_INFO (self._id, /VALID_ID)) THEN dynamic_resize = WIDGET_INFO(self._id, /DYNAMIC_RESIZE)
   self -> WidgetAtom::GetProperty, _Extra=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       BUTTONWIDGET::SETPROPERTY
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
;       BITMAP:         Set this keyword if the VALUE is a bitmap, rather than text.
;
;       DYNAMIC_RESIZE: This keyword will be set to 1 if the button currently will resize dynamically.
;                       It is set to 0 otherwise.
;
;       INPUT_FOCUS:    Set this keyword to make this button have the current keyboard focus.
;
;       MENU:           Set this keyword to turn this into a button capably
;
;       VALUE:          Set this keyword to the current value of the button widget.
;
;       _EXTRA:         Any keyword appropriate for the GETPROPERTY method of superclass objects.
;-
;*****************************************************************************************************
PRO ButtonWidget::SetProperty, $
   BITMAP=bitmap, $
   INPUT_FOCUS=input_focus, $
   MENU=menu, $
   SET_BUTTON=set_button, $
   TOOLTIP=tooltip, $
   VALUE=value, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (value) GT 0) THEN WIDGET_CONTROL, self._id, SET_VALUE=value, BITMAP=Keyword_Set(bitmap)
   IF (N_ELEMENTS (input_focus) GT 0) THEN WIDGET_CONTROL, self._id, INPUT_FOCUS=Keyword_Set(input_focus)
   IF (N_ELEMENTS (menu) GT 0) THEN self._menu = Keyword_Set(menu)
   IF (N_ELEMENTS (set_button) GT 0) THEN WIDGET_CONTROL, self._id, SET_BUTTON=Keyword_Set(set_button)
   IF (N_ELEMENTS (tooltip) GT 0) THEN WIDGET_CONTROL, self._id, TOOLTIP=tooltip
   IF N_Elements(bitmap) NE 0 THEN self._bitmap = Keyword_Set(bitmap)

   self -> WidgetAtom::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed
END

;*****************************************************************************************************
;+
; NAME:
;       BUTTONWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the BUTTONWIDGET object class destructor method.
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
PRO ButtonWidget::CLEANUP

   @cat_pro_error_handler

   self -> WidgetAtom::CLEANUP
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       BUTTONWIDGET::INIT
;
; PURPOSE:
;
;       This is the BUTTONWIDGET object class initialization method.

; ARGUMENTS:
;
;       parent:         The parent object reference.
;
; KEYWORDS:
;
;       ACCELERATOR:    Set this keyword to a string that defines an accelerator key for the
;                       button. See the documentation for WIDGET_BUTTON for details. Accelerator
;                       keys can only be assigned at initialization of the widget.
;
;       ALIGN_CENTER:   Align the button with the center of it's parent base.
;
;       ALIGN_LEFT:     Align the button with the left of it's parent base.
;
;       ALIGN_RIGHT:    Align the button with the right of it's parent base.
;
;       BITMAP:         Set this keyword to specify that the button value is a color bitmap.
;
;       CHECKED_MENU:   Set this keyword to enable a check mark to be placed next to a button
;                       that is used as a menu item.
;
;       DYNAMIC_RESIZE: Set this keyword to specify a button that is dynamically resized.
;
;       FONT:           The name of the font to use for button text.
;
;       FRAME:          Create a frame this many pixels wide around base.
;
;       MENU:           Set this keyword to make this button a menu button.
;
;       NO_RELEASE:     Set this keyword enable only SELECT events for exclusive and non-exclusive
;                       buttons. The keyword has no effect on regular button widgets.
;
;       SCR_XSIZE:      Set the screen X size of the base to this many pixels. (Use discouraged.)
;
;       SCR_YSIZE:      Set the screen Y size of the base to this many pixels. (Use discouraged.)
;
;       SEPARATOR:      Set this keyword to add a separator line above the button.
;
;       TOOLTIP:        The tooltip associated with this button.
;
;       UNITS:          The units for measurments. The default is 0 for pixels. Other values are
;                       1 for inches, and 2 for centimeters.
;
;       VALUE:          The initial value of the button. Text in most cases, but a bitmap if the BITMAP
;                       keyword is set.
;
;       X_BITMAP_EXTRA: When creating a bitmap button that is not of a "byte-aligned" size (in other words,
;                       does not have a dimension that is a multiple of 8), this keyword specifieds how
;                       many bits of the supplied bitmap must be ignored (within the end byte). For example,
;                       to create a 10 by 8 bitmap, you must supply a 2 by 8 array of bytes and ignore the
;                       bottom 6 bits. Therefore, you would specify X_BITMAP_EXTRA=6.
;
;       XOFFSET:        The horizontal space (pixels) from upper left corner of the parent bullitin-board base
;
;       XSIZE:          The X size of the widget. (Use discouraged.)
;
;       YOFFSET:        The vertical space (pixels) from upper left corner of the parent bullitin-board base
;
;       YSIZE:          The Y size of the widget. (Use discouraged.)
;
;       _EXTRA: Any extra keywords are passed along to the WIDGETATOM superclass object.
;-
;*****************************************************************************************************
FUNCTION ButtonWidget::INIT, parent, $
   ACCELERATOR=accelerator, $
   ALIGN_CENTER=align_center, $
   ALIGN_LEFT=align_left, $
   ALIGN_RIGHT=align_right, $
   BITMAP=bitmap, $
   CHECKED_MENU=checked_menu, $
   DYNAMIC_RESIZE=dynamic_resize, $
   FONT=font, $
   FRAME=frame, $
   MENU=menu, $
   NO_RELEASE=no_release, $
   PUSHBUTTON_EVENTS=pushbutton_events, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   SEPARATOR=separator, $
   TOOLTIP=tooltip, $
   UNITS=units, $
   VALUE=value, $
   XSIZE=xsize, $
   YSIZE=ysize, $
   X_BITMAP_EXTRA=x_bitmap_extra, $
   XOFFSET=xoffset, $
   YOFFSET=yoffset, $
   _EXTRA=extraKeywords


      ; Error handling.

   @cat_func_error_handler

      ; The parent must be either a button or base widget object

   IF (Obj_IsA_Valid (parent, 'BaseWidget')   + $
       Obj_IsA_Valid (parent, 'ButtonWidget') + $
       Obj_IsA_Valid (parent, 'MenubarWidget') EQ 0) THEN $
   BEGIN
      Message, 'Parent widget object required that is either a base or button widget.'
   ENDIF

      ; Create the new button widget.

   parent -> GetProperty, ID=parentID
   id = WIDGET_BUTTON (parentID, $
      ACCELERATOR=accelerator, $
      ALIGN_CENTER=align_center, $
      ALIGN_LEFT=align_left, $
      ALIGN_RIGHT=align_right, $
      BITMAP=bitmap, $
      CHECKED_MENU=checked_menu, $
      DYNAMIC_RESIZE=dynamic_resize, $
      FONT=font, $
      FRAME=frame, $
      MENU=menu, $
      NO_RELEASE=no_release, $
      PUSHBUTTON_EVENTS=pushbutton_events, $
      SCR_XSIZE=scr_xsize, $
      SCR_YSIZE=scr_ysize, $
      SEPARATOR=separator, $
      TOOLTIP=tooltip, $
      UNITS=units, $
      X_BITMAP_EXTRA=x_bitmap_extra, $
      XSIZE=xsize, $
      YSIZE=ysize, $
      XOFFSET=xoffset, $
      YOFFSET=yoffset, $
      VALUE=value)

      ; Is this a menu button?

   self._menu = KEYWORD_SET (menu)
   self._bitmap = KEYWORD_SET (bitmap)

      ; Call the superclass INIT method.
   ok = self -> WidgetAtom::INIT (parent, id, _EXTRA=extraKeywords)
   IF NOT ok THEN Message, 'WidgetAtom Initialization failed.'

      ; Print and return status report.

   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       BUTTONWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This is the BUTTONWIDGET object's structure definition code.
;
;*****************************************************************************************************
PRO ButtonWidget__DEFINE, class

   class =  { ButtonWidget, $        ; The BUTTONWIDGET object class definition.
              _menu:0B, $            ; A flag that indicates this is a menu button.
              _bitmap: 0B, $         ; A flag that indicates this is a bitmap button.
              INHERITS WidgetAtom $  ; Inherits WIDGETATOM properties.
            }
END