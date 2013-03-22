;*****************************************************************************************************
;+
; NAME:
;       COMBOBOXWIDGET__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a combobox widget as an object.
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
;       aComboBoxWidget = Obj_New("COMBOBOXWIDGET", parent)
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
;   class = { COMBOBOXWIDGET, $      ; ComboBox widget object class
;             INHERITS WIDGETATOM $  ; Inherits WidgetAtom object class.
;           }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 20 April 2003.
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
;       COMBOBOXWIDGET::ADD
;
; PURPOSE:
;
;       This method overrides the superclass Add method to prevent addition of
;       other widget objects to text objects.
;
; SYNTAX:
;
;       textObject -> Add, object
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
PRO ComboBoxWidget::Add, object

   @cat_pro_error_handler

   IF OBJ_ISA_VALID(object, 'WIDGETATOM') THEN Message, 'Cannot add widget objects to ComboBoxWidgets.'

   self -> Add::WIDGETATOM, object
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       COMBOBOXWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method enables the getting of the COMBOBOXWIDGET object class
;       properties.
;
; SYNTAX:
;
;       aComboBoxWidget -> GetProperty, TEXT=theText
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     DYNAMIC_RESIZE: Returns a 1 if dynamic resizing is on for this widget, and a 0 otherwise.
;
;     NUMBER:         Returns the number of elements in the combobox widget's selection list.
;
;     TEXT:           Returns the current text in the text field of the combobox widget.
;
;     VALUE:          Returns a string or string array containing the combobox widget's current selection list.
;
;     _REF_EXTRA:     Any keywords appropriate for the WIDGETATOM::GetProperty method.
;-
;*****************************************************************************************************
PRO ComboBoxWidget::GetProperty, $
   DYNAMIC_RESIZE=dynamic_resize, $
   NUMBER=number, $
   TEXT=text, $
   VALUE=value, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(dynamic_resize) THEN dynamic_resize = Widget_Info(self._ID, /DYNAMIC_RESIZE)
   IF Arg_Present(number) THEN number = Widget_Info(self._ID, /COMBOBOX_NUMBER)
   IF Arg_Present(text) THEN text = Widget_Info(self._ID, /COMBOBOX_GETTEXT)
   IF Arg_Present(value) THEN Widget_Control, self._ID, GET_VALUE=value

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> WidgetAtom::GetProperty, _EXTRA=extraKeywords
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       COMBOBOXWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the COMBOBOXWIDGET object class
;       properties.
;
; SYNTAX:
;
;       aComboBoxWidget -> SetProperty, DELETE=5
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     ADD:            Set this keyword to a string variable of an item to add to the ComboBox list of items.
;                     By default, the item is added to the end of the list, unless INDEX is specified.
;
;     DELETE:         Set this keyword to the index of an item to be deleted from the ComboBox list of items.
;
;     DYNAMIC_RESIZE: Set this keyword to enable dynamic resizing for this widget.
;
;     INDEX:          Set this keyword to the index where addtional items should be ADDed to the list.
;
;     SELECTION:      Set this keyword to the index of an item to be the current ComboBox selection.
;
;     VALUE:          Set this variable to a string, or array of strings, containing the ComboBox selections.
;
;     _EXTRA:         Any keywords appropriate for the WIDGETATOM::SetProperty method.
;-
;*****************************************************************************************************
PRO ComboBoxWidget::SetProperty, $
   ADD=add, $
   DELETE=delete, $
   DYNAMIC_RESIZE=dynamic_resize, $
   INDEX=index, $
   SELECTION=selection, $
   VALUE=value, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Set the keywords, if available.

   IF N_Elements(index) EQ 0 THEN index = -1 ; Add to end of list.
   IF N_Elements(add) NE 0 THEN Widget_Control, self._ID, COMBOBOX_ADDITEM=add, COMBOBOX_INDEX=index
   IF N_Elements(delete) NE 0 THEN Widget_Control, self._ID, COMBOBOX_DELETEITEM=delete
   IF N_Elements(dynamic_resize) NE 0 THEN Widget_Control, self._ID, DYNAMIC_RESIZE=dynamic_resize
   IF N_Elements(selection) NE 0 THEN Widget_Control, self._ID, SET_COMBOBOX_SELECT=selection
   IF N_Elements(value) NE 0 THEN Widget_Control, self._ID, SET_VALUE=value

   ; Call the superclass method.
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> WIDGETATOM::SetProperty, _EXTRA=extraKeywords
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       COMBOBOXWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the COMBOBOXWIDGET object class destructor method.
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
PRO ComboBoxWidget::CLEANUP

   @cat_pro_error_handler
   self -> WIDGETATOM::CLEANUP
   self -> report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       COMBOBOXWIDGET::INIT
;
; PURPOSE:
;
;       This is the COMBOBOXWIDGET object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     parent          The parent object reference for this object widget.
;
; KEYWORDS:
;
;     DYNAMIC_RESIZE: Set this keyword to enable dynamic resizing for this widget.
;
;     EDITABLE:       Set this keyword to create an editable combobox.
;
;     FONT:           Set this variable to the name of the font to use for the ComboBox elements.
;
;     SCR_XSIZE:      Set the screen X size of the base to this many pixels. (Use discouraged.)
;
;     SCR_YSIZE:      Set the screen Y size of the base to this many pixels. (Use discouraged.)
;
;     SELECTION:      Set this keyword to the index of an item in the VALUE list to be the
;                     current ComboBox selection.
;
;     UNITS:          Set to 0 (the default) to specify sizes in pixels. 1 - inches, 2 - centimeters.
;
;     VALUE:          Set this variable to a string, or array of strings, containing the ComboBox selections.
;
;     XOFFSET:        The X offset of the widget, in UNITS.
;
;     XSIZE:          The desired X size of the widget, in UNITS.
;
;     YOFFSET:        The Y offset of the widget, in UNITS.
;
;     YSIZE:          The desired Y size of the widget, in UNITS.
;
;     _EXTRA:         Any keywords appropriate for the WIDGETATOM::INIT method.
;-
;*****************************************************************************************************
FUNCTION ComboBoxWidget::INIT, $
   parent, $
   DYNAMIC_RESIZE=dynamic_resize, $
   EDITABLE=editable, $
   FONT=font, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   SELECTION=selection, $
   VALUE=value, $
   UNITS=units, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

     ; Create the combobox widget.

   parent -> GetProperty, ID=parentID
   id = WIDGET_COMBOBOX (parentID, $
      DYNAMIC_RESIZE=dynamic_resize, $
      EDITABLE=editable, $
      FONT=font, $
      SCR_XSIZE=scr_xsize, $
      SCR_YSIZE=scr_ysize, $
      UNITS=units, $
      VALUE=value, $
      XOFFSET=xoffset, $
      XSIZE=xsize, $
      YOFFSET=yoffset, $
      YSIZE=ysize)

      ; Call the superclass INIT method.

   ok = self -> WidgetAtom::INIT (parent, id, _EXTRA=extraKeywords)
   IF NOT ok THEN Message, 'WidgetAtom Initialization failed.'

   IF N_Elements(selection) NE 0 THEN Widget_Control, self._ID, SET_COMBOBOX_SELECT=selection

   self -> Report, /Completed

   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       COMBOBOXWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the COMBOBOXWIDGET object.
;
;*****************************************************************************************************
PRO ComboBoxWidget__DEFINE, class

   class = { COMBOBOXWIDGET, $      ; ComboBox widget object class
             INHERITS WIDGETATOM $  ; Inherits WidgetAtom object class.
           }
END
