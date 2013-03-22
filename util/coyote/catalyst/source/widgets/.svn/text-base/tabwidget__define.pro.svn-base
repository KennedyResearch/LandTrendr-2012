;*****************************************************************************************************
;+
; NAME:
;       TABWIDGET__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a tab widget as an object.
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
; CALLING_SYNTAX:
;
;       aTabWidget = Obj_New("TABWIDGET", parent)
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
;   class = { TABWIDGET, $           ; Tab widget object class
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
;       TABWIDGET::ADD
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
;       _Extra: Any keywords for the superclass object.
;
;-
;*****************************************************************************************************
PRO TabWidget::Add, object, _Extra=extraKeywords

   @cat_pro_error_handler

   IF OBJ_ISA_VALID(object, 'BASEWIDGET') EQ 0 THEN Message, 'Only BaseWidgets can be added to TabWidgets.'

   self -> WIDGETATOM::Add, object, _Extra=extraKeywords
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       TABWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method enables the getting of the TABWIDGET object class
;       properties.
;
; SYNTAX:
;
;       aTabWidget -> GetProperty, CURRENT=tab_current
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     CURRENT:     The index value of the current or visible tab.
;
;     MULTILINE:   The current value of the MULTILINE property.
;
;     NUMBER:      The number of tabs contained in the tab widget.
;
;     _REF_EXTRA:  Any keywords appropriate for the WIDGETATOM::GetProperty method.
;-
;*****************************************************************************************************
PRO TabWidget::GetProperty, $
   CURRENT=tab_current, $
   MULTILINE=tab_multiline, $
   NUMBER=tab_number, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(tab_current) THEN tab_current = Widget_Info(self._ID, /TAB_CURRENT)
   IF Arg_Present(tab_multiline) THEN tab_multiline = Widget_Info(self._ID, /TAB_MULTILINE)
   IF Arg_Present(tab_number) THEN tab_number = Widget_Info(self._ID, /TAB_NUMBER)

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> WIDGETATOM::GetProperty, _EXTRA=extraKeywords
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       TABWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the TABWIDGET object class
;       properties.
;
; SYNTAX:
;
;       aTabWidget -> SetProperty, CURRENT=4
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     CURRENT:       Set this keyword equal to the index of the tab to be set as the current
;                    (visible) tab. If the index is invalid, it is quietly ignored.
;
;     MULTILINE:     Set this keyword to control how multiple tabs appear. The value is platform
;                    dependent. See the IDL on-line documentation for WIDGET_TAB for details.
;
;     _EXTRA:       Any keywords appropriate for the WIDGETATOM::SetProperty method.
;-
;*****************************************************************************************************
PRO TabWidget::SetProperty, $
   CURRENT=tab_current, $
   MULTILINE=tab_multiline, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Set the keywords, if available.

   IF N_Elements(tab_current) NE 0 THEN Widget_Control, self._ID, SET_TAB_CURRENT=tab_current
   IF N_Elements(tab_multiline) NE 0 THEN Widget_Control, self._ID, SET_TAB_MULTILINE=tab_multiline

   ; Call the superclass method.
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> WIDGETATOM::SetProperty, _EXTRA=extraKeywords
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       TABWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the TABWIDGET object class destructor method.
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
PRO TabWidget::CLEANUP

   @cat_pro_error_handler
   self -> WIDGETATOM::CLEANUP
   self -> report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       TABWIDGET::INIT
;
; PURPOSE:
;
;       This is the TABWIDGET object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     parent      The parent object reference for this object widget.
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
;     LOCATION:     Set this keyword to an integer that specifies which edge of the widget will contain
;                   the tabs. Possible values are:
;
;                   0 - Tabs are placed along the top of the widget. (The default.)
;                   1 - Tabs are placed along the bottom of the widget.
;                   2 - Tabs are placed along the left edge of the widget. (Text is displayed vertically.)
;                   3 - Tabs are placed along the right edge of the widget. (Text is displayed vertically.)
;
;     MULTILINE:    This keyword controls how tabs appear when all of the tabs do not fit on the widget
;                   in a single row. The behavior and value of the keyword differs in Windows and in Motif.
;                   Check the IDL on-line documentation for WIDGET_TAB for more detail. Set this keyword to
;                   1 on Windows and the tabs will be displayed with scroll bars. On Motif, set this keyword
;                   to the maximum number of tabs for each row of tabs.
;
;     SCR_XSIZE:    Set the screen X size of the base to this many pixels. (Use discouraged.)
;
;     SCR_YSIZE:    Set the screen Y size of the base to this many pixels. (Use discouraged.)
;
;     UNITS:        The units for measurments. The default is 0 for pixels. Other values are
;                   1 for inches, and 2 for centimeters.
;
;     XOFFSET:      The horizontal space (pixels) from upper left corner of the display.
;
;     XSIZE:        The X size of the widget.
;
;     YOFFSET:      The vertical space (pixels) from upper left corner of the display.
;
;     YSIZE:        The Y size of the widget.
;
;     _EXTRA:       Any keywords appropriate for the WIDGETATOM::INIT method.
;-
;*****************************************************************************************************
FUNCTION TabWidget::INIT, parent, $
   ALIGN_BOTTOM=align_bottom,$
   ALIGN_CENTER=align_center, $
   ALIGN_LEFT=align_left, $
   ALIGN_RIGHT=align_right, $
   ALIGN_TOP=align_top, $
   LOCATION=location, $
   MULTILINE=multiline, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   UNITS=units, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

     ; Create the tab widget.

   parent -> GetProperty, ID=parentID
   id = WIDGET_TAB (parentID, $
   ALIGN_BOTTOM=align_bottom,$
   ALIGN_CENTER=align_center, $
   ALIGN_LEFT=align_left, $
   ALIGN_RIGHT=align_right, $
   ALIGN_TOP=align_top, $
   LOCATION=location, $
   MULTILINE=multiline, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   UNITS=units, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   YOFFSET=yoffset, $
   YSIZE=ysize)

      ; Call the superclass INIT method.

   ok = self -> WidgetAtom::INIT (parent, id, _EXTRA=extraKeywords)
   IF NOT ok THEN Message, 'WidgetAtom Initialization failed.'

   self -> Report, /Completed

   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       TABWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the TABWIDGET object.
;
;*****************************************************************************************************
PRO TabWidget__DEFINE, class

   class = { TABWIDGET, $           ; Tab widget object class
             INHERITS WIDGETATOM $  ; Inherits WidgetAtom object class.
           }
END

;*****************************************************************************************************
;
; NAME:
;       TABWIDGET TEST Program
;
; PURPOSE:
;
;       This is program to test the TABWIDGET object.
;
;*****************************************************************************************************

