;*****************************************************************************************************
;+
; NAME:
;       CONTEXTMENUBASE__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide a starting template
;       for object creation.
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
;       theObject = Obj_New("CONTEXTMENUBASE")
;
; BASEWIDGETES:
;
;       WIDGETBASE
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { CONTEXTMENUBASE, $
;             INHERITS BASEWIDGET $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, April 17, 2003.
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
;       CONTEXTMENUBASE::ADD
;
; PURPOSE:
;
;       This method is where you can screen what kinds of objects are
;       added to this object's hierarchy. The method is not always needed.
;       If you do create it, be CERTAIN to call the superclass ADD method
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
;     _EXTRA:     Any keywords appropriate for the superclass Add method.
;-
;*****************************************************************************************************
PRO ContextMenuBase::Add, object, _EXTRA=extraKeywords

   @cat_pro_error_handler

   self -> BASEWIDGET::Add, object, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CONTEXTMENUBASE::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain CONTEXTMENUBASE properties. Be sure
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
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO ContextMenuBase::GetProperty, _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> BASEWIDGET::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CONTEXTMENUBASE::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the CONTEXTMENUBASE object's properties. Be sure
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
;     _EXTRA:     Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO ContextMenuBase::SetProperty, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> BASEWIDGET::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CONTEXTMENUBASE::INIT
;
; PURPOSE:
;
;       This is the CONTEXTMENUBASE object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       theParent:         An object reference to a WIDGETATOM-subclassed object.
;                          This is the same as using the PARENT keyword.
;
; KEYWORDS:
;
;       ALIGN_BOTTOM:      Set this keyword to align the base widget with the bottom of the parent base.
;
;       ALIGN_CENTER:      Set this keyword to align the base widget with the center of the parent base.
;
;       ALIGN_LEFT:        Set this keyword to align the base widget with the left of the parent base.
;
;       ALIGN_RIGHT:       Set this keyword to align the base widget with the right of the parent base.
;
;       ALIGN_TOP:         Set this keyword to align the base widget with the top of the parent base.
;
;       BASE_ALIGN_BOTTOM: Set this keyword to align the children of this base to the bottom of the base widget.
;
;       BASE_ALIGN_CENTER: Set this keyword to align the children of this base to the center of the base widget.
;
;       BASE_ALIGN_LEFT:   Set this keyword to align the children of this base to the left of the base widget.
;
;       BASE_ALIGN_RIGHT:  Set this keyword to align the children of this base to the right of the base widget.
;
;       BASE_ALIGN_TOP:    Set this keyword to align the children of this base to the top of the base widget.
;
;       COLUMN:            Arrange the children of this base into this many columns.
;
;       EXCLUSIVE:         Set this keyword to make an exclusive button base.
;
;       FRAME:             Create a frame this many pixels wide around base.
;
;       GRID_LAYOUT:       Set this keyword to lay children out in equally-spaced grid.
;
;       KBRD_FOCUS_EVENTS: Set this keyworld to enable keyboard focus events.
;
;       MAP:               Set this keyword to map (1) or unmap (0) the base. Mapped by default.
;
;       NONEXCLUSIVE:      Set this keyword to make an non-exclusive button base.
;
;       PARENT:            An object reference to a WIDGETATOM-subclassed object.
;
;       ROW:               Arrange children of this base into this many rows.
;
;       SCR_XSIZE:         Set the screen X size of the base to this many pixels. (Use discouraged.)
;
;       SCR_YSIZE:         Set the screen Y size of the base to this many pixels. (Use discouraged.)
;
;       SCROLL:            Set this keyword to add scroll bars to the base widget.
;
;       SPACE:             Set this keyword to the number of pixels between children in the base.
;                          Ignored for exclusive and non-exclusive bases.
;
;       TITLE:             Set this keyword to a string that will be the title for this base widget.
;                          A title becomes the tab value if the base widget is added to a tab widget.
;                          A title only makes sense in this context.
;
;       UNITS:             The units for measurments. The default is 0 for pixels. Other values are
;                          1 for inches, and 2 for centimeters.
;
;       X_SCROLL_SIZE:     The X size (pixels) of the scrollable window area.
;
;       XOFFSET:           The horizontal space (pixels) from upper left corner of the display.
;
;       XPAD:              The amount of horizontal space (pixels) to add to edges of children.
;
;       XSIZE:             The X size of the widget.
;
;       Y_SCROLL_SIZE:     The Y size (pixels) of the scrollable window area
;
;       YPAD:              The amount of vertical space (pixels) to add to edges of children.
;
;       YOFFSET:           The vertical space (pixels) from upper left corner of the display.
;
;       YSIZE:             The Y size of the widget.
;
;       _EXTRA:            Any keyword appropriate for the superclass INIT methods.
;-
;*****************************************************************************************************
FUNCTION ContextMenuBase::INIT, parent, $
   ALIGN_BOTTOM=align_bottom,$
   ALIGN_CENTER=align_center, $
   ALIGN_LEFT=align_left, $
   ALIGN_RIGHT=align_right, $
   ALIGN_TOP=align_top, $
   BASE_ALIGN_BOTTOM=base_align_bottom, $
   BASE_ALIGN_TOP=base_align_top, $
   BASE_ALIGN_RIGHT=base_align_right, $
   BASE_ALIGN_LEFT=base_align_left, $
   BASE_ALIGN_CENTER=base_align_center, $
   COLUMN=column, $
   EXCLUSIVE=exclusive, $
   FRAME=frame, $
   GRID_LAYOUT=grid_layout, $
   KBRD_FOCUS_EVENTS=kbrd_focus_events, $
   MAP=map, $
   NONEXCLUSIVE=nonexclusive, $
   ROW=row, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   SCROLL=scroll, $
   SPACE=space, $
   TITLE=title, $
   UNITS=units, $
   X_SCROLL_SIZE=x_scroll_size, $
   XPAD=xpad, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   Y_SCROLL_SIZE=y_scroll_size, $
   YPAD=ypad, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
    _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

      ; If there is a parent, make sure it is a valid BASEWIDGET OR TABWIDGET class object.
   wrongParent = 1
   IF OBJ_ISA_VALID(parent, "BASEWIDGET") THEN wrongParent = 0
   IF OBJ_ISA_VALID(parent, "DRAWWIDGET") THEN wrongParent = 0
   IF OBJ_ISA_VALID(parent, "LISTWIDGET") THEN wrongParent = 0
   IF OBJ_ISA_VALID(parent, "TEXTWIDGET") THEN wrongParent = 0
   IF OBJ_ISA_VALID(parent, "TREEWIDGET") THEN wrongParent = 0
   IF wrongParent THEN Message, 'The parent of this CONTEXTMENUBASE widget is not an allowed parent.'

      ; Create the base widget.

   parent -> GetProperty, ID=parentID
   id = WIDGET_BASE (parentID, $
        ALIGN_BOTTOM=align_bottom,$
        ALIGN_CENTER=align_center, $
        ALIGN_LEFT=align_left, $
        ALIGN_RIGHT=align_right, $
        ALIGN_TOP=align_top, $
        BASE_ALIGN_BOTTOM=base_align_bottom, $
        BASE_ALIGN_TOP=base_align_top, $
        BASE_ALIGN_RIGHT=base_align_right, $
        BASE_ALIGN_LEFT=base_align_left, $
        BASE_ALIGN_CENTER=base_align_center, $
        CONTEXT_MENU=1, $
        COLUMN=column, $
        EXCLUSIVE=exclusive, $
        FRAME=frame, $
        GRID_LAYOUT=grid_layout, $
        KBRD_FOCUS_EVENTS=kbrd_focus_events, $
        MAP=map, $
        NONEXCLUSIVE=nonexclusive, $
        ROW=row, $
        SCR_XSIZE=scr_xsize, $
        SCR_YSIZE=scr_ysize, $
        SCROLL=scroll, $
        SPACE=space, $
        TITLE=title, $
        UNITS=units, $
        X_SCROLL_SIZE=x_scroll_size, $
        XOFFSET=xoffset, $
        XPAD=xpad, $
        XSIZE=xsize, $
        Y_SCROLL_SIZE=y_scroll_size, $
        YOFFSET=yoffset, $
        YPAD=ypad, $
        YSIZE=ysize)

      ; Call the super class INIT method and set the widget object properties.

   ok = self -> WidgetAtom::INIT (parent, id, _EXTRA=extrakeywords)
   IF NOT ok THEN Message, 'WIDGETATOM initialization failed. Returning.'

      ; Populate object.

   self._map = Keyword_Set(map)

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       CONTEXTMENUBASE CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the CONTEXTMENUBASE object.
;
;*****************************************************************************************************
PRO ContextMenuBase__DEFINE, class

   class = { CONTEXTMENUBASE, $
             INHERITS BASEWIDGET $
           }

END


;*****************************************************************************************************
;
; NAME:
;       CONTEXTMENUBASE TEST PROGRAM
;
; PURPOSE:
;
;       This is the test program for the CONTEXTMENUBASE object. It may not
;       be required in your object.
;
;*****************************************************************************************************
PRO ContextMenuBase_Test

END