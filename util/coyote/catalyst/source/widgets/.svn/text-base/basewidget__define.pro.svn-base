;*****************************************************************************************************
;+
; NAME:
;       BASEWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to implement a base widget as an object. If you wish
;       to create a top-level base widget object, use the TOPLEVELBASE object.
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
;       baseWidget = Obj_New("baseWidget", parentObjectID)
;
; SUPERCLASSES:
;
;       WIDGETATOM
;       CATATOM
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;    class = { BASEWIDGET, $           ; The BASEWIDGET object class name.
;              INHERITS WidgetAtom, $  ; Subclassed from WIDGETATOM.
;              _Map: 0L $              ; A flag that indicates whether this base is mapped or not.
;            }
;
;
; MODIFICATION_HISTORY:
;
;       Written by: David W.Fanning, 28 June 2002.
;       Added TITLE keyword in SetProperty and GetProperty methods, 23 March 2005. DWF.
;       Added TOOLBAR keyword in INIT method. 26 September 2005. DWF.
;       Made a change in ADD method to keep the basewidget from adding itself as a parent
;          twice to the object being added. 16 July 2007. DWF.
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
;       BASEWIDGET::ADD
;
; PURPOSE:
;
;       This method adds an object widget to the base object widget. The
;       added object widget must be subclassed from the WIDGETATOM object.
;
; SYNTAX:
;
;       self -> Add, object
;
; ARGUMENTS:
;
;       object: The object to add to the container. The object must be
;          subclassed from the WIDGETATOM object. (Required)
;
; KEYWORDS:
;
;       _EXTRA: Any keyword appropriate for superclass ADD methods.
;
;-
;*****************************************************************************************************
PRO BaseWidget::Add, object, _EXTRA=extraKeywords

      ; Error handling.

   @cat_pro_error_handler

   IF N_Elements(object) EQ 0 THEN Message, 'Required object is missing.'
   IF OBJ_ISA_VALID(object, 'WIDGETATOM') EQ 0 THEN $
      Message, 'Supplied object is invalid or not subclassed from WIDGETATOM.'

   ; If the thing you are adding is itself a TOPLEVELBASE, then you have to make
   ; self a group leader, not a parent.

; Removed the ELSE clause because otherwise
   IF OBJ_ISA_VALID(object, 'TOPLEVELBASE') THEN object -> SetProperty, Group_Leader=self ELSE $
      self -> WidgetAtom::Add, object, _EXTRA=extraKeywords
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       BASEWIDGET::DRAW
;
; PURPOSE:
;
;       This method realizes the BaseWidget object if it has not already been realized.
;
; SYNTAX:
;
;       self -> Draw
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       _EXTRA: Any keyword appropriate for the supercalss Draw methods.
;-
;*****************************************************************************************************
PRO BaseWidget::Draw, _EXTRA=extraKeywords

      ; Error handling.
   @cat_pro_error_handler

      ; If the widget is visible, then invoke draw methods to realize.
   IF self._invisible EQ 0 THEN self -> WidgetAtom::Draw, _EXTRA=extraKeywords

      ; Report completion
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       BASEWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to obtain the BaseWidget object's properties
;
; SYNTAX:
;
;       aBaseWidget -> GetProperty, Map=ismapped
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       CONTEXT_EVENTS: Returns a 1 if context events are on, a 0 otherwise.
;
;       MAP:            Returns a 0 or 1 to indicate if the current base widget hierarchy is
;                       mapped (1) or not (0).
;
;       TITLE:          The title of the base widget. Applies only to base widgets
;                       that are children of tab widgets.
;
;       _EXTRA:         Any keywords appropriate for the superclass GetProperty methods.
;-
;*****************************************************************************************************
PRO BaseWidget::GetProperty, $
   CONTEXT_EVENTS=context_events, $
   MAP=map, $
   TITLE=title, $
   _REF_EXTRA=extraKeywords

      ; Error handling.
   @cat_pro_error_handler

      ; Get the properties.
   map = self._map

   IF Arg_Present(context_events) THEN context_events = Widget_Info(self._ID, /Context_Events)
   IF Arg_Present(title) THEN title = self._baseTitle

      ; Call the superclass getproperty method if required
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> WidgetAtom::GetProperty, _EXTRA=extraKeywords

      ; Report completion
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       BASEWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the BaseWidget object's properties
;
; SYNTAX:
;
;       aBaseWidget -> SetProperty, Map=1
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       CONTEXT_EVENTS:    Set to 1 to turn context events on for the base widget.
;
;       KBRD_FOCUS_EVENTS: Set to 1 to turn keyboard focus events on for the base widget.
;
;       MAP:               Set to 1 to map a base widget hierarchy. Set to 0 to unmap.
;
;       TITLE:             Sets the title of the base widget. Applies only to base widgets
;                          that are children of tab widgets.
;
;       _EXTRA:            Any keywords appropriate for the superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO BaseWidget::SetProperty, $
   CONTEXT_EVENTS=context_events, $
   MAP=map, $
   KBRD_FOCUS_EVENTS=kbrd_focus_events, $
   TITLE=title, $
   _EXTRA=extraKeywords

      ; Error handling.

   @cat_pro_error_handler

   IF N_Elements(map) NE 0 THEN $
   BEGIN
      self._map = Keyword_Set(map)
      WIDGET_CONTROL, self._id, MAP=self._map
   ENDIF

   IF N_Elements(kbrd_focus_events) NE 0 THEN $
      WIDGET_CONTROL, self._id, KBRD_FOCUS_EVENTS=Keyword_Set(kbrd_focus_events)

   IF N_Elements(context_events) NE 0 THEN $
      WIDGET_CONTROL, self._id, CONTEXT_EVENTS=Keyword_Set(context_events)

   IF N_Elements(title) NE 0 THEN BEGIN
       WIDGET_CONTROL, self._id, BASE_SET_TITLE=title
       self._baseTitle = title
   ENDIF

   self -> WIDGETATOM::SetProperty, _Extra=extrakeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       BASEWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the BASEWIDGET object class destructor method.
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
PRO BaseWidget::CLEANUP

      ; Error handling.
   @cat_pro_error_handler

      ; Call superclass CLEANUP methods.
   self -> WidgetAtom::CLEANUP

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       BASEWIDGET::INIT
;
; PURPOSE:
;
;       This is the BASEWIDGET object class initialization method
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
;       CONTEXT_EVENTS:    Set this keyword to turn context events on for this widget object.
;
;       CONTEXT_MENU:      Set this keyword to create a base widget that can be used to hold
;                          context-sensitive menu items.
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
;       TOOLBAR:           Setting this keyword allows the base to have a slightly different appearance
;                          on Motif platforms. The keyword is ignored on Windows.
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
FUNCTION BaseWidget::INIT, parent, $
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
   CONTEXT_EVENTS=context_events, $
   CONTEXT_MENU=context_menu, $
   EXCLUSIVE=exclusive, $
   FRAME=frame, $
   GRID_LAYOUT=grid_layout, $
   KBRD_FOCUS_EVENTS=kbrd_focus_events, $
   MAP=map, $
   NONEXCLUSIVE=nonexclusive, $
   NOTIFY_REALIZE=notify_realize, $
   ROW=row, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   SCROLL=scroll, $
   SPACE=space, $
   TITLE=title, $
   TOOLBAR=toolbar, $
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

    COMMON REALIZE_ID, r_id

      ; Error handling.

   @cat_func_error_handler

      ; If there is a parent, make sure it is a valid BASEWIDGET OR TABWIDGET class object.
   IF (OBJ_ISA_VALID(parent, "BASEWIDGET") EQ 0) AND (OBJ_ISA_VALID(parent, "TABWIDGET") EQ 0) THEN $
      Message, 'Parent object invalid or not of type BASEWIDGET.'


      ; Create the base widget.

   parent -> GetProperty, ID=parentID
   IF Keyword_Set(notify_realize) THEN notify_string = 'CatRealizeNotify' ELSE notify_string=''
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
        CONTEXT_EVENTS=context_events, $
        CONTEXT_MENU=context_menu, $
        COLUMN=column, $
        EXCLUSIVE=exclusive, $
        FRAME=frame, $
        GRID_LAYOUT=grid_layout, $
        KBRD_FOCUS_EVENTS=kbrd_focus_events, $
        MAP=map, $
        NONEXCLUSIVE=nonexclusive, $
        NOTIFY_REALIZE=notify_string, $
        ROW=row, $
        SCR_XSIZE=scr_xsize, $
        SCR_YSIZE=scr_ysize, $
        SCROLL=scroll, $
        SPACE=space, $
        TITLE=title, $
        TOOLBAR=toolbar, $
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

   ok = self -> WidgetAtom::INIT (parent, id, NOTIFY_REALIZE=notify_realize, _EXTRA=extrakeywords)
   IF NOT ok THEN Message, 'WIDGETATOM initialization failed. Returning.'

      ; Populate object.

   self._map = Keyword_Set(map)
   IF N_Elements(title) NE 0 THEN self._basetitle = title

     ; Clean up and return status.

   self -> Report, /Completed
   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       BASEWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the BASEWIDGET object class definition.
;       The BASEWIDGET object is subclassed from the WIDGETATOM object.
;
;*****************************************************************************************************
PRO BaseWidget__DEFINE, class

   class = { BASEWIDGET, $           ; The BASEWIDGET object class name.
              INHERITS WidgetAtom, $  ; Subclassed from WIDGETATOM.
              _baseTitle: "", $       ; A title for the base.
              _Map: 0L $              ; A flag that indicates whether this base is mapped or not.
            }
END