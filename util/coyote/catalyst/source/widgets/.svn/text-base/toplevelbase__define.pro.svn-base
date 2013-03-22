;*****************************************************************************************************
;+
; NAME:
;       TOPLEVELBASE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a top-level base widget as an object. If you wish
;       to create a non-top-level base widget object, use the BASEWIDGET object.
;
; AUTHOR:
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
;       topLevelBase = Obj_New("TopLevelBase")
;
; SUPERCLASSES:
;
;       BASEWIDGET
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;    class = { TOPLEVELBASE, $         ; The TOPLEVELBASE object class name.
;              INHERITS BaseWidget, $  ; Subclassed from BASEWIDGET.
;              _TLB_Frame_Attr: 0 $    ; A flag that indicates the current top-level base frame attributes.
;            }
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
;       TOPLEVELBASE::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to obtain the TopLevelBase object's properties
;
; SYNTAX:
;
;       aTopLevelBase -> GetProperty, Map=ismapped
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       ICONIFY_EVENTS: Set to 1 if ICONIFY events are set for this top-level base widget.
;
;       MAP:            Returns a 0 or 1 to indicate if the current base widget hierarchy is
;                       mapped (1) or not (0).
;
;       MODAL:          Returns a 0 or 1 to indicate if the current base widget hierarchy is modal (1) or not (0).
;
;       MOVE_EVENTS:    Set to 1 if move events are set for this top-level base widget.
;
;       SIZE_EVENTS:    Set to 1 if size events are set for this top-level base widget.
;
;       _EXTRA:         Any keyword appropriate for the supercalss Draw methods.
;-
;*****************************************************************************************************
PRO TopLevelBase::GetProperty, $
   ICONIFY_EVENTS=iconify_events, $
   MAP=map, $
   MODAL=modal, $
   MOVE_EVENTS=move_events, $
   SIZE_EVENTS=size_events, $
   _REF_EXTRA=extraKeywords

      ; Error handling.
   @cat_pro_error_handler

      ; Set the properties.
   map = self._map

   IF Arg_Present(iconify_events) THEN iconify_events = Widget_Info(self._ID, /TLB_ICONIFY_EVENTS)
   IF Arg_Present(modal) THEN modal = Widget_Info(self._ID, /MODAL)
   IF Arg_Present(move_events) THEN move_events = Widget_Info(self._ID, /TLB_MOVE_EVENTS)
   IF Arg_Present(size_events) THEN size_events = Widget_Info(self._ID, /TLB_SIZE_EVENTS)

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> WidgetAtom::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TOPLEVELBASE::POSITION
;
; PURPOSE:
;
;       This is a utility method to position the top-level base
;       on the display at an arbitrary location. By default the
;       widget is centered on the display.
;
;
; SYNTAX:
;
;       tlbObject -> POSITION [,x, y, NOCENTER=noCenter]
;
; INPUTS:
;
;       x:  Set this equal to a normalized position for the center
;           of the widget as measured from the left-hand side of the screen.
;           The default value is 0.5 (the center)  Setting this equal to 1.0
;           places the widget at the far right-hand side of the screen.
;
;       y:  Set this equal to a normalized position for the center
;           of the widget as measured from the top of the screen.
;           The default value is 0.5 (the center) Setting this equal to 1.0
;           places the widget at the top of the screen.
;
; KEYWORDS:
;
;      DEVICE:   Normally, the x and y parameters are specified in normalized
;                coordinates. If this keyword is set, they are taken to be in DEVICE
;                coordinates.
;
;      NOCENTER: Typically, the center of the widget is positioned at the
;                x, y point. Setting this keyword, forces the top-left
;                corner to be at x, y.
;
; MODIFICATION HISTORY:
;
;       Written by:  Dick Jackson, 12 Dec 98.
;       Modified to use device-independent Get_Screen_Size
;            function. 31 Jan 2000. DWF.
;       Added x, y, NOCENTER and run-off protection. 26 Jan 2001. BT.
;       Converted to be an object method. DB & DWF, 13th March 2003
;
;-
;*****************************************************************************************************
PRO TopLevelBase::Position, x, y, Device=device, NoCenter=nocenter

    IF N_Elements(x) EQ 0 THEN xc = 0.5 ELSE xc = Float(x[0])
    IF N_Elements(y) EQ 0 THEN yc = 0.5 ELSE yc = Float(y[0])
    center = 1 - Keyword_Set(nocenter)
    
    screenSize = Get_Screen_Size()
    IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
    IF ~Keyword_Set(device) THEN BEGIN ; Normalized coordinates
       xCenter = screenSize[0] * xc
       yCenter = screenSize[1] * yc
    ENDIF ELSE BEGIN ; Device coordinates
       xCenter = xc
       yCenter = yc
    ENDELSE
    
    ; Get the screen sizes of the TLB. Divide by 2.
    geom = Widget_Info(self._ID, /Geometry)
    xHalfSize = geom.Scr_XSize / 2
    yHalfSize = geom.Scr_YSize / 2
    
    ; Are you centering, or placing upper-left corner?
    IF center THEN BEGIN
       XOffset = 0 > (xCenter - xHalfSize) < (screenSize[0] - geom.Scr_Xsize)
       YOffset = 0 > (yCenter - yHalfSize) < (screenSize[1] - geom.Scr_Ysize)
    ENDIF ELSE BEGIN
       XOffset = xcenter
       YOffset = ycenter
    ENDELSE
    
    ; Set the offsets.
    Widget_Control, self._ID, XOffset=XOffset, YOffset=YOffset

END


;*****************************************************************************************************
;+
; NAME:
;       TOPLEVELBASE::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the TopLevelBase object's properties
;
; SYNTAX:
;
;       aTopLevelBase -> SetProperty, Map=1
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       ICONIFY_EVENTS:      Set this keyword to turn iconify events on.
;
;       KBRD_FOCUS_EVENTS:   Set to 1 to turn keyboard focus events on for the base widget.
;
;       KILL_REQUEST_EVENTS: Set this keyword to turn top-level base kill request events on.
;
;       MAP:                 Set to 1 to map a base widget hierarchy. Set to 0 to unmap.
;
;       MOVE_EVENTS:         Set this keyword to turn move event requests on.
;
;       SIZE_EVENTS:         Set this keyword to turn re-size events on.
;
;       TITLE:               The title of the top-level base.
;
;       _EXTRA:              Any keyword appropriate for the supercalss Draw methods.
;-
;*****************************************************************************************************
PRO TopLevelBase::SetProperty, $
   ICONIFY_EVENTS=iconify_events, $
   KBRD_FOCUS_EVENTS=kbrd_focus_events, $
   KILL_REQUEST_EVENTS=kill_request_events, $
   MAP=map, $
   MOVE_EVENTS=move_events, $
   SIZE_EVENTS=size_events, $
   TITLE=title, $
   _EXTRA=extraKeywords

      ; Error handling.

   @cat_pro_error_handler

   IF N_Elements(map) NE 0 THEN $
   BEGIN
      self._map = Keyword_Set(map)
      WIDGET_CONTROL, self._id, MAP=self._map
   ENDIF

   IF N_Elements(iconify_events) NE 0 THEN $
      WIDGET_CONTROL, self._id, TLB_ICONIFY_EVENTS=Keyword_Set(iconify_events)
   IF N_Elements(kbrd_focus_events) NE 0 THEN $
      WIDGET_CONTROL, self._id, KBRD_FOCUS_EVENTS=Keyword_Set(kbrd_focus_events)
   IF N_Elements(kill_request_events) NE 0 THEN $
      WIDGET_CONTROL, self._id, TLB_KILL_REQUEST_EVENTS=Keyword_Set(kill_request_events)
   IF N_Elements(move_events) NE 0 THEN $
      WIDGET_CONTROL, self._id, TLB_MOVE_EVENTS=Keyword_Set(move_events)
   IF N_Elements(size_events) NE 0 THEN $
      WIDGET_CONTROL, self._id, TLB_SIZE_EVENTS=Keyword_Set(size_events)

   IF N_Elements(title) NE 0 THEN $
      WIDGET_CONTROL, self._id, TLB_SET_TITLE=title

   self -> WIDGETATOM::SetProperty, _Extra=extrakeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       TOPLEVELBASE::Draw
;
; PURPOSE:
;
;       This method is used to realize and draw the widget hierarchy. It also starts
;       XMANAGER to managing the widget hierarchy.
;
; SYNTAX:
;
;       aTopLevelBase -> Draw
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       BLOCK:         Set this keyword to create a blocking widget hierarchy.
;
;       CENTER:        Set this keyword to center the TLB before display.
;
;       REGISTER_NAME: The name by which the program will be registered with XManager.
;
;       GROUP_LEADER:  The widget identifier of a group leader for this widget hierarchy.
;
;       JUST_REGISTER: Set his keyword to just register with XManager, but not to fire it up.
;
;       _EXTRA:        To pass additional keywords BaseWidget::Draw
;-
;*****************************************************************************************************
PRO TopLevelBase::Draw, $
                  CENTER=center, $
                  REGISTER_NAME=register_name, $
                  BLOCK=block, $
                  GROUP_LEADER=group_leader, $
                  JUST_REGISTER=just_register, $
                  XOFFSET=xoffset, YOFFSET=yoffset, $
                  _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Keyword_Set(center) THEN self -> Position, xoffset, yoffset

   ; Get a unique name if one is not provided.
   IF self._register_name EQ "" THEN BEGIN
       IF (N_Elements(register_name) EQ 0) THEN BEGIN
            self._register_name = 'Program_' + StrTrim(self._id, 2)
       ENDIF ELSE self._register_name = register_name
   ENDIF
   
   ; Realize the widget hierarchy.
   IF Widget_Info(self._ID, /Realized) EQ 0 THEN self -> Realize

   ; Call the superclass draw method.
   self -> BaseWidget::Draw, _EXTRA=extraKeywords

   ; Start XMANAGER unless this is a PIXMAPWIDGET.
   IF Obj_Class(self) NE 'PIXMAPWIDGET' THEN BEGIN
      IF Keyword_Set(just_register) THEN BEGIN
         XManager, self._register_name, self._id, JUST_REG=1, /NO_BLOCK, $
            EVENT_HANDLER='cateventdispatcher'
      ENDIF ELSE BEGIN
         XManager, self._register_name, self._id, GROUP_LEADER=group_leader, $
            NO_BLOCK=1-Keyword_Set(block), $
            EVENT_HANDLER='cateventdispatcher'
      ENDELSE
   ENDIF
END


;*****************************************************************************************************
;+
; NAME:
;       TOPLEVELBASE::Realize
;
; PURPOSE:
;
;       This method is used to realize the widget hierarchy.
;
; SYNTAX:
;
;       aTopLevelBase -> Realize
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
PRO TopLevelBase::Realize

      ; Realize the widget hierarchy.

   IF self._centerTLB THEN self -> Position
   Widget_Control, self._ID, /REALIZE

END


;*****************************************************************************************************
;+
; NAME:
;       TOPLEVELBASE::CLEANUP
;
; PURPOSE:
;
;       This is the TOPLEVELBASE object class destructor method.
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
;       _EXTRA:  Any keyword appropriate for the "BaseWidget::Cleanup" method.
;-
;*****************************************************************************************************
PRO TopLevelBase::CLEANUP

   @cat_pro_error_handler
   self -> BaseWidget::CLEANUP
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       TOPLEVELBASE::INIT
;
; PURPOSE:
;
;       This is the TOPLEVELBASE object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       None.
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
;       CENTER:            Set this keyword to center the top-level base before it is realized.
;
;       COLUMN:            Arrange the children of this base into this many columns.
;
;       CONTEXT_EVENTS:    Set this keyword to make the base widget return WIDGET_CONTEXT events when
;                          the right mouse button is clicked over this base widget.
;
;       CONTEXT_MENU:      Set this keyword to create a base widget that can be used to hold
;                          context-sensitive menu items.
;
;       EXCLUSIVE:         Set this keyword to make an exclusive button base.
;
;       FLOATING:          Set this keyword to have this base widget float over the top of its group leader.
;                          An error results if the GROUP_LEADER keyword is not set to a valid widget identifier.
;
;       FRAME:             Create a frame this many pixels wide around base.
;
;       GRID_LAYOUT:       Set this keyword to lay children out in equally-spaced grid.
;
;       GROUP_LEADER:      The object reference  of a group leader widget-object. Required for
;                          floating and modal top-level widgets.
;
;       ICONIFY_EVENTS:    Set this keyword to enable events when the top-level base is iconified.
;
;       KBRD_FOCUS_EVENTS: Set this keyworld to enable keyboard focus events.
;
;       KILL_REQUEST_EVENTS: Set this keyword to enable a kill request event to be sent
;                          to widget's event handler method.
;
;       MAP:               Set this keyword to map (1) or unmap (0) the base. Mapped by default.
;
;       MBAR:              This keyword returns an object reference to a MENUBARWIDGET object with
;                          the name MENUBAR - see also the 'NO_MBAR' keyword below. (Output keyword).
;
;       MODAL:             Set this keyword to create a modal top-level base widget.
;                          An error results if the GROUP_LEADER keyword is not set to a valid widget identifier.
;
;       MOVE_EVENTS:       Set this keyword to enable move events in the objects event handler.
;
;       NO_MBAR:           Set this keyword to inhibits the creation of a MENUBARWIDGET object for this base.
;
;       NONEXCLUSIVE:      Set this keyword to make an non-exclusive button base.
;
;       ONLY_ONE:          Set this keyword if you wish to have only one TLB with this REGISTER_NAME on
;                          the display at any one time. If the REGISTER_NAME keyword is undefined or
;                          a null string, then this keyword is ignored.
;
;       PARENT:            Set this keyword to the object reference of the "parent" of the top-level
;                          base object. Doing so will cause any events that reach the top-level base event
;                          handler without being handled to go to the "parent" event handler. This is
;                          necessary, for example, if you are creating a "control panel" for an object,
;                          when the control panel is in it's own top-level base.
;
;       ROW:               Arrange children of this base into this many rows.
;
;       REGISTER_NAME:     A name by which this TLB is to be registered with XMANAGER.
;
;       SCR_XSIZE:         Set the screen X size of the base to this many pixels. (Use discouraged.)
;
;       SCR_YSIZE:         Set the screen Y size of the base to this many pixels. (Use discouraged.)
;
;       SCROLL:            Set this keyword to add scroll bars to the base widget.
;
;       SIZE_EVENTS:       Set this keyword to enable events to be generated when the
;                          top-level base is resized.
;
;       SPACE:             Set this keyword to the number of pixels between children in the base.
;                          Ignored for exclusive and non-exclusive bases.
;
;       TITLE:             Set this keyword to a string containing the title for the widget.
;
;       TLB_FRAME_ATTR:    Set this keyword to a value representing the top-level base's
;                          frame attributes. The values can be additive.
;
;                          1  Base cannot be resized, minimized, or maximized.
;                          2  Suppress display of system menu.
;                          4  Suppress title bar.
;                          8  Base cannot be closed.
;                         16 Base cannot be moved.
;
;       UNITS:            The units for measurments. The default is 0 for pixels. Other values are
;                         1 for inches, and 2 for centimeters.
;
;       X_SCROLL_SIZE:    The X size (pixels) of the scrollable window area.
;
;       XOFFSET:          The horizontal space (pixels) from upper left corner of the display.
;
;       XPAD:             The amount of horizontal space (pixels) to add to edges of children.
;
;       XSIZE:            The X size of the widget.
;
;       Y_SCROLL_SIZE:    The Y size (pixels) of the scrollable window area
;
;       YPAD:             The amount of vertical space (pixels) to add to edges of children.
;
;       YOFFSET:          The vertical space (pixels) from upper left corner of the display.
;
;       YSIZE:            The Y size of the widget.
;
;       _EXTRA:           Any keyword appropriate for the superclass INIT methods.
;-
;*****************************************************************************************************
FUNCTION TopLevelBase::INIT, parent, $
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
   CENTER=center, $
   COLUMN=column, $
   CONTEXT_EVENTS=context_events, $
   CONTEXT_MENU=context_menu, $
   EXCLUSIVE=exclusive, $
   FLOATING=floating, $
   FRAME=frame, $
   GRID_LAYOUT=grid_layout, $
   GROUP_LEADER=group_leader, $
   ICONIFY_EVENTS=tlb_iconify_events, $
   KBRD_FOCUS_EVENTS=kbrd_focus_events, $
   KILL_REQUEST_EVENTS=tlb_kill_request_events, $
   MAP=map, $
   MBAR=mbar, $
   MODAL=modal, $
   MOVE_EVENTS=tlb_move_events, $
   NO_MBAR=no_mbar, $
   NONEXCLUSIVE=nonexclusive, $
   ONLY_ONE=only_one, $
   REGISTER_NAME=register_name, $
   ROW=row, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   SCROLL=scroll, $
   SIZE_EVENTS=tlb_size_events, $
   SPACE=space, $
   TITLE=title, $
   TLB_FRAME_ATTR=tlb_frame_attr, $
   UNITS=units, $
   X_SCROLL_SIZE=x_scroll_size, $
   XOFFSET=xoffset, $
   XPAD=xpad, $
   XSIZE=xsize, $
   Y_SCROLL_SIZE=y_scroll_size, $
   YOFFSET=yoffset, $
   YPAD=ypad, $
   YSIZE=ysize, $
    _EXTRA=extraKeywords

      ; Error handling.

   @cat_func_error_handler

   IF N_Elements(register_name) NE 0 THEN self._register_name = register_name
   IF Keyword_Set(only_one) THEN BEGIN
      IF self._register_name NE "" THEN BEGIN
         test = XRegistered(self._register_name, /NoShow)
         IF test NE 0 THEN BEGIN
            Message, 'Only one version of this application can be running at the same time. Returning...', /Informational
            RETURN, 0
         ENDIF
      ENDIF
   ENDIF

   IF Keyword_Set(floating) THEN BEGIN
      IF N_Elements(group_leader) EQ 0 THEN $
         Message, 'Floating top-level bases must have a group leader defined for them.'
      group_leader -> GetProperty, ID=gleader
   ENDIF

   IF N_Elements(group_leader) NE 0 THEN  group_leader -> GetProperty, ID=gleader

   IF Keyword_Set(modal) THEN BEGIN ; Modal widgets cannot have menu bars, ever!

         ; Create the top-level base widget.

      id = WIDGET_BASE ( $
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
         FLOATING=floating, $
         FRAME=frame, $
         GRID_LAYOUT=grid_layout, $
         GROUP_LEADER=gleader, $
         KBRD_FOCUS_EVENTS=kbrd_focus_events, $
         MAP=map, $
         ;MBAR=mbar, $
         MODAL=modal, $
         NONEXCLUSIVE=nonexclusive, $
         ROW=row, $
         SCR_XSIZE=scr_xsize, $
         SCR_YSIZE=scr_ysize, $
         SCROLL=scroll, $
         SPACE=space, $
         TITLE=title, $
         TLB_FRAME_ATTR=tlb_frame_attr, $
         TLB_ICONIFY_EVENTS=tlb_iconify_events, $
         TLB_KILL_REQUEST_EVENTS=tlb_kill_request_events, $
         TLB_MOVE_EVENTS=tlb_move_events, $
         TLB_SIZE_EVENTS=tlb_size_events, $
         UNITS=units, $
         X_SCROLL_SIZE=x_scroll_size, $
         XOFFSET=xoffset, $
         XPAD=xpad, $
         XSIZE=xsize, $
         YOFFSET=yoffset, $
         Y_SCROLL_SIZE=y_scroll_size, $
         YPAD=ypad, $
         YSIZE=ysize)

   ENDIF ELSE BEGIN

          ; Create the top-level base widget.

      id = WIDGET_BASE ( $
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
         FLOATING=floating, $
         FRAME=frame, $
         GRID_LAYOUT=grid_layout, $
         GROUP_LEADER=gleader, $
         KBRD_FOCUS_EVENTS=kbrd_focus_events, $
         MAP=map, $
         MBAR=mbar, $
         NONEXCLUSIVE=nonexclusive, $
         ROW=row, $
         SCR_XSIZE=scr_xsize, $
         SCR_YSIZE=scr_ysize, $
         SCROLL=scroll, $
         SPACE=space, $
         TITLE=title, $
         TLB_FRAME_ATTR=tlb_frame_attr, $
         TLB_ICONIFY_EVENTS=tlb_iconify_events, $
         TLB_KILL_REQUEST_EVENTS=tlb_kill_request_events, $
         TLB_MOVE_EVENTS=tlb_move_events, $
         TLB_SIZE_EVENTS=tlb_size_events, $
         X_SCROLL_SIZE=x_scroll_size, $
         XOFFSET=xoffset, $
         XPAD=xpad, $
         XSIZE=xsize, $
         YOFFSET=yoffset, $
         Y_SCROLL_SIZE=y_scroll_size, $
         YPAD=ypad, $
         YSIZE=ysize)

     ENDELSE


      ; Call the super class INIT method and set the widget object properties.
      ; Note that we by-pass the BASEWIDGET INIT method because we do not want
      ; the ADD method of the base widget, which only allows WIDGETATOM objects to
      ; be added to BASEWIDGET's object, to become involved. In this way, we can
      ; assign a parent to the TOPLEVELBASE object for the purpose of receiving
      ; events, getting Notify_Realize requests, etc.

   ok = self -> WidgetAtom::INIT (parent, id, GROUP_LEADER=group_leader, _EXTRA=extrakeywords)
   IF NOT ok THEN Message, 'WIDGETATOM initialization failed. Returning.'

      ; Populate object.

   self._map = Keyword_Set(map)
   IF N_Elements(tlb_frame_attr) GT 0 THEN self._tlb_frame_attr = tlb_frame_attr

      ; Wrap the menu bar identifier up in a basic widget object.

   IF Keyword_Set(modal) EQ 0 THEN BEGIN
      IF (NOT KEYWORD_SET (no_mbar)) THEN $
         mbar = Obj_New('MENUBARWIDGET', self, mbar, NAME='MENUBAR')
   ENDIF

   self._centerTLB = Keyword_Set(center)

      ; Clean up and return status.

   self -> Report, /Completed
   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       TOPLEVELBASE CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the TOPLEVELBASE object class definition.
;       The TOPLEVELBASE object is subclassed from the WIDGETATOM object.
;
;*****************************************************************************************************
PRO TopLevelBase__DEFINE, class

   class =  { TOPLEVELBASE, $           ; The TOPLEVELBASE object class name.
              INHERITS BaseWidget, $    ; Subclassed from BASEWIDGET.
              _centerTLB: 0L, $         ; A flag that indicates the TLB should be centered before it is realized.
              _register_name: "", $     ; A name by which this TLB is registered. Used to prevent multiple versions.
              _TLB_Frame_Attr: 0 $      ; A flag that indicates the current top-level base frame attributes.
            }
END
