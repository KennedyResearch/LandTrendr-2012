;*****************************************************************************************************
;+
; NAME:
;       WIDGETATOM
;
; PURPOSE:
;
;       The purpose of this routine is to create a widget object class that can
;       be subclassed for all widget objects. Any keyword that is available for
;       all widgets is implemented in this object class.
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
;       widgetatom = Obj_New("WidgetAtom", parent, widgetID)
;
; SUPERCLASSES:
;
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;    class = { WIDGETATOM,                  $ ; The WidgetAtom object class name.
;              INHERITS CATATOM,            $ ; Subclassed from CATATOM object.
;              _group_leader  : OBJ_NEW (), $ ; The group leader object for this object.
;              _id            : 0L,         $ ; The widget idenfier of the widget wrapped up in the widget object.
;              _invisible     : 0B,         $ ; This is a flag that indicates if the widget is invisible or not.
;              _killNotify    : 0L,         $ ; Set this to an object whose kill notifiy method should be called before self is destroyed.
;              _notifyRealize : 0B,         $ ; Set this flag to send a notify realize callback to the Notify_Realize method.
;              _sensitive     : 0B,         $ ; Set this flag to indicate whether the widget is sensitive (1) or not (0).
;              _helpline      : ""          $ ; A string used, for example, in a status bar when tracking cursor movements.
;            }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 12 June 2002.
;       Added HELPLINE keyword and field. 6 Feb 2004. DWF.
;       Modified the KILL_NOTIFY keyword to accept the name of a KILL_NOTIFY procedure. 29 Aug 2005. DWF.
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
;       WIDGETATOM::DRAW
;
; PURPOSE:
;
;       A dummy DRAW method. All keywords are passed to the ATOM::DRAW superclass.
;       If you overwrite this DRAW method for subclassed widget objects, be sure
;       to also call the ATOM::DRAW method (or this method), so that the DRAW methods
;       of any child objects also get called.
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
;       _EXTRA: Any keyword appropriate for the ATOM::DRAW method.
;-
;*****************************************************************************************************
PRO WidgetAtom::Draw, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Check to see if the widget has been realized. If not, realize it.
   IF (Widget_Info (self._id, /Realized) EQ 0) THEN BEGIN

      ; Special handing for ContextMenuBase objects, which can put widgets
      ; they are attached to into infinite loops if NOTIFY_REALIZE is turned
      ; on for them. I *think* this is the only special case.
       IF Obj_ISA(self, 'CONTEXTMENUBASE') THEN RETURN

       Widget_Control, self._ID, /Realize
   ENDIF

   ; Call the superclass DRAW method.
   self -> CatAtom::Draw, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       WIDGETATOM::EVENTHANDLER
;
; PURPOSE:
;
;       This is an event handler for all widgets. It's real purpose is to
;       serve as a pseudo notify realize mechanism for objects.
;
; SYNTAX:
;
;       theObject -> EventHandler, event
;
; ARGUMENTS:
;
;       event:    The event structure from a widget event.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO WidgetAtom::EventHandler, event

   @cat_pro_error_handler

   CASE event.event_name OF

      'WIDGET_TIMER': BEGIN
         CatRealizeNotify, self
         END

      ; Dummy propertysheets can generate events for NAME and DESCRIPTION.
      ; We wish to handle these silently, while still allowing other properties
      ; to propigate correctly.
      'WIDGET_PROPSHEET_SELECT': BEGIN
         CASE event.identifier OF
            'DESCRIPTION':
            'NAME':
            ELSE: self -> CATATOM::EventHandler, event
         ENDCASE
         END
      ELSE: self -> CATATOM::EventHandler, event
   ENDCASE

   self -> Report, /Completed
   RETURN
END


;*****************************************************************************************************
;+
; NAME:
;       WIDGETATOM::GETID
;
; PURPOSE:
;
;       This returns the widget object's widget identifier.
;
; SYNTAX:
;
;       theValue = object -> GetD()
;
; RETURN VALUE:
;
;       theValue: The widget identifier for this widget, if the widget has been realized.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
FUNCTION WidgetAtom::GetID

   @cat_func_error_handler

   self -> Report, /Completed
   RETURN, self._ID
END


;*****************************************************************************************************
;+
; NAME:
;       WIDGETATOM::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to obtain the object's properties. Keywords here apply to all
;       widgets. Defining the keywords here eliminates work in defining subclassed widget
;       classes.
;
; SYNTAX:
;
;       self -> GetProperty, UVALUE=theValue
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       CHILD:          The object reference of the first child of this widget object. If there
;                       is no child, a null object is returned.
;
;       GEOMETRY:       The current geometry structure of the widget.
;
;       GROUP_LEADER:   The identifier of the group leader widget.
;
;       HELPLINE:        A string used as a help line for widget tracking purposes.
;
;       ID:             The widget identifier of this particular widget object.
;
;       INVISIBLE:      This keyword returns a flag that indicates if the widget is invisible (1) or not (0).
;
;       KBRD_FOCUS_EVENTS: The current keyboard focus event status of the widget.
;
;       KILL_NOTIFY:    Flag indicating that user-defined KILL_NOTIFY method is called for this widget.
;
;       MANAGED:        The current managed status of the widget.
;
;       NOTIFY_REALIZE: Flag indicating that user-defined NOTIFY_REALIZE method is called for this widget.
;
;       REALIZED:       The current realization of the widget.
;
;       SCR_XSIZE:      The X screen size of the widget.
;
;       SCR_YSIZE:      The Y screen size of the widget.
;
;       SIBLING:        The object reference of the first sibling of this widget object. If there
;                       is no sibling, a null object is returned.
;
;       SENSITIVE:      Indicates whether the current widget is sensitive (1) or not (0).
;
;       STRING_SIZE:    Set this keyword to retrieve the dimensions of a string. The return value is a
;                       two-element vector containing the string's width and height in pixels. A widget
;                       identifier must always be specified in the call. The keyword's value can be
;                       either the measured string or a two-element vector containing first the string
;                       and then a font. If a font is specified, that font is used in the calculations.
;                       Otherwise, the widget's font is used. In the case of a table widget with multiple
;                       fonts, the default font is used.
;
;       SYSTEM_COLORS:  Returns a WIDGET_SYSTEM_COLORS structure containing the colors IDL uses for
;                       its widget system. The colors can be used to make your application looks like
;                       IDL widgets.
;
;       TAB_MODE:       The current tab mode for the widget.
;
;       TRACKING_EVENTS: The current state of tracking events for this widget.
;
;       TYPE:           The type code for this widget. See the IDL WIDGET_INFO documentation for details.
;
;       UNITS:          The units that widget size is measured in.
;
;       UPDATE:         Inidcates whether the widget is current enabled (1) or disabled (0) for updates.
;
;       VALID_ID:       Indicates where the ID of the widget is valid or not.
;
;       VERSION:        The version of widgets used. See the IDL WIDGET_INFO documentation for details.
;
;       WIDGETNAME:     The name of the type of widget (e.g, "BASE", "DRAW", etc.). Don't confuse this
;                       with the name of the object (obtained with the NAME keyword to the ATOM object.)
;
;       XOFFSET:        The X offset relative to it parent base in UNITS.
;
;       XSIZE:          The X size of the widget.
;
;       YOFFSET:        The Y offset relative to it parent base in UNITS.
;
;       YSIZE:          The Y size of the widget.
;
;       _REF_EXTRA:     Any keyword appropriate for the superclass GETPROPERTY methods.
;-
;*****************************************************************************************************
PRO WidgetAtom::GetProperty, $
                    CHILD=child, $
                    GEOMETRY=geometry, $
                    GROUP_LEADER=group_leader, $
                    ID=id, $
                    KBRD_FOCUS_EVENTS=kbrd_focus_events, $
                    INVISIBLE=invisible, $
                    KILL_NOTIFY=kill_notify, $
                    MANAGED=managed, $
                    NOTIFY_REALIZE=notify_realize, $
                    REALIZED=realized, $
                    SCR_XSIZE=scr_xsize, $
                    SCR_YSIZE=scr_ysize, $
                    SENSITIVE=sensitive, $
                    SIBLING=sibling, $
                    STRING_SIZE=string_size, $
                    SYSTEM_COLORS=system_colors, $
                    HELPLINE=helpline, $
                    TAB_MODE=tab_mode, $
                    TRACKING_EVENTS=tracking_events, $
                    TYPE=type, $
                    UNITS=units, $
                    UPDATE=update, $
                    VALID_ID=valid_id, $
                    VERSION=version, $
                    WIDGETNAME=widgetname, $
                    XOFFSET=xoffset, $
                    XSIZE=xsize, $
                    YOFFSET=yoffset, $
                    YSIZE=ysize, $
                    _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Object properties.

   group_leader   = self._group_leader
   id             = self._id
   notify_realize = self._notifyRealize
   invisible      = self._invisible
   kill_notify    = self._killNotify
   sensitive      = self._sensitive


   IF (WIDGET_INFO (self._id, /VALID_ID)) THEN $
   BEGIN

      ; Properties we can obtain from WIDGET_INFO.

      kbrd_focus_events = Widget_Info(self._id, /KBRD_FOCUS_EVENTS)
      managed    = Widget_Info(self._id, /MANAGED)
      realized   = Widget_Info(self._id, /REALIZED)
      IF Arg_Present(string_size) THEN string_size = Widget_Info(self._id, /STRING_SIZE)
      IF Arg_Present(tab_mode) THEN tab_mode   = Widget_Info(self._id, /TAB_MODE)
      tracking_events = Widget_Info(self._id, /TRACKING_EVENTS)
      type       = Widget_Info(self._id, /TYPE)
      units      = Widget_Info(self._id, /UNITS)
      update     = Widget_Info(self._id, /UPDATE)
      valid_id   = Widget_Info(self._id, /VALID_ID)
      version    = Widget_Info(self._id, /VERSION)
      widgetname = Widget_Info(self._id, /NAME)

      ; Properties we can obtain from the widget geometry.

      geometry  = WIDGET_INFO (self._id, /GEOMETRY)
      scr_xsize = geometry.scr_xsize
      scr_ysize = geometry.scr_ysize
      xoffset   = geometry.xoffset

      ; Note, there is a bug in IDL at least through IDL 6.2 so that if you set the SCR_XSIZE and SCR_YSIZE
      ; keywords in a draw widget INIT, the geometry of the draw widget is incorrect. This results in incorrect
      ; values being returned for the canvas size of the draw widget. I've found decent results (so far) by NOT
      ; initializing draw widgets to screen coordinates, but just using XSIZE/YSIZE keywords to set their
      ; canvas coordinates. (29 AUG 2005, DWF.)
      IF type EQ 4 THEN xsize = geometry.xsize > geometry.draw_xsize ELSE xsize = geometry.xsize ; Draw widgets.
      yoffset   = geometry.yoffset
      IF type EQ 4 THEN ysize = geometry.ysize > geometry.draw_ysize ELSE ysize = geometry.ysize ; Draw widgets.

      IF Arg_Present(helpline) THEN helpline = self._helpline
      IF Arg_Present(system_colors) THEN system_colors = Widget_Info(self._id, /SYSTEM_COLORS)
      IF Arg_Present(child) THEN $
      BEGIN
         childID = Widget_Info(self._id, /CHILD)
         IF childID EQ 0 THEN child = Obj_New() ELSE Widget_Control, childID, Get_UValue=child
      ENDIF
      IF Arg_Present(sibling) THEN $
      BEGIN
         siblingID = Widget_Info(self._id, /SIBLING)
         IF siblingID EQ 0 THEN sibling = Obj_New() ELSE Widget_Control, siblingID, Get_UValue=sibling
      ENDIF
   ENDIF

      ; Superclass methods.

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CatAtom::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       WIDGETATOM::GET_UVALUE
;
; PURPOSE:
;
;       This returns the widget object's user value.
;
; SYNTAX:
;
;       theValue = object -> Get_UValue()
;
; RETURN VALUE:
;
;       theValue: The IDL variable that is to be transfered from the UVALUE location.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
FUNCTION WidgetAtom::Get_UValue

   @cat_func_error_handler

   self -> Report, /Completed
   RETURN, *self._uvalue
END


;*****************************************************************************************************
;+
; NAME:
;       WIDGETATOM::KILL_NOTIFY
;
; PURPOSE:
;
;       A dummy KILL_NOTIFY method. If a kill notification reaches this point, an
;       warning is issued.
;
; SYNTAX:
;
;       self -> Kill_Notify
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
PRO WidgetAtom::Kill_Notify
   @cat_pro_error_handler
   self -> Report, 'Unhandled Kill_Notify call.', 1
END


;*****************************************************************************************************
;+
; NAME:
;       WIDGETATOM::NOTIFY_REALIZE
;
; PURPOSE:
;
;       A dummy NOTIFY_REALIZE method to catch unhandled NOTIFY_REALIZE calls.
;
; SYNTAX:
;
;       self -> Notify_Realize, object
;
; ARGUMENTS:
;
;       OBJECT: The object that was realized (often the self object).
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO WidgetAtom::Notify_Realize, theObject

   @cat_pro_error_handler

   MESSAGE, 'Unhandled NOTIFY_REALIZE for object ' + OBJ_CLASS (theObject)

END


;*****************************************************************************************************
;+
; NAME:
;       WIDGETATOM::SEND_EVENT
;
; PURPOSE:
;
;       Sends a pseudo-event to a widget. If thisEvent.TOP (see below) is an object refernce,
;       the event is sent to the Event_Handler method of the toThisWidget. If thisEveven.TOP
;       is a valid widget identifier, then Widget_Control is used to send an event to the
;       widget identifed in toThisWidget via SEND_EVENT and thisEvent must be a normal widget
;       event structure, not a widget object event structure.
;
; SYNTAX:
;
;       self -> Send_Event, toThisWidget, thisEvent
;
; ARGUMENTS:
;
;       toThisWidget:   The widget object reference of the widget to receive the event
;
;       thisEvent:      The event structure to send. If TOP field is an object reference, the
;                       event structure is send directly to the EVENT_METHOD of toThisWidget object.
;                       Otherwise, a normal widget event is sent to the widget identified in toThisWidget.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO WidgetAtom::Send_Event, tothisWidget, thisEvent

   @cat_pro_error_handler

   IF N_Params() NE 2 THEN Message, "Two positional parameters (theWidget and theEvent) are required."

   IF Size(thisEvent.top, /TName) EQ 'OBJREF' THEN BEGIN

      ; Get the event objects for the "send to" widget.
      IF Obj_Valid(tothisWidget) EQ 0 THEN Message, 'This widget cannot be sent an object widget event structure.'
      tothisWidget -> GetProperty, Event_Objects=eventObjects, Event_Method=event_method

      ; Call them
      FOR j=0, N_Elements(eventObjects) - 1 DO BEGIN
         Call_Method, event_method, eventObjects[j], thisEvent
      ENDFOR

   ENDIF ELSE BEGIN

      IF Size(tothisWidget, /TName) EQ 'OBJREF' THEN receiveWidget = tothisWidget -> Get_ID() ELSE $
         receiveWidget = tothisWidget
      Widget_Control, receiveWidget, Send_Event=thisEvent

   ENDELSE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       WIDGETATOM::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the object's properties. Keywords here apply to all
;       widgets. Defining the keywords here eliminates work in defining subclassed widget
;       classes.
;
; SYNTAX:
;
;       self -> SetProperty, UVALUE=theValue
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       CLEAR_EVENTS:   Set this keyword to clear all events queued for this widget.
;
;       DESTROY:        Set this keyword to destroy this widget.
;
;       GROUP_LEADER:   The widget group leader for this particular widget.
;
;       HELPLINE:       A string used as a help line for widget tracking purposes.
;
;       ICONIFY:        Set this keyword to iconify/de-iconfy the widget.
;
;       ID:             The widget identifier.
;
;       INVISIBLE:      Set this keyword to create an "invisible" widget.
;
;       KILL_NOTIFY:    Set this keyword to 1 to call the KILL_NOTIFY method of this object
;                       when the widget is destroyed. Or, set this keyword to the name of
;                       a callback procedure to call when the object is destroyed. If you
;                       choose to use a kill notify name, your callback procedure is responsible
;                       for destroying the object. If this is not done, memory leaking is sure
;                       to occur! Here is a sample KILL_NOTIFY procedure:
;
;                       PRO TLB_REPORT, tlbID
;
;                         ; Get the TLB object.
;                         Widget_Control, tlbID, Get_UValue=tlb
;
;                         .... whatever it is you want to do....
;
;                         ; Destroy the TLB object.
;                         Obj_Destroy, tlb
;
;                       END
;
;                       This routine is set up as the callback routine by specifying its name
;                       in the TLB object creation routine:
;
;                       tlb = Obj_New('TopLevelBase', Kill_Notify='TLB_Report')
;
;       NO_COPY:        If a UVALUE is being set, setting this keyword will make the
;                       transfer of the variable to the UVALUE without copying.
;
;       MAP:            Set this keyword to map (1) or unmap (0) the widget.
;
;       NOTIFY_REALIZE: Set this keyword to register and NOTIFY_REALIZE callback to
;                       the widget, which will be directed to the NOTIFY_REALIZE method
;                       of this object.
;
;       SCR_XSIZE:      The X screen size of the widget in pixel units.
;
;       SCR_YSIZE:      The Y screen size of the widget in pixel units.
;
;       SEND_EVENT:     Set this keyword to an event structure in order to send the event
;                       structure to the widget. The event is handled in the normal way.
;
;       SENSITIVE:      Set to set the widget's sensitivity. (Sensitive = 1 is the default.)
;
;       SHOW:           Set this keyword to bring the window containing the widget to the
;                       front of other windows on the display.
;
;       TAB_MODE:       Determines how the widget hierarchy can be navigated using the Tab key.
;                       Default value is 0, indicating no navigation onto or off the widget with Tabs.
;                       Note that this is *always* the case under UNIX. On Windows, a value of 1 enables
;                       navigation onto and off of the widget. A value of 2 enables navigation onto the
;                       widget only. And a value of 3 enables navigation off of the widget only.
;
;       TIMER:          Starts a timer event with the delay set to this value.
;
;       TRACKING_EVENTS: Set this keyword to turn tracking events on for the widget object.
;
;       UPDATE:         Set this keyword to enable (1) or disable (0) immediate screen updates
;                       for this widget. (Normally this would only apply to base widgets.)
;
;       UNITS:          The units that widget size is measured in. Default is 0, pixels.
;                       Other values are 1, inches, and 2, centimeters.
;
;       XOFFSET:        The X offset relative to it parent base in UNITS. Does not apply to
;                       column or row bases.
;
;       XSIZE:          Set this keyword to the X "size" of the widget. (Units of size varies
;                       with type of widget, etc. See the IDL documentation for details.)
;
;       YOFFSET:        The Y offset relative to it parent base in UNITS. Does not apply to
;                       column or row bases.
;
;       YSIZE:          Set this keyword to the Y "size" of the widget. (Units of size varies
;                       with type of widget, etc. See the IDL documentation for details.)
;
;       _EXTRA:         Any keyword appropriate for the superclass SETPROPERTY methods.
;-
;*****************************************************************************************************
PRO WidgetAtom::SetProperty, $
               CLEAR_EVENTS=clear_events, $
               DESTROY=destroy, $
               GROUP_LEADER=group_leader, $
               ICONIFY=iconify, $
               ID=id, $
               INVISIBLE=invisible, $
               KILL_NOTIFY=kill_notify, $
               NO_COPY=no_copy, $
               MAP=map, $
               NOTIFY_REALIZE=notify_realize, $
               SCR_XSIZE=scr_xsize, $
               SCR_YSIZE=scr_ysize, $
               SEND_EVENT=send_event, $
               SENSITIVE=sensitive, $
               SHOW=show, $
               TAB_MODE=tab_mode, $
               TIMER=timer, $
               HELPLINE=helpline, $
               TRACKING_EVENTS=tracking_events, $
               UPDATE=update, $
               UNITS=units, $
               XOFFSET=xoffset, $
               XSIZE=xsize, $
               YOFFSET=yoffset, $
               YSIZE=ysize, $
               _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; If the property is defined, then handle it.
   IF (N_Elements(id) NE 0) THEN self._id = id
   IF (N_Elements(clear_events) NE 0) THEN Widget_Control, self._id, Clear_Events=Keyword_Set(clear_events)
   IF (N_Elements(destroy) NE 0) THEN Widget_Control, self._id, Destroy=Keyword_Set(destroy)
   IF N_Elements(group_leader) NE 0 THEN BEGIN
      IF Obj_ISA_Valid(group_leader, 'WIDGETATOM') THEN $
      BEGIN
      group_leader ->GetProperty, ID=group_leader_ID
      Widget_Control, self._id, Group_Leader=group_leader_ID
      self._group_leader = group_leader
      ENDIF ELSE Message, 'Group leader must be a valid object of WIDGETATOM class.'
   ENDIF
   IF (N_Elements(invisible) NE 0) THEN self._invisible = Keyword_Set(invisible)
   IF N_Elements(kill_notify) NE 0 THEN BEGIN
      type = Size(kill_notify, /TNAME)
      CASE type OF
         'STRING': BEGIN
                   self._killNotify = 1
                   IF kill_notify EQ "" THEN WIDGET_CONTROL, self._id, SET_UVALUE=self, KILL_NOTIFY="CatKillNotify" ELSE $
                      WIDGET_CONTROL, self._id, SET_UVALUE=self, KILL_NOTIFY=kill_notify
                   END

         ELSE: BEGIN
               self._killNotify = Keyword_Set(kill_notify)
               END
      ENDCASE

   ENDIF
   no_copy = Keyword_Set(no_copy)
   IF (N_Elements(map) NE 0) THEN Widget_Control, self._id, MAP=map
   IF (N_Elements(notify_realize) NE 0) THEN BEGIN
      self._notifyRealize = Keyword_Set(notify_realize)
      IF self._notifyRealize THEN Widget_Control, self._ID, Notify_Realize='CatRealizeNotify'
   ENDIF
   IF (N_Elements(scr_xsize) NE 0) THEN Widget_Control, self._id, SCR_XSIZE=scr_xsize, SCR_YSIZE=scr_ysize
   IF (N_Elements(scr_ysize) NE 0) THEN Widget_Control, self._id, SCR_YSIZE=scr_ysize, SCR_XSIZE=scr_xsize
   IF (N_Elements(send_event) NE 0) THEN Widget_Control, self._id, SEND_EVENT=send_event
   IF (N_Elements(sensitive) NE 0) THEN BEGIN
      self._sensitive = Keyword_Set(sensitive)
      Widget_Control, self._id, SENSITIVE=Keyword_Set(sensitive)
   ENDIF
   IF (N_Elements(show) NE 0) THEN Widget_Control, self._id, SHOW=Keyword_Set(show)
   IF (N_Elements(tab_mode) NE 0) THEN Widget_Control, self._id, TAB_MODE=0 > tab_mode < 3
   IF (N_Elements(timer) NE 0) THEN Widget_Control, self._id, TIMER=timer
   IF (N_Elements(helpline) NE 0) THEN self._helpline = helpline
   IF (N_Elements(tracking_events) NE 0) THEN Widget_Control, self._id, TRACKING_EVENTS=Keyword_Set(tracking_events)
   IF (N_Elements(update) NE 0) THEN Widget_Control, self._id, UPDATE=Keyword_Set(update)
   IF (N_Elements(units) NE 0) THEN Widget_Control, self._id, UNITS=units

   IF (OBJ_CLASS (self) NE 'MENUBARWIDGET') THEN $ ; Cannot resize menu widgets
   BEGIN
      size_events = 0
      IF Obj_IsA_Valid(self, 'TOPLEVELBASE') THEN self -> GetProperty, SIZE_EVENTS=size_events
      IF size_events EQ 0 THEN $
      BEGIN
         IF (N_Elements(xsize)   NE 0) THEN Widget_Control, self._id, XSIZE=xsize,YSIZE=ysize
         IF (N_Elements(ysize)   NE 0) THEN Widget_Control, self._id, YSIZE=ysize, XSIZE=xsize
      ENDIF ELSE self -> Report, 'Resizeable top-level base widgets cannot be explicitly sized. Continuing...', 1
         IF (N_Elements(xoffset) NE 0) THEN Widget_Control, self._id, YOFFSET=yoffset, XOFFSET=xoffset
         IF (N_Elements(yoffset) NE 0) THEN Widget_Control, self._id, YOFFSET=yoffset, XOFFSET=xoffset
   ENDIF

   ; Call the superclass method
   self -> CatAtom::SetProperty, _EXTRA=extraKeywords

   IF (N_ELEMENTS (iconify) GT 0) THEN Widget_Control, self._id, ICONIFY=KEYWORD_SET (iconify)

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       WIDGETATOM::SET_UVALUE
;
; PURPOSE:
;
;       This sets the widget object's user value.
;
; SYNTAX:
;
;       object -> Set_UValue, theValue
;
; ARGUMENTS:
;
;       theValue: The IDL variable that is to be transfered to the UVALUE location.
;
; KEYWORDS:
;
;       NO_COPY:  Set this keyword to do the variable transfer without copying the variable.
;-
;*****************************************************************************************************
PRO WidgetAtom::Set_UValue, theValue, NO_COPY=no_copy

   @cat_pro_error_handler

   no_copy = Keyword_Set(no_copy)

   IF Ptr_Valid(self._uvalue) THEN BEGIN
      IF no_copy THEN BEGIN
         Ptr_Free, self._uvalue
         self._uvalue = Ptr_New(theValue, NO_COPY=no_copy)
      ENDIF ELSE *self._uvalue = theValue
   ENDIF ELSE self._uvalue = Ptr_New(theValue, NO_COPY=no_copy)

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       WIDGETATOM::CLEANUP
;
; PURPOSE:
;
;       This is the WIDGETATOM object class destructor method.
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
PRO WidgetAtom::CLEANUP

   @cat_pro_error_handler

   IF NOT OBJ_VALID(self) THEN RETURN

   IF Widget_Info(self._id, /Valid_ID) THEN Widget_Control, self._id, /Destroy

   self -> CatAtom::CLEANUP
   self -> Report, /Completed

END

;*****************************************************************************************************
;+
; NAME:
;       WIDGETATOM::INIT
;
; PURPOSE:
;
;       This is the WIDGETATOM object class creator method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       parent:         The object reference of the parent object for the widget
;
;       widgetID:       The widget idenfier of the widget in the widget object being created.
;
; KEYWORDS:
;
;       HELPLINE:       A string used as a help line for widget tracking purposes.
;
;       INVISIBLE:      Set this keyword if you wish the widget you are creating to
;                       remain "invisible" to the user. Invisible widgets are never
;                       realized, allowing you to use their properties without exposing
;                       them in the widget hierarchy. For example, an invisible widget
;                       could be used to implement a TIMER function.
;
;       KILL_NOTIFY:    Set this keyword to 1 to call the KILL_NOTIFY method of this object
;                       when the widget is destroyed. Or, set this keyword to the name of
;                       a callback procedure to call when the object is destroyed. If you
;                       choose to use a kill notify name, your callback procedure is responsible
;                       for destroying the object. If this is not done, memory leaking is sure
;                       to occur! Here is a sample KILL_NOTIFY procedure:
;
;                       PRO TLB_REPORT, tlbID
;
;                         ; Get the TLB object.
;                         Widget_Control, tlbID, Get_UValue=tlb
;
;                         .... whatever it is you want to do....
;
;                         ; Destroy the TLB object.
;                         Obj_Destroy, tlb
;
;                       END
;
;                       This routine is set up as the callback routine by specifying its name
;                       in the TLB object creation routine:
;
;                       tlb = Obj_New('TopLevelBase', Kill_Notify='TLB_Report')
;
;       GROUP_LEADER:   The group leader widget-object. Must be used for floating and
;                       modal base widgets.
;
;       NOTIFY_REALIZE: Set this keyword to register and NOTIFY_REALIZE callback to
;                       the widget, which will be directed to the NOTIFY_REALIZE method
;                       of this object.
;
;       SENSITIVE:      Set this keyword to set the widget's sensitivity. (Sensitive = 1 is the default.)
;
;       TAB_MODE:       Determines how the widget hierarchy can be navigated using the Tab key.
;                       Default value is 0, indicating no navigation onto or off the widget with Tabs.
;                       Note that this is *always* the case under UNIX. On Windows, a value of 1 enables
;                       navigation onto and off of the widget. A value of 2 enables navigation onto the
;                       widget only. And a value of 3 enables navigation off of the widget only.
;
;       TIMER:          Starts a timer event with the delay set to this value.
;
;       TRACKING_EVENTS: Set this keyword to turn on tracking events.
;
;       _EXTRA:         Any keyword appropriate for the superclass INIT methods.
;-
;*****************************************************************************************************
FUNCTION WidgetAtom::INIT, parent, widgetID, $
   INVISIBLE=invisible, $
   KILL_NOTIFY=kill_notify, $
   GROUP_LEADER=group_leader, $
   NO_COPY=no_copy, $
   NOTIFY_REALIZE=notify_realize, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   SENSITIVE=sensitive, $
   TAB_MODE=tab_mode, $
   TIMER=timer, $
   HELPLINE=helpline, $
   TRACKING_EVENTS=tracking_events, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   @cat_func_error_handler

     ; Check that the widget ID is valid and set some defaults.

   IF (N_ELEMENTS (widgetID) NE 0) THEN $
   BEGIN
      IF (WIDGET_INFO (widgetID, /VALID_ID)) THEN $
      BEGIN

         self._id = widgetID

            ; Is there a group leader for this widget? If so, it must be a WIDGETATOM object.

         IF N_Elements(group_leader) NE 0 THEN BEGIN
            IF Obj_ISA_Valid(group_leader, 'WIDGETATOM') THEN $
            BEGIN
            group_leader ->GetProperty, ID=group_leader_ID
            Widget_Control, self._id, Group_Leader=group_leader_ID
            self._group_leader = group_leader
            ENDIF ELSE Message, 'Group leader must be a valid object of WIDGETATOM class.'
         ENDIF

            ; Do you have a help line?

         IF N_Elements(helpline) NE 0 THEN self._helpline = helpline

            ; Is this an invisible widget?

         self._invisible = Keyword_Set(invisible)

            ; Do we need a Notify_Realize callback?

         self._notifyRealize = Keyword_Set(notify_realize)
         IF self._notifyRealize THEN Widget_Control, self._id, Notify_Realize='CatRealizeNotify'

            ; A Kill_Notify callback is *always* set for widgets. If the user
            ; wants to be notified of a kill, set the KILL_NOTIFY keyword to 1 and
            ; override the KILL_NOTIFY method. Or, set the KILL_NOTIFY keyword to
            ; the name of a callback procedure. You will be responsible for destroying
            ; the object in the procedure. (The object will be found in the USER VALUE
            ; of the object being destroyed and whose ID is sent to the callback routine.)

         IF N_Elements(kill_notify) NE 0 THEN BEGIN
            type = Size(kill_notify, /TNAME)
            CASE type OF
               'STRING': BEGIN
                         self._killNotify = 1
                         WIDGET_CONTROL, self._id, SET_UVALUE=self, KILL_NOTIFY=kill_notify
                         END

               ELSE: BEGIN
                     self._killNotify = Keyword_Set(kill_notify)
                     WIDGET_CONTROL, self._id, SET_UVALUE=self, KILL_NOTIFY="CatKillNotify"
                     END
            ENDCASE

         ENDIF ELSE BEGIN
            self._killNotify = Keyword_Set(kill_notify)
            WIDGET_CONTROL, self._id, SET_UVALUE=self, KILL_NOTIFY="CatKillNotify"
         ENDELSE

            ; Does the user want to set widget sensitivitity?

         IF (N_Elements(sensitive) EQ 0) THEN self._sensitive = 1 ELSE self._sensitive = Keyword_Set(sensitive)
         Widget_Control, self._id, Sensitive=self._sensitive

            ; Does the user want to start a timer event on this widget?

         IF (N_Elements (timer) NE 0) THEN WIDGET_CONTROL, self._id, TIMER=timer

            ; Does the user want tabing?

        IF (N_Elements (tab_mode) NE 0) THEN WIDGET_CONTROL, self._id, TAB_MODE=tab_mode

            ; Does the user want tracking events?

         IF (N_Elements (tracking_events) NE 0) THEN WIDGET_CONTROL, self._id, TRACKING_EVENTS=tracking_events


      ENDIF $
      ELSE BEGIN
         Message, 'Supplied widget ID is invalid. Returning.'
      ENDELSE
   ENDIF $
   ELSE BEGIN
      Message, 'A widget ID is required. Returning.'
   ENDELSE

      ; Call the superclass INIT method.
   ok = self -> CatAtom::INIT (parent, _EXTRA=extraKeywords)
   IF NOT ok THEN Message, 'CATATOM initialization failed. Returning.'

   ; Report and return success.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       WIDGETATOM CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the WIDGETATOM object class definition.
;       The WIDGETATOM__DEFINE object is subclassed from the ATOM object.
;
;*****************************************************************************************************
PRO WidgetAtom__DEFINE, class

    class = { WIDGETATOM,                  $ ; The WidgetAtom object class name.
              INHERITS CATATOM,            $ ; Subclassed from CATATOM object.
              _group_leader  : OBJ_NEW (), $ ; The group leader object for this object.
              _id            : 0L,         $ ; The widget idenfier of the widget wrapped up in the widget object.
              _invisible     : 0B,         $ ; This is a flag that indicates if the widget is invisible or not.
              _killNotify    : 0L,         $ ; Set this to an object whose kill notifiy method should be called before self is destroyed.
              _notifyRealize : 0B,         $ ; Set this flag to send a notify realize callback to the Notify_Realize method.
              _sensitive     : 0B,         $ ; Set this flag to indicate whether the widget is sensitive (1) or not (0).
              _helpline      : ""          $ ; A string used, for example, in a status bar when tracking cursor movements.
            }
END