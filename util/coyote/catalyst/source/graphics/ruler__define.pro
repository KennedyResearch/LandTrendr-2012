;*****************************************************************************************************
;+
; NAME:
;       RULER__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to create a simple ruler for display
;       in the coordinate system of the underlying image. Users will define the
;       length of the ruler (e.g., 5 cm) and the ruler will display
;       this value on the image.

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
;       theObject = Obj_New("RULER")
;
; SUPERCLASSES:
;
;       SELECTABLEOBJECT
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { RULER, $
;             charsize: 0.0, $             ; The character size.
;             format: '', $                ; The character format.
;             length: 0.0, $               ; The absolute length of the ruler.
;             parent_coord: Obj_New(), $   ; The coordinate object of the parent.
;             minor: 0L, $                 ; The number of minor tick intervals.
;             thick: 0L, $                 ; The thickness of the ruler lines.
;             ticklen: 0.0, $              ; The tick length in normalized coordinates.
;             ticks: 0L, $                 ; The number of major tick intervals.
;             title: '', $                 ; The title of the ruler.
;             vertical: 0B, $              ; The vertical flag.
;             x: 0.0, $                    ; The normalized X coordinate of ruler center.
;             y: 0.0, $                    ; The normalized Y coordinate of ruler center.
;             INHERITS SELECTABLEOBJECT $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W Fanning, March 9, 2005.
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
;       RULER::CALCULATEBOUNDARYBOX
;
; PURPOSE:
;
;       This method calculates the extent of a box about the ruler.
;
; SYNTAX:
;
;       theObject -> CalculateBoundaryBox
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
PRO Ruler::CalculateBoundaryBox

   @cat_pro_error_handler

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       RULER::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the RULER object. A
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
PRO Ruler::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Ruler Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.
   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, $
      Name='RULER PROPERTYSHEET', YSize=11)
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       RULER::DRAW
;
; PURPOSE:
;
;       This method draws the ruler.
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
PRO Ruler::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; Is this visible or not.
   IF ~self.visible THEN RETURN

   ; Establish the parent coordinate system.
   self.parent_coord -> GetProperty, XRange=xr, YRange=yr, Position=position
   self.parent_coord -> Draw

   ; How long is the length in normalized coordinates in the parent
   ; coordinate system?
   theLength = self.length
   ticks = self.ticks

   ; Is this a vertical ruler?
   IF self.vertical THEN BEGIN

      c = Convert_Coord([0,0], [0,theLength], /Data, /To_Normal)
      nLength = c[1,1] - c[1,0]
      WHILE nLength GT 1.0 DO BEGIN
         theLength = theLength / 2.0
         nLength = c[1,1] - c[1,0]
         c = Convert_Coord([0,0], [0,theLength], /Data, /To_Normal)
         ticks = (ticks * 2) < 4
         nLength = c[1,1] - c[1,0]
      ENDWHILE
      y = self.y * (position[3]-position[1]) + position[1]
      x = self.x * (position[2]-position[0]) + position[0]
      x0 = position[0]
      x1 = position[2]
      y0 = position[1] > (y - (nLength / 2.0))
      y1 = (y + (nLength / 2.0)) < position[2]
      self._coords -> SetProperty, YRange=[0,nlength], XRange=[0,1], $
         Position=[x0, y0, x1, y1]
      self._coords -> Draw

      ; Draw the ruler.
      IF self.x GT 0.5 THEN yaxis = 1 ELSE yaxis = 0
      AXIS, x, y, $
         YAXIS=yaxis, $
         CHARSIZE=self.charsize, $
         COLOR=FSC_COLOR(self.color), $
         SAVE=1, $
         YMINOR=self.minor, $
         YRANGE=[0.0,theLength], $
         YTHICK=self.thick, $
         YTICKFORMAT=self.format, $
         YTICKLEN=self.ticklen, $
         YTICKS=ticks, $
         YTITLE=self.title, $
         YSTYLE=1, /NORMAL

      atad = 0.015 * self.charsize
      self.box[1,*] = [y0-atad, y1+atad, y1+atad, y0-atad, y0-atad]

      yht = 7.5 * !D.X_CH_SIZE * self.charsize
      c = Convert_Coord(yht, 0, /Device, /To_Normal)
      yht = c[0,0]

      IF yaxis THEN BEGIN
         self.box[0,*] = [self.x - self.ticklen, self.x - self.ticklen, $
            (self.x + yht) < 1.0, (self.x + yht) < 1.0, self.x - self.ticklen]
      ENDIF ELSE BEGIN
         self.box[0,*] = [(self.x - yht) > 0.0, (self.x - yht) > 0.0, $
            self.x + self.ticklen, self.x + self.ticklen, (self.x - yht) > 0.0]
      ENDELSE

   ENDIF ELSE BEGIN

      c = Convert_Coord([0,theLength], [0,0], /Data, /To_Normal)
      nLength = c[0,1] - c[0,0]
      WHILE nLength GT 1.0 DO BEGIN
         theLength = theLength / 2.0
         self.currentLength = theLength
         self._coords ->getProperty,  XRange=xr, YRange=yr & print, xr, yr
         c = Convert_Coord([0,theLength], [0,0], /Data, /To_Normal)
         ticks = (ticks * 2) < 4
         nLength = c[0,1] - c[0,0]
      ENDWHILE
      y = self.y * (position[3]-position[1]) + position[1]
      x = self.x * (position[2]-position[0]) + position[0]
      x0 = position[0] > (x - (nLength / 2.0))
      x1 = (x + (nLength / 2.0)) < position[2]
      y0 = position[1]
      y1 = position[3]
      self._coords -> SetProperty, XRange=[0.0,nlength], YRange=[0.0,1.0], $
         Position=[x0, y0, x1, y1]
      self._coords -> Draw
      IF self.y GT 0.5 THEN xaxis = 1 ELSE xaxis = 0
      AXIS, x, y, $
         XAXIS=xaxis, $
         CHARSIZE=self.charsize, $
         COLOR=FSC_COLOR(self.color), $
         XMINOR=self.minor, $
         XRANGE=[0.0,theLength], $
         XTHICK=self.thick, $
         XTICKFORMAT=self.format, $
         XTICKLEN=self.ticklen, $
         XTICKS=ticks, $
         XTITLE=self.title, $
         XSTYLE=1, /NORMAL

      atad = 0.025 * self.charsize
      self.box[0,*] = [x0-atad, x0-atad, x1+atad, x1+atad, x0-atad]
      xht = 2.5 * !D.Y_CH_SIZE * self.charsize
      c = Convert_Coord(xht, 0, /Device, /To_Normal)
      xht = c[0,0]
      IF xaxis THEN BEGIN
         self.box[1,*] = [self.y - self.ticklen, (self.y + xht) < 1.0, (self.y + xht) < 1.0, $
            self.y - self.ticklen, self.y - self.ticklen]
      ENDIF ELSE BEGIN
         self.box[1,*] = [(self.y - xht) > 0.0, self.y + self.ticklen, self.y + self.ticklen, $
            (self.y - xht) > 0.0, (self.y - xht) > 0.0]
      ENDELSE

    ENDELSE

   ; Draw any objects contained within this object.
   self -> CATATOM::Draw

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;        RULER::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the RULER object. It will typically
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
PRO Ruler::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF


      'RULER PROPERTYSHEET': BEGIN

         IF event.type EQ 0 THEN BEGIN
            CASE StrUpCase(event.identifier) OF

               'COLOR': BEGIN

                  event.component -> GetProperty, Color=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, Color=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, REQUESTER=self

               ENDCASE

               'BG_COLOR': BEGIN

                  event.component -> GetProperty, BG_Color=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, BG_Color=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, REQUESTER=self

                  END

               ELSE: BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value

                  ; Refresh the graphics hierarchy. (Exit if you have deleted the object.)
                  IF Obj_Valid(self) THEN CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, REQUESTER=self ELSE RETURN

               ENDCASE

            ENDCASE
         ENDIF

         ENDCASE

        ; If you can't handle the event here. Pass it along to superclass EventHandler
        ELSE: self -> SelectableObject::EventHandler, event

   ENDCASE

   ; Report completion. Object may have been deleted.
   IF Obj_Valid(self) THEN self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       RULER::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain RULER properties. Be sure
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
;     CHARSIZE:     The character size.
;
;     LENGTH:       The absolute length of the ruler in the parent coordinate system.
;
;     PARENT_COORD: The coordinate object used to specify distance. Obtained from the
;                   parent object at DRAW time if not provided.
;
;     MINOR:        The number of minor tick intervals.
;
;     THICK:        The thickness of the ruler lines.
;
;     FORMAT:       The character format.
;
;     TICKLEN:      The tick length in normalized coordinates.
;
;     TICKS:        The number of major tick intervals.
;
;     TITLE:        The title of the ruler.
;
;     VERTICAL:     The ruler is normally position horizontally, centered at (X,Y).
;                   If this keyword is set, the ruler is position vertically
;                   at (X,Y).
;
;     X:            The ruler is centered at this X coordinate, expressed in
;                   normalized coordinates.
;
;     Y:            The ruler is centered at this Y coordinate, expressed in
;                   normalized coordinates.
;
;     _REF_EXTRA:   Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO Ruler::GetProperty, $
   CHARSIZE=charsize, $
   FORMAT=format, $
   LENGTH=length, $
   MINOR=minor, $
   PARENT_COORD=parent_coord, $
   THICK=thick, $
   TICKLEN=ticklen, $
   TICKS=ticks, $
   TITLE=title, $
   VERTICAL=vertical, $
   X=x, $
   Y=y, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(charsize) THEN charsize = self.charsize
   IF Arg_Present(length) THEN length = self.length
   IF Arg_Present(minor) THEN minor = self.minor
   IF Arg_Present(parent_coord) THEN parent_coord = self.parent_coord
   IF Arg_Present(thick) THEN thick = self.thick
   IF Arg_Present(format) THEN format = self.format
   IF Arg_Present(ticklen) THEN ticklen = self.ticklen
   IF Arg_Present(ticks) THEN ticks = self.ticks
   IF Arg_Present(title) THEN title = self.title
   IF Arg_Present(vertical) THEN vertical = self.vertical
   IF Arg_Present(x) THEN x = self.x
   IF Arg_Present(y) THEN y = self.y

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::MOVE
;
; PURPOSE:
;
;       This method moves the selectable in a graphics window.
;
; SYNTAX:
;
;       theObject -> Move, x, y
;
; ARGUMENTS:
;
;       X:        The number of pixels to move in the X direction.
;
;       Y:        The number of pixels to move in the Y direction.
;
; KEYWORDS:
;
;       NODRAW:   If this keyword is set, then no drawing of the object takes place.
;
;       PIXMAP:   A pixmap that can be supplied for fast re-draw.
;-
;*****************************************************************************************************
PRO Ruler::Move, x, y, Pixmap=pixmap, NoDraw=nodraw

   @cat_pro_error_handler

   ; Convert the device pixels into normalized coordinates.
   self -> ApplyCoords
   c = Convert_Coord(x, y, /Device, /To_Normal)

   ; Update the boundary box coordinates.
   self -> CalculateBoundaryBox

   ; Add to the boundary box.
   self.x = self.x + c[0,0]
   self.y = self.y + c[1,0]

   IF ~Keyword_Set(nodraw) THEN BEGIN

      ; Redraw, fast if you have a pixmap.
      IF Obj_Isa_Valid(pixmap, 'PIXMAPWIDGET') THEN BEGIN
         pixmap -> Copy
         self -> Draw
      ENDIF ELSE BEGIN
         CatRefreshDraw, self, Stop_At='DRAWWIDGET', /NoErase
      ENDELSE

   ENDIF

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       RULER::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the RULER object's properties. Be sure
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
;     CHARSIZE:     The character size.
;
;     DRAW:         Set this keyword to call the draw method for this object after
;                   properties have been set.
;
;     FORMAT:       The character format.
;
;     LENGTH:       The absolute length of the ruler in the parent coordinate system.
;
;     PARENT_COORD: The coordinate object used to specify distance. Obtained from the
;                   parent object at DRAW time if not provided.
;
;     MINOR:        The number of minor tick intervals.
;
;     THICK:        The thickness of the ruler lines.
;
;     TICKLEN:      The tick length in normalized coordinates.
;
;     TICKS:        The number of major tick intervals.
;
;     TITLE:        The title of the ruler.
;
;     VERTICAL:     The ruler is normally position horizontally, centered at (X,Y).
;                   If this keyword is set, the ruler is position vertically
;                   at (X,Y).
;
;     X:            The ruler is centered at this X coordinate, expressed in
;                   normalized coordinates.
;
;     Y:            The ruler is centered at this Y coordinate, expressed in
;                   normalized coordinates.
;
;     _EXTRA:       Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO Ruler::SetProperty, $
   CHARSIZE=charsize, $
   DRAW=draw, $
   FORMAT=format, $
   LENGTH=length, $
   MINOR=minor, $
   PARENT_COORD=parent_coord, $
   THICK=thick, $
   TICKLEN=ticklen, $
   TICKS=ticks, $
   TITLE=title, $
   VERTICAL=vertical, $
   X=x, $
   Y=y, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(charsize) NE 0 THEN self.charsize = charsize
   IF N_Elements(format) NE 0 THEN self.format = format
   IF N_Elements(length) NE 0 THEN BEGIN
      self.length = length
      self.currentLength = length
   ENDIF
   IF N_Elements(minor) NE 0 THEN self.minor = minor
   IF N_Elements(parent_coord) NE 0 THEN self.parent_coord = parent_coord
   IF N_Elements(thick) NE 0 THEN self.thick = thick
   IF N_Elements(ticklen) NE 0 THEN self.ticklen = ticklen
   IF N_Elements(ticks) NE 0 THEN self.ticks = ticks
   IF N_Elements(title) NE 0 THEN self.title = title
   IF N_Elements(vertical) NE 0 THEN self.vertical = Keyword_Set(vertical)
   IF N_Elements(x) NE 0 THEN self.x = x
   IF N_Elements(y) NE 0 THEN self.y = y

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> SELECTABLEOBJECT::SetProperty, NOREFRESH=norefresh, SENDMESSAGE=sendmessage, _EXTRA=extraKeywords

   ; Object may have been deleted in SELECTABLEOBJECT.
   IF Obj_Valid(self) THEN BEGIN
      IF Keyword_Set(draw) THEN self -> Draw
      self -> Report, /Completed
   ENDIF
END


;*****************************************************************************************************
;+
; NAME:
;       RULER::CLEANUP
;
; PURPOSE:
;
;       This is the RULER object class destructor method.
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
PRO Ruler::CLEANUP

   @cat_pro_error_handler

   ;self.parent_coord -> RemoveParent, self

   self -> SELECTABLEOBJECT::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       RULER::INIT
;
; PURPOSE:
;
;       This is the RULER object class initialization method
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
;     CHARSIZE:     The character size. By default, 1.0.
;
;     FORMAT:       The character format. By default, '(I3)'.
;
;     LENGTH:       The absolute length of the ruler in the parent coordinate system.
;                   By default, 1.0.
;
;     PARENT_COORD: The coordinate object used to specify distance. Obtained from the
;                   parent object at DRAW time if not provided.
;
;     MINOR:        The number of minor tick intervals. Set to 4 by default.
;
;     THICK:        The thickness of the ruler lines. By default, 1.
;
;     TICKLEN:      The tick length in normalized coordinates. By default, 0.025.
;
;     TICKS:        The number of major tick intervals. By default, 1.
;
;     TITLE:        The title of the ruler. By default, "Units".
;
;     VERTICAL:     The ruler is normally position horizontally, centered at (X,Y).
;                   If this keyword is set, the ruler is position vertically
;                   at (X,Y).
;
;     X:            The ruler is centered at this X coordinate, expressed in
;                   normalized coordinates. By default, 0.5 for horizontal
;                   rulers and 0.9 for vertical rulers. This is with respect
;                   to the parent coordinate system.
;
;     Y:            The ruler is centered at this Y coordinate, expressed in
;                   normalized coordinates. By default, 0.1 for horizontal
;                   rulers and 0.5 for vertical rulers. This is with respect
;                   to the parent coordinate system.
;
;     _EXTRA:       Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION Ruler::INIT, $
   CHARSIZE=charsize, $
   FORMAT=format, $
   LENGTH=length, $
   MINOR=minor, $
   PARENT_COORD=parent_coord, $
   THICK=thick, $
   TICKLEN=ticklen, $
   TICKS=ticks, $
   TITLE=title, $
   VERTICAL=vertical, $
   X=x, $
   Y=y, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   ok = self -> SELECTABLEOBJECT::INIT (Description='Ruler Properties', _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; Check keywords.
   IF N_Elements(charsize) EQ 0 THEN charsize = 1.0
   IF N_Elements(format) EQ 0 THEN format = ''
   IF N_Elements(length) EQ 0 THEN length = 1.0
   IF N_Elements(minor) EQ 0 THEN minor = 4
   IF N_Elements(parent_coord) EQ 0 THEN BEGIN

      ; Do you have a parent zoom coordinate system? If not, get normal one.
      IF ~Obj_Valid(self.parent_coord) THEN BEGIN
         self -> GetProperty, First_Parent=parent
         IF Obj_Valid(parent) THEN parent -> GetProperty, Zoom_Coord=parent_coord
         IF ~Obj_Valid(parent_coord) THEN BEGIN

            ; Try to get a normal coordinate system.
            IF Obj_Valid(parent) THEN parent -> GetProperty, Coord_Object=parent_coord

            IF ~Obj_Valid(parent_coord) THEN parent_coord = Obj_New('CatCoord')
         ENDIF
         self.parent_coord = parent_coord
         parent_coord -> AddParent, self
      ENDIF

   ENDIF
   IF N_Elements(thick) EQ 0 THEN thick = 1
   IF N_Elements(ticklen) EQ 0 THEN ticklen = 0.025
   IF N_Elements(ticks) EQ 0 THEN ticks = 1
   IF N_Elements(title) EQ 0 THEN title = 'Units'
   vertical = Keyword_Set(vertical)
   IF N_Elements(x) EQ 0 THEN BEGIN
      IF vertical THEN x=0.9 ELSE x = 0.5
   ENDIF
   IF N_Elements(y) EQ 0 THEN BEGIN
      IF vertical THEN y = 0.5 ELSE y = 0.1
   ENDIF

   ; Populate object.
   self.charsize = charsize
   self.format = format
   self.length = length
   self.currentLength = length
   self.minor = minor
   self.parent_coord = parent_coord
   self.thick = thick
   self.ticklen = ticklen
   self.ticks = ticks
   self.title = title
   self.vertical = vertical
   self.x = x
   self.y = y

   ; Establish a coordinate sytem for the ruler.
   self -> SetProperty, Coord_Object=Obj_New('CatCoord')
   parent_coord -> GetProperty, XRange=xrange, YRange=yrange, Position=position
   self._coords -> SetProperty, XRange=xrange, YRange=yrange, Position=position

   ; Register properties for the property sheet.
   self -> RegisterProperty, 'LENGTH', 3, NAME="Length"
   self -> RegisterProperty, 'THICK', 2, NAME="Line Thickness", VALID_RANGE=[1, 10]
   self -> RegisterProperty, 'TICKLEN', 3, NAME="Tick Length"
   self -> RegisterProperty, 'TICKS', 2, NAME="Tick Intervals", VALID_RANGE=[1, 10]
   self -> RegisterProperty, 'TITLE', 4, NAME="Title"

   self -> RegisterProperty, 'X', 3, NAME="X Location"
   self -> RegisterProperty, 'Y', 3, NAME="Y Location"
   ;self -> RegisterProperty, 'DELETE', 1, NAME='Delete Ruler'

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       RULER CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the RULER object.
;
;*****************************************************************************************************
PRO Ruler__DEFINE, class

   class = { RULER, $
             charsize: 0.0, $             ; The character size.
             currentLength: 0.0, $        ; The current length of the ruler.
             format: '', $                ; The character format.
             length: 0.0, $               ; The absolute length of the ruler.
             parent_coord: Obj_New(), $   ; The coordinate object of the parent.
             minor: 0L, $                 ; The number of minor tick intervals.
             thick: 0L, $                 ; The thickness of the ruler lines.
             ticklen: 0.0, $              ; The tick length in normalized coordinates.
             ticks: 0L, $                 ; The number of major tick intervals.
             title: '', $                 ; The title of the ruler.
             vertical: 0B, $              ; The vertical flag.
             x: 0.0, $                    ; The normalized X coordinate of ruler center.
             y: 0.0, $                    ; The normalized Y coordinate of ruler center.
             INHERITS SELECTABLEOBJECT $
           }

END


;*****************************************************************************************************
;
; NAME:
;       RULER TEST PROGRAM
;
; PURPOSE:
;
;       This is the test program for the RULER object. It may not
;       be required in your object.
;
;*****************************************************************************************************
PRO Ruler_Test

   window
   c = Obj_New('CatCoord', XRange=[0,5], YRange=[0,10])
   r = Obj_New('Ruler', Parent_Coord=c, Color='Cyan', Ticks=2, $
      Length=2, Title='mm', /Vertical)
   r -> draw
   m = Obj_New('Ruler', Parent_Coord=c, Color='Cyan', Ticks=2, $
      Length=2, Title='mm')
   m -> draw
   m -> controlpanel
END