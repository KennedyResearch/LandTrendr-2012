;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL
;
; PURPOSE:
;
;       The purpose of this routine is to create a selectable object for measuring angles
;       in a graphics window. This tool is used in an interaction. See the medical
;       image tab in the Catalyst example application for an example of its use.
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
;       newObject = Obj_New("ANGLETOOL", ...)
;       drawObject -> Add, newObject
;
; SUPERCLASSES:
;
;       SELECTABLEOBJECT
;       CATDATAATOM
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { ANGLETOOL, $
;             angle: 0.0, $                 ; The current angle of the tool.
;             arrowhead_size: 0L, $         ; The size of the arrowheads used with the tool.
;             clockwise: 0L, $              ; If set, positive angle is in clockwise direction.
;             insertedObject: Obj_New(), $  ; A reference to the new object created in the CreateNewObject method. (Ignored in CLEANUP.)
;             layerObject: Obj_New(), $     ; An optional CATLAYER object for holding the inserted selectable object.
;             linestyle: 0L, $              ; The linestyle of the selectable object.
;             modemap: Obj_New(), $         ; A pixmap for calculating "mode", or which vertex to move.
;             moveVertex: 0L, $             ; The vertex index of the vertex we are moving.
;             npoints: 0L, $                ; The  number of points currently in the triangle.
;             pointsPixID: Obj_New(), $     ; A pixmap for storing polygon points.
;             radians: 0L, $                ; If set, angle is in radians, not degrees.
;             statusbar: Obj_New(), $       ; If valid, motion events are reported to this object.
;             textcolor: "", $              ; The name of the color for the textual annotation.
;             text:Obj_New(), $             ; A TEXTLINE object for displaying AngleTool annotation.
;             thickness: 0.0, $             ; The thickness of the line drawing the selectable object.
;             sx: 0L, $                     ; The static X location.
;             sy: 0L, $                     ; The static Y location.
;             xpts: Ptr_New(), $            ; The three X points of the triangle.
;             ypts: Ptr_New(), $            ; The three Y points of the triangle.
;             INHERITS SELECTABLEOBJECT $
;           }
;
; MESSAGES:
;
;   ANGLETOOL_CHANGED:   This message is sent whenever SetProperty method is called and the NOMESSAGE keyword
;                        is NOT set.
;
; EVENT_STRUCTURE:
;
;       This object will add the following fields to the event structure created by an interaction.
;       The fields are defined as:
;
;       ANGLE:      The current angle of the Angle Tool.
;
;       CLOCKWISE:  A flag that if positive indicates the positive angle direction is in the clockwise direction.
;
;       RADIANS:    A flag that if positive indicates the angle is specified in radians rather than degress.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, July 13, 2005.
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
;       ANGLETOOL::ADDTOEVENTSTRUCTURE
;
; PURPOSE:
;
;       This method receives an event structure, which it can add information to before being sent
;       to some other event handler. Normally, this method is called by an INTERACTION object of
;       some kind.
;
; SYNTAX:
;
;       theObject -> AddToEventStructure, event
;
; ARGUMENTS:
;
;       event:      The event structure that will be added to.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
FUNCTION AngleTool::AddToEventStructure, event

   @cat_func_error_handler

   ; Add appropriate fields to the event structure.
   event = Create_Struct( event, $
      'ANGLE', self.angle, $
      'CLOCKWISE', self.clockwise, $
      'RADIANS', self.radians )

   RETURN, event

END




;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::CALCULATEBOUNDARYBOX
;
; PURPOSE:
;
;       This method calculates the extent of a boundary box about the selectable object
;       itself. The boundary box (self.box) is always stored in normalized coordinates.
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
PRO AngleTool::CalculateBoundaryBox

   @cat_pro_error_handler

   ; Create the data coordinate system.
   self -> ApplyCoords

   ; Find the min and max points in each direction.
   x1 = Min(*self.xpts, Max=x2)
   y1 = Min(*self.ypts, Max=y2)
   c = Convert_Coord([x1,x2], [y1,y2], /Data, /To_Normal)

   ; The actual boundary box is 5 pixels larger than the box itself. This
   ; serves as an aid in selecting the box handles for movement.
   b = Convert_Coord(5, 5, /Device, /To_Normal)

   self.box[0,*] = [c[0,0]-b[0,0], c[0,0]-b[0,0], c[0,1]+b[0,0], c[0,1]+b[0,0], c[0,0]-b[0,0]]
   self.box[1,*] = [c[1,0]-b[1,0], c[1,1]+b[1,0], c[1,1]+b[1,0], c[1,0]-b[1,0], c[1,0]-b[1,0]]

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the selectable object.
;
; SYNTAX:
;
;       boxObject -> ControlPanel, baseObject
;
; ARGUMENTS:
;
;       baseObject:    The object reference of a base widget for this control to
;                      be added to. If not supplied, the control panel will be in a
;                      self contained window.
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the CATCONTROLPANEL::INIT method.
;-
;*****************************************************************************************************
PRO AngleTool::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Create a new control panel

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='AngleTool Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, Name='ANGLETOOL PROPERTYSHEET', YSize=12)
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::COPYPARAMETERS
;
; PURPOSE:
;
;       This method returns the lower-left corner of the bounary box
;       in the DESTINATION keyword, and the number of columns and rows
;       in the boundary box in the EXTENT keyword, all in window or pixel
;       coordinates. It's purpose is to return a section of a pixmap, for
;       example, so that only that section can be copied.
;
; SYNTAX:
;
;       theObject -> CopyParameters, drawid, DESTINATION=destination, EXTENT=extent
;
; ARGUMENTS:
;
;       drawID:         The identifier of a draw widget object whose extent will
;                       provide the size of the window for calculating device coordinates.
;                       This parameter is required.
;
; KEYWORDS:
;
;       DESTINATION:    A two-element array containing the lower-left corner
;                       of the boundary box in device coordinates. An output keyword.
;
;       EXTENT:         A two-element array containing the number of columns and
;                       rows in the boundary box in device coordinates. An output keyword.
;
;-
;*****************************************************************************************************
PRO AngleTool::CopyParameters, drawid, DESTINATION=destination, EXTENT=extent

   @cat_pro_error_handler

   IF N_Elements(drawid) EQ 0 THEN Message, 'The DRAWID argument is required.'

   ; Make sure the draw widget is the current graphics window.
   drawID -> SetWindow

   ; Get the boundary box for the TextLine object.
   self.text -> GetProperty, Boundary_Box=text_bbox

   ; Get the largest box of the text box and the self box.
   x1 = Min([self.box[0,*], text_bbox[0,*]], Max=x2)
   y1 = Min([self.box[1,*], text_bbox[1,*]], Max=y2)
   self.box[0,*] = [x1, x1, x2, x2, x1]
   self.box[1,*] = [y1, y2, y2, y1, y1]

   ; Now find the min and max sizes of these, convert to device coordinates.
   minx = Min(self.box[0,*], Max=maxx)
   miny = Min(self.box[1,*], Max=maxy)
   c = Convert_Coord([minx, maxx-minx], [miny, maxy-miny], /Normal, /To_Device)

   destination = [c[0,0]-8,c[1,0]-8]
   extent = [c[0,1]+16, c[1,1]+16]

   ; Make sure you are not out of the window. (Primarily for X Windows devices.)
   IF destination[0] LT 0 THEN destination[0] = 0.0
   IF destination[0] GT ((!D.X_Size-1) - extent[0]) THEN destination[0] = (!D.X_Size) - extent[0] + 1
   IF destination[1] LT 0 THEN destination[1] = 0.0
   IF destination[1] GT ((!D.Y_Size-1) - extent[1]) THEN destination[1] = (!D.Y_Size) - extent[1] + 1

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::CREATENEWOBJECT
;
; PURPOSE:
;
;       This method creates a new object and adds it to both a draw widget and pixmap container.
;       If the object contains a layer object, the new object is instead added to the layer,
;       and the layer is added to the draw widget and pixmap containers.
;
; SYNTAX:
;
;       theObject -> CreateNewObject, drawID
;
; ARGUMENTS:
;
;       drawID:    The draw widget which will contain the newly created object. Required unless
;                  you are calling this method simply to make a copy of the current object.
;
;       pixmapID:  The pixmap which will contain the newly created object. Optional.
;
;
; KEYWORDS:
;
;       NEWOBJECT: An output keyword containing the new box object that gets created. If the
;                  method is called with just this keyword and no other arguments, the method
;                  functions as a object COPY.
;
;-
;*****************************************************************************************************
PRO AngleTool::CreateNewObject, drawID, pixmapID, NEWOBJECT=newObject

   @cat_pro_error_handler

   self -> GetProperty, $
      ANGLE=angle, $
      ARROWHEAD_SIZE=arrowhead_size, $
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      CLOCKWISE=clockwise, $
      RADIANS=radians, $
      STATUSBAR=statusbar, $
      LAYER=layer, $
      LINESTYLE=linestyle, $
      TEXTCOLOR=textcolor, $
      THICKNESS=thickness, $
      XPTS=xpts, $
      YPTS=ypts

   newObject = Obj_New('ANGLETOOL',  $
      ANGLE=angle, $
      ARROWHEAD_SIZE=arrowhead_size, $
      COORD_OBJECT=coord_object, $
      COLOR=color, $
      CLOCKWISE=clockwise, $
      RADIANS=radians, $
      STATUSBAR=statusbar, $
      LAYER=layer, $
      LINESTYLE=linestyle, $
      TEXTCOLOR=textcolor, $
      THICKNESS=thickness, $
      XPTS=xpts, $
      YPTS=ypts) ; Create new object.

   ; Make sure you have at least one positional parameter to proceed.
   IF N_Params() LT 1 THEN RETURN

   ; If you have a layer object, add the newObject to it instead of directly
   ; to the draw widget and pixmap.
   IF Obj_Valid(self.layerObject) THEN BEGIN

      ; Add new object to the layer object.
      self.layerObject -> Add, newObject
      self.insertedObject = newObject

      ; If the annotation object is visible, draw the new object.
      self.layerObject -> GetProperty, Visible=visible
      IF visible THEN BEGIN

         ; Draw the new object.
         newObject -> Draw

         ; Refresh the pixmap, or it will think it is ready to draw.
         IF Obj_Valid(pixmapID) THEN pixmapID -> Refresh

      ENDIF

   ENDIF ELSE BEGIN

      ; Add the object to the draw widget and pixmap.
      IF Obj_Valid(drawID) THEN drawID -> Add, newObject
      IF Obj_Valid(pixmapID) THEN pixmapID -> Add, newObject
      self.insertedObject = newObject

      ; Draw the new object.
      newObject -> Draw

   ENDELSE


   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::DRAW
;
; PURPOSE:
;
;       This method draws the selectable object in the current graphics display window.
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
;       STATUSBAR_ONLY:   If this keyword is set, and there is a valid statusbar object
;                         to update, the angle is sent to the statusbar and not displayed
;                         in the display window.
;
;-
;*****************************************************************************************************
PRO AngleTool::Draw, Statusbar_Only=sbonly, _Extra=extrakeywords

   @cat_pro_error_handler

   ; If the box is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   ; Calculate the boundary box.
   self -> CalculateBoundaryBox

   ;Draw the background if required.
   IF self.background THEN BEGIN

      IF Keyword_Set(sbonly) THEN BEGIN
         PolyFill, self.box[0,*], self.box[1,*], Fill=1, Color=FSC_Color(self.bg_color), /Normal
      ENDIF ELSE BEGIN
         self.text -> GetProperty, Boundary_Box=text_bbox

         ; Get the largest box of the text box and the self box.
         x1 = Min([self.box[0,*], text_bbox[0,*]], Max=x2)
         y1 = Min([self.box[1,*], text_bbox[1,*]], Max=y2)
         self.box[0,*] = [x1, x1, x2, x2, x1]
         self.box[1,*] = [y1, y2, y2, y1, y1]

         ; Now find the min and max sizes of these.
         minx = Min(self.box[0,*], Max=maxx)
         miny = Min(self.box[1,*], Max=maxy)

         PolyFill, [minx, minx, maxx, maxx, minx], $
                   [miny, maxy, maxy, miny, miny], $
                   Fill=1, Color=FSC_Color(self.bg_color), /Normal
      ENDELSE

   ENDIF

   ; Number of points in the vector.
   npoints = N_Elements(*self.xpts)

   ; Draw the object here.
   IF self.arrowhead_size LT 0 THEN BEGIN
      arrowhead_size = !D.X_Size/50.
   ENDIF ELSE BEGIN
      arrowhead_size = self.arrowhead_size
      IF (!D.Flags AND 1) NE 0 THEN arrowhead_size = arrowhead_size * 25
   ENDELSE

   Cat_Arrow, (*self.xpts)[npoints-2], (*self.ypts)[npoints-2], $
      (*self.xpts)[npoints-3], (*self.ypts)[npoints-3], $
      Color=self.color, Thick=self.thickness, /Solid, $
      Linestyle=self.linestyle, HSIZE=arrowhead_size, /Data

   Cat_Arrow, (*self.xpts)[npoints-2], (*self.ypts)[npoints-2], $
      (*self.xpts)[npoints-1], (*self.ypts)[npoints-1], $
      Color=self.color, Thick=self.thickness, /Solid, $
      Linestyle=self.linestyle, HSIZE=arrowhead_size, /Data

  ; Apply the coordinate system.
   self -> ApplyCoords
   ;c = Convert_Coord([0,10],[0,10], /Device, /To_Data)
   self._coords -> GetProperty, XRANGE=xr, YRANGE=yr
   xx = Abs(xr[1] - xr[0]) / !D.X_Size * 10
   yy = Abs(yr[1] - yr[0]) / !D.Y_Size * 10

   ; Where should the text be output?
   format = self.radians ? '(F5.2)' : '(F7.2)'
   IF Obj_Valid(self.statusbar) THEN BEGIN
      IF Keyword_Set(sbonly) THEN BEGIN
         self.statusbar -> SetProperty, TEXT = 'Measured Angle: ' + String(self.angle, Format=format)
      ENDIF ELSE BEGIN
         self.statusbar -> SetProperty, TEXT = 'Measured Angle: ' + String(self.angle, Format=format)
         IF (*self.ypts)[1] GT (*self.ypts)[0] THEN ypt = (*self.ypts)[0] - (yy*2.0) $
            ELSE ypt = (*self.ypts)[0] + yy
         self.text -> SetProperty, X=(*self.xpts)[0], Y=ypt, $
            Text='< ' + String(self.angle, Format=format) + ' >', Color=self.textcolor
         ENDELSE
   ENDIF ELSE BEGIN
         IF (*self.ypts)[1] GT (*self.ypts)[0] THEN ypt = (*self.ypts)[0] - (yy*2.0) $
            ELSE ypt = (*self.ypts)[0] + yy
         self.text -> SetProperty, X=(*self.xpts)[0], Y=ypt, $
            Text='< ' + String(self.angle, Format=format) + ' >', Color=self.textcolor
   ENDELSE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::DRAWSELECTIONBOX
;
; PURPOSE:
;
;       This method draws a selection box around the boundary box of the selectable object.
;
; SYNTAX:
;
;       theObject -> DrawSelectionBox
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       COLOR:    The name of a color to draw the selectable object in. By default, the color of
;                 the selectable object (self.color).
;
;-
;*****************************************************************************************************
PRO AngleTool::DrawSelectionBox, Color=color

   @cat_pro_error_handler

   ; If the box is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   IF N_Elements(color) EQ 0 THEN color = self.color

   ; Apply the coordinate system.
   self -> ApplyCoords

   ; Mark the vertices.
   PLOTS, (*self.xpts)[0], (*self.ypts)[0], PSYM=6, Color=FSC_Color(color), Symsize=1.25
   PLOTS, (*self.xpts)[1], (*self.ypts)[1], PSYM=6, Color=FSC_Color(color), Symsize=1.25
   PLOTS, (*self.xpts)[2], (*self.ypts)[2], PSYM=6, Color=FSC_Color(color), Symsize=1.25

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::EVENTHANDLER
;
; PURPOSE:
;
;       This method is an event handler for the Control Panel.
;
; SYNTAX:
;
;       Called automatically by the event handling system
;
; ARGUMENTS:
;
;       event:  The event structure.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO AngleTool::EventHandler, event

   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF


      'ANGLETOOL PROPERTYSHEET': BEGIN

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

                  event.component -> GetProperty, BG_Color=bg_color
                  event.id -> GetProperty, ID=group_leader
                  bg_color = PickColorName(bg_color, Group_Leader=group_leader)
                  event.component -> SetProperty, BG_Color=bg_color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, REQUESTER=self

               ENDCASE

               'TEXTCOLOR': BEGIN

                  event.component -> GetProperty, TextColor=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, TextColor=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, REQUESTER=self

               ENDCASE
               ELSE: BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value

                  ; Refresh the graphics hierarchy.
                  IF Obj_Valid(self) THEN BEGIN
                     CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, REQUESTER=self
                  ENDIF ELSE RETURN


               ENDCASE

            ENDCASE

         ENDIF ; of BUTTON DOWN EVENT

         ENDCASE ; of ANGLETOOL PROPERYSHEET events

        ; If you can't handle the event here. Pass it along to superclass EventHandler
        ELSE: self -> SelectableObject::EventHandler, event

   ENDCASE

   IF Obj_Valid(self) THEN self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain ANGLETOOL properties. Be sure
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
;     ANGLE:          The angle of the current Angletool object.
;
;     ARROWHEAD_SIZE: The size of the arrowhead in device coordiates. By default 1/50th of the width of
;                     the display window (!D.X_Size/50.0).
;
;     CLOCKWISE:      Set to 1 if positive angles in the clockwise direction. To 0 for counterclockwise
;                     positive direction.
;
;     HEIGHT:         The height of the polygon boundary box in normalized coordinates.
;
;     INSERTEDOBJECT: The new object that is inserted in the CreateNewObject method.
;
;     LAYER:          The annotation layer associated with this object.
;
;     LINESTYLE:      The type of linestyle required. See PLOT documentation for details.
;
;     NPOINTS:        The number of points in the polygon.
;
;     RADIANS:        Set to 1 if the angle to be reported in radians. Set to 0 for degrees.
;
;     STATUSBAR:      The statusbar object for reporting motion events.
;
;     TEXTCOLOR:      The color of the object's annotation text. By default, the same as the COLOR.
;
;     THICKNESS:      The current thickness of the polygon.
;
;     WIDTH:          The width of the polygon boundary polygon in normalized coordinates.
;
;     XPTS:           The X locations of points making up the polygon.
;
;     YPTS:           The Y locations of points making up the polygon.
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO AngleTool::GetProperty, $
   ANGLE=angle, $
   ARROWHEAD_SIZE=arrowhead_size, $
   CLOCKWISE=clockwise, $
   HEIGHT=height, $
   INSERTEDOBJECT=insertedObject, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   NPOINTS=npoints, $
   RADIANS=radians, $
   ROTATION=rotation, $
   STATUSBAR=statusbar, $
   TEXTCOLOR=textcolor, $
   THICKNESS=thickness, $
   WIDTH=width, $
   XPTS=xpts, $
   YPTS=ypts, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(angle) THEN angle = self.angle
   IF Arg_Present(arrowhead_size) THEN arrowhead_size = self.arrowhead_size
   IF Arg_Present(clockwise) THEN clockwise = self.clockwise
   IF Arg_Present(height) THEN BEGIN
      maxy = Max(self.box[1,*], Min=miny)
      height = maxy - miny
   ENDIF
   IF Arg_Present(insertedObject) THEN insertedObject = self.insertedObject
   IF Arg_Present(layer) THEN layer = self.layerObject
   IF Arg_Present(linestyle) THEN linestyle = self.linestyle
   IF Arg_Present(npoints) THEN npoints = self.npoints
   IF Arg_Present(radians) THEN radians = self.radians
   IF Arg_Present(statusbar) THEN statusbar = self.statusbar
   IF Arg_Present(send_to_back) THEN send_to_back = 0 ; Strictly for PropertySheet widget.
   IF Arg_Present(textcolor) THEN textcolor = self.textcolor
   IF Arg_Present(thickness) THEN thickness = self.thickness
   IF Arg_Present(width) THEN BEGIN
      maxx = Max(self.box[0,*], Min=minx)
      width = maxx - minx
   ENDIF
   IF Arg_Present(xpts) THEN IF Ptr_Valid(self.xpts) THEN BEGIN
      xpts = *self.xpts
   ENDIF
   IF Arg_Present(ypts) THEN IF Ptr_Valid(self.ypts) THEN BEGIN
      ypts = *self.ypts
   ENDIF

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::INTERACTIONEVENTS
;
; PURPOSE:
;
;       This method accepts events from an interaction object of some type. The interaction
;       may pre-process events, or send them directly here. You are required to have the
;       following modes: SELECT, INSERT, and DRAW. Other modes are optional and are left
;       to the programmer to interpret.
;
;       SELECT:   All SELECT mode events are passed to the selectable object for initialization
;                 of the object, as needed. If no initialization is necessary, return immediately.
;
;       DRAW:     DRAW mode events are transitory. The object disappears as soon as the drawing of
;                 the object is complete and an ACCEPT event is sent to the responsible event handler.
;
;       INSERT:   INSERT mode events result in a new selectable object being created and inserted into
;                 the object hierarchy. These objects can be selected and manipulated by other "modes"
;                 of your choosing. Include the appropriate modes here.
;
; SYNTAX:
;
;       theObject -> INTERACTIONEVENTS
;
; ARGUMENTS:
;
;     event:          The widget event that is generated by the draw widget and handled by the Interaction
;                     object.
;
; KEYWORDS:
;
;     INTERACTION:    The object reference to a Interaction object that is receiving events.
;                     This is a *required* parameter, but is written as a keyword for programming clarity.
;
;-
;*****************************************************************************************************
PRO AngleTool::InteractionEvents, event, Interaction=interaction

   @cat_pro_error_handler

   ; Get information from the INTERACTION object. In particular, you want to know the MODE, and
   ; the object references of the draw widget and pixmap.
   IF Obj_Valid(interaction) EQ 0 THEN Message, 'An "interaction" object is a required keyword parameter.'
   interaction -> GetProperty, MODE=mode, DRAWWIDGET=drawID, PIXMAP=pixmap

   ; What kind of event is this?
   eventTypes = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSED', 'CHARACTER', 'KEYPRESS']
   thisEvent = eventTypes[event.type]

   ; Action is based on the current mode of the interaction.
   CASE mode OF

      'SELECT': BEGIN

                       ; Did you click the LEFT mouse button? Then we are moving the object
                       ; or one of its vertices.
                       IF event.press EQ 1 THEN BEGIN

                         ; If you are close to a vertex circle, then you are moving a vertex
                         ; and not the polygon itself.
                         theVertex = self -> SelectVertex(event.x, event.y, DRAWID=drawID)
                         IF theVertex LE N_Elements(*self.xpts) THEN BEGIN
                            self.moveVertex = theVertex
                            interaction -> SetProperty, Mode='MOVE_VERTEX'
                         ENDIF ELSE BEGIN
                            self.sx = event.x
                            self.sy = event.y
                         ENDELSE
                       END ; of PRESS event

         END ; of SELECT mode

      'INSERT': BEGIN

         ; Only DOWN, UP, and MOTION events handled here.
          IF event.type GT 2 THEN RETURN

         ; What kind of event is this?
         possibleEvents = ['DOWN', 'UP', 'MOTION']
         thisEvent = possibleEvents[event.type]
         whichButton = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']

         ; What kind of event was this? Do the right thing here. :-)
         thisEvent = possibleEvents[event.type]

         CASE thisEvent OF

            'DOWN':BEGIN ; Button DOWN event.

               ; Which button was pressed? Branch accordingly. RIGHT button finishes trianble,
               ; anything else selects/deselects.
               theButton = whichButton[0 > event.press < 4]

               CASE theButton OF

                  'RIGHT':BEGIN

                     ; No events unless we have clicked another button first.
                     IF self.npoints LE 0 THEN RETURN

                     ; No double clicking.
                     IF event.clicks EQ 2 THEN RETURN

                     ; Need at least three points for an angle.
                     IF self.npoints LT 2 THEN BEGIN
                       ok = Dialog_Message('Three points required to create triangle.')
                       self.npoints = 0
                       Ptr_Free, self.xpts
                       Ptr_Free, self.ypts
                       RETURN
                     ENDIF

                     ; Add point to the ROI
                     self -> ApplyCoords
                     c = Convert_Coord(event.x, event.y, /Device, /To_Data)

                     IF Ptr_Valid(self.xpts) THEN IF N_Elements(*self.xpts) LT 3 THEN *self.xpts = [ *self.xpts, c[0,0]] ELSE $
                       self.xpts = Ptr_New([c[0,0]])
                     IF Ptr_Valid(self.ypts) THEN IF N_Elements(*self.ypts) LT 3 THEN *self.ypts = [ *self.ypts, c[1,0]] ELSE $
                       self.ypts = Ptr_New([c[1,0]])
                     IF N_Elements(*self.xpts) LT 3 THEN self.npoints = self.npoints + 1

                     ; Draw the completed ROI
                     self -> Draw

                     ; Motion events off.
                     drawID -> SetProperty, Motion_Events=0, /Clear_Events
                     interaction -> SetProperty, Mode='FINISHED_INSERT'
                     self -> CopyParameters, drawID, Destination=d, Extent=e
                     pixmap -> Copy, Destination=d, Extent=e,  Origin=d
                     self -> CreateNewObject, drawID, pixmap

                  END

                  ELSE: BEGIN ; It is LEFT of MIDDLE mouse button.

                     IF event.clicks EQ 1 THEN BEGIN ; Single click selects new point.

                        ; If pointsID pixmap doesn't exist, create it.
                        IF Obj_Valid(self.pointsPixID) EQ 0 THEN BEGIN
                           drawID -> GetProperty, XSize=xsize, YSize=ysize
                           self.pointsPixID = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize, Name='Points Pixmap')
                           drawID -> SetWindow
                           pixmap -> Copy
                           self.pointsPixID -> SetWindow
                           pixmap -> Copy
                           Ptr_Free, self.xpts
                           Ptr_Free, self.ypts
                           self.npoints = 0
                        ENDIF

                        drawID -> SetProperty, Motion_Events=1

                        ; Add point to ROI.
                        self -> ApplyCoords
                        c = Convert_Coord(event.x, event.y, /Device, /To_Data)

                        IF Ptr_Valid(self.xpts) THEN BEGIN
                           IF N_Elements(*self.xpts) LT 3 THEN BEGIN
                              *self.xpts = [ *self.xpts, c[0,0]]
                           ENDIF
                        ENDIF ELSE self.xpts = Ptr_New([c[0,0]])
                        IF Ptr_Valid(self.ypts) THEN BEGIN
                           IF N_Elements(*self.ypts) LT 3 THEN BEGIN
                              *self.ypts = [ *self.ypts, c[1,0]]
                            ENDIF
                        ENDIF ELSE self.ypts = Ptr_New([c[1,0]])
                        IF N_Elements(*self.xpts) LT 4 THEN self.npoints = self.npoints + 1

                        ; If you have more than two points, draw on the points pixmap.
                        IF self.npoints GE 2 THEN BEGIN
                           self.pointsPixID -> SetWindow
                           PlotS, (*self.xpts)[self.npoints-2:self.npoints-1], Color=FSC_Color(self.color), $
                                 (*self.ypts)[self.npoints-2:self.npoints-1], Thick=self.thickness, $
                                 Linestyle=self.linestyle
                        ENDIF

                        ; If this is the third point, we are finished.
                        IF self.npoints EQ 3 THEN BEGIN
                           drawID -> SetProperty, Motion=0, /Clear_Events
                           interaction -> SetProperty, Mode='FINISHED_INSERT'
                           drawID -> SetWindow
                           self -> CalculateBoundaryBox
                           self -> CopyParameters, drawID, Destination=d, Extent=e
                           pixmap -> Copy, Destination=d, Extent=e,  Origin=d
                           self -> CreateNewObject, drawID, pixmap
                           Obj_Destroy, self.pointsPixID
                        ENDIF



                        ENDIF

                     ENDELSE

               ENDCASE

               END

            'UP':

            'MOTION': BEGIN

               ; Copy and draw the last two points.
               IF self.npoints GE 1 THEN BEGIN

                  drawID -> SetWindow
                  self.pointsPixID -> Copy
                  self -> ApplyCoords
                  c = Convert_Coord([event.x, 10], [event.y, 10], /Device, /To_Data)
                  IF self.arrowhead_size LT 0 THEN arrowhead_size = !D.X_Size/50. $
                     ELSE arrowhead_size = self.arrowhead_size

                  IF self.npoints LE 1 THEN BEGIN
                  Cat_Arrow, (*self.xpts)[self.npoints-1], (*self.ypts)[self.npoints-1], c[0,0], c[1,0], $
                     Color=self.color, Thick=self.thickness, /Solid, $
                     Linestyle=self.linestyle, HSIZE=arrowhead_size, /Data
                  ENDIF

                  IF self.npoints EQ 2 THEN BEGIN

                     Cat_Arrow, (*self.xpts)[self.npoints-1], (*self.ypts)[self.npoints-1], $
                        (*self.xpts)[self.npoints-2], (*self.ypts)[self.npoints-2], $
                        Color=self.color, Thick=self.thickness, /Solid, $
                        Linestyle=self.linestyle, HSIZE=arrowhead_size, /Data

                     Cat_Arrow, (*self.xpts)[self.npoints-1], (*self.ypts)[self.npoints-1], c[0,0], c[1,0], $
                        Color=self.color, Thick=self.thickness, /Solid, $
                        Linestyle=self.linestyle, HSIZE=arrowhead_size, /Data

                     ; Calculate the angle and display it.
                     v1 = (ATAN(c[1,0] - (*self.ypts)[1], c[0,0] - (*self.xpts)[1])) * !RaDeg
                     v2 = (ATAN(((*self.ypts)[1] - (*self.ypts)[0]), (*self.xpts)[1] - (*self.xpts)[0])) * !RaDeg

                     angle = (v1 - v2)
                     angle = (angle GT 180) ? angle - 360.0 : (angle LT -180.0) ? angle + 360 : angle
                     angle = 180 - angle
                     angle = angle GT 180 ? (360 - angle)*(-1) : angle
                     self.angle = self.clockwise ? angle : -angle
                     self.angle = self.radians ? angle / !RaDeg : self.angle
                     format = self.radians ? '(F5.2)' : '(F7.2)'

                     IF Obj_Valid(self.statusbar) THEN BEGIN
                        self.statusbar -> SetProperty, TEXT='Measured Angle: ' + String(self.angle, Format=format)
                     ENDIF ELSE BEGIN
                        self._coords -> GetProperty, XRANGE=xr, YRANGE=yr
                        xx = Abs(xr[1] - xr[0]) / !D.X_Size * 10
                        yy = Abs(yr[1] - yr[0]) / !D.Y_Size * 10
                        IF (*self.ypts)[1] GT (*self.ypts)[0] THEN ypt = (*self.ypts)[0] - (yy*2.0) $
                           ELSE ypt = (*self.ypts)[0] + yy
                        self.text -> SetProperty, X=(*self.xpts)[0], Y=ypt, $
                           Text='< ' + String(self.angle, Format=format) + ' >', Color=self.textcolor
                     ENDELSE
                  ENDIF

               ENDIF
               END

           ELSE:

         ENDCASE ; of thisEvent in INSERT

         END ; of INSERT mode

      'DRAW': BEGIN

         ; Only DOWN, UP, and MOTION events handled here.
          IF event.type GT 2 THEN RETURN

         ; What kind of event is this?
         possibleEvents = ['DOWN', 'UP', 'MOTION']
         thisEvent = possibleEvents[event.type]
         whichButton = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']

         ; What kind of event was this? Do the right thing here. :-)
         thisEvent = possibleEvents[event.type]

         CASE thisEvent OF

            'DOWN':BEGIN ; Button DOWN event.

               ; Which button was pressed? Branch accordingly. RIGHT button finishes trianble,
               ; anything else selects/deselects.
               theButton = whichButton[0 > event.press < 4]

               CASE theButton OF

                  'RIGHT':BEGIN

                     ; No events unless we have clicked another button first.
                     IF self.npoints LE 0 THEN RETURN

                     ; No double clicking.
                     IF event.clicks EQ 2 THEN RETURN

                     ; Need at least three points for an angle.
                     IF self.npoints LT 2 THEN BEGIN
                       ok = Dialog_Message('Three points required to create triangle.')
                       self.npoints = 0
                       Ptr_Free, self.xpts
                       Ptr_Free, self.ypts
                       RETURN
                     ENDIF

                     ; Add point to the ROI
                     self -> ApplyCoords
                     c = Convert_Coord(event.x, event.y, /Device, /To_Data)

                     IF Ptr_Valid(self.xpts) THEN IF N_Elements(*self.xpts) LT 3 THEN *self.xpts = [ *self.xpts, c[0,0]] ELSE $
                       self.xpts = Ptr_New([c[0,0]])
                     IF Ptr_Valid(self.ypts) THEN IF N_Elements(*self.ypts) LT 3 THEN *self.ypts = [ *self.ypts, c[1,0]] ELSE $
                       self.ypts = Ptr_New([c[1,0]])
                     IF N_Elements(*self.xpts) LT 3 THEN self.npoints = self.npoints + 1

                     ; Draw the completed ROI
                     self -> Draw

                     ; Motion events off.
                     drawID -> SetProperty, Motion_Events=0, /Clear_Events
                     interaction -> SetProperty, Mode='FINISHED_DRAW'
                     self -> CopyParameters, drawID, Destination=d, Extent=e
                     pixmap -> Copy, Destination=d, Extent=e,  Origin=d

                  END

                  ELSE: BEGIN ; It is LEFT of MIDDLE mouse button.

                     IF event.clicks EQ 1 THEN BEGIN ; Single click selects new point.

                        ; If pointsID pixmap doesn't exist, create it.
                        IF Obj_Valid(self.pointsPixID) EQ 0 THEN BEGIN
                           drawID -> GetProperty, XSize=xsize, YSize=ysize
                           self.pointsPixID = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize, Name='Points Pixmap')
                           drawID -> SetWindow
                           pixmap -> Copy
                           self.pointsPixID -> SetWindow
                           pixmap -> Copy
                           Ptr_Free, self.xpts
                           Ptr_Free, self.ypts
                           self.npoints = 0
                        ENDIF

                        drawID -> SetProperty, Motion_Events=1

                        ; Add point to ROI.
                        self -> ApplyCoords
                        c = Convert_Coord(event.x, event.y, /Device, /To_Normal)

                        IF Ptr_Valid(self.xpts) THEN BEGIN
                           IF N_Elements(*self.xpts) LT 3 THEN BEGIN
                              *self.xpts = [ *self.xpts, c[0,0]]
                           ENDIF
                        ENDIF ELSE self.xpts = Ptr_New([c[0,0]])
                        IF Ptr_Valid(self.ypts) THEN BEGIN
                           IF N_Elements(*self.ypts) LT 3 THEN BEGIN
                              *self.ypts = [ *self.ypts, c[1,0]]
                            ENDIF
                        ENDIF ELSE self.ypts = Ptr_New([c[1,0]])
                        IF N_Elements(*self.xpts) LT 4 THEN self.npoints = self.npoints + 1

                        ; If you have more than two points, draw on the points pixmap.
                        IF self.npoints GE 2 THEN BEGIN
                           self.pointsPixID -> SetWindow
                           PlotS, (*self.xpts)[self.npoints-2:self.npoints-1], Color=FSC_Color(self.color), $
                                 (*self.ypts)[self.npoints-2:self.npoints-1], Thick=self.thickness, $
                                 Linestyle=self.linestyle
                        ENDIF

                        ; If this is the third point, we are finished.
                        IF self.npoints EQ 3 THEN BEGIN
                           drawID -> SetProperty, Motion=0, /Clear_Events
                           interaction -> SetProperty, Mode='FINISHED_DRAW'
                           drawID -> SetWindow
                           self -> CalculateBoundaryBox
                           self -> CopyParameters, drawID, Destination=d, Extent=e
                           pixmap -> Copy, Destination=d, Extent=e,  Origin=d
                           Obj_Destroy, self.pointsPixID
                        ENDIF



                        ENDIF

                     ENDELSE

               ENDCASE

               END

            'UP':

            'MOTION': BEGIN


               ; Copy and draw the last two points.
               IF self.npoints GE 1 THEN BEGIN

                  drawID -> SetWindow
                  self.pointsPixID -> Copy
                  self -> ApplyCoords
                  c = Convert_Coord([event.x, 10], [event.y, 10], /Device, /To_Data)
                  IF self.arrowhead_size LT 0 THEN arrowhead_size = !D.X_Size/50. $
                     ELSE arrowhead_size = self.arrowhead_size

                  IF self.npoints LE 1 THEN BEGIN
                  Cat_Arrow, (*self.xpts)[self.npoints-1], (*self.ypts)[self.npoints-1], c[0,0], c[1,0], $
                     Color=self.color, Thick=self.thickness, /Solid, $
                     Linestyle=self.linestyle, HSIZE=arrowhead_size, /Data
                  ENDIF

                  IF self.npoints EQ 2 THEN BEGIN

                     Cat_Arrow, (*self.xpts)[self.npoints-1], (*self.ypts)[self.npoints-1], $
                        (*self.xpts)[self.npoints-2], (*self.ypts)[self.npoints-2], $
                        Color=self.color, Thick=self.thickness, /Solid, $
                        Linestyle=self.linestyle, HSIZE=arrowhead_size, /Data

                     Cat_Arrow, (*self.xpts)[self.npoints-1], (*self.ypts)[self.npoints-1], c[0,0], c[1,0], $
                        Color=self.color, Thick=self.thickness, /Solid, $
                        Linestyle=self.linestyle, HSIZE=arrowhead_size, /Data

                     ; Calculate the angle and display it.
                     v1 = (ATAN(c[1,0] - (*self.ypts)[1], c[0,0] - (*self.xpts)[1])) * !RaDeg
                     v2 = (ATAN(((*self.ypts)[1] - (*self.ypts)[0]), (*self.xpts)[1] - (*self.xpts)[0])) * !RaDeg

                     angle = (v1 - v2)
                     angle = (angle GT 180) ? angle - 360.0 : (angle LT -180.0) ? angle + 360 : angle
                     angle = 180 - angle
                     angle = angle GT 180 ? (360 - angle)*(-1) : angle
                     self.angle = self.clockwise ? angle : -angle
                     self.angle = self.radians ? angle / !RaDeg : self.angle
                     format = self.radians ? '(F5.2)' : '(F7.2)'

                     IF Obj_Valid(self.statusbar) THEN BEGIN
                        self.statusbar -> SetProperty, TEXT='Measured Angle: ' + String(self.angle, Format=format)
                     ENDIF ELSE BEGIN
                        self._coords -> GetProperty, XRANGE=xy, YRANGE=yr
                        xx = (xr[1] - xr[0]) /!D.X_Size * 10
                        yy = (yr[1] - yr[0]) /!D.Y_Size * 10
                        IF (*self.ypts)[1] GT (*self.ypts)[0] THEN ypt = (*self.ypts)[0] - (yy*2.0) $
                           ELSE ypt = (*self.ypts)[0] + yy
                        self.text -> SetProperty, X=(*self.xpts)[0], Y=ypt, $
                           Text='< ' + String(self.angle, Format=format) + ' >', Color=self.textcolor
                     ENDELSE
                  ENDIF

               ENDIF
               END

           ELSE:

         ENDCASE ; of thisEvent in DRAW

         END ; of DRAW mode

         'MOVE_VERTEX': BEGIN

         ; What kind of event is this?
         possibleEvents = ['DOWN', 'UP', 'MOTION']
         thisEvent = possibleEvents[event.type]
         whichButton = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']

         ; What kind of event was this? Do the right thing here. :-)
         thisEvent = possibleEvents[event.type]

         CASE thisEvent OF

           'UP': BEGIN
                 drawID -> SetProperty, Motion_Events=0, /Clear_Events
                 drawID -> SetWindow
                 interaction -> SetProperty, Mode='FINISHED_MOVE_VERTEX'
                 pixmap -> Refresh
                 self -> Draw
                 self -> DrawSelectionBox
              END



           'MOTION': BEGIN

                 drawID -> SetWindow
                 self -> CopyParameters, drawID, Destination=d, Extent=e
                 pixmap -> Copy, Destination=d, Extent=e, Origin=d
                 self -> ApplyCoords
                 c = Convert_Coord(event.x, event.y, /Device, /To_Normal)
                 self -> GetProperty, XPTS=xpts, YPTS=ypts
                 v = Convert_Coord(xpts, ypts, /Data, /To_Normal)
                 vx = Reform(v[0,*])
                 vy = Reform(v[1,*])
                 IF self.moveVertex LE N_Elements(xpts) THEN BEGIN
                    vx[self.moveVertex] = c[0,0]
                    vy[self.moveVertex] = c[1,0]
                    c = Convert_Coord(vx,vy, /Normal, /To_Data)
                    self -> SetProperty, XPTS=Reform(c[0,*]), YPTS=Reform(c[1,*])
                 ENDIF

                 ; Calculate the angle and display it.
                 v1 = (ATAN((*self.ypts)[2] - (*self.ypts)[1], (*self.xpts)[2] - (*self.xpts)[1])) * !RaDeg
                 v2 = (ATAN(((*self.ypts)[1] - (*self.ypts)[0]), (*self.xpts)[1] - (*self.xpts)[0])) * !RaDeg

                 angle = (v1 - v2)
                 angle = (angle GT 180) ? angle - 360.0 : (angle LT -180.0) ? angle + 360 : angle
                 angle = 180 - angle
                 angle = angle GT 180 ? (360 - angle)*(-1) : angle
                 self.angle = self.clockwise ? angle : -angle
                 self.angle = self.radians ? angle / !RaDeg : self.angle
                 format = self.radians ? '(F5.2)' : '(F7.2)'

                 IF Obj_Valid(self.statusbar) THEN BEGIN
                    self.statusbar -> SetProperty, TEXT='Measured Angle: ' + String(self.angle, Format=format)
                 ENDIF

                 self -> Draw, /Statusbar_Only

                 ; Draw the selection box.
                 self -> DrawSelectionBox

              END ; Of MOTION events.
           ENDCASE

          END ; ov MOVE_VERTEX mode.

      ELSE:

   ENDCASE

   self -> Report, /Completed


END



;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::MOVE
;
; PURPOSE:
;
;       This method moves the selectable object in a graphics window.
;
; SYNTAX:
;
;       theObject -> Move, x, y
;
; ARGUMENTS:
;
;       X:          The number of pixels to move in the X direction.
;
;       Y:          The number of pixels to move in the Y direction.
;
; KEYWORDS:
;
;       NODRAW:     If this keyword is set, only the coordinates are updated. No drawing occurs.
;
;       PIXMAP:     Set this keyword to a pixmap that can be used to erase the previous
;                   contents of the window.
;-
;*****************************************************************************************************
PRO AngleTool::Move, x, y, PIXMAP=pixmap, NODRAW=noDraw

   @cat_pro_error_handler

   ; Convert the device pixels into normalized coordinates.
   self -> ApplyCoords
   self._coords -> GetProperty, XRANGE=xr, YRANGE=yr, POSITION=pos
   xx = Abs(xr[1] - xr[0]) / (!D.X_Size * Abs(pos[2]-pos[0])) * x
   yy = Abs(yr[1] - yr[0]) / (!D.Y_Size * Abs(pos[3]-pos[1])) * y
   *self.xpts = *self.xpts + xx
   *self.ypts = *self.ypts + yy

   ; Do you need to draw?
   IF ~Keyword_Set(nodraw) THEN BEGIN
      IF Obj_Isa_Valid(pixmap, 'PIXMAPWIDGET') THEN BEGIN
         self -> CopyParameters, pixmap, Destination=d, Extent=e
         pixmap -> Copy, Destination=d, Extent=e, Origin=d
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
;       ANGLETOOL::OUTLINE
;
; PURPOSE:
;
;       This method returns a box in normalized coordinates that contains the selectable object
;       and all of its parts (labels, text, etc). It is similar to the boundary box of the
;       selectable object and, in fact, is often just the boundary box. Like the boundary box,
;       the last element in the 2x5 array is the same as the first element.
;
; SYNTAX:
;
;       outline = theObject -> Outline()
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
; RETURN_VALUE:
;
;     outline:    A 2x5 array containing the XY point pairs in normalized coordinates of a rectangle
;                 big enough to contain all the parts of the object.
;
;     None.
;-
;*****************************************************************************************************
FUNCTION AngleTool::Outline

   ; Create the data coordinate system (normalized coordinates here).
   self -> ApplyCoords

   ; Find the min and max points of the angle in each direction.
   xa1 = Min(*self.xpts, Max=xa2)
   ya1 = Min(*self.ypts, Max=ya2)
   c = Convert_Coord([xa1, xa2], [ya1, ya2], /Data, /To_Normal)

   ; Find the min and max of the text object that is the label.
   toutline = self.text -> Outline()
   xb1 = Min(toutline[0,*], Max=xb2)
   yb1 = Min(toutline[1,*], Max=yb2)

   x1 = (xa1 < xb1) > 0.0
   x2 = (xa2 > xb2) < 1.0
   y1 = (ya1 < yb1) > 0.0
   y2 = (ya2 > yb2) < 1.0

   ; The actual boundary box is 5 pixels on a side larger than necessary to give some fudge room.
   b = Convert_Coord(5, 5, /Device, /To_Normal)
   theOutline = FltArr(2, 5)
   theOutline[0,*] = [c[0,0]-b[0,0], c[0,0]-b[0,0], c[0,1]+b[0,0], c[0,1]+b[0,0], c[0,0]-b[0,0]]
   theOutline[1,*] = [c[1,0]-b[1,0], c[1,1]+b[1,0], c[1,1]+b[1,0], c[1,0]-b[1,0], c[1,0]-b[1,0]]

   RETURN, theOutline

END


;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::SELECT
;
; PURPOSE:
;
;       This method returns the object reference if the requested point is inside
;       the bounding box of the object.
;
; SYNTAX:
;
;       selectedObject = theObject -> Select, x, y
;
; ARGUMENTS:
;
;       X:   The X location of a point in device or window coordinates.
;
;       Y:   The Y location of a point in device or window coordinates.
;
; KEYWORDS:
;
;       SUCCESS:   Set to 1 if a selection is made. To 0 otherwise.
;-
;*****************************************************************************************************
FUNCTION AngleTool::Select, x, y, SUCCESS=success

   @cat_func_error_handler

   ; Set up return values.
   retval = Obj_New()
   success = 0

   ; No selection is possible if the object is invisible.
   IF self.visible EQ 0 THEN RETURN, retval

   ; If you belong to a group, you cannot be selected individually.
   IF Obj_Valid(self.mygroup) THEN RETURN, retval

   ; No selection is possible, if the object is currently unselectable.
   IF ~self.selectable THEN RETURN, retval

   ; Convert the point from device to normal coordinates.
   self -> ApplyCoords
   c = Convert_Coord(x, y, /Device, /To_Normal)
   xx = c[0,0]
   yy = c[1,0]

   ; Update the box coordinates.
   self -> CalculateBoundaryBox

   ; Are you inside?
   isInside = Inside(xx, yy, Reform(self.box[0,0:3]), Reform(self.box[1,0:3]))
   IF isInside THEN BEGIN

      ; Are you within 10 pixels of any of the three vertices?
      c = Convert_Coord(*self.xpts, *self.ypts, /Data, /To_Device)
      xpts = Reform(c[0,*])
      ypts = Reform(c[1,*])
      len = SQRT((xpts-x)^2 + (ypts-y)^2)
      index = Where(len LE 10, count)

      ; If not, then you are only selected IF you are inside
      ; the triangle of vertices.
      IF count EQ 0 THEN BEGIN
         isInside = Inside(x, y, xpts, ypts)
         IF isInside THEN BEGIN
            retVal = self
            success = 1
         ENDIF

      ENDIF ELSE BEGIN

         retVal = self
         success = 1

      ENDELSE

   ENDIF

   self -> Report, /Completed
   RETURN, retval

END



;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::SELECTVERTEX
;
; PURPOSE:
;
;       This method selects a particular vertex for movement.
;
; SYNTAX:
;
;       theObject -> SelectVertex, x, y
;
; ARGUMENTS:
;
;       X:          The X location of the selection.
;
;       Y:          The Y location of the selection.
;
; KEYWORDS:
;
;       DrawID:     A window object that contains the polygon. Required for converting to
;                   the proper device coodinate system.
;-
;*****************************************************************************************************
FUNCTION AngleTool::SelectVertex, x, y, DRAWID=drawID

   @cat_func_error_handler

   ; Update the mode map
   self -> Update_Modemap
   self.modemap -> GetProperty, XSize=xsize, YSize=ysize

   ; Normalize the location with respect to the angletool.
   IF N_Elements(drawID) NE 0 THEN drawID -> SetWindow
   b = Convert_Coord([self.box[0,0], self.box[0,2]], [self.box[1,0], self.box[1,1]], /Normal, /To_Device)
   xx = 0 > (x - Round(b[0,0])) < (xsize-1)
   yy = 0 > (y - Round(b[1,0])) < (ysize-1)

   ; Take a snapshot of the modemap.
   self.modemap -> SetWindow
   map = TVRD()

   theVertex = map[xx,yy] - 1

   self -> Report, /Completed

   RETURN, theVertex
END




;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the ANGLETOOL object's properties. Be sure
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
;     Usually the same keywords that can be set in the INIT method.
;
;     ANGLE:          The angle. Usually calculated and not set. Used here to create to Angletool objects.
;
;     ARROWHEAD_SIZE: The size of the arrowhead in device coordiates. By default 1/50th of the width of
;                     the display window (!D.X_Size/50.0).
;
;     CLOCKWISE:      Normally postitive angles are calculated in a counter-clockwise direction. Setting
;                     this keyword causes the positive direction to be calculated in a clockwise direction.
;
;     LAYER:          A CATLAYER object for holding other objects. Used here only when there is an UP
;                     event in INSERT mode. At that time a copy of this object is made and inserted
;                     the layer object and this is then inserted into the DrawWidget and/or Pixmap object.
;
;     LINESTYLE:      The linestyle used to draw the selectable object.
;
;     RADIANS:        Normally the angle is reported in degrees (-180 to 180). Setting this keyword
;                     to 1 causes the angle to be reported in radians.
;
;     STATUSBAR:      Set this keyword equal to an object reference of an object with a SETPROPERTY
;                     method and a TEXT keyword. The angle will be sent to the statusbar object
;                     during the draw motion event handling.
;
;     TEXTCOLOR:      The color of the object's annotation text. By default, the same as the COLOR.
;
;
;     THICKNESS:      Set this to the thickness of the selectable object. By default, 1.0.
;
;     XPTS:           The three X points making up the triangle. The middle point is the apex of the
;                     triangle. Expressed in data coordinates.
;
;     YPTS:           The three Y points making up the triangle. The middle point is the apex of the
;                     triangle. Expressed in data coordinates.
;
;     _EXTRA:       Any keyword appropriate for the superclass SetProperty method.
;
;-
;*****************************************************************************************************
PRO AngleTool::SetProperty, $
   ANGLE=angle, $
   ARROWHEAD_SIZE=arrowhead_size, $
   CLOCKWISE=clockwise, $
   DRAW=draw, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   NOMESSAGE=nomessage, $
   NOREFRESH=norefresh, $
   RADIANS=radians, $
   STATUSBAR=statusbar, $
   TEXTCOLOR=textcolor, $
   THICKNESS=thickness, $
   XPTS=xpts, $
   YPTS=ypts, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   sendMessage = 0
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> SELECTABLEOBJECT::SetProperty, NOREFRESH=norefresh, SENDMESSAGE=sendmessage, _EXTRA=extraKeywords

   ; The object could have been deleted. If so, RETURN.
   IF ~Obj_Valid(self) THEN RETURN

   IF N_Elements(xpts) NE 0 THEN BEGIN
      IF Ptr_Valid(self.xpts) THEN BEGIN
         sendMessage = 1
         *self.xpts = xpts
      ENDIF ELSE self.xpts = Ptr_New(xpts)
   ENDIF
   IF N_Elements(ypts) NE 0 THEN BEGIN
      IF Ptr_Valid(self.ypts) THEN BEGIN
         sendMessage = 1
         *self.ypts = ypts
      ENDIF ELSE self.xpts = Ptr_New(ypts)
   ENDIF

   IF N_Elements(angle) NE 0 THEN BEGIN
      sendMessage = 1
      self.angle = angle
   ENDIF
   IF N_Elements(arrowhead_size) NE 0 THEN BEGIN
      sendMessage = 1
      self.arrowhead_size = arrowhead_size
   ENDIF
   IF N_Elements(clockwise) NE 0 THEN BEGIN
      self.clockwise = Keyword_Set(clockwise)
      v1 = (ATAN((*self.ypts)[2] - (*self.ypts)[1], (*self.xpts)[2] - (*self.xpts)[1])) * !RaDeg
      v2 = (ATAN(((*self.ypts)[1] - (*self.ypts)[0]), (*self.xpts)[1] - (*self.xpts)[0])) * !RaDeg
      angle = (v1 - v2)
      angle = (angle GT 180) ? angle - 360.0 : (angle LT -180.0) ? angle + 360 : angle
      angle = 180 - angle
      angle = angle GT 180 ? (360 - angle)*(-1) : angle
      self.angle = self.clockwise ? angle : -angle
      self.angle = self.radians ? angle / !RaDeg : self.angle
   ENDIF
   IF N_Elements(layer) NE 0 THEN BEGIN
      IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self
      self.layerObject = layer
      self.layerObject -> AddParent, self
   ENDIF
   IF N_Elements(linestyle) NE 0 THEN BEGIN
      sendMessage = 1
      self.linestyle = 0 > linestyle < 5
   ENDIF
   IF N_Elements(radians) NE 0 THEN BEGIN
      self.radians = Keyword_Set(radians)
      v1 = (ATAN((*self.ypts)[2] - (*self.ypts)[1], (*self.xpts)[2] - (*self.xpts)[1])) * !RaDeg
      v2 = (ATAN(((*self.ypts)[1] - (*self.ypts)[0]), (*self.xpts)[1] - (*self.xpts)[0])) * !RaDeg
      angle = (v1 - v2)
      angle = (angle GT 180) ? angle - 360.0 : (angle LT -180.0) ? angle + 360 : angle
      angle = 180 - angle
      angle = angle GT 180 ? (360 - angle)*(-1) : angle
      self.angle = self.clockwise ? angle : -angle
      self.angle = self.radians ? angle / !RaDeg : self.angle
   ENDIF
   IF N_Elements(statusbar) NE 0 THEN BEGIN
      IF Obj_Valid(self.statusbar) THEN self.statusbar -> RemoveParent, self
      self.statusbar = statusbar
      self.statusbar -> AddParent, self
   ENDIF
   IF N_Elements(textcolor) NE 0 THEN BEGIN
      sendMessage = 1
      self.textcolor = textcolor
      self -> SetPropertyAttribute, 'TEXTCOLOR', USERDEF=CapFirstLetter(self.textcolor)
      IF Obj_Valid(self._controlpanel) THEN self._controlpanel -> Refresh_Properties, Properties='TEXTCOLOR'
   ENDIF
   IF N_Elements(thickness) NE 0 THEN BEGIN
      sendMessage = 1
      self.thickness = thickness
   ENDIF

   ; Update the number of points.
   IF N_Elements(xpts) NE 0 THEN BEGIN
      IF N_Elements(xpts) NE N_Elements(ypts) THEN Message, 'Number of X and Y points is not the same.' ELSE $
         self.npoints = N_Elements(*self.xpts)
   ENDIF

   ; Send message?
   IF sendMessage AND ~Keyword_Set(nomessage) THEN self -> SendMessage, 'ANGLETOOL_CHANGED'

   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed


END



;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::UPDATE_MODEMAP
;
; PURPOSE:
;
;       This method updates the modemap with the latest information.
;
; SYNTAX:
;
;       self -> Update_Modemap
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;      CLEAR:     If this keyword is set, the modemap is cleared of all information.
;-
;*****************************************************************************************************
PRO AngleTool::Update_Modemap, CLEAR=clear

   @cat_pro_error_handler

   IF Keyword_Set(clear) THEN BEGIN
      self.modemap -> Refresh
      RETURN
   ENDIF

   ; Convert the boundary box from data to device coordinates.
   self -> ApplyCoords
   b = Convert_Coord(self.box, /Normal, /To_Device)
   b1 = b[0,0]
   b2 = b[0,2]
   d1 = b[1,0]
   d2 = b[1,1]

   ; Resize the modemap and refresh it.
   self.modeMap -> SetProperty, XSize=b2-b1+1, YSize=d2-d1+1
   self.modemap -> Refresh

   ; Convert from Data to Device coordinates.
   x1 = Min(*self.xpts, Max=x2)
   y1 = Min(*self.ypts, Max=y2)
   c = Convert_Coord([x1,x2], [y1,y2], /Data, /To_Device)
   xoffset = c[0,0] - 5
   yoffset = c[1,0] - 5
   x1 = c[0,0] - xoffset
   x2 = c[0,1] - xoffset
   y1 = c[1,0] - yoffset
   y2 = c[1,1] - yoffset

   ; Find midpoint of box.
   xl = Min([x1, x2], Max=xr)
   midx = (xr - xl) / 2.0 + xl
   yl = Min([y1, y2], Max=yr)
   midy = (yr - yl) / 2.0 + yl

   ; Draw circles at each vertex point, consecutively numbered.
   ; Middle point is the vertex.
   phi = Findgen(32) * (!PI * 2 / 32.)
   phi = [ phi, phi[0] ]
   UserSym, Cos(phi), Sin(phi), /Fill
   c = Convert_Coord(*self.xpts, *self.ypts, /Data, /To_DEVICE)
   xpts = c[0,*]-xoffset
   ypts = c[1,*]-yoffset

   self.modemap -> SetWindow
   Device, Decomposed=0, Get_Decomposed=theState
   TVLCT, r, g, b, /Get
   LoadCT, 0, /Silent
   POLYFILL, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=255

   FOR j=0,N_Elements(*self.xpts)-1 DO BEGIN
      PLOTS, xpts[j], ypts[j], PSYM=8, Color=j+1, /DEVICE, Symsize=2.5
   ENDFOR

   Device, Decomposed=theState
   TVLCT, r, g, b

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::CLEANUP
;
; PURPOSE:
;
;       This is the ANGLETOOL object class destructor method.
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
PRO AngleTool::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self.xpts
   Ptr_Free, self.ypts
   Obj_Destroy, self.modemap
   Obj_Destroy, self.pointsPixID
   IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self
   IF Obj_Valid(self.statusbar) THEN self.statusbar -> RemoveParent, self
   Obj_Destroy, self.text

   self -> SELECTABLEOBJECT::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ANGLETOOL::INIT
;
; PURPOSE:
;
;       This is the ANGLETOOL object class initialization method
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
;     ANGLE:          The angle. Usually calculated and not set. Used here to create to Angletool objects.
;
;     ARROWHEAD_SIZE: The size of the arrowhead in device coordiates. By default 1/50th of the width of
;                     the display window (!D.X_Size/50.0).
;
;     CLOCKWISE:      Normally postitive angles are calculated in a counter-clockwise direction. Setting
;                     this keyword causes the positive direction to be calculated in a clockwise direction.
;
;     RADIANS:        Normally the angle is reported in degrees (-180 to 180). Setting this keyword
;                     to 1 causes the angle to be reported in radians.
;
;     STATUSBAR:      Set this keyword equal to an object reference of an object with a SETPROPERTY
;                     method and a TEXT keyword. The angle will be sent to the statusbar object
;                     during the draw motion event handling. If a statusbar is used, the angle will
;                     NOT be displayed in the display window.
;
;     LAYER:          A CATLAYER object for holding other objects. Used here only when there is an UP
;                     event in INSERT mode. At that time a copy of this object is made and inserted
;                     the layer object and this is then inserted into the DrawWidget and/or Pixmap object.
;
;     LINESTYLE:      The linestyle used to draw the selectable object.
;
;     TEXTCOLOR:      The color of the object's annotation text. By default, the same as the COLOR.
;
;
;     THICKNESS:      Set this to the thickness of the selectable object. By default, 1.0.
;
;     XPTS:           The three X points making up the triangle. The middle point is the apex of the
;                     triangle. Expressed in data coordinates.
;
;     YPTS:           The three Y points making up the triangle. The middle point is the apex of the
;                     triangle. Expressed in data coordinates.
;
;     _EXTRA:         Any keywords appropriate for the SELECTABLEOBJECT INIT method.
;-
;*****************************************************************************************************
FUNCTION AngleTool::INIT, $
   ANGLE=angle, $
   ARROWHEAD_SIZE=arrowhead_size, $
   CLOCKWISE=clockwise, $
   RADIANS=radians, $
   STATUSBAR=statusbar, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   TEXTCOLOR=textcolor, $
   THICKNESS=thickness, $
   XPTS=xpts, $
   YPTS=ypts, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   ok = self -> SELECTABLEOBJECT::INIT (DESCRIPTION='AngleTool Properties', _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; If a coordinate object wasn't provided, try to copy one from the
   ; parent. If this isn't available, create a normalized one.
   IF Obj_Valid(self._coords) EQ 0 THEN BEGIN
      self -> GetProperty, First_Parent=parent
      IF Obj_Valid(parent) THEN parent -> GetProperty, Coord_Object=coords
      IF Obj_Valid(coords) THEN self._coords = coords ELSE $
         self._coords = Obj_New('CatCoord', XRange=[0,1], YRange=[0,1], Position=[0,0,1,1])
   ENDIF

   ; Check keywords.
   IF Obj_Isa_Valid(layer, "CATLAYER") NE 0 THEN BEGIN
      self.layerObject = layer
      self.layerObject -> AddParent, self
   ENDIF
   IF N_Elements(clockwise) EQ 0 THEN clockwise = 0
   IF N_Elements(arrowhead_size) EQ 0 THEN arrowhead_size = -1
   IF N_Elements(linestyle) EQ 0 THEN linestyle = 0 ELSE linestyle = 0 > linestyle < 5
   IF N_Elements(radians) EQ 0 THEN radians = 0
   IF N_Elements(statusbar) NE 0 THEN BEGIN
      IF Obj_Valid(statusbar) THEN BEGIN
         self.statusbar = statusbar
         self.statusbar -> AddParent, self
      ENDIF
   ENDIF
   IF N_Elements(textcolor) EQ 0 THEN textcolor = self.color
   IF N_Elements(thickness) EQ 0 THEN thickness = 1.0
   IF N_Elements(xpts) EQ 0 THEN xpts = [0.7, 0.0, 1.0]
   IF N_Elements(xpts) NE 3 THEN Message, 'XPTS variable must have 3 points.'
   IF N_Elements(ypts) EQ 0 THEN ypts = [0.7, 0.0, 0.0]
   IF N_Elements(ypts) NE 3 THEN Message, 'YPTS variable must have 3 points.'

   ; Load object.
   self.arrowhead_size = arrowhead_size
   self.clockwise = clockwise
   self.linestyle = linestyle
   self.radians = radians
   self.textcolor = textcolor
   self.thickness = thickness
   IF Ptr_Valid(self.xpts) THEN *self.xpts = xpts ELSE self.xpts = Ptr_New(xpts)
   IF Ptr_Valid(self.ypts) THEN *self.ypts = ypts ELSE self.ypts = Ptr_New(ypts)

   ; Create mode map.
   self.modemap = Obj_New('Pixmapwidget')

   ; Calculate angle if needed.
   IF N_Elements(angle) EQ 0 THEN BEGIN

      IF Ptr_Valid(self.xpts) THEN BEGIN
         v1 = (ATAN((*self.ypts)[2] - (*self.ypts)[1], (*self.xpts)[2] - (*self.xpts)[1])) * !RaDeg
         v2 = (ATAN(((*self.ypts)[1] - (*self.ypts)[0]), (*self.xpts)[1] - (*self.xpts)[0])) * !RaDeg

         angle = (v1 - v2)
         angle = (angle GT 180) ? angle - 360.0 : (angle LT -180.0) ? angle + 360 : angle
         angle = 180 - angle
         angle = angle GT 180 ? (360 - angle)*(-1) : angle
         angle = clockwise ? angle : -angle
         angle = radians ? angle / !RaDeg : angle
      ENDIF ELSE angle = 0.0

   ENDIF
   self.angle = angle
   self.text = Obj_New('TEXTLINE', '< ' + String(self.angle, Format=radians ? '(F5.2)' : '(F7.2)') + ' >', $
      COLOR = self.textcolor, COORD_OBJECT=self._coords)

   ; Register properties for the property sheet.
   currentVisible = self.visible
   self.visible = 0
   self -> RegisterProperty, 'BG_COLOR', 0, USERDEF=CapFirstLetter(self.bg_color), NAME="Color (Background)"
   self -> RegisterProperty, 'TEXTCOLOR', 0, USERDEF=CapFirstLetter(self.textcolor), NAME="Color (Text)"
   self -> RegisterProperty, 'ARROWHEAD_SIZE', 2, NAME="Arrowhead Size"
   self -> RegisterProperty, 'BACKGROUND', 1, NAME="Background On"
   self -> SetPropertyByIdentifier, 'BACKGROUND', self.background
   self -> RegisterProperty, 'CLOCKWISE', 1, NAME="Clockwise is Positive"
   self -> RegisterProperty, 'RADIANS', 1, NAME="Display in Radians"
   self -> SetPropertyByIdentifier, 'RADIANS', self.radians
   self -> SetPropertyByIdentifier, 'CLOCKWISE', self.clockwise
   self -> RegisterProperty, 'LINESTYLE', 2, NAME="Linestyle", VALID_RANGE=[0, 5]
   self -> RegisterProperty, 'THICKNESS', 2, NAME="Thickness", VALID_RANGE=[1, 10]
   self.visible = currentVisible

   self -> Report, /Completed

   RETURN, 1

END

;*****************************************************************************************************
;
; NAME:
;       ANGLETOOL CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the ANGLETOOL object.
;
;*****************************************************************************************************
PRO AngleTool__Define, class

   class = { ANGLETOOL, $
             angle: 0.0, $                 ; The current angle of the tool.
             arrowhead_size: 0L, $         ; The size of the arrowheads used with the tool.
             clockwise: 0L, $              ; If set, positive angle is in clockwise direction.
             insertedObject: Obj_New(), $  ; A reference to the new object created in the CreateNewObject method. (Ignored in CLEANUP.)
             layerObject: Obj_New(), $     ; An optional CATLAYER object for holding the inserted selectable object.
             linestyle: 0L, $              ; The linestyle of the selectable object.
             modemap: Obj_New(), $         ; A pixmap for calculating "mode", or which vertex to move.
             moveVertex: 0L, $             ; The vertex index of the vertex we are moving.
             npoints: 0L, $                ; The  number of points currently in the triangle.
             pointsPixID: Obj_New(), $     ; A pixmap for storing polygon points.
             radians: 0L, $                ; If set, angle is in radians, not degrees.
             statusbar: Obj_New(), $       ; If valid, motion events are reported to this object.
             textcolor: "", $              ; The name of the color for the textual annotation.
             text:Obj_New(), $             ; A TEXTLINE object for displaying AngleTool annotation.
             thickness: 0.0, $             ; The thickness of the line drawing the selectable object.
             sx: 0L, $                     ; The static X location.
             sy: 0L, $                     ; The static Y location.
             xpts: Ptr_New(), $            ; The three X points of the triangle.
             ypts: Ptr_New(), $            ; The three Y points of the triangle.
             INHERITS SELECTABLEOBJECT $
           }

END

