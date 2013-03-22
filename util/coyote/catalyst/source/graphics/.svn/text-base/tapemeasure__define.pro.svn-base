;*****************************************************************************************************
;+
; NAME:
;       TAPEMEASURE__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide a tape measure that can be displayed
;       in a direct graphics draw widget. Typically, the user drags a line between two
;       points in an image and the distance is calculated using the coordinate system
;       of the image (which is provided to the TapeMeasure.
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
;       tapeMeasureObject = Obj_New("TAPEMEASURE", X1=0.5, Y1=0.5, X2=0.75, Y2=0.75)
;       drawObject -> Add, tapeMeasureObject
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
;   class = { TAPEMEASURE, $
;             format: "", $               ; The format of the measured length. Default: '(F8.2)'.
;             headsize: 0L, $             ; The tape measure head size in pixels. By default !D.X_Size / 50.
;             label: Obj_New(), $         ; A TextLine object for labeling the distance between two points.
;             layerObject: Obj_New(), $   ; A CATLAYER object for holding the annotation.
;             linestyle: 0L, $            ; The linestyle the tape measure is drawn in.
;             midx: 0.0, $                ; The midpoint of the tape measure in X.
;             midy: 0.0, $                ; The midpoint of the tape measure in Y.
;             moveend: 0L, $              ; Indicates which end of tape measure (1 or 2) you are moving.
;             orientation: 0.0, $         ; The orientation of the tape measure
;             thickness: 0.0, $           ; The thickness of the tape measure.
;             x1: 0.0, $                  ; The X location for one end of the tape measure.
;             y1: 0.0, $                  ; The Y location for one end of the tape measure.
;             x2: 0.0, $                  ; The X location for the other end of the tape measure.
;             y2: 0.0, $                  ; The Y location for the other end of the tape measure.
;             sx: 0L, $                   ; The static end of a moving tape measure.
;             sy: 0L, $                   ; The static end of a moving tape measure.
;             units: "", $                ; A string. Appended to length measurement when displaying. Null by default.
;             userCoord: Obj_New(), $     ; A coordinate system provided by the user.
;             INHERITS SelectableObject $
;           }
;
; MESSAGES:
;
;   TAPEMEASURE_CHANGED:   This message is sent whenever SetProperty method is called and the NOMESSAGE
;                    keyword is NOT set.
;
; EVENT_STRUCTURE:
;
;       This object will add the following fields to the event structure created by an interaction.
;       The fields are defined as:
;
;       BOUNDARY_BOX:     A 2x5 element array in normalized coordinates giving the boundary box of
;                         the object.
;
;       LENGTH:           The current length of the tape measure in data coordinates.
;
;       UNITS:            A string that is the units of the length. Appended to length, usually.
;
;       XPTS:             A two element array giving the X endpoints of the line in data coordinates.
;
;       YPTS:             A two element array giving the Y endpoints of the line in data coordinates.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 25 Jan 2004.
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
;       TAPEMEASURE::ADDTOEVENTSTRUCTURE
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
FUNCTION TapeMeasure::AddToEventStructure, event

   @cat_func_error_handler

   ; Add appropriate fields to the event structure.
   self -> GetProperty, Length=length, UNITS=units, X1=x1, Y1=y1, X2=x2, Y2=y2

   event = Create_Struct( event, $
      'BOUNDARY_BOX', self.box, $
      'LENGTH', length, $
      'UNITS', units, $
      'XPTS', [x1,x2], $
      'YPTS', [y1,y2] )

   RETURN, event

END


;*****************************************************************************************************
;+
; NAME:
;       TAPEMEASURE::CALCULATEBOUNDARYBOX
;
; PURPOSE:
;
;       This method calculates the extent of a box about the tape measure.
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
PRO TapeMeasure::CalculateBoundaryBox

   @cat_pro_error_handler

   ; Apply coordinate system.
   self -> ApplyCoords

   ; Find midpoint of tapemeasure line in device coordinates.
   c = Convert_Coord([self.x1, self.x2], [self.y1, self.y2], /Data, /To_Device)
   x1 = Min(c[0,*], Max=x2)
   y1 = Min(c[1,*], Max=y2)
   midx = (x2 - x1) / 2.0D + x1
   midy = (y2 - y1) / 2.0D + y1

   ; Translate the line to origin and rotate about the Z axis.
   T3D, /Reset, Translate=[-midx, -midy, 0], Matrix=ctm
   p1 = Transpose([c[0,0], c[1,0], 0, 1])
   p2 = Transpose([c[0,1], c[1,1], 0, 1])
   p1 = ctm ## p1
   p2 = ctm ## p2
   T3D, /Reset, Rotate=[0, 0, -self.orientation], Matrix=ctm
   p1 = ctm ## p1
   p2 = ctm ## p2
   x1 = p1[0]
   y1 = p1[1]
   x2 = p2[0]
   y2 = p2[1]

   ; Make the box slight larger than the tape measure.
   IF x1 LE x2 THEN BEGIN
      x1 = x1 - 10
      x2 = x2 + 10
   ENDIF ELSE BEGIN
      x2 = x2 - 10
      x1 = x1 + 10
   ENDELSE
   IF y1 LE y2 THEN BEGIN
      y1 = y1 - 10
      y2 = y2 + 10
   ENDIF ELSE BEGIN
      y2 = y2 - 10
      y1 = y1 + 10
   ENDELSE

   p1 = Transpose([x1, y1, 0, 1])
   p2 = Transpose([x1, y2, 0, 1])
   p3 = Transpose([x2, y2, 0, 1])
   p4 = Transpose([x2, y1, 0, 1])

   ; Apply a rotation, if necessary.
   IF self.orientation NE 0.0 THEN BEGIN
      T3D, /Reset, Rotate=[0, 0, self.orientation], Matrix=ctm
      p1 = ctm ## p1
      p2 = ctm ## p2
      p3 = ctm ## p3
      p4 = ctm ## p4
   ENDIF

   ; Translate back to where you found the box.
   T3D, /Reset, Translate=[midx, midy, 0], Matrix=ctm
   v1 = ctm ## p1
   v2 = ctm ## p2
   v3 = ctm ## p3
   v4 = ctm ## p4

   ; Convert the points back to Normalized coordinates.
   c = Convert_Coord([v1[0], v2[0], v3[0], v4[0]], $
                    [v1[1], v2[1], v3[1], v4[1]], /Device, /To_Normal)

   ; Set the rotated points.
   self.box[0,*] = [c[0,0], c[0,1], c[0,2], c[0,3], c[0,0]]
   self.box[1,*] = [c[1,0], c[1,1], c[1,2], c[1,3], c[1,0]]

   ; Fudge for rotation.
   n = Convert_Coord(8,0, /Device, /To_Normal)
   fudge = n[0]
   IF Abs(self.orientation) GE 85 THEN BEGIN
      self.box[0,*] = [c[0,0]+fudge, c[0,1]-fudge, c[0,2]-fudge, c[0,3]+fudge, c[0,0]+fudge]
   ENDIF
   IF Abs(self.orientation) LE 10 THEN BEGIN
      self.box[1,*] = [c[1,0]-fudge, c[1,1]+fudge, c[1,2]+fudge, c[1,3]-fudge, c[1,0]-fudge]
   ENDIF
   IF Abs(180 - Abs(self.orientation)) LE 10 THEN BEGIN
      self.box[1,*] = [c[1,0]-fudge, c[1,1]+fudge, c[1,2]+fudge, c[1,3]-fudge, c[1,0]-fudge]
   ENDIF


   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TAPEMEASURE::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the TAPEMEASURE object.
;
; SYNTAX:
;
;       tapeMeasureObject -> ControlPanel, baseObject
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
PRO TapeMeasure::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Create a new control panel

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='TapeMeasure Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, Name='TAPEMEASURE PROPERTYSHEET', YSize=16)
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TAPEMEANSURE::COPYPARAMETERS
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
PRO TapeMeasure::CopyParameters, drawid, DESTINATION=destination, EXTENT=extent

   @cat_pro_error_handler

   IF N_Elements(drawid) EQ 0 THEN Message, 'The DRAWID argument is required.'

   ; Make sure the draw widget is the current graphics window.
   drawID -> SetWindow

   ; Find min and max for the arrow.
   minx = Min(self.box[0,*], Max=maxx)
   miny = Min(self.box[1,*], Max=maxy)

   ; Find min and max for the label.
   self.label -> GetProperty, Boundary_Box=box
   minlx = Min(box[0,*], Max=maxlx)
   minly = Min(box[1,*], Max=maxly)

   ; Get largest extent.
   minx = minx < minlx
   miny = miny < minly
   maxx = maxx > maxlx
   maxy = maxy > maxly

   self -> ApplyCoords
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
;       TAPEMEASURE::CREATENEWOBJECT
;
; PURPOSE:
;
;       This method creates a new object and adds it to both a draw widget and pixmap container.
;       If the object contains an annotation layer, the new object is instead added to the layer,
;       and the layer is added to the draw widget and pixmap container.
;
; SYNTAX:
;
;       theObject -> CreateNewObject, drawID, pixmapID
;
; ARGUMENTS:
;
;       drawID:    The draw widget which will contain the new tape measure object. Required in normal operation.
;
;       pixmapID:  The pixmap which will contain the new tape measure object. Optional.
;
; KEYWORDS:
;
;       NEWOBJECT: An output keyword containing the new tape measure object that gets created.
;
;-
;*****************************************************************************************************
PRO TapeMeasure::CreateNewObject, drawID, pixmapID, NEWOBJECT=newObject

   @cat_pro_error_handler

   self -> GetProperty, $
      BACKGROUND=background, $
      BG_COLOR=bg_color, $
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      FORMAT=format, $
      HEADSIZE=headsize, $
      LAYER=layer, $
      THICKNESS=thickness, $
      X1=x1, $
      Y1=y1, $
      X2=x2, $
      Y2=y2, $
      UNITS=units, $
      _EXTRA=extraKeywords

   newObject = Obj_New('TAPEMEASURE', $
      BACKGROUND=background, $
      BG_COLOR=bg_color, $
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      FORMAT=format, $
      HEADSIZE=headsize, $
      LAYER=layer, $
      THICKNESS=thickness, $
      VISIBLE=1, $
      X1=x1, $
      X2=x2, $
      Y1=y1, $
      Y2=y2, $
      UNITS=units)

   ; Save the new object reference.
   self.insertedObject = newObject

   ; Make sure you have at least one positional parameter to proceed.
   IF N_Params() LT 1 THEN RETURN

   ; If you have a layer object, add the newObject to it instead of directly
   ; to the draw widget and pixmap.
   IF Obj_Valid(self.layerObject) THEN BEGIN

      ; Add new object to the layer object.
      self.layerObject -> Add, newObject

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
      drawID -> Add, newObject
      IF Obj_Valid(pixmapID) THEN pixmapID -> Add, newObject

      ; Draw the new tape measure.
      newObject -> Draw

   ENDELSE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TAPEMEASURE::DRAW
;
; PURPOSE:
;
;       This method draws the tape measure in the current direct graphics display window.
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
PRO TapeMeasure::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; If the tape measure is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   ; Apply the coordinate system, if you have one.
   self -> ApplyCoords

   ; Calculate the boundary box.
   self -> CalculateBoundaryBox

   ;Draw the background if required.
   IF self.background THEN $
      PolyFill, self.box[0,*], self.box[1,*], Fill=1, Color=FSC_Color(self.bg_color), /NORMAL

   ; Draw the line.
   PLOTS, [self.x1, self.x2], [self.y1, self.y2], Color=Fsc_Color(self.color), $
      Thick=self.thickness, Linestyle=self.linestyle

   ; Draw vertical and horizontal "locators" at endpoints.
   PLOTS, [self.x1, self.x2], [self.y1, self.y2], PSYM=1, SYMSIZE=2, Color=Fsc_Color(self.color)
   ; Determine line length.

   length = StrTrim(String(SQRT((self.x1 - self.x2)^2 + (self.y1 - self.y2)^2), Format=self.format), 2)
   length = length + ' ' + self.units

   ; Convert the device pixels into data coordinates.
   self -> ApplyCoords
   self._coords -> GetProperty, XRANGE=xr, YRANGE=yr
   xx = Abs(xr[1] - xr[0]) / !D.X_Size * 15
   yy = Abs(yr[1] - yr[0]) / !D.Y_Size * 15

   ; Where should the text be output?
   CASE 1 OF
      (self.x1 LE self.x2) AND (self.y1 LE self.y2): BEGIN
         self.label -> SetProperty, X=self.x1-xx, Y=self.y1-yy, Alignment=2.0, Text=length
         END
      (self.x1 LE self.x2) AND (self.y2 LE self.y1): BEGIN
         self.label -> SetProperty, X=self.x1-xx, Y=self.y1-yy, Alignment=2.0, Text=length
         END
      (self.x1 GT self.x2) AND (self.y1 LE self.y2): BEGIN
         self.label -> SetProperty, X=self.x1+xx, Y=self.y1-yy, Alignment=0.0, Text=length
         END
      (self.x1 GT self.x2) AND (self.y2 LE self.y1): BEGIN
         self.label -> SetProperty, X=self.x1+xx, Y=self.y1-yy, Alignment=0.0, Text=length
         END
   ENDCASE

   self.label -> Draw;, _Extra=extrakeywords

   self -> GetProperty, First_Parent=interaction
   IF Obj_Isa_Valid(interaction, 'SELECTINTERACTION') THEN BEGIN
      interaction -> GetProperty, StatusBar=statusbar
      IF Obj_Valid(statusbar) THEN statusbar -> SetProperty, TEXT='Measured Length: ' + length
   ENDIF

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TAPEMEASURE::DRAWSELECTIONBOX
;
; PURPOSE:
;
;       This method draws a selection box around the boundary box of the tape measure
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
;       COLOR:    The name of a color to draw the box in. By default, the color of the tape measure.
;
;-
;*****************************************************************************************************
PRO TapeMeasure::DrawSelectionBox, Color=color

   @cat_pro_error_handler

   ; If the tape measure is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   IF N_Elements(color) EQ 0 THEN color = self.color

   ; Draw the handles on the tape measure.
   PLOTS, self.x1, self.y1, PSYM=6, Color=FSC_Color(color), Symsize=1.25
   PLOTS, self.x2, self.y2, PSYM=6, Color=FSC_Color(color), Symsize=1.25

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TAPEMEASURE::EVENTHANDLER
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
PRO TapeMeasure::EventHandler, event

   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF


      'TAPEMEASURE PROPERTYSHEET': BEGIN

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

   IF Obj_Valid(self) THEN self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       TAPEMEASURE::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain TAPEMEASURE properties. Be sure
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
;     FORMAT:      The current format for the measured length.
;
;     HEADSIZE:    The size of the arrowhead in pixel units. But default, !D.X_Size/50.
;
;     HEIGHT:      The height of the tape measure boundary box in normalized coordinates.
;
;     INSERTEDOBJECT: The new object that is inserted in the CreateNewObject method.
;
;     LABEL:       A TEXTLINE object for labelling the distance between the two points we are measuring.
;
;     LAYER:       The annotation layer associated with this object.
;
;     LENGTH:      The length of the tape measure in data coordinates.
;
;     LINESTYLE:   The type of linestyle required. See PLOT documentation for details.
;
;     ROTATION:    The current rotation of the tape measure (in degrees).
;
;     THICKNESS:   The current thickness of the tape measure.
;
;     WIDTH:       The width of the tape measure boundary box in normalized coordinates.
;
;     X1:          The X location of one end of the tape measure.
;
;     Y1:          The Y location of one end of the tape measure.
;
;     X2:          The X location of the other end of the tape measure. (Arrowhead here if TAPEMEASUREHEAD=1.)
;
;     Y2:          The Y location of the other end of the tape measure. (Arrowhead here if TAPEMEASUREHEAD=1.)
;
;     UNITS:       The current unit string.
;
;     _REF_EXTRA:  Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO TapeMeasure::GetProperty, $
   FORMAT=format, $
   HEADSIZE=headsize, $
   HEIGHT=height, $
   INSERTEDOBJECT=insertedobject, $
   LABEL=label, $
   LAYER=layer, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   ROTATION=rotation, $
   THICKNESS=thickness, $
   WIDTH=width, $
   X1=x1, $
   Y1=y1, $
   X2=x2, $
   Y2=y2, $
   UNITS=units, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(format) THEN format = self.format
   IF Arg_Present(headsize) THEN BEGIN
      IF self.headsize EQ -1 THEN headsize = !D.X_Size / 50 ELSE headsize = self.headsize
   ENDIF
   IF Arg_Present(height) THEN BEGIN
      maxy = Max(self.box[1,*], Min=miny)
      height = maxy - miny
   ENDIF
   IF Arg_Present(insertedobject) THEN insertedobject = self.insertedobject
   IF Arg_Present(label) THEN label = self.label
   IF Arg_Present(layer) THEN layer = self.layerObject
   IF Arg_Present(length) THEN BEGIN
      length = SQRT((self.x2-self.x1)^2 + (self.y2-self.y1)^2)
   ENDIF
   IF Arg_Present(linestyle) THEN linestyle = self.linestyle
   IF Arg_Present(rotation) THEN rotation = self.orientation
   IF Arg_Present(thickness) THEN thickness = self.thickness
   IF Arg_Present(width) THEN BEGIN
      maxx = Max(self.box[0,*], Min=minx)
      width = maxx - minx
   ENDIF
   IF Arg_Present(x1) THEN x1 = self.x1
   IF Arg_Present(y1) THEN y1 = self.y1
   IF Arg_Present(x2) THEN x2 = self.x2
   IF Arg_Present(y2) THEN y2 = self.y2
   IF Arg_Present(units) THEN units = self.units

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::GetProperty, _EXTRA=extraKeywords

   IF Obj_Valid(self) THEN self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       TAPEMEASURE::INTERACTIONEVENTS
;
; PURPOSE:
;
;       This method accepts events from the SelectableInteraction object based on the "mode"
;       of the SelectableInteraction object. The SelectableInteraction object can process "SELECT"
;       and "MOVE" mode events. All other mode events are sent here for processing by a SELECTABLEOBJECT.
;
;
; SYNTAX:
;
;       theObject -> INTERACTIONEVENTS
;
; ARGUMENTS:
;
;     event:          The widget event that is generated by the draw widget and handled by the SelectableInteraction
;                     interaction.
;
; KEYWORDS:
;
;     INTERACTION:    The object reference to a SELECTABLEINTERACTION object that is receiving events.
;                     This is a *required* parameter, but is written as a keyword for programming clarity.
;
;-
;*****************************************************************************************************
PRO TapeMeasure::InteractionEvents, event, Interaction=interaction

   @cat_pro_error_handler

   ; Get information from the INTERACTION object.
   IF Obj_Valid(interaction) EQ 0 THEN Message, 'An "interaction" object is a required keyword parameter.'
   interaction -> GetProperty, MODE=mode, DRAWWIDGET=drawID, PIXMAP=pixmap

   ; What kind of event is this?
   eventTypes = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSED', 'CHARACTER', 'KEYPRESS']
   thisEvent = eventTypes[event.type]

   CASE mode OF

      'SELECT': BEGIN

                       ; Did you click the LEFT mouse button? Then we need to know if we found
                       ; a tape measure end.
                       IF event.press EQ 1 THEN BEGIN

                          ; If you are close to a tape measure end (within 5%), then you are moving
                          ; the end of the tape measure you are close to.
                          self -> ApplyCoords
                          c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                          test_x = c[0,0]
                          test_y = c[1,0]
                          self -> GetProperty, X1=x1, Y1=y1, X2=x2, Y2=y2, Length=length

                          IF  (test_x GE (x2 - (length*0.1))) AND (test_x LE (x2 + (length*0.1))) $
                          AND (test_y GE (y2 - (length*0.1))) AND (test_y LE (y2 + (length*0.1))) THEN BEGIN
                             self.moveend = 2
                             interaction -> SetProperty, Mode='MOVE_TAPEMEASURE_END'
                          ENDIF

                          IF  (test_x GE (x1 - (length*0.1))) AND (test_x LE (x1 + (length*0.1))) $
                          AND (test_y GE (y1 - (length*0.1))) AND (test_y LE (y1 + (length*0.1))) THEN BEGIN
                             self.moveend = 1
                             interaction -> SetProperty, Mode='MOVE_TAPEMEASURE_END'
                          ENDIF

                       END ; of PRESS event

         END ; of SELECT mode

      'INSERT': BEGIN


         CASE thisEvent OF

           'DOWN': BEGIN

                 self -> ApplyCoords
                 c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                 self.sx = event.x
                 self.sy = event.y
                 drawID -> SetProperty, MOTION_EVENTS=1
                 self -> SetProperty, Visible=0, X1=c[0,0], Y1=c[1,0]
                 drawID -> GetProperty, XSize=xsize, YSize=ysize
                 pixmap -> GetProperty, XSize=pxsize, YSize=pysize

                 IF (xsize NE pxsize) OR (ysize NE pysize) THEN BEGIN
                    pixmap -> SetProperty, XSize=xsize, YSize=ysize
                 ENDIF
                 drawID -> SetWindow
                 pixmap -> Copy
              END

           'UP': BEGIN

                 drawID -> SetProperty, Motion_Events=0, /Clear_Events
                 drawID -> SetWindow
                 interaction -> SetProperty, Mode='FINISHED_INSERT'
                 IF (self.sx EQ event.x) AND (self.sy EQ event.y) THEN BEGIN
                     self -> SetProperty, Visible=1
                     RETURN
                 ENDIF
                 self -> ApplyCoords
                 c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                 self -> SetProperty, Visible=1, X2=c[0,0], Y2=c[1,0]
                 IF Obj_Valid(self._controlpanel) THEN $
                    self._controlpanel -> Refresh_Properties, Properties=['X2','Y2']
                 self -> CopyParameters, drawID, Destination=d, Extent=e
                 pixmap -> Copy, Destination=d, Extent=e,  Origin=d
                 self.label -> CopyParameters, drawID, Destination=d, Extent=e
                 pixmap -> Copy, Destination=d, Extent=e, Origin=d
                 self -> CreateNewObject, drawID, pixmap

              END

           'MOTION': BEGIN
                 drawID -> SetWindow
                 self -> CopyParameters, drawID, Destination=d, Extent=e
                 pixmap -> Copy, Destination=d, Extent=e, Origin=d
                 self -> ApplyCoords
                 c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                 self -> SetProperty, Visible=1, X2=c[0,0], Y2=c[1,0]
                 self -> Draw
                 IF Obj_Valid(self._controlpanel) THEN $
                    self._controlpanel -> Refresh_Properties, Properties=['X2','Y2']

              END

           ELSE:

         ENDCASE ; of thisEvent in INSERT

        END ; of INSERT mode

      'WRITE': BEGIN

        END ; of WRITE mode

      'MOVE_TAPEMEASURE_END': BEGIN

         CASE thisEvent OF


           'UP': BEGIN
                 drawID -> SetProperty, Motion_Events=0, /Clear_Events
                 drawID -> SetWindow
                 interaction -> SetProperty, Mode='FINISHED_MOVE_TAPEMEASURE_END'
                 self -> Draw
                 self -> DrawSelectionBox
              END



           'MOTION': BEGIN
                 drawID -> SetWindow
                 self -> CopyParameters, drawID, Destination=d, Extent=e
                 pixmap -> Copy, Destination=d, Extent=e, Origin=d
                 self -> ApplyCoords
                 c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                 IF self.moveend EQ 1 THEN self -> SetProperty, X1=c[0,0], Y1=c[1,0], /Draw
                 IF self.moveend NE 1 THEN self -> SetProperty, X2=c[0,0], Y2=c[1,0], /Draw

                 ; Convert the device pixels into data coordinates.
                 self._coords -> GetProperty, XRANGE=xr, YRANGE=yr
                 xx = Abs(xr[1] - xr[0]) / !D.X_Size * 15
                 yy = Abs(yr[1] - yr[0]) / !D.Y_Size * 15

                 ; Where should the text be output?
                 CASE 1 OF
                  (self.x1 LE self.x2) AND (self.y1 LE self.y2): BEGIN
                     self.label -> SetProperty, X=self.x1-xx, Y=self.y1-yy, Alignment=2.0, Text=length
                     END
                  (self.x1 LE self.x2) AND (self.y2 LE self.y1): BEGIN
                     self.label -> SetProperty, X=self.x1-xx, Y=self.y1-yy, Alignment=2.0, Text=length
                     END
                  (self.x1 GT self.x2) AND (self.y1 LE self.y2): BEGIN
                     self.label -> SetProperty, X=self.x1+xx, Y=self.y1-yy, Alignment=0.0, Text=length
                     END
                  (self.x1 GT self.x2) AND (self.y2 LE self.y1): BEGIN
                     self.label -> SetProperty, X=self.x1+xx, Y=self.y1-yy, Alignment=0.0, Text=length
                     END
                 ENDCASE

                 self.label -> Draw, _Extra=extrakeywords

                 IF Obj_Valid(self._controlpanel) THEN $
                    self._controlpanel -> Refresh_Properties, Properties=['X2', 'Y2', 'X1', 'Y1']
                 self -> DrawSelectionBox
              END


           ELSE:

         ENDCASE ; of thisEvent in MOVE_TAPEMEASURE_END

         END ; of MOVE_TAPEMEASURE_END

  ENDCASE

   self -> Report, /Completed


END



;*****************************************************************************************************
;+
; NAME:
;       TAPEMEASURE::MOVE
;
; PURPOSE:
;
;       This method moves the tape measure in a graphics window.
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
PRO TapeMeasure::Move, x, y, PIXMAP=pixmap, NODRAW=noDraw

   @cat_pro_error_handler

   ; Convert the device pixels into data coordinates.
   self -> ApplyCoords
   self._coords -> GetProperty, XRANGE=xr, YRANGE=yr, POSITION=pos
   xx = Abs(xr[1] - xr[0]) / (!D.X_Size * Abs(pos[2]-pos[0])) * x
   yy = Abs(yr[1] - yr[0]) / (!D.Y_Size * Abs(pos[3]-pos[1])) * y
   self.x1 = self.x1 + xx
   self.y1 = self.y1 + yy
   self.x2 = self.x2 + xx
   self.y2 = self.y2 + yy

   IF Obj_Valid(self._controlpanel) THEN $
      self._controlpanel -> Refresh_Properties, Properties=['X1','X2','Y1','Y2']

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
;       TAPEMEASURE::OUTLINE
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
FUNCTION TapeMeasure::Outline

   @cat_func_error_handler

   ; Create the data coordinate system.
   self -> ApplyCoords

   ; Find the min and max points of the angle in each direction.
   xa1 = Min([self.x1, self.x2], Max=xa2)
   ya1 = Min([self.y1, self.y2], Max=ya2)
   c = Convert_Coord([xa1, xa2], [ya1, ya2], /Data, /To_Normal)

   ; Find the min and max of the text object that is the label.
   toutline = self.label -> Outline()
   xb1 = Min(toutline[0,*], Max=xb2)
   yb1 = Min(toutline[1,*], Max=yb2)

   x1 = (c[0,0] < xb1) > 0.0
   x2 = (c[0,1] > xb2) < 1.0
   y1 = (c[1,0] < yb1) > 0.0
   y2 = (c[1,1] > yb2) < 1.0

   ; The actual boundary box is 5 pixels on a side larger than necessary to give some fudge room.
   b = Convert_Coord(5, 5, /Device, /To_Normal)
   theOutline = FltArr(2, 5)
   theOutline[0,*] = [x1-b[0,0], x1-b[0,0], x2+b[0,0], x2+b[0,0], x1-b[0,0]]
   theOutline[1,*] = [y1-b[1,0], y2+b[1,0], y2+b[1,0], y1-b[1,0], y1-b[1,0]]

   self -> Report, /Completed

   RETURN, theOutline

END


;*****************************************************************************************************
;+
; NAME:
;       TAPEMEASURE::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the TAPEMEASURE object's properties. Be sure
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
;     BACKGROUND:   Set this keyword to draw a background for the text.
;
;     BG_COLOR:     The name of the background color.
;
;     DRAW:         Set this keyword to immediate draw the object after properties are set.
;
;     FORMAT:       The format of the measured length.
;
;     HEADSIZE:     The size of the tape measure head in pixel units. But default, !D.X_Size/50.
;
;     LABEL:        A TEXTLINE object for labelling the distance between the two points we are measuring.
;
;     LAYER:        The annotation layer for the object.
;
;     LENGTH:       The length of the tape measure in data coordinates. Calculated from (x1,y1).
;
;     LINESTYLE:    The type of linestyle required. See PLOT documentation for details.
;
;     NOMESSAGE:    Set this keyword to suppress any messaging as a result of going through the
;                   SetProperty method. Messaging is essential for PropertySheet widget capability,
;                   but causes too many draw methods on occasion. This will prevent going through DRAW
;                   methods needlessly.
;
;     NOREFRESH:    Set this keyword if immediate refreshing of the object on the display is not required.
;
;     ROTATION:     Set this keyword to the value of the final rotation (in degrees).
;
;     THICKNESS:    Set this to the thickness of the output. By default, 1.0.
;
;     X1:           The X location of one end of the tape measure.
;
;     Y1:           The Y location of one end of the tape measure.
;
;     X2:           The X location of the other end of the tape measure. (Arrowhead here if TAPEMEASUREHEAD=1.)
;
;     Y2:           The Y location of the other end of the tape measure. (Arrowhead here if TAPEMEASUREHEAD=1.)
;
;     UNITS:        A string, appended to the measured length when displayed.
;-
;*****************************************************************************************************
PRO TapeMeasure::SetProperty, $
   BACKGROUND=background, $
   BG_COLOR=bg_color, $
   COLOR=color, $
   DRAW=draw, $
   FORMAT=format, $
   HEADSIZE=headsize, $
   LABEL=label, $
   LAYER=layer, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   NOMESSAGE=nomessage, $
   NOREFRESH=norefresh, $
   ROTATION=rotation, $
   THICKNESS=thickness, $
   X1=x1, $
   Y1=y1, $
   X2=x2, $
   Y2=y2, $
   UNITS=units, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   sendMessage = 0

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> SELECTABLEOBJECT::SetProperty, NOREFRESH=norefresh, SENDMESSAGE=sendmessage, _EXTRA=extraKeywords

   ; The object could have been deleted.
   IF ~Obj_Valid(self) THEN RETURN

   IF N_Elements(background) NE 0 THEN BEGIN
      sendMessage = 1
      self.background = Keyword_Set(background)
      self.label -> SetProperty, Background=Keyword_Set(background), /NoMessage, /NoRefresh
      IF ~Keyword_Set(norefresh) THEN CatRefreshDraw, self, Stop_At='DRAWWIDGET'
   ENDIF

   IF N_Elements(bg_color) NE 0 THEN BEGIN
      sendMessage = 1
      self.bg_color = bg_color
      self.label -> SetProperty, BG_COLOR=bg_color, /NoMessage, /NoRefresh
      self -> SetPropertyAttribute, 'BG_COLOR', USERDEF=CapFirstLetter(self.bg_color)
      IF Obj_Valid(self._controlpanel) THEN self._controlpanel -> Refresh_Properties, Properties='BG_COLOR'
   ENDIF

   IF N_Elements(color) NE 0 THEN BEGIN
      sendMessage = 1
      self.color = color
      self.label -> SetProperty, Color=color
      self -> SetPropertyAttribute, 'COLOR', USERDEF=CapFirstLetter(self.color)
      IF Obj_Valid(self._controlpanel) THEN self._controlpanel -> Refresh_Properties, Properties='COLOR'
   ENDIF

   IF N_Elements(format) NE 0 THEN BEGIN
      sendMessage = 1
      self.format = format
   ENDIF
   IF N_Elements(headsize) NE 0 THEN BEGIN
      sendMessage = 1
      self.headsize = Round(headsize)
   ENDIF
   IF N_Elements(label) NE 0 THEN BEGIN
      IF Obj_Valid(self.label) THEN self.label -> RemoveParent, self
      self.label = layer
   ENDIF
   IF N_Elements(layer) NE 0 THEN BEGIN
      IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self
      self.layerObject = layer
   ENDIF
   IF N_Elements(linestyle) NE 0 THEN BEGIN
      sendMessage = 1
      self.linestyle = 0 > linestyle < 5
   ENDIF
   IF N_Elements(thickness) NE 0 THEN BEGIN
      sendMessage = 1
      self.thickness = thickness
   ENDIF
   IF N_Elements(visible) NE 0 THEN BEGIN
      sendMessage = 1
      self.visible = visible
   ENDIF
   IF N_Elements(x1) NE 0 THEN BEGIN
      sendMessage = 1
      self.x1 = x1
   ENDIF
   IF N_Elements(y1) NE 0 THEN BEGIN
      sendMessage = 1
      self.y1 = y1
   ENDIF
   IF N_Elements(x2) NE 0 THEN BEGIN
      sendMessage = 1
      self.x2 = x2
   ENDIF
   IF N_Elements(y2) NE 0 THEN BEGIN
      sendMessage = 1
      self.y2 = y2
   ENDIF
   IF N_Elements(units) NE 0 THEN BEGIN
      sendMessage = 1
      self.units = units
   ENDIF

   IF N_Elements(rotation) NE 0 THEN BEGIN ; Must go AFTER setting of point positions!
      sendMessage = 1

      ; Rotate to 0 degrees, then rotate to specified amount.
      x1 = Min([self.x1, self.x2], Max=x2)
      y1 = Min([self.y1, self.y2], Max=y2)
      midx = (x2 - x1) / 2.0 + x1
      midy = (y2 - y1) / 2.0 + y1

      ; Translate to origin and rotate about the Z axis.
      T3D, /Reset, Translate=[-midx, -midy, 0], Rotate=[0, 0, -self.orientation], Matrix=ctm
      p1 = Transpose([self.x1, self.y1, 0, 1])
      p2 = Transpose([self.x2, self.y2, 0, 1])
      p1 = ctm ## p1
      p2 = ctm ## p2

      ; Rotate requested amount
      T3D, /Reset, Rotate=[0, 0, rotation], Matrix=ctm
      p1 = ctm ## p1
      p2 = ctm ## p2

      ; Translate back to original location.
      T3D, /Reset, Translate=[midx, midy, 0], Matrix=ctm
      p1 = ctm ## p1
      p2 = ctm ## p2
      self.x1 = p1[0]
      self.y1 = p1[1]
      self.x2 = p2[0]
      self.y2 = p2[1]

   ENDIF

   IF N_Elements(length) NE 0 THEN BEGIN
      sendMessage = 1
      self -> ApplyCoords
      c = Convert_Coord([self.x1, self.x2], [self.y1, self.y2], /Normal, /To_Data)
      x2 = self.x1 + length * Cos(self.orientation * !DtoR)
      y2 = self.y1 + length * Sin(self.orientation * !DtoR)
      self.x2 = x2
      self.y2 = y2
   ENDIF

   ; What is the orientation?
   ydiff = self.y2 - self.y1
   xdiff = self.x2 - self.x1
   self.orientation = ATan(ydiff, xdiff) * !RaDeg

   ; Find midpoint of tape measure.
   xl = Min([self.x1, self.x2], Max=xr)
   self.midx = (xr - xl) / 2.0 + xl
   yl = Min([self.y1, self.y2], Max=yr)
   self.midy = (yr - yl) / 2.0 + yl

   ; Send message?
   IF sendMessage AND ~Keyword_Set(nomessage) THEN self -> SendMessage, 'TAPEMEASURE_CHANGED'

   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TAPEMEASURE::CLEANUP
;
; PURPOSE:
;
;       This is the TAPEMEASURE object class destructor method.
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
PRO TapeMeasure::CLEANUP

   @cat_pro_error_handler

   IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self
   IF Obj_Valid(self.label) THEN self.label -> RemoveParent, self

   self -> SELECTABLEOBJECT::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TAPEMEASURE::INIT
;
; PURPOSE:
;
;       This is the TAPEMEASURE object class initialization method
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
;     FORMAT:       The format for the length measurement. By default, '(F8.2)'.
;
;     HEADSIZE:     The size of the tape measure head in pixel units. But default, !D.X_Size/50.
;
;     LABEL:        A TEXTLINE object for labelling the distance between the two points we are measuring.
;
;     LAYER:        A CATLAYER object for holding annotations.
;
;     LINESTYLE:    The linestyle of the tape measure. By default, 1.0 (solid).
;
;     THICKNESS:    Set this to the thickness of the output. By default, 1.0.
;
;     X1:           The X location of one end of the tape measure.
;
;     Y1:           The Y location of one end of the tape measure.
;
;     X2:           The X location of the other end of the tape measure. (Arrowhead here if TAPEMEASUREHEAD=1.)
;
;     Y2:           The Y location of the other end of the tape measure. (Arrowhead here if TAPEMEASUREHEAD=1.)
;
;     UNITS:        A string that is appended to the length measurement when it is displayed. By default, "".
;
;     _EXTRA:       Any keywords appropriate for the SELECTABLEOBJECT INIT method.
;-
;*****************************************************************************************************
FUNCTION TapeMeasure::INIT, $
   FORMAT=format, $
   HEADSIZE=headsize, $
   LABEL=label, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   THICKNESS=thickness, $
   X1=x1, $
   Y1=y1, $
   X2=x2, $
   Y2=y2, $
   UNITS=units, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   ok = self -> SELECTABLEOBJECT::INIT (DESCRIPTION='TapeMeasure Properties', _EXTRA=extraKeywords)
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
   IF N_Elements(format) EQ 0 THEN format = '(F8.2)'
   IF N_Elements(headsize) EQ 0 THEN headsize = !D.X_Size/50
   IF Obj_Isa_Valid(label, "TEXTLINE") NE 0 THEN BEGIN
      self.label = label
      self.label -> AddParent, self
   ENDIF ELSE self.label = Obj_New('TEXTLINE', Color=self.color, COORD_OBJECT=self._coords)
   IF Obj_Isa_Valid(layer, "CATLAYER") NE 0 THEN BEGIN
      self.layerObject = layer
      self.layerObject -> AddParent, self
   ENDIF
   IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
   IF N_Elements(orientation) EQ 0 THEN orientation = 0.0
   IF N_Elements(thickness) EQ 0 THEN thickness = 1.0
   IF N_Elements(x1) EQ 0 THEN x1 = 0.25
   IF N_Elements(y1) EQ 0 THEN y1 = 0.50
   IF N_Elements(x2) EQ 0 THEN x2 = 0.75
   IF N_Elements(y2) EQ 0 THEN y2 = 0.50
   IF N_Elements(units) EQ 0 THEN units = ""

   ; Load object.
   self.format = format
   self.headsize = headsize
   self.linestyle = 0 > linestyle < 5
   self.x1 = x1
   self.x2 = x2
   self.y1 = y1
   self.y2 = y2
   self.orientation = ATan((self.y2-self.y1),(self.x2-self.x1))*!RaDeg
   self.thickness = thickness
   self.units = units

   ; Register properties for the property sheet.
   currentVisible = self.visible
   self.visible = 0
   self -> RegisterProperty, 'BG_COLOR', 0, USERDEF=CapFirstLetter(self.bg_color), NAME="Color (Background)"
   self -> RegisterProperty, 'BACKGROUND', 1, NAME="Background On"
   self -> SetPropertyByIdentifier, 'BACKGROUND', self.background
   self -> RegisterProperty, 'FORMAT', 4, NAME="Label Format"
   self -> RegisterProperty, 'LENGTH', 3, NAME="Length"
   self -> RegisterProperty, 'LINESTYLE', 2, NAME="Linestyle", VALID_RANGE=[0, 5]
   self -> RegisterProperty, 'ROTATION', 3, NAME="Rotate", VALID_RANGE=[-180, 180]
   self -> RegisterProperty, 'THICKNESS', 2, NAME="Thickness", VALID_RANGE=[1, 10]
   self -> RegisterProperty, 'X1', 3, NAME="X1 Location"
   self -> RegisterProperty, 'Y1', 3, NAME="Y1 Location"
   self -> RegisterProperty, 'X2', 3, NAME="X2 Location"
   self -> RegisterProperty, 'Y2', 3, NAME="Y2 Location"
   self -> RegisterProperty, 'UNITS', 4, NAME="Label Units"
   self.visible = currentVisible

   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       TAPEMEASURE CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the TAPEMEASURE object.
;
;*****************************************************************************************************
PRO TapeMeasure__Define, class

   class = { TAPEMEASURE, $
             format: "", $               ; The format of the measured length. Default: '(F8.2)'.
             headsize: 0L, $             ; The tape measure head size in pixels. By default !D.X_Size / 50.
             insertedObject: Obj_New(), $; The new object created in the CreateNewObject method. (Ignored in CLEANUP.)
             label: Obj_New(), $         ; A TextLine object for labeling the distance between two points.
             layerObject: Obj_New(), $   ; A CATLAYER object for holding the annotation.
             linestyle: 0L, $            ; The linestyle the tape measure is drawn in.
             midx: 0.0, $                ; The midpoint of the tape measure in X.
             midy: 0.0, $                ; The midpoint of the tape measure in Y.
             moveend: 0L, $              ; Indicates which end of tape measure (1 or 2) you are moving.
             orientation: 0.0D, $         ; The orientation of the tape measure
             thickness: 0.0, $           ; The thickness of the tape measure.
             x1: 0.0D, $                 ; The X location for one end of the tape measure.
             y1: 0.0D, $                 ; The Y location for one end of the tape measure.
             x2: 0.0D, $                 ; The X location for the other end of the tape measure.
             y2: 0.0D, $                 ; The Y location for the other end of the tape measure.
             sx: 0L, $                   ; The static end of a moving tape measure.
             sy: 0L, $                   ; The static end of a moving tape measure.
             units: "", $                ; A string. Appended to length measurement when displaying. Null by default.
             INHERITS SelectableObject $
           }

END
