;*****************************************************************************************************
;+
; NAME:
;       ARROW__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide an arrow that can be displayed
;       in a direct graphics draw widget. The coordinate system of the Arrow
;       object is either passed to it (a CatCoord object) or is a normalized
;       coordinate system by default.
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
;       arrowObject = Obj_New("ARROW", X1=0.5, Y1=0.5, X2=0.75, Y2=0.75)
;       drawObject -> Add, arrowObject
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
;   class = { ARROW, $
;             arrowhead: 0L, $            ; A flag to indicate which arrow heads should be drawn.
;             headsize: 0L, $             ; The arrow head size in pixels. By default !D.X_Size / 50.
;             layerObject: Obj_New(), $   ; A CATLAYER object for holding the annotation.
;             linestyle: 0L, $            ; The linestyle the arrow is drawn in.
;             midx: 0.0, $                ; The midpoint of the arrow in X.
;             midy: 0.0, $                ; The midpoint of the arrow in Y.
;             moveend: 0L, $              ; Indicates which end of arrow (1 or 2) you are moving.
;             orientation: 0.0, $         ; The orientation of the arrow
;             thickness: 0.0, $           ; The thickness of the arrow.
;             x1: 0.0, $                  ; The X location for one end of the arrow.
;             y1: 0.0, $                  ; The Y location for one end of the arrow.
;             x2: 0.0, $                  ; The X location for the other end of the arrow.
;             y2: 0.0, $                  ; The Y location for the other end of the arrow.
;             sx: 0L, $                   ; The static end of a moving arrow.
;             sy: 0L, $                   ; The static end of a moving arrow.
;             INHERITS SelectableObject $
;           }
;
; MESSAGES:
;
;   ARROW_CHANGED:   This message is sent whenever SetProperty method is called and the NOMESSAGE
;                    keyword is NOT set.
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
;       ARROW::CALCULATEBOUNDARYBOX
;
; PURPOSE:
;
;       This method calculates the extent of a box about the arrow.
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
PRO Arrow::CalculateBoundaryBox

   @cat_pro_error_handler

   ; Apply coordinate system.
   self -> ApplyCoords

   ; Find midpoint of arrow in device coordinates.
   c = Convert_Coord([self.x1, self.x2], [self.y1, self.y2], /Data, /To_Device)
   x1 = Min(c[0,*], Max=x2)
   y1 = Min(c[1,*], Max=y2)
   midx = (x2 - x1) / 2.0 + x1
   midy = (y2 - y1) / 2.0 + y1

   ; Translate to origin and rotate about the Z axis.
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

   ; Make the box slight larger than the arrow.
   IF x1 LE x2 THEN BEGIN
      x1 = x1 - 5
      x2 = x2 + 5
   ENDIF ELSE BEGIN
      x2 = x2 - 5
      x1 = x1 + 5
   ENDELSE
   IF y1 LE y2 THEN BEGIN
      y1 = y1 - 5
      y2 = y2 + 5
   ENDIF ELSE BEGIN
      y2 = y2 - 5
      y1 = y1 + 5
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

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       ARROW::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the ARROW object.
;
; SYNTAX:
;
;       arrowObject -> ControlPanel, baseObject
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
PRO Arrow::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Create a new control panel

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Arrow Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, Name='ARROW PROPERTYSHEET', YSize=15)
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ARROW::CREATENEWOBJECT
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
;       drawID:    The draw widget which will contain the new arrow object. Required in normal operation.
;
;       pixmapID:  The pixmap which will contain the new arrow object. Optional.
;
; KEYWORDS:
;
;       NEWOBJECT: An output keyword containing the new arrow object that gets created.
;
;-
;*****************************************************************************************************
PRO Arrow::CreateNewObject, drawID, pixmapID, NEWOBJECT=newObject

   @cat_pro_error_handler

   self -> GetProperty, $
      ARROWHEAD=arrowhead, $
      BACKGROUND=background, $
      BG_COLOR=bg_color, $
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      HEADSIZE=headsize, $
      LAYER=layer, $
      THICKNESS=thickness, $
      X1=x1, $
      Y1=y1, $
      X2=x2, $
      Y2=y2, $
      _EXTRA=extraKeywords

   newObject = Obj_New('ARROW', $
      ARROWHEAD=arrowhead, $
      BACKGROUND=background, $
      BG_COLOR=bg_color, $
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      HEADSIZE=headsize, $
      LAYER=layer, $
      THICKNESS=thickness, $
      VISIBLE=1, $
      X1=x1, $
      X2=x2, $
      Y1=y1, $
      Y2=y2)

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

      ; Draw the new arrow.
      newObject -> Draw

   ENDELSE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ARROW::DRAW
;
; PURPOSE:
;
;       This method draws the arrow in the current direct graphics display window.
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
PRO Arrow::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; If the arrow is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   ; Apply the coordinate system.
   self -> ApplyCoords

   ; Calculate the boundary box.
   self -> CalculateBoundaryBox

   ; Draw the background if required.
   IF self.background THEN $
      PolyFill, self.box[0,*], self.box[1,*], Fill=1, Color=FSC_Color(self.bg_color), /Normal

   ; Output the line.
   PLOTS, [self.x1, self.x2], [self.y1, self.y2], Color=Fsc_Color(self.color), $
      Thick=self.thickness, Linestyle=self.linestyle

   ; Draw the arrowheads. Headsize is the length of the arrowhead (along the arrow axis).
   IF self.headsize EQ -1 THEN BEGIN
      headsize = !D.X_Size / 50
   ENDIF ELSE BEGIN
      headsize = self.headsize
      IF (!D.Flags AND 1) NE 0 THEN headsize = headsize * 25
   ENDELSE

   c = Convert_Coord([self.x1, self.x2], [self.y1, self.y2], /Data, /To_Device)
   x1 = c[0,0]
   x2 = c[0,1]
   y1 = c[1,0]
   y2 = c[1,1]
   length = SQRT((x2-x1)^2 + (y2-y1)^2)

   ; If headsize GT 15, then use 20% arrow. Otherwise use 30%.
   IF headsize LT 15 THEN BEGIN
      mcost = -0.866D
      sint = 0.500D
      msint = -sint
   ENDIF ELSE BEGIN
      mcost = - 0.939693D
      sint = 0.342020D
      msint = -sint
   ENDELSE
   costheta = (x2-x1) / length
   sintheta = (y2-y1) / length

   CASE self.arrowhead OF

      1: BEGIN ; Single arrowhead at (x2,y2)

          xx0 = x2 + headsize * (costheta*mcost - sintheta*msint)
          yy0 = y2 + headsize * (costheta*msint + sintheta*mcost)
          xx1 = x2 + headsize * (costheta*mcost - sintheta*sint)
          yy1 = y2 + headsize * (costheta*sint + sintheta*mcost)
          PolyFill, [xx0, xx1, x2, xx0], [yy0, yy1, y2, yy0], /Device, Color=Fsc_Color(self.color)

         END

      2: BEGIN ; Double arrow heads
          xx0 = x2 + headsize * (costheta*mcost - sintheta*msint)
          yy0 = y2 + headsize * (costheta*msint + sintheta*mcost)
          xx1 = x2 + headsize * (costheta*mcost - sintheta*sint)
          yy1 = y2 + headsize * (costheta*sint + sintheta*mcost)
          PolyFill, [xx0, xx1, x2, xx0], [yy0, yy1, y2, yy0], /Device, Color=Fsc_Color(self.color)

          xx0 = x1 - headsize * (costheta*mcost - sintheta*msint)
          yy0 = y1 - headsize * (costheta*msint + sintheta*mcost)
          xx1 = x1 - headsize * (costheta*mcost - sintheta*sint)
          yy1 = y1 - headsize * (costheta*sint + sintheta*mcost)
          PolyFill, [xx0, xx1, x1, xx0], [yy0, yy1, y1, yy0], /Device, Color=Fsc_Color(self.color)
         END

      3: BEGIN ; Single arrowhead at (x1,y1)
          xx0 = x1 - headsize * (costheta*mcost - sintheta*msint)
          yy0 = y1 - headsize * (costheta*msint + sintheta*mcost)
          xx1 = x1 - headsize * (costheta*mcost - sintheta*sint)
          yy1 = y1 - headsize * (costheta*sint + sintheta*mcost)
          PolyFill, [xx0, xx1, x1, xx0], [yy0, yy1, y1, yy0], /Device, Color=Fsc_Color(self.color)
         END

      ELSE: ; No arrowheads.

   ENDCASE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ARROW::DRAWSELECTIONBOX
;
; PURPOSE:
;
;       This method draws a selection box around the boundary box of the arrow
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
;       COLOR:    The name of a color to draw the box in. By default, the color of the arrow.
;
;-
;*****************************************************************************************************
PRO Arrow::DrawSelectionBox, Color=color

   @cat_pro_error_handler

   ; If the arrow is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   ; Apply the coordinate system.
   self -> ApplyCoords

   IF N_Elements(color) EQ 0 THEN color = self.color

   PLOTS, self.x1, self.y1, PSYM=6, Color=FSC_Color(color), Symsize=1.25
   PLOTS, self.x2, self.y2, PSYM=6, Color=FSC_Color(color), Symsize=1.25

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       ARROW::EVENTHANDLER
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
PRO Arrow::EventHandler, event

   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF


      'ARROW PROPERTYSHEET': BEGIN

         IF event.type EQ 0 THEN BEGIN
            CASE StrUpCase(event.identifier) OF

               'COLOR': BEGIN

                  event.component -> GetProperty, Color=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, Color=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, Requester=self

               ENDCASE

               'BG_COLOR': BEGIN

                  event.component -> GetProperty, BG_Color=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, BG_Color=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, Requester=self

                  END

               ELSE: BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value

                  ; Refresh the graphics hierarchy. (Exit if you have deleted the object.)
                  IF Obj_Valid(self) THEN $
                     CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase , Requester=self $
                     ELSE RETURN

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
;       ARROW::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain ARROW properties. Be sure
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
;     ARROWHEAD:   Which arrowheads should be drawn? The current state.
;
;     HEADSIZE:    The size of the arrow head in pixel units. But default, !D.X_Size/50.
;
;     HEIGHT:      The height of the arrow boundary box in normalized coordinates.
;
;     LAYER:       The annotation layer associated with this object.
;
;     LENGTH:      The length of the arrow in data coordinates.
;
;     LINESTYLE:   The type of linestyle required. See PLOT documentation for details.
;
;     ROTATION:    The current rotation of the arrow (in degrees).
;
;     THICKNESS:   The current thickness of the arrow.
;
;     WIDTH:       The width of the arrow boundary box in normalized coordinates.
;
;     X1:          The X location of one end of the arrow.
;
;     Y1:          The Y location of one end of the arrow.
;
;     X2:          The X location of the other end of the arrow. (Arrowhead here if ARROWHEAD=1.)
;
;     Y2:          The Y location of the other end of the arrow. (Arrowhead here if ARROWHEAD=1.)
;
;     _REF_EXTRA:  Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO Arrow::GetProperty, $
   ARROWHEAD=arrowhead, $
   HEADSIZE=headsize, $
   HEIGHT=height, $
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
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(arrowhead) THEN arrowhead = self.arrowhead
   IF Arg_Present(headsize) THEN BEGIN
      headsize = self.headsize
   ENDIF
   IF Arg_Present(height) THEN BEGIN
      maxy = Max(self.box[1,*], Min=miny)
      height = maxy - miny
   ENDIF
   IF Arg_Present(layer) THEN layer = self.layerObject
   IF Arg_Present(length) THEN BEGIN
      length = SQRT((self.x2-self.x1)^2 + (self.y2-self.y1)^2)
   END
   IF Arg_Present(linestyle) THEN linestyle = self.linestyle
   IF Arg_Present(rotation) THEN rotation = self.orientation
   IF Arg_Present(thickness) THEN thickness = self.thickness
   IF Arg_Present(width) THEN BEGIN
      maxx = Max(self.box[0,*], Min=minx)
      width = maxx - minx
   ENDIF
   IF Arg_Present(x1) THEN x1 = self.x1
   IF Arg_Present(x2) THEN x2 = self.x2
   IF Arg_Present(y1) THEN y1 = self.y1
   IF Arg_Present(y2) THEN y2 = self.y2

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::GetProperty, _EXTRA=extraKeywords

   IF Obj_Valid(self) THEN self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ARROW::INTERACTIONEVENTS
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
PRO Arrow::InteractionEvents, event, Interaction=interaction

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
                       ; an arrow end.
                       IF event.press EQ 1 THEN BEGIN

                          ; If you are close to an arrow end (within 5%), then you are moving
                          ; the end of the arrow you are close to.
                          self -> ApplyCoords
                          c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                          test_x = c[0,0]
                          test_y = c[1,0]
                          self -> GetProperty, X1=x1, Y1=y1, X2=x2, Y2=y2, Length=length

                          IF  (test_x GE (x2 - (length*0.05))) AND (test_x LE (x2 + (length*0.05))) $
                          AND (test_y GE (y2 - (length*0.05))) AND (test_y LE (y2 + (length*0.05))) THEN BEGIN
                             self.moveend = 2
                             interaction -> SetProperty, Default_Object=Obj_New(), Mode='MOVE_ARROW_END'
                          ENDIF

                          IF  (test_x GE (x1 - (length*0.05))) AND (test_x LE (x1 + (length*0.05))) $
                          AND (test_y GE (y1 - (length*0.05))) AND (test_y LE (y1 + (length*0.05))) THEN BEGIN
                             self.moveend = 1
                             interaction -> SetProperty, Default_Object=Obj_New(), Mode='MOVE_ARROW_END'
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
                 interaction -> SetProperty, Mode='SELECT'
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
                 self -> CreateNewObject, drawID, pixmap

              END

           'MOTION': BEGIN
                 drawID -> SetWindow
                 self -> CopyParameters, drawID, Destination=d, Extent=e
                 pixmap -> Copy, Destination=d, Extent=e, Origin=d
                 self -> ApplyCoords
                 c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                 self -> SetProperty, Visible=1, X2=c[0,0], Y2=c[1,0], /Draw
                 IF Obj_Valid(self._controlpanel) THEN $
                     self._controlpanel -> Refresh_Properties, Properties=['X2','Y2']
                 ;self -> Draw
              END

           ELSE:

         ENDCASE ; of thisEvent in INSERT

        END ; of INSERT mode

      'WRITE': BEGIN

        END ; of WRITE mode

      'MOVE_ARROW_END': BEGIN

         CASE thisEvent OF


           'UP': BEGIN
                 drawID -> SetProperty, Motion_Events=0, /Clear_Events
                 drawID -> SetWindow
                 interaction -> SetProperty, Mode='SELECT'
                 self -> Draw
                 self -> DrawSelectionBox
                 pixmap -> Refresh

              END



           'MOTION': BEGIN
                 drawID -> SetWindow
                 self -> CopyParameters, drawID, Destination=d, Extent=e
                 pixmap -> Copy, Destination=d, Extent=e, Origin=d
                 self -> ApplyCoords
                 c = Convert_Coord([event.x,0], [event.y,0], /Device, /To_Data)
                 IF self.moveend EQ 1 THEN BEGIN
                    self -> SetProperty, X1=c[0,0], Y1=c[1,0], /Draw
                    IF Obj_Valid(self._controlpanel) THEN $
                        self._controlpanel -> Refresh_Properties, Properties=['X1','Y1']
                 ENDIF
                  IF self.moveend NE 1 THEN BEGIN
                     self -> SetProperty, X2=c[0,0], Y2=c[1,0], /Draw
                     IF Obj_Valid(self._controlpanel) THEN $
                        self._controlpanel -> Refresh_Properties, Properties=['X2','Y2']
                 ENDIF
                 self -> DrawSelectionBox
              END


           ELSE:

         ENDCASE ; of thisEvent in MOVE_ARROW_END

         END ; of MOVE_ARROW_END

  ENDCASE

   self -> Report, /Completed


END



;*****************************************************************************************************
;+
; NAME:
;       ARROW::MOVE
;
; PURPOSE:
;
;       This method moves the arrow in a graphics window.
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
PRO Arrow::Move, x, y, PIXMAP=pixmap, NODRAW=noDraw

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
;       ARROW::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the ARROW object's properties. Be sure
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
;     ARROWHEAD:    Set this keyword to 0 to draw no arrowheads on the line. Set to 1 (the default)
;                   to draw a single arrowhead at (x2,y2). Set to 2 to draw a double-headed arrow. Set
;                   to 3 to draw a single arrowhead at (x1,y1).
;
;     HEADSIZE:     The size of the arrow head in pixel units. But default, !D.X_Size/50.
;
;     LAYER:        The annotation layer for the object.
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
;     X1:           The X location of one end of the arrow.
;
;     Y1:           The Y location of one end of the arrow.
;
;     X2:           The X location of the other end of the arrow. (Arrowhead here if ARROWHEAD=1.)
;
;     Y2:           The Y location of the other end of the arrow. (Arrowhead here if ARROWHEAD=1.)
;-
;*****************************************************************************************************
PRO Arrow::SetProperty, $
   ARROWHEAD=arrowhead, $
   DRAW=draw, $
   HEADSIZE=headsize, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   NOMESSAGE=nomessage, $
   NOREFRESH=norefresh, $
   ROTATION=rotation, $
   THICKNESS=thickness, $
   X1=x1, $
   Y1=y1, $
   X2=x2, $
   Y2=y2, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   sendMessage = 0

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> SELECTABLEOBJECT::SetProperty, NOREFRESH=norefresh, SENDMESSAGE=sendmessage, _EXTRA=extraKeywords

   ; The object could have been deleted.
   IF ~Obj_Valid(self) THEN RETURN

   IF N_Elements(arrowhead) NE 0 THEN BEGIN
      sendMessage = 1
      self.arrowhead = arrowhead
   ENDIF
   IF N_Elements(headsize) NE 0 THEN BEGIN
      sendMessage = 1
      self.headsize = headsize
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

   ; What is the orientation?
   ydiff = self.y2 - self.y1
   xdiff = self.x2 - self.x1
   self.orientation = ATan(ydiff, xdiff) * !RaDeg

   ; Find midpoint of arrow.
   xl = Min([self.x1, self.x2], Max=xr)
   self.midx = (xr - xl) / 2.0 + xl
   yl = Min([self.y1, self.y2], Max=yr)
   self.midy = (yr - yl) / 2.0 + yl

   ; Send message?
   IF sendMessage AND ~Keyword_Set(nomessage) THEN self -> SendMessage, 'ARROW_CHANGED'

   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ARROW::CLEANUP
;
; PURPOSE:
;
;       This is the ARROW object class destructor method.
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
PRO Arrow::CLEANUP

   @cat_pro_error_handler

   IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self

   self -> SELECTABLEOBJECT::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ARROW::INIT
;
; PURPOSE:
;
;       This is the ARROW object class initialization method
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
;     ARROWHEAD:    Set this keyword to 0 to draw no arrowheads on the line. Set to 1 (the default)
;                   to draw a single arrowhead at (x2,y2). Set to 2 to draw a double-headed arrow. Set
;                   to 3 to draw a single arrowhead at (x1,y1).
;
;     HEADSIZE:     The size of the arrow head in pixel units. But default, !D.X_Size/50. If set
;                   to -1, the headsize is calculated at DRAW time. This is preferred for resizeable
;                   graphics windows and PostScript output.
;
;     LAYER:        A CATLAYER object for holding annotations.
;
;     LINESTYLE:    The linestyle of the arrow. By default, 1.0 (solid).
;
;     THICKNESS:    Set this to the thickness of the output. By default, 1.0.
;
;     X1:           The X location of one end of the arrow.
;
;     Y1:           The Y location of one end of the arrow.
;
;     X2:           The X location of the other end of the arrow. (Arrowhead here if ARROWHEAD=1.)
;
;     Y2:           The Y location of the other end of the arrow. (Arrowhead here if ARROWHEAD=1.)
;
;     _EXTRA:       Any keywords appropriate for the SELECTABLEOBJECT INIT method.
;-
;*****************************************************************************************************
FUNCTION Arrow::INIT, $
   ARROWHEAD=arrowhead, $
   HEADSIZE=headsize, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   THICKNESS=thickness, $
   X1=x1, $
   Y1=y1, $
   X2=x2, $
   Y2=y2, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   ok = self -> SELECTABLEOBJECT::INIT (DESCRIPTION='Arrow Properties', _EXTRA=extraKeywords)
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
   IF N_Elements(arrowhead) EQ 0 THEN arrowhead = 1
   IF N_Elements(headsize) EQ 0 THEN headsize = -1
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

   ; Load object.
   self.arrowhead = arrowhead
   self.headsize = headsize
   self.linestyle = 0 > linestyle < 5
   self.orientation = ATan((y2-y1),(x2-x1))*!RaDeg
   self.thickness = thickness
   self.x1 = x1
   self.y1 = y1
   self.x2 = x2
   self.y2 = y2

   ; Find midpoint of arrow.
   xl = Min([self.x1, self.x2], Max=xr)
   self.midx = (xr - xl) / 2.0 + xl
   yl = Min([self.y1, self.y2], Max=yr)
   self.midy = (yr - yl) / 2.0 + yl

   ; Register properties for the property sheet. Turn visibility off, since some properties
   ; cause the object to refresh and draw prematurely.
   currentVisible = self.visible
   self.visible = 0
   self -> RegisterProperty, 'ARROWHEAD', 9, NAME="Arrow Heads", $
      ENUMLIST=['None', 'Single Right', 'Double', 'Single Left']
   self -> SetPropertyByIdentifier, 'ARROWHEAD', self.arrowhead
   self -> RegisterProperty, 'BG_COLOR', 0, USERDEF=CapFirstLetter(self.bg_color), NAME="Color (Background)"
   self -> RegisterProperty, 'BACKGROUND', 1, NAME="Background On"
   self -> SetPropertyByIdentifier, 'BACKGROUND', self.background
   self -> RegisterProperty, 'HEADSIZE', 3, NAME="Arrowhead Size"
   self -> RegisterProperty, 'LINESTYLE', 2, NAME="Linestyle", VALID_RANGE=[0, 5]
   self -> RegisterProperty, 'ROTATION', 3, NAME="Rotate", VALID_RANGE=[-180, 180]
   self -> RegisterProperty, 'THICKNESS', 2, NAME="Thickness", VALID_RANGE=[1, 10]
   self -> RegisterProperty, 'X1', 3, NAME="X1 Location"
   self -> RegisterProperty, 'Y1', 3, NAME="Y1 Location"
   self -> RegisterProperty, 'X2', 3, NAME="X2 Location"
   self -> RegisterProperty, 'Y2', 3, NAME="Y2 Location"
   self.visible = currentVisible

   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       ARROW CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the ARROW object.
;
;*****************************************************************************************************
PRO Arrow__Define, class

   class = { ARROW, $
             arrowhead: 0L, $            ; A flag to indicate which arrow heads should be drawn.
             headsize: 0L, $             ; The arrow head size in pixels. By default !D.X_Size / 50.
             layerObject: Obj_New(), $   ; A CATLAYER object for holding the annotation.
             linestyle: 0L, $            ; The linestyle the arrow is drawn in.
             midx: 0.0, $                ; The midpoint of the arrow in X.
             midy: 0.0, $                ; The midpoint of the arrow in Y.
             moveend: 0L, $              ; Indicates which end of arrow (1 or 2) you are moving.
             orientation: 0.0, $         ; The orientation of the arrow
             thickness: 0.0, $           ; The thickness of the arrow.
             x1: 0.0, $                  ; The X location for one end of the arrow.
             y1: 0.0, $                  ; The Y location for one end of the arrow.
             x2: 0.0, $                  ; The X location for the other end of the arrow.
             y2: 0.0, $                  ; The Y location for the other end of the arrow.
             sx: 0L, $                   ; The static end of a moving arrow.
             sy: 0L, $                   ; The static end of a moving arrow.
             INHERITS SelectableObject $
           }

END
