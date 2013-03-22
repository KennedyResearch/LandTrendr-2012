;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide a circle or ellipse that can be displayed
;       in a direct graphics draw widget. The coordinate system of the Ellipse
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
;       ellipseObject = Obj_New("ELLIPSE", X1=0.5, Y1=0.5, X2=0.75, Y2=0.75)
;       drawObject -> Add, ellipseObject
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
;   class = { ELLIPSE, $
;             clip_to_data: 0B, $         ; Flag that if set will allow clipping to data range in DRAW method.
;             insertedObject: Obj_New(), $; The new object created in the CreateNewObject method. (Ignored in CLEANUP.)
;             linestyle: 0L, $            ; The line style of the ellipse.
;             layerObject: Obj_New(), $   ; A CATLAYER object for holding the annotation.
;             modemap: Obj_New(), $       ; A pixmap for calculating "mode".
;             moveend: 0L, $              ; Indicates which end of ellipse you are moving.
;             npoints: 0L, $              ; The number of points used to draw the ellipse. 120 by default.
;             rotation: 0.0, $            ; The rotation of the x axis of the ellipse.
;             sx: 0L, $                   ; The static X location.
;             sy: 0L, $                   ; The static Y location.
;             thickness: 0.0, $           ; The thickness of the ellipse.
;             xcenter: 0.0, $             ; The midpoint of the ellipse in X.
;             ycenter: 0.0, $             ; The midpoint of the ellipse in Y.
;             xradius: 0.0, $             ; The radius of the X axis.
;             yradius: 0.0, $             ; The radius of the Y axis.
;             INHERITS SelectableObject $
;           }
;
; MESSAGES:
;
;   ELLIPSE_CHANGED:   This message is sent whenever SetProperty method is called and the NOMESSAGE
;                      keyword is NOT set.
;
; EVENT_STRUCTURE:
;
;       This object will add the following fields to the event structure created by an interaction.
;       The fields are defined as:
;
;       BOUNDARY_BOX:     A 2x5 element array in normalized coordinates giving the boundary box of
;                         the object.
;
;       XPTS:             An array of X points in data coordinates descriping an ellipse.
;
;       YPTS:             An array of Y points in data coordinates descriping an ellipse.
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
;       ELLIPSE::ADDTOEVENTSTRUCTURE
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
FUNCTION Ellipse::AddToEventStructure, event

   @cat_func_error_handler

   xradius = self.xradius
   xcenter = self.xcenter
   yradius = self.yradius
   ycenter = self.ycenter

   ; Calculate the points for an ellipse.
   phi = 2*!PI*(Findgen(self.npoints)/(self.npoints-1))
   ang = self.rotation/!RADEG
   cosang = Cos(ang)
   sinang = Sin(ang)

   x =  xradius * Cos(phi)
   y =  yradius * Sin(phi)
   xprime = xcenter + x*cosang - y*sinang
   yprime = ycenter + x*sinang + y*cosang

   ; Add appropriate fields to the event structure.
   event = Create_Struct( event, $
      'BOUNDARY_BOX', self.box, $
      'XPTS', xprime, $
      'YPTS', yprime)

   RETURN, event

END




;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::CALCULATEBOUNDARYBOX
;
; PURPOSE:
;
;       This method calculates the extent of a boundary ellipse about the ellipse itself.
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
PRO Ellipse::CalculateBoundaryBox

   @cat_pro_error_handler

   x1 = self.xcenter - self.xradius
   x2 = self.xcenter + self.xradius
   y1 = self.ycenter - self.yradius
   y2 = self.ycenter + self.yradius

   ; The actual boundary box is 10 pixels larger than the box itself. This
   ; serves as an aid in selecting the box handles for movement.
   self -> ApplyCoords
   c = Convert_Coord([x1,x2], [y1,y2], /Data, /To_Normal)
   b = Convert_Coord(5, 5, /Device, /To_Normal)
   self.box[0,*] = [c[0,0]-b[0,0], c[0,0]-b[0,0], c[0,1]+b[0,0], c[0,1]+b[0,0], c[0,0]-b[0,0]]
   self.box[1,*] = [c[1,0]-b[1,0], c[1,1]+b[1,0], c[1,1]+b[1,0], c[1,0]-b[1,0], c[1,0]-b[1,0]]

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the ELLIPSE object.
;
; SYNTAX:
;
;       ellipseObject -> ControlPanel, baseObject
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
PRO Ellipse::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Create a new control panel

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Ellipse Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, Name='ELLIPSE PROPERTYSHEET', YSize=12)
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::CREATENEWOBJECT
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
PRO Ellipse::CreateNewObject, drawID, pixmapID, NEWOBJECT=newObject

   @cat_pro_error_handler

   self -> GetProperty, $
      BACKGROUND=background, $
      BG_COLOR=bg_color, $
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      LINESTYLE=linestyle, $
      ROTATION=rotation, $
      THICKNESS=thickness, $
      XCENTER=xcenter, $
      YCENTER=ycenter, $
      XRADIUS=xradius, $
      YRADIUS=yradius

   newObject = Obj_New('ELLIPSE',  $
      BACKGROUND=background, $
      BG_COLOR=bg_color, $
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      LINESTYLE=linestyle, $
      ROTATION=rotation, $
      THICKNESS=thickness, $
      XCENTER=xcenter, $
      YCENTER=ycenter, $
      XRADIUS=xradius, $
      YRADIUS=yradius)


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
      drawID -> Add, newObject
      IF Obj_Valid(pixmapID) THEN pixmapID -> Add, newObject
      self.insertedObject = newObject

      ; Draw the new arrow.
      newObject -> Draw

   ENDELSE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::DRAW
;
; PURPOSE:
;
;       This method draws the ellipse in the current direct graphics display window.
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
PRO Ellipse::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; If the ellipse is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   ; Calculate the boundary box.
   self -> CalculateBoundaryBox

   self -> ApplyCoords

   ; Calculate the points for an ellipse.
   phi = 2*!PI*(Findgen(self.npoints)/(self.npoints-1))
   ang = self.rotation/!RADEG
   cosang = Cos(ang)
   sinang = Sin(ang)

   x =  self.xradius * Cos(phi)
   y =  self.yradius * Sin(phi)
   xprime = self.xcenter + x*cosang - y*sinang
   yprime = self.ycenter + x*sinang + y*cosang

   ; Draw the background if required.
   IF self.background THEN $
      PolyFill, xprime, yprime, Fill=1, Color=FSC_Color(self.bg_color), NOCLIP=1-self.clip_to_data

   ; Draw the ellipse.
   PlotS, xprime, yprime, Color=FSC_Color(self.color), $
          Thick=self.thickness, Linestyle=self.linestyle, NOCLIP=1-self.clip_to_data

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::DRAWSELECTIONBOX
;
; PURPOSE:
;
;       This method draws a selection box around the boundary ellipse of the ellipse
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
;       COLOR:    The name of a color to draw the ellipse in. By default, the color of the ellipse.
;
;-
;*****************************************************************************************************
PRO Ellipse::DrawSelectionBox, Color=color

   @cat_pro_error_handler

   ; If the ellipse is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   IF N_Elements(color) EQ 0 THEN color = self.color

   ; Update the boundary box.
   self -> CalculateBoundaryBox

  ; Apply the coordinate system.
   self -> ApplyCoords
   PLOTS, self.xcenter - self.xradius, self.ycenter, PSYM=6, Color=FSC_Color(color), Symsize=1.25, NOCLIP=1-self.clip_to_data
   PLOTS, self.xcenter + self.xradius, self.ycenter, PSYM=6, Color=FSC_Color(color), Symsize=1.25, NOCLIP=1-self.clip_to_data
   PLOTS, self.xcenter, self.ycenter - self.yradius, PSYM=6, Color=FSC_Color(color), Symsize=1.25, NOCLIP=1-self.clip_to_data
   PLOTS, self.xcenter, self.ycenter + self.yradius, PSYM=6, Color=FSC_Color(color), Symsize=1.25, NOCLIP=1-self.clip_to_data
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::EVENTHANDLER
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
PRO Ellipse::EventHandler, event

   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF


      'ELLIPSE PROPERTYSHEET': BEGIN

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
                  CatRefreshDraw, self, Stop_At='DRAWWIDGET', /NoErase, REQUESTER=self

                  END

               ELSE: BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value

                  ; Refresh the graphics hierarchy.
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
;       ELLIPSE::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain ELLIPSE properties. Be sure
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
;     CLIP_TO_DATA:   If set, indicates that the ellipse is clipped to the data coordinate range.
;
;     HEIGHT:         The height of the ellipse boundary ellipse in normalized coordinates.
;
;     INSERTEDOBJECT: The new object that is inserted in the CreateNewObject method.
;
;     LAYER:          The annotation layer associated with this object.
;
;     LINESTYLE:      The type of linestyle required. See PLOT documentation for details.
;
;     ROTATION:       The rotation in degrees counterclockwise of the X axis.
;
;     THICKNESS:      The current thickness of the ellipse.
;
;     WIDTH:          The width of the ellipse boundary ellipse in normalized coordinates.
;
;     XCENTER:        The X coordinate of the center of the ellipse.
;
;     XRADIUS:        The axis radius in the X direction.
;
;     YCENTER:        The Y coordinate of the center of the ellipse.
;
;     YRADIUS:        The axis radius in the Y direction.
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO Ellipse::GetProperty, $
   CLIP_TO_DATA=clip_to_data, $
   HEIGHT=height, $
   INSERTEDOBJECT=insertedObject, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   ROTATION=rotation, $
   THICKNESS=thickness, $
   WIDTH=width, $
   XCENTER=xcenter, $
   YCENTER=ycenter, $
   XRADIUS=xradius, $
   YRADIUS=yradius, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(height) THEN BEGIN
      maxy = Max(self.box[1,*], Min=miny)
      height = maxy - miny
   ENDIF
   IF Arg_Present(clip_to_data) THEN clip_to_data = self.clip_to_data
   IF Arg_Present(insertedObject) THEN insertedObject = self.insertedObject
   IF Arg_Present(layer) THEN layer = self.layerObject
   IF Arg_Present(linestyle) THEN linestyle = self.linestyle
   IF Arg_Present(rotation) THEN rotation = self.rotation
   IF Arg_Present(thickness) THEN thickness = self.thickness
   IF Arg_Present(width) THEN BEGIN
      maxx = Max(self.box[0,*], Min=minx)
      width = maxx - minx
   ENDIF
   IF Arg_Present(xcenter) THEN xcenter = self.xcenter
   IF Arg_Present(xradius) THEN xradius = self.xradius
   IF Arg_Present(ycenter) THEN ycenter = self.ycenter
   IF Arg_Present(yradius) THEN yradius = self.yradius

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::INTERACTIONEVENTS
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
PRO Ellipse::InteractionEvents, event, Interaction=interaction

   @cat_pro_error_handler

   ; Get information from the INTERACTION object.
   IF Obj_Valid(interaction) EQ 0 THEN Message, 'An "interaction" object is a required keyword parameter.'
   interaction -> GetProperty, MODE=mode, DRAWWIDGET=drawID, PIXMAP=pixmap

   ; What kind of event is this?
   eventTypes = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSED', 'CHARACTER', 'KEYPRESS']
   thisEvent = eventTypes[event.type]

   CASE mode OF

      'SELECT': BEGIN

                       ; Did you click the LEFT mouse button? Then we are moving the object.
                       IF event.press EQ 1 THEN BEGIN

                         ; If you are close to a selection circle, then you are moving
                         ; an edge of the ellipse and not the ellipse itself.
                         ellipsemode = self -> SelectMode(event.x, event.y, DRAWID=drawID)
                         IF ellipsemode NE 9 THEN BEGIN
                            self.moveend = ellipsemode
                            interaction -> SetProperty, Mode='DRAG_RADIUS'
                         ENDIF
                         self.sx = event.x
                         self.sy = event.y

                       END ; of PRESS event

         END ; of SELECT mode

      'INSERT': BEGIN

         CASE thisEvent OF

           'DOWN': BEGIN

                self -> ApplyCoords
                c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                self.sx = event.x
                self.sy = event.y
                self -> SetProperty, XCenter=c[0,0], YCenter=c[1,0], XRadius = 0.0, YRadius=0.0
                drawID -> SetProperty, MOTION_EVENTS=1
                self -> SetProperty, Visible=0
                drawID -> GetProperty, XSize=xsize, YSize=ysize
                pixmap -> GetProperty, XSize=pxsize, YSize=pysize
                IF (xsize NE pxsize) OR (ysize NE pysize) THEN $
                  pixmap -> SetProperty, XSize=xsize, YSize=ysize
                drawID -> SetWindow
                pixmap -> Copy

              END

           'UP': BEGIN

              drawID -> SetProperty, Motion_Events=0, /Clear_Events
              IF (self.sx EQ event.x) AND (self.sy EQ event.y) THEN RETURN

              interaction -> SetProperty, Mode='FINISHED_INSERT'
              drawID -> SetWindow
              self -> ApplyCoords
              c = Convert_Coord([self.sx, event.x], [self.sy, event.y], /Device, /To_Data)
              self.xradius = Abs(c[0,0]-c[0,1])
              self.yradius = Abs(c[1,0]-c[1,1])
              IF Obj_Valid(self._controlpanel) THEN $
                  self._controlpanel -> Refresh_Properties, Properties=['XRADIUS', 'YRADIUS']
              self -> SetProperty, Visible=1
              self -> CopyParameters, drawID, Destination=d, Extent=e
              pixmap -> Copy, Destination=d, Extent=e,  Origin=d
              self -> CreateNewObject, drawID, pixmap

              END

           'MOTION': BEGIN

              drawID -> SetWindow
              self -> CopyParameters, drawID, Destination=d, Extent=e
              pixmap -> Copy, Destination=d, Extent=e, Origin=d
              self -> SetProperty, Visible=1
              self -> ApplyCoords
              c = Convert_Coord([self.sx, event.x], [self.sy, event.y], /Device, /To_Data)
              self.xradius = Abs(c[0,0]-c[0,1])
              self.yradius = Abs(c[1,0]-c[1,1])
              IF Obj_Valid(self._controlpanel) THEN $
                  self._controlpanel -> Refresh_Properties, Properties=['XRADIUS', 'YRADIUS']
              self -> Draw
              END

           ELSE:

         ENDCASE ; of thisEvent in INSERT

         END ; of INSERT mode

      'DRAW': BEGIN

         CASE thisEvent OF

           'DOWN': BEGIN

                ; If a coordinate object wasn't provided, try to copy one from the
                ; parent. If this isn't available, create a normalized one.
                IF Obj_Valid(self._coords) EQ 0 THEN BEGIN
                   self -> GetProperty, First_Parent=parent
                  IF Obj_Valid(parent) THEN parent -> GetProperty, Coord_Object=coords
                    IF Obj_Valid(coords) THEN self._coords = coords ELSE $
                      self._coords = Obj_New('CatCoord', XRange=[0,1], YRange=[0,1], Position=[0,0,1,1])
                ENDIF
                self -> ApplyCoords
                c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                self.sx = event.x
                self.sy = event.y
                self -> SetProperty, XCenter=c[0,0], YCenter=c[1,0]
                drawID -> SetProperty, MOTION_EVENTS=1
                self -> SetProperty, Visible=0
                drawID -> GetProperty, XSize=xsize, YSize=ysize
                pixmap -> GetProperty, XSize=pxsize, YSize=pysize
                IF (xsize NE pxsize) OR (ysize NE pysize) THEN $
                  pixmap -> SetProperty, XSize=xsize, YSize=ysize
                drawID -> SetWindow
                pixmap -> Copy

              END

           'UP': BEGIN

              drawID -> SetProperty, Motion_Events=0, /Clear_Events
              IF (self.sx EQ event.x) AND (self.sy EQ event.y) THEN RETURN

              interaction -> SetProperty, Mode='FINISHED_DRAW'
              drawID -> SetWindow
              self -> ApplyCoords
              c = Convert_Coord([self.sx, event.x], [self.sy, event.y], /Device, /To_Data)
              self -> SetProperty, XRADIUS=Abs(c[0,0]-c[0,1]), YRADIUS=Abs(c[1,0]-c[1,1]), Visible=1
              IF Obj_Valid(self._controlpanel) THEN $
                  self._controlpanel -> Refresh_Properties, Properties=['XRADIUS', 'YRADIUS']
              self -> CopyParameters, drawID, Destination=d, Extent=e
              pixmap -> Copy, Destination=d, Extent=e,  Origin=d

              END

           'MOTION': BEGIN

              drawID -> SetWindow
              self -> CopyParameters, drawID, Destination=d, Extent=e
              pixmap -> Copy, Destination=d, Extent=e, Origin=d
              self -> ApplyCoords
              c = Convert_Coord([self.sx, event.x], [self.sy, event.y], /Device, /To_Data)
              self -> SetProperty, Visible=1, XRADIUS=Abs(c[0,0]-c[0,1]), YRADIUS=Abs(c[1,0]-c[1,1]), /NoMessage
              IF Obj_Valid(self._controlpanel) THEN $
                  self._controlpanel -> Refresh_Properties, Properties=['XRADIUS', 'YRADIUS']
               self -> Draw
              END

           ELSE:

         ENDCASE ; of thisEvent in DRAW

         END ; of DRAW mode


      'DRAG_RADIUS': BEGIN

         CASE thisEvent OF


           'UP': BEGIN
                 drawID -> SetProperty, Motion_Events=0, /Clear_Events
                 drawID -> SetWindow
                 interaction -> SetProperty, Mode='FINISHED_DRAG_RADIUS'
                 self -> Draw
                 self -> DrawSelectionBox
                 pixmap -> Refresh

              END



           'MOTION': BEGIN

                 drawID -> SetWindow
                 self -> CopyParameters, drawID, Destination=d, Extent=e
                 pixmap -> Copy, Destination=d, Extent=e, Origin=d
                 xlen = self.sx - event.x
                 ylen = self.sy - event.y
                 self -> ApplyCoords
                 c = Convert_Coord([self.sx, event.x], [self.sy, event.y], /Device, /To_Data)
                 xlen = c[0,0]-c[0,1]
                 ylen = c[1,0]-c[1,1]

                 CASE self.moveend OF

                     1: BEGIN ; Left.
                        self.xradius = self.xradius + xlen
                        self -> SetProperty, Visible=1, /NoMessage, /Draw
                        END

                     2: BEGIN ; Right.
                        self.xradius = self.xradius - xlen
                        self -> SetProperty, Visible=1, /NoMessage, /Draw
                        END

                     3: BEGIN ; Bottom.
                        self.yradius = self.yradius + ylen
                        self -> SetProperty, Visible=1, /NoMessage, /Draw
                        END

                     4: BEGIN ; Top.
                        self.yradius = self.yradius - ylen
                        self -> SetProperty, Visible=1, /NoMessage, /Draw
                        END

                     ELSE:

                 ENDCASE

                 IF Obj_Valid(self._controlpanel) THEN $
                     self._controlpanel -> Refresh_Properties, Properties=['XRADIUS', 'YRADIUS']
                 ; Update the static location.
                 self.sx = event.x
                 self.sy = event.y

                 ; Draw the selection ellipse.
                 self -> DrawSelectionBox

              END ; Of MOTION events.

           ELSE:

         ENDCASE ; of thisEvent in DRAG_RADIUS

         END ; of MOVE_EDGE


     ELSE:

  ENDCASE

   self -> Report, /Completed


END



;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::MOVE
;
; PURPOSE:
;
;       This method moves the ellipse in a graphics window.
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
PRO Ellipse::Move, x, y, PIXMAP=pixmap, NODRAW=noDraw

   @cat_pro_error_handler

   ; Convert the device pixels into normal coordinates.

   self -> ApplyCoords
   self._coords -> GetProperty, XRANGE=xr, YRANGE=yr, POSITION=pos
   xx = Abs(xr[1] - xr[0]) / (!D.X_Size * Abs(pos[2]-pos[0])) * x
   yy = Abs(yr[1] - yr[0]) / (!D.Y_Size * Abs(pos[3]-pos[1])) * y
   self.xcenter = self.xcenter + xx
   self.ycenter = self.ycenter + yy


   IF Obj_Valid(self._controlpanel) THEN $
       self._controlpanel -> Refresh_Properties, Properties=['XCENTER', 'YCENTER']

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
;       ELLIPSE::SELECT
;
; PURPOSE:
;
;       This method returns the object reference if the requested point is inside
;       the bounding box of the ellipse.
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
FUNCTION Ellipse::Select, x, y, SUCCESS=success

   @cat_func_error_handler

   ; Set up return values.
   retval = Obj_New()
   success = 0

   ; No selection possible, if invisible.
   IF self.visible EQ 0 THEN RETURN, retval

   ; If you belong to a group, you cannot be selected individually.
   IF Obj_Valid(self.mygroup) THEN RETURN, retval

   ; No selection is possible, if the object is currently unselectable.
   IF ~self.selectable THEN RETURN, retval

   ; Convert the point from device to normalized coordinates.
   ; Convert the device pixels into data coordinates.
   self -> ApplyCoords
   c = Convert_Coord(x, y, /Device, /To_Data)
   xx = c[0]
   yy = c[1]

   ; You wish the radii to be just five pixels longer than they really
   ; are as an aid to selecting the corners of the ellipse.
   self._coords -> GetProperty, XRANGE=xr, YRANGE=yr
   nx = (xr[1] - xr[0]) / Float(!D.X_Size) * 5
   ny = (yr[1] - yr[0]) / Float(!D.Y_Size) * 5
   xradius = self.xradius + nx
   yradius = self.yradius + ny

   ; Update the box coordinates.
   self -> CalculateBoundaryBox

   ; Calculate the points for an ellipse.
   phi = 2*!PI*(Findgen(self.npoints)/(self.npoints-1))
   ang = self.rotation/!RADEG
   cosang = Cos(ang)
   sinang = Sin(ang)

   xpts =  xradius * Cos(phi)
   ypts =  yradius * Sin(phi)
   xprime = self.xcenter + xpts*cosang - ypts*sinang
   yprime = self.ycenter + xpts*sinang + ypts*cosang

   ; Are you inside?
   isInside = Inside(xx, yy, xprime, yprime)
   IF isInside THEN BEGIN

      retVal = self
      success = 1

   ENDIF

   self -> Report, /Completed
   RETURN, retval

END



;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::SELECTMODE
;
; PURPOSE:
;
;       This method selects the movement mode based on which which handle is closest
;       to the selection point. The selection point is a location in a draw widget in
;       device coordinates.
;
; SYNTAX:
;
;       theObject -> SelectMode, x, y
;
; ARGUMENTS:
;
;       X:          The X location of the selection.
;
;       Y:          The Y location of the selection.
;
; KEYWORDS:
;
;       DrawID:     A window object that contains the ellipse. Required for converting to
;                   the proper device coodinate system.
;-
;*****************************************************************************************************
FUNCTION Ellipse::SelectMode, x, y, DRAWID=drawID

   @cat_func_error_handler

   ; Update the mode map
   self -> Update_Modemap
   self.modemap -> GetProperty, XSize=xsize, YSize=ysize

   ; Normalize the location with respect to the ellipse.
   IF N_Elements(drawID) NE 0 THEN drawID -> SetWindow
   self -> ApplyCoords
   b = Convert_Coord([self.box[0,0], self.box[0,2]], [self.box[1,0], self.box[1,1]], /Normal, /To_Device)
   xx = 0 > (x - Round(b[0,0])) < (xsize-1)
   yy = 0 > (y - Round(b[1,0])) < (ysize-1)

   ; Take a snapshot of the modemap.
   self.modemap -> SetWindow
   map = TVRD()

   theMode = map[xx,yy]

   self -> Report, /Completed
   RETURN, theMode
END



;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the ELLIPSE object's properties. Be sure
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
;     CLIP_TO_DATA: The ellipse is draw with PLOTS, which normally does NOT clip to
;                   the data range. Set this keyword to put clipping into effect for
;                   drawing the ellipse.
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
;     ROTATION:     The rotation in degrees counterclockwise of the X axis.
;
;     THICKNESS:    Set this to the thickness of the output. By default, 1.0.
;
;     XCENTER:      The X coordinate of the center of the ellipse.
;
;     XRADIUS:      The axis radius in the X direction.
;
;     YCENTER:      The Y coordinate of the center of the ellipse.
;
;     YRADIUS:      The axis radius in the Y direction.
;-
;*****************************************************************************************************
PRO Ellipse::SetProperty, $
   CLIP_TO_DATA=clip_to_data, $
   DRAW=draw, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   NOMESSAGE=nomessage, $
   NOREFRESH=norefresh, $
   ROTATION=rotation, $
   THICKNESS=thickness, $
   XCENTER=xcenter, $
   YCENTER=ycenter, $
   XRADIUS=xradius, $
   YRADIUS=yradius, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   sendMessage = 0

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> SELECTABLEOBJECT::SetProperty, NOREFRESH=norefresh, SENDMESSAGE=sendmessage, _EXTRA=extraKeywords

   ; The object could have been deleted.
   IF ~Obj_Valid(self) THEN RETURN

   IF N_Elements(clip_to_data) NE 0 THEN BEGIN
      self.clip_to_data = Keyword_Set(clip_to_data)
   ENDIF
   IF N_Elements(layer) NE 0 THEN BEGIN
      IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self
      self.layerObject = layer
   ENDIF
   IF N_Elements(linestyle) NE 0 THEN BEGIN
      sendMessage = 1
      self.linestyle = 0 > linestyle < 5
   ENDIF
   IF N_Elements(rotation) NE 0 THEN BEGIN
      sendMessage = 1
      self.rotation = rotation
   ENDIF
   IF N_Elements(thickness) NE 0 THEN BEGIN
      sendMessage = 1
      self.thickness = thickness
   ENDIF
   IF N_Elements(xcenter) NE 0 THEN BEGIN
      sendMessage = 1
      self.xcenter = xcenter
   ENDIF
   IF N_Elements(ycenter) NE 0 THEN BEGIN
      sendMessage = 1
      self.ycenter = ycenter
   ENDIF
   IF N_Elements(yradius) NE 0 THEN BEGIN
      sendMessage = 1
      self.yradius = yradius
   ENDIF
   IF N_Elements(xradius) NE 0 THEN BEGIN
      sendMessage = 1
      self.xradius = xradius
   ENDIF

   ; Send message?
   IF sendMessage AND ~Keyword_Set(nomessage) THEN self -> SendMessage, 'ELLIPSE_CHANGED'

   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed

END




;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::UPDATE_MODEMAP
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
PRO Ellipse::Update_Modemap, CLEAR=clear

   @cat_pro_error_handler

   IF Keyword_Set(clear) THEN BEGIN
      self.modemap -> Refresh
      RETURN
   ENDIF

   ; Convert the boundary box from normal to device coordinates.
   b = Convert_Coord(self.box, /Normal, /To_Device)
   b1 = b[0,0]
   b2 = b[0,2]
   d1 = b[1,0]
   d2 = b[1,1]

   ; Resize the modemap and refresh it.

   self.modeMap -> SetProperty, XSize=b2-b1+1, YSize=d2-d1+1
   self.modemap -> Refresh

   self.modemap -> SetWindow
   Device, Decomposed=0, Get_Decomposed=theState
   TVLCT, r, g, b, /Get
   LoadCT, 0, /Silent
   phi = 2*!PI*(Findgen(self.npoints)/(self.npoints-1))
   ang = self.rotation/!RADEG
   cosang = Cos(ang)
   sinang = Sin(ang)

   x =  ((b2-b1+1)/2-5) * Cos(phi)
   y =  ((d2-d1+1)/2-5) * Sin(phi)

   xprime = (b2-b1+1)/2 + x*cosang - y*sinang
   yprime = (d2-d1+1)/2 + x*sinang + y*cosang

   PolyFill, xprime, yprime, Fill=1, Color=9, /Device

      ; Draw the handles on the box.
   phi = Findgen(32) * (!PI * 2 / 32.)
   phi = [ phi, phi[0] ]
   UserSym, Cos(phi), Sin(phi), /Fill

   PLOTS, (b2-b1+1)/2 - ((b2-b1+1)/2-5), (d2-d1+1)/2, PSYM=8, Color=1, /DEVICE, SYMSIZE=2 ; Left
   PLOTS, (b2-b1+1)/2 + ((b2-b1+1)/2-5), (d2-d1+1)/2, PSYM=8, Color=2, /DEVICE, SYMSIZE=2 ; Right
   PLOTS, (b2-b1+1)/2, (d2-d1+1)/2 - ((d2-d1+1)/2-5), PSYM=8, Color=3, /DEVICE, SYMSIZE=2 ; Bottom
   PLOTS, (b2-b1+1)/2, (d2-d1+1)/2 + ((d2-d1+1)/2-5), PSYM=8, Color=4, /DEVICE, SYMSIZE=2

   Device, Decomposed=theState
   TVLCT, r, g, b

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::CLEANUP
;
; PURPOSE:
;
;       This is the ELLIPSE object class destructor method.
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
PRO Ellipse::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self.modemap
   IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self
   self -> SELECTABLEOBJECT::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE::INIT
;
; PURPOSE:
;
;       This is the ELLIPSE object class initialization method
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
;     CLIP_TO_DATA: The ellipse is draw with PLOTS, which normally does NOT clip to
;                   the data range. Set this keyword to put clipping into effect for
;                   drawing the ellipse.
;
;     LAYER:        A CATLAYER object for holding annotations.
;
;     LINESTYLE:    The linestyle of the ellipse. By default, 1.0 (solid).
;
;     NPOINTS:      The number of points used to draw the ellipse. By default, 120.
;
;     ROTATION:     The rotation in degrees counterclockwise of the X axis.
;
;     THICKNESS:    Set this to the thickness of the output. By default, 1.0.
;
;     XCENTER:      The X coordinate of the center of the ellipse.
;
;     XRADIUS:      The axis radius in the X direction.
;
;     YCENTER:      The Y coordinate of the center of the ellipse.
;
;     YRADIUS:      The axis radius in the Y direction.
;
;     _EXTRA:       Any keywords appropriate for the SELECTABLEOBJECT INIT method.
;-
;*****************************************************************************************************
FUNCTION Ellipse::INIT, $
   CLIP_TO_DATA=clip_to_data, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   NPOINTS=npoints, $
   ROTATION=rotation, $
   THICKNESS=thickness, $
   XCENTER=xcenter, $
   YCENTER=ycenter, $
   XRADIUS=xradius, $
   YRADIUS=yradius, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   ok = self -> SELECTABLEOBJECT::INIT (DESCRIPTION='Ellipse Properties', _EXTRA=extraKeywords)
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
   IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
   IF N_Elements(npoints) EQ 0 THEN npoints = 120
   IF N_Elements(rotation) EQ 0 THEN rotation = 0.0
   IF N_Elements(thickness) EQ 0 THEN thickness = 1.0
   IF N_Elements(xcenter) EQ 0 THEN xcenter = 0.5
   IF N_Elements(ycenter) EQ 0 THEN ycenter = 0.5
   IF N_Elements(xradius) EQ 0 THEN xradius = 0.25
   IF N_Elements(yradius) EQ 0 THEN yradius = 0.20

   ; Load object.
   self.clip_to_data = Keyword_Set(clip_to_data)
   self.linestyle = 0 > linestyle < 5
   self.modemap = Obj_New('Pixmapwidget')
   self.npoints = npoints
   self.rotation = rotation
   self.thickness = thickness
   self.xcenter = xcenter
   self.ycenter = ycenter
   self.xradius = xradius
   self.yradius = yradius

   ; Register properties for the property sheet. Turn visibility off, since some properties
   ; cause the object to refresh and draw prematurely.
   currentVisible = self.visible
   self.visible = 0
   self -> RegisterProperty, 'BG_COLOR', 0, USERDEF=CapFirstLetter(self.bg_color), NAME="Color (Background)"
   self -> RegisterProperty, 'BACKGROUND', 1, NAME="Background On"
   self -> SetPropertyByIdentifier, 'BACKGROUND', self.background
   self -> RegisterProperty, 'LINESTYLE', 2, NAME="Linestyle", VALID_RANGE=[0, 5]
   self -> RegisterProperty, 'THICKNESS', 2, NAME="Thickness", VALID_RANGE=[1, 10]
   self -> RegisterProperty, 'XCENTER', 3, NAME="X Center"
   self -> RegisterProperty, 'YCENTER', 3, NAME="Y Center"
   self -> RegisterProperty, 'XRADIUS', 3, NAME="X Radius"
   self -> RegisterProperty, 'YRADIUS', 3, NAME="Y Radius"
   self.visible = currentVisible

   self -> Report, /Completed

   RETURN, 1

END

;*****************************************************************************************************
;
; NAME:
;       ELLIPSE CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the ELLIPSE object.
;
;*****************************************************************************************************
PRO Ellipse__Define

   class = { ELLIPSE, $
             clip_to_data: 0B, $         ; Flag that if set will allow clipping to data range in DRAW method.
             insertedObject: Obj_New(), $; The new object created in the CreateNewObject method. (Ignored in CLEANUP.)
             linestyle: 0L, $            ; The line style of the ellipse.
             layerObject: Obj_New(), $   ; A CATLAYER object for holding the annotation.
             modemap: Obj_New(), $       ; A pixmap for calculating "mode".
             moveend: 0L, $              ; Indicates which end of ellipse you are moving.
             npoints: 0L, $              ; The number of points used to draw the ellipse. 120 by default.
             rotation: 0.0, $            ; The rotation of the x axis of the ellipse.
             sx: 0L, $                   ; The static X location.
             sy: 0L, $                   ; The static Y location.
             thickness: 0.0, $           ; The thickness of the ellipse.
             xcenter: 0.0, $             ; The midpoint of the ellipse in X.
             ycenter: 0.0, $             ; The midpoint of the ellipse in Y.
             xradius: 0.0, $             ; The radius of the X axis.
             yradius: 0.0, $             ; The radius of the Y axis.
             INHERITS SelectableObject $
           }

END

