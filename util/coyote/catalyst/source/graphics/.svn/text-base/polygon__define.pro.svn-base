;*****************************************************************************************************
;+
; NAME:
;       POLYGON__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide a polygon that can be displayed
;       in a direct graphics draw widget. The coordinate system of the Polygon
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
;       polygonObject = Obj_New("POLYGON", XPTS, YPTS)
;       drawObject -> Add, polygonObject
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
;   class = { POLYGON, $
;             background: 0L, $           ; A flag to indicate if a background polygon is drawn.
;             bg_color: "", $             ; The background color.
;             color: "", $                ; The name of a color to draw polygon in.
;             linestyle: 0L, $            ; The line style of the polygon.
;             xpts: Ptr_New(), $          ; The X points making up the polygon.
;             ypts: Ptr_New(), $          ; The X points making up the polygon.
;             npoints: 0L, $              ; The number of points in the polygon
;             thickness: 0.0, $           ; The thickness of the polygon.
;             sx: 0L, $                   ; The static X location.
;             sy: 0L, $                   ; The static Y location.
;             INHERITS SelectableObject $
;           }
;
; MESSAGES:
;
;   POLYGON_CHANGED:   This message is sent whenever SetProperty method is called and the NOMESSAGE
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
;       XPTS:             An array of X points in data coordinates descriping a polygon.
;
;       YPTS:             An array of Y points in data coordinates descriping a polygon.
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
;       POLYGON::ADDTOEVENTSTRUCTURE
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
FUNCTION Polygon::AddToEventStructure, event

   @cat_func_error_handler

   ; Add appropriate fields to the event structure.
   self -> GetProperty, XPTS=xpts, YPTS=ypts
   event = Create_Struct( event, $
      'BOUNDARY_BOX', self.box, $
      'XPTS', xpts, $
      'YPTS', ypts)


   RETURN, event

END



;*****************************************************************************************************
;+
; NAME:
;       POLYGON::CALCULATEBOUNDARYBOX
;
; PURPOSE:
;
;       This method calculates the extent of a boundary polygon about the polygon itself.
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
PRO Polygon::CalculateBoundaryBox

   @cat_pro_error_handler

   ; If there are not at least three points, then return.
   IF self.npoints LT 3 THEN BEGIN
      self.box[0:1,0:4] = 0.0
      RETURN
   ENDIF

   ; Find the min and max points in each direction.
   x1 = Min(*self.xpts, Max=x2)
   y1 = Min(*self.ypts, Max=y2)
   d = Convert_Coord([x1,x2], [y1,y2], /Data, /To_Normal)

   ; The actual boundary box is 10 pixels larger than the box itself. This
   ; serves as an aid in selecting the box handles for movement.
   b = Convert_Coord(5, 5, /Device, /To_Normal)
   self.box[0,*] = [d[0,0]-b[0,0], d[0,0]-b[0,0], d[0,1]+b[0,0], d[0,1]+b[0,0], d[0,0]-b[0,0]]
   self.box[1,*] = [d[1,0]-b[1,0], d[1,1]+b[1,0], d[1,1]+b[1,0], d[1,0]-b[1,0], d[1,0]-b[1,0]]

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       POLYGON::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the POLYGON object.
;
; SYNTAX:
;
;       polygonObject -> ControlPanel, baseObject
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
PRO Polygon::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Create a new control panel

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Polygon Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, Name='POLYGON PROPERTYSHEET', YSize=8)
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       POLYGON::CREATENEWOBJECT
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
PRO Polygon::CreateNewObject, drawID, pixmapID, NEWOBJECT=newObject

   @cat_pro_error_handler

   self -> GetProperty, $
      BACKGROUND=background, $
      BG_COLOR=bg_color, $
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      LINESTYLE=linestyle, $
      THICKNESS=thickness, $
      XPTS=xpts, $
      YPTS=ypts

   newObject = Obj_New('POLYGON',  $
      BACKGROUND=background, $
      BG_COLOR=bg_color, $
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      LINESTYLE=linestyle, $
      THICKNESS=thickness, $
      XPTS=xpts, $
      YPTS=ypts)

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
;       POLYGON::DRAW
;
; PURPOSE:
;
;       This method draws the polygon in the current direct graphics display window.
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
PRO Polygon::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; If the polygon is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   ; Apply the coordinate system.
   self -> ApplyCoords

   ; Calculate the boundary box.
   self -> CalculateBoundaryBox

   ; Draw the background if required.
   IF self.background THEN $
      PolyFill, *self.xpts, *self.ypts, $
                 Fill=1, Color=FSC_Color(self.bg_color)

   ; Draw the polygon.
   PlotS, *self.xpts, *self.ypts, Color=FSC_Color(self.color), $
          Thick=self.thickness, Linestyle=self.linestyle

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       POLYGON::DRAWSELECTIONBOX
;
; PURPOSE:
;
;       This method draws a selection box around the boundary polygon of the polygon
;
; SYNTAX:
;
;       theObject -> DrawSelectionPolygon
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       COLOR:    The name of a color to draw the polygon in. By default, the color of the polygon.
;
;-
;*****************************************************************************************************
PRO Polygon::DrawSelectionBox, Color=color

   @cat_pro_error_handler

   ; If the polygon is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   IF N_Elements(color) EQ 0 THEN color = self.color

   ; Update the boundary box.
   self -> CalculateBoundaryBox

   ; Apply the coordinate system.
   self -> ApplyCoords

   ; Draw the selection box.
   PLOTS, *self.xpts, *self.ypts, PSYM=6, Color=FSC_Color(color), Symsize=1.25

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       POLYGON::EVENTHANDLER
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
PRO Polygon::EventHandler, event

   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF


      'POLYGON PROPERTYSHEET': BEGIN

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
;       POLYGON::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain POLYGON properties. Be sure
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
PRO Polygon::GetProperty, $
   HEIGHT=height, $
   INSERTEDOBJECT=insertedObject, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   NPOINTS=npoints, $
   ROTATION=rotation, $
   THICKNESS=thickness, $
   WIDTH=width, $
   XPTS=xpts, $
   YPTS=ypts, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(height) THEN BEGIN
      maxy = Max(self.box[1,*], Min=miny)
      height = maxy - miny
   ENDIF
   IF Arg_Present(insertedObject) THEN insertedObject = self.insertedObject
   IF Arg_Present(layer) THEN layer = self.layerObject
   IF Arg_Present(linestyle) THEN linestyle = self.linestyle
   IF Arg_Present(npoints) THEN npoints = self.npoints
   IF Arg_Present(send_to_back) THEN send_to_back = 0 ; Strictly for PropertySheet widget.
   IF Arg_Present(thickness) THEN thickness = self.thickness
   IF Arg_Present(width) THEN BEGIN
      maxx = Max(self.box[0,*], Min=minx)
      width = maxx - minx
   ENDIF
   IF Arg_Present(xpts) THEN xpts = *self.xpts
   IF Arg_Present(ypts) THEN ypts = *self.ypts

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       POLYGON::INITIALIZE
;
; PURPOSE:
;
;       This method initializes the polygon object for drawing.
;
; SYNTAX:
;
;       theObject -> Initialize
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
PRO Polygon::Initialize

   @cat_pro_error_handler

   ; Freshen up the vertices pointers.
   Ptr_Free, self.xpts
   Ptr_Free, self.ypts
   self.npoints = 0
   Obj_Destroy, self.pointsPixID

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       POLYGON::INTERACTIONEVENTS
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
PRO Polygon::InteractionEvents, event, Interaction=interaction

   @cat_pro_error_handler

   ; Get information from the INTERACTION object.
   IF Obj_Valid(interaction) EQ 0 THEN Message, 'An "interaction" object is a required keyword parameter.'
   interaction -> GetProperty, MODE=mode, DRAWWIDGET=drawID, PIXMAP=pixmap

   ; What kind of event is this?
   eventTypes = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSED', 'CHARACTER', 'KEYPRESS']
   thisEvent = eventTypes[event.type]

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

               ; Which button was pressed? Branch accordingly. RIGHT button closes ROI,
               ; anything else selects/deselects.
               theButton = whichButton[0 > event.press < 4]

               CASE theButton OF

                  'RIGHT':BEGIN

                     ; No events unless we have clicked another button first.
                     IF self.npoints LE 0 THEN RETURN

                     ; No double clicking.
                     IF event.clicks EQ 2 THEN RETURN

                     ; Need at least three points for a polygon.
                     IF self.npoints LT 2 THEN BEGIN
                       ok = Dialog_Message('Three points required to create polygon ROI.')
                       self.npoints = 0
                       Ptr_Free, self.xpts
                       Ptr_Free, self.ypts
                       RETURN
                     ENDIF

                     ; Add point to the ROI
                     c = Convert_Coord(event.x, event.y, /Device, /To_Data)

                     IF Ptr_Valid(self.xpts) THEN *self.xpts = [ *self.xpts, c[0,0]] ELSE $
                       self.xpts = Ptr_New([c[0,0]])
                     IF Ptr_Valid(self.ypts) THEN *self.ypts = [ *self.ypts, c[1,0]] ELSE $
                       self.ypts = Ptr_New([c[1,0]])
                     self.npoints = self.npoints + 1

                     ; Wrap the points around on each other.
                     *self.xpts = [*self.xpts, (*self.xpts)[0]]
                     *self.ypts = [*self.ypts, (*self.ypts)[0]]
                     self.npoints = self.npoints + 1

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
                        IF Ptr_Valid(self.xpts) THEN *self.xpts = [ *self.xpts, c[0,0]] ELSE $
                           self.xpts = Ptr_New([c[0,0]])
                        IF Ptr_Valid(self.ypts) THEN *self.ypts = [ *self.ypts, c[1,0]] ELSE $
                           self.ypts = Ptr_New([c[1,0]])
                        self.npoints = self.npoints + 1
                        IF self.npoints GE 2 THEN BEGIN
                           self.pointsPixID -> SetWindow
                           PlotS, (*self.xpts)[self.npoints-2:self.npoints-1], Color=FSC_Color(self.color), $
                                 (*self.ypts)[self.npoints-2:self.npoints-1], Thick=self.thickness, $
                                 Linestyle=self.linestyle
                        ENDIF

                      ENDIF ELSE BEGIN ; Double click to close loop.

                        ; Need at least three points for a polygon.
                        IF self.npoints LT 2 THEN BEGIN
                          ok = Dialog_Message('Three points required to create polygon.')
                          self.npoints = 0
                          Ptr_Free, self.xpts
                          Ptr_Free, self.ypts
                          RETURN
                        ENDIF

                        ; Close the loop.
                        *self.xpts = [(*self.xpts), (*self.xpts)[0]]
                        *self.ypts = [(*self.ypts), (*self.ypts)[0]]
                        self.npoints = self.npoints + 1
                        interaction -> SetProperty, Mode='FINISHED_INSERT'
                        drawID -> SetWindow
                        self -> CalculateBoundaryBox
                        self -> CopyParameters, drawID, Destination=d, Extent=e
                        pixmap -> Copy, Destination=d, Extent=e,  Origin=d
                        self -> CreateNewObject, drawID, pixmap

                        END

                     ENDELSE

               ENDCASE

               END

            'UP':

            'MOTION': BEGIN


               ; Copy and draw the last two points.
               IF self.npoints GE 1 THEN BEGIN
                  drawID -> SetWindow
                  self.pointsPixID -> Copy
                  c = Convert_Coord(event.x, event.y, /Device, /To_Data)

                  PlotS, [(*self.xpts)[self.npoints-1], c[0,0]], Color=FSC_Color(self.color), $
                         [(*self.ypts)[self.npoints-1], c[1,0]], Thick=self.thickness, $
                         Linestyle=self.linestyle
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
         thisEvent = possibleEvents[ 0 > event.type < 4]

         CASE thisEvent OF

            'DOWN':BEGIN ; Button DOWN event.

               ; Which button was pressed? Branch accordingly. RIGHT button closes ROI,
               ; anything else selects/deselects.
               theButton = whichButton[0 > event.press < 4]

               CASE theButton OF

                  'RIGHT':BEGIN

                     ; No events unless we have clicked another button first.
                     IF self.npoints LE 0 THEN RETURN

                     ; No double clicking.
                     IF event.clicks EQ 2 THEN RETURN

                     ; Need at least three points for a polygon.
                     IF self.npoints LT 2 THEN BEGIN
                       ok = Dialog_Message('Three points required to create polygon ROI.')
                       self.npoints = 0
                       Ptr_Free, self.xpts
                       Ptr_Free, self.ypts
                       RETURN
                     ENDIF

                     ; Add point to the ROI
                     c = Convert_Coord(event.x, event.y, /Device, /To_Data)

                     IF Ptr_Valid(self.xpts) THEN *self.xpts = [ *self.xpts, c[0,0]] ELSE $
                       self.xpts = Ptr_New([c[0,0]])
                     IF Ptr_Valid(self.ypts) THEN *self.ypts = [ *self.ypts, c[1,0]] ELSE $
                       self.ypts = Ptr_New([c[1,0]])
                     self.npoints = self.npoints + 1

                     ; Wrap the points around on each other.
                     *self.xpts = [*self.xpts, (*self.xpts)[0]]
                     *self.ypts = [*self.ypts, (*self.ypts)[0]]
                     self.npoints = self.npoints + 1

                     ; Draw the completed ROI
                     self -> Draw
                     Wait, 0.1 ; So they can see it closed.

                     ; Motion events off.
                     drawID -> SetProperty, Motion_Events=0, /Clear_Events
                     interaction -> SetProperty, Mode='FINISHED_DRAW'

                     ; Erase yourself.
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
                        c = Convert_Coord(event.x, event.y, /Device, /To_Data)

                        IF Ptr_Valid(self.xpts) THEN *self.xpts = [ *self.xpts, c[0,0]] ELSE $
                           self.xpts = Ptr_New([c[0,0]])
                        IF Ptr_Valid(self.ypts) THEN *self.ypts = [ *self.ypts, c[1,0]] ELSE $
                           self.ypts = Ptr_New([c[1,0]])
                        self.npoints = self.npoints + 1
                        IF self.npoints GE 2 THEN BEGIN
                           self.pointsPixID -> SetWindow
                           PlotS, (*self.xpts)[self.npoints-2:self.npoints-1], Color=FSC_Color(self.color), $
                                 (*self.ypts)[self.npoints-2:self.npoints-1], Thick=self.thickness, $
                                 Linestyle=self.linestyle
                        ENDIF

                      ENDIF ELSE BEGIN ; Double click to close loop.

                        ; Need at least three points for a polygon.
                        IF self.npoints LT 2 THEN BEGIN
                          ok = Dialog_Message('Three points required to create polygon.')
                          self.npoints = 0
                          Ptr_Free, self.xpts
                          Ptr_Free, self.ypts
                          RETURN
                        ENDIF

                        ; Close the loop.
                        *self.xpts = [(*self.xpts), (*self.xpts)[0]]
                        *self.ypts = [(*self.ypts), (*self.ypts)[0]]
                        self.npoints = self.npoints + 1
                        interaction -> SetProperty, Mode='FINISHED_DRAW'
                        drawID -> SetWindow
                        self -> CalculateBoundaryBox

                        self -> Draw
                        Wait, 0.1 ; So they can see it closed.

                       ; Erase yourself.
                        self -> CopyParameters, drawID, Destination=d, Extent=e
                        pixmap -> Copy, Destination=d, Extent=e,  Origin=d

                        END

                     ENDELSE

               ENDCASE

               END

            'UP':

            'MOTION': BEGIN


               ; Copy and draw the last two points.
               IF self.npoints GE 1 THEN BEGIN
                  drawID -> SetWindow
                  self.pointsPixID -> Copy
                  c = Convert_Coord(event.x, event.y, /Device, /To_Data)

                  PlotS, [(*self.xpts)[self.npoints-1], c[0,0]], Color=FSC_Color(self.color), $
                         [(*self.ypts)[self.npoints-1], c[1,0]], Thick=self.thickness, $
                         Linestyle=self.linestyle
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
                 c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                 self -> GetProperty, XPTS=xpts, YPTS=ypts
                 IF self.moveVertex LT N_Elements(xpts) THEN BEGIN
                    CASE self.moveVertex OF

                        0: BEGIN
                           last = N_Elements(xpts)-1
                           xpts[self.moveVertex] = c[0,0]
                           ypts[self.moveVertex] = c[1,0]
                           xpts[last] = c[0,0]
                           ypts[last] = c[1,0]
                           END

                        ELSE: BEGIN
                           xpts[self.moveVertex] = c[0,0]
                           ypts[self.moveVertex] = c[1,0]
                           END
                    ENDCASE
                    self -> SetProperty, XPTS=xpts, YPTS=ypts
                 ENDIF

                 self -> Draw

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
;       POLYGON::MOVE
;
; PURPOSE:
;
;       This method moves the polygon in a graphics window.
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
PRO Polygon::Move, x, y, PIXMAP=pixmap, NODRAW=noDraw

   @cat_pro_error_handler

   ; Convert the device pixels into data coordinates.
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
;       POLYGON::SELECT
;
; PURPOSE:
;
;       This method returns the object reference if the requested point is inside
;       the bounding box of the polygon.
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
FUNCTION Polygon::Select, x, y, SUCCESS=success

   @cat_func_error_handler

   ; Set up return values.
   retval = Obj_New()
   success = 0

   ; No selection possible, if invisible.
   IF self.visible EQ 0 THEN RETURN, retval

   ; Convert the point from device to normal coordinates.
   self -> ApplyCoords
   c = Convert_Coord(x, y, /Device, /To_Normal)
   xx = c[0,0]
   yy = c[1,0]

   ; If you belong to a group, you cannot be selected individually.
   IF Obj_Valid(self.mygroup) THEN RETURN, Obj_New()

   ; Update the box coordinates.
   self -> CalculateBoundaryBox

   ; Are you inside?
   isInside = Inside(xx, yy, Reform(self.box[0,0:3]), Reform(self.box[1,0:3]))
   IF isInside THEN BEGIN

      ; Are you within 10 pixels of any of the vertices?
      d = Convert_Coord(*self.xpts, *self.ypts, /Data, /To_Device)
      xpts = Reform(d[0,*])
      ypts = Reform(d[1,*])
      len = SQRT((xpts-x)^2 + (ypts-y)^2)
      index = Where(len LE 10, count)

      ; If not, then you are only selected IF you are inside
      ; the polygon of vertices.
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
;       POLYGON::SELECTVERTEX
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
FUNCTION Polygon::SelectVertex, x, y, DRAWID=drawID

   @cat_func_error_handler

   ; Update the mode map
   self -> Update_Modemap
   self.modemap -> GetProperty, XSize=xsize, YSize=ysize

   ; Normalize the location with respect to the polygon.
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
;       POLYGON::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the POLYGON object's properties. Be sure
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
;     THICKNESS:    Set this to the thickness of the output. By default, 1.0.
;
;     XPTS:         The X locations of the points of the polygon.
;
;     YPTS:         The Y locations of the points of the polygon.
;
;    _EXTRA:        Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO Polygon::SetProperty, $
   DRAW=draw, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   NOMESSAGE=nomessage, $
   NOREFRESH=norefresh, $
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

   ; Update the number of points.
   IF N_Elements(xpts) NE 0 THEN BEGIN
      IF N_Elements(xpts) NE N_Elements(ypts) THEN Message, 'Number of X and Y points is not the same.' ELSE $
         self.npoints = N_Elements(*self.xpts)
   ENDIF

   ; Send message?
   IF sendMessage AND ~Keyword_Set(nomessage) THEN self -> SendMessage, 'POLYGON_CHANGED'

   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed

END




;*****************************************************************************************************
;+
; NAME:
;       POLYGON::UPDATE_MODEMAP
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
PRO Polygon::Update_Modemap, CLEAR=clear

   @cat_pro_error_handler

   ; Get the current graphics window.
   currentWindow = !D.Window

   IF Keyword_Set(clear) THEN BEGIN
      self.modemap -> Refresh
      RETURN
   ENDIF

   ; Convert the boundary box from data to device coordinates.
   b = Convert_Coord(self.box, /Normal, /To_Device)
   b1 = b[0,0]
   b2 = b[0,2]
   d1 = b[1,0]
   d2 = b[1,1]

   ; Resize the modemap and refresh it.
   self.modeMap -> SetProperty, XSize=b2-b1+1, YSize=d2-d1+1
   self.modemap -> Refresh

   ; Convert the read box from data to device coordinates.
   self -> ApplyCoords
   self -> GetProperty, XPTS=xpts, YPTS=ypts
   x1 = Min(xpts, Max=x2)
   y1 = Min(ypts, Max=y2)
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

   ; Draw the handles on the box.
   phi = Findgen(32) * (!PI * 2 / 32.)
   phi = [ phi, phi[0] ]
   UserSym, Cos(phi), Sin(phi), /Fill


   ; Draw circles at each vertex point, consecutively numbered.
   ; First point is the same as last point.
   c = Convert_Coord(*self.xpts, *self.ypts, /Data, /To_DEVICE)
   xpts = c[0,*]-xoffset
   ypts = c[1,*]-yoffset

   self.modemap -> SetWindow
   Device, Decomposed=0, Get_Decomposed=theState
   TVLCT, r, g, b, /Get
   LoadCT, 0, /Silent
   POLYFILL, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=255

   FOR j=0,N_Elements(*self.xpts)-2 DO BEGIN
      PLOTS, xpts[j], ypts[j], PSYM=8, Color=j+1, /DEVICE, Symsize=2.5
   ENDFOR

   Device, Decomposed=theState
   TVLCT, r, g, b

   ; Restore the current graphics window.
   IF currentWindow GE 0 THEN WSet, currentWindow

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       POLYGON::CLEANUP
;
; PURPOSE:
;
;       This is the POLYGON object class destructor method.
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
PRO Polygon::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self.modemap
   Ptr_Free, self.xpts
   Ptr_Free, self.ypts
   IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self
   self -> SELECTABLEOBJECT::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       POLYGON::INIT
;
; PURPOSE:
;
;       This is the POLYGON object class initialization method
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
;     LAYER:        A CATLAYER object for holding annotations.
;
;     LINESTYLE:    The linestyle of the polygon. By default, 1.0 (solid).
;
;     THICKNESS:    Set this to the thickness of the output. By default, 1.0.
;
;     XPTS:         The X locations of the points of the polygon.
;
;     YPTS:         The Y locations of the points of the polygon.
;
;     _EXTRA:       Any keywords appropriate for the SELECTABLEOBJECT INIT method.
;-
;*****************************************************************************************************
FUNCTION Polygon::INIT, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   THICKNESS=thickness, $
   XPTS=xpts, $
   YPTS=ypts, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler


   ok = self -> SELECTABLEOBJECT::INIT (DESCRIPTION='Polygon Properties', _EXTRA=extraKeywords)
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
   IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
   IF N_Elements(thickness) EQ 0 THEN thickness = 1.0
   IF N_Elements(xpts) NE 0 THEN self.xpts = Ptr_New(xpts)
   IF N_Elements(ypts) NE 0 THEN self.ypts = Ptr_New(ypts)

   ; Update the number of points.
   IF N_Elements(xpts) NE 0 THEN BEGIN
      IF N_Elements(xpts) NE N_Elements(ypts) THEN Message, 'Number of X and Y points is not the same.' ELSE $
         self.npoints = N_Elements(*self.xpts)
   ENDIF

   ; Load object.
   IF Obj_Isa_Valid(layer, "CATLAYER") NE 0 THEN BEGIN
      self.layerObject = layer
      self.layerObject -> AddParent, self
   ENDIF
   self.linestyle = 0 > linestyle < 5
   self.thickness = thickness

   ; Create mode map.
   self.modemap = Obj_New('Pixmapwidget')

   ; Register properties for the property sheet. Turn visibility off, since some properties
   ; cause the object to refresh and draw prematurely.
   currentVisible = self.visible
   self.visible = 0
   self -> RegisterProperty, 'BG_COLOR', 0, USERDEF=CapFirstLetter(self.bg_color), NAME="Color (Background)"
   self -> RegisterProperty, 'BACKGROUND', 1, NAME="Background On"
   self -> SetPropertyByIdentifier, 'BACKGROUND', self.background
   self -> RegisterProperty, 'LINESTYLE', 2, NAME="Linestyle", VALID_RANGE=[0, 5]
   self -> RegisterProperty, 'THICKNESS', 2, NAME="Thickness", VALID_RANGE=[1, 10]
   self.visible = currentVisible

   self -> Report, /Completed

   RETURN, 1

END

;*****************************************************************************************************
;
; NAME:
;       POLYGON CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the POLYGON object.
;
;*****************************************************************************************************
PRO Polygon__Define

   class = { POLYGON, $
             insertedObject: Obj_New(), $; The new object created in the CreateNewObject method. (Ignored in CLEANUP.)
             layerObject: Obj_New(), $   ; A CATLAYER object for holding the annotation.
             linestyle: 0L, $            ; The line style of the polygon.
             xpts: Ptr_New(), $          ; The X points making up the polygon.
             ypts: Ptr_New(), $          ; The X points making up the polygon.
             modemap: Obj_New(), $       ; A pixmap for calculating "mode", or which vertex to move.
             moveVertex: 0L, $           ; The vertex index of the vertex we are moving.
             npoints: 0L, $              ; The number of points in the polygon
             pointsPixID: Obj_New(), $   ; A pixmap for storing polygon points.
             thickness: 0.0, $           ; The thickness of the polygon.
             sx: 0L, $                   ; The static X location.
             sy: 0L, $                   ; The static Y location.
             INHERITS SelectableObject $
           }

END

