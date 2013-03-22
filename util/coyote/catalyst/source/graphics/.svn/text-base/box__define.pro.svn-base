;*****************************************************************************************************
;+
; NAME:
;       BOX__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide a rectangle or box that can be displayed
;       in a direct graphics draw widget. The coordinate system of the Box
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
;       boxObject = Obj_New("BOX", X1=0.5, Y1=0.5, X2=0.75, Y2=0.75)
;       drawObject -> Add, boxObject
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
;   class = { BOX, $
;             clip_to_data: 0B, $         ; Flag that if set will allow clipping to data range in DRAW method.
;             layerObject: Obj_New(), $   ; A CATLAYER object for holding the annotation.
;             linestyle: 0L, $            ; The line style of the box.
;             midx: 0.0, $                ; The midpoint of the box in X.
;             midy: 0.0, $                ; The midpoint of the box in Y.
;             modemap: Obj_New(), $       ; A pixmap for calculating "mode".
;             moveend: 0L, $              ; Indicates which end of box you are moving.
;             thickness: 0.0, $           ; The thickness of the box.
;             x1: 0.0, $                  ; The X location for lower-left corner of the box.
;             y1: 0.0, $                  ; The Y location for lower-left corner of the box.
;             x2: 0.0, $                  ; The X location for upper-right corner of the box.
;             y2: 0.0, $                  ; The Y location for upper-right corner of the box.
;             sx: 0L, $                   ; The static X location.
;             sy: 0L, $                   ; The static Y location.
;             INHERITS SelectableObject $
;           }
;
; MESSAGES:
;
;   BOX_CHANGED:   This message is sent whenever SetProperty method is called and the NOMESSAGE
;                  keyword is NOT set.
;
; EVENT_STRUCTURE:
;
;       This object will add the following fields to the event structure created by an interaction.
;       The fields are defined as:
;
;       BOUNDARY_BOX:     A 2x5 element array in normalized coordinates giving the boundary box of
;                         the object.
;
;       XPTS:             A two element array giving the X values of the lower-left and upper-right
;                         corners of the box in data coordinates.
;
;       YPTS:             A two element array giving the Y values of the lower-left and upper-right
;                         corners of the box in data coordinates.
;
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
;       BOX::ADDTOEVENTSTRUCTURE
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
FUNCTION Box::AddToEventStructure, event

   @cat_func_error_handler

   ; Add appropriate fields to the event structure.
   self -> GetProperty, X1=x1, X2=x2, Y1=y1, Y2=y2
   event = Create_Struct( event, $
      'BOUNDARY_BOX', self.box, $
      'XPTS', [x1, x2], $
      'YPTS', [y1, y2] )

   RETURN, event

END


;*****************************************************************************************************
;+
; NAME:
;       BOX::CALCULATEBOUNDARYBOX
;
; PURPOSE:
;
;       This method calculates the extent of a boundary box about the box itself. The boundary box
;       (self.box) is always stored in normalized coordinates.
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
PRO Box::CalculateBoundaryBox

   @cat_pro_error_handler

   x1 = Min([self.x1,self.x2], Max=x2)
   y1 = Min([self.y1,self.y2], Max=y2)
   self -> ApplyCoords
   c = Convert_Coord([x1,x2], [y1,y2], /Data, /To_Normal)

   ; The actual boundary box is 10 pixels larger than the box itself. This
   ; serves as an aid in selecting the box handles for movement.
   b = Convert_Coord(5, 5, /Device, /To_Normal)
   self.box[0,*] = [c[0,0]-b[0,0], c[0,0]-b[0,0], c[0,1]+b[0,0], c[0,1]+b[0,0], c[0,0]-b[0,0]]
   self.box[1,*] = [c[1,0]-b[1,0], c[1,1]+b[1,0], c[1,1]+b[1,0], c[1,0]-b[1,0], c[1,0]-b[1,0]]

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       BOX::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the BOX object.
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
PRO Box::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Create a new control panel

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Box Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, Name='BOX PROPERTYSHEET', YSize=12)
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       BOX::CREATENEWOBJECT
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
;       drawID:    The draw widget which will contain the newly created object. Required in normal operation.
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
PRO Box::CreateNewObject, drawID, pixmapID, NEWOBJECT=newObject

   @cat_pro_error_handler

   self -> GetProperty, $
      BACKGROUND=background, $
      BG_COLOR=bg_color, $
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      LAYER=layer, $
      LINESTYLE=linestyle, $
      THICKNESS=thickness, $
      X1=x1, $
      Y1=y1, $
      X2=x2, $
      Y2=y2

   newObject = Obj_New('BOX',  $
      BACKGROUND=background, $
      BG_COLOR=bg_color, $
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      LAYER=layer, $
      LINESTYLE=linestyle, $
      THICKNESS=thickness, $
      X1=x1, $
      Y1=y1, $
      X2=x2, $
      Y2=y2)

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

      ; Draw the new arrow.
      newObject -> Draw

   ENDELSE


   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       BOX::DRAW
;
; PURPOSE:
;
;       This method draws the box in the current direct graphics display window.
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
PRO Box::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; If the box is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   ; Apply the coordinate system.
   self -> ApplyCoords

   ; Calculate the boundary box.
   self -> CalculateBoundaryBox

   ; Draw the background if required.
   IF self.background THEN $
      PolyFill, [self.x1, self.x1, self.x2, self.x2, self.x1], $
                [self.y1, self.y2, self.y2, self.y1, self.y1], $
                 Fill=1, Color=FSC_Color(self.bg_color), NOCLIP=1-self.clip_to_data

   ; Draw the box.
   PlotS, [self.x1, self.x1, self.x2, self.x2, self.x1], $
          [self.y1, self.y2, self.y2, self.y1, self.y1], Color=FSC_Color(self.color), $
          Thick=self.thickness, Linestyle=self.linestyle, NOCLIP=1-self.clip_to_data

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       BOX::DRAWSELECTIONBOX
;
; PURPOSE:
;
;       This method draws a selection box around the boundary box of the box
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
;       COLOR:    The name of a color to draw the box in. By default, the color of the box.
;
;-
;*****************************************************************************************************
PRO Box::DrawSelectionBox, Color=color

   @cat_pro_error_handler

   ; If the box is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   ; Apply the coordinate system.
   self -> ApplyCoords

   IF N_Elements(color) EQ 0 THEN color = self.color

   ; Find midpoint of box.
   xl = Min([self.x1, self.x2], Max=xr)
   self.midx = (xr - xl) / 2.0 + xl
   yl = Min([self.y1, self.y2], Max=yr)
   self.midy = (yr - yl) / 2.0 + yl

   ; Draw the handles on the box.
   PLOTS, self.x1, self.y1,   PSYM=6, Color=FSC_Color(color), Symsize=1.25, NOCLIP=1-self.clip_to_data
   PLOTS, self.x1, self.y2,   PSYM=6, Color=FSC_Color(color), Symsize=1.25, NOCLIP=1-self.clip_to_data
   PLOTS, self.x2, self.y1,   PSYM=6, Color=FSC_Color(color), Symsize=1.25, NOCLIP=1-self.clip_to_data
   PLOTS, self.x2, self.y2,   PSYM=6, Color=FSC_Color(color), Symsize=1.25, NOCLIP=1-self.clip_to_data
   PLOTS, self.x1, self.midy, PSYM=6, Color=FSC_Color(color), Symsize=1.25, NOCLIP=1-self.clip_to_data
   PLOTS, self.x2, self.midy, PSYM=6, Color=FSC_Color(color), Symsize=1.25, NOCLIP=1-self.clip_to_data
   PLOTS, self.midx, self.y1, PSYM=6, Color=FSC_Color(color), Symsize=1.25, NOCLIP=1-self.clip_to_data
   PLOTS, self.midx, self.y2, PSYM=6, Color=FSC_Color(color), Symsize=1.25, NOCLIP=1-self.clip_to_data

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       BOX::EVENTHANDLER
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
PRO Box::EventHandler, event

   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF


      'BOX PROPERTYSHEET': BEGIN

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
                  IF Obj_Valid(self) THEN BEGIN
                     CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, REQUESTER=self
                  ENDIF ELSE RETURN


               ENDCASE

            ENDCASE

         ENDIF ; of BUTTON DOWN EVENT

         ENDCASE ; of BOX PROPERYSHEET events

        ; If you can't handle the event here. Pass it along to superclass EventHandler
        ELSE: self -> SelectableObject::EventHandler, event

   ENDCASE

   IF Obj_Valid(self) THEN self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       BOX::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain BOX properties. Be sure
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
;     HEIGHT:         The height of the box boundary box in normalized coordinates.
;
;     INSERTEDOBJECT: The new object that is inserted in the CreateNewObject method.
;
;     LAYER:          The annotation layer associated with this object.
;
;     LINESTYLE:      The type of linestyle required. See PLOT documentation for details.
;
;     LENGTH:         The length of the box in data coordinates.
;
;     THICKNESS:      The current thickness of the box.
;
;     WIDTH:          The width of the box boundary box in normalized coordinates.
;
;     X1:             The X location of lower-left corner of the box.
;
;     Y1:             The Y location of lower-left corner of the box.
;
;     X2:             The X location of upper-right corner of the box.
;
;     Y2:             The Y location of upper-right corner of the box.
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO Box::GetProperty, $
   CLIP_TO_DATA=clip_to_data, $
   HEIGHT=height, $
   INSERTEDOBJECT=insertedObject, $
   LAYER=layer, $
   LENGTH=length, $
   LINESTYLE=linestyle, $
   THICKNESS=thickness, $
   WIDTH=width, $
   X1=x1, $
   Y1=y1, $
   X2=x2, $
   Y2=y2, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(clip_to_data) THEN clip_to_data = self.clip_to_data
   IF Arg_Present(height) THEN BEGIN
      maxy = Max(self.box[1,*], Min=miny)
      height = maxy - miny
   ENDIF
   IF Arg_Present(insertedObject) THEN insertedObject = self.insertedObject
   IF Arg_Present(layer) THEN layer = self.layerObject
   IF Arg_Present(length) THEN BEGIN
      length = SQRT((self.x2-self.x1)^2 + (self.y2-self.y1)^2)
   END
   IF Arg_Present(linestyle) THEN linestyle = self.linestyle
   IF Arg_Present(thickness) THEN thickness = self.thickness
   IF Arg_Present(width) THEN BEGIN
      maxx = Max(self.box[0,*], Min=minx)
      width = maxx - minx
   ENDIF
   IF Arg_Present(x1) THEN x1 = self.x1
   IF Arg_Present(y1) THEN y1 = self.y1
   IF Arg_Present(x2) THEN x2 = self.x2
   IF Arg_Present(y2) THEN y2 = self.y2

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       BOX::INTERACTIONEVENTS
;
; PURPOSE:
;
;       This method accepts events from an interaction object of some type. The interaction
;       may pre-process events, or send them directly here.
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
PRO Box::InteractionEvents, event, Interaction=interaction

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

                       ; Only interested in LEFT mouse events PRESS events, here.
                       IF event.press EQ 1 THEN BEGIN

                         ; If you are close to a selection circle, then you are moving
                         ; an edge or edges of the box and not the box itself.
                         boxmode = self -> SelectMode(event.x, event.y, DRAWID=drawID)
                         IF boxmode NE 9 THEN BEGIN
                            self.moveend = boxmode
                            interaction -> SetProperty, Mode='MOVE_EDGE'
                         ENDIF

                       END ; of LEFT PRESS event

         END ; of SELECT mode

      'INSERT': BEGIN

         CASE thisEvent OF

           'DOWN': BEGIN

                ; Draw your coordinate system and convert DOWN location from device to
                ; data coordinates and store it.
                self -> ApplyCoords
                c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                self.sx = event.x
                self.sy = event.y
                self -> SetProperty, X1=c[0,0], Y1=c[1,0]

                ; Turn motion events on.
                drawID -> SetProperty, MOTION_EVENTS=1

                ; Take a snapshot of the background. Make sure pixmap is correct size.
                drawID -> GetProperty, XSize=xsize, YSize=ysize
                pixmap -> GetProperty, XSize=pxsize, YSize=pysize
                IF (xsize NE pxsize) OR (ysize NE pysize) THEN $
                  pixmap -> SetProperty, XSize=xsize, YSize=ysize
                drawID -> SetWindow
                pixmap -> Copy
              END

           'UP': BEGIN

              ; Turn motion events off and clear any events still on the queue.
              drawID -> SetProperty, Motion_Events=0, /Clear_Events

              ; If there has been no movement of mouse, then there is nothing to do.
              IF (self.sx EQ event.x) AND (self.sy EQ event.y) THEN RETURN

              ; Indicate you have finished the mode.
              interaction -> SetProperty, Mode='FINISHED_INSERT'


              drawID -> SetWindow
              self -> ApplyCoords
              c = Convert_Coord(event.x, event.y, /Device, /To_Data)
              self -> SetProperty, X2=c[0,0], Y2=c[1,0]
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
              self -> SetProperty, X2=c[0,0], Y2=c[1,0], /Draw
              IF Obj_Valid(self._controlpanel) THEN $
                   self._controlpanel -> Refresh_Properties, Properties=['X2','Y2']
              END

           ELSE:

         ENDCASE ; of thisEvent in INSERT

         END ; of INSERT mode


      'DRAW': BEGIN

         CASE thisEvent OF

           'DOWN': BEGIN

                ; Draw your coordinate system and convert DOWN location from device to
                ; data coordinates and store it.
                self -> ApplyCoords
                c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                self.sx = event.x
                self.sy = event.y
                self -> SetProperty, X1=c[0,0], Y1=c[1,0]

                ; Turn motion events on.
                drawID -> SetProperty, MOTION_EVENTS=1

                ; Take a snapshot of the background. Make sure pixmap is correct size.
                drawID -> GetProperty, XSize=xsize, YSize=ysize
                pixmap -> GetProperty, XSize=pxsize, YSize=pysize
                IF (xsize NE pxsize) OR (ysize NE pysize) THEN $
                  pixmap -> SetProperty, XSize=xsize, YSize=ysize
                drawID -> SetWindow
                pixmap -> Copy

              END

           'UP': BEGIN

              ; Turn motion events off and clear any events still on the queue.
              drawID -> SetProperty, Motion_Events=0, /Clear_Events

              ; If there has been no movement of mouse, then there is nothing to do.
              IF (self.sx EQ event.x) AND (self.sy EQ event.y) THEN RETURN

              ; Indicate you have finished the mode.
              interaction -> SetProperty, Mode='FINISHED_DRAW'

              ; Update the coordinates.
              drawID -> SetWindow
              self -> ApplyCoords
              c = Convert_Coord(event.x, event.y, /Device, /To_Data)
              self -> SetProperty, X2=c[0,0], Y2=c[1,0]
              IF Obj_Valid(self._controlpanel) THEN $
                  self._controlpanel -> Refresh_Properties, Properties=['X2','Y2']

              ; Erase yourself.
              self -> CopyParameters, drawID, Destination=d, Extent=e
              pixmap -> Copy, Destination=d, Extent=e,  Origin=d

              END

           'MOTION': BEGIN

                 drawID -> SetWindow
                 self -> CopyParameters, drawID, Destination=d, Extent=e
                 pixmap -> Copy, Destination=d, Extent=e, Origin=d
                 self -> ApplyCoords
                 c = Convert_Coord(event.x, event.y, /Device, /To_Data)
                 self -> SetProperty, X2=c[0,0], Y2=c[1,0], /Draw
                 IF Obj_Valid(self._controlpanel) THEN $
                      self._controlpanel -> Refresh_Properties, Properties=['X2','Y2']
              END

           ELSE:

         ENDCASE ; of thisEvent in DRAW

         END ; of DRAW mode

      'MOVE_EDGE': BEGIN

         CASE thisEvent OF


           'UP': BEGIN
                 drawID -> SetProperty, Motion_Events=0, /Clear_Events
                 drawID -> SetWindow
                 interaction -> SetProperty, Mode='FINISHED_MOVE_EDGE'
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

                 CASE self.moveend OF

                     0: BEGIN ; Outside Box
                        END

                     1: BEGIN ; Lower-left corner handle.
                        self -> SetProperty, X1=c[0,0], Y1=c[1,0], /Draw
                        END

                     2: BEGIN ; Upper-left corner handle.
                        self -> SetProperty, X1=c[0,0], Y2=c[1,0], /Draw
                        END

                     3: BEGIN ; Lower-right corner handle.
                        self -> SetProperty, X2=c[0,0], Y1=c[1,0], /Draw
                        END

                     4: BEGIN ; Upper-right corner handle.
                        self -> SetProperty, X2=c[0,0], Y2=c[1,0], /Draw
                        END

                     5: BEGIN ; Left side handle.
                        self -> SetProperty, X1=c[0,0], /Draw
                        END

                     6: BEGIN ; Right side handle.
                        self -> SetProperty, X2=c[0,0], /Draw
                        END

                     7: BEGIN ; Bottom handle.
                        self -> SetProperty, Y1=c[1,0], /Draw
                        END

                     8: BEGIN ; Top handle.
                        self -> SetProperty, Y2=c[1,0], /Draw
                        END

                     ELSE:

                 ENDCASE
                 IF Obj_Valid(self._controlpanel) THEN $
                      self._controlpanel -> Refresh_Properties, Properties=['X1','Y1','X2','Y2']

                 ; Draw the selection box.
                 self -> DrawSelectionBox

              END ; Of MOTION events.

           ELSE:

         ENDCASE ; of thisEvent in MOVE_EDGE

         END ; of MOVE_EDGE

         ELSE: BEGIN
               ok = Dialog_Message('Unknown MODE [ ' + mode + ' ] in BOX::InteractionEvents method.')
               END
  ENDCASE

   self -> Report, /Completed


END



;*****************************************************************************************************
;+
; NAME:
;       BOX::MOVE
;
; PURPOSE:
;
;       This method moves the box in a graphics window.
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
PRO Box::Move, x, y, PIXMAP=pixmap, NODRAW=noDraw

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
       self._controlpanel -> Refresh_Properties, Properties=['X1','Y1','X2','Y2']

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
;       BOX::SELECTMODE
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
;       DrawID:     A window object that contains the box. Required for converting to
;                   the proper device coodinate system.
;-
;*****************************************************************************************************
FUNCTION Box::SelectMode, x, y, DRAWID=drawID

   @cat_func_error_handler

   ; Update the mode map
   self -> Update_Modemap
   self.modemap -> GetProperty, XSize=xsize, YSize=ysize

   ; Normalize the location with respect to the box.
   IF N_Elements(drawID) NE 0 THEN drawID -> SetWindow
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
;       BOX::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the BOX object's properties. Be sure
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
;     DRAW:         If this keyword is set, the DRAW method will be called at the end of the SetProperty method.
;
;     LAYER:        A CATLAYER object for holding annotations.
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
;     X1:           The X location of lower-left corner of the box.
;
;     Y1:           The Y location of lower-left corner of the box.
;
;     X2:           The X location of upper-right corner of the box.
;
;     Y2:           The Y location of upper-right corner of the box.
;-
;*****************************************************************************************************
PRO Box::SetProperty, $
   CLIP_TO_DATA=clip_to_data, $
   DRAW=draw, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   NOMESSAGE=nomessage, $
   NOREFRESH=norefresh, $
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

   ; The object could have been deleted. If so, RETURN.
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
   IF N_Elements(thickness) NE 0 THEN BEGIN
      sendMessage = 1
      self.thickness = thickness
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

   ; Send message?
   IF sendMessage AND ~Keyword_Set(nomessage) THEN self -> SendMessage, 'BOX_CHANGED'

   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed

END




;*****************************************************************************************************
;+
; NAME:
;       BOX::UPDATE_MODEMAP
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
PRO Box::Update_Modemap, CLEAR=clear

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

   ; Convert the read box from data to device coordinates.
   x1 = Min([self.x1, self.x2], Max=x2)
   y1 = Min([self.y1, self.y2], Max=y2)
   self.x1 = x1
   self.x2 = x2
   self.y1 = y1
   self.y2 = y2
   self -> ApplyCoords
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

   self.modemap -> SetWindow
   Device, Decomposed=0, Get_Decomposed=theState
   TVLCT, r, g, b, /Get
   LoadCT, 0, /Silent
   POLYFILL, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=9
   PLOTS, x1, y1,   PSYM=8, Color=1, /Device, Symsize=1.5
   PLOTS, x1, y2,   PSYM=8, Color=2, /Device, Symsize=1.5
   PLOTS, x2, y1,   PSYM=8, Color=3, /Device, Symsize=1.5
   PLOTS, x2, y2,   PSYM=8, Color=4, /Device, Symsize=1.5
   PLOTS, x1, midy, PSYM=8, Color=5, /Device, Symsize=1.5
   PLOTS, x2, midy, PSYM=8, Color=6, /Device, Symsize=1.5
   PLOTS, midx, y1, PSYM=8, Color=7, /Device, Symsize=1.5
   PLOTS, midx, y2, PSYM=8, Color=8, /Device, Symsize=1.5

   Device, Decomposed=theState
   TVLCT, r, g, b

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       BOX::CLEANUP
;
; PURPOSE:
;
;       This is the BOX object class destructor method.
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
PRO Box::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self.modemap
   IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self

   self -> SELECTABLEOBJECT::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       BOX::INIT
;
; PURPOSE:
;
;       This is the BOX object class initialization method
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
;     CLIP_TO_DATA: The box is draw with PLOTS, which normally does NOT clip to
;                   the data range. Set this keyword to put clipping into effect for
;                   drawing the box.
;
;     LAYER:        A CATLAYER object for holding other objects. Used here only when there is an UP
;                   event in INSERT mode. At that time a copy of this object is made and inserted
;                   the layer object and this is then inserted into the DrawWidget and/or Pixmap object.
;
;
;     LINESTYLE:    The linestyle of the box. By default, 1.0 (solid).
;
;     THICKNESS:    Set this to the thickness of the output. By default, 1.0.
;
;     X1:           The X location of one corner of the box in data coordinates.
;
;     Y1:           The Y location of one corner of the box in data coordinates.
;
;     X2:           The X location of the corner end of the box in data coordinates.
;
;     Y2:           The Y location of the corner end of the box in data coordinates.
;
;     _EXTRA:       Any keywords appropriate for the SELECTABLEOBJECT INIT method.
;-
;*****************************************************************************************************
FUNCTION Box::INIT, $
   CLIP_TO_DATA=clip_to_data, $
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

   ok = self -> SELECTABLEOBJECT::INIT (DESCRIPTION='Box Properties', _EXTRA=extraKeywords)
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
   IF Obj_Isa_Valid(layer, "CATLAYER") NE 0 THEN BEGIN
      self.layerObject = layer
      self.layerObject -> AddParent, self
   ENDIF
   IF N_Elements(thickness) EQ 0 THEN thickness = 1.0
   IF N_Elements(x1) EQ 0 THEN x1 = 0.25
   IF N_Elements(y1) EQ 0 THEN y1 = 0.25
   IF N_Elements(x2) EQ 0 THEN x2 = 0.75
   IF N_Elements(y2) EQ 0 THEN y2 = 0.75

   ; Load object.
   self.clip_to_data = Keyword_Set(clip_to_data)
   self.linestyle = 0 > linestyle < 5
   self.modemap = Obj_New('Pixmapwidget')
   self.thickness = thickness
   self.x1 = x1
   self.x2 = x2
   self.y1 = y1
   self.y2 = y2

   ; Register properties for the property sheet. Turn visibility off, since some properties
   ; cause the object to refresh and draw prematurely.
   currentVisible = self.visible
   self.visible = 0
   self -> RegisterProperty, 'BG_COLOR', 0, USERDEF=CapFirstLetter(self.bg_color), NAME="Color (Background)"
   self -> RegisterProperty, 'BACKGROUND', 1, NAME="Background On"
   self -> SetPropertyByIdentifier, 'BACKGROUND', self.background
   self -> RegisterProperty, 'LINESTYLE', 2, NAME="Linestyle", VALID_RANGE=[0, 5]
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
;       BOX CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the BOX object.
;
;*****************************************************************************************************
PRO Box__Define, class

   class = { BOX, $
             clip_to_data: 0B, $         ; Flag that if set will allow clipping to data range in DRAW method.
             insertedObject: Obj_New(), $; The new object created in the CreateNewObject method. (Ignored in CLEANUP.)
             layerObject: Obj_New(), $   ; A CATLAYER object for holding the annotation.
             linestyle: 0L, $            ; The line style of the box.
             midx: 0.0, $                ; The midpoint of the box in X.
             midy: 0.0, $                ; The midpoint of the box in Y.
             modemap: Obj_New(), $       ; A pixmap for calculating "mode".
             moveend: 0L, $              ; Indicates which end of box you are moving.
             thickness: 0.0, $           ; The thickness of the box.
             x1: 0.0, $                  ; The X location for lower-left corner of the box.
             y1: 0.0, $                  ; The Y location for lower-left corner of the box.
             x2: 0.0, $                  ; The X location for upper-right corner of the box.
             y2: 0.0, $                  ; The Y location for upper-right corner of the box.
             sx: 0L, $                   ; The static X location.
             sy: 0L, $                   ; The static Y location.
             INHERITS SelectableObject $
           }

END

