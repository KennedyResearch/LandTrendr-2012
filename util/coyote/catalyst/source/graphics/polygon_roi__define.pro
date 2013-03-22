;*****************************************************************************************************
;+
; NAME:
;       POLYGON_ROI__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to create a polygon ROI drawing
;       interaction. When the interaction is in place, the user can
;       draw a polygon ROI in the display window.
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
;       theObject = Obj_New("POLYGON_ROI")
;
; SUPERCLASSES:
;
;       FREEHAND_ROI
;       INTERACTION
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { POLYGON_ROI, $
;             pointsID: Obj_New(), $        ; A pixmap for the selected points.
;             INHERITS FREEHAND_ROI $
;           }
;
; MESSAGES:
;
;   None.
;
; EVENT_STRUCTURE:
;
;       event = { ID:theObject, TOP:topObject, HANDLER:Obj_New(), EVENT_NAME='POLYGON_ROI_EVENT', $
;                  NAME: self._name, XPTS:FLTARR(), YPTS:FLATARR(), COUNT:0L }
;
;       In which the XPTS and YPTS fields contain the X and Y locations of the polygon in a form
;       suitable for drawing the ROI on the display with PLOTS. The COUNT field contains the number
;       of points in the XPTS and YPTS vectors. The XPTS and YPTS are converted to the coordinate
;       system of the draw widget associated with the interaction before being placed in the event structure.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. September 12, 2004.
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
;       POLYGON_ROI::ACCEPTROI
;
; PURPOSE:
;
;       This method accepts the box on the display and sends an event to the client.
;
; SYNTAX:
;
;       theObject -> AcceptROI, event
;
; ARGUMENTS:
;
;       event:        The event structure.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO Polygon_ROI::AcceptROI, event

   @cat_pro_error_handler

   ; Erase the last ROI drawn.
   self._drawID -> SetProperty, Motion_Events=0, /Clear_Events
   self._drawID -> SetWindow
   self._drawID_pixmap -> Copy

   ; Create the event structure.
   thisEvent = event
   thisEvent.ID = self._drawID
   thisEvent.HANDLER = Obj_New()
   thisEvent.EVENT_NAME='POLYGON_ROI_EVENT'
   thisEvent.NAME = self._name

   ; Convert the return coordinates, if you can.
   self._drawID -> GetProperty, Coord_Object=coords
   IF Obj_Valid(coords) THEN BEGIN
      coords -> Draw
      c = Convert_Coord(*self.xpts, *self.ypts, /Device, /To_Data)
      *self.xpts = c[0,*]
      *self.ypts = c[1,*]
   ENDIF

   ; Add coordinates to the event structure and send the event.
   thisEvent = Create_Struct(thisEvent, 'xpts', *self.xpts, 'ypts', *self.ypts, 'count', self.count)

   ; Send an event to the real draw widget event handlers.
   self -> SendEvent, thisEvent

   ; Clean up, if you are still around. The event handler may
   ; have killed you.
   IF Obj_Valid(self) THEN BEGIN

      self -> RestoreDisplay
      Ptr_Free, self.xpts
      Ptr_Free, self.ypts
      self.count = 0

      self -> Report, /Completed

   ENDIF

END



;*****************************************************************************************************
;+
; NAME:
;        POLYGON_ROI::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the POLYGON_ROI object.
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
PRO Polygon_ROI::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Pick the possible context menu button events out of the more like draw widget events.
   CASE event.name OF

      'ACCEPT': BEGIN
         self -> AcceptROI, event
         IF Obj_Valid(self._selectedObject) THEN BEGIN
            self._drawID -> Remove, self._selectedObject
            Obj_Destroy, self._selectedObject
         ENDIF

         RETURN
         END

      'CANCEL': BEGIN
         Ptr_Free, self.xpts
         Ptr_Free, self.ypts
         self.count = 0
         self -> CancelROI, event
         IF Obj_Valid(self._selectedObject) THEN BEGIN
            self._drawID -> Remove, self._selectedObject
            Obj_Destroy, self._selectedObject
         ENDIF
         RETURN
         END

      ELSE: ; From draw widget. Fall through.

   ENDCASE

   ; Only DOWN, UP, and MOTION events handled here.
   IF event.type GT 2 THEN RETURN

   event.id -> SetWindow
   CASE self._mode OF

      'MOVE': BEGIN

         ; What kind of event is this?
         eventTypes = ['DOWN', 'UP', 'MOTION']
         thisEvent = eventTypes[event.type]

         CASE thisEvent OF

           'UP': BEGIN

              self._drawID -> SetProperty, Motion_Events=0, /Clear_Events
              self._mode = 'DRAW'

              ; First, erase the last box.
              self.bufferID -> SetWindow
              self._drawID_pixmap -> Copy

              ; Draw the object.
              self.bufferID -> SetWindow
              self._selectedObject -> Draw

              ; Copy buffer to display.
              self._drawID -> SetWindow
              self.bufferID -> Copy

              ; Need a dialog?
              IF self._ask_on_up THEN BEGIN

                  ; Call context display menu.
                  Widget_DisplayContextMenu, event.id -> GetID(), event.x+20 , event.y+20, self._contextMenu->GetID()

              ENDIF

              END

           'MOTION': BEGIN

                 IF Obj_Valid(self._selectedObject) THEN BEGIN

                    ; How much have we moved?
                    deltaX = event.x - self._click_x
                    deltaY = event.y - self._click_y

                    ; First, erase the last box.
                    self.bufferID -> SetWindow
                    self._drawID_pixmap -> Copy

                    ; Move the object and draw it.
                    self._selectedObject -> Move, deltaX, deltaY, /NoDraw
                    self.bufferID -> SetWindow
                    self._selectedObject -> Draw

                    ; Copy buffer to display.
                    self._drawID -> SetWindow
                    self.bufferID -> Copy

                    ; Update center of motion.
                    self._click_x = event.x
                    self._click_y = event.y
                 ENDIF

              END

           ELSE:

         ENDCASE ; of thisEvent in MOVE

         END ; of case MOVE


      'DRAW': BEGIN

         ; Did you find a selectable object. If so, your mode is MOVE, otherwise, your MODE is DRAW.
         objects = self._drawID -> SelectObjects(event.x, event.y, Count=count)
         IF count NE 0 THEN self._mode = 'MOVE' ELSE self._mode = 'DRAW'
         IF self._mode EQ 'MOVE' THEN BEGIN
            self._drawID -> SetProperty, MOTION_EVENTS=1
            self._selectedObject = objects[0]
            self._click_x = event.x
            self._click_y = event.y
            RETURN
         ENDIF ELSE BEGIN
            ; Find the previous POLYGON object and remove it.
            polygon = self._drawID -> Get('SELECTABLE_POLYGON_ROI', Count=foundit)
            IF foundit THEN Obj_Destroy, polygon
            self._drawID -> Draw
         ENDELSE


         ; What kind of event is this?
         possibleEvents = ['DOWN', 'UP', 'MOTION']
         thisEvent = possibleEvents[event.type]
         whichButton = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']

         ; What kind of event was this? Do the right thing here. :-)
         thisEvent = possibleEvents[event.type]


         CASE thisEvent OF

            'DOWN':BEGIN ; Button DOWN event.

               ; Copy the display window into the pixmap
               IF Obj_Valid(self._drawID_Pixmap) EQ 0 THEN BEGIN
                  self._drawID -> GetProperty, XSize=xsize, YSize=ysize
                  self._drawID_Pixmap = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)
               ENDIF
               self._drawID_Pixmap -> SetWindow
               self._drawID -> Copy

               ; Which button was pressed? Branch accordingly. RIGHT button closes ROI,
               ; anything else selects/deselects.
               theButton = whichButton[event.press]

               CASE theButton OF

                  'RIGHT':BEGIN

                     ; No events unless we have clicked another button first.
                     IF self.count LE 0 THEN RETURN

                     ; No double clicking.
                     IF event.clicks EQ 2 THEN RETURN

                     ; Need at least three points for a polygon.
                     IF self.count LT 2 THEN BEGIN
                       ok = Dialog_Message('Three points required to create polygon ROI.')
                       self.count = 0
                       Ptr_Free, self.xpts
                       Ptr_Free, self.ypts
                       RETURN
                     ENDIF

                     ; Add point to the ROI
                     IF Ptr_Valid(self.xpts) THEN *self.xpts = [ *self.xpts, event.x] ELSE $
                       self.xpts = Ptr_New([event.x])
                     IF Ptr_Valid(self.ypts) THEN *self.ypts = [ *self.ypts, event.y] ELSE $
                       self.ypts = Ptr_New([event.y])
                     self.count = self.count + 1

                     ; Wrap the points around on each other.
                     *self.xpts = [*self.xpts, (*self.xpts)[0]]
                     *self.ypts = [*self.ypts, (*self.ypts)[0]]
                     self.count = self.count + 1

                     ; Close the loop.
                     *self.xpts = [(*self.xpts), (*self.xpts)[0]]
                     *self.ypts = [(*self.ypts), (*self.ypts)[0]]
                     self.count = self.count + 1

                     ; Create a selectable object.
                     self -> GetProperty, Color=color, Linestyle=linestyle, Thick=thick, DrawWidget=drawID
                     drawID -> GetProperty, XSize=xsize, YSize=ysize

                     ; Relevant points are in device coordinates. Should be converted to
                     ; normalized coordinates for moving and selecting.
                     c = Convert_Coord(*self.xpts, *self.ypts, /Device, /To_Normal)
                     polygonObj = Obj_New('Polygon', $
                        Color=color, $
                        Coord_Object=Obj_New('CatCoord', XRange=[0,1], YRange=[0,1], Position=[0,0,1,1]), $
                        Linestyle=linestyle, $
                        Name='SELECTABLE_POLYGON_ROI', $
                        Thick=thick, $
                        XPTS=c[0,*], $
                        YPTS=c[1,*] )

                     ; If the selectedObject still exists, remove it from
                     ; the draw widget and destroy it.
                     IF Obj_Valid(self._selectedObject) THEN BEGIN
                        self._drawID -> Remove, self._selectedObject
                        Obj_Destroy, self._selectedObject
                     ENDIF

                     ; Add the polygon object to the window so it can be selected and moved.
                     drawID -> Add, polygonObj
                     self.bufferID -> SetWindow
                     polygonObj -> Draw
                     self._selectedObject = polygonObj

                     ; Copy buffer to display.
                     self._drawID -> SetWindow
                     self.bufferID -> Copy

                     ; Flush the graphics buffer because the context menu happens too quickly.
                     Empty

                     ; Call context display menu.
                     x = Max(*self.xpts) + 10
                     y = Round(Total(*self.ypts) / N_Elements(*self.ypts))
                     self._drawID -> SetProperty, Motion_Events=0, /Clear_Events

                     ; Need a dialog?
                     IF self._ask_on_up THEN BEGIN

                        ; Call context display menu.
                        x = Max(*self.xpts) + 10
                        y = Round(Total(*self.ypts) / N_Elements(*self.ypts))
                        Widget_DisplayContextMenu, event.id -> GetID(), x, y, self._contextMenu->GetID()
                        Obj_Destroy, self.pointsID

                     ENDIF ELSE BEGIN

                        self -> AcceptROI, event
                        Obj_Destroy, self.pointsID
                        RETURN

                     ENDELSE

                  END

                  ELSE: BEGIN ; It is LEFT of MIDDLE mouse button.
                     IF event.clicks EQ 1 THEN BEGIN ; Single click selects new point.

                        ; If pointsID pixmap doesn't exist, create it.
                           IF Obj_Valid(self.pointsID) EQ 0 THEN BEGIN
                              self._drawID -> GetProperty, XSize=xsize, YSize=ysize
                              self.pointsID = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize, Name='Points Pixmap')
                              self._drawID -> SetWindow
                              self._drawID_pixmap -> Copy
                              self.pointsID -> SetWindow
                              self._drawID_pixmap -> Copy
                              Ptr_Free, self.xpts
                              Ptr_Free, self.ypts
                              self.count = 0
                           ENDIF

                        IF self.count LE 0 THEN self._drawID -> SetProperty, Motion_Events=1

                        ; Add point to ROI.
                        IF Ptr_Valid(self.xpts) THEN *self.xpts = [ *self.xpts, event.x] ELSE $
                           self.xpts = Ptr_New([event.x])
                        IF Ptr_Valid(self.ypts) THEN *self.ypts = [ *self.ypts, event.y] ELSE $
                           self.ypts = Ptr_New([event.y])
                        self.count = self.count + 1
                        IF self.count GE 2 THEN BEGIN
                           self.pointsID -> SetWindow
                           PlotS, (*self.xpts)[self.count-2:self.count-1], Color=FSC_Color(self._roi_color), $
                                 (*self.ypts)[self.count-2:self.count-1], /Device, Thick=self.thick, $
                                 Linestyle=self.linestyle
                        ENDIF
                        IF self.count GT 1 THEN self -> Draw


                      ENDIF ELSE BEGIN ; Double click to close loop.

                        ; Need at least three points for a polygon.
                        IF self.count LT 2 THEN BEGIN
                          ok = Dialog_Message('Three points required to create polygon ROI.')
                          self.count = 0
                          Ptr_Free, self.xpts
                          Ptr_Free, self.ypts
                          RETURN
                        ENDIF

                        ; Close the loop.
                        *self.xpts = [(*self.xpts), (*self.xpts)[0]]
                        *self.ypts = [(*self.ypts), (*self.ypts)[0]]
                        self.count = self.count + 1

                        ; Create a selectable object.
                        self -> GetProperty, Color=color, Linestyle=linestyle, Thick=thick, DrawWidget=drawID
                        drawID -> GetProperty, XSize=xsize, YSize=ysize

                        ; Relevant points are in device coordinates. Should be converted to
                        ; normalized coordinates for moving and selecting.
                        c = Convert_Coord(*self.xpts, *self.ypts, /Device, /To_Normal)
                        polygonObj = Obj_New('Polygon', $
                           Color=color, $
                           Coord_Object=Obj_New('CatCoord', XRange=[0,1], YRange=[0,1], Position=[0,0,1,1]), $
                           Linestyle=linestyle, $
                           Name='SELECTABLE_POLYGON', $
                           Thick=thick, $
                           XPTS=c[0,*], $
                           YPTS=c[1,*] )

                        drawID -> Add, polygonObj
                        self.bufferID -> SetWindow
                        polygonObj -> Draw
                        self._selectedObject = polygonObj

                        ; Copy buffer to display.
                        self._drawID -> SetWindow
                        self.bufferID -> Copy

                        ; Flush the graphics buffer because the context menu happens too quickly.
                        Empty

                        ; Need a dialog?
                        IF self._ask_on_up THEN BEGIN

                           ; Call context display menu.
                           x = Max(*self.xpts) + 10
                           y = Round(Total(*self.ypts) / N_Elements(*self.ypts))
                           Widget_DisplayContextMenu, event.id -> GetID(), x, y, self._contextMenu->GetID()
                           Obj_Destroy, self.pointsID

                        ENDIF ELSE BEGIN

                           self -> AcceptROI, event
                           RETURN

                        ENDELSE

                        END

                     ENDELSE

               ENDCASE

               END

            'UP':

            'MOTION': BEGIN

               ; Copy and draw the last two points.
               IF self.count GE 1 THEN BEGIN
                  self._drawID -> SetWindow
                  self.pointsID -> Copy
                  PlotS, [(*self.xpts)[self.count-1], event.x], Color=FSC_Color(self._roi_color), $
                         [(*self.ypts)[self.count-1], event.y], /Device, Thick=self.thick, $
                         Linestyle=self.linestyle
               ENDIF
               END

            ELSE:

         ENDCASE ; of thisEvent

         END ; of DRAW mode event

   ENDCASE ; of self._mode

   ; Report completion
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       POLYGON_ROI::RESTOREDISPLAY
;
; PURPOSE:
;
;       This method ...
;
; SYNTAX:
;
;       theObject -> RestoreDisplay
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
PRO Polygon_ROI::RestoreDisplay

   @cat_pro_error_handler

   ; Call Superclass RestoreDisplay.
   self -> FREEHAND_ROI::RestoreDisplay

   ; Delete the point pixmap object.
   Obj_Destroy, self.pointsID

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       POLYGON_ROI::CLEANUP
;
; PURPOSE:
;
;       This is the POLYGON_ROI object class destructor method.
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
PRO Polygon_ROI::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self.pointsID
   self -> FREEHAND_ROI::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;
; NAME:
;       POLYGON_ROI CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the POLYGON_ROI object.
;
;*****************************************************************************************************
PRO Polygon_ROI__DEFINE, class

   class = { POLYGON_ROI, $
             pointsID: Obj_New(), $        ; A pixmap for the selected points.
             INHERITS FREEHAND_ROI $
           }

END

