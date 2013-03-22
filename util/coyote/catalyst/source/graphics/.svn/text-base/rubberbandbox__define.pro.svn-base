;*****************************************************************************************************
;+
; NAME:
;       RUBBERBANDBOX__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a rubberband-box ROI interaction.
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
;       theObject = Obj_New("RUBBERBANDBOX")
;
; SUPERCLASSES:
;
;       INTERACTION
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { RUBBERBANDBOX, $
;             box: FltArr(4), $            ; The corners of the box, once drawn.
;             INHERITS INTERACTION $
;           }
;
; MESSAGES:
;
;   None.
;
; EVENT_STRUCTURE:
;
;       event = { ID:theObject, TOP:topObject, HANDLER:Obj_New(), EVENT_NAME='RUBBERBANDBOX_EVENT', $
;                  NAME: self._name, BOX:self.box, XPTS:FLTARR(5), YPTS:FLATARR(5), $
;                  ACTION:"" }
;
;       In which the XPTS and YPTS fields contain the X and Y locations of the box in a form
;       suitable for drawing the ROI on the display with PLOTS. The BOX, XPTS, and YPTS are
;       converted to the coordinate system of the draw widget associated with the interaction
;       before being placed in the event structure. The ACTION field value will be "ACCEPT"
;       by default, unless the ASK_ON_UP keyword is set and the user choses "CANCEL".
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 25 October 2004.
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
;       RUBBERBANDBOX::ACCEPTROI
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
PRO RubberbandBox::AcceptROI, event

   @cat_pro_error_handler

   ; Erase the last ROI drawn.
   self._drawID -> SetProperty, Motion_Events=0, /Clear_Events
   self._drawID -> SetWindow
   self._drawID_pixmap -> Copy

   ; Create the event structure.
   thisEvent = event
   thisEvent.ID = self._drawID
   thisEvent.HANDLER = Obj_New()
   thisEvent.EVENT_NAME='RUBBERBANDBOX_EVENT'
   thisEvent.NAME = self._name

   thisEvent = Create_Struct(thisEvent, 'ACTION', 'ACCEPT')

   ; Convert the return coordinates, if you can.
   self._drawID -> GetProperty, Coord_Object=coords
   IF Obj_Valid(coords) THEN BEGIN
      coords -> Draw
      c = Convert_Coord([self.box[0], self.box[2]], [self.box[1], self.box[3]], /Device, /To_Data)
      box = [c[0,0], c[1,0], c[0,1], c[1,1]]
   ENDIF ELSE box = self.box

   ; Add coordinates to the event structure and send the event.
   xpts = [box[0], box[0], box[2], box[2], box[0]]
   ypts = [box[1], box[3], box[3], box[1], box[1]]
   thisEvent = Create_Struct(thisEvent, 'BOX', box, 'XPTS', xpts, 'YPTS', ypts)

   ; Restore the display.
   self -> RestoreDisplay

   ; Send an event to the real draw widget event handler.
   self -> SendEvent, thisEvent

   IF Obj_Valid(self) THEN self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       RUBBERBANDBOX::CANCELROI
;
; PURPOSE:
;
;       This method cancels the box on the display and allows the user to start over.
;
; SYNTAX:
;
;       theObject -> CancelROI, event
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
PRO RubberbandBox::CancelROI, event

   @cat_pro_error_handler

   self._drawID -> SetWindow
   self._drawID_pixmap -> Copy

   ; Create the event structure.
   thisEvent = event
   thisEvent.ID = self._drawID
   thisEvent.HANDLER = Obj_New()
   thisEvent.EVENT_NAME='RUBBERBANDBOX_EVENT'
   thisEvent.NAME = self._name

   thisEvent = Create_Struct(thisEvent, 'ACTION', 'CANCEL')

   ; Send an event to the real draw widget event handler.
   self -> SendEvent, thisEvent

   IF Obj_Valid(self) THEN self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       RUBBERBANDBOX::DRAW
;
; PURPOSE:
;
;       This method may or may not be needed by your object, depending
;       upon whether a graphical representation of the object is required.
;       If you wish the DRAW method to automatically propogates down to any
;       objects contained in this object's container, call the superclass DRAW
;       method.
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
PRO RubberbandBox::Draw, _EXTRA=extra

   @cat_pro_error_handler

  ; First, erase the last box.
  self._drawID -> SetWindow
  self._drawID_pixmap -> Copy

   PlotS, [self.box[0], self.box[0], self.box[2], self.box[2], self.box[0]], $
          [self.box[1], self.box[3], self.box[3], self.box[1], self.box[1]], $
          /Device, Color=FSC_Color(self._roi_color), Linestyle=self.linestyle, $
          Thick=self.thick

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;        RUBBERBANDBOX::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the RUBBERBANDBOX object. It will typically
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
PRO RubberbandBox::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Pick the possible button events out of the more like draw widget events.
   CASE event.name OF

      'ACCEPT': $
         BEGIN
            self -> AcceptROI, event
            IF Obj_Valid(self._selectedObject) THEN BEGIN
               self._drawID -> Remove, self._selectedObject
               Obj_Destroy, self._selectedObject
            ENDIF
            RETURN

         END

      'CANCEL': $
         BEGIN
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

   CASE self._mode OF

      'MOVE': $
         BEGIN

            ; What kind of event is this?
            eventTypes = ['DOWN', 'UP', 'MOTION']
            thisEvent = eventTypes[event.type]

            CASE thisEvent OF

              'UP': BEGIN

                 self._drawID -> SetProperty, Motion_Events=0, /Clear_Events
                 self._mode = 'DRAW'

                 ; First, erase the last box.
                 self._drawID -> SetWindow
                 self._drawID_pixmap -> Copy

                 ; Draw the object.
                 self._selectedObject -> Draw

                  ; Flush the graphics buffer because the context menu happens too quickly.
                  Empty

                 ; Update the points in the box.
                 self._selectedObject -> GetProperty, X1=x1, X2=x2, Y1=y1, Y2=y2
                 self.box = [x1, y1, x2, y2]

                 ; Need a dialog?
                 IF self._ask_on_up THEN BEGIN

                     ; Call context display menu.
                     Widget_DisplayContextMenu, event.id -> GetID(), event.x+20 , event.y+20, self._contextMenu->GetID()

                 ENDIF

                 END

              'MOTION': BEGIN

                    IF Obj_Valid(self._selectedObject) THEN BEGIN

                        ; First, erase the last box.

                        self._selectedObject -> CopyParameters, self._drawID, Destination=d, Extent=e
                        self._drawID_pixmap -> Copy, Destination=d, Extent=e,  Origin=d

                        ; How much have we moved?
                        deltaX = event.x - self._click_x
                        deltaY = event.y - self._click_y

                        ; Move the object and draw it.
                        self._selectedObject -> Move, deltaX, deltaY, /NoDraw

                        self._selectedObject -> Draw

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
            self._click_x = event.x
            self._click_y = event.y
            self._drawID -> SetProperty, MOTION_EVENTS=1
            self._selectedObject = objects[0]
            RETURN
         ENDIF ELSE BEGIN
            ; Find the previous BOX object and remove it.
            box = self._drawID -> Get('SELECTABLE_RUBBERBANDBOX', Count=foundit)
            IF foundit THEN Obj_Destroy, box
            self._drawID -> Draw
         ENDELSE

         ; What kind of event is this?
         eventTypes = ['DOWN', 'UP', 'MOTION']
         thisEvent = eventTypes[event.type]

         CASE thisEvent OF

            'DOWN':BEGIN ; Button DOWN event.

               ; Copy the display window into the pixmap
               IF Obj_Valid(self._drawID_Pixmap) EQ 0 THEN BEGIN
                  self._drawID -> GetProperty, XSize=xsize, YSize=ysize
                  self._drawID_Pixmap = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)
               ENDIF
               self._drawID_Pixmap -> SetWindow
               self._drawID -> Copy

               ; Turn motion events on for the draw widget.
               self._drawID -> SetProperty, Motion_Events=1

               ; Get and store the static corner of the box.
               self._click_x = event.x
               self._click_y = event.y

               END

            'UP':BEGIN ; Button UP event.

               ; Make sure this is not just an up and down click.
               IF (self._click_x EQ event.x) AND (self._click_y EQ event.y) THEN BEGIN
                  self._drawID -> SetProperty, Motion_Events=0
                  self._drawID -> SetProperty, /Clear_Events
                  RETURN
               END

               ; Turn draw motion events off. Clear any events queued for widget.
               self._drawID -> SetProperty, Motion_Events=0, Clear_Events=1

               ; Order the box coordinates.
               sx = Min([self._click_x, event.x], Max=dx)
               sy = Min([self._click_y, event.y], Max=dy)
               self.box = [sx, sy, dx, dy]
               self -> GetProperty, Color=color, Linestyle=linestyle, Thick=thick
               self._drawID -> GetProperty, XSize=xsize, YSize=ysize
               x1 = self.box[0]
               y1 = self.box[1]
               x2 = self.box[2]
               y2 = self.box[3]

               boxObj = Obj_New('Box', $
                  Color=color, $
                  Coord_Object=Obj_New('CatCoord', XRange=[0,xsize], YRange=[0,ysize], Position=[0,0,1,1]), $
                  Linestyle=linestyle, $
                  Name='SELECTABLE_RUBBERBANDBOX', $
                  Thick=thick, $
                  X1=x1, Y1=y1, X2=x2, Y2=y2)

               IF Obj_Valid(self._selectedObject) THEN BEGIN
                  self._drawID -> Remove, self._selectedObject
                  Obj_Destroy, self._selectedObject
               ENDIF

               ; First, erase the last box.
               self._drawID -> Add, boxObj
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy

               boxObj -> Draw
               self._selectedObject = boxObj

               ; Flush the graphics buffer because the context menu happens too quickly.
               Empty

               ; Need a dialog?
               IF self._ask_on_up THEN BEGIN

                  ; Call context display menu.
                  x = Max([self.box[0], self.box[2]]) + 10
                  y = Min([self.box[1], self.box[3]])
                  Widget_DisplayContextMenu, event.id -> GetID(), x, y, self._contextMenu->GetID()

               ENDIF ELSE BEGIN

                  self -> AcceptROI, event
                  Obj_Destroy, self._selectedObject
                  RETURN

               ENDELSE


               END ; UP event


            'MOTION': BEGIN

               ; Get the coodinates of the new box and draw it.

               sx = self._click_x
               sy = self._click_y
               dx = event.x
               dy = event.y
               self.box = [sx, sy, dx, dy]
               self -> Draw

               END ; MOTION event

            ENDCASE ; of event.type

        END ; of DRAW

   ENDCASE


   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       RUBBERBANDBOX::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain RUBBERBANDBOX properties. Be sure
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
;     BOX:            A four-elements array of the form [x1,y1,x2,y2] representing the two
;                     diagonal corners of a box in the coordinate system of the window used
;                     to draw the box.
;
;     DEVICE:         If this keyword is set, the BOX coordinates are always returned in device coordinates.
;
;     LINESTYLE:      The line style index. Value from 0 to 5, as for PLOT command. Default, 0, solid line.
;
;     THICK:          The thickness of the line used to draw the ROI. Be default, 2.
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO RubberbandBox::GetProperty, $
   BOX=box, $
   DEVICE=device, $
   LINESTYLE=linestyle, $
   THICK=thick, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(box) THEN BEGIN
      IF Keyword_Set(device) THEN BEGIN
         box = self.box
      ENDIF ELSE BEGIN
         self._drawID -> GetProperty, Coord_Object=coords
         IF Obj_Valid(coords) THEN BEGIN
            thisWindow = !D.Window
            self._drawID -> SetWindow
            coords -> Draw
            c = Convert_Coord([self.box[0], self.box[2]], [self.box[1], self.box[3]], /Device, /To_Data)
            box = [c[0,0], c[1,0], c[0,1], c[1,1]]
            IF thisWindow GE 0 THEN WSet, thisWindow
         ENDIF ELSE box = self.box
      ENDELSE
   ENDIF

   IF Arg_Present(linestyle) THEN linestyle = self.linestyle
   IF Arg_Present(thick) THEN thick = self.thick

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> INTERACTION::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       RUBBERBANDBOX::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the RUBBERBANDBOX object's properties. Be sure
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
;     LINESTYLE:    The line style index. Value from 0 to 5, as for PLOT command. Default, 0, solid line.
;
;     THICK:        The thickness of the line used to draw the ROI. Be default, 2.
;
;     _EXTRA:       Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
PRO RubberbandBox::SetProperty, $
   LINESTYLE=linestyle, $
   THICK=thick, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(linestyle) NE 0 THEN self.linestyle = 0 > linestyle < 5
   IF N_Elements(thick) NE 0 THEN self.thick = thick

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> INTERACTION::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       RUBBERBANDBOX::CLEANUP
;
; PURPOSE:
;
;       This is the RUBBERBANDBOX object class destructor method.
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
PRO RubberbandBox::CLEANUP

   @cat_pro_error_handler

   self -> INTERACTION::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       RUBBERBANDBOX::INIT
;
; PURPOSE:
;
;       This is the RUBBERBANDBOX object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     drawObject:   The draw widget object that you will be taking over events from.
;
; KEYWORDS:
;
;     LINESTYLE:    The line style index. Value from 0 to 5, as for PLOT command. Default, 0, solid line.
;
;     THICK:        The thickness of the line used to draw the ROI. Be default, 2.
;
;     _EXTRA:       Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION RubberbandBox::INIT, drawObject, $
   LINESTYLE=linestyle, $
   THICK=thick, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   ok = self -> INTERACTION::INIT (drawObject, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   IF N_Elements(linestyle) EQ 0 THEN linestyle=0
   IF N_Elements(thick) EQ 0 THEN thick = 2

   self.linestyle = 0 > linestyle < 5
   self.thick = thick
   self._mode = 'DRAW'

   ; Finished.
   self -> Report, /Completed

   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       RUBBERBANDBOX CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the RUBBERBANDBOX object.
;
;*****************************************************************************************************
PRO RubberbandBox__DEFINE, class

   class = { RUBBERBANDBOX, $
             box: FltArr(4), $            ; The corners of the box, once drawn.
             linestyle: 0L, $             ; The linestyle of the line drawing the box.
             thick: 0L, $                 ; The thickness of the line drawing the box.
             INHERITS INTERACTION $
           }

END


