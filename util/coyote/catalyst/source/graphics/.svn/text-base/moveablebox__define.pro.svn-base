;*****************************************************************************************************
;+
; NAME:
;       MOVEABLEBOX__DEFINE
;
; PURPOSE:
;
;       Implements a rubberband-style box ROI that can be selected and moved
;       about the window.
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
;       theObject = Obj_New("MOVEABLEBOX")
;
; SUPERCLASSES:
;
;       RUBBERBANDBOX
;       INTERACTION
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { MOVEABLEBOX, $
;             phi: FltArr(33), $             ; Coordinates of a circle.
;             handle_color: "", $            ; The color of the box handles.
;             modemap: Obj_New(), $          ; A pixmap for calculating "mode".
;             currentmode: 0L, $             ; The current mode.
;             sx: 0L, $                      ; The static X corner of the box.
;             sy: 0L, $                      ; The static Y corner of the box.
;             bufferID: Obj_New(), $         ; A buffer pixmap object for smooth graphics display.
;             INHERITS RUBBERBANDBOX $
;           }
;
; MESSAGES:
;
;   None.
;
; EVENT_STRUCTURE:
;
;       event = { ID:theObject, TOP:topObject, HANDLER:Obj_New(), EVENT_NAME='RUBBERBANDBOX_EVENT', $
;                  NAME: self._name, BOX:self.box, XPTS:FLTARR(5), YPTS:FLATARR(5) }
;
;       In which the XPTS and YPTS fields contain the X and Y locations of the box in a form
;       suitable for drawing the ROI on the display with PLOTS. The BOX, XPTS, and YPTS are
;       converted to the coordinate system of the draw widget associated with the interaction
;       before being placed in the event structure.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 25 October 2004.
;-
;*****************************************************************************************************
;+
; NAME:
;       MOVEABLEBOX::ACCEPTBOX
;
; PURPOSE:
;
;       This method accepts the box on the display and sends an event to the client.
;
; SYNTAX:
;
;       theObject -> AcceptBox, event
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
PRO MoveableBox::AcceptBox, event

   @cat_pro_error_handler

   ; Erase the last box drawn. Destroy the pixmap.
   self._drawID -> SetProperty, Motion_Events=0, /Clear_Events
   self._drawID -> SetWindow
   self._drawID_pixmap -> Copy

   ; Create the event structure.
   thisEvent = event
   thisEvent.ID = self._drawID
   thisEvent.HANDLER = Obj_New()
   thisEvent.EVENT_NAME='MOVEABLEBOX_EVENT'
   thisEvent.NAME = self._name

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

   ; Send an event to the real draw widget event handler.
   self -> SendEvent, thisEvent

   ; Clean up.
   self -> RestoreDisplay
   self -> Update_ModeMap, /Clear

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       MOVEABLEBOX::CANCELBOX
;
; PURPOSE:
;
;       This method cancels the box on the display and allows the user to start over
; SYNTAX:
;
;       theObject -> CancelBox, event
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
PRO MoveableBox::CancelBox, event

   @cat_pro_error_handler

   self._drawID -> SetWindow
   self._drawID_pixmap -> Copy
   self._drawID -> SetProperty, Motion_Events=0, /Clear_Events
   self -> Update_ModeMap, /Clear

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       MOVEABLEBOX::DRAW
;
; PURPOSE:
;
;       This method draws the interaction in the display window.
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
PRO MoveableBox::Draw, _EXTRA=extra

   @cat_pro_error_handler

   ; Draw into the buffer pixmap
   self.bufferID -> SetWindow
   self._drawID_pixmap -> Copy

   ; Draw the box.
   PlotS, [self.box[0], self.box[0], self.box[2], self.box[2], self.box[0]], $
       [self.box[1], self.box[3], self.box[3], self.box[1], self.box[1]], $
       /Device, Color=FSC_Color(self._roi_color)

   ; Draw the handles on the box.
   UserSym, Cos(self.phi), Sin(self.phi), /Fill
   self -> GetProperty, Box=box, /Device

   x1 = box[0]
   y1 = box[1]
   x2 = box[2]
   y2 = box[3]
   midy = (Abs(y2 - y1) / 2) + Min([y1,y2])
   midx = (Abs(x2 - x1) / 2) + Min([x1,x2])

   PLOTS, x1, y1, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, x1, y2, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, x2, y1, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, x2, y2, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, x1, midy, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, x2, midy, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, midx, y1, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, midx, y2, PSYM=8, Color=FSC_Color(self.handle_color), /Device

   ; Transfer buffer to the display
   self._drawID -> SetWindow
   self.bufferID -> Copy

   self -> Report, /Completed

END

;*****************************************************************************************************
;+
; NAME:
;        MOVEABLEBOX::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the MOVEABLEBOX object.
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
PRO MoveableBox::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Pick the possible context menu button events out of the more like draw widget events.
   CASE event.name OF

      'ACCEPT': BEGIN
            self -> AcceptBox, event
            RETURN
         END

      'CANCEL': BEGIN
            self -> CancelBox, event
            RETURN
         END

      ELSE: ; From draw widget. Fall through.

   ENDCASE

   ; Deal only with DOWN, UP, and MOTION events.
   IF event.type GT 2 THEN RETURN

   ; What kind of event is this?
   eventTypes = ['DOWN', 'UP', 'MOTION']
   thisEvent = eventTypes[event.type]

   CASE thisEvent OF

      'DOWN': $
         BEGIN

            ; What mode are you in?
            theMode = self -> GetMode(event.x, event.y, /Set)

            ; Turn motion events on for the draw widget.
            self._drawID -> SetProperty, Motion_Events=1

            ; Get and store the down location of cursor.
            self._click_x = event.x
            self._click_y = event.y

            CASE theMode OF

               1: BEGIN ; Lower-left corner handle.
                     self.sx = self.box[2]
                     self.sy = self.box[3]
                  END

               2: BEGIN ; Upper-left corner handle.
                     self.sx = self.box[2]
                     self.sy = self.box[1]
                  END

               3: BEGIN ; Lower-right corner handle.
                     self.sx = self.box[0]
                     self.sy = self.box[3]
                  END

               4: BEGIN ; Upper-right corner handle.
                     self.sx = self.box[0]
                     self.sy = self.box[1]
                  END

               5: BEGIN ; Left side handle.
                  END

               6: BEGIN ; Right side handle.
                  END

               7: BEGIN ; Bottom handle.
                  END

               8: BEGIN ; Top handle.
                  END

               9: BEGIN ; Inside box.
                  self.sx = Round((self.box[2] - self.box[0]) / 2) ; Half of x box length.
                  self.sy = Round((self.box[3] - self.box[1]) / 2) ; Half of y box length.
                  END

               ELSE: BEGIN

                     ; If right button, then we need context menu.
                     IF event.press EQ 4 THEN $
                        BEGIN
                           IF Total(self.box) EQ 0 THEN RETURN
                           Widget_DisplayContextMenu, event.id -> GetID(), event.x, event.y, self._contextMenu->GetID()
                           RETURN
                        END

                     self.sx = event.x
                     self.sy = event.y
                  END

         ENDCASE
         END ; of thisEvent->DOWN case

      'UP': $
         BEGIN

         ; Turn draw motion events off. Clear any events queued for widget.
         self._drawID -> SetProperty, Motion_Events=0, Clear_Events=1

         ; Make sure this is not just an up and down click.
         IF self.currentMode NE 9 THEN IF (self._click_x EQ event.x) AND (self._click_y EQ event.y) THEN RETURN

         ; First, erase the last box.
         self._drawID -> SetWindow
         ;self._drawID_pixmap -> Copy

         ; Order the box coordinates.
         sx = Min([self.box[0], self.box[2]], Max=dx)
         sy = Min([self.box[1], self.box[3]], Max=dy)
         self.box = [sx, sy, dx, dy]
         self -> Draw
         self -> Update_ModeMap

         ; Calculate new box centers.
         self.sx = Round((self.box[2] - self.box[0]) / 2)
         self.sy = Round((self.box[3] - self.box[1]) / 2)

         END

      'MOTION': $
         BEGIN

         ; Here is where the actual box is drawn and erased. First, erase the last box.
         self._drawID -> SetWindow
         ;self._drawID_pixmap -> Copy

         ; What size is the window? Keep the cursor inside it.
         self._drawID -> GetProperty, XSize=xsize, YSize=ysize
         xloc = 0 > event.x < (xsize-1)
         yloc = 0 > event.y < (ysize-1)

         CASE self.currentMode OF

            0: BEGIN ; No particular mode or outside the box.

               ; Get the coodinates of the new box and draw it.
               self.box = [self.sx, self.sy, xloc, yloc]
               self -> Draw

               END

            1: BEGIN ; Lower-left corner handle.

               ; Get the coodinates of the new box and draw it.
               self.box = [self.sx, self.sy, xloc, yloc]
               self -> Draw

               END

            2: BEGIN ; Upper-left corner handle.

               ; Get the coodinates of the new box and draw it.
               self.box = [self.sx, self.sy, xloc, yloc]
               self -> Draw

               END

            3: BEGIN ; Lower-right corner handle.

               ; Get the coodinates of the new box and draw it.
               self.box = [self.sx, self.sy, xloc, yloc]
               self -> Draw

               END

            4: BEGIN ; Upper-right corner handle.

               ; Get the coodinates of the new box and draw it.
               self.box = [self.sx, self.sy, xloc, yloc]
               self -> Draw

               END

            5: BEGIN ; Left side handle.
               self.box = [xloc, self.box[1], self.box[2], self.box[3]]
               self -> Draw
               END

            6: BEGIN ; Right side handle.
               self.box = [self.box[0], self.box[1], xloc, self.box[3]]
               self -> Draw
               END

            7: BEGIN ; Bottom handle
               self.box = [self.box[0], yloc, self.box[2], self.box[3]]
               self -> Draw
               END

            8: BEGIN ; Top handle.
               self.box = [self.box[0], self.box[1], self.box[2], yloc]
               self -> Draw
               END

            9: BEGIN ; Inside box.
               xloc = self.sx > event.x < (xsize - self.sx)
               yloc = self.sy > event.y < (ysize - self.sy)

               delta_x = event.x - self._click_x
               delta_y = event.y - self._click_y
               self.box = [ self.box[0] + delta_x, $
                            self.box[1] + delta_y, $
                            self.box[2] + delta_x, $
                            self.box[3] + delta_y ]

               ; Is the box still inside the window? Check and fix, if needed.
               needs_fix = 0
               IF self.box[0] LT 0 THEN BEGIN
                  self.box[2] = self.box[2] + Abs(self.box[0])
                  self.box[0] = 0
                  needs_fix = 1
               ENDIF
               IF self.box[2] GT (xsize - 1) THEN BEGIN
                  self.box[0] = self.box[0] - Abs(self.box[2] - xsize + 1)
                  self.box[2] = xsize-1
                  needs_fix = 1
               ENDIF
               IF self.box[1] LT 0 THEN BEGIN
                  self.box[3] = self.box[3] + Abs(self.box[1])
                  self.box[1] = 0
                  needs_fix = 1
               ENDIF
               IF self.box[3] GT (ysize - 1) THEN BEGIN
                  self.box[1] = self.box[1] - Abs(self.box[3] - ysize + 1)
                  self.box[3] = ysize-1
                  needs_fix = 1
               ENDIF
               IF needs_fix THEN BEGIN

                  ; Calculate new box centers.
                  self.sx = Round((self.box[2] - self.box[0]) / 2)
                  self.sy = Round((self.box[3] - self.box[1]) / 2)

                  ; Clear any events that queued up behind you.
                  self._drawID -> SetProperty, /Clear_Events
               ENDIF
               self._click_x = event.x
               self._click_y = event.y
               self -> Draw
               END


            255: BEGIN ; No particular mode or outside the box.

               ; Get the coodinates of the new box and draw it.
               self.box = [self.sx, self.sy, xloc, yloc]
               self -> Draw

               END

            ELSE: Print, 'Unknown mode: ', self.currentMode

         ENDCASE

         END

   ENDCASE

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       MOVEABLEBOX::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain MOVEABLEBOX properties. Be sure
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
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO MoveableBox::GetProperty, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> RUBBERBANDBOX::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       MOVEABLEBOX::GETMODE
;
; PURPOSE:
;
;       This method gets the current mode for the draw widget.
;
; SYNTAX:
;
;       theMode = self -> GetMode(x, y)
;
; ARGUMENTS:
;
;       x:    The x location for obtaining the mode.
;
;       y:    The y location for obtaining the mode.
;
; KEYWORDS:
;
;      SET;   If this keyword is set, the currentMode flag is set to this mode.
;-
;*****************************************************************************************************
FUNCTION MoveableBox::GetMode, x, y, SET=set


   @cat_func_error_handler

   ; Take a snapshot of the modemap.
   self.modemap -> SetWindow
   map = TVRD()

   theMode = map[x,y]
   IF Keyword_Set(set) THEN self.currentMode = theMode

   self -> Report, /Completed
   RETURN, theMode

END



;*****************************************************************************************************
;+
; NAME:
;       MOVEABLEBOX::RESTOREDISPLAY
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
PRO MoveableBox::RestoreDisplay

   @cat_pro_error_handler

   ; Call Superclass RestoreDisplay.
   self -> RUBBERBANDBOX::RestoreDisplay

   ; Delete the buffer object.
   Obj_Destroy, self.bufferID

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       MOVEABLEBOX::SETDISPLAY
;
; PURPOSE:
;
;       This method ...
;
; SYNTAX:
;
;       theObject -> SetDisplay
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
PRO MoveableBox::SetDisplay

   @cat_pro_error_handler

   ; Call Superclass RestoreDisplay.
   self -> RUBBERBANDBOX::SetDisplay

   IF Obj_Valid(self.bufferID) EQ 0 THEN $
      self.bufferID = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       MOVEABLEBOX::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the MOVEABLEBOX object's properties. Be sure
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
;     _EXTRA:       Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO MoveableBox::SetProperty, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> RUBBERBANDBOX::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       MOVEABLEBOX::UPDATE_MODEMAP
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
PRO MoveableBox::Update_Modemap, CLEAR=clear

   @cat_pro_error_handler

   IF Keyword_Set(clear) THEN BEGIN
      self.modemap -> Refresh
      RETURN
   ENDIF

   self.modemap -> Refresh

   UserSym, Cos(self.phi), Sin(self.phi), /Fill
   self -> GetProperty, Box=box, /Device

   x1 = box[0]
   y1 = box[1]
   x2 = box[2]
   y2 = box[3]
   midy = (Abs(y2 - y1) / 2) + Min([y1,y2])
   midx = (Abs(x2 - x1) / 2) + Min([x1,x2])

   self.modemap -> SetWindow
   Device, Decomposed=0, Get_Decomposed=theState
   TVLCT, r, g, b, /Get
   LoadCT, 0, /Silent
   Erase, Color=0
   POLYFILL, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=9
   PLOTS, x1, y1, PSYM=8, Color=1, /Device, Symsize=1.5
   PLOTS, x1, y2, PSYM=8, Color=2, /Device, Symsize=1.5
   PLOTS, x2, y1, PSYM=8, Color=3, /Device, Symsize=1.5
   PLOTS, x2, y2, PSYM=8, Color=4, /Device, Symsize=1.5
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
;       MOVEABLEBOX::CLEANUP
;
; PURPOSE:
;
;       This is the MOVEABLEBOX object class destructor method.
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
PRO MoveableBox::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self.modemap

   self -> RUBBERBANDBOX::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       MOVEABLEBOX::INIT
;
; PURPOSE:
;
;       This is the MOVEABLEBOX object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     drawObject:    The draw widget object that you will be taking over events from.
;
; KEYWORDS:
;
;     BOX:           A four-element array containing the initial box coordinates in device
;                    coordinates.
;
;     HANDLE_COLOR:  The name of the color to draw the box handles in. By default, "sky blue".
;
;     _EXTRA:        Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION MoveableBox::INIT, drawObject, $
   BOX=box, $
   HANDLE_COLOR=handle_color, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   ok = self -> RUBBERBANDBOX::INIT (drawObject, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   phi = Findgen(32) * (!PI * 2 / 32.)
   self.phi = [ phi, phi[0] ]

   IF N_Elements(handle_color) EQ 0 THEN handle_color = 'Sky Blue'
   self.handle_color = handle_color

   self._drawID -> GetProperty, XSize=xsize, YSize=ysize
   self.modemap = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)

   IF N_Elements(box) NE 0 THEN self.box = box

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       MOVEABLEBOX CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the MOVEABLEBOX object.
;
;*****************************************************************************************************
PRO MoveableBox__DEFINE, class

   class = { MOVEABLEBOX, $
             phi: FltArr(33), $             ; Coordinates of a circle.
             handle_color: "", $            ; The color of the box handles.
             modemap: Obj_New(), $          ; A pixmap for calculating "mode".
             currentmode: 0L, $             ; The current mode.
             sx: 0L, $                      ; The static X corner of the box.
             sy: 0L, $                      ; The static Y corner of the box.
             bufferID: Obj_New(), $         ; A buffer pixmap object for smooth graphics display.
             INHERITS RUBBERBANDBOX $
           }

END


;*****************************************************************************************************
;
; NAME:
;       MOVEABLEBOX TEST PROGRAM
;
; PURPOSE:
;
;       This is the test program for the MOVEABLEBOX object. It may not
;       be required in your object.
;
;*****************************************************************************************************
PRO MoveableBox_Test

   LoadCT, 0, /Silent
   tlb = Obj_New('TOPLEVELBASE')
   drawID = Obj_New('DrawWidget', tlb, XSize=400, ysize=400)
   drawID -> Add, Obj_New('catimage2d', loaddata(7))

   tlb -> Draw, /Center
   roi = Obj_New('MOVEABLEBOX', drawID)
   roi -> SetDisplay
END