;*****************************************************************************************************
;+
; NAME:
;       CONTRASTBOX__DEFINE
;
; PURPOSE:
;
;       A contrastbox object is a moveable, rubberband-type box interaction
;       in which only the portion of the window inside the box is shown in
;       full color. The portion of the window outside the box is shown in
;       muted colors. The colors are muted by reducing the saturation by 50%.
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
;       theObject = Obj_New("CONTRASTBOX")
;
; SUPERCLASSES:
;
;       MOVEABLEBOX
;       RUBBERBANDBOX
;       INTERACTION
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { CONTRASTBOX, $
;             contrastID: Obj_New(), $          ; The contrast window.
;             INHERITS MOVEABLEBOX $
;           }
;
; MESSAGES:
;
;       None.
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
;       CONTRASTBOX::ACCEPTBOX
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
;*****************************************************************************************************
PRO ContrastBox::AcceptBox, event

   @cat_pro_error_handler

   ; Erase the last box drawn.
   self._drawID -> SetWindow
   self._drawID -> SetProperty, Motion_Events=0, /Clear_Events
   self.contrastID -> Copy

   ; Clean up.
   self -> RestoreDisplay
   self -> Update_ModeMap, /Clear

   ; Create the event structure.
   thisEvent = event
   thisEvent.ID = self._drawID
   thisEvent.HANDLER = Obj_New()
   thisEvent.EVENT_NAME='CONTRASTBOX_EVENT'
   thisEvent.NAME = self._name

   ; Convert the return coordinates, if you can.
   self._drawID -> GetProperty, Coord_Object=coords
   IF Obj_Valid(coords) THEN BEGIN
      coords -> Draw
      c = Convert_Coord([self.box[0], self.box[2]], [self.box[1], self.box[3]], /Device, /To_Data)
      box = [c[0,0], c[1,0], c[0,1], c[1,1]]
   ENDIF ELSE box = *self.box

   ; Add coordinates to the event structure and send the event.
   xpts = [box[0], box[0], box[2], box[2], box[0]]
   ypts = [box[1], box[3], box[3], box[1], box[1]]
   thisEvent = Create_Struct(thisEvent, 'BOX', box, 'XPTS', xpts, 'YPTS', ypts)

   ; Send an event to the real draw widget event handler.
   self -> SendEvent, thisEvent

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CONTRASTBOX::CANCELBOX
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
PRO ContrastBox::CancelBox, event

   @cat_pro_error_handler

   self._drawID -> SetWindow
   self.contrastID -> Copy
   self._drawID -> SetProperty, Motion_Events=0, /Clear_Events
   self -> Update_ModeMap, /Clear

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CONTRASTBOX::DRAW
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
PRO ContrastBox::Draw, _Extra=extra

   @cat_pro_error_handler

   self.bufferID -> SetWindow
   self._drawID_pixmap -> Copy

   UserSym, Cos(self.phi), Sin(self.phi), /Fill
   self -> GetProperty, Box=box, /Device
   sx = Min([self.box[0], self.box[2]], Max=dx)
   sy = Min([self.box[1], self.box[3]], Max=dy)
   box = [sx, sy, dx, dy]
   x1 = box[0]
   y1 = box[1]
   x2 = box[2]
   y2 = box[3]
   midy = (Abs(y2 - y1) / 2) + Min([y1,y2])
   midx = (Abs(x2 - x1) / 2) + Min([x1,x2])

   self.contrastID -> Copy, Destination=[x1, y1], Origin=[x1,y1], Extent=[Abs(x2-x1)+1, Abs(y2-y1)+1]
   PLOTS, [self.box[0], self.box[0], self.box[2], self.box[2], self.box[0]], $
       [self.box[1], self.box[3], self.box[3], self.box[1], self.box[1]], $
       /Device, Color=FSC_Color(self._roi_color)
   PLOTS, x1, y1, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, x1, y2, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, x2, y1, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, x2, y2, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, x1, midy, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, x2, midy, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, midx, y1, PSYM=8, Color=FSC_Color(self.handle_color), /Device
   PLOTS, midx, y2, PSYM=8, Color=FSC_Color(self.handle_color), /Device

   self._drawID -> SetWindow
   self.bufferID -> Copy

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       CONTRASTBOX::REFRESHPIXMAP
;
; PURPOSE:
;
;       This method refreshes the pixmap (the original display might have changed in some way) and
;       (optionally) calls the DRAW method.
;
; SYNTAX:
;
;       theObject -> RefreshPixmap
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DRAW:       If this keyword is set, the DRAW method is called..
;-
;*****************************************************************************************************
PRO ContrastBox::RefreshPixmap, DRAW=draw

   @cat_pro_error_handler

   ; Make sure you can do something here.
   IF Obj_Valid(self._drawID) EQ 0 THEN RETURN

   ; Make sure there is a pixmap to refresh.
   IF Obj_Valid(self.contrastID) EQ 0 THEN RETURN

   self.contrastID -> SetWindow
   self._drawID -> Copy

   IF Keyword_Set(draw) THEN self -> Draw

END



;*****************************************************************************************************
;+
; NAME:
;       CONTRASTBOX::RESTOREDISPLAY
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
PRO ContrastBox::RestoreDisplay

   @cat_pro_error_handler

   ; Remove the light image and destroy it.
   IF Obj_Valid(self._drawID_Pixmap) THEN BEGIN
      oldimage = self._drawID_pixmap -> Get('PIXMAP_IMAGE', Count=count)
      IF count GT 0 THEN Obj_Destroy, oldimage
   ENDIF

   ; Copy the contents of the contrast pixmap into the interaction's pixmap.
   self._drawID_pixmap -> SetWindow
   self.contrastID -> Copy
   Obj_Destroy, self.contrastID

   ; Call Superclass RestoreDisplay.
   self -> MOVEABLEBOX::RestoreDisplay

   ; Delete the contrast object.
   Obj_Destroy, self.contrastID

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CONTRASTBOX::SETDISPLAY
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
PRO ContrastBox::SetDisplay

   @cat_pro_error_handler

   ; Determine how the draw widget is set up:
   self._drawID -> GetProperty, Tracking_Events=tevents, Button_Events=bevents, $
      Expose_Events=eevents, Keyboard_Events=kevents, Motion_Events=mevents, $
      Viewport_Events=vevents, Context_Events=cevents, Exclusive_Event_Object=excl_event_obj, $
      XSize=xsize, YSize=ysize, Event_Objects=event_objects

   ; Store draw window information.
   self._drawID_events = [tevents, bevents, eevents, kevents, mevents, vevents, cevents]
   self._drawID_excl_event_obj = excl_event_obj

   ; You are only going to get the event objects once, the first time through
   ; SetDisplay. This will (hopefully) prevent unwanted consequences. The pointer
   ; is freed on every trip though RestoreDisplay.
   IF N_Elements(event_objects) NE 0 THEN BEGIN
      IF Ptr_Valid(self._drawID_event_objects) EQ 0 THEN self._drawID_event_objects = Ptr_New(event_objects) ELSE $
         *self._drawID_event_objects = event_objects
   ENDIF

   ; Get the size of the display window.
   self._drawID -> GetProperty, XSize=xsize, YSize=ysize

   ; Make pixmap objects for the program, if needed.
   IF Obj_Valid(self._drawID_pixmap) EQ 0 THEN $
      self._drawID_pixmap = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)
   IF Obj_Valid(self.contrastID) EQ 0 THEN $
      self.contrastID = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)
   IF Obj_Valid(self.bufferID) EQ 0 THEN $
      self.bufferID = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)

   ; Make a a light image for display in the pixmap.
   self._drawID -> SetWindow
   image = TVRead(True=3)

   image24 = BytArr(xsize, ysize, 3)
   Color_Convert, Reform(image[*,*,0]), Reform(image[*,*,1]), Reform(image[*,*,2]), hue, light, sat, /RGB_HLS
   light = Scale_Vector(light, 0.5, 1.0)
   Color_Convert, hue, light, sat, r, g, b, /HLS_RGB
   image24[*,*,0] = r
   image24[*,*,1] = g
   image24[*,*,2] = b
   oldimage = self._drawID_pixmap -> Get('PIXMAP_IMAGE', Count=count)
   IF count GT 0 THEN Obj_Destroy, oldimage
   self._drawID_pixmap -> Add, Obj_New('CatTrueColorImage', image24, /No_Copy, Name='PIXMAP_IMAGE')

   ; Copy the display window into the contrast pixmap.
   self.contrastID -> SetWindow
   self._drawID -> Copy

   ; Set up the window for your own purposes.
   self._drawID -> SetProperty, Tracking_Events=0, Button_Events=1, $
      Expose_Events=0, Keyboard_Events=0, Motion_Events=0, $
      Viewport_Events=0, Exclusive_Event_Object=self

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       CONTRASTBOX::CLEANUP
;
; PURPOSE:
;
;       This is the CONTRASTBOX object class destructor method.
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
PRO ContrastBox::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self.contrastID
   Obj_Destroy, self.bufferID

   self -> MOVEABLEBOX::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CONTRASTBOX::INIT
;
; PURPOSE:
;
;       This is the CONTRASTBOX object class initialization method
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
;     _EXTRA:       Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION ContrastBox::INIT, drawObject, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   ok = self -> MOVEABLEBOX::INIT (drawObject, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       CONTRASTBOX CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the CONTRASTBOX object.
;
;*****************************************************************************************************
PRO ContrastBox__DEFINE, class

   class = { CONTRASTBOX, $
             contrastID: Obj_New(), $          ; The contrast window.
             INHERITS MOVEABLEBOX $
           }

END


;*****************************************************************************************************
;
; NAME:
;       CONTRASTBOX TEST PROGRAM
;
; PURPOSE:
;
;       This is the test program for the CONTRASTBOX object. It may not
;       be required in your object.
;
;*****************************************************************************************************
PRO ContrastBox_Test, roi

   LoadCT, 0, /Silent
   tlb = Obj_New('TOPLEVELBASE')
   drawID = Obj_New('DrawWidget', tlb, XSize=400, ysize=400)
   drawID -> Add, Obj_New('catimage', loaddata(7), position=[0.1, 0.1, 0.9, 0.9])

   tlb -> Draw, /Center
   roi = Obj_New('CONTRASTBOX', drawID)
   roi -> SetDisplay
END