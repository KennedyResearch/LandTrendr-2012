;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE_ROI__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide an ellipse ROI interaction.
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
;       theObject = Obj_New("ELLIPSE_ROI")
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
;   class = { ELLIPSE_ROI, $
;             xpts: Ptr_New(), $        ; The X points in the ROI.
;             ypts: Ptr_New(), $        ; The X points in the ROI.
;             count: 0L, $              ; The number of points in the ROI.
;             bufferID: Obj_New(), $    ; A pixmap buffer for smooth graphics display.
;             INHERITS INTERACTION $
;           }
;
; MESSAGES:
;
;   None.
;
;
; EVENT_STRUCTURE:
;
;       event = { ID:theObject, TOP:topObject, HANDLER:Obj_New(), EVENT_NAME='ELLIPSE_ROI_EVENT', $
;                  NAME: self._name, XPTS:FLTARR(), YPTS:FLATARR(), COUNT:0L }
;
;       In which the XPTS and YPTS fields contain the X and Y locations of the polygon in a form
;       suitable for drawing the ROI on the display with PLOTS. The COUNT field contains the number
;       of points in the XPTS and YPTS vectors. The XPTS and YPTS are converted to the coordinate
;       system of the draw widget associated with the interaction before being placed in the event structure.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, December 27, 2004.
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
;       ELLIPSE_ROI::ACCEPTROI
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
PRO Ellipse_ROI::AcceptROI, event

   @cat_pro_error_handler

   ; Erase the last ROI drawn.
   self._drawID -> SetProperty, Motion_Events=0, /Clear_Events
   self._drawID -> SetWindow
   self._drawID_pixmap -> Copy

  ; Create the event structure.
  thisEvent = event
  thisEvent.ID = self._drawID
  thisEvent.HANDLER = Obj_New()
  thisEvent.EVENT_NAME='ELLIPSE_ROI_EVENT'
  thisEvent.NAME = self._name
  thisEvent.ACTION = 'ACCEPT'

   ; Convert the return coordinates, if you can.
   self._drawID -> GetProperty, Coord_Object=coords
   IF Obj_Valid(coords) THEN BEGIN
      coords -> Draw
      c = Convert_Coord(*self.xpts, *self.ypts, /Device, /To_Data)
      xpts = Reform(c[0,*])
      ypts = Reform(c[1,*])
   ENDIF ELSE BEGIN
      xpts = *self.xpts
      ypts = *self.ypts
   ENDELSE

   ; Add coordinates to the event structure and send the event.
  thisEvent = Create_Struct(thisEvent, 'xpts', xpts, 'ypts', ypts, 'count', self.count)

   ; Restore Display
   self -> RestoreDisplay

   ; Send an event to the real draw widget event handlers.
   self -> SendEvent, thisEvent

   ; Clean up, if you are still around. The event handler may
   ; have killed you.
   IF Obj_Valid(self) THEN BEGIN

      Ptr_Free, self.xpts
      Ptr_Free, self.ypts

      self -> Report, /Completed

   ENDIF

END



;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE_ROI::CANCELROI
;
; PURPOSE:
;
;       This method cancels the box on the display and allows the user to start over
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
PRO Ellipse_ROI::CancelROI, event

   @cat_pro_error_handler

   self._drawID -> SetWindow
   self._drawID_pixmap -> Copy
   self._drawID -> SetProperty, Motion_Events=0, /Clear_Events

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE_ROI::DRAW
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
PRO Ellipse_ROI::Draw, _EXTRA=extra


   @cat_pro_error_handler

   ; Have to have valid points to proceed.
   IF Ptr_Valid(self.xpts) EQ 0 THEN RETURN

  ; First, erase the last box.
  self.bufferID -> SetWindow
  self._drawID_pixmap -> Copy

   PlotS, (*self.xpts)[0:self.count-1], (*self.ypts)[0:self.count-1], /Device, $
      Color=FSC_Color(self._roi_color), Thick=self.thick, Linestyle=self.linestyle

   ; Copy buffer to display.
   self._drawID -> SetWindow
   self.bufferID -> Copy

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;        ELLIPSE_ROI::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the ELLIPSE_ROI object.
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
PRO Ellipse_ROI::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Pick the possible context menu button events out of the more like draw widget events.
   CASE event.name OF

      'ACCEPT': BEGIN
         self -> AcceptROI, event

         ; If the selectedObject still exists, remove it from
         ; the draw widget and destroy it.
         IF Obj_Valid(self._selectedObject) THEN BEGIN
            self._drawID -> Remove, self._selectedObject
            Obj_Destroy, self._selectedObject
         ENDIF

         RETURN
         END

      'CANCEL': BEGIN
         self -> CancelROI, event

         ; If the selectedObject still exists, remove it from
         ; the draw widget and destroy it.
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
              self._selectedObject -> CopyParameters, self.bufferID, Destination=d, Extent=e
              self._drawID_pixmap -> Copy, Destination=d, Extent=e,  Origin=d

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
                    self._selectedObject -> CopyParameters, self.bufferID, Destination=d, Extent=e
                    self._drawID_pixmap -> Copy, Destination=d, Extent=e,  Origin=d

                    ; Move the object and draw it.
                    self._selectedObject -> Move, deltaX, deltaY, /NoDraw
                    self.bufferID -> SetWindow
                    self._selectedObject -> Draw

                    ; Copy buffer to display.
                    self._drawID -> SetWindow
                    self.bufferID -> Copy

                    ; Update center of ellipse.
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
            ; Find the previous ELLIPSE object and remove it.
            ellipse = self._drawID -> Get('SELECTABLE_ELLIPSE_ROI', Count=foundit)
            IF foundit THEN Obj_Destroy, ellipse
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

               ; Set the center of the ellipse.
               self._click_x = event.x
               self._click_y = event.y

               ; Prepare points pointers.
               Ptr_Free, self.xpts
               Ptr_Free, self.ypts
               self.xpts = Ptr_New(/Allocate_Heap)
               self.ypts = Ptr_New(/Allocate_Heap)

               END

            'UP':BEGIN ; Button UP event.

               ; Motion events off. Clear all extra events.
               self._drawID -> SetProperty, Motion_Events=0, /Clear_Events

               ; Make sure this is not just an up and down click.
               IF (self._click_x EQ event.x) AND (self._click_y EQ event.y) THEN RETURN


               ; Have to have valid points to proceed.
               IF Ptr_Valid(self.xpts) EQ 0 THEN RETURN

               ; First, erase the last box.
               self.bufferID -> SetWindow
               self._drawID_pixmap -> Copy

               ; Create a selectable object.
               self -> GetProperty, Color=color, Linestyle=linestyle, Thick=thick, DrawWidget=drawID
               drawID -> GetProperty, XSize=xsize, YSize=ysize

               xradius = Abs(self._click_x - event.x)
               yradius = Abs(self._click_y - event.y)

               xcenter = self._click_x
               ycenter = self._click_y

               ; Relevant points are in device coordinates. Should be converted to
               ; normalized coordinates for moving and selecting.
               c = Convert_Coord([xcenter, xradius], [ycenter, yradius], /Device, /To_Normal)
               ellipseObj = Obj_New('Ellipse', $
                  Color=color, $
                  Coord_Object=Obj_New('CatCoord', XRange=[0,1], YRange=[0,1], Position=[0,0,1,1]), $
                  Linestyle=linestyle, $
                  Name='SELECTABLE_ELLIPSE_ROI', $
                  Thick=thick, $
                  npoints=120, $
                  XCenter=c[0,0], $
                  YCenter=c[1,0], $
                  XRadius=c[0,1], $
                  YRadius=c[1,1] )

               ; If the selectedObject still exists, remove it from
               ; the draw widget and destroy it.
               IF Obj_Valid(self._selectedObject) THEN BEGIN
                  self._drawID -> Remove, self._selectedObject
                  Obj_Destroy, self._selectedObject
               ENDIF

               drawID -> Add, ellipseObj
               self.bufferID -> SetWindow
               ellipseObj -> Draw
               self._selectedObject = ellipseObj

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

               ENDIF ELSE BEGIN

                  self -> AcceptROI, event
                  Obj_Destroy, self._selectedObject
                  RETURN

               ENDELSE

               END

            'MOTION': BEGIN

               ; Calculate the ellipse, based on cursor location.
               rotation = 0.0 ; No rotation currently. Possibly implemented later.

               ; Divide circle into Npoints
               phi = 2*!PI*(Findgen(self.count)/(self.count-1))

               ; Position angle in radians
               ang = rotation/!RADEG
               cosang = Cos(ang)
               sinang = Sin(ang)

               ; Radius based on cursor travel distance.
               self.xradius = Abs(self._click_x - event.x)
               self.yradius = Abs(self._click_y - event.y)

               ; Parameterized equation of ellipse
               x =  self.xradius * Cos(phi)
               y =  self.yradius * Sin(phi)

               ; Rotate to desired position angle
               *self.xpts = self._click_x + x*cosang - y*sinang
               *self.ypts = self._click_y + x*sinang + y*cosang

               ; Draw the completed ROI
               self -> Draw
               END

            ENDCASE

      END

   ENDCASE

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE_ROI::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain ELLIPSE_ROI properties. Be sure
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
;     DEVICE:       Set this keyword if you want the X and Y points (see XPTS and YPTS) returned
;                   in device coordinates. Otherwise, they are returned in the coordinate system
;                   of the display window (if there is one).
;
;     LINESTYLE:    The line style index. Value from 0 to 5, as for PLOT command. Default, 0, solid line.
;
;     THICK:        The thickness of the line used to draw the ROI. By default, 2.
;
;     XPTS:         The X points in the ROI. Returned in coordinate system of the display window, if possible.
;
;     YPTS:         The Y points in the ROI. Returned in coordinate system of the display window, if possible.
;
;     _REF_EXTRA:   Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO Ellipse_ROI::GetProperty, $
   DEVICE=device, $
   LINESTYLE=linestyle, $
   THICK=thick, $
   XPTS=xpts, $
   YPTS=ypts, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(linestyle) THEN linestyle = self.linestyle
   IF Arg_Present(thick) THEN thick = self.thick

   IF Arg_Present(xpts) THEN BEGIN
      IF Keyword_Set(device) THEN BEGIN
         xpts = Reform(*self.xpts)
      ENDIF ELSE BEGIN
         self._drawID -> GetProperty, Coord_Object=coords
         IF Obj_Valid(coords) THEN BEGIN
            thisWindow = !D.Window
            self._drawID -> SetWindow
            coords -> Draw
            c = Convert_Coord(Reform(*self.xpts), Reform(*self.ypts), /Device, /To_Data)
            xpts = Reform(c[0,*])
            ypts = Reform(c[1,*])
            IF thisWindow GE 0 THEN WSet, thisWindow
         ENDIF ELSE xpts = Reform(*self.xpts)
      ENDELSE
   ENDIF

      IF Arg_Present(ypts) THEN BEGIN
      IF Keyword_Set(device) THEN BEGIN
         ypts = Reform(*self.ypts)
      ENDIF ELSE BEGIN
         self._drawID -> GetProperty, Coord_Object=coords
         IF Obj_Valid(coords) THEN BEGIN
            thisWindow = !D.Window
            self._drawID -> SetWindow
            coords -> Draw
            c = Convert_Coord(Reform(*self.xpts), Reform(*self.ypts), /Device, /To_Data)
            xpts = Reform(c[0,*])
            ypts = Reform(c[1,*])
            IF thisWindow GE 0 THEN WSet, thisWindow
         ENDIF ELSE ypts = Reform(*self.ypts)
      ENDELSE
   ENDIF

IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> INTERACTION::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE_ROI::RESTOREDISPLAY
;
; PURPOSE:
;
;       This method overrides the INTERACTION RestoreDisplay method by also destroying
;       the buffer pixmap.
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
PRO Ellipse_ROI::RestoreDisplay

   @cat_pro_error_handler

   ; Call Superclass RestoreDisplay.
   self -> INTERACTION::RestoreDisplay

   ; Delete the buffer object.
   Obj_Destroy, self.bufferID

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE_ROI::SETDISPLAY
;
; PURPOSE:
;
;       This method overrides the INTERACTION SetDisplay method by adding
;       a pixmap widget for buffering output.
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
PRO Ellipse_ROI::SetDisplay, drawWidget

   @cat_pro_error_handler

   ; Call Superclass SetDisplay.
   self -> INTERACTION::SetDisplay, drawWidget

   ; Create a graphics buffer object.
   IF Obj_Valid(self.bufferID) EQ 0 THEN BEGIN
      self._drawID -> GetProperty, XSize=xsize, YSize=ysize
      self.bufferID = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize)
   ENDIF

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE_ROI::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the ELLIPSE_ROI object's properties. Be sure
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
;     _EXTRA:       Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO Ellipse_ROI::SetProperty, $
   LINESTYLE=linestyle, $
   THICK=thick, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> INTERACTION::SetProperty, _EXTRA=extraKeywords

   IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
   IF N_Elements(thick) NE 0 THEN self.thick = thick

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE_ROI::CLEANUP
;
; PURPOSE:
;
;       This is the ELLIPSE_ROI object class destructor method.
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
PRO Ellipse_ROI::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self.xpts
   Ptr_Free, self.ypts
   Obj_Destroy, self.bufferID
   Obj_Destroy, self._selectedObject
   self -> INTERACTION::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ELLIPSE_ROI::INIT
;
; PURPOSE:
;
;       This is the ELLIPSE_ROI object class initialization method
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
;     COUNT:        The number of points used to draw the ellipse. By default, 120.
;
;     LINESTYLE:    The line style index. Value from 0 to 5, as for PLOT command. Default, 0, solid line.
;
;     THICK:        The thickness of the line used to draw the ROI. By default, 2.
;
;     _EXTRA:       Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION Ellipse_ROI::INIT, drawObject, $
   COUNT=count, $
   LINESTYLE=linestyle, $
   THICK=thick, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   ok = self -> INTERACTION::INIT (drawObject, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; Check keywords.
   IF N_Elements(count) EQ 0 THEN count = 120
   IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
   IF N_Elements(thick) EQ 0 THEN thick = 2

   ; Load object.
   self.count = count
   self.linestyle = linestyle
   self._mode = 'DRAW'
   self.thick = thick

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       ELLIPSE_ROI CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the ELLIPSE_ROI object.
;
;*****************************************************************************************************
PRO Ellipse_ROI__DEFINE

   class = { ELLIPSE_ROI, $
             xpts: Ptr_New(), $        ; The X points in the ROI.
             ypts: Ptr_New(), $        ; The Y points in the ROI.
             count: 0L, $              ; The number of points in the ROI.
             bufferID: Obj_New(), $    ; A pixmap buffer for smooth graphics display.
             thick: 0.0, $             ; The thickness of the line used to draw ROI.
             linestyle: 0L, $          ; The line style of the line used to draw ROI.
             xradius: 0.0, $
             yradius: 0.0, $
             INHERITS INTERACTION $
           }

END


;*****************************************************************************************************
;
; NAME:
;       ELLIPSE_ROI TEST PROGRAM
;
; PURPOSE:
;
;       This is the test program for the ELLIPSE_ROI object. It may not
;       be required in your object.
;
;*****************************************************************************************************
PRO Ellipse_ROI_Test

   LoadCT, 0, /Silent
   tlb = Obj_New('TOPLEVELBASE')
   drawID = Obj_New('DrawWidget', tlb, XSize=400, ysize=400)
   drawID -> Add, Obj_New('catimage2d', loaddata(7))

   tlb -> Draw, /Center
   roi = Obj_New('ELLIPSE_ROI', drawID)
   roi -> SetDisplay

END