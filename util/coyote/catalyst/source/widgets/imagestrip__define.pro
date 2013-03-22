;*****************************************************************************************************
;+
; NAME:
;       IMAGESTRIP__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to allow a sequence of images to exist in
;       a rwo or strip of images. The object is a compound widget object and is
;       a subclassed BASEWIDGET. Buttons at either end of the image strip draw widget
;       allow the strip to be "moved" one image at a time. If you hold the buttons down
;       and touch the space bar, images can be move very rapidly. See the text program at
;       the end of this file for an example.
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
;       theObject = Obj_New("IMAGESTRIP")
;
; SUPERCLASSES:
;
;       BASEWIDGET
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { IMAGESTRIP, $
;             imageContainer: Obj_New(), $           ; An ImageContainer object.
;             l_buttonID: Obj_New(), $               ; The left button identifier.
;             r_buttonID: Obj_New(), $               ; The right button identifier.
;             start_frame: 0L, $                     ; The starting frame on the left.
;             drawID: Obj_New(), $                   ; Draw widget for image display. (X size is xsize*frames, Y size is ysize.)
;             pixmap: Obj_New(), $                   ; A pixmap for buffering image display.
;             xsize: 0L, $                           ; Image X size.
;             ysize: 0L, $                           ; Image Y size.
;             frames: 0L, $                          ; The number of frames in the strip.
;             annotateColor: "", $                   ; The annotation color.
;             _event_handler:Obj_New(), $            ; The real event handler object(s) for the IMAGESTRIP.
;             _event_method_real: "", $              ; The event method assigned by the user to this object widget.
;             INHERITS BASEWIDGET $
;           }
;
; EVENT_STRUCTURE:
;
;   This is the event structure sent from the IMAGESTRIP when someone clicks the mouse
;   inside the image strip window..
;
;   event  = { IMAGESTRIP_EVENT, $
;                ID:Obj_New(), $            ; The widget object that caused the event.
;                TOP: Obj_New(), $          ; The object at the top of the object hierarchy.
;                HANDLER:Obj_New(), $       ; The event handler object.
;                EVENT_NAME: "", $          ; The name of the event, IMAGESTRIP_EVENT.
;                NAME: "", $                ; The name of the object.
;                SELECTION:Obj_New() $      ; The selected image object in the ImageContainer.
;                BUTTON_DOWN: ""            ; The button clicked DOWN in draw widget. "LEFT", "MIDDLE", or "RIGHT".
;            }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 21 February 2004.
;       Added BUTTON_DOWN field in IMAGESSTRIP_EVENT. 20 Dec 2004. DWF.
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
;       IMAGESTRIP::DRAW
;
; PURPOSE:
;
;       This method draws all the images in the ImageContainer object. It is assumed
;       that the ImageContainer object contains CatImage or ImageFrame objects that
;       can have their output size and starting coordinates set by the SetProperty
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
PRO ImageStrip::Draw, _Extra=extraKeywords

   @cat_pro_error_handler

   ; Do this in a pixmap, because it might take a while.

   self.pixmap -> SetWindow
   self.pixmap -> Refresh

   ; Any draw widget colors?
   self.drawID -> ApplyColors

   ; Get the image objects to draw. Set the output size before you draw the objects.
   objects = self.imageContainer -> Get(/All, Count=imagecount)
   IF imagecount NE 0 THEN BEGIN
      IF imagecount LT self.frames THEN BEGIN
         FOR j=0,imagecount-1 DO BEGIN
            objects[j] -> SetProperty, XSize=self.xsize, YSize=self.ysize, XStart=self.xsize*j, YStart=0, Display_Mode=1
            objects[j] -> Draw
         ENDFOR
      ENDIF ELSE BEGIN
         count = 0
         IF (self.start_frame + self.frames) GT imagecount THEN BEGIN
            self.start_frame = self.start_frame - 1
         ENDIF
         FOR j=self.start_frame, self.start_frame+self.frames - 1 DO BEGIN
            objects[j] -> SetProperty, XSize=self.xsize, YSize=self.ysize, XStart=self.xsize*count, YStart=0, Display_Mode=1
            objects[j] -> Draw
            count = count + 1
         ENDFOR
      ENDELSE

   ENDIF

   ; Draw frame divisions.
   Device, Decomposed=1, Get_Decomposed=theState
   FOR j=1,self.frames-1 DO BEGIN
      Plots, [self.xsize*j, self.xsize*j], [0, !D.Y_Size], /Device, Color=FSC_Color(self.annotateColor)
   ENDFOR
   Device, Decomposed=theState

   ; Draw any objects contained within this object.
   self -> BASEWIDGET::Draw

   ; Copy to the display window.
   self.drawID -> SetWindow
   self.pixmap -> Copy

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;        IMAGESTRIP::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the IMAGESTRIP object.
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
PRO ImageStrip::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   CASE event.name OF

      'SHIFT_LEFT': $
         BEGIN
            IF event.select EQ 0 THEN RETURN
            self.start_frame = 0 > (self.start_frame - 1)
            IF self.start_frame EQ 0 THEN $
               self.l_buttonID ->SetProperty, Sensitive=0 ELSE $
               self.l_buttonID ->SetProperty, Sensitive=1
            IF (self.start_frame + self.frames) EQ (self.imageContainer -> Count() ) THEN $
               self.r_buttonID ->SetProperty, Sensitive=0 ELSE $
               self.r_buttonID ->SetProperty, Sensitive=1
            self -> Draw
         END

      'SHIFT_RIGHT': $
         BEGIN
            IF event.select EQ 0 THEN RETURN
            self.start_frame = 0 > ((self.start_frame + self.frames + 1) - self.frames)
            IF self.start_frame EQ 0 THEN $
               self.l_buttonID ->SetProperty, Sensitive=0 ELSE $
               self.l_buttonID ->SetProperty, Sensitive=1
            IF (self.start_frame + self.frames) EQ (self.imageContainer -> Count() ) THEN $
               self.r_buttonID ->SetProperty, Sensitive=0 ELSE $
               self.r_buttonID ->SetProperty, Sensitive=1
            self -> Draw
         END

      'IMAGESTRIP': BEGIN

            ; Only button down events in the draw widget.
            IF event.type NE 0 THEN RETURN

            CASE event.press OF
               1: button_pressed = 'LEFT'
               2: button_pressed = 'MIDDLE'
               4: button_pressed = 'RIGHT'
               ELSE: button_pressed = 'LEFT'
            ENDCASE
            ; What image index did you select?
            index = event.x / self.xsize + self.start_frame
            dummy = self.imageContainer -> Get(/All, Count=count)
            IF index GE count THEN RETURN

            theImageObject = self.imageContainer -> Get(Position=index)

            ; If we need to send an event, package the event up. Include both
            ; the index number (what normal droplist events produce), the current
            ; selection, and the self object reference.
            ; Check that there is at least one valid event object, otherwise swallow the event.
            eventObjs = self._event_handler -> Get(/All)
            thisEvent = {IMAGESTRIP_EVENT, self, Obj_New(), 'IMAGESTRIP_EVENT', $
               self._name, theImageObject, button_pressed}
            IF (MAX (OBJ_VALID (eventObjs)) EQ 0) THEN RETURN ; There is no valid event object.

            ; Find out the target object for the event, get the event method from
            ; the target object, and send the event to this method, UNLESS this
            ; object has its own object method.
            FOR e = 0, N_ELEMENTS (eventObjs) - 1 DO $
            BEGIN
               IF OBJ_VALID (eventObjs [e]) THEN $
               BEGIN
                  thisEvent.handler = eventObjs [e]
                  eventObjs[e] -> CatAtom::GetProperty, Event_Method=event_method
                  IF (self._event_method_real NE "") AND (e EQ 0) THEN $
                     thisMethod = self._event_method_real ELSE thisMethod = event_method
                  Call_Method, thisMethod, eventObjs[e], thisEvent
               ENDIF
            ENDFOR
         END

      ELSE: Print, 'ImageStrip: Received an event from: ', event.name
   ENDCASE

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       IMAGESTRIP::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain IMAGESTRIP properties. Be sure
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
;     ANNOTATECOLOR:    The name of the annotation color for this object. Image frame
;                       dividers are drawn in this color.
;
;     COLOR_OBJECT:     A ColorTool object for the draw widget object. Use this if you
;                       want the draw widget to manage colors for the images.
;
;     DRAWOBJECT:       The object reference of the draw widget object.
;
;     IMAGECONTAINER:   The image container object containing CATIMAGE or IMAGEFRAME objects.
;
;     FRAMES:           The number of image "frames" to display in the frame window. Note, this
;                       is the number of visible images, not the total number of images in the
;                       container.
;
;     START_FRAME:      The initial frame to put on the left side of the visible images. By default, 0.
;
;     XSIZE:            The output image X size. Each image frame with have this X size.
;
;     YSIZE:            The output image Y size. Each image frame with have this Y size.
;
;     _REF_EXTRA:       Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO ImageStrip::GetProperty, $
   ANNOTATECOLOR=annotateColor, $
   COLOR_OBJECT=color_object, $
   IMAGECONTAINER=imagecontainer, $
   FRAMES=frames, $
   START_FRAME=start_frame, $
   XSIZE=xsize, $
   YSIZE=ysize, $
   DRAWOBJECT=drawObject, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(annotateColor) THEN annotateColor = self.annotateColor
   IF Arg_Present(color_object) THEN self.drawID -> GetProperty, Color_Object=color_object
   IF Arg_Present(drawObject) THEN drawObject = self.drawID
   IF Arg_Present(frames) THEN frames = self.frames
   IF Arg_Present(imagecontainer) THEN imagecontainer = self.imagecontainer
   IF Arg_Present(start_frame) THEN start_frame = self.start_frame
   IF Arg_Present(xsize) THEN xsize = self.xsize
   IF Arg_Present(ysize) THEN ysize = self.ysize

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> BASEWIDGET::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       IMAGESTRIP::MESSAGEHANDLER
;
; PURPOSE:
;
;       This method receives notification of a message call from another object's SENDMESSAGE
;       method.
;
; SYNTAX:
;
;       thisObject -> MessageHandler, title, SENDER=sender, MESSAGE=message
;
; ARGUMENTS:
;
;       TITLE:   The title of the message.
;
; KEYWORDS:
;
;       DATA:    A keyword that contains any information the sender wishes to pass
;                with the message. It can be empty.
;
;       SENDER:  The object that generated the message
;
;-
;*****************************************************************************************************
PRO ImageStrip::MessageHandler, title, SENDER=sender, DATA=data

   @cat_pro_error_handler

   IF N_Elements(title) EQ 0 THEN Message, 'Ill-formed message received. No title.'

   ; Handle various kinds of messages.
   CASE title OF

      'COLORTOOL_SETPROPERTY': self -> Draw

      ELSE: self -> Report, 'Cannot repond to message: ' + title, 1

   ENDCASE

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       IMAGESTRIP::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the IMAGESTRIP object's properties. Be sure
;       you ALWAYS call the superclass SETPROPERTY method if you have extra keywords!
;
;
; SYNTAX:
;
;       theObject -> SetProperty ...
;
; ARGUMENTS:
;
;     ANNOTATECOLOR:    The name of the annotation color for this object. Image frame
;                       dividers are drawn in this color.
;
;     COLOR_OBJECT:     A ColorTool object for the draw widget object. Use this if you
;                       want the draw widget to manage colors for the images.
;
;     IMAGECONTAINER:   The image container object containing CATIMAGE or IMAGEFRAME objects.
;
;     FRAMES:           The number of image "frames" to display in the frame window. Note, this
;                       is the number of visible images, not the total number of images in the
;                       container.
;
;     START_FRAME:      The initial frame to put on the left side of the visible images. By default, 0.
;
;     XSIZE:            The output image X size. Each image frame with have this X size.
;
;     YSIZE:            The output image Y size. Each image frame with have this Y size.
;
; KEYWORDS:
;
;     _EXTRA:     Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO ImageStrip::SetProperty, $
   ANNOTATECOLOR=annotateColor, $
   COLOR_OBJECT=color_object, $
   EVENT_METHOD=event_method, $   ; Required to intercept event method intended for CATATOM.
   EVENT_OBJECTS=event_objects, $ ; Required to intercept event objects intended for CATATOM.
   IMAGECONTAINER=imagecontainer, $
   FRAMES=frames, $
   START_FRAME=start_frame, $
   XSIZE=xsize, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(color_object) NE 0 THEN self.drawID -> SetProperty, Color_Object=color_object
   resize = 0
   IF N_Elements(frames) NE 0 THEN BEGIN
      self.frames = frames
      resize = 1
   ENDIF
   IF N_Elements(xsize) NE 0 THEN BEGIN
      self.xsize = xsize
      resize = 1
   ENDIF
   IF N_Elements(ysize) NE 0 THEN BEGIN
      self.ysize = ysize
      resize = 1
   ENDIF

   IF resize THEN BEGIN
      self.drawID -> SetProperty, XSize=self.xsize*self.frames, YSize=self.ysize
      self.pixmap -> SetProperty, XSize=self.xsize*self.frames, YSize=self.ysize
   ENDIF

   IF N_Elements(imagecontainer) NE 0 THEN BEGIN
      IF Obj_Valid(self.imagecontainer) THEN self.imagecontainer -> RemoveParent, self
      self.imagecontainer = imagecontainer
      self.imagecontainer -> AddParent, self
   ENDIF

   IF N_Elements(annotateColor) NE 0 THEN self.annotateColor = annotateColor

      ; The following is required because IMAGESTRIP is a compound object.
      ; Get the assigned event object. Replace this with the self object. This
      ; assures you that the events will go through the IMAGESTRIP's EventHandler
      ; method first. Save the assigned event object so it can be used at the end
      ; of the compound object's EventHandler method.

  IF N_Elements(event_objects) NE 0 THEN BEGIN
      first = self._event_handler->Get(Position=0)
      self._event_handler -> Remove, /All
      IF Obj_Valid(first) THEN self._event_handler -> Add, first
      FOR j=0,N_Elements(event_objects) -1 DO BEGIN
         IF Obj_Valid(event_objects[j]) THEN self._event_handler -> Add, event_objects[j]
      ENDFOR
  ENDIF

   IF N_Elements(start_frame) NE 0 THEN $
      IF Obj_Valid(self.imagecontainer) THEN $
         self.start_frame = 0 > start_frame < (self.imagecontainer -> Count() - self.frames) ELSE $
         self.start_frame = start_frame

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> BASEWIDGET::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       IMAGESTRIP::CLEANUP
;
; PURPOSE:
;
;       This is the IMAGESTRIP object class destructor method.
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
PRO ImageStrip::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self.pixmap
   IF Obj_Valid(self.imageContainer) THEN self.imageContainer -> RemoveParent, self

   ; Remove all the children in self._event_handler and destroy the container.
   self._event_handler -> Remove, /All
   Obj_Destroy, self._event_handler

   self -> BASEWIDGET::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       IMAGESTRIP::INIT
;
; PURPOSE:
;
;       This is the IMAGESTRIP object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     ANNOTATECOLOR:    The name of the annotation color for this object. Image frame
;                       dividers are drawn in this color. By default, "sky blue".
;
;     COLOR_OBJECT:     A ColorTool object for the draw widget object. Use this if you
;                       want the draw widget to manage colors for the images.
;
;     IMAGECONTAINER:   The image container object containing CATIMAGE or IMAGEFRAME objects.
;
;     FRAMES:           The number of image "frames" to display in the frame window. Note, this
;                       is the number of visible images, not the total number of images in the
;                       container.
;
;     START_FRAME:      The initial frame to put on the left side of the visible images. By default, 0.
;
;     UNITS:            The units for measurments. The default is 0 for pixels. Other values are
;                       1 for inches, and 2 for centimeters.
;
;     XSIZE:            The output image X size. Each image frame with have this X size.
;
;     YSIZE:            The output image Y size. Each image frame with have this Y size.
;
; KEYWORDS:
;
;     _EXTRA:           Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION ImageStrip::INIT, parent, $
   ANNOTATECOLOR=annotatecolor, $
   COLOR_OBJECT=color_object, $
   EVENT_METHOD=event_method, $   ; Required to intercept event method intended for CATATOM.
   EVENT_OBJECTS=event_objects, $ ; Required to intercept event objects intended for CATATOM.
   IMAGECONTAINER=imagecontainer, $
   FRAMES=frames, $
   START_FRAME=start_frame, $
   UNITS=units, $
   XSIZE=xsize, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   ok = self -> BASEWIDGET::INIT (parent, Row=1, XPad=0, YPad=0, Space=0, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

      ; The following is required because  IMAGESTRIP is a compound object.
      ; Get the assigned event object. Replace this with the self object. This
      ; assures you that the events will go through the IMAGESTRIP's EventHandler
      ; method first. Save the assigned event object so it can be used at the end
      ; of the compound object's EventHandler method.

   IF N_Elements(event_objects) EQ 0 THEN $
   BEGIN
      parent -> CATATOM::GetProperty, Event_Objects=event_objects
      self._event_handler = OBJ_NEW ('CatContainer', Memory_Management=0)
      FOR j=0,N_Elements(event_objects) -1 DO BEGIN
         IF Obj_Valid(event_objects[j]) THEN self._event_handler -> Add, event_objects[j]
      ENDFOR
   ENDIF ELSE BEGIN
      self._event_handler = OBJ_NEW ('CatContainer', Memory_Management=0)
      FOR j=0,N_Elements(event_objects) -1 DO BEGIN
         IF Obj_Valid(event_objects[j]) THEN self._event_handler -> Add, event_objects[j]
      ENDFOR
   ENDELSE

      ; All events MUST come to the EventHandler method before they are dispatched
      ; elsewhere. This is typical of compound objects.

   IF N_Elements(event_method) NE 0 THEN self._event_method_real = event_method
   self._event_method = 'EventHandler'


   IF Obj_Isa_Valid(imagecontainer, 'ImageContainer') THEN BEGIN
      self.imagecontainer = imagecontainer
      self.imagecontainer -> AddParent, self
   ENDIF

   IF N_Elements(annotatecolor) EQ 0 THEN annotatecolor = 'sky blue'
   IF N_Elements(frames) EQ 0 THEN frames = 8
   IF N_Elements(start_frame) EQ 0 THEN start_frame = 0
   IF N_Elements(xsize) EQ 0 THEN xsize = 100
   IF N_Elements(ysize) EQ 0 THEN ysize = 100

   self.annotatecolor = annotatecolor
   self.frames = frames
   IF Obj_Valid(self.imagecontainer) THEN $
      self.start_frame = 0 > start_frame < (self.imagecontainer -> Count() - self.frames) ELSE $
      self.start_frame = start_frame
   IF self.start_frame LT 0 THEN self.start_frame = 0
   self.xsize = xsize
   self.ysize = ysize

   filename = Filepath(Subdir=['resource', 'bitmaps'], 'shift_left.bmp')
   self.l_buttonID = Obj_New('ButtonWidget', self, Value=filename, Name='SHIFT_LEFT', /Bitmap, /Pushbutton_Events)
   self.drawID = Obj_New('DrawWidget', self, XSize=self.frames*self.xsize, YSize=self.ysize, $
      Button_Events=1, Name='IMAGESTRIP', Color_Object=color_object, Units=units)
   filename = Filepath(Subdir=['resource', 'bitmaps'], 'shift_right.bmp')
   self.r_buttonID =Obj_New('ButtonWidget', self, Value=filename, Name='SHIFT_RIGHT', /Bitmap, /Pushbutton_Events)
   IF self.start_frame EQ 0 THEN self.l_buttonID -> SetProperty, Sensitive=0
   IF self.start_frame EQ (self.imageContainer -> Count() -1 - self.frames) THEN $
      self.l_buttonID -> SetProperty, Sensitive=0


   self.pixmap = Obj_New('PixmapWidget', XSize=self.frames*self.xsize, YSize=self.ysize)

   ; Register interest in color object
   IF OBJ_ISA_VALID (color_object, 'COLORTOOL')  THEN $
   BEGIN
      color_object -> RegisterForMessage, self, 'COLORTOOL_SETPROPERTY'
   END

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       IMAGESTRIP CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the IMAGESTRIP object.
;
;*****************************************************************************************************
PRO ImageStrip__DEFINE, class

   event  = { IMAGESTRIP_EVENT, $
                ID:Obj_New(), $            ; The widget object that caused the event.
                HANDLER:Obj_New(), $       ; The event handler object.
                EVENT_NAME: "", $          ; The name of the event, IMAGESTRIP_EVENT.
                NAME: "", $                ; The name of the object.
                SELECTION:Obj_New(), $     ; The selected image object in the ImageContainer.
                BUTTON_DOWN: "" $          ; The button clicked DOWN in draw widget. "LEFT", "MIDDLE", or "RIGHT".
            }


   class = { IMAGESTRIP, $
             imageContainer: Obj_New(), $           ; An ImageContainer object.
             l_buttonID: Obj_New(), $               ; The left button identifier.
             r_buttonID: Obj_New(), $               ; The right button identifier.
             start_frame: 0L, $                     ; The starting frame on the left.
             drawID: Obj_New(), $                   ; Draw widget for image display. (X size is xsize*frames, Y size is ysize.)
             pixmap: Obj_New(), $                   ; A pixmap for buffering image display.
             xsize: 0L, $                           ; Image X size.
             ysize: 0L, $                           ; Image Y size.
             frames: 0L, $                          ; The number of frames in the strip.
             annotateColor: "", $                   ; The annotation color.
             _event_handler:Obj_New(), $            ; The real event handler object(s) for the IMAGESTRIP.
             _event_method_real: "", $              ; The event method assigned by the user to this object widget.
             INHERITS BASEWIDGET $
           }

END


;*****************************************************************************************************
;
; NAME:
;       IMAGESTRIP TEST PROGRAM
;
; PURPOSE:
;
;       This is the test program for the IMAGESTRIP object. It may not
;       be required in your object.
;
;*****************************************************************************************************
PRO ImageStrip_Test

   LoadCT, 0, /Silent
   images = LoadData(8)
   imagestack = Obj_New('ImageStack', images)
   imageContainer = Obj_New('ImageContainer', Name='bob')
   imageStack -> GetProperty, ZSize=number
   FOR j=0,number-1 DO BEGIN
      imageContainer -> Add, Obj_New('ImageFrame', imageStack, Framenumber=j, Display_Mode=1)
   ENDFOR
   tlb = Obj_New('TOPLEVELBASE')
   strip = Obj_New('IMAGESTRIP', tlb, ImageContainer=imageContainer, XSIZE=80, YSize=100, Frames=8, $
      Start_Frame=43)
   tlb -> Draw, /Center, /Block

   Obj_Destroy, imagestack
   Obj_Destroy, imageContainer

END