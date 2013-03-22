;*****************************************************************************************************
;+
; NAME:
;       ImageFrame
;
; PURPOSE:
;
;       This object implements a class for handling image data. This object is
;       conceptually identical to a CatImage object, except that the image data
;       is not stored directly in the object. Rather, a pointer to an ImageStack
;       object, a frame number, and a slice orientation are used to access the
;       image data when needed for display. This object was used in a medical
;       imaging application in support of volumentric image data.
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
; SUPERCLASSES:
;
;       CATDATAATOM
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; SYNTAX:
;
;       imageObject = OBJ_NEW ('ImageFrame', imageStack)
;
; CLASS DEFINITION:
;
;    class = { ImageFrame, $                    ; The ImageFrame object class.
;              _stack        : Obj_New(), $     ; The IMAGESTACK object.
;              _display_mode : 0B, $            ; A flag indicating which display method to use for drawing.
;              _inverse      : 0B, $            ; If this flag is set, the image is drawn in inverse color.
;              _keep_aspect  : 0L, $            ; Flag: Keep the aspect ratio when calculating image position.
;              _location     : DblArr(6,2), $   ; Location information about the "displayed" image.
;              _interpolate  : 0L, $            ; Flag: When set, bilinear interpolation is used.
;              _order        : 0L, $            ; Image display order.
;              _out_xsize    : 0L, $            ; The output X size of the image. (Display_Mode=1 and Display_Mode=2)
;              _out_ysize    : 0L, $            ; The output Y size of the image. (Display_Mode=1 and Display_Mode=2)
;              _position     : DblArr(4), $     ; The position of the image in the output window. (Display_Mode=0)
;              _xsize        : 0L, $            ; The X size of the image.
;              _xstart       : 0L, $            ; The starting X location for displaying image. (Display_Mode=1)
;              _ysize        : 0L, $            ; The Y size of the image.
;              _ystart       : 0L, $            ; The starting Y location for displaying image. (Display_Mode=1)
;              _wid          : Obj_New(), $     ; The window object where this image should be displayed.
;              _window_position: 0L, $          ; The window position for displaying this image. (Display_Mode=2)
;              _orientation  : "", $            ; The orientation of the slice: "X", "Y", or "Z" (the default).
;              _framenumber  : 0L, $            ; The frame number.
;              _thick        : 0L, $            ; The "thickness" of the image slice. (See IMAGESTACK documentation.)
;              INHERITS CatDataAtom $           ; Subclassed from CATDATAATOM.
;            }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 27 March 2003.
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
;       ImageFrame::DRAW
;
; PURPOSE:
;
;       This method draws the image to the display based on the current DISPLAY_MODE
;       in effect for the widget.
;
; SYNTAX:
;
;       imageObject -> Draw
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
PRO ImageFrame::Draw, _Extra=extrakeywords

   ; Set up the error handler.
   @cat_pro_error_handler

   ; Which display mode?
   CASE self._display_mode OF
      0: self -> Draw_Mode_0, _Extra=extrakeywords
      1: self -> Draw_Mode_1, _Extra=extrakeywords
      2: self -> Draw_Mode_2, _Extra=extrakeywords
   ENDCASE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ImageFrame::DRAW_MODE_0
;
; PURPOSE:
;
;       This display mode position's the image in the display window based
;       on the POSITION of the image.
;
; SYNTAX:
;
;       imageObject -> Draw_Mode_0
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
PRO ImageFrame::Draw_Mode_0, _Extra=extrakeywords

   ; Set up the error handler.
   @cat_pro_error_handler

  ; Get the image slice from the data cube, if it is available.
  IF Obj_Valid(self._stack) EQ 0 THEN RETURN
  theImageFrame = self._stack -> GetFrame(self._framenumber, Thick=self._thick, Orientation=self._orientation)
  IF self._inverse THEN theImageFrame = 255 - Temporary(theImageFrame)

  ; Gather information about the image.
  dimensions = Image_Dimensions(theImageFrame, XSize=xsize, YSize=ysize)

  ; Set to display window, if you have one. Otherwise, draw in current window.
  IF Obj_Valid(self._wid) THEN self._wid -> SetWindow

  ; Keep aspect ratio?
  IF self._keep_aspect THEN $
  BEGIN

     ; Find aspect ratio of image.
     ratio = FLOAT(ysize) / xsize

     ; Find the proposed size of the image in pixels without aspect
     ; considerations.
     xpixSize = (self._position[2] - self._position[0]) * !D.X_VSize
     ypixSize = (self._position[3] - self._position[1]) * !D.Y_VSize

     ; Try to fit the image width. If you can't maintain
     ; the aspect ratio, fit the image height.
     trialX = xpixSize
     trialY = trialX * ratio
     IF trialY GT ypixSize THEN BEGIN
        trialY = ypixSize
        trialX = trialY / ratio
     ENDIF

     ; Recalculate the position of the image in the window.
     position = FltArr(4)
     position[0] = (((xpixSize - trialX) / 2.0) / !D.X_VSize) + self._position[0]
     position[2] = position[0] + (trialX/Double(!D.X_VSize))
     position[1] = (((ypixSize - trialY) / 2.0) / !D.Y_VSize)  + self._position[1]
     position[3] = position[1] + (trialY/Double(!D.Y_VSize))

  ENDIF ELSE position = self._position

  ; Calculate the image size and start locations.
  xsize = (position[2] - position[0]) * !D.X_VSIZE
  ysize = (position[3] - position[1]) * !D.Y_VSIZE
  xstart = position[0] * !D.X_VSIZE
  ystart = position[1] * !D.Y_VSIZE

  ; Display the image. Sizing differently for scalable pixels devices.
  IF (!D.Flags AND 1) NE 0 THEN $
  BEGIN

     ; Set up colors and coordinates (if they exist).
     self -> ApplyColors
     self -> ApplyCoords

     ; Display it.
     TV, theImageFrame, ROUND(xstart), ROUND(ystart), XSIZE=CEIL(xsize), YSIZE=CEIL(ysize), ORDER=self._order

  ENDIF ELSE $
  BEGIN ; All other devices.

     Device, Get_Decomposed=theState
     Device, Decomposed=0

     ; Set up colors and coordinates (if they exist).
     self -> ApplyColors
     self -> ApplyCoords

     TV, Congrid(theImageFrame, CEIL(xsize), CEIL(ysize), INTERP=self._interpolate, $
              MINUS_ONE=1), ROUND(xstart), ROUND(ystart), Order=self._order

     Device, Decomposed=theState

     ; Update the location variables, as these may have changed.
     self._location[*,0] = [Floor(xstart), Floor(ystart), $
                            Round(Floor(xstart) + Round(xsize)), Round(Floor(ystart) + Round(ysize)), $
                            Double(!D.X_VSize), Double(!D.Y_VSize)]
     self._location[*,1] = [ self._location[0,0]/self._location[4,0], $
                             self._location[1,0]/self._location[5,0], $
                             self._location[2,0]/self._location[4,0], $
                             self._location[3,0]/self._location[5,0], $
                             self._location[4,0]/self._location[4,0], $
                             self._location[5,0]/self._location[5,0] ]
  ENDELSE

     ; Are there any image axes? Update their position.

  containedObjects = self -> Get(/All)
  FOR jj=0,N_Elements(containedObjects)-1 DO BEGIN

     IF Obj_Class(containedObjects[jj]) EQ 'IMGAXES' THEN $
        containedObjects[jj] -> SetProperty, Position=self._location[0:3,1]

  ENDFOR

  ; Is there a contained coordinate object that needs updating?
  IF Obj_Isa_Valid(self._coords, 'CATCOORD') THEN BEGIN
     self._coords -> SetProperty, Position=self._location[0:3,1]
  END

  ; Any children to draw?
  self -> CatDataAtom::Draw, _Extra=extrakeywords

  self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ImageFrame::DRAW_MODE_1
;
; PURPOSE:
;
;       This display mode position's the image in the display window based
;       on the lower-left corner of the image.
;
; SYNTAX:
;
;       imageObject -> Draw_Mode_1
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
PRO ImageFrame::Draw_Mode_1, _Extra=extrakeywords

   ; Set up the error handler.
   @cat_pro_error_handler

  ; Get the image slice from the data cube, if it is available.
  IF Obj_Valid(self._stack) EQ 0 THEN RETURN
  theImageFrame = self._stack -> GetFrame(self._framenumber, Thick=self._thick, Orientation=self._orientation)
  IF self._inverse THEN theImageFrame = 255 - Temporary(theImageFrame)

  ; Gather information about the image.
  dimensions = Image_Dimensions(theImageFrame, XSize=xsize, YSize=ysize)

  ; Set to display window, if you have one. Otherwise, draw in current window.
  IF Obj_Valid(self._wid) THEN self._wid -> SetWindow


  ; Display the image. Sizing differently for scalable pixels devices.
  IF (!D.Flags AND 1) NE 0 THEN $
  BEGIN

      ; Size the image for output.
      self -> GetProperty, XSize=xsize, YSize=ysize

      ; Set up colors and coordinates (if they exist).
      self -> ApplyColors
      self -> ApplyCoords

      ; Display it.
      TV, theImageFrame, self._xstart, self._ystart, XSIZE=xsize, YSIZE=ysize, ORDER=self._order

  ENDIF ELSE $
  BEGIN ; All other devices.

     Device, Get_Decomposed=theState
     Device, Decomposed=0

     ; Set up colors and coordinates (if they exist).
     self -> ApplyColors
     self -> ApplyCoords

     ; Make sure you have valid output sizes
     IF (self._out_xsize LE 0) OR (self._out_ysize LE 0) THEN Message, 'Image output sizes cannot be zero.'

     TV, Congrid(theImageFrame, self._out_xsize, self._out_ysize, INTERP=self._interpolate, $
              MINUS_ONE=1), self._xstart, self._ystart, Order=self._order

     Device, Decomposed=theState

     ; Update the location variables, as these may have changed.
     self._location[*,0] = [Floor(self._xstart), Floor(self._ystart), $
                            Round(Floor(self._xstart) + Round(self._out_xsize)), $
                            Round(Floor(self._ystart) + Round(self._out_ysize)), $
                            Double(!D.X_VSize), Double(!D.Y_VSize)]
     self._location[*,1] = [ self._location[0,0]/self._location[4,0], $
                             self._location[1,0]/self._location[5,0], $
                             self._location[2,0]/self._location[4,0], $
                             self._location[3,0]/self._location[5,0], $
                             self._location[4,0]/self._location[4,0], $
                             self._location[5,0]/self._location[5,0] ]
  ENDELSE

     ; Are there any image axes? Update their position.

  containedObjects = self -> Get(/All)
  FOR jj=0,N_Elements(containedObjects)-1 DO BEGIN

     IF Obj_Class(containedObjects[jj]) EQ 'IMGAXES' THEN $
        containedObjects[jj] -> SetProperty, Position=self._location[0:3,1]

  ENDFOR

  ; Is there a contained coordinate object that needs updating?
  IF Obj_Isa_Valid(self._coords, 'CATCOORD') THEN BEGIN
     self._coords -> SetProperty, Position=self._location[0:3,1]
  END

  ; Any children to draw?
  self -> CatDataAtom::Draw, _Extra=extrakeywords

  self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ImageFrame::DRAW_MODE_2
;
; PURPOSE:
;
;       This display mode position's the image in the display window based
;       on a window position parameter.
;
; SYNTAX:
;
;       imageObject -> Draw_Mode_2
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
PRO ImageFrame::Draw_Mode_2, _Extra=extrakeywords

   ; Set up the error handler.
   @cat_pro_error_handler

  ; Get the image slice from the data cube, if it is available.
  IF Obj_Valid(self._stack) EQ 0 THEN RETURN
  theImageFrame = self._stack -> GetFrame(self._framenumber, Thick=self._thick, Orientation=self._orientation)
  IF self._inverse THEN theImageFrame = 255 - Temporary(theImageFrame)

  ; Gather information about the image.
  dimensions = Image_Dimensions(theImageFrame, XSize=xsize, YSize=ysize)

  ; Set to display window, if you have one. Otherwise, draw in current window.
  IF Obj_Valid(self._wid) THEN self._wid -> SetWindow


  ; Display the image. Sizing differently for scalable pixels devices.
  IF (!D.Flags AND 1) NE 0 THEN $
  BEGIN

      ; Size the image for output.
      self -> GetProperty, XSize=xsize, YSize=ysize

      ; Set up colors and coordinates (if they exist).
      self -> ApplyColors
      self -> ApplyCoords

      ; Display it
      TV, theImageFrame, self._window_position, XSIZE=xsize, YSIZE=ysize, ORDER=self._order

  ENDIF ELSE $
  BEGIN ; All other devices.

     Device, Get_Decomposed=theState
     Device, Decomposed=0

     ; Set up colors and coordinates (if they exist).
     self -> ApplyColors
     self -> ApplyCoords

     ; Make sure you have valid output sizes
     IF (self._out_xsize LE 0) OR (self._out_ysize LE 0) THEN Message, 'Image output sizes cannot be zero.'

     TV, Congrid(theImageFrame, self._out_xsize, self._out_ysize, INTERP=self._interpolate, $
              MINUS_ONE=1), self._window_position, Order=self._order

     Device, Decomposed=theState

     ; Update the location variables, as these may have changed.
     self._location[*,0] = [Floor(self._xstart), Floor(self._ystart), $
                            Round(Floor(self._xstart) + Round(self._out_xsize)), $
                            Round(Floor(self._ystart) + Round(self._out_ysize)), $
                            Double(!D.X_VSize), Double(!D.Y_VSize)]
     self._location[*,1] = [ self._location[0,0]/self._location[4,0], $
                             self._location[1,0]/self._location[5,0], $
                             self._location[2,0]/self._location[4,0], $
                             self._location[3,0]/self._location[5,0], $
                             self._location[4,0]/self._location[4,0], $
                             self._location[5,0]/self._location[5,0] ]
  ENDELSE

     ; Are there any image axes? Update their position.

  containedObjects = self -> Get(/All)
  FOR jj=0,N_Elements(containedObjects)-1 DO BEGIN

     IF Obj_Class(containedObjects[jj]) EQ 'IMGAXES' THEN $
        containedObjects[jj] -> SetProperty, Position=self._location[0:3,1]

  ENDFOR

  ; Is there a contained coordinate object that needs updating?
  IF Obj_Isa_Valid(self._coords, 'CATCOORD') THEN BEGIN
     self._coords -> SetProperty, Position=self._location[0:3,1]
  END

  ; Any children to draw?
  self -> CatDataAtom::Draw, _Extra=extrakeywords

  self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ImageFrame::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to get properties of the ImageFrame object
;
; SYNTAX:
;
;       imageObject -> GETPROPERTY, XSIZE=xsize, YSIZE=ysize
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       AXES:             The image axes object for this image. If not available, a null object.
;
;       DIMENSIONS:       A three-element array containing the dimensions of the imageStack object.
;
;       IMAGE:            The image data.
;
;       INVERSE:          If this keyword is set, the image is drawn in inverse color (255-image).
;
;       KEEP_ASPECT:      The keep aspect ratio flag.
;
;       LOCATION:         A 6-by-2 floating array containing information about the position of the
;                         "displayed" image, which is updated after every image DRAW. The first two
;                         elements (location[0:1,0] contain the X and Y positions of the
;                         lower-left corner of the image (device units), the next two elements
;                         (location[2:3,0]) the X and Y positions of the upper-right corner of the
;                         image (device units)), and the last two elements contain the X and Y size of
;                         the display window (device units). The location[*,1] elements are
;                         exactly the same as the location[*,0] elements, except they are
;                         expressed in normalized (0 to 1) coordinates.
;
;       NOINTERPOLATE:    The nointerpolate flag.
;
;       OUT_XSIZE:        The X size of the image on the display.
;
;       OUT_YSIZE:        The Y size of the image on the display.
;
;       ORIENTATION:      The current orientation of the slice.
;
;
;       POSITION:         A four-element vector giving the position of the image in the
;                         display window in normalized coordinates: [x0, y0, x1, y1].
;
;       FRAMENUMBER:      The current index number of the slice.
;
;       THICK:            The current slice "thickness".
;
;       XSIZE:            The X size of the image.
;
;       XSTART:           The X location (in pixels) of the lower-left corner of the image. Used only
;                         in DISPLAY_MODE=1.
;
;       YSIZE:            The Y size of the image.
;
;       YSTART:           The Y location (in pixels) of the lower-left corner of the image. Used only
;                         in DISPLAY_MODE=1.
;       WID:              The window object where this image will be displayed.
;
;       WINDOW_POSITION:  A single number that gives a "window position" for displaying the image.
;                         The same as calling the TV command with a single positional parameter other
;                         than the image.
;
;       _REF_EXTRA:       Any keyword appropriate for the DATAATOM::GETPROPERTY method.
;
;-
;*****************************************************************************************************
PRO ImageFrame::GetProperty,$
   AXES=axes, $
   DIMENSIONS=dimensions, $
   IMAGESTACK=imagecube, $
   INVERSE=inverse, $
   KEEP_ASPECT=keep_aspect, $
   LOCATION=location, $
   NOINTERPOLATE=nointerpolate, $
   OUT_XSIZE=out_xsize, $
   OUT_YSIZE=out_ysize, $
   ORIENTATION=orientation, $
   POSITION=position, $
   FRAMENUMBER=framenumber, $
   THICK=thick, $
   XSTART=xstart, $
   XSIZE=xsize, $
   YSTART=ystart, $
   YSIZE=ysize, $
   WID=wid, $
   WINDOW_POSITION=window_position, $
    _REF_EXTRA=extraKeywords

      ; Initialise the error handler
   @cat_pro_error_handler

      ; Create the return variables

   IF Arg_Present(axes) THEN axes = self -> Get('ImageAxes')
   IF Arg_Present(dimensions) THEN IF Obj_Valid(self._stack) THEN $
      self._stack -> GetProperty, DIMENSIONS=dimensions
   IF Arg_Present(imagecube) THEN imagecube = self._stack
   IF Arg_Present(inverse) THEN inverse = self._inverse
   IF Arg_Present(keep_aspect) THEN keep_aspect = self._keep_aspect
   IF Arg_Present(location) THEN location = self._location
   IF Arg_Present(nointerpolate) THEN nointerpolate = 1 - self._interpolate
   IF Arg_Present(position) THEN position = self._position
   IF Arg_Present(orientation) THEN orientation = self._orientation
   IF Arg_Present(framenumber) THEN framenumber = self._framenumber
   IF Arg_Present(thick) THEN thick = self._thick
   IF Arg_Present(out_xsize) THEN out_xsize = self._out_xsize
   IF Arg_Present(out_ysize) THEN out_ysize = self._out_ysize
   IF Arg_Present(xstart) THEN xstart = self._xstart
   IF Arg_Present(ystart) THEN ystart = self._ystart
   IF Arg_Present(xsize) OR Arg_Present(ysize) THEN BEGIN
      IF Ptr_Valid(self._stack) THEN BEGIN
         theImageFrame = self._stack -> GetFrame(self._framenumber, Thick=self._thick, Orientation=self._orientation)
         s = Size(theImageFrame, /Dimensions)
         xsize = s[0]
         ysize = s[1]
      ENDIF
   ENDIF
   IF Arg_Present(wid) THEN wid = self._wid
   IF Arg_Present(window_position) THEN window_position = self._window_position

      ; Call the superclass method if required
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CatDataAtom::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ImageFrame::MESSAGEHANDLER
;
; PURPOSE:
;
;       This method responds to "messages" sent from other objects. It is called
;       automatically by other objects. To receive messages, it is necessary to
;       "register" with the messaging object.
;
; SYNTAX:
;
;       None. Called by other objects.
;
; ARGUMENTS:
;
;       TITLE:  The message title. This is the "name" of the message indicated when
;               the object registered for messages with the messaging object.
;
; KEYWORDS:
;
;       DATA:   Information from the SENDER that may be relevant in processing the message.
;               Typically, an anonymous structure variable, although it could be anything at all.
;
;       SENDER: An output keyword. This is the object reference of the object that is sending
;               the message.
;-
;*****************************************************************************************************
PRO ImageFrame::MessageHandler, title, SENDER=sender, DATA=data

   ; Initialise the error handler
   @cat_pro_error_handler

   CASE title OF

      'COLORTOOL_TABLECHANGE': BEGIN

            ; If the sender is not the image's colortool object, then update image colors.
            IF sender NE self._colors THEN $
               self._colors -> SetProperty, Red=data.r, Green=data.g, Blue=data.b, Bottom=data.bottom

            ; Redraw the image without erasing.
            self -> Draw


         ENDCASE

      'COLORTOOL_SETPROPERTY': BEGIN

            ; If the sender is not the image's colortool object, then update image colors.
            IF sender NE self._colors THEN BEGIN
               sender -> GetProperty, Red=red, Green=green, Blue=blue
               self._colors -> SetProperty, Red=red, Green=green, Blue=blue
            ENDIF
            self -> Draw

            ; Redraw the image without erasing.
            self -> Draw


         ENDCASE

      ELSE: self -> CATDATAATOM::MessageHandler, title, SENDER=sender, DATA=data


   ENDCASE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ImageFrame::PIXEL_TO_VALUE
;
; PURPOSE:
;
;       This method calculates the original image value underneath a pixel location
;       in the display window. To be accurate, the window that contains the image
;       MUST BE the current graphics window. To be certain of this, use the WID
;       keyword in the INIT or SetProperty methods to associate an image with its
;       display window.
;
; SYNTAX:
;
;       image_value = imageObject -> Pixel_to_Value(x, y)
;
; RETURN_VALUE:
;
;       image_value:   The image value at image[x,y]. For 2D images, a single value. For
;                      true-color images a three-element array representing the RGB values
;                      at that image pixel.
;
; ARGUMENTS:
;
;       X:             The x location in the display window (pixel coordinates).
;
;       Y:             The y location in the display window (pixel coordinates).
;
; OUTPUT_KEYWORDS:
;
;       INSIDE:        Returns a 1 if the (x,y) values are inside the image and 0 otherwise. No values
;                      are returned in other keywords if the point is not inside the image.
;
;       XDATA:         The x data value with respect to the image data coordinate system,
;
;       XPIXEL:        The x pixel value in terms of image (rather than window) device coordinates.
;
;       YDATA:         The y data value with respect to the image data coordinate system,
;
;       YPIXEL:        The y pixel value in terms of image (rather than window) device coordinates.
;-
;*****************************************************************************************************
FUNCTION ImageFrame::Pixel_to_Value, x, y, $
   Inside=inside, $
   XData=xdata, $
   XPixel=x_img, $
   YData=ydata, $
   YPixel=y_img

   @cat_func_error_handler

   currentWindow = !D.Window
   IF Obj_Valid(self._wid) THEN self._wid -> SetWindow

   ; The point must be inside the image, else RETURN.
   inside = self -> Point_Inside(x, y)
   IF inside NE 1 THEN RETURN, 0

  ; Get the image slice from the data cube, if it is available.
  theImageFrame = self._stack -> GetFrame(self._framenumber, Thick=self._thick, Orientation=self._orientation)

   ; Convert the point, which is in device coordinates with respect to the window, into
   ; device coordinates with respect to the image.
   xprime = x - self._location[0,0]
   yprime = y - self._location[1,0]
   x_img = 0 > Round((xprime * Float(self._xsize)) / (self._location[2,0] - self._location[0,0] + 1)) < (self._xsize -1)
   y_img = 0 > Round((yprime * Float(self._ysize)) / (self._location[3,0] - self._location[1,0] + 1)) < (self._ysize -1)

   ; Get the data coordinate system.
   IF Obj_Isa_Valid(self._coords, 'CATCOORD') THEN BEGIN
      self._coords -> Draw
      c = Convert_Coord(x, y, /Device, /To_Data)
      xdata = c[0,0]
      ydata = c[1,0]
   ENDIF ELSE BEGIN
      xdata = x_img
      ydata = y_img
   ENDELSE

   ; Return the image value.
   retValue = (theImageFrame)[x_img, y_img]

   ; Don't return byte values because they make lousy strings. :-(
   IF Size(retValue, /TNAME) EQ 'BYTE' THEN retValue=Fix(retValue)

   ; Set the window back to entering value.
   IF currentWindow GE 0 THEN WSet, currentWindow

   RETURN, retValue

END



;*****************************************************************************************************
;+
; NAME:
;       ImageFrame::POINT_INSIDE
;
; PURPOSE:
;
;       This method returns a 1 if the specified point is inside the image, and a 0
;       if the specified point is outside the image.
;
; SYNTAX:
;
;       is_inside = imageObject -> Point_Inside(x, y)
;
; ARGUMENTS:
;
;       x:      The X value of the point in window or pixel coordinates.
;
;       y:      The Y value of the point in window or pixel coordinates.
;
; KEYWORDS:
;
;       None
;-
;*****************************************************************************************************
FUNCTION ImageFrame::Point_Inside, x, y

   @cat_func_error_handler

   xpix = x / self._location[4,0]
   ypix = y / self._location[5,0]

   ; Assume the point is inside. It it is outside, change the value.
   retValue = 1
   IF (xpix LT self._location[0,1]) OR (xpix GT self._location[2,1]) THEN retValue = 0
   IF (ypix LT self._location[1,1]) OR (ypix GT self._location[3,1]) THEN retValue = 0

   self -> Report, /Completed
   RETURN, retValue

END



;*****************************************************************************************************
;+
; NAME:
;       ImageFrame::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set properties of the ImageFrame object
;
; SYNTAX:
;
;       imageObject -> SETPROPERTY, XSIZE=xsize, YSIZE=ysize
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;     AXES:             Set this keyword to add axes to the image display. Or, set this
;                       keyword to an axes object that is added to the image.
;
;     AXIS_PROPERTY:    Set this keyword to invoke the axes control panel (if there are axes)
;                       in a separate window.
;
;     DISPLAY_MODE:     Images can be displayed in any of three display "modes". Display Mode 0 is
;                       the default and allows images to be positioned in the window using the
;                       POSITION keyword. Display Mode 1 is the standard "TV" display mode in which
;                       the image is positioned with respect to its lower-left corner in the
;                       display window. Display Mode 2 is the standard "TV" display mode in which
;                       the image is positioned in the window according to the WINDOW_POSITION.
;
;     DRAW:             Set this keyword to DRAW the image after it's properties have been set.
;
;     IMAGESTACK:        An object reference to an IMAGESTACK object.
;
;     INVERSE:          If this keyword is set, the image is drawn in inverse color (255-image).
;
;     KEEP_ASPECT:      If this keyword is set, the POSITION of the image in the window
;                       is always adjusted to preserve the image aspect ratio (ratio of height
;                       divided by width). Otherwise, the image is resized arbitrarily. Applies
;                       only to DISPLAY_MODE=0.
;
;     NO_COPY:          If this keyword is set, the image data is transfered directly to the object
;                       and not copied. The image parameter will become undefined if this keyword is set.
;
;     NOINTERPOLATE:    If the image is resized to fit the window, this keyword, if set, will
;                       cause nearest neighbor interpolation of image values to be used. The
;                       default is to use  bilinear interpolation. Applies only to DISPLAY_MODE=0.
;                       Note: True-Color images *always* use bilinear interpolation no matter what
;                       the value of this keyword.
;
;     NULL_FOR_SAVE:    If this keyword is set, any object field in this object that will cause
;                       unwanted object references to be saved in a save file are nulled out.
;
;     ORIENTATION:      Slices can be taken in the X, Y, and Z directions. This keyword accepts
;                       a single character string that selects the directions. "Z": the slice is taken
;                       from the XY plane (the default). "X" the slice is taken from the YZ plane. "Y" the
;                       slice is taken from the XZ plane.
;
;
;     POSITION:         The position of the image in the display window. The position is given
;                       as a four-element array in normalized (0 to 1) coordinates of the form
;                       [x0, y0, x1, y1], where (x0,y0) is the lower-left corner of the image and
;                       (x1,y1) is the upper-right corner of the image. If the KEEP_ASPECT keyword
;                       is set, the image will be located within the specified POSITION in a way
;                       that preserves the aspect ratio of the image. For the image's exact location
;                       in the window after it has been displayed, use the LOCATION keyword to the
;                       GetProperty method.
;
;     FRAMENUMBER:      The index number of the slice desired from the image cube. By default, 0.
;
;     THICK:            Normally, a slice one image frame thick is returned. If this keyword is
;                       set to a value greater than 1, then THICK number of image frames are averaged
;                       together to produce the slice that is returned. The slice will go from number
;                       to (number + thick - 1). If there are not enough image frames to produce a slice
;                       with a particular thickness, then as many frames as possible will be averaged.
;
;     XSIZE:            The desired X size of the image upon output. This is used only for DISPLAY_MODE=1
;                       and DISPLAY_MODE=2.
;
;     XSTART:           The X location (in pixels) of the lower-left corner of the image. Used only
;                       in DISPLAY_MODE=1.
;
;     YSIZE:            The desired Y size of the image upon output. This is used only for DISPLAY_MODE=1
;                       and DISPLAY_MODE=2.
;
;     YSTART:           The Y location (in pixels) of the lower-left corner of the image. Used only
;                       in DISPLAY_MODE=1.
;
;     WID:              The window objectr where this image is to be displayed.
;
;     WINDOW_POSITION:  A single number that gives a "window position" for displaying the image.
;                       The same as calling the TV command with a single positional parameter other
;                       than the image.
;
;     _EXTRA:           Any keyword appropriate for the SETPROPERTY method of the superclass object.
;
;-
;*****************************************************************************************************
PRO ImageFrame::SetProperty, $
   AXES=axes, $
   AXIS_PROPERTY=axis_property, $
   DISPLAY_MODE=display_mode, $
   DRAW=draw, $
   INVERSE=inverse, $
   IMAGESTACK=imageStack, $
   KEEP_ASPECT=keep_aspect, $
   NO_COPY=no_copy, $
   NOINTERPOLATE=nointerpolate, $
   NULL_FOR_SAVE=null_for_save, $
   ORIENTATION=orientation, $
   POSITION=position, $
   FRAMENUMBER=framenumber, $
   THICK=thick, $
   XSIZE=out_xsize, $
   XSTART=xstart, $
   YSIZE=out_ysize, $
   YSTART=ystart, $
   WID=wid, $
   WINDOW_POSITION=window_position, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(display_mode) NE 0 THEN self._display_mode = display_mode
   IF N_Elements(keep_aspect) NE 0 THEN self._keep_aspect = Keyword_Set(keep_aspect)
   IF N_Elements(inverse) NE 0 THEN self._inverse = Keyword_Set(inverse)
   IF N_Elements(nointerpolate) NE 0 THEN self._interpolate = 1 - Keyword_Set(nointerpolate)
   IF N_Elements(position) NE 0 THEN self._position = position
   IF N_Elements(orientation) NE 0 THEN self._orientation = StrUpCase(orientation)
   IF N_Elements(out_xsize) NE 0 THEN self._out_xsize = out_xsize
   IF N_Elements(out_ysize) NE 0 THEN self._out_ysize = out_ysize
   IF N_Elements(framenumber) NE 0 THEN self._framenumber = framenumber
   IF N_Elements(thick) NE 0 THEN self._thick = thick
   IF N_Elements(xstart) NE 0 THEN self._xstart = xstart
   IF N_Elements(ystart) NE 0 THEN self._ystart = ystart
   IF Obj_Valid(wid) NE 0 THEN self._wid = wid
   IF N_Elements(window_position) NE 0 THEN self._window_position = window_position

   ; Did we get an imageStack?
   IF N_Elements(imageStack) NE 0 THEN BEGIN

      ; ImageStack must be an object reference.
      IF Size(imageStack, /TName) NE 'OBJREF' THEN Message, 'The imageStack argument must be an object reference.'
      IF Obj_Valid(self._stack) THEN self._stack -> RemoveParent, self
      self._stack = imageStack
      imageStack -> AddParent, self
      theImageFrame = self._stack -> GetFrame(self._framenumber, Thick=self._thick, Orientation=self._orientation)
      s = Size(theImageFrame, /Dimensions)
      IF self._out_xsize EQ 0 THEN self._out_xsize = s[0]
      IF self._out_ysize EQ 0 THEN self._out_ysize = s[1]
      self._xsize = s[0]
      self._ysize = s[1]

      ; Are there axes with this image. They may need to be changed.
      ; Update their range, if requested.

      containedObjects = self -> Get(/All)
      FOR jj=0,N_Elements(containedObjects)-1 DO BEGIN

         IF Obj_Class(containedObjects[jj]) EQ 'IMGAXES' THEN BEGIN
            answer = Dialog_Message('Update image axes?', /Question)
            IF StrUpCase(answer) EQ 'YES' THEN BEGIN
               self -> GetProperty, XSize=xsize, YSize=ysize
               containedObjects[jj] -> SetProperty, XRange=[0,xsize], YRange=[0,ysize]
            ENDIF
         ENDIF
      ENDFOR

   ENDIF

   ; Do you need axes for this image?
   IF Keyword_Set(axes) THEN BEGIN

      containedObjects = self -> Get(/All)
      FOR jj=0,N_Elements(containedObjects)-1 DO BEGIN

         IF Obj_Class(containedObjects[jj]) EQ 'IMGAXES' THEN $
            IF Obj_Valid(containedObjects[jj]) THEN self -> Remove, containedObjects[jj]
      ENDFOR

     IF Obj_ISA_Valid(axes, 'IMGAXES') THEN self -> Add, axes $
        ELSE BEGIN
           self -> GetProperty, XSIZE=xsize, YSIZE=ysize
           self -> Add, Obj_New('IMGAXES', Position=self._location[0:3, 1], XRange=[0,xsize], YRange=[0,ysize])
        ENDELSE

   ENDIF

   ; Need axes properties?
   IF Keyword_Set(axis_property) THEN BEGIN

      ; Are there axes?
      containedObjects = self -> Get(/All)
      FOR j=0,N_Elements(containedObjects)-1 DO BEGIN

         IF Obj_Class(containedObjects[j]) EQ 'IMGAXES' THEN $
            containedObjects[j] -> ControlPanel

      ENDFOR

   ENDIF

      ; Call the superclass SetProperty method
   self -> CatDataAtom::SetProperty, _EXTRA=extraKeywords
   IF Keyword_Set(draw) THEN self -> Draw

   ; Null this object out in preparation for saving.
   IF Keyword_Set(null_for_save) THEN BEGIN
      self._stack = Obj_New()
      self._wid = Obj_New()
   ENDIF

      ; Report status
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ImageFrame::SHOW
;
; PURPOSE:
;
;       This method attempts to pull the draw widget containing the image forward on
;       the display, if possible.
;
; SYNTAX:
;
;           imageObject -> SHOW
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
PRO ImageFrame::Show

   @cat_pro_error_handler

   IF Obj_Valid(self._wid) THEN self._wid -> Show

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ImageFrame::CLEANUP
;
; PURPOSE:
;
;       This is the ImageFrame object class destructor method.
;
; SYNTAX:
;
;       Called automatically when the object is destroyed thus:
;
;           OBJ_DESTROY, imageObject
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       _EXTRA:  Any keyword appropriate for the DATAATOM::CLEANUP method.
;-
;*****************************************************************************************************
PRO ImageFrame::CLEANUP, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Obj_Valid(self._stack) THEN self._stack -> RemoveParent, self

   self -> CatDataAtom::CLEANUP, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       ImageFrame::INIT
;
; PURPOSE:
;
;       This method is used upon object creation.
;
; SYNTAX:
;
;       Called automatically when the object is created thus:
;
;           imageObject = OBJ_NEW ('ImageFrame', imageStackObject)
;
; ARGUMENTS:
;
;       imageStack       The object reference to an IMAGESTACK object.
;
; KEYWORDS:
;
;     AXES:             Set this keyword to add axes to the image display. Or, set this
;                       keyword to an axis object.
;
;     DISPLAY_MODE:     Images can be displayed in any of three display "modes". Display Mode 0 is
;                       the default and allows images to be positioned in the window using the
;                       POSITION keyword. Display Mode 1 is the standard "TV" display mode in which
;                       the image is positioned with respect to its lower-left corner in the
;                       display window. Display Mode 2 is the standard "TV" display mode in which
;                       the image is positioned in the window according to the WINDOW_POSITION.
;
;     INVERSE:          If this keyword is set, the image is drawn in inverse color (255-image).
;
;     KEEP_ASPECT:      If this keyword is set, the POSITION of the image in the window
;                       is always adjusted to preserve the image aspect ratio (ratio of height
;                       divided by width). Otherwise, the image is resized arbitrarily. Applies
;                       only to DISPLAY_MODE=0.
;
;     NO_COPY:          If this keyword is set, the image data is transfered directly to the object
;                       and not copied. The image parameter will become undefined if this keyword is set.
;
;     NOINTERPOLATE:    If the image is resized to fit the window, this keyword, if set, will
;                       cause nearest neighbor interpolation of image values to be used. The
;                       default is to use  bilinear interpolation. Applies only to DISPLAY_MODE=0.
;
;     ORIENTATION:      Slices can be taken in the X, Y, and Z directions. This keyword accepts
;                       a single character string that selects the directions. "Z": the slice is taken
;                       from the XY plane (the default). "X" the slice is taken from the YZ plane. "Y" the
;                       slice is taken from the XZ plane.
;
;
;     POSITION:         The position of the image in the display window. The position is given
;                       as a four-element array in normalized (0 to 1) coordinates of the form
;                       [x0, y0, x1, y1], where (x0,y0) is the lower-left corner of the image and
;                       (x1,y1) is the upper-right corner of the image. If the KEEP_ASPECT keyword
;                       is set, the image will be located within the specified POSITION in a way
;                       that preserves the aspect ratio of the image. For the image's exact location
;                       in the window after it has been displayed, use the LOCATION keyword to the
;                       GetProperty method.
;
;     FRAMENUMBER:      The index number of the slice desired from the image cube. By default, 0.
;
;     THICK:            Normally, a slice one image frame thick is returned. If this keyword is
;                       set to a value greater than 1, then THICK number of image frames are averaged
;                       together to produce the slice that is returned. The slice will go from number
;                       to (number + thick - 1). If there are not enough image frames to produce a slice
;                       with a particular thickness, then as many frames as possible will be averaged.
;
;     XSIZE:            The desired X size of the image upon output. This is used only for DISPLAY_MODE=1
;                       and DISPLAY_MODE=2.
;
;     XSTART:           The X location (in pixels) of the lower-left corner of the image. Used only
;                       in DISPLAY_MODE=1.
;
;     YSIZE:            The desired Y size of the image upon output. This is used only for DISPLAY_MODE=1
;                       and DISPLAY_MODE=2.
;
;     YSTART:           The Y location (in pixels) of the lower-left corner of the image. Used only
;                       in DISPLAY_MODE=1.
;
;     WID:              The window object where this image is to be displayed.
;
;     WINDOW_POSITION:  A single number that gives a "window position" for displaying the image.
;                       The same as calling the TV command with a single positional parameter other
;                       than the image.
;
;     _EXTRA:           Any keyword appropriate for the INIT method of the superclass object.
;-
;*****************************************************************************************************
FUNCTION ImageFrame::INIT, imageStack, $
   AXES=axes, $
   DISPLAY_MODE=display_mode, $
   INVERSE=inverse, $
   KEEP_ASPECT=keep_aspect, $
   NO_COPY=no_copy, $
   NOINTERPOLATE=nointerpolate, $
   ORDER=order, $
   ORIENTATION=orientation, $
   POSITION=position, $
   FRAMENUMBER=framenumber, $
   THICK=thick, $
   XSIZE=out_xsize, $
   XSTART=xstart, $
   YSIZE=out_ysize, $
   YSTART=ystart, $
   WID=wid, $
   WINDOW_POSITION=window_position, $
   _EXTRA=extraKeywords

   @cat_func_error_handler

   ; Handle keywords that are not going to be set to zero by default.
   IF N_Elements(display_mode) EQ 0 THEN display_mode = 0
   inverse = Keyword_Set(inverse)
   keep_aspect = Keyword_Set(keep_aspect)
   nointerpolate = Keyword_Set(nointerpolate)
   order = Keyword_Set(order)
   IF N_Elements(orientation) EQ 0 THEN orientation = 'Z' ELSE orientation = StrUpCase(orientation)
   IF N_Elements(position) EQ 0 THEN position = [0.0, 0.0, 1.0, 1.0]
   IF N_Elements(framenumber) EQ 0 THEN framenumber = 0
   IF N_Elements(thick) EQ 0 THEN thick = 1 ELSE thick = (1 > Round(thick))

   ; Call the superclass INIT method
   ok = self -> CatDataAtom::INIT (_EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; Did we get an imageStack?
   IF N_Elements(imageStack) NE 0 THEN BEGIN

      ; ImageStack must be an object reference.
      IF Size(imageStack, /TName) NE 'OBJREF' THEN Message, 'The ImageStack argument must be an object reference.'
      self._stack = imageStack
      imageStack -> AddParent, self
      theImageFrame = self._stack -> GetFrame(framenumber, Thick=thick, Orientation=orientation)
      s = Size(theImageFrame, /Dimensions)
      IF N_Elements(out_xsize) EQ 0 THEN self._out_xsize = s[0] ELSE self._out_xsize = out_xsize
      IF N_Elements(out_ysize) EQ 0 THEN self._out_ysize = s[1] ELSE self._out_ysize = out_ysize
      self._xsize = s[0]
      self._ysize = s[1]
   ENDIF

   ; Populate the object.
   self._display_mode = display_mode
   self._interpolate = 1 - nointerpolate
   self._inverse = inverse
   self._keep_aspect = keep_aspect
   self._order = order
   self._orientation = orientation
   self._position = position
   self._framenumber = framenumber
   self._thick = thick
   IF Obj_Valid(wid) NE 0 THEN self._wid = wid

   ; Do you need axes for this image?
   IF Keyword_Set(axes) THEN BEGIN

     IF Obj_ISA_Valid(axes, 'IMGAXES') THEN self -> Add, axes $
        ELSE BEGIN
           self -> GetProperty, XSIZE=xsize, YSIZE=ysize
           self -> Add, Obj_New('IMGAXES', Position=self._location[0:3, 1], XRange=[0,xsize], $
            YRange=[0,ysize], Tickformat='(I5)')
        ENDELSE

   ENDIF

   ; Register for messages from the COLORTOOL object.
   self -> GetProperty, Color_Object = colors
   IF Obj_Valid(colors) THEN colors -> RegisterForMessage, self, 'COLORTOOL_TABLECHANGE'

   ; Report and return status
   self -> Report, /Completed
   RETURN, 1
END



;*****************************************************************************************************
;
; NAME:
;       ImageFrame CLASS DEFINITION
;
; PURPOSE:
;
;       This is the class definition code for the ImageFrame object.
;
;*****************************************************************************************************
PRO ImageFrame__DEFINE, class

    class = { ImageFrame, $                    ; The ImageFrame object class.
              _stack        : Obj_New(), $     ; The IMAGESTACK object.
              _display_mode : 0B, $            ; A flag indicating which display method to use for drawing.
              _inverse      : 0B, $            ; If this flag is set, the image is drawn in inverse color.
              _keep_aspect  : 0L, $            ; Flag: Keep the aspect ratio when calculating image position.
              _location     : DblArr(6,2), $   ; Location information about the "displayed" image.
              _interpolate  : 0L, $            ; Flag: When set, bilinear interpolation is used.
              _order        : 0L, $            ; Image display order.
              _out_xsize    : 0L, $            ; The output X size of the image. (Display_Mode=1 and Display_Mode=2)
              _out_ysize    : 0L, $            ; The output Y size of the image. (Display_Mode=1 and Display_Mode=2)
              _position     : DblArr(4), $     ; The position of the image in the output window. (Display_Mode=0)
              _xsize        : 0L, $            ; The X size of the image.
              _xstart       : 0L, $            ; The starting X location for displaying image. (Display_Mode=1)
              _ysize        : 0L, $            ; The Y size of the image.
              _ystart       : 0L, $            ; The starting Y location for displaying image. (Display_Mode=1)
              _wid          : Obj_New(), $     ; The window object where this image should be displayed.
              _window_position: 0L, $          ; The window position for displaying this image. (Display_Mode=2)
              _orientation  : "", $            ; The orientation of the slice: "X", "Y", or "Z" (the default).
              _framenumber  : 0L, $            ; The frame number.
              _thick        : 0L, $            ; The "thickness" of the image frame. (See IMAGESTACK documentation.)
              INHERITS CatDataAtom $           ; Subclassed from CATDATAATOM.
            }

END

