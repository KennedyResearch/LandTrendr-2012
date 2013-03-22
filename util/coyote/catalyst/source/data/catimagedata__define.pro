;*****************************************************************************************************
;+
; NAME:
;       CatImageData
;
; PURPOSE:
;
;       This object implements a class for handling image data. It is a subclassed
;       CATDATAATOM object and can be subclassed for specific image implementations.
;       This object was an early attempt at creating an image object for the Catalyst
;       Library, and has been depreciated it favor of CatImage. It is left in the Library
;       for historical reasons and because there are Catalyst applications that use it.
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
;       imageObject = OBJ_NEW ('CatImageData', image)
;
; MESSAGES:
;
;    If a new image is loaded with the LOADIMAGE method, a CATIMAGE_NEWIMAGE message is sent to registered users.
;
; CLASS DEFINITION:
;
;    class = { CatImageData, $                  ; The CatImageData object class.
;              _backgroundColor: "", $          ; The name of a background color to use when ERASING.
;              _bytsclmin  : 0.0D, $            ; The MIN value when byte scaling the data for display.
;              _bytsclmax  : 0.0D, $            ; The MAX value when byte scaling the data for display.
;              _directory  : "", $              ; The directory to look in for reading image files.
;              _erase      : 0L, $              ; Flag: Erase the window before displaying image.
;              _filename   : "", $              ; The name of the image file that has been read.
;              _interpolate: 0L, $              ; Flag: Interpolate when resizing. Nearest neighbor is default.
;              _keep_aspect: 0L, $              ; Flag: Keep the aspect ratio when calculating image position.
;              _location   : FltArr(6,2), $     ; Location information about the "displayed" image.
;              _original   : Ptr_New(), $       ; The original image data
;              _noresize   : 0L, $              ; Flag: No display resizing.
;              _position   : FltArr(4), $       ; The position of the image in the output window.
;              _wid        : 0L, $              ; The window this image should be drawn in.
;              _xsize      : 0L, $              ; The X size of the image.
;              _ysize      : 0L, $              ; The Y size of the image.
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
;       CatImageData::IP
;
; PURPOSE:
;
;       This method creates an image processing tool (CATIPTOOL) to perform an image
;       processing function, and executes the tool process.
;
; SYNTAX:
;
;       processedImage = imageObject -> IP(theProcessName)
;
; ARGUMENTS:
;
;       theProcessName: The name of an image processing command. For example, "SMOOTH".
;
;       p1:             This is the first additional parameter of the image processing command.
;                       The *first* parameter of the image processing command is always the
;                       image data obtained from the targetObject with the GETDATA method. For
;                       example, in this case P1 will be equal to 7:
;
;                         smoothObj = Obj_New('CATIPTOOL', 'Smooth', 7)
;
;                       And the command that will be executed is this:
;
;                         targetObject -> SetData, Smooth(targetObject->GetData(), 7)
;
;       p2:             This is the second additional parameter of the image processing command.
;
;       p3:             This is the third additional parameter of the image processing command.
;
; KEYWORDS:
;
;       DESTROY:        Normally, the image processing object created in this method is added
;                       to the tool list of the image. If the DESTROY keyword is set, however,
;                       the new object is destroyed and not added to the image tool list. This
;                       is appropriate for image processing tasks that can't be undone, for
;                       example.
;
;       DRAW:           Set this keyword to DRAW the image after the processing has been applied.
;
;       ORIGINAL:       Set this keyword to apply the image processing operation to the original
;                       image data and not to the display image data.
;
;       _EXTRA:         Any keyword appropriate for the INIT method of the image processing tool.
;
;-
;*****************************************************************************************************
PRO CatImageData::IP, theProcessName, p1, p2, p3, $
   DESTROY=destroy, $
   DRAW=draw, $
   ORIGINAL=original, $
   _Extra=extrakeywords

   @cat_pro_error_handler

   ; Must have something to apply process to.

   IF Ptr_Valid(self._dataPtr) EQ 0 THEN Message, 'There is no current image to process.'

   ; The image processing object must have an APPLY method that applies the image
   ; processing functionality.

   theProcessObject = Obj_New('CATIPTOOL', theProcessName, TARGETOBJECT=self, p1, p2, p3, $
      _Extra=extrakeywords, ORIGINAL=original)
   IF Obj_IsA_Valid(theProcessObject, 'CATIPTOOL') THEN $
   BEGIN

      ; Apply the image processing process.
      theProcessObject -> Apply

      ; Add the object to the tool list of this image object, or destory it if necessary.
      IF Keyword_Set(destroy) THEN Obj_Destroy, theProcessObject ELSE self -> AddTool, theProcessObject

   ENDIF

   ; Do we need a draw?

   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CatImageData::DRAW
;
; PURPOSE:
;
;       This method draws the image to the current window at the size
;       specified by the xsize and ysize properties. These default to
;       the image size. If either/both are zero nothing is drawn.
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
;       NOCOLORS:  Set this keyword if you just want to draw the image
;                  without first loading the image color tables. This is
;                  useful if you want to see the image with a temporary
;                  color table.
;
;       NOCOORDS:  Set this keyword to display the image without first
;                  drawing the COORDINATE object.
;
;       _EXTRA:    Any keyword appropriate for the DRAW method of the superclass objects.
;
;-
;*****************************************************************************************************
PRO CatImageData::Draw, NoColors=nocolors, NoCoords=nocoords, _Extra=extraKeywords


   ; Set up the error handler.

   @cat_pro_error_handler

   ; If there is no image to display, try to load one.

   IF Ptr_Valid(self._dataPtr) EQ 0 THEN $
   BEGIN
      self -> LoadImage, Cancel=cancelled
      IF cancelled THEN RETURN
   ENDIF

   ; Gather information about the image.

  maxDisplay = Max (*self._dataPtr, Min=minDisplay)
  typeDisplay = Size(*self._dataPtr, /TNAME)
  dimensions = Image_Dimensions(*self._dataPtr, XSize=xsize, YSize=ysize, TrueIndex=trueIndex, $
         XIndex=xindex, YIndex=yindex)

  pixelInterleaving = trueIndex + 1
  needscaling = 0
  IF (minDisplay LT 0) THEN needscaling = 1
  IF maxDisplay GT 255 THEN needscaling = 1
  IF self._bytsclmin GT minDisplay THEN needscaling = 1
  IF self._bytsclmax LT maxDisplay THEN needscaling = 1

   ; Set to display window.
   IF Obj_Valid(self._wid) THEN self._wid -> SetWindow ELSE BEGIN

      ; Whoops! Can't find one. Traverse your hierarchy looking for a draw widget or
      ; pixmap. If you find one, use that as your display window.
      parent = self
      WHILE Obj_Valid(parent) DO BEGIN
         parent -> GetProperty, First_Parent=first_parent
         IF Obj_IsA_Valid(first_parent, 'PIXMAPWIDGET') OR Obj_Isa_Valid(first_parent, 'DRAWWIDGET') THEN BEGIN
            self._wid = first_parent
            BREAK
         ENDIF ELSE parent = first_parent
      ENDWHILE

      ; Still can't find a window to draw in? Draw in the current graphics window.

   ENDELSE

   ; If no resize, then task is simple. Do it and RETURN.
   IF self._noresize THEN $
   BEGIN

      Device, Get_Decomposed=theState
      IF trueIndex GE 0 THEN Device, Decomposed=1 ELSE Device, Decomposed=0

      ; Calculate the image size and start locations.

      xstart = self._position[0] * !D.X_VSIZE
      ystart = self._position[1] * !D.Y_VSIZE

      ; Set up colors and coordinates (if they exist).

      IF self._erase THEN Erase, Color = self._colors -> GetColor(self._backgroundColor)

      IF NOT Keyword_Set(nocolors) THEN self -> ApplyColors
      IF NOT Keyword_Set(nocoords) THEN self -> ApplyCoords
      IF needscaling THEN $
      BEGIN
         diff = maxDisplay - minDisplay
         TV, BytScl(*self._dataPtr, Top=(diff < 255), MIN=self._bytsclmin, MAX=self._bytsclmax), $
            self._position[0], self._position[1], /Normal, True=pixelInterleaving, Order=self._order
      ENDIF $
      ELSE TV, *self._dataPtr, self._position[0], self._position[1], $
         True=pixelInterleaving, /Normal, Order=self._order
      Device, Decomposed=theState

      self._location[*,0] = [Floor(xstart), Floor(ystart), $
                             Round(Floor(xstart) + Round(xsize)), Round(Floor(ystart) + Round(ysize)), $
                             Double(!D.X_VSize), Double(!D.Y_VSize)]
      self._location[*,1] = [ self._location[0,0]/self._location[4,0], $
                              self._location[1,0]/self._location[5,0], $
                              self._location[2,0]/self._location[4,0], $
                              self._location[3,0]/self._location[5,0], $
                              self._location[4,0]/self._location[4,0], $
                              self._location[5,0]/self._location[5,0] ]
      RETURN

   ENDIF

   ; More complicated image display coming up.

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

      ; Display the image. Sizing different for scalable pixels devices.

   IF (!D.Flags AND 1) NE 0 THEN $
   BEGIN

      IF self._erase THEN Erase

      ; Set up colors and coordinates (if they exist).

      IF NOT Keyword_Set(nocolors) THEN self -> ApplyColors
      IF NOT Keyword_Set(nocoords) THEN self -> ApplyCoords
      IF needscaling THEN $
         BEGIN
            diff = maxDisplay - minDisplay
            TV, BytScl(*self._dataPtr, Top=(diff < 255), MIN=self._bytsclmin, MAX=self._bytsclmax), $
               xstart, ystart, XSIZE=xsize, YSIZE=ysize, True=pixelInterleaving, Order=self._order
         ENDIF ELSE TV, *self._dataPtr, xstart, ystart, XSIZE=xsize, YSIZE=ysize, True=pixelInterleaving, Order=self._order
      self._location[*,0] = [Floor(xstart), Floor(ystart), $
                             Round(Floor(xstart) + Round(xsize)), Round(Floor(ystart) + Round(ysize)), $
                             Double(!D.X_VSize), Double(!D.Y_VSize)]
      self._location[*,1] = [ self._location[0,0]/self._location[4,0], $
                              self._location[1,0]/self._location[5,0], $
                              self._location[2,0]/self._location[4,0], $
                              self._location[3,0]/self._location[5,0], $
                              self._location[4,0]/self._location[4,0], $
                              self._location[5,0]/self._location[5,0] ]

   ENDIF ELSE $
   BEGIN ; All other devices.

      Device, Get_Decomposed=theState
      IF trueIndex GE 0 THEN Device, Decomposed=1 ELSE Device, Decomposed=0

      ; Set up colors and coordinates (if they exist).

      IF self._erase THEN $
         Erase, Color = self._colors -> GetColor(self._backgroundColor)
      IF NOT Keyword_Set(nocolors) THEN self -> ApplyColors
      IF NOT Keyword_Set(nocoords) THEN self -> ApplyCoords

      CASE pixelInterleaving OF
         0: IF needscaling THEN $
            BEGIN
               diff = maxDisplay - minDisplay
               TV, BytScl(Congrid(*self._dataPtr, CEIL(xsize), CEIL(ysize), INTERP=self._interpolate, $
                  MINUS_ONE=1), Top=(diff < 255), MIN=self._bytsclmin, MAX=self._bytsclmax), xstart, ystart, $
                  XSIZE=xsize, YSIZE=ysize, True=pixelInterleaving, Order=self._order
            ENDIF ELSE TV, Congrid(*self._dataPtr, CEIL(xsize), CEIL(ysize), INTERP=self._interpolate, $
               MINUS_ONE=1), ROUND(xstart), ROUND(ystart), True=pixelInterleaving, Order=self._order

         1: IF needscaling THEN $
            BEGIN
               diff = maxDisplay - minDisplay
               TV, BytScl(Congrid(*self._dataPtr, 3, CEIL(xsize), CEIL(ysize), INTERP=self._interpolate, $
                  MINUS_ONE=1), Top=(diff < 255), MIN=self._bytsclmin, MAX=self._bytsclmax), xstart, ystart, $
                  XSIZE=xsize, YSIZE=ysize, True=pixelInterleaving, Order=self._order
            ENDIF ELSE TV, Congrid(*self._dataPtr, 3, CEIL(xsize), CEIL(ysize), INTERP=self._interpolate, $
               MINUS_ONE=1), ROUND(xstart), ROUND(ystart), True=pixelInterleaving, Order=self._order


         2: IF needscaling THEN $
            BEGIN
               diff = maxDisplay - minDisplay
               TV, BytScl(Congrid(*self._dataPtr, CEIL(xsize), 3, CEIL(ysize), INTERP=self._interpolate, $
                  MINUS_ONE=1), Top=(diff < 255), MIN=self._bytsclmin, MAX=self._bytsclmax), xstart, ystart, $
                  XSIZE=xsize, YSIZE=ysize, True=pixelInterleaving, Order=self._order
            ENDIF ELSE TV, Congrid(*self._dataPtr, CEIL(xsize), 3, CEIL(ysize), INTERP=self._interpolate, $
               MINUS_ONE=1), ROUND(xstart), ROUND(ystart), True=pixelInterleaving, Order=self._order

         3: IF needscaling THEN $
            BEGIN
               diff = maxDisplay - minDisplay
               TV, BytScl(Congrid(*self._dataPtr, CEIL(xsize), CEIL(ysize), 3, INTERP=self._interpolate, $
                  MINUS_ONE=1), Top=(diff < 255), MIN=self._bytsclmin, MAX=self._bytsclmax), xstart, ystart, $
                  XSIZE=xsize, YSIZE=ysize, True=pixelInterleaving, Order=self._order
            ENDIF ELSE TV, Congrid(*self._dataPtr, CEIL(xsize), CEIL(ysize), 3, INTERP=self._interpolate, $
               MINUS_ONE=1), ROUND(xstart), ROUND(ystart), True=pixelInterleaving , Order=self._order

      ENDCASE

      Device, Decomposed=theState
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
;       CatImageData::GetData
;
; PURPOSE:
;
;       This function method enables data to be retreived from this object. It is written
;       for internal use and should not be called without due consideration. All data
;       handling should be done inside the object.
;
; SYNTAX:
;
;       data = imageObject -> GetData (Success=s)
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       ORIGINAL: Set this keyword to obtain the original image data, rather than the display data.
;
;       SUCCESS:  This flag shows whether the get has been successful or not.
;
;-
;*****************************************************************************************************
FUNCTION CatImageData::GetData, Original=original, Success=success

   ; Default error handler
   @cat_func_error_handler

   IF Keyword_Set(original) THEN BEGIN

      ; Attempt to get the original data from the pointer.
      success = Ptr_VALID (self._original)
      IF NOT success THEN result = 0 $
      ELSE result = *self._original

   ENDIF ELSE BEGIN

      ; Attempt to get data from the data pointer.
      success = PTR_VALID (self._dataPtr)
      IF NOT success THEN result = 0 $
      ELSE result = *self._dataPtr

   ENDELSE

   ; Report completion and return result
   self -> Report, /Completed

   RETURN, result

END



;*****************************************************************************************************
;+
; NAME:
;       CatImageData::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to get properties of the CatImageData object
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
;       ASPECT_RATIO:     The ratio of ysize/xsize for the image.
;
;       AXES:             The image axes object for this image. If not available, a null object.
;
;       BYTSCLMAX:        The maximum value (MAX keyword) for byte scaling the image for display.
;
;       BYTSCLMIN:        The minimum value (MIN keyword) for byte scaling the image for display.
;
;       BACKGROUNDCOLOR:  The name of the background color.
;
;       ERASE:            Erase flag.
;
;       FILENAME:         The name of the image file.
;
;       DIMENSIONS:       A two- or three-element array representing the dimensions of the image.
;
;       DIRECTORY:        The starting location to search for images when reading image
;                         files. This directory is updated to reflect the last directory
;                         used when reading an image file.
;
;       DISPLAY_IMAGE:    A copy of the display image.
;
;       DISPLAY_RANGE:    The minimum and maximum value of the display image as a two-element array.
;
;       IMAGE:            A copy of the original image.
;
;       INTERPOLATE:      Interpolate flag.
;
;       KEEP_ASPECT:      Keep aspect ratio flag.
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
;       NORESIZE:         No resize flag.
;
;       POSITION:         A four-element vector giving the position of the image in the
;                         display window in normalized coordinates: [x0, y0, x1, y1].
;
;       RANGE:            The minimum and maximum value of the original image as a two-element array.
;
;       TOOLDATA:         A copy of the image that is to be processed. (The same as DISPLAY_IMAGE.
;                         This keyword MUST be defined to work with TOOLATOM objects.)
;
;       TRUEINDEX:        The position of the "true color" index. It is -1 for 2D images. Add one
;                         to get the "pixel interleaving" value for this image.
;
;       WID:              The window object where this image will be displayed.
;
;       XINDEX:           The index of the X dimension of the image in the DIMENSIONS variable.
;
;       XSIZE:            The X size of the image.
;
;       YINDEX:           The index of the Y dimension of the image in the DIMENSIONS variable.
;
;       YSIZE:            The Y size of the image.
;
;       UNDOLIMIT:        The number of images kept in the undo buffer.
;
;       _REF_EXTRA:       Any keyword appropriate for the DATAATOM::GETPROPERTY method.
;
;-
;*****************************************************************************************************
PRO CatImageData::GetProperty,$
   ASPECT_RATIO=aspect_ratio, $
   AXES=axes, $
   BACKGROUNDCOLOR=backgroundColor, $
   BYTSCLMAX=bytsclmax, $
   BYTSCLMIN=bytsclmin,$
   DIRECTORY=directory, $
   ERASE=erase, $
   FILENAME=filename, $
   DISPLAY_IMAGE=display_image, $
   DISPLAY_RANGE=display_range, $
   IMAGE=image, $
   INTERPOLATE=interpolate, $
   KEEP_ASPECT=keep_aspect, $
   LOCATION=location, $
   NORESIZE=noresize, $
   POSITION=position, $
   RANGE=range, $
   TOOLDATA=tooldata, $
   TRUEINDEX=trueindex, $
   WID=wid, $
   XINDEX=xindex, $
   XSIZE=xsize, $
   YINDEX=yindex, $
   YSIZE=ysize, $
    _REF_EXTRA=extraKeywords

      ; Initialise the error handler
   @cat_pro_error_handler

      ; Create the return variables

   IF Arg_Present(aspect_ratio) THEN aspect_ratio = Float(self._ysize)/self._xsize
   IF Arg_Present(axes) THEN axes = self -> Get('ImageAxes')
    backgroundColor = self._backgroundColor
   directory = self._directory
   display_position = self._location[0:3, 1]
   erase = self._erase
   filename = self._filename
   IF Arg_Present(display_image) THEN IF Ptr_Valid(self._dataPtr) THEN display_image = *self._dataPtr
   IF Arg_Present(tooldata) THEN IF Ptr_Valid(self._dataPtr) THEN tooldata = *self._dataPtr
   IF Arg_Present(display_image) THEN display_image = self -> GetData()
   IF Arg_Present(display_range) THEN BEGIN
      display_image = self -> GetData()
      display_range = [Min(display_image), Max(display_image)]
   ENDIF
   IF Arg_Present(range) THEN BEGIN
      IF Ptr_Valid(self._original) THEN range = [Min(*self._original), Max(*self._original)]
   ENDIF
   IF Arg_Present(image) THEN IF Ptr_Valid(self._original) THEN image = *self._original
   interpolate = self._interpolate
   keep_aspect = self._keep_aspect
   location = self._location
   noresize = self._noResize
   position = self._position
   bytsclmin = self._bytsclmin
   bytsclmax = self._bytsclmax
   wid = self._wid

   IF Ptr_Valid(self._original) THEN $
      dimensions = Image_Dimensions(*self._original, XSize=xsize, YSize=ysize, TrueIndex=trueIndex, XIndex=xindex, YIndex=yindex)

      ; Call the superclass method if required
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CatDataAtom::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CatImageData::LOADCT
;
; PURPOSE:
;
;       This method loads a pre-determined color table for the image. The parameters
;       and keywords are passed along to the image's built-in color table.
;
; SYNTAX:
;
;       imageObject -> LoadCT, colorIndex
;
; ARGUMENTS:
;
;       colorIndex: The color table index number to load. If missing, 0 is assumed.
;
; KEYWORDS:
;
;       DRAW:       If this keyword is set, the DRAW method is called after the colors
;                   have been loaded.
;-
;*****************************************************************************************************
PRO CatImageData::LoadCT, index, DRAW=draw

   @cat_pro_error_handler

   ; Check parameters.

   IF N_Elements(index) EQ 0 THEN index = 0


   IF Obj_Valid(self._colors) EQ 0 THEN self._colors = Obj_New('Colortool')
   self._colors -> LoadCT, index, DRAW=draw

   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CatImageData::LOADIMAGE
;
; PURPOSE:
;
;       This method is used to load images into the object.
;
; SYNTAX:
;
;       imageObject -> LOADIMAGE
;
; ARGUMENTS:
;
;       None.
;
; INPUT KEYWORDS:
;
;       BMP:            Set this keyword to select BMP files.
;
;       COLORTABLE:     The index number of a standard color table to use for this image.
;                       The image is displayed with a gray-scale color table by default.
;
;       DICOM:          Set this keyword to select DICOM files.
;
;       DIRECTORY:      The initial input directory name. The current directory by default.
;
;       DEMO:           Set this keyword to load demonstration files via LOADDATA rather than SELECTIMAGE.
;                       Setting this keyword will disable all other keywords.
;
;       DRAW:           Set this keyword to immediately draw the image after loading.
;
;       FILENAME:       The initial filename. If the initial directory has image files of the
;                       correct type, the default is to display the first of these files. Otherwise, blank.
;
;       FLIPIMAGE:      Set this keyword if you wish to flip the image from its current orientation.
;                       Setting this keyword reverses the Y dimension of the image.
;
;       _EXTRA:         This keyword is used to collect and pass keywords on to the FSC_FILESELECT
;                       object. See the code for FSC_FILESELECT for details.
;
;       GIF:            Set this keyword to select GIF files. This capability is not available in IDL 5.4 and higher.
;
;       GROUP_LEADER:   Set this keyword to a widget identifier group leader. This keyword MUST be
;                       set when calling this program from another widget program to guarantee modal operation.
;
;       JPEG:           Set this keyword to select JPEG files.
;
;       ONLY2D:         Set this keyword if you only want to allow 2D image arrays to be loaded.
;
;       ONLY3D:         Set this keyword if you only want to allow 3D or true-color image arrays to be loaded.
;
;       PICT:           Set this keyword to select PICT files.
;
;       PNG:            Set this keyword to select PNG files.
;
;       PREVIEWSIZE:    Set this keyword to the maximum size (in pixels) of the preview window. Default is 150.
;
;       TIFF:           Set this keyword to select TIFF files. (This is the default filter selection.)
;
; OUTPUT KEYWORDS:
;
;       CANCEL:        This keyword is set to 1 if the user exits the program in any way except
;                      hitting the ACCEPT button. The ACCEPT button will set this keyword to 0.
;
;       FILEINFO:      This keyword returns information about the selected file. Obtained from the QUERY_**** functions.
;
;       OUTDIRECTORY:  The directory where the selected file is found.
;
;       OUTFILENAME:   The short filename of the selected file.
;-
;*****************************************************************************************************
PRO CatImageData::LoadImage, $
   BMP=bmp, $                      ; Set this keyword to select BMP files.
   Cancel=cancel, $                ; An output keyword. Returns 0 if the ACCEPT button is used, 1 otherwise.
   Demo=demo, $                    ; Set this keyword to choose demo files using LOADDATA.
   ColorTable=colortable, $        ; Set this to an index of a color table to load into the ColorTool object of the image.
   Dicom=dicom, $                  ; Set this keyword to select DICOM files
   Directory=directory, $          ; Initial directory to search for files.
   Draw=draw, $                    ; Set this keyword to immediately draw the image after loading.
   FileInfo=fileInfo, $            ; An output keyword containing file information from the Query_*** routine.
   Filename=filename, $            ; Initial file name of image file.
   Flipimage=flipimage, $          ; Set this keyword to flip the Y indices of the image. Set to 0 by default.
   _Extra=extra, $                 ; This is used to pass keywords on to FSC_FILESELECT. See that documentation for details.
   GIF=gif, $                      ; Set this keyword to select GIF files
   Group_Leader=group_leader, $    ; The group leader ID of this widget program.
   JPEG=jpeg, $                    ; Set this keyword to select JPEG files
   Only2D=only2d, $                ; Set this keyword to only allow 2D images to be accepted.
   Only3D=only3d, $                ; Set this keyword to only allow 3D or true-color images to be accepted.
   OutDirectory=outdirectory, $    ; The directory name of the selected image file.
   OutFilename=outfilename, $      ; The short filename (without directory) of the selected image file.
   PICT=pict, $                    ; Set this keyword to select PICT files
   PNG=png, $                      ; Set this keyword to select PNG files
   TIFF=tiff, $                    ; Set this keyword to select TIFF files
   PreviewSize=previewsize         ; The maximum size of the image preview window. 150 pixels by default.

   @cat_pro_error_handler

   ; Is the group leader an object?
   IF N_Elements(group_leader) NE 0 THEN $
      IF Obj_ISA_Valid(group_leader, 'CATATOM') THEN group_leader -> GetProperty, ID=gleader $
      ELSE gleader = group_leader

   ; Unregister for COLORTOOL object messages.
   IF Obj_Valid(self._colors) THEN self._colors -> RegisterForMessage, self, 'COLORTOOL_TABLECHANGE', /UnRegister

   ; Do we want demo files?
   CASE Keyword_Set(demo) OF
      1: BEGIN
         image = LoadData(/Images, Group_Leader=gleader, Cancel=cancel)
      ENDCASE

      ELSE: BEGIN

         IF N_Elements(directory) EQ 0 THEN directory = self._directory

         image = SelectImage( $
            BMP=bmp, $
            Cancel=cancel, $
            Demo=demo, $
            Dicom=dicom, $
            Directory=directory, $
            FileInfo=fileInfo, $
            Filename=filename, $
            Flipimage=flipimage, $
            _Extra=extra, $
            GIF=gif, $
            Group_Leader=gleader, $
            JPEG=jpeg, $
            Only2D=only2d, $
            Only3D=only3d, $
            OutDirectory=outdirectory, $
            OutFilename=outfilename, $
            PICT=pict, $
            PNG=png, $
            TIFF=tiff, $
            PreviewSize=previewsize)
     ENDCASE

  ENDCASE

   ; Did the user cancel?
   IF cancel THEN RETURN

   ; Process the image into the object.
   Ptr_Free, self._original
   self._original = Ptr_New(image, /No_Copy)
   IF N_Elements(outdirectory) NE 0 THEN self._directory = outdirectory
   IF N_Elements(outfilename) NE 0 THEN self._filename = outfilename
   dimensions = Image_Dimensions(*self._original, XSize=xsize, YSize=ysize, TrueIndex=trueIndex, $
      XIndex=xindex, YIndex=yindex)
   self._xsize = xsize
   self._ysize = ysize
   self._bytsclmin = Min(*self._original)
   self._bytsclmax = Max(*self._original)
   *self._dataPtr = *self._original
   IF N_Elements(filename) GT 0 THEN self._filename = filename
   IF Obj_Valid(self._toolList) THEN self._toolList -> Remove, /All
   self._location[*,0] = [0, 0, xsize-1, ysize-1, xsize-1, ysize-1]
   self._location[*,1] = [0.0, 0.0, 1.0, 1.0, 1.0, 1.0]

   ; Work out the color tables for this image.
   IF Obj_Valid(self._colors) THEN $
   BEGIN
      IF N_Elements(colortable) NE 0 THEN self._colors -> LoadCT, colortable
      IF fileInfo.has_palette THEN $
      BEGIN
         TVLCT, r, g, b, /Get
         self._colors ->SetProperty, Red=r, Green=g, Blue=b
      ENDIF
      IF trueIndex GE 0 THEN self._colors -> SetProperty, RED=Bindgen(256), GREEN=Bindgen(256), BLUE=Bindgen(256)
   ENDIF $
   ELSE $
   BEGIN
      IF N_Elements(colortable) NE 0 THEN $
      BEGIN
         colors = Obj_New('ColorTool', colortable)
         self -> SetProperty, Color_Object = colors
         IF N_Elements(colortable) NE 0 THEN self._colors -> LoadCT, colortable
         IF fileInfo.has_palette THEN $
         BEGIN
            TVLCT, r, g, b, /Get
            self._colors ->SetProperty, Red=r, Green=g, Blue=b
         ENDIF
         IF trueIndex GE 0 THEN self._colors -> SetProperty, RED=Bindgen(256), GREEN=Bindgen(256), BLUE=Bindgen(256)
      ENDIF ELSE $
      BEGIN
         colors = Obj_New('ColorTool')
         self -> SetProperty, Color_Object = colors
         IF N_Elements(colortable) NE 0 THEN self._colors -> LoadCT, colortable
         IF fileInfo.has_palette THEN $
         BEGIN
            TVLCT, r, g, b, /Get
            self._colors ->SetProperty, Red=r, Green=g, Blue=b
         ENDIF ELSE self._colors -> SetProperty, RED=Bindgen(256), GREEN=Bindgen(256), BLUE=Bindgen(256)
      ENDELSE
   ENDELSE

   ; Register for COLORTOOL messages.
   self._colors -> RegisterForMessage, self, 'COLORTOOL_TABLECHANGE'

   ; Send message
   self -> SendMessage, 'CATIMAGE_NEWIMAGE'

   ; Draw the image?
   IF Keyword_Set(draw) THEN self -> Draw

END



;*****************************************************************************************************
;+
; NAME:
;       CatImageData::MESSAGEHANDLER
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
PRO CatImageData::MessageHandler, title, SENDER=sender, DATA=data

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

         ENDCASE

      'HISTOSTRETCH_CHANGE': BEGIN

            ; Get the histogram threshold values from DATA and stretch image.
            self -> SetProperty, BytSclMin=data.minThresh, BytSclMax=data.maxThresh

            ; Redraw the image without erasing.
            self -> draw

         ENDCASE

      'HISTOSTRETCH_MOTION': BEGIN

            ; Get the histogram threshold values from DATA and stretch image.
            self -> SetProperty, BytSclMin=data.minThresh, BytSclMax=data.maxThresh
            self -> Draw

         ENDCASE

      ELSE: self -> CATDATAATOM::MessageHandler, title, SENDER=sender, DATA=data


   ENDCASE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CatImageData::SetData
;
; PURPOSE:
;
;       This function method enables data to be set from this object. It is written
;       for internal use and should not be called without due consideration. All data
;       handling should be done inside the object.
;
; SYNTAX:
;
;       imageObject -> SetData (Success=s, image)
;
; ARGUMENTS:
;
;       image:    The image data to be placed into the object.
;
; KEYWORDS:
;
;       DRAW:     Set this keyword makes the object DRAW itself once the data is loaded.
;
;       NO_COPY:  Set this keyword to "move" supplied data into the object rather than
;                 make a copy. Note that this will leave the input "data" variable
;                 undefined.
;       ORIGINAL: Set this keyword to replace the original image data, rather than the display data.
;
;-
;*****************************************************************************************************
PRO CatImageData::SetData, image, $
   Draw=draw, $
   No_Copy=no_copy, $
   Original=original
   Success=success

   ; Default error handler
   @cat_pro_error_handler

   ; Check that the incoming data is valid
   IF (N_ELEMENTS (image) EQ 0) THEN MESSAGE, 'No image data supplied.'

   ; Set the data pointer
   IF Keyword_Set(original) THEN BEGIN
      IF (PTR_VALID (self._original)) THEN PTR_FREE, self._original
      self._original = PTR_NEW (image, No_Copy=no_copy)
   ENDIF

   ; Update the display data pointer.
   IF (PTR_VALID (self._dataPtr)) THEN PTR_FREE, self._dataPtr
   IF N_Elements(image) EQ 0 THEN $
      self._dataPtr = PTR_NEW (*self._original) ELSE $
      self._dataPtr = PTR_NEW (image, No_Copy=no_copy)

   ; Do DRAW if requested
   IF KEYWORD_SET (draw) THEN self -> Draw

   ; Report completion
   self -> Report, /Completed



END


;*****************************************************************************************************
;+
; NAME:
;       CatImageData::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set properties of the CatImageData object
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
;     AX_PROPERTY:      Set this keyword to invoke the axes control panel (if there are axes)
;                       in a separate window.
;
;     BYTSCLMAX:        The maximum value (MAX keyword) for byte scaling the image for display.
;
;     BYTSCLMIN:        The minimum value (MIN keyword) for byte scaling the image for display.
;
;     BACKGROUNDCOLOR:  The name of the background color. Used only if the ERASE
;                       keyword is set.
;
;     COLOR_OBJECT:     Use this keyword to load a COLORTOOL object for setting up colors
;                       for data display. The image will automatically register for COLORTOOL_TABLECHANGE
;                       messages.
;
;     COLORTABLE:       The index number of a standard color table to use for this image.
;                       The image is displayed with a gray-scale color table by default.
;
;     DIRECTORY:        The starting location to search for images when reading image
;                       files. This directory is updated to reflect the last directory
;                       used when reading an image file.
;
;     DRAW:             Set this keyword to DRAW the image after it's properties have been set.
;
;     ERASE:            Set this keyword if you wish to erase the display before
;                       the image is drawn.
;
;     FILENAME:         The name of the image file. This keyword is only used if an IMAGE
;                       is passed in simultaneously.
;
;     IMAGE:            A image to load into the object.
;
;     INTERPOLATE:      If the image is resized to fit the window, this keyword will
;                       cause bilinear interpolation of image values to be used. The
;                       default is to use nearest neighbor interpolation.
;
;     KEEP_ASPECT:      If this keyword is set, the POSITION of the image in the window
;                       is always adjusted to preserve the image aspect ratio. Otherwise,
;                       the image is resized arbitrarily.
;
;     NO_COPY:          If this keyword is set, the image data is transfered directly to the object
;                       and not copied. The image parameter will become undefined if this keyword is set.
;
;     NORESIZE:         Set this keyword to ensure the image is always drawn at its original size,
;                       rather than that specified by XSIZE and YSIZE. IF NORESIZE is set,
;                       the lower-left corner of the image is positioned at (POSITION[0], POSITION[1]).
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
;     WID:              The window objectr where this image is to be displayed.
;
;     UNDOLIMIT:        The number of tool objects kept in the image's undo buffer.
;
;     _EXTRA:           Any keyword appropriate for the SETPROPERTY method of the superclass object.
;
;-
;*****************************************************************************************************
PRO CatImageData::SetProperty, $
   AXES=axes, $
   AX_PROPERTY=ax_property, $
   BYTSCLMAX=bytsclmax, $
   BYTSCLMIN=bytsclmin, $
   BACKGROUNDCOLOR=backgroundColor, $
   COLOR_OBJECT=color_object, $
   COLORTABLE=colortable, $
   DIRECTORY=directory, $
   DISPLAY_IMAGE=display_image, $
   DRAW=draw, $
   ERASE=erase, $
   FILENAME=filename, $
   IMAGE=image, $
   INTERPOLATE=interpolate, $
   KEEP_ASPECT=keep_aspect, $
   NO_COPY=no_copy, $
   NORESIZE=noresize, $
   POSITION=position, $
   TOOLDATA=tooldata, $
   UNDOLIMIT=undolimit, $
   WID=wid, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(image) NE 0 THEN $
   BEGIN
      ndims = Size(image, /N_Dimensions)
      IF (ndims LT 2) OR (ndims GT 3) THEN Message, 'Supplied image has incorrect dimensions.'
      dimensions = Image_Dimensions(image, XSize=xsize, YSize=ysize, TrueIndex=trueIndex, $
         XIndex=xindex, YIndex=yindex)
      Ptr_Free, self._original
      self._bytsclmin = Min(image)
      self._bytsclmax = Max(image)
      self._original = Ptr_New(image, No_Copy=Keyword_Set(no_copy))
      Ptr_Free, self._dataPtr
      self._dataPtr = Ptr_New(*self._original)
      IF N_Elements(filename) GT 0 THEN self._filename = filename
      self._toolList -> Remove, /All
      self._location[*,0] = [0, 0, xsize-1, ysize-1, xsize-1, ysize-1]
      self._location[*,1] = [0.0, 0.0, 1.0, 1.0, 1.0, 1.0]
      self._xsize = xsize
      self._ysize = ysize

      ; Are there axes with this image. They may need to be changed.
      ; Update their range, if requested.

   containedObjects = self -> Get(/All)
   FOR jj=0,N_Elements(containedObjects)-1 DO BEGIN

      IF Obj_Class(containedObjects[jj]) EQ 'IMGAXES' THEN BEGIN
         answer = Dialog_Message('Update image axes?', /Question)
         IF StrUpCase(answer) EQ 'YES' THEN BEGIN
            containedObjects[jj] -> SetProperty, XRange=[0,self._xsize], YRange=[0,self._ysize]
         ENDIF
      ENDIF
   ENDFOR
   ENDIF

   IF N_Elements(display_image) NE 0 THEN *self._dataPtr = display_image
   IF N_Elements(color_object) NE 0 THEN BEGIN
      IF Obj_Valid(self._colors) THEN self._colors -> RemoveParent, self
      self._colors = color_object
      self._colors -> AddParent, self
      IF OBJ_ISA_VALID(self._colors, 'COLORTOOL') THEN $
         self._colors -> RegisterForMessage, self, 'COLORTOOL_TABLECHANGE'
   ENDIF
   IF N_Elements(bytsclmax) NE 0 THEN self._bytsclmax = bytsclmax
   IF N_Elements(bytsclmin) NE 0 THEN self._bytsclmin = bytsclmin
   IF N_Elements(backgroundColor) NE 0 THEN self._backgroundColor = backgroundColor
   IF N_Elements(colortable) NE 0 THEN BEGIN
      self -> GetProperty, Color_Object=colors
      colors -> LoadCT, colortable
   ENDIF
   IF N_Elements(directory) NE 0 THEN self._directory = directory
   IF N_Elements(erase) NE 0 THEN self._erase = Keyword_Set(erase)
   IF N_Elements(interpolate) NE 0 THEN self._interpolate = Keyword_Set(interpolate)
   IF N_Elements(keep_aspect) NE 0 THEN self._keep_aspect = Keyword_Set(keep_aspect)
   IF N_Elements(noresize) NE 0 THEN self._noresize = Keyword_Set(noresize)
   IF N_Elements(position) NE 0 THEN $
   BEGIN
      IF N_Elements(position) EQ 1 THEN position = [position, position, 1.0-position, 1.0-position]
      IF N_Elements(position) EQ 2 THEN position = [position, 1.0-position[0], 1.0-position[1]]
      self._position = position
   ENDIF
   IF N_Elements(undolimit) NE 0 THEN self._undolimit = undolimit
   IF Obj_Valid(wid) NE 0 THEN self._wid = wid
   IF N_Elements(tooldata) NE 0 THEN $
   BEGIN
      IF Ptr_Valid(self._dataPtr) THEN *self._dataPtr = tooldata ELSE $
          self._dataPtr = Ptr_New(tooldata, /No_Copy)
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
   ax_property = Keyword_Set(ax_property)
   IF ax_property THEN BEGIN

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

      ; Report status
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CatImageData::SETORIGINAL
;
; PURPOSE:
;
;       This method attempts simply replaces the display image with the original image.
;       As a side effect, any processing objects on the UNDO list are automatically deleted.
;
; SYNTAX:
;
;           imageObject -> SetOriginal
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DRAW:    Set this keyword to draw the image after replacement.
;-
;*****************************************************************************************************
PRO CatImageData::SetOriginal, Draw=draw

   @cat_pro_error_handler

   ; Must have valid image to continue.
   IF Ptr_Valid(self._original) EQ 0 THEN Message, 'No original image currently available.'
   IF Ptr_Valid(self._dataPtr) THEN *self._dataPtr = *self._original $
   ELSE self._dataPtr = Ptr_New(*self._original)

   ;Need to draw?
   IF Keyword_Set(draw) THEN self -> Draw
END


;*****************************************************************************************************
;+
; NAME:
;       CatImageData::SHOW
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
PRO CatImageData::Show

   @cat_pro_error_handler

  parents = self._parents -> Get(/All)
  FOR j=0,N_Elements(parents)-1 DO $
     IF Obj_IsA_Valid(parents[j], 'DRAWWIDGET') THEN $
     BEGIN
        parents[j] -> SetProperty, SHOW=1
        BREAK
     ENDIF
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CatImageData::XCOLORS
;
; PURPOSE:
;
;       This method changes color tables for the image. It is only
;       available for 2D image arrays.
;
; SYNTAX:
;
;           imageObject -> XColors
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       _EXTRA:  Any keyword appropriate for the COLORTOOL::XCOLORS method.
;-
;*****************************************************************************************************
PRO CatImageData::XColors, _Extra=extraKeywords

   ; Initialise the error handler.

   @cat_pro_error_handler

   ; This functionality is only avaiable for 2D images.

   ndims = Size(*self._dataPtr, /N_Dimensions)
   IF ndims GT 2 THEN $
   BEGIN
      ok = Dialog_Message('The XCOLORS method is only available for 2D image arrays.')
      RETURN
   ENDIF

   ; Can you find a widget to be the group leader for XCOLORS?

   parents = self._parents -> Get(/All)
   FOR j=0,N_Elements(parents)-1 DO $
      IF Obj_IsA_Valid(parents[j], 'WIDGETATOM') THEN $
      BEGIN
         group_leader = parents[j]
         BREAK
      ENDIF

   ; Get the color object and call its XCOLORS method.

   self -> GetProperty, Color_Object=colors
   IF Obj_IsA_Valid(colors, 'COLORTOOL') THEN $
      colors -> XColors, GROUP_LEADER=group_leader, _Extra=extraKeywords

END



;*****************************************************************************************************
;+
; NAME:
;       CatImageData::WINDOW
;
; PURPOSE:
;
;       This method with create a CATGRAPHICSWINDOW for the image and add itself
;       to the window. The image will be destroyed when the graphics window
;       is destroyed.
;
;       This method is provided mostly for interactive development of the image
;       object.
;
;       Note that other image properties are also set when the GRAPHICSWINDOW is created.
;
; SYNTAX:
;
;       imageObject -> Window
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       _EXTRA:   Any keyword appropriate for the image SETPROPERY method.
;-
;*****************************************************************************************************
PRO CatImageData::Window, _Extra=extrakeywords

   @cat_pro_error_handler

   ; If you create a window for yourself, don't destroy the image object
   ; when you exit.

   s = Size(*self._original, /Dimensions)
   gwindow = Obj_New('CATGRAPHICSWINDOW', XSize=s[0] < 1000, YSize=s[1] < 1000, $
      Title='Resizeable Image Graphics Window' )
   self -> SetProperty, Interpolate=0, Auto_Destroy=0, Position=self._position, _Extra=extraKeywords
   gwindow -> Add, self
   gwindow -> Draw
   self._wid = gwindow

   self -> Report, /Completed
END
;*****************************************************************************************************
;+
; NAME:
;       CatImageData::CLEANUP
;
; PURPOSE:
;
;       This is the CatImageData object class destructor method.
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
PRO CatImageData::CLEANUP, _EXTRA=extraKeywords

   @cat_pro_error_handler

   Ptr_Free, self._original
   self -> CatDataAtom::CLEANUP, _EXTRA=extraKeywords
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CatImageData::INIT
;
; PURPOSE:
;
;       This method is used upon object creation.
;
; SYNTAX:
;
;       Called automatically when the object is created thus:
;
;           imageObject = OBJ_NEW ('CatImageData', image)
;
; ARGUMENTS:
;
;       image           The image array of any data type.
;
; KEYWORDS:
;
;     AXES:             Set this keyword to add axes to the image display. Or, set this
;                       keyword to an axis object.
;
;     BACKGROUNDCOLOR:  The name of the background color. Used only if the ERASE
;                       keyword is set. Set to "IVORY" by default.
;
;     BYTSCLMAX:        The maximum value for bytscaling the image. If undefined, Max(image).
;
;     BYTSCLMIN:        The minimum value for bytscaling the image. If undefined, Min(image).
;
;     COLOR_OBJECT:     Use this keyword to load a COLORTOOL object for setting up colors
;                       for data display. The image will automatically register for COLORTOOL_TABLECHANGE
;                       messages.
;
;     COLORTABLE:       The index number of a standard color table to use for this image.
;                       The image is displayed with a gray-scale color table by default.
;
;     DIRECTORY:        The starting location to search for images when reading image
;                       files. This directory is updated to reflect the last directory
;                       used when reading an image file.
;
;     ERASE:            Set this keyword if you wish to erase the display before
;                       the image is drawn.
;
;     FILENAME:         The name of the image file. This keyword is only used if an IMAGE
;                       is passed in simultaneously or if the LOAD_IMAGE keyword is used.
;
;     INTERPOLATE:      If the image is resized to fit the window, this keyword will
;                       cause bilinear interpolation of image values to be used. The
;                       default is to use nearest neighbor interpolation.
;
;     KEEP_ASPECT:      If this keyword is set, the POSITION of the image in the window
;                       is always adjusted to preserve the image aspect ratio (ratio of height
;                       divided by width). Otherwise, the image is resized arbitrarily.
;
;     LOAD_IMAGE:       If this keyword is set, and no image data is passed into the INIT method,
;                       then the ReadImageFile method is called to obtain an image data set.
;
;     NO_COPY:          If this keyword is set, the image data is transfered directly to the object
;                       and not copied. The image parameter will become undefined if this keyword is set.
;
;     NORESIZE:         Set this keyword to ensure the image is always drawn at
;                       its original size, rather than that specified by XSIZE
;                       and YSIZE.
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
;     XSIZE:            The X size of the image.
;
;     YSIZE:            The Y size of the image.
;
;     WID:              The window object where this image is to be displayed.
;
;     _EXTRA:           Any keyword appropriate for the INIT method of the superclass object or for
;                       the LoadImage method.
;-
;*****************************************************************************************************
FUNCTION CatImageData::INIT, image, $
                          AXES=axes, $
                          BACKGROUNDCOLOR=backgroundColor, $
                          BYTSCLMAX=bytsclmax, $
                          BYTSCLMIN=bytsclmin, $
                          COLOR_OBJECT=color_object, $
                          COLORTABLE=colortable, $
                          DIRECTORY=directory, $
                          ERASE=erase, $
                          FILENAME=filename, $
                          INTERPOLATE=interpolate, $
                          KEEP_ASPECT=keep_aspect, $
                          LOAD_IMAGE=load_image, $
                          NO_COPY=no_copy, $
                          NORESIZE=noresize, $
                          ORDER=order, $
                          POSITION=position, $
                          XSIZE=xsize, $
                          YSIZE=ysize, $
                          WID=wid, $
                          _EXTRA=extraKeywords

   @cat_func_error_handler


   IF N_Elements(backgroundColor) EQ 0 THEN backgroundColor = "BLACK"
   IF N_Elements(bytsclmax) EQ 0 THEN bytsclmax = Max(image)
   IF N_Elements(bytsclmin) EQ 0 THEN bytsclmin = Min(image)
   IF N_Elements(color_object) NE 0 THEN BEGIN
      IF Obj_Valid(self._colors) THEN self._colors -> RemoveParent, self
      self._colors = color_object
      self._colors -> AddParent, self
      IF OBJ_ISA_VALID(self._colors, 'COLORTOOL') THEN $
         self._colors -> RegisterForMessage, self, 'COLORTOOL_TABLECHANGE'
   ENDIF ELSE self._colors = Obj_New('Colortool')
   IF N_Elements(directory) EQ 0 THEN directory = DirPath()
   interpolate = Keyword_Set(interpolate)
   keep_aspect = Keyword_Set(keep_aspect)
   noresize = Keyword_Set(noresize)
   erase    = Keyword_Set(erase)
   IF N_Elements(position) EQ 0 THEN position = [0.0, 0.0, 1.0, 1.0]
   IF N_Elements(position) EQ 1 THEN position = [position, position, 1.0-position, 1.0-position]
   IF N_Elements(position) EQ 2 THEN position = [position, 1.0-position[0], 1.0-position[1]]
   IF N_Elements(image) NE 0 THEN $
   BEGIN

      ndims = Size(image, /N_Dimensions)
      IF (ndims LT 2) OR (ndims GT 3) THEN Message, 'Supplied image has incorrect dimensions.'
      dimensions = Image_Dimensions(image, XSize=xsize, YSize=ysize, TrueIndex=trueIndex, $
         XIndex=xindex, YIndex=yindex)
      self -> SetData, image, NO_COPY=no_copy
      self._original = Ptr_New(*self._dataPtr)
      IF N_Elements(filename) GT 0 THEN self._filename = filename
      self._location[*,0] = [0, 0, xsize-1, ysize-1, xsize-1, ysize-1]
      self._location[*,1] = [0.0, 0.0, 1.0, 1.0, 1.0, 1.0]
      self._xsize = xsize
      self._ysize = ysize
      self._bytsclmin = bytsclmin
      self._bytsclmax = bytsclmax

   ENDIF $
   ELSE IF Keyword_Set(load_image) THEN $
      self -> LoadImage,  Filename=filename, Directory=directory, _Extra=extrakeywords

   ; Call the superclass INIT method
   ok = self -> CatDataAtom::INIT (_EXTRA=extraKeywords)

   ; Work out the color tables for this image.
   IF Obj_Valid(self._colors) THEN $
   BEGIN
      IF N_Elements(colortable) NE 0 THEN self._colors -> LoadCT, colortable
   END

   ; Populate the object.
   self._backgroundColor = backgroundColor
   self._directory = directory
   self._erase = erase
   self._interpolate = interpolate
   self._keep_aspect = keep_aspect
   self._noresize = noresize
   self._order = Keyword_Set(order)
   self._position = position
   IF Obj_Valid(wid) NE 0 THEN self._wid = wid

   ; Do you need axes for this image?
   IF Keyword_Set(axes) THEN BEGIN

      containedObjects = self -> Get(/All)
      FOR jj=0,N_Elements(containedObjects)-1 DO BEGIN

         ;Only one set of axes per image.
         IF Obj_Class(containedObjects[jj]) EQ 'IMGAXES' THEN $
            IF Obj_Valid(containedObjects[jj]) THEN self -> Remove, containedObjects[jj]
      ENDFOR

     IF Obj_ISA_Valid(axes, 'IMGAXES') THEN self -> Add, axes $
        ELSE BEGIN
           self -> GetProperty, XSIZE=xsize, YSIZE=ysize
           self -> Add, Obj_New('IMGAXES', Position=self._location[0:3, 1], XRange=[0,xsize], $
            YRange=[0,ysize], Tickformat='(I5)')
        ENDELSE

   ENDIF

   ; Need a graphics window?
   IF Keyword_Set(window) THEN self -> Window

   ; Register for messages from the COLORTOOL object.
   self -> GetProperty, Color_Object = colors
   IF Obj_Valid(colors) THEN colors -> RegisterForMessage, self, 'COLORTOOL_TABLECHANGE'

   ; Report and return status
   IF (ok) THEN self -> Report, /Completed $
   ELSE self -> Report, /Failed
   RETURN, ok
END



;*****************************************************************************************************
;
; NAME:
;       CatImageData CLASS DEFINITION
;
; PURPOSE:
;
;       This is the class definition code for the CatImageData object.
;
;*****************************************************************************************************
PRO CatImageData__DEFINE, class

    class = { CatImageData, $                  ; The CatImageData object class.
              _backgroundColor: "", $          ; The name of a background color to use when ERASING.
              _bytsclmin  : 0.0D, $            ; The MIN value when byte scaling the data for display.
              _bytsclmax  : 0.0D, $            ; The MAX value when byte scaling the data for display.
              _directory  : "", $              ; The directory to look in for reading image files.
              _erase      : 0L, $              ; Flag: Erase the window before displaying image.
              _filename   : "", $              ; The name of the image file that has been read.
              _interpolate: 0L, $              ; Flag: Interpolate when resizing. Nearest neighbor is default.
              _keep_aspect: 0L, $              ; Flag: Keep the aspect ratio when calculating image position.
              _location   : DblArr(6,2), $     ; Location information about the "displayed" image.
              _order      : 0L, $              ; Image display order.
              _original   : Ptr_New(), $       ; The original image data
              _noresize   : 0L, $              ; Flag: No display resizing.
              _position   : DblArr(4), $       ; The position of the image in the output window.
              _wid        : Obj_New(), $       ; The window object this image should be drawn in.
              _xsize      : 0L, $              ; The X size of the image.
              _ysize      : 0L, $              ; The Y size of the image.
              INHERITS CatDataAtom $           ; Subclassed from CATDATAATOM.
            }

END

