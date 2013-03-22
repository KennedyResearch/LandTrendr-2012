;*****************************************************************************************************
;+
; NAME:
;       CatImage
;
; PURPOSE:
;
;       This object implements a class for handling image data. It is a subclassed
;       CATDATAATOM object and can be subclassed for specific image implementations.
;       
;       Out first attempts to define an image object class resulted in the three
;       object classes CatImageData, CatImage2D, and CatTrueColorImage. These are
;       still used in Catalyst applications, but are included more for historical
;       reasons than for practical reasons. These days all image object implementations
;       start with CatImage (or, more likely, with ScaleImage, which is subclassed from
;       CatImage). It is our recommendation that you start your own object implementation
;       from this CatImage object.
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
;       imageObject = OBJ_NEW ('CatImage', image)
;
; CLASS DEFINITION:
;
;    class = { CatImage, $                      ; The CatImage object class.
;              _displayImage: Ptr_New(), $      ; The byte-scaled, congridded image for display.
;              _display_mode: 0B, $             ; A flag indicating which display method to use for drawing.
;              _keep_aspect: 0L, $              ; Flag: Keep the aspect ratio when calculating image position.
;              _location   : DblArr(6,2), $     ; Location information about the "displayed" image.
;              _interleaving : 0L, $            ; The type of pixel interleaving for this image.
;              _interpolate: 0L, $              ; Flag: When set, bilinear interpolation is used.
;              _order      : 0L, $              ; Image display order.
;              _out_xsize  : 0L, $              ; The output X size of the image. (Display_Mode=1 and Display_Mode=2)
;              _out_ysize  : 0L, $              ; The output Y size of the image. (Display_Mode=1 and Display_Mode=2)
;              _position   : DblArr(4), $       ; The position of the image in the output window. (Display_Mode=0)
;              _scale      : 0.0, $             ; The zoom scale factor.
;              _x1          : 0L, $             ; The starting X coordinate in the zoom.
;              _x2          : 0L, $             ; The ending X coordinate in the zoom.
;              _y1          : 0L, $             ; The starting Y coordinate in the zoom.
;              _y2          : 0L, $             ; The ending Y coordinate in the zoom.
;              _xsize      : 0L, $              ; The X size of the image.
;              _xstart     : 0L, $              ; The starting X location for displaying image. (Display_Mode=1)
;              _ysize      : 0L, $              ; The Y size of the image.
;              _ystart     : 0L, $              ; The starting Y location for displaying image. (Display_Mode=1)
;              _wid        : Obj_New(), $       ; The window object where this image should be displayed.
;              _window_position: 0L, $          ; The window position for displaying this image. (Display_Mode=2)
;              _zoomcoords  : Obj_New(), $      ; A coordinate object for the zoom capability.
;              INHERITS CatDataAtom $           ; Subclassed from CATDATAATOM.
;            }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 27 March 2003.
;       Added PAN, ZOOMIN, and ZOOMOUT methods 25 June 2004. DWF.
;       Removed MINUS_ONE keywords from the CONGRIDs in CreateDisplayImage.
;         They caused me to get, for example, 15 colors instead of 16 when I
;         restricted the number of colors in the image. 5 July 2005. DWF.
;       Fixed a bug in Pixel_To_Value method that caused value to be slightly off.
;          This was most noticable with really small images. 10 Oct 2008. DWF.
;-
;*******************************************************************************************
;* Copyright (c) 2008, jointly by Fanning Software Consulting, Inc.                        *
;* and Burridge Computing. All rights reserved.                                            *
;*                                                                                         *
;* Redistribution and use in source and binary forms, with or without                      *
;* modification, are permitted provided that the following conditions are met:             *
;*     * Redistributions of source code must retain the above copyright                    *
;*       notice, this list of conditions and the following disclaimer.                     *
;*     * Redistributions in binary form must reproduce the above copyright                 *
;*       notice, this list of conditions and the following disclaimer in the               *
;*       documentation and/or other materials provided with the distribution.              *
;*     * Neither the name of Fanning Software Consulting, Inc. or Burridge Computing       *
;*       nor the names of its contributors may be used to endorse or promote products      *
;*       derived from this software without specific prior written permission.             *
;*                                                                                         *
;* THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. AND BURRIDGE COMPUTING   *
;* ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE     *
;* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE          *
;* DISCLAIMED. IN NO EVENT SHALL FANNING SOFTWARE CONSULTING, INC. OR BURRIDGE COMPUTING   *
;* BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL    *
;* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;    *
;* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *
;* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              *
;* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           *
;* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            *
;*******************************************************************************************

;+
; NAME:
;       CatImage::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the CATIMAGE object. A
;       control panel is a graphical user interface for setting object
;       properties. If you create a control panel, the events are typically
;       sent to the EVENTHANDLER method.
;
; SYNTAX:
;
;       theObject -> ControlPanel, baseObject
;
; ARGUMENTS:
;
;       baseObject:    The object reference of a base widget for this control to
;                      be added to. If not supplied, the control panel will be in a
;                      self contained window (i.e., a TOPLEVELBASE object).
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the CatControlPanel::INIT method.
;
;-
;*****************************************************************************************************
PRO CatImage::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.
   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Image Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)
   self -> SetProperty, Description='CatImage Properties'

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.
   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, $
      Name='IMAGE PROPERTYSHEET', YSize=5, Description='CatImage Properties')
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CatImage::COPYPARAMETERS
;
; PURPOSE:
;
;       This method returns the lower-left corner of the bounary box
;       in the DESTINATION keyword, and the number of columns and rows
;       in the boundary box in the EXTENT keyword, all in window or pixel
;       coordinates. It's purpose is to return a section of a pixmap, for
;       example, so that only that section can be copied.
;
; SYNTAX:
;
;       theObject -> CopyParameters, drawid, DESTINATION=destination, EXTENT=extent
;
; ARGUMENTS:
;
;       drawID:         The identifier of a draw widget object whose extent will
;                       provide the size of the window for calculating device coordinates.
;                       This parameter is required.
;
; KEYWORDS:
;
;       DESTINATION:    A two-element array containing the lower-left corner
;                       of the boundary box in device coordinates. An output keyword.
;
;       EXTENT:         A two-element array containing the number of columns and
;                       rows in the boundary box in device coordinates. An output keyword.
;
;-
;*****************************************************************************************************
PRO CatImage::CopyParameters, drawid, DESTINATION=destination, EXTENT=extent

   @cat_pro_error_handler

   IF N_Elements(drawid) EQ 0 THEN Message, 'The DRAWID argument is required.'

   ; Make sure the draw widget is the current graphics window.
   drawID -> SetWindow

   ; If the image contains axes, then the boundary box is larger than the image.
   ; Seff if you can find axes.
   IF self -> Count() GT 0 THEN BEGIN

      containedObjects = self -> Get(/All)
      FOR jj=0,N_Elements(containedObjects)-1 DO BEGIN

         IF Obj_Class(containedObjects[jj]) EQ 'IMGAXES' THEN BEGIN
            containedObjects[jj] -> GetProperty, Boundary_Box=box
            minx = Min(box[0,*], Max=maxx)
            miny = Min(box[1,*], Max=maxy)
            BREAK
         ENDIF
      ENDFOR

      IF N_Elements(minx) EQ 0 THEN BEGIN
         minx = Min([self._position[0],self._position[2]], Max=maxx)
         miny = Min([self._position[1],self._position[3]], Max=maxy)
      ENDIF

   ENDIF ELSE BEGIN

      minx = Min([self._position[0],self._position[2]], Max=maxx)
      miny = Min([self._position[1],self._position[3]], Max=maxy)

   ENDELSE
   c = Convert_Coord([minx, maxx-minx], [miny, maxy-miny], /Normal, /To_Device)

   destination = [c[0,0]-8,c[1,0]-8]
   extent = [c[0,1]+16, c[1,1]+16]

   ; Make sure you are not out of the window. (Primarily for X Windows devices.)
   IF destination[0] LT 0 THEN destination[0] = 0.0
   IF destination[0] GT ((!D.X_Size-1) - extent[0]) THEN destination[0] = (!D.X_Size) - extent[0] + 1
   IF destination[1] LT 0 THEN destination[1] = 0.0
   IF destination[1] GT ((!D.Y_Size-1) - extent[1]) THEN destination[1] = (!D.Y_Size) - extent[1] + 1

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       CatImage::CREATEDISPLAYIMAGE
;
; PURPOSE:
;
;       This method creates a display image for the object
;
; SYNTAX:
;
;       imageObject -> CreateDisplayImage
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
PRO CatImage::CreateDisplayImage

   @cat_pro_error_handler
   
    ; Doing multiple plots?
    IF Total(!P.Multi) GT 0 THEN multi = 1 ELSE multi = 0
    IF Keyword_Set(multi) THEN BEGIN
    
          ; Draw an invisible plot to get plot position. In pixmap to avoid background
          ; color change in window.
          currentWindow = !D.Window
          Window, XSIZE=!D.X_SIZE, YSIZE=!D.Y_SIZE, /PIXMAP
          Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, XMargin=[1,1], YMargin=[1,1]
          WDelete, !D.Window
          IF currentWindow GE 0 THEN WSet, currentWindow
          self._position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
        
   ENDIF 
    
   ;Keep the aspect ratio of the image?
   IF self._keep_aspect THEN BEGIN

      ; Find aspect ratio of image.
      ratio = FLOAT(self._ysize) / self._xsize

      ; Find the proposed size of the image in pixels without aspect considerations.
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
    xsize = Ceil((position[2] - position[0]) * !D.X_VSIZE)
    ysize = Ceil((position[3] - position[1]) * !D.Y_VSIZE)
    xstart = Round(position[0] * !D.X_VSIZE)
    ystart = Round(position[1] * !D.Y_VSIZE)

   ; Update the location variables, as these may have changed.
   self._location[*,0] = [xstart, ystart, xstart + xsize, ystart + ysize, Double(!D.X_VSize), Double(!D.Y_VSize)]
   self._location[*,1] = [ self._location[0,0]/self._location[4,0], $
                           self._location[1,0]/self._location[5,0], $
                           self._location[2,0]/self._location[4,0], $
                           self._location[3,0]/self._location[5,0], $
                           self._location[4,0]/self._location[4,0], $
                           self._location[5,0]/self._location[5,0] ]

  ; Is there a contained coordinate object that needs updating?
  IF Obj_Valid(self._coords) THEN BEGIN
     self._coords -> SetProperty, Position=self._location[0:3,1]
  END
  IF Obj_Valid(self._zoomCoords) THEN BEGIN
     self._zoomCoords -> SetProperty, Position=self._location[0:3,1]
  END

  ; Compute a display image.
  IF Ptr_Valid(self._dataPtr) THEN BEGIN

     ; If this is PostScript, then get the image and RETURN.
     IF (!D.Flags AND 1) NE 0 THEN BEGIN

        ; Get the image itself.
        CASE self._interleaving OF
            0: image = (*self._dataPtr)[self._x1:self._x2, self._y1:self._y2]
            1: image = (*self._dataPtr)[*, self._x1:self._x2, self._y1:self._y2]
            2: image = (*self._dataPtr)[self._x1:self._x2, *, self._y1:self._y2]
            3: image = (*self._dataPtr)[self._x1:self._x2, self._y1:self._y2, *]
        ENDCASE

        IF Ptr_Valid(self._displayImage) THEN BEGIN
           *self._displayImage = image
        ENDIF ELSE BEGIN
           self._displayImage = Ptr_New(image)
        ENDELSE
        RETURN
     ENDIF

     ; It is not PostScript, so find the right image based on interleaving.
     CASE self._interleaving OF

         0: BEGIN

            image = (*self._dataPtr)[self._x1:self._x2, self._y1:self._y2]
            IF Ptr_Valid(self._displayImage) THEN BEGIN
               *self._displayImage = Congrid(image, ROUND(xsize), ROUND(ysize), $
                  INTERP=self._interpolate)
            ENDIF ELSE BEGIN
               self._displayImage = Ptr_New(Congrid(image, ROUND(xsize), ROUND(ysize), $
                  INTERP=self._interpolate), /No_Copy)
            ENDELSE

            END

        1: BEGIN

           image = (*self._dataPtr)[*, self._x1:self._x2, self._y1:self._y2]
           IF Ptr_Valid(self._displayImage) THEN BEGIN
              *self._displayImage = Congrid(image, 3, ROUND(xsize), ROUND(ysize), $
               INTERP=self._interpolate)
           ENDIF ELSE BEGIN
              self._displayImage = Ptr_New(Congrid(image, 3, ROUND(xsize), ROUND(ysize), $
               INTERP=self._interpolate), /No_Copy)
           ENDELSE

           END

        2: BEGIN

           image = (*self._dataPtr)[self._x1:self._x2, *, self._y1:self._y2]
           IF Ptr_Valid(self._displayImage) THEN BEGIN
              *self._displayImage = Congrid(image, ROUND(xsize), 3, ROUND(ysize), $
               INTERP=self._interpolate)
           ENDIF ELSE BEGIN
              self._displayImage = Ptr_New(Congrid(image, ROUND(xsize), 3, ROUND(ysize), $
               INTERP=self._interpolate), /No_Copy)
           ENDELSE

           END

        3: BEGIN

           image = (*self._dataPtr)[self._x1:self._x2, self._y1:self._y2, *]
           IF Ptr_Valid(self._displayImage) THEN BEGIN
              *self._displayImage = Congrid(image, ROUND(xsize), ROUND(ysize), 3, $
               INTERP=self._interpolate)
           ENDIF ELSE BEGIN
              self._displayImage = Ptr_New(Congrid(image, ROUND(xsize), ROUND(ysize), 3, $
               INTERP=self._interpolate), /No_Copy)
           ENDELSE

           END

     ENDCASE
  ENDIF

END



;*****************************************************************************************************
;+
; NAME:
;       CatImage::DRAW
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
PRO CatImage::Draw, _Extra=extrakeywords

   ; Set up the error handler.
   @cat_pro_error_handler

   ; No drawing if invisible.
   IF self._visible EQ 0 THEN RETURN

   ; Which display mode?
   CASE self._display_mode OF
      0: self -> Draw_Mode_0, _Extra=extrakeywords ; Based on image's position in window (similar to TVIMAGE)
      1: self -> Draw_Mode_1, _Extra=extrakeywords ; Just a standard TV.
      2: self -> Draw_Mode_2, _Extra=extrakeywords ; Standard TV, but with a position index (TV, image, 1).
   ENDCASE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CatImage::DRAW_MODE_0
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
PRO CatImage::Draw_Mode_0, _Extra=extrakeywords

   ; Set up the error handler.
   @cat_pro_error_handler

  ; Set to display window, if you have one. Otherwise, draw in current window.
  IF Obj_Valid(self._wid) THEN self._wid -> SetWindow

   ; Create a display image.
   self -> CreateDisplayImage

   ; Get the starting and size information from the location.
   xstart = self._location[0,0]
   ystart = self._location[1,0]
   xsize =  self._location[2,0] - self._location[0,0]
   ysize =  self._location[3,0] - self._location[1,0]

   ; Display the image. Sizing differently for scalable pixels devices.
   IF (!D.Flags AND 1) NE 0 THEN $
   BEGIN

      ; Set up colors and coordinates (if they exist).
      self -> ApplyColors
      self -> ApplyCoords

       ; Need a gray-scale color table if this is a true color image.
       IF self._interleaving GT 0 THEN LOADCT, 0, /Silent
       TV, *self._displayImage, ROUND(xstart), ROUND(ystart), XSIZE=ROUND(xsize), YSIZE=ROUND(ysize), $
           True=self._interleaving, Order=self._order
   ENDIF ELSE $
   BEGIN ; All other devices.

      Device, Get_Decomposed=theState
      IF self._interleaving GT 0 THEN Device, Decomposed=1 ELSE Device, Decomposed=0

      ; Set up colors and coordinates (if they exist).
      self -> ApplyColors
      self -> ApplyCoords

      ; Display the display image.
      TV, *self._displayImage, True=self._interleaving, xstart, ystart, order=self._order

      ; Clean up.
      Device, Decomposed=theState

   ENDELSE

     ; Are there any image axes? Update their position.

  containedObjects = self -> Get(/All)

  FOR jj=0,N_Elements(containedObjects)-1 DO BEGIN

     IF Obj_Class(containedObjects[jj]) EQ 'IMGAXES' THEN $
        containedObjects[jj] -> SetProperty, Position=self._location[0:3,1]

  ENDFOR

  ; Any children to draw?
  self -> CatDataAtom::Draw, _Extra=extrakeywords

  self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CatImage::DRAW_MODE_1
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
PRO CatImage::Draw_Mode_1, _Extra=extrakeywords

   ; Set up the error handler.
   @cat_pro_error_handler

  ; Gather information about the image.
  dimensions = Image_Dimensions(*self._dataPtr, XSize=xsize, YSize=ysize, TrueIndex=trueIndex, $
         XIndex=xindex, YIndex=yindex)
  pixelInterleaving = trueIndex + 1

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

      ; Need a gray-scale color table if this is a true color image.
      IF trueIndex GE 0 THEN LOADCT, 0, /Silent
      TV, *self._dataPtr, self._xstart, self._ystart, XSIZE=xsize, YSIZE=ysize, ORDER=self._order, True=pixelInterleaving

  ENDIF ELSE $
  BEGIN ; All other devices.

     Device, Get_Decomposed=theState
     IF trueIndex GE 0 THEN Device, Decomposed=1 ELSE Device, Decomposed=0

     ; Set up colors and coordinates (if they exist).
     self -> ApplyColors
     self -> ApplyCoords

     ; Make sure you have valid output sizes
     IF (self._out_xsize LE 0) OR (self._out_ysize LE 0) THEN Message, 'Image output sizes cannot be zero.'

     CASE pixelInterleaving OF
        0: TV, Congrid(*self._dataPtr, self._out_xsize, self._out_ysize, INTERP=self._interpolate, $
              MINUS_ONE=1), self._xstart, self._ystart, True=pixelInterleaving, Order=self._order

        1: TV, Congrid(*self._dataPtr, 3, self._out_xsize, self._out_ysize, INTERP=self._interpolate, $
              MINUS_ONE=1), self._xstart, self._ystart, True=pixelInterleaving, Order=self._order


        2: TV, Congrid(*self._dataPtr, self._out_xsize, 3, self._out_ysize, INTERP=self._interpolate, $
              MINUS_ONE=1), self._xstart, self._ystart, True=pixelInterleaving, Order=self._order

        3: TV, Congrid(*self._dataPtr, self._out_xsize, self._out_ysize, 3, INTERP=self._interpolate, $
              MINUS_ONE=1), self._xstart, self._ystart, True=pixelInterleaving , Order=self._order

     ENDCASE

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
;       CatImage::DRAW_MODE_2
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
PRO CatImage::Draw_Mode_2, _Extra=extrakeywords

   ; Set up the error handler.
   @cat_pro_error_handler

  ; Gather information about the image.
  dimensions = Image_Dimensions(*self._dataPtr, XSize=xsize, YSize=ysize, TrueIndex=trueIndex, $
         XIndex=xindex, YIndex=yindex)
  pixelInterleaving = trueIndex + 1

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

      ; Need a gray-scale color table if this is a true color image.
      IF trueIndex GE 0 THEN LOADCT, 0, /Silent
      TV, *self._dataPtr, self._window_position, XSIZE=xsize, YSIZE=ysize, ORDER=self._order, True=pixelInterleaving

  ENDIF ELSE $
  BEGIN ; All other devices.

     Device, Get_Decomposed=theState
     IF trueIndex GE 0 THEN Device, Decomposed=1 ELSE Device, Decomposed=0

     ; Set up colors and coordinates (if they exist).
     self -> ApplyColors
     self -> ApplyCoords

     ; Make sure you have valid output sizes
     IF (self._out_xsize LE 0) OR (self._out_ysize LE 0) THEN Message, 'Image output sizes cannot be zero.'

     CASE pixelInterleaving OF
        0: TV, Congrid(*self._dataPtr, self._out_xsize, self._out_ysize, INTERP=self._interpolate, $
              MINUS_ONE=1), self._window_position, True=pixelInterleaving, Order=self._order

        1: TV, Congrid(*self._dataPtr, 3, self._out_xsize, self._out_ysize, INTERP=self._interpolate, $
              MINUS_ONE=1), self._window_position, True=pixelInterleaving, Order=self._order


        2: TV, Congrid(*self._dataPtr, self._out_xsize, 3, self._out_ysize, INTERP=self._interpolate, $
              MINUS_ONE=1), self._window_position, True=pixelInterleaving, Order=self._order

        3: TV, Congrid(*self._dataPtr, self._out_xsize, self._out_ysize, 3, INTERP=self._interpolate, $
              MINUS_ONE=1), self._window_position, True=pixelInterleaving , Order=self._order

     ENDCASE

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
;        CatImage::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the CATIMAGE object. It will typically
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
PRO CatImage::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF

      'DELETE_OBJECT': self -> SetProperty, Delete=1

      'IMAGE PROPERTYSHEET': BEGIN

         IF event.type EQ 0 THEN BEGIN
            CASE StrUpCase(event.identifier) OF

               'COLOR': BEGIN

                  event.component -> GetProperty, Color=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, Color=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase
                  RETURN
               ENDCASE

               'POSITION': BEGIN
                  event.component -> GetProperty, Position=pos
                  event.id -> GetProperty, ID=group_leader
                  position = AdjustPosition(pos, Group_Leader=group_leader)
                  event.component -> SetProperty, Position=position
                  CatRefreshDraw, self, Stop_At='DrawWidget'
                  RETURN
               ENDCASE

               ELSE: BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value

                  ; Refresh the graphics hierarchy. (Exit if you have deleted the object.)
                  IF Obj_Valid(self) THEN CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase ELSE RETURN

               ENDCASE

            ENDCASE
         ENDIF

         ENDCASE

      'MOVE_FORWARD': self -> SetProperty, Move_Forward=1

      'MOVE_BACKWARD': self -> SetProperty, Move_Backward=1

      'OTHER_PROPERTIES': BEGIN
                    event.id -> GetProperty, UValue=drawID
                    self -> ControlPanel, Group_Leader=drawID
                    END

      'SEND_FRONT': self -> SetProperty, Bring_To_Front=1

      'SEND_BACK': self -> SetProperty, Send_To_Back=1

        ELSE: BEGIN
         Message, 'Unable to respond to a EventHandler event..'
         END

   ENDCASE

   ; Report completion. Object may have been deleted.
   IF Obj_Valid(self) THEN self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CatImage::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to get properties of the CatImage object
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
;       ASPECT_RATIO:     The aspect ratio of the image: ysize/xsize.
;
;       AXES:             The image axes object for this image. If not available, a null object.
;       
;       DIMENSIONS:       The dimensions of the image (e.g, Size(image, /DIMENSIONS).
;
;       IMAGE:            The image data.
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
;       MINMAX:           A two-element array giving the minimum and maximum value of the orginal image data.
;       
;       N_DIMENSIONS:     The number of dimensions of the image (e.g, Size(image, /N_DIMENSIONS).
;
;       NOINTERPOLATE:    The nointerpolate flag.
;
;       ORDER:            The display order of the image.
;
;       OUT_XSIZE:        The X size of the image on the display.
;
;       OUT_YSIZE:        The Y size of the image on the display.
;
;       POSITION:         A four-element vector giving the position of the image in the
;                         display window in normalized coordinates: [x0, y0, x1, y1].
;
;       SELECTABLE:       Set to 1 if the image is selectable.
;       
;       TRUEINDEX:        Set the the index of the "3" in a true-color image, or to -1 otherwise.
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
;
;       VISIBLE:          Set to 1 if the image is visible.
;
;       WID:              The window object where this image will be displayed.
;
;       WINDOW_POSITION:  A single number that gives a "window position" for displaying the image.
;                         The same as calling the TV command with a single positional parameter other
;                         than the image.
;
;       ZOOM_COORD:       The zoom coordinate system.
;
;       ZOOMRECT:         The coordinates of the zoom rectangle in the image in the form [x1, y1, x2, y2].
;                         The coordinates are in the image pixel coordinate system (e.g., 0 to self._xsize-1).
;
;
;       _REF_EXTRA:       Any keyword appropriate for the DATAATOM::GETPROPERTY method.
;
;-
;*****************************************************************************************************
PRO CatImage::GetProperty, $
   ASPECT_RATIO=aspect_ratio, $
   AXES=axes, $
   DIMENSIONS=dimensions, $
   IMAGE=image, $
   KEEP_ASPECT=keep_aspect, $
   LOCATION=location, $
   MINMAX=minmax, $
   N_DIMENSIONS=n_dimensions, $
   NOINTERPOLATE=nointerpolate, $
   ORDER=order, $
   OUT_XSIZE=out_xsize, $
   OUT_YSIZE=out_ysize, $
   POSITION=position, $
   SELECTABLE=selectable, $
   TRUEINDEX=trueindex, $
   XSTART=xstart, $
   XSIZE=xsize, $
   YSTART=ystart, $
   YSIZE=ysize, $
   VISIBLE=visible, $
   WID=wid, $
   WINDOW_POSITION=window_position, $
   ZOOM_COORD=zoom_coord, $
   ZOOMRECT=zoomrect, $
    _REF_EXTRA=extraKeywords

      ; Initialise the error handler
   @cat_pro_error_handler

      ; Create the return variables

   IF Arg_Present(axes) THEN axes = self -> Get('ImageAxes')
   IF Arg_Present(dimensions) THEN IF Ptr_Valid(self._dataPtr) THEN $
        dimensions = Size(*self._dataPtr, /DIMENSIONS)
   IF Arg_Present(image) THEN IF Ptr_Valid(self._dataPtr) THEN image = *self._dataPtr
   IF Arg_Present(keep_aspect) THEN keep_aspect = self._keep_aspect
   IF Arg_Present(location) THEN location = self._location
   IF Arg_Present(minmax) THEN BEGIN
      minval = Min(*self._dataPtr, MAX=maxval)
      minmax = [minval, maxval]
   ENDIF
   IF Arg_Present(n_dimensions) THEN IF Ptr_Valid(self._dataPtr) THEN $
        n_dimensions = Size(*self._dataPtr, /N_DIMENSIONS)
   IF Arg_Present(nointerpolate) THEN nointerpolate = 1 - self._interpolate
   IF Arg_Present(order) THEN order = self._order
   IF Arg_Present(position) THEN position = self._position
   IF Arg_Present(out_xsize) THEN out_xsize = self._out_xsize
   IF Arg_Present(out_ysize) THEN out_ysize = self._out_ysize
   IF Arg_Present(selectable) THEN selectable = self._selectable
   IF Arg_Present(trueindex) THEN BEGIN
      dimensions = Image_Dimensions(*self._dataPtr, TRUEINDEX=trueindex)
   ENDIF
   IF Arg_Present(xstart) THEN xstart = self._xstart
   IF Arg_Present(ystart) THEN ystart = self._ystart
   IF Arg_Present(xsize) OR Arg_Present(ysize) THEN BEGIN
      IF Ptr_Valid(self._dataPtr) THEN BEGIN
         dimensions = Image_Dimensions(*self._dataPtr, XSize=xsize, YSize=ysize)
      ENDIF
   ENDIF
   IF Arg_Present(visible) THEN visible = self._visible
   IF Arg_Present(wid) THEN wid = self._wid
   IF Arg_Present(window_position) THEN window_position = self._window_position
   IF Arg_Present(zoom_coord) THEN zoom_coord = self._zoomCoords
   IF Arg_Present(zoomrect) THEN zoomrect = [self._x1, self._y1, self._x2, self._y2]
   IF Arg_Present(aspect_ratio) THEN aspect_ratio = Float(self._ysize) / self._xsize

      ; Call the superclass method if required
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CatDataAtom::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CatImage::MESSAGEHANDLER
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
PRO CatImage::MessageHandler, title, SENDER=sender, DATA=data

   ; Initialise the error handler
   @cat_pro_error_handler

   ; What kind of message are we receiving?
   CASE title OF

      'COLORTOOL_TABLECHANGE': BEGIN

            ; If the sender is not the image's colortool object, then update image colors.
            IF sender NE self._colors THEN $
               self._colors -> SetProperty, Red=data.r, Green=data.g, Blue=data.b, Bottom=data.bottom

            ; If the image has a parent that is a DrawWidget or Pixmap, ask that parent
            ; to re-draw the image. If you can't find a parent, just draw.
            self -> GetProperty, First_Parent=parent
            IF Obj_Valid(parent) EQ 0 THEN self -> Draw ELSE BEGIN

               CASE 1 OF
                  Obj_ISA_Valid(parent, 'DrawWidget'): parent -> Draw, Targets=self
                  Obj_ISA_Valid(parent, 'PixmapWidget'): parent -> Refresh, Targets=self
                  ELSE: self -> Draw
               ENDCASE

            ENDELSE


         ENDCASE

      'COLORTOOL_SETPROPERTY': BEGIN

            ; If the sender is not the image's colortool object, then update image colors.
            IF sender NE self._colors THEN BEGIN
               sender -> GetProperty, Red=red, Green=green, Blue=blue
               self._colors -> SetProperty, Red=red, Green=green, Blue=blue
            ENDIF

            ; If the image has a parent that is a DrawWidget or Pixmap, ask that parent
            ; to re-draw the image. If you can't find a parent, just draw.
            self -> GetProperty, First_Parent=parent
            IF Obj_Valid(parent) EQ 0 THEN self -> Draw ELSE BEGIN

               CASE 1 OF
                  Obj_ISA_Valid(parent, 'DrawWidget'): parent -> Draw, Targets=self
                  Obj_ISA_Valid(parent, 'PixmapWidget'): parent -> Refresh, Targets=self
                  ELSE: self -> Draw
               ENDCASE

            ENDELSE


         ENDCASE

      ; If a message has come in here, and we can't resolve it, pass it along to our superclass objects.
      ELSE: self -> CATDATAATOM::MessageHandler, title, SENDER=sender, DATA=data


   ENDCASE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CatImage::MOVE
;
; PURPOSE:
;
;       This method moves the image in a graphics window.
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
PRO CatImage::Move, x, y, PIXMAP=pixmap, NODRAW=noDraw

   @cat_pro_error_handler

   ; Convert the device pixels into normalized coordinates.

   c = Convert_Coord(x, y, /Device, /To_Normal)
   self._position[0] = self._position[0] + c[0,0]
   self._position[1] = self._position[1] + c[1,0]
   self._position[2] = self._position[2] + c[0,0]
   self._position[3] = self._position[3] + c[1,0]

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
;       CatImage::PAN
;
; PURPOSE:
;
;       This method pans the image.
;
; SYNTAX:
;
;       theImage -> Pan, movex, movey
;
; ARGUMENTS:
;
;       movex:  The number of (window) pixels to move in the X direction.
;
;       movey:  The number of (window) pixels to move in the Y direction.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO CatImage::Pan, movex, movey

   @cat_pro_error_handler

   ; Get the current zoom rectangle.
   self ->GetProperty, ZoomRect=rect

   movex = -3 > movex < 3
   movey = -3 > movey < 3

   ; Calculate a factor for moving the image in the image pixel coordinate system
   ; as opposed to the window coordinate system.
   xfactor = self._xsize / Float(self._location[2,0] - self._location[0,0])
   yfactor = self._ysize / Float(self._location[3,0] - self._location[1,0])

   ; Move the zoom rectangle by the specified amount in the image pixel coordinate system.
   x1 = Round(rect[0] - (movex * xfactor))
   IF self._order THEN y1 = Round(rect[1] + (movey * yfactor )) ELSE y1 = Round(rect[1] - (movey * yfactor ))
   x2 = Round(rect[2] - (movex * xfactor))
   IF self._order THEN y2 = Round(rect[3] + (movey * yfactor)) ELSE y2 = Round(rect[3] - (movey * yfactor))

   ; Make sure the endpoints are inside the image.
   IF x1 LT 0 THEN BEGIN
      x2 = x2 + Abs(x1)
      x1 = 0
   ENDIF
   IF y1 LT 0 THEN BEGIN
      y2 = y2 + Abs(y1)
      y1 = 0
   ENDIF
   IF x2 GE (self._xsize-1) THEN BEGIN
      x2 = self._xsize - 1
      x1 = x2 - (rect[2] - rect[0])
   ENDIF
   IF y2 GE (self._ysize-1) THEN BEGIN
      y2 = self._ysize - 1
      y1 = y2 - (rect[3] - rect[1])
   ENDIF

   ; Set the zoom rectangle.
   self -> GetProperty, ZoomRect=zrect
   self -> SetProperty, ZoomRect=Round([x1, y1, x2, y2])

   ; Update the zoom coordinate system.
   self._coords -> GetProperty, XRange=xr, YRange=yr
   IF Ptr_Valid(self._z_orig_xr) EQ 0 THEN self._z_orig_xr = Ptr_New(xr)
   IF Ptr_Valid(self._z_orig_yr) EQ 0 THEN self._z_orig_yr = Ptr_New(yr)

   xvec = Scale_Vector(Findgen(self._xsize), (*self._z_orig_xr)[0], (*self._z_orig_xr)[1])
   xr = [xvec[Round(x1)], xvec[Round(x2)]]

   yvec = Scale_Vector(Findgen(self._ysize),  (*self._z_orig_yr)[0], (*self._z_orig_yr)[1])
   yr = [yvec[Round(y1)], yvec[Round(y2)]]

   self._zoomcoords -> SetProperty, XRange=xr, YRange=yr

   ; Draw it.
   self -> Draw

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CatImage::PIXEL_TO_VALUE
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
FUNCTION CatImage::Pixel_to_Value, x, y, $
   Inside=inside, $
   XData=xdata, $
   XPixel=xpixel, $
   YData=ydata, $
   YPixel=ypixel

   @cat_func_error_handler

   self -> ApplyCoords

   currentWindow = !D.Window
   IF Obj_Valid(self._wid) THEN self._wid -> SetWindow

   ; The point must be inside the image, else RETURN.
   inside = self -> Point_Inside(x, y)
   IF inside NE 1 THEN RETURN, 0
   
   ; Where is th image in the window, and what size is it?
   thePos = self._location[0:3, 0]
   dims = Image_Dimensions(*self._dataPtr, XSIZE=xsize, YSIZE=ysize)
   
   ; Create vectors for locating the image dimensions with VALUE_LOCATE.
   xvec = Scale_Vector(Findgen(xsize+1), thePos[0], thePos[2])
   yvec = Scale_Vector(Findgen(ysize+1  ), thePos[1], thePos[3])
   xpixel = 0 > Value_Locate(xvec, x) < (xsize - 1)
   ypixel = 0 > Value_Locate(yvec, y) < (ysize - 1)
   
   ; Output depends in whether this is 2D or 3D image.
   trueIndex = Where(dims EQ 3)

   IF trueIndex[0] EQ -1 THEN BEGIN

         value = (*self._dataPtr)[xpixel, ypixel]
         IF Size(value, /TNAME) EQ 'BYTE' THEN value = Fix(value)
         retvalue = value

   ENDIF ELSE BEGIN

       ; 3D image processing here.
       CASE trueindex[0] OF
           0: rgb = (*self._dataPtr)[*, xpixel, ypixel]
           1: rgb = (*self._dataPtr)[xpixel, *, ypixel]
           2: rgb = (*self._dataPtr)[xpixel, ypixel, *]
       ENDCASE
       IF Size(rgb, /TNAME) EQ 'BYTE' THEN rgb = Fix(rgb)
       retvalue = rgb
   ENDELSE

   ; Get the data coordinate system. Could be zoomed, have to
   ; get the right coordinate object to create data coordinates.
   IF Obj_Valid(self._zoomCoords) THEN BEGIN
        self._zoomCoords -> Draw
        c = Convert_Coord(x, y, /Device, /To_Data)
        xdata = c[0,0]
        ydata = c[1,0]
   ENDIF ELSE BEGIN
        IF Obj_Valid(self._coords) THEN BEGIN
              self._coords -> Draw
              c = Convert_Coord(x, y, /Device, /To_Data)
              xdata = c[0,0]
              ydata = c[1,0]
        ENDIF ELSE BEGIN
            xdata = xpixel
            ydata = ypixel        
        ENDELSE
   ENDELSE


   ; Set the window back to entering value.
   IF currentWindow GE 0 THEN WSet, currentWindow

   RETURN, retValue

END



;*****************************************************************************************************
;+
; NAME:
;       CatImage::POINT_INSIDE
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
FUNCTION CatImage::Point_Inside, x, y

   @cat_func_error_handler

   ; Apply the coordinate system.
   self -> ApplyCoords

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
;       CatImage::SELECT
;
; PURPOSE:
;
;       This method returns the object reference if the requested point is inside
;       the image
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
FUNCTION CatImage::Select, x, y, SUCCESS=success

   @cat_func_error_handler

   ; Set up return values.
   retval = Obj_New()
   success = 0

   ; No selection is possible, if the object is currently unselectable.
   IF ~self._selectable THEN RETURN, retval

   ; No selection is possible, if the object is currently invisible.
   IF ~self._visible THEN RETURN, retval

   ; Are you inside?
   isInside = self -> Point_Inside(x, y)
   IF isInside THEN BEGIN

      retVal = self
      success = 1

   ENDIF

   self -> Report, /Completed
   RETURN, retval

END



;*****************************************************************************************************
;+
; NAME:
;       CatImage::SELECTPANEL
;
; PURPOSE:
;
;       Similar to a Control Panel, it gives context menu access to properties
;       of selectable objects.
;
; SYNTAX:
;
;       selectedObject = theObject -> SelectPanel, x, y, drawID
;
; ARGUMENTS:
;
;       X:       The X location of a point in device or window coordinates.
;
;       Y:       The Y location of a point in device or window coordinates.
;
;       DRAWID:  The identifer of the draw widget object in which the selection is taking place.
;
; KEYWORDS:
;
;       None:
;-
;*****************************************************************************************************
PRO CatImage::SelectPanel,  x, y, drawID

   @cat_pro_error_handler

   IF N_Params() NE 3 THEN Message, 'Incorrect number of positonal parameters.'

   IF Obj_Valid(self._contextmenu) THEN Obj_Destroy, self._contextMenu
   self._contextMenu = Obj_New('ContextMenuBase', drawID, Column=1, Event_Object=self, NAME='CONTEXTMENU PROPERTIES')
   button = Obj_New('ButtonWidget', self._contextMenu, Value='Bring to Front', Name='SEND_FRONT')
   button = Obj_New('ButtonWidget', self._contextMenu, Value='Send to Back', Name='SEND_BACK')
   button = Obj_New('ButtonWidget', self._contextMenu, Value='Move Forward', Name='MOVE_FORWARD')
   button = Obj_New('ButtonWidget', self._contextMenu, Value='Move Backward', Name='MOVE_BACKWARD')
   button = Obj_New('ButtonWidget', self._contextMenu, Value='Other Properties...', Name='OTHER_PROPERTIES', UValue=drawID)
   button = Obj_New('ButtonWidget', self._contextMenu, Value='Delete', Name='DELETE_OBJECT', /Separator)

   Widget_DisplayContextMenu, drawID -> GetID(), x+10, y-5, self._contextMenu->GetID()

END



;*****************************************************************************************************
;+
; NAME:
;       CatImage::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set properties of the CatImage object
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
;     BRING_TO_FRONT:   Set this keyword to move this selectable object to the front of all selectable objects.
;
;     COLOR_OBJECT:     Use this keyword to load a COLORTOOL object for setting up colors
;                       for data display. The image will automatically register for COLORTOOL_TABLECHANGE
;                       messages.
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
;     IMAGE:            A image to load into the object.
;
;     KEEP_ASPECT:      If this keyword is set, the POSITION of the image in the window
;                       is always adjusted to preserve the image aspect ratio (ratio of height
;                       divided by width). Otherwise, the image is resized arbitrarily. Applies
;                       only to DISPLAY_MODE=0.
;
;     MOVE_BACKWARD:    Set this keyword to move this selectable object to the back of all selectable objects.
;
;     MOVE_FORWARD:     Set this keyword to move this selectable object to the front of all selectable objects.
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
;     ORDER:            The display order of the image.
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
;     SELECTABLE:       Set this keyword to make the image selectable.
;
;     SEND_TO_BACK:     Set this keyword to move this selectable object to the back of all selectable objects.
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
;     VISIBLE:          Set this keyword to 1 to make the object visible.
;
;     WID:              The window object where this image is to be displayed.
;
;     WINDOW_POSITION:  A single number that gives a "window position" for displaying the image.
;                       The same as calling the TV command with a single positional parameter other
;                       than the image.
;
;     ZOOMRECT:         The coordinates of the zoom rectangle in the image in the form [x1, y1, x2, y2].
;                       The coordinates are in the image pixel coordinate system (e.g., 0 to self._xsize-1).
;
;     _EXTRA:           Any keyword appropriate for the SETPROPERTY method of the superclass object.
;
;-
;*****************************************************************************************************
PRO CatImage::SetProperty, $
   AXES=axes, $
   AXIS_PROPERTY=axis_property, $
   COLOR_OBJECT=color_object, $
   DISPLAY_MODE=display_mode, $
   DRAW=draw, $
   IMAGE=image, $
   KEEP_ASPECT=keep_aspect, $
   LOCATION=location, $
   NO_COPY=no_copy, $
   NOINTERPOLATE=nointerpolate, $
   ORDER=order, $
   POSITION=position, $
   SELECTABLE=selectable, $
   XSIZE=out_xsize, $
   XSTART=xstart, $
   YSIZE=out_ysize, $
   YSTART=ystart, $
   VISIBLE=visible, $
   WID=wid, $
   WINDOW_POSITION=window_position, $
   ZOOMRECT=zoomrect, $

   ; For "selectable object" status.
   BRING_TO_FRONT=bring_to_front, $
   DELETE=delete, $
   MOVE_BACKWARD=move_backward, $
   MOVE_FORWARD=move_forward, $
   NOMESSAGE=nomessage, $
   NOREFRESH=norefresh, $
   SEND_TO_BACK=send_to_back, $


   _EXTRA=extraKeywords

   @cat_pro_error_handler
   
   IF N_Elements(location) NE 0 THEN self._location = location

   ; Color object?
   IF N_Elements(color_object) NE 0 THEN BEGIN
      IF Obj_Valid(self._colors) THEN self._colors -> RemoveParent, self
      self._colors = color_object
      self._colors -> AddParent, self
      IF OBJ_ISA_VALID(self._colors, 'COLORTOOL') OR OBJ_ISA_VALID(self._colors, 'CATCOLORS') THEN $
         self._colors -> RegisterForMessage, self, 'COLORTOOL_TABLECHANGE'
   ENDIF

   IF N_Elements(bring_to_front) NE 0 THEN BEGIN
      sendMessage = 1
      self -> GetProperty, Parent=parents

      ; Have to do the move for all parents.
      FOR j=0,N_Elements(parents)-1 DO BEGIN
         selectableObjects = parents[j] -> Get(/All, ISA='SELECTABLEOBJECT')
         children = parents[j] -> Get(/All)
         index = Where(children EQ self, count)
         last = Where(children EQ selectableObjects[N_Elements(selectableObjects)-1])
         parents[j] -> Move, index, last
      ENDFOR

      IF ~Keyword_Set(norefresh) THEN CatRefreshDraw, self, Stop_At='DRAWWIDGET'
   ENDIF
   IF Keyword_Set(delete) THEN BEGIN
      self -> SetProperty, Visible=0
      IF ~Keyword_Set(norefresh) THEN CatRefreshDraw, self, Stop_At='DRAWWIDGET'
      Obj_Destroy, self._controlPanel
      self -> GetProperty, Parent=parents
      FOR j=0,N_Elements(parents)-1 DO parents[j] -> Remove, self
      Obj_Destroy, self
      RETURN
   ENDIF
   IF N_Elements(display_mode) NE 0 THEN self._display_mode = display_mode
   IF N_Elements(keep_aspect) NE 0 THEN self._keep_aspect = Keyword_Set(keep_aspect)
   IF N_Elements(nointerpolate) NE 0 THEN self._interpolate = 1 - Keyword_Set(nointerpolate)
   IF N_Elements(order) NE 0 THEN self._order = order
   IF N_Elements(position) NE 0 THEN self._position = position
   IF N_Elements(out_xsize) NE 0 THEN self._out_xsize = out_xsize
   IF N_Elements(out_ysize) NE 0 THEN self._out_ysize = out_ysize
   IF N_Elements(move_forward) NE 0 THEN BEGIN
      sendMessage = 1
      self -> GetProperty, Parent=parents

      ; Have to do the move for all parents.
      FOR j=0,N_Elements(parents)-1 DO BEGIN
         selectableObjects = parents[j] -> Get(/All, ISA='SELECTABLEOBJECT')
         children = parents[j] -> Get(/All)
         index = Where(children EQ self, count)
         last = Where(children EQ selectableObjects[N_Elements(selectableObjects)-1])
         parents[j] -> Move, index, (index + 1) < last
      ENDFOR

      IF ~Keyword_Set(norefresh) THEN CatRefreshDraw, self, Stop_At='DRAWWIDGET'
   ENDIF
   IF N_Elements(move_backward) NE 0 THEN BEGIN
      sendMessage = 1
      self -> GetProperty, Parent=parents

      ; Have to do the move for all parents.
      FOR j=0,N_Elements(parents)-1 DO BEGIN
         selectableObjects = parents[j] -> Get(/All, ISA='SELECTABLEOBJECT')
         children = parents[j] -> Get(/All)
         index = Where(children EQ self, count)
         first = Where(children EQ selectableObjects[0])
         parents[j] -> Move, index, (index - 1) > first
      ENDFOR

      IF ~Keyword_Set(norefresh) THEN CatRefreshDraw, self, Stop_At='DRAWWIDGET'
   ENDIF
   IF N_Elements(send_to_back) NE 0 THEN BEGIN
      sendMessage = 1
      self -> GetProperty, Parent=parents

      ; Have to do the move for all parents.
      FOR j=0,N_Elements(parents)-1 DO BEGIN
         selectableObjects = parents[j] -> Get(/All, ISA='SELECTABLEOBJECT')
         children = parents[j] -> Get(/All)
         index = Where(children EQ self, count)
         first = Where(children EQ selectableObjects[0])
         parents[j] -> Move, index, first
      ENDFOR

      IF ~Keyword_Set(norefresh) THEN CatRefreshDraw, self, Stop_At='DRAWWIDGET'
   ENDIF
   IF N_Elements(selectable) NE 0 THEN self._selectable = Keyword_Set(selectable)
   IF N_Elements(xstart) NE 0 THEN self._xstart = xstart
   IF N_Elements(ystart) NE 0 THEN self._ystart = ystart
   IF N_Elements(visible) NE 0 THEN BEGIN
      self._visible = Keyword_Set(visible)

     ; Are there any image axes? Turn them off, too
     containedObjects = self -> Get(/All)
     FOR jj=0,N_Elements(containedObjects)-1 DO BEGIN

        IF Obj_Class(containedObjects[jj]) EQ 'IMGAXES' THEN $
           containedObjects[jj] -> SetProperty, VISIBLE=Keyword_Set(visible)

     ENDFOR
   ENDIF
   IF Obj_Valid(wid) NE 0 THEN self._wid = wid
   IF N_Elements(window_position) NE 0 THEN self._window_position = window_position
   IF N_Elements(zoomrect) NE 0 THEN BEGIN
      self._x1 = zoomrect[0]
      self._y1 = zoomrect[1]
      self._x2 = zoomrect[2]
      self._y2 = zoomrect[3]
   ENDIF

   IF N_Elements(image) NE 0 THEN BEGIN

      ndims = Size(image, /N_Dimensions)
      IF (ndims LT 2) OR (ndims GT 3) THEN Message, 'Supplied image has incorrect dimensions.'
      dimensions = Image_Dimensions(image, XSize=img_xsize, YSize=img_ysize, TrueIndex=trueIndex, $
         XIndex=xindex, YIndex=yindex)
      self._interleaving = trueIndex + 1
      Ptr_Free, self._dataPtr
      self -> SetData, image, NO_COPY=Keyword_Set(no_copy)
      self._location[0:3,0] = [ self._position[0]*img_xsize, self._position[1]*img_ysize, $
                              self._position[2]*img_xsize, self._position[3]*img_ysize ]
      self._location[4:5,0] = [!D.X_Size, !D.Y_Size]
      self._location[0:3,1] = self._position
      self._location[4:5,1] = [1.0,1.0]
      self._xsize = img_xsize
      self._ysize = img_ysize
      self._x2 = img_xsize - 1
      self._y2 = img_ysize - 1

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
   IF Obj_Valid(self) THEN self -> CatDataAtom::SetProperty, _EXTRA=extraKeywords

   IF Keyword_Set(draw) THEN IF Obj_Valid(self) THEN self -> Draw

      ; Report status
  IF Obj_Valid(self) THEN self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CatImage::SHOW
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
PRO CatImage::Show

   @cat_pro_error_handler

   IF Obj_Valid(self._wid) THEN self._wid -> Show

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CatImage::ZOOMIN
;
; PURPOSE:
;
;       This method allows the user to zoom into the image.
;
; SYNTAX:
;
;       imageObject -> ZoomIn, x, y
;
; ARGUMENTS:
;
;       x_img: The X focal point for zooming. If not provided, the center of the image.
;              The value will be a pixel value in the native image coordinate system.
;              That is to say, a value between 0 and the image X size. The value can
;              be obtained from an event structure by passing the event structure to
;              the method Pixel_To_Value:
;
;                 imgVal = self -> Pixel_To_Value(event.x, event.y, XPixel=x_img, YPixel=y_img)
;                 self -> ZoomIn, x_img, y_img
;
;       y_img: The Y focal point for zooming. If not provided, the center of the image.
;              The value will be a pixel value in the native image coordinate system.
;              That is to say, a value between 0 and the image Y size.
;
; KEYWORDS:
;
;       NODRAW: Normally the image is drawn as soon as it is resized. Setting this
;               keyword prevents the redraw.
;
;-
;*****************************************************************************************************
PRO CatImage::ZoomIn, x_img, y_img, NoDraw=nodraw

   @cat_pro_error_handler
   
   ; Is this the first time we have tried to zoom? If so, we have to get everything
   ; organized.
   IF Obj_Valid(self._zoomCoords) EQ 0 THEN self._zoomCoords = self._coords

   ; Do we have a focal point?
   IF N_Elements(x_img) EQ 0 THEN BEGIN
      x_img = (self._xsize) / 2
      y_img = (self._ysize) / 2
   ENDIF

   ; Increase the zoom scale.
   self._scale = self._scale + 0.25

   ; Find the endpoints of a zoom.
   xnumpix = self._xsize / (2^self._scale)
   ynumpix = self._ysize / (2^self._scale)
   x1 = x_img - (xnumpix/2.)
   x2 = x1 + xnumpix
   y1 = y_img - (ynumpix/2.)
   y2 = y1 + ynumpix

   ; Make sure the endpoints are inside the image.
   IF x1 LT 0 THEN BEGIN
      x2 = x2 + Abs(x1)
      x1 = 0
   ENDIF
   IF y1 LT 0 THEN BEGIN
      y2 = y2 + Abs(y1)
      y1 = 0
   ENDIF
   IF x2 GE (self._xsize-1) THEN BEGIN
      x2 = self._xsize - 1
      x1 = x2 - xnumpix + 1
   ENDIF
   IF y2 GE (self._ysize-1) THEN BEGIN
      y2 = self._ysize - 1
      y1 = y2 - ynumpix + 1
   ENDIF

   ; Set the zoom rectangle.
   self -> GetProperty, ZoomRect=zrect
   self -> SetProperty, ZoomRect=Round([x1, y1, x2, y2])

   ; Update the zoom coordinate system.
   self._coords -> GetProperty, XRange=xr, YRange=yr
   IF Ptr_Valid(self._z_orig_xr) EQ 0 THEN self._z_orig_xr = Ptr_New(xr)
   IF Ptr_Valid(self._z_orig_yr) EQ 0 THEN self._z_orig_yr = Ptr_New(yr)

   xvec = Scale_Vector(Findgen(self._xsize), (*self._z_orig_xr)[0], (*self._z_orig_xr)[1])
   xr = [xvec[Round(x1)], xvec[Round(x2)]]

   yvec = Scale_Vector(Findgen(self._ysize),  (*self._z_orig_yr)[0], (*self._z_orig_yr)[1])
   yr = [yvec[Round(y1)], yvec[Round(y2)]]

   self._zoomcoords -> SetProperty, XRange=xr, YRange=yr

  ; Draw it.
  IF ~Keyword_Set(nodraw) THEN self -> Draw

  self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CatImage::ZOOMOUT
;
; PURPOSE:
;
;       This method allows the user to zoom out of the image.
;
; SYNTAX:
;
;       imageObject -> ZoomOut, x, y
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       ALLTHEWAY:  If this keyword is set, the image is zoomed all the way back to its starting value.
;
;       NODRAW:     Normally the image is drawn as soon as it is resized. Setting this
;                   keyword prevents the redraw.
;
;-
;*****************************************************************************************************
PRO CatImage::ZoomOut, x_img, y_img, ALLTHEWAY=allTheWay, NODRAW=nodraw

   @cat_pro_error_handler

   ; Do we have a focal point?
   IF N_Elements(x_img) EQ 0 THEN BEGIN
      x_img = (self._xsize) / 2
      y_img = (self._ysize) / 2
   ENDIF

   ; If there is nothing left to zoom out on, just return silently.
   IF self._scale EQ 0.0 THEN RETURN

   ; Update the zoom scale factor.
   IF Keyword_Set(allTheWay) THEN self._scale = 0.0 ELSE self._scale = (self._scale - 0.25) > 0.0

   ; Find the endpoints of a zoom.
   xnumpix = self._xsize / (2^self._scale)
   ynumpix = self._ysize / (2^self._scale)
   x1 = x_img - (xnumpix/2.)
   x2 = x1 + xnumpix
   y1 = y_img - (ynumpix/2.)
   y2 = y1 + ynumpix

   ; Make sure the endpoints are inside the image.
   IF x1 LT 0 THEN BEGIN
      x2 = x2 + Abs(x1)
      x1 = 0
   ENDIF
   IF y1 LT 0 THEN BEGIN
      y2 = y2 + Abs(y1)
      y1 = 0
   ENDIF
   IF x2 GE (self._xsize-1) THEN BEGIN
      x2 = self._xsize - 1
      x1 = x2 - xnumpix + 1
   ENDIF
   IF y2 GE (self._ysize-1) THEN BEGIN
      y2 = self._ysize - 1
      y1 = y2 - ynumpix + 1
   ENDIF

   ; Set the zoom rectangle.
   self -> GetProperty, ZoomRect=zrect
   self -> SetProperty, ZoomRect=Round([x1, y1, x2, y2])

   ; Update the zoom coordinate system.
   self._coords -> GetProperty, XRange=xr, YRange=yr

   xvec = Scale_Vector(Findgen(self._xsize), (*self._z_orig_xr)[0], (*self._z_orig_xr)[1])
   xr = [xvec[Round(x1)], xvec[Round(x2)]]

   yvec = Scale_Vector(Findgen(self._ysize),  (*self._z_orig_yr)[0], (*self._z_orig_yr)[1])
   yr = [yvec[Round(y1)], yvec[Round(y2)]]

   self._zoomcoords -> SetProperty, XRange=xr, YRange=yr

  ; Draw it.
  IF ~Keyword_Set(nodraw) THEN self -> Draw

  IF self._scale EQ 0.0 THEN BEGIN
      Ptr_Free, self._z_orig_xr
      Ptr_Free, self._z_orig_yr
  ENDIF

  self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CatImage::CLEANUP
;
; PURPOSE:
;
;       This is the CatImage object class destructor method.
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
PRO CatImage::CLEANUP, _EXTRA=extraKeywords

   @cat_pro_error_handler

   Ptr_Free, self._displayImage
   Obj_Destroy, self._contextmenu
   Obj_Destroy, self._zoomCoords
   Ptr_Free, self._z_orig_xr
   Ptr_Free, self._z_orig_yr

   self -> CatDataAtom::CLEANUP, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CatImage::INIT
;
; PURPOSE:
;
;       This method is used upon object creation.
;
; SYNTAX:
;
;       Called automatically when the object is created thus:
;
;           imageObject = OBJ_NEW ('CatImage', image)
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
;     DISPLAY_MODE:     Images can be displayed in any of three display "modes". Display Mode 0 is
;                       the default and allows images to be positioned in the window using the
;                       POSITION keyword. Display Mode 1 is the standard "TV" display mode in which
;                       the image is positioned with respect to its lower-left corner in the
;                       display window. Display Mode 2 is the standard "TV" display mode in which
;                       the image is positioned in the window according to the WINDOW_POSITION.
;
;                       PAN and ZOOM capability is only available when in DISPLAY_MODE=0.
;
;     NOINTERPOLATE:    If the image is resized to fit the window, this keyword, if set, will
;                       cause nearest neighbor interpolation of image values to be used. The
;                       default is to use  bilinear interpolation. Applies only to DISPLAY_MODE=0.
;
;     KEEP_ASPECT:      If this keyword is set, the POSITION of the image in the window
;                       is always adjusted to preserve the image aspect ratio (ratio of height
;                       divided by width). Otherwise, the image is resized arbitrarily. Applies
;                       only to DISPLAY_MODE=0.
;
;     NO_COPY:          If this keyword is set, the image data is transfered directly to the object
;                       and not copied. The image parameter will become undefined if this keyword is set.
;
;     ORDER:            The display order of the image. We seriously recommend that you NOT use this
;                       keyword!! Instead, use REVERSE to change the Y values of your image:
;                            image = Reverse(image,1)
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
;     SELECTABLE:       Set this keyword to make the image selectable.
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
;     WID:              The window object where this image is to be displayed. Use this with some trepidation,
;                       because having an image wedded to a particular window can cause difficulties for
;                       pixmaps and interaction objects. Better to write code so that the image does not
;                       know where it is drawn. Still, it is sometimes useful to have this functionality.
;
;     WINDOW_POSITION:  A single number that gives a "window position" for displaying the image.
;                       The same as calling the TV command with a single positional parameter other
;                       than the image.
;
;     _EXTRA:           Any keyword appropriate for the INIT method of the superclass object.
;-
;*****************************************************************************************************
FUNCTION CatImage::INIT, image, $
   AXES=axes, $
   DISPLAY_MODE=display_mode, $
   NOINTERPOLATE=nointerpolate, $
   KEEP_ASPECT=keep_aspect, $
   NO_COPY=no_copy, $
   ORDER=order, $
   POSITION=position, $
   SELECTABLE=selectable, $
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
   nointerpolate = Keyword_Set(nointerpolate)
   keep_aspect = Keyword_Set(keep_aspect)
   order = Keyword_Set(order)
   IF N_Elements(position) EQ 0 THEN position = [0.0, 0.0, 1.0, 1.0]

   ; Did we get an image?
   IF N_Elements(image) NE 0 THEN BEGIN

      ndims = Size(image, /N_Dimensions)
      IF (ndims LT 2) OR (ndims GT 3) THEN Message, 'Supplied image has incorrect dimensions.'
      dimensions = Image_Dimensions(image, XSize=img_xsize, YSize=img_ysize, TrueIndex=trueIndex, $
         XIndex=xindex, YIndex=yindex)
      self._interleaving = trueIndex + 1
      self -> SetData, image, NO_COPY=no_copy
      self._location[0:3,0] = [ position[0]*img_xsize, position[1]*img_ysize, $
                              position[2]*img_xsize, position[3]*img_ysize ]
      self._location[4:5,0] = [!D.X_Size, !D.Y_Size]
      self._location[0:3,1] = position
      self._location[4:5,1] = [1.0,1.0]
      IF N_Elements(out_xsize) EQ 0 THEN self._out_xsize = img_xsize ELSE self._out_xsize = out_xsize
      IF N_Elements(out_ysize) EQ 0 THEN self._out_ysize = img_ysize ELSE self._out_ysize = out_ysize
      self._xsize = img_xsize
      self._ysize = img_ysize
      self._x2 = img_xsize - 1
      self._y2 = img_ysize - 1

   ENDIF

   ; Call the superclass INIT method
   ok = self -> CatDataAtom::INIT (_EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; If there is no valid coordinate object at this point, create one.
   IF Obj_Valid(self._coords) EQ 0 THEN $
      self._coords = Obj_New('CatCoord', XRange=[0, self._xsize > 1], YRange=[0, self._ysize > 1])

   ; Populate the object.
   self._display_mode = display_mode
   self._interpolate = 1 - Keyword_Set(nointerpolate)
   self._keep_aspect = keep_aspect
   self._order = order
   self._position = position
   self._scale = 0.0
   self._selectable = Keyword_Set(selectable)
   IF N_Elements(xstart) NE 0 THEN self._xstart = xstart
   IF N_Elements(ystart) NE 0 THEN self._ystart = ystart
   self._visible = 1

   IF Obj_Valid(wid) NE 0 THEN self._wid = wid

   ; Do you need axes for this image?
   IF Keyword_Set(axes) THEN BEGIN

     IF Obj_ISA_Valid(axes, 'IMGAXES') THEN $
        BEGIN
            axes -> GetProperty, XRANGE=xrange, YRANGE=yrange
            self._coords -> SetProperty, XRANGE=xrange, YRANGE=yrange
            self -> Add, axes 
        ENDIF $
        ELSE BEGIN
           self -> GetProperty, XSIZE=xsize, YSIZE=ysize
           self -> Add, Obj_New('IMGAXES', Position=self._location[0:3, 1], XRange=[0,xsize], $
            YRange=[0,ysize], XTickformat='(I5)', YTickformat='(I5)')
        ENDELSE

   ENDIF

   ; Register for messages from the COLORTOOL object.
   self -> GetProperty, Color_Object = colors
   IF Obj_Isa_Valid(colors, 'COLORTOOL') OR Obj_Isa_Valid(colors, 'CATCOLORS')THEN BEGIN
      colors -> RegisterForMessage, self, 'COLORTOOL_TABLECHANGE'
   ENDIF ELSE BEGIN
      self._colors = Obj_New('CatColors')
      self._colors -> RegisterForMessage, self, 'COLORTOOL_TABLECHANGE'
   ENDELSE


   ; Register properties for the property sheet.
   self->RegisterProperty, 'Keep_Aspect', 1, NAME="Keep Aspect Ratio"
   self -> SetPropertyByIdentifier, 'Keep_Aspect', self._keep_aspect
   self->RegisterProperty, 'NoInterpolate', 1, NAME=" No Bilinear Interpolation"
   self -> SetPropertyByIdentifier, 'NoInterpolate', 1 - self._interpolate
   self->RegisterProperty, 'Position', 0, NAME="Image Position", USERDEF='Image Position'

   ; Report and return status
   self -> Report, /Completed
   RETURN, 1
END



;*****************************************************************************************************
;
; NAME:
;       CatImage CLASS DEFINITION
;
; PURPOSE:
;
;       This is the class definition code for the CatImage object.
;
;*****************************************************************************************************
PRO CatImage__DEFINE, class

    class = { CatImage, $                      ; The CatImage object class.
              _contextMenu: Obj_New(), $       ; A SelectPanel context menu object.
              _displayImage: Ptr_New(), $      ; The byte-scaled, congridded image for display.
              _display_mode: 0B, $             ; A flag indicating which display method to use for drawing.
              _keep_aspect: 0L, $              ; Flag: Keep the aspect ratio when calculating image position.
              _location   : DblArr(6,2), $     ; Location information about the "displayed" image.
              _interleaving : 0L, $            ; The type of pixel interleaving for this image.
              _interpolate: 0L, $              ; Flag: When set, bilinear interpolation is used.
              _order      : 0L, $              ; Image display order.
              _out_xsize  : 0L, $              ; The output X size of the image. (Display_Mode=1 and Display_Mode=2)
              _out_ysize  : 0L, $              ; The output Y size of the image. (Display_Mode=1 and Display_Mode=2)
              _position   : DblArr(4), $       ; The position of the image in the output window. (Display_Mode=0)
              _scale      : 0.0, $             ; The zoom scale factor.
              _selectable: 0L, $               ; A flag that indicates if the image is selectable or not.
              _x1          : 0L, $             ; The starting X coordinate in the zoom.
              _x2          : 0L, $             ; The ending X coordinate in the zoom.
              _y1          : 0L, $             ; The starting Y coordinate in the zoom.
              _y2          : 0L, $             ; The ending Y coordinate in the zoom.
              _xsize      : 0L, $              ; The X size of the image.
              _xstart     : 0L, $              ; The starting X location for displaying image. (Display_Mode=1)
              _ysize      : 0L, $              ; The Y size of the image.
              _ystart     : 0L, $              ; The starting Y location for displaying image. (Display_Mode=1)
              _visible    : 0L, $              ; A flag that indicates if the object is visible or not.
              _wid        : Obj_New(), $       ; The window object where this image should be displayed.
              _window_position: 0L, $          ; The window position for displaying this image. (Display_Mode=2)
              _zoomcoords  : Obj_New(), $      ; A coordinate object for the zoom capability
              _z_orig_xr: Ptr_New(), $         ; The original x range when the image is zoomed or panned.
              _z_orig_yr: Ptr_New(), $         ; The original y range when the image is zoomed or panned.
              INHERITS CatDataAtom $           ; Subclassed from CATDATAATOM.
            }

END

