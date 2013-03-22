;*****************************************************************************************************
;+
; NAME:
;       SCALEIMAGE__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide a CATIMAGE object that can be scaled or
;       stretched. The algorithm for scaling is similar to this (linear scaling is shown):
;
;           image = ORIGINAL_IMAGE
;           i = WHERE(image EQ MISSING_VALUE, count)
;           IF count GT 0 THEN image[i] = MISSING_COLOR
;           image = BytScl(image, MIN=SCLMIN, MAX=SCLMAX, TOP=NCOLORS-1) + BOTTOM
;           
;       Scaling or stretching types include: linear, 2% linear, gamma, log, hyperbolic sine,
;       square-root, equilization, and gaussian. See this article for additional information:
;       
;           http://www.dfanning.com/ip_tips/xstretch.html
;       
;       Scaling only applies to 2D images. True-color images are not scaled and none
;       of the scaling parameters apply to them. Supported methods of scaling are 
;
; AUTHORS:
;
;        FANNING SOFTWARE CONSULTING
;        1645 Sheely Drive
;        Fort Collins
;        CO 80526 USA
;        Phone: 970-221-0438
;        E-mail: davidf@dfanning.com
;
; CATEGORY:
;
;       Objects.
;
; SYNTAX:
;
;       theObject = Obj_New("SCALEIMAGE")
;
; CATIMAGEES:
;
;       CATIMAGE
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { SCALEIMAGE, $
;             bottom: 0, $                    ; The lowest value in the image.
;             missing_value: Ptr_New(),  $    ; The missing value in the image.
;             missing_color: 0, $             ; The color index for missing colors.
;             ncolors: 0, $                   ; The number of colors to display.
;             sclmin: 0.0D, $                 ; The minimum scale value.
;             sclmax: 0.0D, $                 ; The maximun scale value.
;             gamma: 0.0D, $                  ; The gamma scale factor.
;             beta: 0.0D, $                   ; The beta scale factor.
;             mean: 0.0D, $                   ; The mean scale factor.
;             exponent: 0.0, $                ; The exponent scale factor.
;             scaletype: 0, $                 ; The type of scaling. 
;             INHERITS CATIMAGE $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 27 July 2006.
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
;       ScaleImage::CONTROLPANEL
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
PRO ScaleImage::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.
   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Image Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)
   self -> SetProperty, Description='ScaleImage Properties'

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.
   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, $
      Name='IMAGE PROPERTYSHEET', YSize=13, Description='ScaleImage Properties')
   aproperties -> SetProperty, Event_Object=self

    ; Display the control panel if it created its own TLB.
    IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center
   
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ScaleImage::CREATEDISPLAYIMAGE
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
PRO ScaleImage::CreateDisplayImage

   @cat_pro_error_handler

    ; Doing multiple plots?
    IF Total(!P.Multi) GT 0 THEN multi = 1 ELSE multi = 0
    IF Keyword_Set(multi) THEN BEGIN
    
          ; Draw the invisible plot to get plot position.
          currentWindow = !D.WINDOW
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

        IF Ptr_Valid(self.missing_value) THEN BEGIN
           i = Where(image EQ *self.missing_value, count)
           IF count NE 0 THEN BEGIN
              image = Float(Temporary(image))
              image[i] = !VALUES.F_NAN
           ENDIF
        ENDIF
        i = Where(Finite(image) EQ 0, count)
        image = Temporary(self->ScaleTheImage(image)) + self.bottom
        IF count GT 0 THEN image[i] = self.missing_color
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
            IF Ptr_Valid(self.missing_value) THEN BEGIN
               i = Where(image EQ *self.missing_value, count)
               IF count NE 0 THEN Begin
                  image = Float(Temporary(image))
                  image[i] = !VALUES.F_NAN
               ENDIF
            ENDIF
            i = Where(Finite(image) EQ 0, count)
            image = Temporary(self->ScaleTheImage(image)) + self.bottom
            IF count GT 0 THEN IF Ptr_Valid(self.missing_value) THEN image[i] = *self.missing_value
            IF Ptr_Valid(self._displayImage) THEN BEGIN
               *self._displayImage = Congrid(image, xsize, ysize, $
                  INTERP=self._interpolate)
            ENDIF ELSE BEGIN
               self._displayImage = Ptr_New(Congrid(image, xsize, ysize, $
                  INTERP=self._interpolate), /No_Copy)
            ENDELSE

            END

        1: BEGIN
        
            image = (*self._dataPtr)[*, self._x1:self._x2, self._y1:self._y2]
            IF Ptr_Valid(self.missing_value) THEN BEGIN
               i = Where(image EQ *self.missing_value, count)
               IF count NE 0 THEN Begin
                  image = Float(Temporary(image))
                  image[i] = !VALUES.F_NAN
               ENDIF
            ENDIF
            i = Where(Finite(image) EQ 0, count)
            image = Temporary(self->ScaleTheImage(image)) + self.bottom
            IF count GT 0 THEN image[i] = self.missing_color
           IF Ptr_Valid(self._displayImage) THEN BEGIN
              *self._displayImage = Congrid(image, 3, xsize, ysize, $
               INTERP=self._interpolate)
           ENDIF ELSE BEGIN
              self._displayImage = Ptr_New(Congrid(image, 3, xsize, ysize, $
               INTERP=self._interpolate), /No_Copy)
           ENDELSE

           END

        2: BEGIN

           image = (*self._dataPtr)[self._x1:self._x2, *, self._y1:self._y2]
            IF Ptr_Valid(self.missing_value) THEN BEGIN
               i = Where(image EQ *self.missing_value, count)
               IF count NE 0 THEN Begin
                  image = Float(Temporary(image))
                  image[i] = !VALUES.F_NAN
               ENDIF
            ENDIF
            i = Where(Finite(image) EQ 0, count)
            image = Temporary(self->ScaleTheImage(image)) + self.bottom
            IF count GT 0 THEN image[i] = self.missing_color
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
            IF Ptr_Valid(self.missing_value) THEN BEGIN
               i = Where(image EQ *self.missing_value, count)
               IF count NE 0 THEN Begin
                  image = Float(Temporary(image))
                  image[i] = !VALUES.F_NAN
               ENDIF
            ENDIF
            i = Where(Finite(image) EQ 0, count)
            image = Temporary(self->ScaleTheImage(image)) + self.bottom
            IF count GT 0 THEN image[i] = self.missing_color
           IF Ptr_Valid(self._displayImage) THEN BEGIN
              *self._displayImage = Congrid(image, xsize, ysize, 3, $
               INTERP=self._interpolate)
           ENDIF ELSE BEGIN
              self._displayImage = Ptr_New(Congrid(image, xsize, ysize, 3, $
               INTERP=self._interpolate), /No_Copy)
           ENDELSE

           END

     ENDCASE
  ENDIF

END


;*****************************************************************************************************
;+
; NAME:
;        ScaleImage::EVENT_HANDLER
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
PRO ScaleImage::EventHandler, event

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
               END

               'POSITION': BEGIN
                  event.component -> GetProperty, Position=pos
                  event.id -> GetProperty, ID=group_leader
                  position = AdjustPosition(pos, Group_Leader=group_leader)
                  event.component -> SetProperty, Position=position
                  CatRefreshDraw, self, Stop_At='DrawWidget'
                  RETURN
               END
               
               'SCALETYPE': BEGIN
                  event.component -> GetProperty, SCALETYPE=scaletype
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=event.component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value
                  
                  ; After scale type changes, you need to update the min and max.
                  event.component -> GetProperty, SCLMIN=sclmin, SCLMAX=sclmax
                  event.component -> SetPropertyByIdentifier, 'SCLMIN', sclmin
                  event.component -> SetPropertyByIdentifier, 'SCLMAX', sclmax
                  CatRefreshDraw, self, Stop_At='DrawWidget'
                  self._controlPanel -> Refresh_Properties
               END

               ELSE: BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value

                  ; Refresh the graphics hierarchy. (Exit if you have deleted the object.)
                  IF Obj_Valid(self) THEN CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase ELSE RETURN

               END

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
;       SCALEIMAGE::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain SCALEIMAGE properties. Be sure
;       you ALWAYS call the CATIMAGE GETPROPERTY method if you have extra
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
;     BETA:          The beta factor in a Hyperpolic Sine stretch. 
;
;     BOTTOM:         The lowest value of the image.
;
;     EXPONENT:      The logarithm exponent in a logarithmic stretch. 
;
;     GAMMA:         The gamma factor in a gamma stretch. 
;
;     MEAN:          The mean factor in a logarithmic stretch. Default is 0.5.
;
;     MISSING_COLOR:  The name of the missing color.
;
;     MISSING_VALUE:  The number that represents missing value in the image.
;
;     NCOLORS:        The number of colors the image is scaled into.
;
;     NEGATIVE:       If set, the display is doing a negative or reverse stretch.
;
;     SCALETYPE:     The type of scaling performed prior to display. Default is 0, linear scaling.
;           Number   Type of Stretch
;             0         Linear         scaled = BytScl(image, MIN=minThresh, MAX=maxThresh)
;             1         Gamma          scaled = GmaScl(image, MIN=minThresh, MAX=maxThresh, Gamma=gamma)
;             2         Log            scaled = LogScl(image, MIN=minThresh, MAX=maxThresh, Mean=mean, Exponent=exponent)
;             3         Asinh          scaled = AsinhScl(image, MIN=minThresh, MAX=maxThresh, Beta=beta)
;             4         Linear 2%      A linear stretch, with 2 percent of pixels clipped at both the top and bottom
;             5         Square Root    A linear stretch of the square root histogram of the image values.
;             6         Equalization   A linear stretch of the histogram equalized image histogram.
;             7         Gaussian       A Gaussian normal function is applied to the image histogram.
;
;     SCLMIN:         The image data is scaled between SCLMIN and SCLMAX before display. Default = 0.
;
;     SCLMAX:         The image data is scaled between SCLMIN and SCLMAX before display. Default = 255.
;
;     SIGMA:          The current value of the SIGMA scale factor in Gaussian stretches.
;
;     _REF_EXTRA:     Any keywords appropriate for the CATIMAGE GetProperty method.
;-
;*****************************************************************************************************
PRO SCALEIMAGE::GetProperty, $
   BETA=beta, $
   BOTTOM=bottom, $
   EXPONENT=exponent, $
   GAMMA=gamma, $
   MEAN=mean, $
   MISSING_COLOR=missing_color, $
   MISSING_VALUE=missing_value, $
   NCOLORS=ncolors, $
   NEGATIVE=negative, $
   SCALETYPE=scaletype, $
   SCLMIN=sclmin, $
   SCLMAX=sclmax, $
   SIGMA=sigma, $
   _REF_EXTRA=extraKeywords


   @cat_pro_error_handler

   beta = self.beta
   bottom = self.bottom
   exponent = self.exponent
   gamma = self.gamma
   mean = self.mean
   missing_color = self.missing_color
   IF Ptr_Valid(self.missing_value) THEN missing_value=*self.missing_value ELSE missing_value = !VALUES.F_NAN
   ncolors = self.ncolors
   negative = self.negative
   scaletype = self.scaletype
   sclmin = self.sclmin
   sclmax = self.sclmax
   sigma = self.sigma
   
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATIMAGE::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       SCALEIMAGE::SCALEtheIMAGE
;
; PURPOSE:
;
;       This method does the actual scaling of the image data.
;
;
; SYNTAX:
;
;       scaledImage = theObject -> ScaleTheImage()
;
; RETURN_VALUE:
;
;     scaledImage:  The scaled image.
;
; ARGUMENTS:
;
;     image:        The image to be scaled. Must be a 2D image. A 24-bit image (a 3D image,
;                   in which one of the dimensions is a 3) is immediately returned.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
FUNCTION SCALEIMAGE::ScaleTheImage, image

   @cat_func_error_handler
   
   ; Only 2D images are scaled. 24-bit images are returned immediately.
   dims = Size(image, /N_DIMENSIONS)
   IF dims EQ 3 THEN BEGIN
      index = Where(Size(image, /DIMENSIONS) EQ 3, count)
      IF count EQ 1 THEN RETURN, image
   ENDIF
   
   stretchTypes = ['Linear', 'Gamma', 'Log', 'Asinh', 'Linear 2%', 'Square Root', $
                   'Equalization', 'Gaussian', 'None']
   
   CASE self.scaleType OF
   
      0: BEGIN
         scaledImage = BytScl(image, Max=self.sclmax, Min=self.sclmin, /NAN, TOP=self.ncolors-1)
         IF self.negative THEN RETURN, Byte(self.ncolors-1) - scaledImage ELSE RETURN, scaledImage
         END

      1: BEGIN
         scaledImage = GmaScl(image, Max=self.sclmax, Min=self.sclmin, $
                   Gamma=self.gamma, Negative=self.negative, OMAX=self.ncolors-1)
         RETURN, scaledImage
         END

      2: BEGIN
         scaledImage =  LogScl(image, Max=self.sclmax, Min=self.sclmin, $
                   Mean=self.mean, Exponent=self.exponent, Negative=self.negative, OMAX=self.ncolors-1)
         RETURN, scaledImage
         END

      3: BEGIN
         scaledImage = ASinhScl(image, Max=self.sclmax, Min=self.sclmin, $
                  BETA=self.beta, Negative=self.negative, OMAX=self.ncolors-1)
         RETURN, scaledImage
         END
           
      4: BEGIN
         scaledImage = BytScl(image, Max=self.sclmax, Min=self.sclmin, /NAN, TOP=self.ncolors-1)
         IF self.negative THEN RETURN, Byte(self.ncolors-1) - scaledImage ELSE RETURN, scaledImage
         END

      5: BEGIN
         scaledImage = BytScl(SQRT(image), Max=self.sclmax, Min=self.sclmin, /NAN, TOP=self.ncolors-1)
         IF self.negative THEN RETURN, Byte(self.ncolors-1) - scaledImage ELSE RETURN, scaledImage
         RETURN, scaledImage
         END

      6: BEGIN
         scaledImage = BytScl(Hist_Equal(image), Max=self.sclmax, Min=self.sclmin, /NAN, TOP=self.ncolors-1)
         IF self.negative THEN RETURN, Byte(self.ncolors-1) - scaledImage ELSE RETURN, scaledImage
         END

      7: BEGIN
         scaledImage = GaussScl(image, Max=self.sclmax, Min=self.sclmin, $
                   Sigma=self.sigma, Negative=self.negative, OMAX=self.ncolors-1)
         RETURN, scaledImage
         END

      8: RETURN, image
      

        ELSE: Message, 'Unknown scaling index.'
        
   ENDCASE
   

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SCALEIMAGE::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the SCALEIMAGE object's properties. Be sure
;       you ALWAYS call the CATIMAGE SETPROPERTY method if you have extra keywords!
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
;     BETA:          The beta factor in a Hyperpolic Sine stretch. Default is 3.0.
;
;     BOTTOM:        The lowest value of the scaled image.
;
;     EXPONENT:      The logarithm exponent in a logarithmic stretch. Default is 4.0.
;
;     GAMMA:         The gamma factor in a gamma stretch. Default is 1.5.
;
;     MEAN:          The mean factor in a logarithmic stretch. Default is 0.5.
;
;     NEGATIVE:      Set this keyword to get a negative scaling (reverse).
;
;     IMAGE:         The image data. If SCLMIN and SCLMAX are not used in conjunction with this
;                    keyword, the image is scaled with a 2% linear scaling.
;
;     MISSING_COLOR: The name of the missing color.
;
;     MISSING_VALUE: The number that represents missing value in the image.
;
;     NCOLORS:       The number of colors to scale the data into, as in this: (Default: 256)
;
;                     displayImage = BYTSCL(image, MIN=self.sclmin, MAX=self.sclmax, TOP=self.ncolors-1)
;
;     SCALETYPE:     The type of scaling performed prior to display. Default is 0, linear scaling.
;
;           Number   Type of Stretch
;             0         Linear         scaled = BytScl(image, MIN=minThresh, MAX=maxThresh)
;             1         Gamma          scaled = GmaScl(image, MIN=minThresh, MAX=maxThresh, Gamma=gamma)
;             2         Log            scaled = LogScl(image, MIN=minThresh, MAX=maxThresh, Mean=mean, Exponent=exponent)
;             3         Asinh          scaled = AsinhScl(image, MIN=minThresh, MAX=maxThresh, Beta=beta)
;             4         Linear 2%      A linear stretch, with 2 percent of pixels clipped at both the top and bottom
;             5         Square Root    A linear stretch of the square root histogram of the image values.
;             6         Equalization   A linear stretch of the histogram equalized image histogram.
;             7         Gaussian       A Gaussian normal function is applied to the image histogram.
;
;     SCLMIN:        The image data is scaled between SCLMIN and SCLMAX before display. Default = 0.
;
;     SCLMAX:        The image data is scaled between SCLMIN and SCLMAX before display. Default = 255.
;
;     SIGMA:         The sigma scale factor for Gaussian scaling. Default is 1.0.
;
;     _EXTRA:        Any keywords appropriate for the CATIMAGE SetProperty method.
;-
;*****************************************************************************************************
PRO SCALEIMAGE::SetProperty, $
   BETA=beta, $
   BOTTOM=bottom, $
   EXPONENT=exponent, $
   GAMMA=gamma, $
   IMAGE=image, $
   MEAN=mean, $
   MISSING_COLOR=missing_color, $
   MISSING_VALUE=missing_value, $
   NCOLORS=ncolors, $
   NEGATIVE=negative, $
   SCALETYPE=scaletype, $
   SCLMIN=sclmin, $
   SCLMAX=sclmax, $
   SIGMA=sigma, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(beta) NE 0 THEN self.beta = beta
   IF N_Elements(bottom) NE 0 THEN self.bottom = bottom
   IF N_Elements(exponent) NE 0 THEN self.exponent = exponent
   IF N_Elements(gamma) NE 0 THEN self.gamma = gamma
   IF N_Elements(mean) NE 0 THEN self.mean = mean
   IF N_Elements(negative) NE 0 THEN self.negative = negative
   IF N_Elements(image) NE 0 THEN self -> CATIMAGE::SetProperty, IMAGE=image
   IF N_Elements(missing_color) NE 0 THEN self.missing_color = missing_color
   IF N_Elements(missing_value) NE 0 THEN BEGIN
      IF Ptr_Valid(self.missing_value) THEN *self.missing_value = missing_value ELSE $
         self.missing_value = Ptr_New(missing_value)
   ENDIF
   IF N_Elements(ncolors) NE 0 THEN self.ncolors = ncolors
   IF N_Elements(sclmin) NE 0 THEN self.sclmin = sclmin
   IF N_Elements(sclmax) NE 0 THEN self.sclmax = sclmax
   IF N_Elements(sigma) NE 0 THEN self.sigma = sigma
   
   IF N_Elements(scaletype) NE 0 THEN BEGIN
        IF Size(scaletype, /TNAME) EQ 'STRING' THEN BEGIN
              possibleTypes = ['LINEAR', 'GAMMA', 'LOG', 'ASINH', $
                               'LINEAR 2%', 'SQUARE ROOT', 'EQUALIZATION', 'GAUSSIAN']
              index = Where(possibleTypes EQ StrUpCase(scaletype), count)
              IF count EQ 0 THEN Message, 'Unknown scaling type encountered.'
              scaletype = index
        ENDIF
        self.scaletype = scaletype
                
        ; Linear 2% Scaling needs to be initialized properly.
        IF scaletype EQ 4 THEN BEGIN 
        
           
        
            ; Calculate binsize.
            maxr = Max(Float(*self._dataPtr), MIN=minr, /NAN)
            range = maxr - minr
            IF Size(*self._dataPtr, /TName) EQ 'BYTE' THEN binsize = 1.0 ELSE binsize = range / 300.
            h = Histogram(*self._dataPtr, BINSIZE=binsize, OMIN=omin, OMAX=omax)
            n = N_Elements(*self._dataPtr)
            cumTotal = Total(h, /CUMULATIVE)
            minIndex = Value_Locate(cumTotal, n * 0.02)
            IF minIndex EQ -1 THEN minIndex = 0
            WHILE cumTotal[minIndex] EQ cumTotal[minIndex + 1] DO BEGIN
                 minIndex = minIndex + 1
            ENDWHILE
            self.sclmin = minIndex * binsize + omin

            maxIndex  = Value_Locate(cumTotal, n * 0.98)
            WHILE cumTotal[maxIndex] EQ cumTotal[maxIndex - 1] DO BEGIN
                maxIndex = maxIndex - 1
            ENDWHILE
            self.sclmax = maxIndex * binsize + omin
        ENDIF
           
   ENDIF

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATIMAGE::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SCALEIMAGE::CLEANUP
;
; PURPOSE:
;
;       This is the SCALEIMAGE object class destructor method.
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
PRO SCALEIMAGE::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self.missing_value

   self -> CATIMAGE::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SCALEIMAGE::INIT
;
; PURPOSE:
;
;       This is the SCALEIMAGE object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     image       The image to load into the object.
;
; KEYWORDS:
;
;     BOTTOM:        The lowest value of the image.
;
;     BETA:          The beta factor in a Hyperpolic Sine stretch. Default is 3.0.
;
;     EXPONENT:      The logarithm exponent in a logarithmic stretch. Default is 4.0.
;
;     GAMMA:         The gamma factor in a gamma stretch. Default is 1.5.
;
;     MEAN:          The mean factor in a logarithmic stretch. Default is 0.5.
;
;     MISSING_COLOR: The name of the missing color. By default, "ivory".
;     
;     MISSING_VALUE: The number that represents missing value in the image.
;
;     NCOLORS:       The number of colors to scale the data into, as in this: (Default: 256)
;
;     NEGATIVE:      Set this keyword if you want to display the image with a negative or reverse stretch.
;
;     SCALETYPE:     The type of scaling performed prior to display. Default is 0, linear scaling.
;                    May be specified as a number or as a string (e.g, 3 or "ASIGH").
;
;           Number   Type of Stretch
;             0         Linear         scaled = BytScl(image, MIN=minThresh, MAX=maxThresh)
;             1         Gamma          scaled = GmaScl(image, MIN=minThresh, MAX=maxThresh, Gamma=gamma)
;             2         Log            scaled = LogScl(image, MIN=minThresh, MAX=maxThresh, Mean=mean, Exponent=exponent)
;             3         Asinh          scaled = AsinhScl(image, MIN=minThresh, MAX=maxThresh, Beta=beta)
;             4         Linear 2%      A linear stretch, with 2 percent of pixels clipped at both the top and bottom
;             5         Square Root    A linear stretch of the square root histogram of the image values.
;             6         Equalization   A linear stretch of the histogram equalized image histogram.
;             7         Gaussian       A Gaussian normal function is applied to the image histogram.
;             8         None           No scaling whatsoever is done.
;
;     SCLMIN:        The image data is scaled between SCLMIN and SCLMAX before display. Default is MIN(image).
;
;     SCLMAX:        The image data is scaled between SCLMIN and SCLMAX before display. Default is MAX(image).
;
;     SIGMA:         The sigma scale factor in a Gaussian stretch. Default is 1.0.
;
;     _EXTRA:        Any keywords appropriate for the CATIMAGE INIT method.
;-
;*****************************************************************************************************
FUNCTION SCALEIMAGE::INIT, image, $
   BETA=beta, $
   BOTTOM=bottom, $
   EXPONENT=exponent, $
   GAMMA=gamma, $
   MEAN=mean, $
   MISSING_COLOR=missing_color, $
   MISSING_VALUE=missing_value, $
   NCOLORS=ncolors, $
   SCALETYPE=scaletype, $
   SCLMIN=sclmin, $
   SCLMAX=sclmax, $
   SIGMA=sigma, $
   _EXTRA=extraKeywords

   ; Set up error handler and call CATIMAGE INIT method
   @cat_func_error_handler

   ok = self -> CATIMAGE::INIT (image, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   IF N_Elements(bottom) EQ 0 THEN bottom = 0
   IF N_Elements(beta) EQ 0 THEN beta = 3.0
   IF N_Elements(exponent) EQ 0 THEN exponent = 4.0
   IF N_Elements(gamma) EQ 0 THEN gamma = 1.5
   IF N_Elements(mean) EQ 0 THEN mean = 0.5
   IF N_Elements(missing_color) EQ 0 THEN missing_color = "ivory"
   IF N_Elements(ncolors) EQ 0 THEN ncolors = 256
   possibleTypes = ['LINEAR', 'GAMMA', 'LOG', 'ASINH', $
                    'LINEAR 2%', 'SQUARE ROOT', 'EQUALIZATION', 'GAUSSIAN', 'NONE']
   IF N_Elements(scaletype) EQ 0 THEN scaletype = 0 ELSE BEGIN
         IF Size(scaletype, /TNAME) EQ 'STRING' THEN BEGIN
              index = Where(possibleTypes EQ StrUpCase(scaletype), count)
              IF count EQ 0 THEN Message, 'Unknown scaling type encountered.'
              scaletype = index
        ENDIF
   ENDELSE
   IF N_Elements(sigma) EQ 0 THEN sigma = 1.0
   IF N_Elements(sclmin) EQ 0 THEN IF N_Elements(image) NE 0 THEN sclmin = Min(image, /NAN) ELSE sclmin = 0
   IF N_Elements(sclmax) EQ 0 THEN IF N_Elements(image) NE 0 THEN sclmax = Max(image, /NAN) ELSE sclmax = 255

   ; Load the object.
   self.beta = beta
   self.bottom = bottom
   self.exponent = exponent
   self.gamma = gamma
   self.mean = mean
   self.missing_color = missing_color
   IF N_Elements(missing_value) NE 0 THEN self.missing_value = Ptr_New(missing_value)
   self.ncolors = ncolors
   self.negative = 0
   self.sclmin = sclmin
   self.sclmax = sclmax
   self.sigma = sigma
   self.scaletype = scaletype
   
   ; Linear 2% scaling needs to be initialized properly.
   IF self.scaletype EQ 4 THEN self -> SetProperty, ScaleType=4
   
   ; Register properties for the property sheet.
   self->RegisterProperty, 'ScaleType', 9, NAME="Scale Type", ENUMLIST=possibleTypes
   self->RegisterProperty, 'Beta', 3, NAME="Beta Value"
   self->RegisterProperty, 'Exponent', 3, NAME="Exponent Value"
   self->RegisterProperty, 'Gamma', 3, NAME="Gamma Value"
   self->RegisterProperty, 'Mean', 3, NAME="Mean Value"
   self->RegisterProperty, 'Negative', 1, NAME="Reverse Image"
   self->RegisterProperty, 'Sclmin', 3, NAME="Minimum Threshold"
   self->RegisterProperty, 'Sclmax', 3, NAME="Maximum Threshold"

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       SCALEIMAGE CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the SCALEIMAGE object.
;
;*****************************************************************************************************
PRO SCALEIMAGE__DEFINE, class

   class = { SCALEIMAGE, $
             bottom: 0, $                    ; The lowest value in the image.
             missing_color: "", $             ; The color index for missing color.
             missing_value: Ptr_New(),  $    ; The missing value in the image.
             ncolors: 0, $                   ; The number of colors to display.
             sclmin: 0.0D, $                 ; The minimum scale value.
             sclmax: 0.0D, $                 ; The maximun scale value.
             gamma: 0.0D, $                  ; The gamma scale factor.
             beta: 0.0D, $                   ; The beta scale factor.
             mean: 0.0D, $                   ; The mean scale factor.
             negative: 0, $                  ; Take the reverse of the stretch.
             exponent: 0.0, $                ; The exponent scale factor.
             scaletype: 0, $                 ; The type of scaling. 
             sigma: 0.0, $                   ; The sigma scale factor.
             INHERITS CATIMAGE $
           }

END