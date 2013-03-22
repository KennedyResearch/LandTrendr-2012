;*****************************************************************************************************
;+
; NAME:
;       CATTRUECOLORIMAGE
;
; PURPOSE:
;
;       This object implements a class for handling true-color image data. It is a subclassed
;       CatImageData object and can be subclassed for specific image implementations.
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
;       CATIMAGEDATA
;       CATDATAATOM
;       ATOM
;       CATCONTAINER IDL_CONTAINER
;
; SYNTAX:
;
;       imageObject = OBJ_NEW ('CATTRUECOLORIMAGE', image)
;
; CLASS DEFINITION:
;
;    class = { CATTRUECOLORIMAGE, $              ; The CATTRUECOLORIMAGE object class.
;              INHERITS CatImageData $           ; Inherits the CatImageData class.
;            }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 19 April 2003.
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
;       CATTRUECOLORIMAGE::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the object
;
; SYNTAX:
;
;       imageObject -> ControlPanel, baseObj
;
; ARGUMENTS:
;
;       baseObject:    The object reference of a base widget for this control to
;                      be added to. If not supplied, the control panel will be a
;                      self contained window.
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the "CONTROLPANEL::INIT" method.
;
;
;-
;*****************************************************************************************************
PRO CatTrueColorImage::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Create a new control panel

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Image Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

      ; Create the rest of the widgets.

   base = Obj_New('BASEWIDGET', cp, Column=1, Name='STASH')

   otherbutton = Obj_New('BUTTONWIDGET', base, Value='True-Color Image Properties', NAME='IMAGE PROPERTIES')

   ; Add axes controls if this image has axes.
   axes = self -> Get(/ALL, ISA='IMGAXES', Count=numAxes)
   IF numAxes GT 0 THEN BEGIN
      axes[0] -> GetProperty, VISIBLE=onoff
      void = Obj_New('BUTTONWIDGET', base, Value='Axis Properties', NAME='AXES_PROPERTIES')
   ENDIF

   ; Display the control panel if it created its own TLB.

   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATTRUECOLORIMAGE::EVENTHANDLER
;
; PURPOSE:
;
;       This method is an event handler for the Control Panel.
;
; SYNTAX:
;
;       Called automatically by the event handling system
;
; ARGUMENTS:
;
;       event:  The event structure.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO CatTrueColorImage::EventHandler, event

   @cat_pro_error_handler


   event.ID -> GetProperty, Name=name
   CASE name OF


      'AXES_PROPERTIES': BEGIN

         axes = self -> Get(/ALL, ISA='IMGAXES', Count=numAxes)
         IF numAxes GT 0 THEN FOR j=0,numAxes-1 DO axes[j] -> ControlPanel, GROUP_LEADER=self._controlPanel

         END

      'IMAGE PROPERTIES': BEGIN
         self -> GetProperty, Name=theName
         register_name = theName + Get_Object_ID(self)
         IF XRegistered(register_name) EQ 0 THEN BEGIN
            pbase = Obj_New('TOPLEVELBASE', Group_Leader=event.id, Title='Image Properties', $
               Name='IMAGE_PROPERTIES_TLB_RESIZE', Exclusive_Event_Object=self, /SIZE_EVENTS)
            iproperties = Obj_New('PROPERTYSHEETWIDGET', pbase, Value=self, Name='IMAGE PROPERTYSHEET', $
               YSize=11)
            iproperties -> SetProperty, Exclusive_Event_Object=self
            pbase -> SetProperty, UValue=iproperties
            pbase -> Draw, /Center, Register_Name=register_name
         ENDIF
         END

      'IMAGE_PROPERTIES_TLB_RESIZE': BEGIN
         event.id -> GetProperty, UValue=propertysheet
         propertysheet -> SetProperty, Scr_XSize=event.x
         propertysheet -> SetProperty, /Refresh_Property
         END

      'IMAGE PROPERTYSHEET': BEGIN

         IF event.type EQ 0 THEN BEGIN
            CASE StrUpCase(event.identifier) OF

               'IMAGE': BEGIN
                  self->LoadImage, Group_Leader=event.id
                  CatRefreshDraw, self, Stop_At='DrawWidget'
               ENDCASE

               'BACKGROUNDCOLOR': BEGIN

                  event.component -> GetProperty, BackgroundColor=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, BackgroundColor=color
                  CatRefreshDraw, self, Stop_At='DrawWidget'

               ENDCASE

               'POSITION': BEGIN
                  event.component -> GetProperty, Position=pos
                  event.id -> GetProperty, ID=group_leader
                  position = AdjustPosition(pos, Group_Leader=group_leader)
                  event.component -> SetProperty, Position=position
                  CatRefreshDraw, self, Stop_At='DrawWidget'

               ENDCASE

               ELSE: BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value
                  CatRefreshDraw, self, Stop_At='DrawWidget'

                  CASE StrUpCase(event.identifier) OF

                     'ERASE': BEGIN
                        IF value EQ 1 THEN $
                           self -> SetPropertyAttribute, 'BackgroundColor', Sensitive=1 ELSE $
                           self -> SetPropertyAttribute, 'BackgroundColor', Sensitive=0
                           event.id -> SetProperty, /Refresh_Property
                        END
                     ELSE:

                  ENDCASE

               ENDCASE

            ENDCASE
         ENDIF


         END

      ELSE: Message, 'Unhandled event ' + StrUpCase(name) + ' in CatTrueColorImage::EventHandler.'
   ENDCASE

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATTRUECOLORIMAGE::LOADIMAGE
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
;       _EXTRA:        Any keyword appropriate for the CatImageData::LOADIMAGE method.
;
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
PRO CatTrueColorImage::LoadImage, $
   Cancel=cancel, $                ; An output keyword. Returns 0 if the ACCEPT button is used, 1 otherwise.
   FileInfo=fileInfo, $            ; An output keyword containing file information from the Query_*** routine.
   OutDirectory=outdirectory, $    ; The directory name of the selected image file.
   OutFilename=outfilename, $      ; The short filename (without directory) of the selected image file.
   _Extra=extrakeywords

   @cat_pro_error_handler

   self -> CatImageData::LoadImage, Only3D=1, Cancel=cancel, FileInfo=fileinfo, $
      OutDirectory=outdirectory, OutFilename=outfilename,  _Extra=extrakeywords

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CATTRUECOLORIMAGE::PIXEL_TO_VALUE
;
; PURPOSE:
;
;       This method calculates the image RGB value underneath a pixel location
;       in the display window. To be accurate, the window containing the image MUST
;       be the current graphics window.
;
; SYNTAX:
;
;       image_value = imageObject -> Pixel_to_Value(x, y)
;
; RETURN_VALUE:
;
;       image_value: The image value at image[x,y]
;
; ARGUMENTS:
;
;       X:           The x location in the display window (pixel coordinates).
;
;       Y:           The y location in the display window (pixel coordinates).
;
; INPUT_KEYWORDS:
;
;       OUTSIDE:     The value to represent "outside" the image. Default is !Values.F_NAN.
;
; OUTPUT_KEYWORDS:
;
;       INSIDE:      Returns a 1 if the (x,y) values are inside the image and 0 othewise.
;
;       WINDOWINDEX: The window index number of the window containing the image object. Set
;                    to !D.Window by default.
;
;       XDATA:       The x data value with respect to the image data coordinate system,
;
;       XPIXEL:      The x pixel value in terms of image (rather than window) device coordinates.
;
;       YDATA:       The y data value with respect to the image data coordinate system,
;
;       YPIXEL:      The y pixel value in terms of image (rather than window) device coordinates.
;-
;*****************************************************************************************************
FUNCTION CatTrueColorImage::Pixel_to_Value, x, y, $
   Inside=inside, $
   Outside=outside, $
   WindowIndex=wid, $
   XData=xdata, $
   XPixel=x_img, $
   YData=ydata, $
   YPixel=y_img

   @cat_func_error_handler

   IF N_Elements(wid) EQ 0 THEN wid = !D.Window
   currentWindow = !D.Window
   WSet, wid

   ; What outside value does user want?

   IF N_Elements(outside) EQ 0 THEN outside = !Values.F_NAN
   inside = 0

   ; Convert input pixel location to normalized coordinates.

   xpix = x / self._location[4,0]
   ypix = y / self._location[5,0]

   ; If input point is outside the image, return the outside value.

   IF (xpix LT self._location[0,1]) OR (xpix GT self._location[2,1]) THEN BEGIN
      IF currentWindow GE 0 THEN WSet, currentWindow
      RETURN, outside
   ENDIF
   IF (ypix LT self._location[1,1]) OR (ypix GT self._location[3,1]) THEN BEGIN
      IF currentWindow GE 0 THEN WSet, currentWindow
      RETURN, outside
   ENDIF

   ; Now you are inside the image.

   inside = 1

   ; Convert the point, which is in device coordinates with respect to the window, into
   ; device coordinates with respect to the image.

   xprime = x - self._location[0,0]
   yprime = y - self._location[1,0]
   x_img = 0 > Round((xprime * Float(self._xsize)) / (self._location[2,0] - self._location[0,0] + 1)) < (self._xsize -1)
   y_img = 0 > Round((yprime * Float(self._ysize)) / (self._location[3,0] - self._location[1,0] + 1)) < (self._ysize -1)

   ; Get the data coordinate system.

   IF Obj_Valid(self._coords) THEN BEGIN
      self._coords -> Draw
      c = Convert_Coord(x, y, /Device, /To_Data)
      xdata = c[0,0]
      ydata = c[1,0]
   ENDIF ELSE BEGIN
      xdata = x_img
      ydata = y_img
   ENDELSE

   ; Return the image RGB value.

   dimensions = Image_Dimensions(*self._original, TrueIndex=trueIndex)
   CASE trueIndex OF
      0: BEGIN
         r = (Reform((*self._original)[0,*,*], self._xsize, self._ysize))[x_img,y_img]
         g = (Reform((*self._original)[1,*,*], self._xsize, self._ysize))[x_img,y_img]
         b = (Reform((*self._original)[2,*,*], self._xsize, self._ysize))[x_img,y_img]
         ENDCASE

      1: BEGIN
         r = (Reform((*self._original)[*,0,*], self._xsize, self._ysize))[x_img,y_img]
         g = (Reform((*self._original)[*,1,*], self._xsize, self._ysize))[x_img,y_img]
         b = (Reform((*self._original)[*,2,*], self._xsize, self._ysize))[x_img,y_img]
         ENDCASE

      2: BEGIN
         r = (Reform((*self._original)[*,*,0], self._xsize, self._ysize))[x_img,y_img]
         g = (Reform((*self._original)[*,*,1], self._xsize, self._ysize))[x_img,y_img]
         b = (Reform((*self._original)[*,*,2], self._xsize, self._ysize))[x_img,y_img]
         ENDCASE
   ENDCASE

   retValue = [r, g, b]

   ; Don't return byte values because they make lousy strings. :-(

   IF Size(retValue, /TNAME) EQ 'BYTE' THEN retValue=Fix(retValue)
   RETURN, retValue
END



;*****************************************************************************************************
;+
; NAME:
;       CATTRUECOLORIMAGE::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set properties of the CATTRUECOLORIMAGE object
;
; SYNTAX:
;
;       imageObject -> SETPROPERTY, Image=newImage
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;     IMAGE:   A true-color (24-bit) image array to load into the object.
;
;     _EXTRA:  Any keyword appropriate for the SETPROPERTY method of the superclass object.
;
;-
;*****************************************************************************************************
PRO CatTrueColorImage::SetProperty, Image=image, _Extra=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(image) NE 0 THEN $
   BEGIN
      dims = Size(image, /N_Dimensions)
      IF dims NE 3 THEN Message, 'The image does not appear to be a true-color image.'
      index = Where(dims EQ 3, count)
      IF count EQ 0 THEN Message, 'The image does not appear to be a true-color image.'
   ENDIF

   self -> CatImageData::SetProperty, Image=image, _Extra=extraKeywords

   self -> Report, /Completed
END




;*****************************************************************************************************
;+
; NAME:
;       CATTRUECOLORIMAGE::INIT
;
; PURPOSE:
;
;       This method is used upon object creation.
;
; SYNTAX:
;
;       Called automatically when the object is created thus:
;
;           imageObject = OBJ_NEW ('CatTrueColorImage', image)
;
; ARGUMENTS:
;
;       image           The 24-bit image array of byte type.
;
; KEYWORDS:
;
;     _EXTRA:           Any keyword appropriate for the INIT method of the superclass object or for
;                       the LoadImage method.
;-
;*****************************************************************************************************
FUNCTION CatTrueColorImage::INIT, $
   image, $
   _EXTRA=extraKeywords

   @cat_func_error_handler

   ; Only 24-bit images are allowed.
   IF N_Elements(image) NE 0 THEN $
   BEGIN

      dims = Size(image, /N_Dimensions)
      IF dims NE 3 THEN Message, 'The image does not appear to be a true-color image.'
      index = Where(dims EQ 3, count)
      IF count EQ 0 THEN Message, 'The image does not appear to be a true-color image.'

   ENDIF

   ; Call the superclass INIT method
   ok = self -> CatImageData::INIT (image, _EXTRA=extraKeywords, Description='True-Color Image Properties')
   IF ~ok THEN BEGIN
      self -> Report, /Failed
      RETURN, 0
   ENDIF

   ; Register properties for the property sheet.
   self->RegisterProperty, 'Image', 0, NAME="Image Data", USERDEF='Load New Image'
   self->RegisterProperty, 'Position', 0, NAME="Image Position", USERDEF='Image Position'
   self->RegisterProperty, 'Keep_Aspect', 1, NAME="Keep Aspect Ratio"
   self->RegisterProperty, 'Erase', 1, NAME="Erase Before Draw"
   self->RegisterProperty, 'BackgroundColor', 0, NAME="Background Color", USERDEF="Background Color", $
      SENSITIVE=Keyword_Set(self._erase)
   self->RegisterProperty, 'NoResize', 1, NAME="No Image Resize"

   self -> Report, /Completed
   RETURN, 1
END



;*****************************************************************************************************
;
; NAME:
;       CATTRUECOLORIMAGE CLASS DEFINITION
;
; PURPOSE:
;
;       This is the class definition code for the CATTRUECOLORIMAGE object.
;
;*****************************************************************************************************
PRO CatTrueColorImage__DEFINE, class

    class = { CATTRUECOLORIMAGE, $              ; The CATTRUECOLORIMAGE object class.
              INHERITS CatImageData $           ; Inherits the CatImageData object class.
            }
END

