;*****************************************************************************************************
;+
; NAME:
;       CATIMAGE2D
;
; PURPOSE:
;
;       This object implements a class for handling 2D image data. It is a subclassed
;       ImageData object and can be subclassed for specific image implementations.
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
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; SYNTAX:
;
;       imageObject = OBJ_NEW ('CATIMAGE2D', image)
;
; CLASS_DEFINITION:
;
;    class = { CATIMAGE2D, $                     ; The CATIMAGE2D object class.
;              INHERITS CatImageData, $          ; Inherits the CatImageData object class. MUST be inherited first.
;              _smooth_width: 0L, $              ; The width value for the SMOOTH operation.
;              _median_width: 0L $               ; The width value for the MEDIAN operation.
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
;       CATIMAGE2D::CONTROLPANEL
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
PRO CatImage2D::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Create a new control panel

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Image Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

      ; Create the rest of the widgets.

   base = Obj_New('BASEWIDGET', cp, Column=1, Name='STASH')

   pbutton = Obj_New('BUTTONWIDGET', base, /MENU, Value='Image Processing')

      edgeID = Obj_New('BUTTONWIDGET', pbutton, /MENU, Value='Edge Enhancement', /Separator)
         void = Obj_New('BUTTONWIDGET', edgeID, Value='Sobel', NAME='SOBEL')
         void = Obj_New('BUTTONWIDGET', edgeID, Value='Roberts', NAME='ROBERTS')
         void = Obj_New('BUTTONWIDGET', edgeID, Value='Sharpen', NAME='SHARPEN')

      smoothID = Obj_New('BUTTONWIDGET', pbutton, /MENU, Value='Smoothing')
         void = Obj_New('BUTTONWIDGET', smoothID, Value='Smooth', NAME='SMOOTH')
         void = Obj_New('BUTTONWIDGET', smoothID, Value='Median', NAME='MEDIAN')

      reverseID = Obj_New('BUTTONWIDGET', pbutton, /MENU, Value='Reverse Image')
         void = Obj_New('BUTTONWIDGET', reverseID, Value='Flip Horizontal', NAME='FLIP HORIZONTAL')
         void = Obj_New('BUTTONWIDGET', reverseID, Value='Flip Vertical', NAME='FLIP VERTICAL')

      contrastID = Obj_New('BUTTONWIDGET', pbutton, /MENU, Value='Contrast')
         void = Obj_New('BUTTONWIDGET', contrastID, Value='Histogram Equalize', NAME='HISTOGRAM EQUALIZE')

      void = Obj_New('BUTTONWIDGET', pbutton,Value='Original Image', NAME='ORIGINAL IMAGE', /Separator)

      void = Obj_New('BUTTONWIDGET', pbutton, Value='Undo', NAME='UNDO', /Dynamic_Resize, Sensitive=0, /Separator)
      void = Obj_New('BUTTONWIDGET', pbutton, Value='Redo', NAME='REDO', /Dynamic_Resize, Sensitive=0)

   cbutton = Obj_New('BUTTONWIDGET', base, /MENU, Value='Color Adjustments')
      void = Obj_New('BUTTONWIDGET', cbutton, Value='Image Colors', NAME='XCOLORS')
      void = Obj_New('BUTTONWIDGET', cbutton, Value='Reverse Colors', NAME='REVERSE_COLORS')
      void = Obj_New('BUTTONWIDGET', cbutton, Value='Reset Histogram Stretch', NAME='RESET_HISTOSTRETCH')

   histo = Obj_New('CW_HISTOSTRETCH', base, self, /Frame, Name='HISTOSTRETCH')
   self -> RegisterForMessage, histo, 'CATIMAGE_NEWIMAGE'

   otherbutton = Obj_New('BUTTONWIDGET', base, Value='Other Image Properties', NAME='OTHER PROPERTIES')

   ; Add axes controls if this image has axes.
   axes = self -> Get(/ALL, ISA='IMGAXES', Count=numAxes)
   IF numAxes GT 0 THEN BEGIN
      axes[0] -> GetProperty, On=onoff
      void = Obj_New('BUTTONWIDGET', base, Value='Axis Properties', NAME='AXES_PROPERTIES')
   ENDIF

   ; Display the control panel if it created its own TLB.

   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATIMAGE2D::EVENTHANDLER
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
PRO CatImage2D::EventHandler, event

   @cat_pro_error_handler


   event.ID -> GetProperty, Name=name
   CASE name OF

      'XCOLORS': BEGIN
         self._colors -> XColors, Group_Leader=event.id
         END

      'REVERSE_COLORS': BEGIN
         self._colors -> Reverse
         END

      'SOBEL': BEGIN
         self -> IP, 'sobel', Name='Sobel'
         undoButton = self._controlPanel -> Get('UNDO', /Recursive_Search)
         undoButton -> SetProperty, Value='Undo (Sobel)', Sensitive=1
         redoButton = self._controlPanel -> Get('REDO', /Recursive_Search)
         redoButton -> SetProperty, Value='Redo', Sensitive=0

         ; Reset the histostretch program.
         histoStretch = self._controlPanel -> Get('HISTOSTRETCH', /Recursive_Search)
         IF Obj_Valid(histoStretch) THEN histoStretch -> Reset
         END

      'ROBERTS': BEGIN
         self -> IP, 'roberts', Name='Roberts'
         undoButton = self._controlPanel -> Get('UNDO', /Recursive_Search)
         undoButton -> SetProperty, Value='Undo (Roberts)', Sensitive=1
         redoButton = self._controlPanel -> Get('REDO', /Recursive_Search)
         redoButton -> SetProperty, Value='Redo', Sensitive=0

         ; Reset the histostretch program.
         histoStretch = self._controlPanel -> Get('HISTOSTRETCH', /Recursive_Search)
         IF Obj_Valid(histoStretch) THEN histoStretch -> Reset
         END

      'SHARPEN': BEGIN
         self -> IP, 'sharpen', Name='Sharpen'
         undoButton = self._controlPanel -> Get('UNDO', /Recursive_Search)
         undoButton -> SetProperty, Value='Undo (Sharpen)', Sensitive=1
         redoButton = self._controlPanel -> Get('REDO', /Recursive_Search)
         redoButton -> SetProperty, Value='Redo', Sensitive=0

         ; Reset the histostretch program.
         histoStretch = self._controlPanel -> Get('HISTOSTRETCH', /Recursive_Search)
         IF Obj_Valid(histoStretch) THEN histoStretch -> Reset
         END


      'SMOOTH': BEGIN
         self -> IP, 'smooth', self._smooth_width, Name='Smooth'
         undoButton = self._controlPanel -> Get('UNDO', /Recursive_Search)
         undoButton -> SetProperty, Value='Undo (Smooth)', Sensitive=1
         redoButton = self._controlPanel -> Get('REDO', /Recursive_Search)
         redoButton -> SetProperty, Value='Redo', Sensitive=0

         ; Reset the histostretch program.
         histoStretch = self._controlPanel -> Get('HISTOSTRETCH', /Recursive_Search)
         IF Obj_Valid(histoStretch) THEN histoStretch -> Reset
         END

      'MEDIAN': BEGIN
         self -> IP, 'median', self._median_width, Name='Median'
         undoButton = self._controlPanel -> Get('UNDO', /Recursive_Search)
         undoButton -> SetProperty, Value='Undo (Median)', Sensitive=1
         redoButton = self._controlPanel -> Get('REDO', /Recursive_Search)
         redoButton -> SetProperty, Value='Redo', Sensitive=0

         ; Reset the histostretch program.
         histoStretch = self._controlPanel -> Get('HISTOSTRETCH', /Recursive_Search)
         IF Obj_Valid(histoStretch) THEN histoStretch -> Reset
          END

      'FLIP HORIZONTAL': BEGIN

         ; Check for IMGAXES children. If any found, flip the X axis range.
         children = self -> Get(/All, Count=childNo)
         FOR j=0, childNo-1 DO BEGIN
            IF Obj_Class(children[j]) EQ 'IMGAXES' THEN BEGIN
               children[j] -> GetProperty, XRange=xr
               children[j] -> SetProperty, XRange=Reverse(xr)
             ENDIF
         ENDFOR

         ; Axes will not be draw properly unless the image erases
         ; the display.
         eraseIt = self._erase
         self._erase = 1
         self -> IP, 'reverse', 1, /Draw, Name='Flip Horizontal', /NoCache, /Destroy
         self._erase=eraseIt

         END

      'FLIP VERTICAL': BEGIN

         ; Check for IMGAXES children. If any found, flip the X axis range.
         children = self -> Get(/All, Count=childNo)
         FOR j=0, childNo-1 DO BEGIN
            IF Obj_Class(children[j]) EQ 'IMGAXES' THEN BEGIN
               children[j] -> GetProperty, YRange=yr
               children[j] -> SetProperty, YRange=Reverse(yr)
             ENDIF
         ENDFOR

         ; Axes will not be draw properly unless the image erases
         ; the display.
         self -> GetProperty, Erase=eraseIt
         self -> SetProperty, Erase=1
         self -> IP, 'reverse', 2, /Draw, Name='Flip Vertical', /NoCache, /Destroy
         self -> SetProperty, Erase=eraseIt
         END

      'HISTOGRAM EQUALIZE': BEGIN
         self -> IP, 'hist_equal',  Name='Histogram Equalize'
         undoButton = self._controlPanel -> Get('UNDO', /Recursive_Search)
         undoButton -> SetProperty, Value='Undo (Histogram Equalize)', Sensitive=1
         redoButton = self._controlPanel -> Get('REDO', /Recursive_Search)
         redoButton -> SetProperty, Value='Redo', Sensitive=0

         ; Reset the histostretch program.
         histoStretch = self._controlPanel -> Get('HISTOSTRETCH', /Recursive_Search)
         IF Obj_Valid(histoStretch) THEN histoStretch -> Reset
         END

      'ORIGINAL IMAGE': BEGIN
         self -> GetProperty, TOOLLIST=theToolList
         IF Obj_Valid(theToolList) THEN theToolList -> Remove, /All
         self -> SetOriginal
         undoButton = self._controlPanel -> Get('UNDO', /Recursive_Search)
         undoButton -> SetProperty, Value='Undo', Sensitive=0
         redoButton = self._controlPanel -> Get('REDO', /Recursive_Search)
         redoButton -> SetProperty, Value='Redo', Sensitive=0

         ; Reset the histostretch program.
         histoStretch = self._controlPanel -> Get('HISTOSTRETCH', /Recursive_Search)
         IF Obj_Valid(histoStretch) THEN histoStretch -> Reset
         END

      'UNDO': BEGIN
         self -> GetProperty, TOOLLIST=theToolList
         IF Obj_Valid(theToolList) THEN theToolList -> GetProperty, Current_Tool=theTool
         IF Obj_Valid(theTool) THEN theTool -> GetProperty, Name=toolName
         redoButton = self._controlPanel -> Get('REDO', /Recursive_Search)
         IF N_Elements(toolName) NE 0 THEN BEGIN
             redoButton -> SetProperty, Value='Redo (' + toolName + ')', Sensitive=1
         ENDIF ELSE redoButton -> SetProperty, Value='Redo', Sensitive=0

         self -> Undo

         self -> GetProperty, TOOLLIST=theToolList
         IF Obj_Valid(theToolList) THEN theToolList -> GetProperty, Current_Tool=theTool, Current_Index=theIndex
         IF Obj_Valid(theTool) THEN theTool -> GetProperty, Name=toolName
         undoButton = self._controlPanel -> Get('UNDO', /Recursive_Search)
         IF N_Elements(toolName) NE 0 THEN BEGIN
            IF theIndex LT 0 THEN $
            undoButton -> SetProperty, Value='Undo', Sensitive=0 ELSE $
            undoButton -> SetProperty, Value='Undo (' + toolName + ')', Sensitive=1
         ENDIF ELSE undoButton -> SetProperty, Value='Undo', Sensitive=0

         ; Reset the histostretch program.
         histoStretch = self._controlPanel -> Get('HISTOSTRETCH', /Recursive_Search)
         IF Obj_Valid(histoStretch) THEN histoStretch -> Reset
         END


      'REDO': BEGIN
         self -> Redo

         self -> GetProperty, TOOLLIST=theToolList
         IF Obj_Valid(theToolList) THEN theToolList -> GetProperty, Current_Tool=theTool, Current_Index=theIndex
         IF Obj_Valid(theTool) THEN theTool -> GetProperty, Name=toolName
         undoButton = self._controlPanel -> Get('UNDO', /Recursive_Search)
         IF N_Elements(toolName) NE 0 THEN BEGIN
            undoButton -> SetProperty, Value='Undo (' + toolName + ')', Sensitive=1
         ENDIF ELSE undoButton -> SetProperty, Value='Undo', Sensitive=0


         IF theIndex NE (theToolList -> Count() - 1) THEN BEGIN
            theTools = theToolList -> Get(/All)
            theTool = theTools[theIndex+1]
         ENDIF
         IF Obj_Valid(theTool) THEN theTool -> GetProperty, Name=toolName
         redoButton = self._controlPanel -> Get('REDO', /Recursive_Search)
         IF theIndex EQ (theToolList -> Count() - 1) THEN BEGIN
                redoButton -> SetProperty, Value='Redo', Sensitive=0
         ENDIF ELSE redoButton -> SetProperty, Value='Redo (' + toolName + ')', Sensitive=1

         ; Reset the histostretch program.
         histoStretch = self._controlPanel -> Get('HISTOSTRETCH', /Recursive_Search)
         IF Obj_Valid(histoStretch) THEN histoStretch -> Reset
         END

      'RESET_HISTOSTRETCH': BEGIN

         ; Reset the histostretch program.
         histoStretch = self._controlPanel -> Get('HISTOSTRETCH', /Recursive_Search)
         IF Obj_Valid(histoStretch) THEN histoStretch -> Reset
         END

      'OK': BEGIN
         Obj_Destroy, self._controlpanel
         END
      'APPLY':
      'CANCEL':

      'AXES_PROPERTIES': BEGIN

         axes = self -> Get(/ALL, ISA='IMGAXES', Count=numAxes)
         IF numAxes GT 0 THEN FOR j=0,numAxes-1 DO axes[j] -> ControlPanel, GROUP_LEADER=self._controlPanel

         END

      'OTHER PROPERTIES': BEGIN
         self -> GetProperty, Name=theName
         register_name = theName + Get_Object_ID(self)
         IF XRegistered(register_name) EQ 0 THEN BEGIN
            topObject = CatGetTopObject(event.id)
            pbase = Obj_New('TOPLEVELBASE', Group_Leader=topObject, Title='Image Properties', $
               Name='OTHER_PROPERTIES_TLB_RESIZE', Exclusive_Event_Object=self, /SIZE_EVENTS)
            iproperties = Obj_New('PROPERTYSHEETWIDGET', pbase, Value=self, Name='IMAGE PROPERTYSHEET', $
               YSize=11)
            iproperties -> SetProperty, Exclusive_Event_Object=self
            pbase -> SetProperty, UValue=iproperties
            pbase -> Draw, /Center, Register_Name=register_name
         ENDIF
         END

      'OTHER_PROPERTIES_TLB_RESIZE': BEGIN
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

      ELSE: Message, 'Unhandled event ' + StrUpCase(name) + ' in CatImage2D::EventHandler.'
   ENDCASE

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATIMAGE2D::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain CATIMAGE2D properties.
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
;     MEDIAN_WIDTH: The size of the two-dimensional neighborhood to use with the MEDIAN function.
;
;     SMOOTH_WIDTH: The size of the two-dimensional neighborhood to use with the SMOOTH function.
;
;     __REF_EXTRA:  Any keyword appropriate for the SETPROPERTY method of the superclass object.
;
;-
;*****************************************************************************************************
PRO CatImage2D::GetProperty, $
   Median_Width=median_width, $
   Smooth_Widths=smooth_width, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(smooth_width) THEN smooth_width = self._smooth_width
   IF Arg_Present(median_width) THEN median_width = self._median_width
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATIMAGEDATA::GetProperty, _EXTRA=extraKeywords


   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATIMAGE2D::LOADIMAGE
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
; INPUT_KEYWORDS:
;
;       _EXTRA:        Any keyword appropriate for the ImageData::LOADIMAGE method.
;
;
; OUTPUT_KEYWORDS:
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
PRO CatImage2D::LoadImage, $
   Cancel=cancel, $                ; An output keyword. Returns 0 if the ACCEPT button is used, 1 otherwise.
   FileInfo=fileInfo, $            ; An output keyword containing file information from the Query_*** routine.
   OutDirectory=outdirectory, $    ; The directory name of the selected image file.
   OutFilename=outfilename, $      ; The short filename (without directory) of the selected image file.
   _Extra=extrakeywords

   @cat_pro_error_handler

   self -> CatImageData::LoadImage, Only2D=1, Cancel=cancel, FileInfo=fileinfo, $
      OutDirectory=outdirectory, OutFilename=outfilename, _Extra=extrakeywords

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CATIMAGE2D::PIXEL_TO_VALUE
;
; PURPOSE:
;
;       This method calculates the original image value underneath a pixel location
;       in the display window. To be accurate, the window that contains the image
;       MUST BE the current graphics window or you MUST PASS the window number in
;       with the WINDOWINDEX keyword. This is NOT nessarily the value of the image
;       in the display window. It is the original image value.
;
; SYNTAX:
;
;       image_value = imageObject -> Pixel_to_Value(x, y)
;
; RETURN_VALUE:
;
;       image_value:   The image value at image[x,y]
;
; ARGUMENTS:
;
;       X:             The x location in the display window (pixel coordinates).
;
;       Y:             The y location in the display window (pixel coordinates).
;
; INPUT_KEYWORDS:
;
;       OUTSIDE:       The value to represent "outside" the image. Default is !Values.F_NAN.
;
; OUTPUT_KEYWORDS:
;
;       DISPLAY_VALUE: Set this keyword to a named variable that will contain the value
;                      of the display image, rather than the original image.
;
;       INSIDE:        Returns a 1 if the (x,y) values are inside the image and 0 otherwise.
;
;       WINDOWINDEX:   The window index number of the window containing the image object. Set
;                      to !D.Window by default.
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
FUNCTION CatImage2D::Pixel_to_Value, x, y, $
   DISPLAY_VALUE=display_value, $
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

   ; Convert input pixel location to normalized coordinates in the display window.

   xpix = x / self._location[4,0]
   ypix = y / self._location[5,0]

   ; If input point is outside the image, return the outside value.

   IF (xpix LT self._location[0,1]) OR (xpix GT self._location[2,1]) THEN BEGIN
      IF currentWindow GE 0 THEN WSet, currentWindow
      retValue = outside
      RETURN, retValue
   ENDIF
   IF (ypix LT self._location[1,1]) OR (ypix GT self._location[3,1]) THEN BEGIN
      IF currentWindow GE 0 THEN WSet, currentWindow
      retValue = outside
      RETURN, retValue
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

   IF Obj_Isa_Valid(self._coords, 'CATCOORD') THEN BEGIN
      self._coords -> Draw
      c = Convert_Coord(x, y, /Device, /To_Data)
      xdata = c[0,0]
      ydata = c[1,0]
      ;self._coords -> Restore
   ENDIF ELSE BEGIN
      xdata = x_img
      ydata = y_img
   ENDELSE

   ; Return the original image value.

   retValue = (*self._original)[x_img, y_img]

   ; Need the display value?

   IF Arg_Present(display_value) THEN display_value = (self -> GetData())[x_img, y_img]

   ; Don't return byte values because they make lousy strings. :-(

   IF Size(retValue, /TNAME) EQ 'BYTE' THEN retValue=Fix(retValue)

   ; Set the window back to entering value.

   IF currentWindow GE 0 THEN WSet, currentWindow

;   Update_ControlPanel:
;
;   IF Obj_Valid(self._controlPanel) THEN BEGIN
;      self._controlPanel -> GetProperty, UValue=statusbar
;      IF N_Elements(statusbar) NE 0 THEN BEGIN
;         IF inside THEN BEGIN
;               s = 'X: '+ StrTrim(xdata,2) + '  Y: ' + StrTrim(ydata,2)
;               IF Obj_Valid(statusbar[0]) THEN statusbar[0] -> SetProperty, Text=s
;               s =  + 'Value: ' + StrTrim(retValue,2)
;               IF Obj_Valid(statusbar[0]) THEN statusbar[1] -> SetProperty, Text=s
;         ENDIF ELSE BEGIN
;            IF Obj_Valid(statusbar[0]) THEN statusbar[0] -> SetProperty, Text='Outside Image'
;            IF Obj_Valid(statusbar[0]) THEN statusbar[1] -> SetProperty, Text='Outside Image'
;         ENDELSE
;      ENDIF
;   ENDIF

   RETURN, retValue
END



;*****************************************************************************************************
;+
; NAME:
;       CATIMAGE2D::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set properties of the CATIMAGE2D object
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
;     IMAGE:   A 2D image array to load into the object.
;
;     MEDIAN_WIDTH: The size of the two-dimensional neighborhood to use with the MEDIAN function.
;
;     SMOOTH_WIDTH: The size of the two-dimensional neighborhood to use with the SMOOTH function.
;
;     _EXTRA:  Any keyword appropriate for the SETPROPERTY method of the superclass object.
;
;-
;*****************************************************************************************************
PRO CatImage2D::SetProperty, $
   Image=image, $
   Median_Width=median_width, $
   Smooth_Width=smooth_width, $
   _Extra=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(image) NE 0 THEN $
   BEGIN
      dims = Size(image, /N_Dimensions)
      IF dims NE 2 THEN Message, 'The image argument must be a 2D array.'
      self -> CatImageData::SetProperty, Image=image
   ENDIF

   IF N_Elements(median_width) NE 0 THEN self._median_width = median_width
   IF N_Elements(smooth_width) NE 0 THEN self._smooth_width = smooth_width

   self -> CatImageData::SetProperty, _Extra=extraKeywords

   self -> Report, /Completed
END




;*****************************************************************************************************
;+
; NAME:
;       CATIMAGE2D::INIT
;
; PURPOSE:
;
;       This method is used upon object creation.
;
; SYNTAX:
;
;       Called automatically when the object is created thus:
;
;           imageObject = OBJ_NEW ('CatImage2D', image)
;
; ARGUMENTS:
;
;       image           The 2D image array of any data type.
;
; KEYWORDS:
;
;     MEDIAN_WIDTH:     The size of the two-dimensional neighborhood to use with the MEDIAN function.
;
;     SMOOTH_WIDTH:     The size of the two-dimensional neighborhood to use with the SMOOTH function.
;
;     _EXTRA:           Any keyword appropriate for the INIT method of the superclass object or for
;                       the LoadImage method.
;-
;*****************************************************************************************************
FUNCTION CatImage2D::INIT, image, $
   NAME=name, $
   Median_Width=median_width, $
   Smooth_Width=smooth_width, $
   _EXTRA=extraKeywords


   @cat_func_error_handler

   IF N_Elements(name) EQ 0 THEN name = '2D Image Object'

   ; Only 2D images are allowed.
   IF N_Elements(image) NE 0 THEN $
   BEGIN

      ndim = Size(image, /N_Dimensions)
      IF ndim NE 2 THEN Message, 'A 2D image is required. This image has ' +StrTrim(ndim,2) + ' dimensions.

   ENDIF

   ; Check keywords.
   IF N_Elements(smooth_width) EQ 0 THEN smooth_width = 7
   IF N_Elements(median_width) EQ 0 THEN median_width = 7

   ; Load object.
   self._smooth_width = smooth_width
   self._median_width = median_width

   ; Call the superclass INIT method
   ok = self -> CatImageData::INIT (image, _EXTRA=extraKeywords, Name=name, Description='Image Parameters')

   ; Report and return status
   IF ~ok THEN BEGIN
      self -> Report, /Failed
      RETURN, 0
   ENDIF

   ; If there is no valid color object at this point, create one.
   IF Obj_Valid(self._colors) EQ 0 THEN BEGIN
      self._colors = Obj_New('ColorTool')
      self._colors -> RegisterForMessage, self, 'COLORTOOL_TABLECHANGE'
   ENDIF

   ; Register properties for the property sheet.
   self->RegisterProperty, 'Image', 0, NAME="Image Data", USERDEF='Load New Image'
   self->RegisterProperty, 'Position', 0, NAME="Image Position", USERDEF='Image Position'
   self->RegisterProperty, 'Keep_Aspect', 1, NAME="Keep Aspect Ratio"
   self->RegisterProperty, 'Erase', 1, NAME="Erase Before Draw"
   self->RegisterProperty, 'BackgroundColor', 0, NAME="Background Color", USERDEF="Background Color", $
      SENSITIVE=Keyword_Set(self._erase)
   self->RegisterProperty, 'NoResize', 1, NAME="No Image Resize"
   self->RegisterProperty, 'Interpolate', 1, NAME="Bilinear Interpolation"
   self->RegisterProperty, 'Smooth_Width', 2, NAME="Smoothing Width", VALID_RANGE=[3, 99]
   self->RegisterProperty, 'Median_Width', 2, NAME="Median Width", VALID_RANGE=[3, 99]

   RETURN, 1
END

;*****************************************************************************************************
;
; NAME:
;       CATIMAGE2D CLASS DEFINITION
;
; PURPOSE:
;
;       This is the class definition code for the CATIMAGE2D object.
;
;*****************************************************************************************************
PRO CatImage2D__DEFINE, class

   @cat_pro_error_handler

    class = { CATIMAGE2D, $                     ; The CATIMAGE2D object class.
              INHERITS CatImageData, $          ; Inherits the CatImageData object class. MUST be inherited first.
              _smooth_width: 0L, $              ; The width value for the SMOOTH operation.
              _median_width: 0L $               ; The width value for the MEDIAN operation.
            }
END

