;*****************************************************************************************************
;+
; NAME:
;       CATALYST
;
; PURPOSE:
;
;       This program has no purpose except to test and evaluate Catalyst Library
;       functionality. It can be used as examples of good programming practices
;       (well, assuming your definition of "good" is pretty loose). The way it is 
;       used mostly is to look in the GUI method for the "names" of objects that
;       generate events. Once you find a name, you look for that name in the EventHandler
;       method. In this way, you slowly learn how Catalyst functionality is achieved.
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
;       Catalyst
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, April 17, 2003.
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
PRO Catalyst::CreateStatusBar

; The purpose of this method is to create the statusbar for the program.

      ; Create a statusbar object.
   self._statusbar = OBJ_NEW('STATUSBAR', self, Name='Statusbar', /Align_Left)


END



;*****************************************************************************************************
PRO Catalyst::DroplistEvents, event

; The purpose of this method is to see if events can be directed to
; an event handler method *other* than one named EventHandler.

   self._statusbar -> SetProperty, Text='Widget '+ event.name + ' value: ' + StrTrim(*event.selection,2)

END
;*****************************************************************************************************


PRO Catalyst::EventHandler, event

; This is the main event handler for the test program. All widget objects
; causing events have been named so we can branch on EVENT.NAME.

   @cat_pro_error_handler

   ; Branch on the object name.
   CASE StrUpCase(event.name) OF

      'COMBOBOX': BEGIN
         IF event.index EQ -1 THEN BEGIN
            event.id -> GetProperty, Text=val
            event.id -> SetProperty, Add=val
         ENDIF ELSE BEGIN
            event.id -> GetProperty, Value=val
            val = val[event.index]
         ENDELSE
         self._statusbar -> SetProperty, Text= $
            'Widget '+  event.name +  ' value: ' +  StrTrim(val,2)
         END

      'SPINNER': BEGIN
         self._statusbar -> SetProperty, Text= $
            'Spinner widget value: ' + String(event.value, Format='(G0)')
         END

      'SURFACE DRAW WIDGET': BEGIN

         ; Post instructions in the status bar.
         IF event.type NE 2 THEN self._statusbar -> SetProperty, $
            Text='ROTAING SURFACE -- LEFT button: Rotate   MIDDLE button: Zoom Out   RIGHT button: Zoom In'

         END

      'HISTOGRAM DRAW WIDGET': BEGIN

         ; Make this draw widget the current window.
         event.id -> SetWindow

         ; Get the histogram plot object and find out if you are inside the plot.
         histogramPlot = event.id -> Get('HistogramPlot')
         val = histogramPlot -> Pixel_To_Value(event.x, event.y, Inside=inside)

         ; If you are inside, then find the closest X data value and
         ; use that to plot the X and Y data values.
         IF inside THEN BEGIN
            histogramPlot -> GetProperty, P1=histodata
            xdata = Indgen(N_Elements(histodata))
            x = val[0]
            realx = Value_Locate( xdata, x)
            strText = 'Value: ' + Strtrim(realx[0], 2) + '  No. Pixels: ' + StrTrim(histodata[realx], 2)
         ENDIF ELSE strText = 'Outside Plot Area'
         self._statusbar -> SetProperty, Text=strText

         END

      'TRUE-COLOR DRAW WIDGET': BEGIN

         ; Make this draw widget the current window.
         event.id -> SetWindow

         ; Get the image associated with this draw widget.
         imageObject = event.ID -> Get('TrueColor Image Example')

         ; Find the RGB value of the image at this location and display in statusbar.
         value = imageObject -> Pixel_To_Value(event.x, event.y, Inside=inside, XPixel=xpix, YPixel=ypix, $
            XData=xdata, YData=ydata)
         IF inside EQ 0 THEN self._statusbar -> SetProperty, Text='Outside Image' ELSE $
         BEGIN
            s = 'R: ' + StrTrim(value[0],2) + '  G: ' + StrTrim(value[1],2) + '  B: ' + StrTrim(value[2],2)
            s = s + '    Image Coordinate: (' + StrTrim(xpix,2) + ', ' + StrTrim(ypix,2) + ')' + $
            '    Image Location: (' + String(xdata,Format='(F6.2)') + ', ' + String(ydata,Format='(F6.2)') + ')'
            self._statusbar -> SetProperty, Text=s
         ENDELSE
         END

      'MAP IMAGE DRAW WIDGET': BEGIN

         ; It is possible that the user can delete the image!
         IF Obj_Valid(imageObject) EQ 0 THEN RETURN

         ; Make this draw widget the current window.
         event.id -> SetWindow

         ; Get the image associated with this draw widget.
         imageObject = event.ID -> Get('Moveable Graphics')

         ; Find the lat/lon values of the image at this location and display the value
         ; at that location in the statusbar.

         value = imageObject -> Pixel_To_Value(event.x, event.y, Inside=inside, $
            XPixel=xpix, YPixel=ypix, XData=xdata, YData=ydata)
         IF inside EQ 0 THEN self._statusbar -> SetProperty, Text='Outside Image' ELSE $
         BEGIN
            s = 'Lon: ' + StrTrim(xdata,2) + '  Lat: ' + StrTrim(ydata,2) + '  Value: ' + StrTrim(value,2)
            self._statusbar -> SetProperty, Text=s
         ENDELSE
         END

      'MEDICAL IMAGE DRAW WIDGET': BEGIN

         ; If the first button of the tool bar is selected, these are draw widget events.
         topObject = CatGetTopObject(event.id)
         toolbar = topObject -> Get('TOOLBAR', /Recursive)
         firstButton = toolbar -> Get()
         firstButton -> GetProperty, Button_Set=set

         IF ~set THEN BEGIN
            IF Obj_Valid(self.currentInteraction) THEN BEGIN
               self.currentInteraction -> SetDisplay, event.id
               self.currentInteraction -> EventHandler, event
            ENDIF
            RETURN
         ENDIF

         ; Make this draw widget the current window.
         event.id -> SetWindow

         ; Get the image associated with this draw widget.
         knee = event.ID -> Get('KNEEIMAGE')

         ; What kind of event is this?
         possibleEvents = ['DOWN', 'UP', 'MOTION', 'SCROLL', 'EXPOSE', 'CH_CHAR', 'KEY_CHAR']
         thisEvent = possibleEvents[event.type]

         CASE thisEvent OF
           'DOWN': BEGIN
                    CASE event.press OF
                        1: BEGIN
                           ; If there is a control modifier, then this is from a non-three button mouse and
                           ; this is a pan event.
                           IF Float(!Version.Release) GE 5.4 THEN BEGIN
                              IF event.modifiers NE 0 THEN BEGIN
                                 event.id -> SetProperty, Motion_Events=1
                                 event.id -> GetProperty, XSize=xs, YSize=ys
                                 self.pan_x = 0 > event.x < (xs-1)
                                 self.pan_y = 0 > event.y < (ys-1)
                              ENDIF ELSE BEGIN
                                 void = knee -> Pixel_To_Value(event.x, event.y, XPixel=x_img, YPixel=y_img, Inside=inside)
                                 IF inside THEN knee -> ZoomIn, x_img, y_img
                              ENDELSE
                           ENDIF ELSE BEGIN
                              void = knee -> Pixel_To_Value(event.x, event.y, XPixel=x_img, YPixel=y_img, Inside=inside)
                              IF inside THEN knee -> ZoomIn, x_img, y_img
                           ENDELSE
                           END

                        2: BEGIN
                           event.id -> SetProperty, Motion_Events=1
                           event.id -> GetProperty, XSize=xs, YSize=ys
                           self.pan_x = 0 > event.x < (xs-1)
                           self.pan_y = 0 > event.y < (ys-1)
                           END

                        4: BEGIN
                           void = knee -> Pixel_To_Value(event.x, event.y, XPixel=x_img, YPixel=y_img, Inside=inside)
                           IF inside THEN knee -> ZoomOut, x_img, y_img
                           END

                        ELSE:

                    ENDCASE

                 END

           'UP': event.id -> SetProperty, Motion_Events=0

           'MOTION': BEGIN
                      event.id -> GetProperty, XSize=xs, YSize=ys
                      event.x = 0 > event.x < (xs-1)
                      event.y = 0 > event.y < (ys-1)
                      knee -> Pan, event.x - self.pan_x, event.y - self.pan_y
                      self.pan_x = event.x
                      self.pan_y = event.y
                     END ; MOTION case

           ELSE:

         ENDCASE

         END ; of DRAW5 Case

      'EXIT' : OBJ_DESTROY, self

      'FIELD': BEGIN
           event.id -> GetProperty, Value=val
           self._statusbar -> SetProperty, Text= 'Field value: '+ StrTrim(val[0],2)
        END

      'SAVE_AS' : BEGIN

         ; Set the tab to the third window.
         tabwidget = self -> Get('TabWidget', /Recursive_Search)
         tabwidget -> GetProperty, Current=currentTab
         baseWidget = tabwidget -> Get(Position=currentTab)
         drawWidget = baseWidget -> Get(Position=0)
         event.id -> GetProperty, Value=buttonValue

         CASE buttonValue OF
            'JPEG File': drawWidget -> Output, /JPEG, Filename='catalyst.jpg'
            'TIFF File': drawWidget -> Output, /TIFF, Filename='catalyst.tif'
            'BMP File': drawWidget -> Output, /BMP, Filename='catalyst.bmp'
            'PNG File': drawWidget -> Output, /PNG, Filename='catalyst.png'
            'PostScript File': drawWidget -> Output, /PostScript, Filename='catalyst.ps'
         ENDCASE

         END

      'SHOW CONTENTS' : self -> ShowContents

      'LIST': self._statusbar -> SetProperty, Text= $
         'Widget '+ event.name + ' value: ' + StrTrim(*event.selection,2)

      'MULTIPLE_LIST': BEGIN
         text = 'Widget '+ event.name + ' values: '
         FOR j=0, N_Elements(*event.selection)-1 DO BEGIN
            text = text + StrTrim((*event.selection)[j],2) + ', '
         ENDFOR
         text = StrMid(text, 0, StrLen(text)-2)
         self._statusbar -> SetProperty, Text=text
         END

      'RESIZEDRAWWIDGET': BEGIN

         ; Find the draw widget, resize it, and draw.
         drawObject = self -> Get('MAP IMAGE DRAW WIDGET', /Recursive_Search)

         ; Get the image associated with this draw widget.
         imageObject = drawObject -> Get('Moveable Graphics')

         ; Resize the draw widget.
         drawObject -> SetProperty, XSize=800, YSize=600

         ; Reconfigure the image in the window.
         imageObject -> SetProperty, Position=[0.1, 0.1, 0.9, 0.9]
         drawObject -> Draw, /Erase

         ; Set the tab to the fourth window.
         tabwidget = self -> Get('TabWidget', /Recursive_Search)
         tabwidget -> SetProperty, Current=3

         ; Make the button insensitive.
         event.ID -> SetProperty, Sensitive=0

         END

      'TABWIDGET': BEGIN
         CASE event.tab OF
            0: self._statusbar -> SetProperty, Text='Left Button to ZOOM IN, Right Button to ZOOM OUT, and Middle Button to PAN.' + $
                                                    ' Use buttons on right to select various types of ROIs.'
            1: self._statusbar -> SetProperty, Text='Add annotations, drag them into place, set properties by RIGHT clicking.'
            2: BEGIN
               self._statusbar -> SetProperty, Text='ROTAING SURFACE -- LEFT button: Rotate   MIDDLE button: Zoom Out     RIGHT button: Zoom In'
               drawWidget = self -> Get('SURFACE DRAW WIDGET', /Recursive_Search)
               drawWidget -> Draw
               END
            3: self._statusbar -> SetProperty, Text='LEFT click selects objects. RIGHT click selected objects for Properties. SHIFT-LEFT for multiple selection.'
            4: self._statusbar -> SetProperty, Text='All data has its own coordinate system. Move cursor on plot to get value at that location'
            5: self._statusbar -> SetProperty, Text='Move cursor on plot to get RGB value and position in image and pixel coordinates'
            ELSE:
         ENDCASE
         END


   ; Medical Image Toolbar Buttons

      'ARROW': BEGIN
         event.id -> GetProperty, Helpline=text
         self._statusbar -> SetProperty, Text=text
         IF Obj_Valid(self.currentInteraction) THEN BEGIN
            self.currentInteraction -> RestoreDisplay
            Obj_Destroy, self.currentInteraction
            self.draw5ID -> Draw
         ENDIF
         ruler = self.draw5ID -> Get('Ruler', /Recursive_Search)
         IF Obj_Valid(ruler) THEN ruler -> SetProperty, Selectable=1
         self.draw5ID -> SetProperty, Motion=0
         END

      'ANGLETOOL': BEGIN
         event.id -> GetProperty, Helpline=text
         self._statusbar -> SetProperty, Text=text
         IF Obj_Valid(self.currentInteraction) THEN BEGIN
            self.currentInteraction -> RestoreDisplay
            Obj_Destroy, self.currentInteraction
            self.draw5ID -> Draw
         ENDIF
         ruler = self.draw5ID -> Get('Ruler', /Recursive_Search)
         kneeImage = self.draw5ID -> Get('KNEEIMAGE', /Recursive)
         kneeImage -> GetProperty, Image=theImage, Zoom_Coord=zcoord
         self.currentInteraction = Obj_New('Selectinteraction', self.draw5ID, /Start_Now, $
            Name='INTERACTION', /ASK_ON_UP, UVALUE='ANGLETOOL',Thick=2, $
            /NoPicRestore, Coord_Object=zcoord, $
            STATUSBAR=self._statusbar, MODE='INSERT', $
            SelectedObject=Obj_New('AngleTool', /Clockwise, TextColor='Yellow', $
               Coord_Object=zcoord, Color='Yellow'))
         IF Obj_Valid(ruler) THEN ruler -> SetProperty, Selectable=0
         self.draw5ID -> SetProperty, Motion=0
         END

      'RECTANGLE': BEGIN
         event.id -> GetProperty, Helpline=text
         self._statusbar -> SetProperty, Text=text
         IF Obj_Valid(self.currentInteraction) THEN BEGIN
            self.currentInteraction -> RestoreDisplay
            Obj_Destroy, self.currentInteraction
            self.draw5ID -> Draw
         ENDIF
         ruler = self.draw5ID -> Get('Ruler', /Recursive_Search)
         kneeImage = self.draw5ID -> Get('KNEEIMAGE', /Recursive)
         kneeImage -> GetProperty, Image=theImage, Zoom_Coord=zcoord
         self.currentInteraction = Obj_New('Selectinteraction', self.draw5ID, /Start_Now, $
            Name='INTERACTION', /ASK_ON_UP, UVALUE='BOX',Thick=2, Color='Yellow', $
            /NoPicRestore, Coord_Object=zcoord, SelectedObject=Obj_New('Box'), MODE='INSERT')
         IF Obj_Valid(ruler) THEN ruler -> SetProperty, Selectable=0
         self.draw5ID -> SetProperty, Motion=0
         END

      'ELLIPSE': BEGIN
         event.id -> GetProperty, Helpline=text
         self._statusbar -> SetProperty, Text=text
         IF Obj_Valid(self.currentInteraction) THEN BEGIN
            Obj_Destroy, self.currentInteraction
            self.draw5ID -> Draw
         ENDIF
         self.currentInteraction = Obj_New('Selectinteraction', self.draw5ID, /Start_Now, $
            Name='INTERACTION', /ASK_ON_UP, UVALUE='ELLIPSE', Thick=2, Color='Yellow', $
            /NoPicRestore, Coord_Object=zcoord, SelectedObject=Obj_New('Ellipse'), MODE='INSERT')
         ruler = self.draw5ID -> Get('Ruler', /Recursive_Search)
         IF Obj_Valid(ruler) THEN ruler -> SetProperty, Selectable=0
         self.draw5ID -> SetProperty, Motion=0
         END

      'FREEHAND': BEGIN
         event.id -> GetProperty, Helpline=text
         self._statusbar -> SetProperty, Text=text
         IF Obj_Valid(self.currentInteraction) THEN BEGIN
            self.currentInteraction -> RestoreDisplay
            Obj_Destroy, self.currentInteraction
            self.draw5ID -> Draw
         ENDIF
         self.currentInteraction = Obj_New('Freehand_ROI', self.draw5ID, /Start_Now, $
            Name='INTERACTION', UValue='FREEHAND', /ASK_ON_UP, /NoPicRestore)
         ruler = self.draw5ID -> Get('Ruler', /Recursive_Search)
         IF Obj_Valid(ruler) THEN ruler -> SetProperty, Selectable=0
         self.draw5ID -> SetProperty, Motion=0
         END

      'MEASURETOOL': BEGIN
         event.id -> GetProperty, Helpline=text
         self._statusbar -> SetProperty, Text=text
         IF Obj_Valid(self.currentInteraction) THEN BEGIN
            self.currentInteraction -> RestoreDisplay
            Obj_Destroy, self.currentInteraction
            self.draw5ID -> Draw
         ENDIF
         ruler = self.draw5ID -> Get('Ruler', /Recursive_Search)
         kneeImage = self.draw5ID -> Get('KNEEIMAGE', /Recursive)
         kneeImage -> GetProperty, Image=theImage, Zoom_Coord=zcoord
         self.currentInteraction = Obj_New('Selectinteraction', self.draw5ID, /Start_Now, $
            Name='INTERACTION', /ASK_ON_UP, UVALUE='MEASURETOOL',Thick=2, Color='Yellow', $
            /NoPicRestore, Coord_Object=zcoord, $
            SelectedObject=Obj_New('TapeMeasure', UNITS='(cm)', Coord_Object=zcoord), $
            STATUSBAR=self._statusbar, MODE='INSERT')
         IF Obj_Valid(ruler) THEN ruler -> SetProperty, Selectable=0
         self.draw5ID -> SetProperty, Motion=0
         END

      'POLYGON': BEGIN
         event.id -> GetProperty, Helpline=text
         self._statusbar -> SetProperty, Text=text
         IF Obj_Valid(self.currentInteraction) THEN BEGIN
            self.currentInteraction -> RestoreDisplay
            Obj_Destroy, self.currentInteraction
            self.draw5ID -> Draw
         ENDIF
         self.currentInteraction = Obj_New('Selectinteraction', self.draw5ID, /Start_Now, $
            Name='INTERACTION', /ASK_ON_UP, UVALUE='POLYGON',Thick=2, Color='Yellow', $
            /NoPicRestore, Coord_Object=zcoord, SelectedObject=Obj_New('Polygon'), MODE='INSERT')
         ruler = self.draw5ID -> Get('Ruler', /Recursive_Search)
         IF Obj_Valid(ruler) THEN ruler -> SetProperty, Selectable=0
         self.draw5ID -> SetProperty, Motion=0
         END

      'RULER': ; Nothing to do. Allow ruler to be moved.
      'COLORBAR': ; Nothing to do. Allow colorbar to be moved.
      'MOVEABLE GRAPHICS': ; Nothing to do. Allow image to be moved.
      'MOVEABLE TEXT': ; Nothing to do. Allow image to be moved.
      'EASY TO CODE': ; Nothing to do. Allow image to be moved.

      ; All the various interactions from the medical image toolbars have their
      ; actual events handled here.
      'INTERACTION': BEGIN

         IF Obj_Valid(self.currentInteraction) THEN BEGIN

            ; Get the ROI points from the current interaction.
            self.currentInteraction -> GetProperty, UValue=username

            CASE username OF

               'ANGLETOOL': BEGIN

                  IF event.action EQ 'CANCEL' THEN RETURN
                  unit = event.radians ? 'radians' : 'degrees'
                  angle = 'Measured Angle = ' + String(event.angle, Format='(f0.2)') + ' ' + unit
                  self._statusbar -> SetProperty, Text=angle
                  RETURN
                  END

               'BOX': BEGIN
                  IF event.action EQ 'CANCEL' THEN RETURN

                  ; These points have been converted to the draw widget coordinate
                  ; system, which are the same as the zoom coordinate system of the
                  ; image in the window. (A result of setting the draw widget CATCOORD
                  ; object to the ZOOM coordinate object of the image in the GUI method.)
                  xpts = [event.xpts[0], event.xpts[1], event.xpts[1], event.xpts[0]]
                  ypts = [event.ypts[0], event.ypts[0], event.ypts[1], event.ypts[1]]
                  END

               'ELLIPSE': BEGIN

                  IF event.action EQ 'CANCEL' THEN RETURN

                  ; These points have been converted to the draw widget coordinate
                  ; system, which are the same as the zoom coordinate system of the
                  ; image in the window. (A result of setting the draw widget CATCOORD
                  ; object to the ZOOM coordinate object of the image in the GUI method.)
                  xpts = event.xpts
                  ypts = event.ypts
                  END

               'FREEHAND': BEGIN
                  ; These points have been converted to the draw widget coordinate
                  ; system, which are the same as the zoom coordinate system of the
                  ; image in the window.(A result of setting the draw widget CATCOORD
                  ; object to the ZOOM coordinate object of the image in the GUI method.)
                  xpts = event.xpts
                  ypts = event.ypts
                  END

               'MEASURETOOL': BEGIN

                  IF event.action EQ 'CANCEL' THEN RETURN
                  text = 'Measured Distance = ' + String(event.length, Format='(f0.2)') + ' ' + event.units
                  self._statusbar -> SetProperty, Text=text
                  RETURN

                  END
               'POLYGON': BEGIN

                  IF event.action EQ 'CANCEL' THEN RETURN

                  ; These points have been converted to the draw widget coordinate
                  ; system, which are the same as the zoom coordinate system of the
                  ; image in the window.(A result of setting the draw widget CATCOORD
                  ; object to the ZOOM coordinate object of the image in the GUI method.)
                  xpts = event.xpts
                  ypts = event.ypts
                  END

            ENDCASE

             ; Get the knee image object, and from it get the real image data, along
             ; with the zoom coordinate object and the order of the image on the display.
            topObject = CatGetTopObject(event.id)
            kneeImage = topObject -> Get('KNEEIMAGE', /Recursive)
            kneeImage -> GetProperty, Image=theImage, Zoom_Coord=zcoord, Order=order, ZoomRect=zrect

            ; If the image is displayed with (0,0) at upper left, then the ypts from
            ; the interaction have to be reversed with respect to the window coordinate
            ; system's range.
            IF order THEN BEGIN
               zcoord -> GetProperty, YRange=yrange
               ypts = Scale_Vector(ypts, yrange[1], yrange[0], Minvalue=Min(yrange), MaxValue=Max(yrange))
            ENDIF

           ; Create an ROI object for computing stats, etc.
            roi = Obj_New('IDLanROI', xpts, ypts)
            void = roi -> ComputeGeometry(Area=area, Perimeter=perimeter)
            Obj_Destroy, roi

            ; Convert the xpts and ypts from a data coordinate space to a window
            ; pixel coordinate space.
            zcoord -> Draw
            c = Convert_Coord(xpts, ypts, /Data, /To_Device)
            xpts_wpix = c[0,*]
            ypts_wpix = c[1,*]

            ; Convert from window pixel coordinate space to image pixel coordinate space.
            ximgrange = Abs(zrect[2]-zrect[0]) + 1
            yimgrange = Abs(zrect[3]-zrect[1]) + 1
            self.draw5ID -> GetProperty, XSize=xsize, YSize=ysize
            xpts_imgpix = (ximgrange * xpts_wpix) / xsize
            ypts_imgpix = (yimgrange * ypts_wpix) / ysize

            ; Calculate an image mask.
            roi = Obj_New('IDLanROI', xpts_imgpix, ypts_imgpix)
            mask = roi -> ComputeMask(Dimensions=[ximgrange, yimgrange], Mask_Rule=2)
            mask = mask GT 0
            Obj_Destroy, roi

            ; Calculate the subimage that the mask should apply to and get the average
            ; value of the pixels there.
            subimage = theimage[zrect[0]:zrect[2], zrect[1]:zrect[3]]
            avgValue = Total(subImage * mask) / Total(mask)

            ; Update the status bar with information.
            self._statusbar -> SetProperty, Text='Area: ' + StrTrim(area,2) + ',   Perimeter: ' + $
                 StrTrim(perimeter,2) + ',   Average Pixel Value: ' + StrTrim(avgValue,2)

         ENDIF


         END

         ELSE: BEGIN

         event.id -> GetProperty, Value=val
         self._statusbar -> SetProperty, Text= $
            'CATTEST::EventHandler ---> Widget ' + event.name + ' value: '+ StrTrim(val[0],2)

         END

   ENDCASE

END
;*****************************************************************************************************


PRO Catalyst::GUI, menuBar

; The purpose of this method is create all the graphical user interface elements.

   tlb_org =  OBJ_NEW ('BaseWidget', self, Column=2, /Frame)

   col1     = OBJ_NEW ('BaseWidget', tlb_org, Column=1, /Base_Align_Center)
   fileMenu = OBJ_NEW ('ButtonWidget', menuBar ,  Value='File', /Menu, Name='File Menu')
   self.saveAS = OBJ_NEW ('ButtonWidget', fileMenu,  Value='Save Window As...', /MENU, Name='SAVE AS MENU')
   void = OBJ_NEW ('ButtonWidget', self.saveAS,  Name='SAVE_AS', Value='JPEG File', Event_Object=self.saveAs)
   void = OBJ_NEW ('ButtonWidget', self.saveAS,  Name='SAVE_AS', Value='TIFF File', Event_Object=self.saveAs)
   void = OBJ_NEW ('ButtonWidget', self.saveAS,  Name='SAVE_AS', Value='BMP File', Event_Object=self.saveAs)
   void = OBJ_NEW ('ButtonWidget', self.saveAS,  Name='SAVE_AS', Value='PNG File', Event_Object=self.saveAs)
   void = OBJ_NEW ('ButtonWidget', self.saveAS,  Name='SAVE_AS', Value='PostScript File', Event_Object=self.saveAs)
   void = OBJ_NEW ('ButtonWidget', fileMenu, Value='Show Contents', NAME='SHOW CONTENTS')

   void = OBJ_NEW ('ButtonWidget', fileMenu,  Name='Exit', Value='Exit', /Separator)

   label    = OBJ_NEW ('LabelWidget',  col1, Value='Droplist Widget')
   dropList = OBJ_NEW ('DropListWidget', col1, Name='Droplist', Value=['None', 'Bill', 'Ted'], $
      Event_Method='DroplistEvents')

   label    = OBJ_NEW ('LabelWidget',  col1, Value=' ')
   label    = OBJ_NEW ('LabelWidget',  col1, Value='List Widget')
   list     = OBJ_NEW ('ListWidget' ,  col1, Name='List', Value=['None', 'Wayne', 'Garth'])

   label    = OBJ_NEW ('LabelWidget',  col1, Value=' ')
   label    = OBJ_NEW ('LabelWidget',  col1, Value='Multiple Element List Widget')
   list     = OBJ_NEW ('ListWidget' ,  col1, Name='Multiple_List', $
      Value=['Frog', 'Toad', 'Coyote', 'Cow', 'Horse', 'Wolf', 'Blue Heron'], /Multiple)

   spinner  = OBJ_NEW ('Spinner',      col1, Title='Spinner', Value=10, Increment=1, Name='SPINNER')

   label    = OBJ_NEW ('LabelWidget',  col1, Value=' ')
   label    = OBJ_NEW ('LabelWidget',  col1, Value='Slider Widget')
   slider   = OBJ_NEW ('SliderWidget', col1, Name='Slider', /Drag)

   label    = OBJ_NEW ('LabelWidget',  col1, Value=' ')
   label    = OBJ_NEW ('LabelWidget',  col1, Value='Text Widget')
   text     = OBJ_NEW ('TextWidget' ,  col1, Name='Text', Value='Some text', /Editable, /All_Events)

   label    = OBJ_NEW ('LabelWidget',  col1, Value=' ')
   label    = OBJ_NEW ('LabelWidget',  col1, Value='ComboBox Widget')
   comboBox = OBJ_NEW ('ComboBoxWidget', col1, Name='ComboBox', Value=['Coyote', 'Bill', 'Ted'], /Editable)

   label    = OBJ_NEW ('LabelWidget',  col1, Value=' ')
   label    = OBJ_NEW ('LabelWidget',  col1, Value='Button Widget')
   button   = OBJ_NEW('ButtonWidget',  col1, Name='ResizeDrawWidget', Value='Resize Map Image')

   label    = OBJ_NEW ('LabelWidget',  col1, Value=' ')
   label    = OBJ_NEW ('LabelWidget',  col1, Value='Field Widgets')
   IF !D.Name NE 'WIN' THEN fieldsize=120 ELSE fieldsize=75
   field1   = OBJ_NEW ('FieldWidget' , col1, Name='FIELD', Value='text', Title='String Field: ', $
      XSize=10, Labelsize=fieldsize, CR_EVENTS=1)
   field2   = OBJ_NEW ('FieldWidget' , col1, Name='FIELD', Value=54, Title='Integer Field: ', $
      XSize=10, Labelsize=fieldsize, CR_EVENTS=1)
   field3   = OBJ_NEW ('FieldWidget' , col1, Name='FIELD', Value=36.5, Title='Float Field: ', $
      XSize=10, Labelsize=fieldsize, CR_EVENTS=1)

   ; Set up TABing between fields.
   field1 -> SetProperty, TabNext=field2
   field2 -> SetProperty, TabNext=field3
   field3 -> SetProperty, TabNext=field1

   ;*** TEST TABWIDGET ***
   col2     = OBJ_NEW ('BaseWidget', tlb_org, Column=1)
   tabwidget = OBJ_NEW ('TABWIDGET', col2, Name='TabWidget')

   base5 = Obj_New('BASEWIDGET', tabwidget, Title='Medical Image', Row=1, Name='Medical Image Base')
   base6 = Obj_New('BASEWIDGET', tabwidget, Title='Annotation', Row=1, Name='Annotation Base')
   base1 = Obj_New('BASEWIDGET', tabwidget, Title='Surface', Row=1, Name='Surface Base')
   base4 = Obj_New('BASEWIDGET', tabwidget, Title='Map Image', Row=1, Name='Map Image Base')
   base2 = Obj_New('BASEWIDGET', tabwidget, Title='Histogram', Row=1, Name='Histogram Base')
   base3 = Obj_New('BASEWIDGET', tabwidget, Title='TrueColor Image', Row=1, Name='True-Color Image Base')

   ; Linux machines don't normally retain windows.
   IF !D.Name NE 'WIN' THEN retain=2 ELSE retain=1
   draw1 = Obj_New('ODRAWWIDGET', base1, XSIZE=500, YSize=600, NAME='SURFACE DRAW WIDGET', $
      /Button_Events, /Expose_Events, RETAIN=retain)
   draw2 = Obj_New('DRAWWIDGET',  base2, XSIZE=500, YSize=600, NAME='HISTOGRAM DRAW WIDGET', $
      RETAIN=retain, /Motion_Events)
   draw3 = Obj_New('DRAWWIDGET',  base3, XSIZE=500, YSize=600, NAME='TRUE-COLOR DRAW WIDGET', $
      RETAIN=retain, /Motion_Events, Initial_Color='ivory', /Notify_Realize)
   draw4 = Obj_New('SELECTABLEDRAWWIDGET', base4, XSIZE=600, YSize=600, NAME='MAP IMAGE DRAW WIDGET', $
      RETAIN=retain, Initial_Color='ivory', /Notify_Realize, ERASE_WINDOW=1, /SELECT_EVENTS, /BUTTON_EVENTS, /BUFFER)
   draw5 = Obj_New('SELECTABLEDRAWWIDGET',  /SELECT_EVENTS, base5, XSIZE=500, YSize=600, $
      NAME='MEDICAL IMAGE DRAW WIDGET', RETAIN=retain, /Button_Events, /Buffer)
   draw6 = Obj_New('DRAWWIDGET',  base6, XSIZE=500, YSize=600, NAME='ANNOTATION DRAW WIDGET', $
      RETAIN=retain, Initial_Color='beige', /Notify_Realize)
   self.draw5ID = draw5


   ; First TAB Window to demonstate use of object graphics in Catalyst.
   surface = Obj_New('CatSurface', Loaddata(2), Title='A Fully Rotatable Surface', NAME='Object Graphics Surface')
   draw1 -> Add, surface, /Handle_Events ; The surface object will handle these events.

    ; Create an embedded control panel widget.
   base1cpbase= Obj_New('BASEWIDGET', base1, Name='Surface ControlPanel')
   surface -> ControlPanel, base1cpbase

   ; Second TAB Window to demonstate a standard IDL graphics command.
   ; The CatGraphicsCmdTool can be used execute any IDL graphics command.
   histogramCmd = Obj_New('CatGraphicsCmdTool', Name='HistogramPlot',Decomposed=1)
   histogramCmd -> LoadCommand, 'Plot', Histogram(loaddata(5)), Max_Value=5000, XStyle=1, $
      Title='Histogram Plot', Background=FSC_Color('charcoal', Decomposed=1), $
      Color=FSC_Color('green', Decomposed=1), /NoData, $
      Position=[0.15, 0.15, 0.9, 0.9]
   histogramOplotCmd = Obj_New('CatGraphicsCmdTool', 'OPlot', Histogram(loaddata(5)), Max_Value=5000, $
      Color=FSC_Color('yellow', Decomposed=1), Name='HistogramOPlot')

   ; Adding the two graphics command tools to the draw object causes
   ; them to be drawn in the order in which they are added to the draw object.
   draw2 -> Add, histogramCmd
   draw2 -> Add, histogramOplotCmd

   ; Third TAB Window to demonstate a true-color image with axes and ability to print
   ; out image information in the statusbar window. The LOAD button will interact with
   ; this image.

   ; Create a coordinate object for the image.
   coords = Obj_New('CATCOORD', Name='True Color Image Coordinates', XRange=[0,227], YRange=[0,149])

   image3 = Obj_New('CATTRUECOLORIMAGE', LoadData(16), Position=[0.15, 0.15, 0.85, 0.85], $
      Name='TrueColor Image Example', Coord_Object=coords)
   draw3 -> Add, image3

   ; Create image axes in the window.
   axisObj = Obj_New('IMGAXES', XRange=[0,1000], YRange=[0,1000], Color='navy', $
      Name='True-Color Image Axes', Description='Axis Properties', Coord_Object=coords)
   image3 -> Add, axisObj

    ; Create an imbedded control panel widget.
   base3cpbase= Obj_New('BASEWIDGET', base3, Name='True-Color Image ControlPanel')
   image3 -> ControlPanel, base3cpbase, Event_Object=image3

  ; Fourth TAB Window to demonstate 2D image with control panel.

   ; Set up coordinates so that image preserves its aspect ratio in
   ; the window and simulates map coordinate space.
   pos = [0.1, 0.1, 0.9, 0.9]
   coords = Obj_New('CATCOORD', Name='2D Image Coordinates', XRange=[-180, 180], YRange=[-90, 90])
   colors = Obj_New('COLORTOOL', NAME='Earth Colors', 4)

   ; Create the image object.
   image4 = Obj_New('CATIMAGE', Congrid(Loaddata(7), 360, 180), Coord_Object=coords, $
      Position=pos, Name='Moveable Graphics', /Keep_Aspect, Color_Object=colors, /Selectable)

   ; Create image axes in the window.
   axisObj = Obj_New('IMGAXES', XRange=[-180, 180], YRange=[-90, 90], Color='navy', $
      Name='2D Image Axes', Description='Axis Properties', Coord_Object=coords)
   image4 -> Add, axisObj

   ; Add the image to the draw widget object, so it will be drawn in the window.
   draw4 -> Add, image4

   ; Get the color object from the image.
   image4 -> GetProperty, COLOR_OBJECT=i4colors

   ; Create a colorbar for this image
   cb = Obj_New('CatColorbar', Color_Object=i4colors, NAME='COLORBAR', $
      Color='navy', Position=[0.1, 0.80, 0.9, 0.84])
   i4colors -> RegisterForMessage, draw4, 'COLORTOOL_TABLECHANGE'
   draw4 -> Add, cb

   ; Create a text object for this image.
   text = Obj_New('TextLine', 'Moveable DIRECT graphics. Just CLICK and DRAG!!', $
      Name='MOVEABLE TEXT', X=0.5, Y=0.24, Color='DARK ORCHID')
   draw4 -> Add, text

   ; Create a text object for this image.
   text = Obj_New('TextLine', 'Easy to code with current IDL knowledge.', $
      Name='EASY TO CODE', X=0.5, Y=0.20, Color='RED')
   draw4 -> Add, text

   text = Obj_New('TextLine', 'Right click in object to access Control Panel.', $
      Name='EASY TO CODE', X=0.5, Y=0.16, Color='Dodger Blue')
   draw4 -> Add, text

  ; Fifth TAB Window to demonstate zoom and pan capabilities off CATIMAGE.

   ; Create a medical image object.
   filename = Filepath(Subdir=['examples', 'data'], 'mr_knee.dcm')
   knee = Read_Dicom(filename)
   kneescl = BytScl(knee)
   knee_coord = Obj_New('CATCOORD', Position=[0,0,1,1], XRange=[0,5], YRange=[0,10.])
   kneeImage = Obj_New('CatImage', knee, Order=1, Name='KNEEIMAGE', Coord_Object=knee_coord)
   draw5 -> Add, kneeImage
   kneeImage -> GetProperty, Color_Object=colors

   ; Make sure the draw widget has the same coordinate object as the image.
   draw5 -> SetProperty, Coord_Object=knee_coord

   ; Add a ruler to the image to distinguish size of features.
   ruler = Obj_New('RULER', X=0.5, Y=0.1, Length=1.0, Title='CM', Color='cyan', Parent_Coord=knee_coord, $
      Name='Ruler', Selectable=1)
   kneeImage -> Add, ruler


   ; Create a tool bar for interacting with the image.
   base = Obj_New('BASEWIDGET', base5, Column=1, /Base_Align_Center)
   toolbar = Obj_New('BASEWIDGET', base, Column=1, /Exclusive, Name='TOOLBAR')

   button = Obj_New('BUTTONWIDGET', toolbar, $
                     VALUE=Filepath('arrow.bmp', Subdir=['resource','bitmaps']), $
                     /BitMap, Tooltip='Zoom and Pan', $
                     Helpline ='Left Button to ZOOM IN, Right Button to ZOOM OUT, and Middle Button to PAN', $
                     NAME='ARROW', /No_Release)
   button -> SetProperty, Set_Button=1
   button = Obj_New('BUTTONWIDGET', toolbar, $
                     VALUE=Filepath('rectangl.bmp', Subdir=['resource','bitmaps']), $
                     /BitMap, Tooltip='Draw Rectangle ROI', $
                     Helpline ='Draw Rectangle ROI on Image. Click inside to drag.', $
                     NAME='RECTANGLE', /No_Release)
   button = Obj_New('BUTTONWIDGET', toolbar, $
                     VALUE=Filepath('ellipse.bmp', Subdir=['resource','bitmaps']), $
                     /BitMap, Tooltip='Draw Ellipse', $
                     Helpline ='Draw Ellipse on Image. Click inside to drag.', $
                     NAME='ELLIPSE', /No_Release)
   button = Obj_New('BUTTONWIDGET', toolbar, $
                     VALUE=Filepath('freepoly.bmp', Subdir=['resource','bitmaps']), $
                     /BitMap, Tooltip='Draw Freehand ROI', $
                     Helpline ='Draw Freehand ROI on Image. Click inside to drag.', $
                     NAME='FREEHAND', /No_Release)
   button = Obj_New('BUTTONWIDGET', toolbar, $
                     VALUE=Filepath('segpoly.bmp', Subdir=['resource','bitmaps']), $
                     /BitMap, Tooltip='Draw Polygon ROI', $
                     Helpline ='Draw Polygon ROI on image. Click inside to drag.', $
                     NAME='POLYGON', /No_Release)

   resourcefile = Filepath('cat_linemeasure.bmp', Root_Dir=ProgramRootDir(), Subdir=['resources'])
   file = File_Search(resourcefile, Count=count)
   IF count EQ 0 THEN BEGIN
      resourcefile = Filepath('cat_linemeasure.bmp', Subdir=['resource'])
      file = File_Search(resourcefile, Count=count)
   ENDIF
   IF count EQ 0 THEN BEGIN
      resourcefile =    [       $
     [000B, 000B],      $
     [000B, 112B],      $
     [000B, 080B],      $
     [000B, 112B],      $
     [000B, 008B],      $
     [000B, 004B],      $
     [000B, 002B],      $
     [000B, 001B],      $
     [128B, 000B],      $
     [064B, 000B],      $
     [032B, 000B],      $
     [016B, 000B],      $
     [014B, 000B],      $
     [010B, 000B],      $
     [014B, 000B],      $
     [000B, 000B]      $
     ]
   ENDIF
   button = Obj_New('BUTTONWIDGET', toolbar, $
                     VALUE=resourcefile, $
                     /BitMap, Tooltip='Line Measurement Tool', $
                     Helpline ='Click and measure distance to another point.', $
                     NAME='MEASURETOOL', No_Release=1)


   resourcefile = Filepath('cat_angle.bmp', Root_Dir=ProgramRootDir(), Subdir=['resources'])
   file = File_Search(resourcefile, Count=count)
   IF count EQ 0 THEN BEGIN
      resourcefile = Filepath('cat_angle.bmp', Subdir=['resource'])
      file = File_Search(resourcefile, Count=count)
   ENDIF
   IF count EQ 0 THEN BEGIN
      resourcefile = [   $
     [000B, 000B],      $
     [004B, 000B],      $
     [014B, 000B],      $
     [031B, 000B],      $
     [004B, 000B],      $
     [004B, 000B],      $
     [004B, 000B],      $
     [004B, 000B],      $
     [028B, 000B],      $
     [036B, 000B],      $
     [068B, 000B],      $
     [132B, 016B],      $
     [132B, 048B],      $
     [252B, 127B],      $
     [000B, 048B],      $
     [000B, 016B]      $
     ]
   ENDIF
   button = Obj_New('BUTTONWIDGET', toolbar, $
                     VALUE=resourcefile, $
                     /BitMap, Tooltip='Angle Measurement Tool', $
                     Helpline ='Click and measure angle to second point.', $
                     NAME='ANGLETOOL', No_Release=1)

   colorbar = Obj_New('CT_STRETCH', base, YSize=400, Range=[Min(knee), Max(knee)], Colortool=colors, $
      Format='(I4)', Name='Medical Colortable')
   colors -> RegisterForMessage, kneeImage, 'COLORTOOL_SETPROPERTY'

   ; Sixth TAB window will be a demonstation of annotation.

   ; Open an image file.
   toolbar = OBJ_NEW('BaseWidget', base6, Row=1, Base_Align_Top=1)
   filename = Filepath(SubDir=['examples', 'data'], 'rbcells.jpg')
   file = File_Search(filename, Count=count)
   IF count GT 0 THEN Read_JPEG, filename, image ELSE image = Dist(256)
   pos = [0.15, 0.15, 0.85, 0.85]
   coords = Obj_New('CatCoord', XRange=[0,5], YRange=[0,6])
   draw6image = OBJ_NEW('CatImage', image, Position=pos, /Keep_Aspect, Coord_Object=coords)
   axes = Obj_New('ImgAxes', Coord_Object=coords, XTickformat='(F4.1)', YTickformat='(F4.1)', $
      XRange=[0,5], YRange=[0,6], Color='Navy')
   draw6image -> Add, axes
   coords -> SetProperty, Position=pos
   draw6 -> Add, draw6image

   ; Create the statusbar.
   self -> CreateStatusBar

   ; Create an annotation interaction.
   self.annotateLayer = Obj_New('CatLayer', Name='AnnotationLayer')
   self.annotate = Obj_New('AnnotateInteraction', draw6, Layer=self.annotateLayer, /Start_Now, $
      Coord_Object=coords, Statusbar=self._statusbar, Color='navy')

   ; Add some text to the annotation layer.
   text = Obj_New('TextLine', 'Annotation Example: Drag Me Around!!', X=0.35, Y=0.85, Color='NAVY', $
      Font=1, Name='Initial_Text')
   self.annotateLayer -> Add, text

   ; Add the interaction control panel to the toolbar.
   self.annotate -> ControlPanel, toolbar, Column=1

   ; Display the entire application in the window.
   self -> Draw;, /Center

END
;*****************************************************************************************************



PRO Catalyst::Kill_Notify

; The purpose of this method is to call the utility routine CatDestroyDefaults.
; This will completely clean up any objects that were used to set up the
; Catalyst system as well as "system" variables. This method is called when
; the application is destroyed by the user.

   CatDestroyDefaults

END
;*****************************************************************************************************



PRO Catalyst::CLEANUP

   Obj_Destroy, self.currentInteraction
   Obj_Destroy, self.annotate
   Obj_Destroy, self.annotateLayer
   self -> TOPLEVELBASE::Cleanup

END



FUNCTION Catalyst::INIT, _Ref_Extra=extra

; This INIT method simply instantiates a top-level base object with a status bar.

   ok = self->TOPLEVELBASE::INIT(_Extra=extra)
   IF ~ok THEN RETURN, 0

RETURN, 1
END
;*****************************************************************************************************


PRO Catalyst__Define

; The Catalyst class definition. It is a top-level base object widget.

   class = {Catalyst, INHERITS TopLevelBase, $
      _statusbar:Obj_New(), $
      currentInteraction: Obj_New(), $     ; The current interaction for the medical image.
      draw5ID: Obj_New(), $                ; The draw widget containing the medical image.
      annotate: Obj_New(), $               ; An annotation interaction.
      annotateLayer: Obj_New(), $          ; The annotation layer object.
      saveAS: Obj_New(), $                 ; The SAVE AS button.
      pan_x: 0L, $                         ; The last pan location in X.
      pan_y: 0L}                           ; The last pan location in Y.
END
;*****************************************************************************************************


PRO Catalyst, tlb

   @cat_pro_error_handler

   ; Check to see if the system is set up in 24-bit color.
   Device, Get_Visual_Name=visualName, Get_Visual_Depth=theDepth
   CASE StrUpCase(visualName) OF
      'TRUECOLOR': BEGIN
      END
      ELSE: BEGIN
      text = [ '  Catalyst requires a 24-bit TrueColor visual class. You are running a', $
               ' ', $
               '       ' + StrTrim(theDepth,2) + '-bit ' + StrTrim(visualName,2), $
               '', $
               '  visual class. Please see your on-line IDL documentation for how to ', $
               '  configure IDL to run in the proper visual class. Typically, this ', $
               '  selection occurs in an IDL startup file with a command similar to this:', $
               '', $
               '       ' + 'DEVICE, TRUE_COLOR=24', $
               '']

     IF StrUpCase(visualName) EQ 'DIRECTCOLOR' THEN text = [text, $
               '  NOTE: The DirectColor visual class is NOT recommended for *any* IDL application! ', $
               '  You will almost certainly want to learn how to change this. You can find additional', $
               '  information here:', $
               '', $
               '       http://www.dfanning.com/misc_tips/idlsetup.html', $
               '']

      xDisplayFile, 'dummy', Text=text, /Modal, Title='Problem with the IDL Visual Class and Catalyst Program', $
         Height=N_Elements(text), Done_Button='Dismiss'

      RETURN

      END
   ENDCASE

   ; Create a default data directory, and restore all system variables
   ; to their default values.
   CatSetDefault, 'DATADIR', FILEPATH ('', SubDir=['examples','data'])

   ; Create the widgets that make up the application. Run it.
   tlb = OBJ_NEW('Catalyst', Column=1, MBar=menubar, /Kill_Notify, $
      Title='Catalyst Test Application: Everything in this window is an OBJECT and part of' + $
             ' the Catalyst Library!', Name='Catalyst Test Application', xoffset=50, yoffset=200)
   tlb -> GUI, menubar

END
;*****************************************************************************************************
