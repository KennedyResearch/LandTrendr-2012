;*****************************************************************************************************
;+
; NAME:
;       CW_HISTOSTRETCH__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a histogram stretch interactive
;       widget for images.
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
;       tlb = Obj_New("TOPLEVELBASE")
;       stretch = Obj_New('CW_HISTOSTRETCH', tlb, imageObject)
;
; SUPERCLASSES:
;
;       BASEWIDGET
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { CW_HISTOSTRETCH, $
;             image: Obj_New(), $             ; The image object we are interacting with.
;             drawID: Obj_New(), $            ; The draw widget object.
;             pixmapID: Obj_New(), $          ; The pixmap widget object.
;             minThresh: 0.0, $               ; The minimum threshold.
;             maxThresh: 0.0, $               ; The maximum threshold.
;             cenThresh: 0.0, $               ; The center of the threshold range.
;             minTextID: Obj_New(), $         ; The minimum Text Widget.
;             maxTextID: Obj_New(), $         ; The maximum Text Widget.
;             cenTextID: Obj_New(), $         ; The center Text Widget.
;             format: "", $                   ; The string text format.
;             colors: Obj_New(), $            ; A colortool object.
;             coords: Obj_New(), $            ; A coordinates object.
;             xdata: Ptr_New(), $             ; The X data for the histogram plot.
;             histoplot: Ptr_New(), $         ; The histogram plot data itself.
;             closeto: 0.0, $                 ; A delta for establishing if you are "close" to a line.
;             lineToMove: "", $               ; A string identifying which line we should move. MIN, MAX, CENTER.
;             range: FltArr(2), $             ; The range of the histogram plot.
;             INHERITS BASEWIDGET $           ; Inherits base widget capability.
;           }
;
; MESSAGES:
;
;       A message with a title of 'HISTOSTRETCH_CHANGE is sent whenever there is a change in
;       a histogram threshold line. The image object passed into the INIT method is automatically
;       registered to receive these messages. The DATA passed with the message contains the
;       minumum and maximum threshold values, like this:
;
;         DATA={maxThresh:self.maxThresh, minThresh:self.minThresh}
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, July 17, 2003.
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
;       CW_HISTOSTRETCH::DRAW
;
; PURPOSE:
;
;       This method draws the image histogram and image threshold lines in the draw widget.
;
; SYNTAX:
;
;       histoStretch -> Draw
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     _EXTRA:  Any keywords appropriate for the superclass Add method.
;-
;*****************************************************************************************************
PRO CW_HistoStretch::Draw, _Extra=extraKeywords

   @cat_pro_error_handler

   ; Draw the plot and the threshold lines.
   self -> DrawPlot
   self -> DrawLines

   ; Call the superclass DRAW method.
   self -> BASEWIDGET::Draw, _Extra=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CW_HISTOSTRETCH::DRAWLINES
;
; PURPOSE:
;
;       This method draws the image threshold lines on the histogram plot. It also
;       updates the threshold value widgets in the interface.
;
; SYNTAX:
;
;       histoStretch -> DrawLines
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO CW_HistoStretch::DrawLines

   @cat_pro_error_handler

   ; Make the draw widget current and set up the drawing colors and coordinate system.
   self.drawID -> SetWindow
   self.colors -> Draw
   self.coords -> Draw

   ; This plot requires color decomposition to be off. Save the current decomposition state.
   Device, Decomposed=0, Get_Decomposed=theState

   ; Draw the threshold lines.

   PlotS, [!X.CRange[0], !X.CRange[1]], [self.minThresh, self.minThresh], $
      Color=252, Thick=3
   cmin = Convert_Coord(0, self.minThresh,  /Data, /To_Normal)
   PolyFill, [0.1, 0.15, 0.1], [cmin[1]-0.025, cmin[1], cmin[1]+0.025], /Normal, Fill=1, Color=252
   ;XYOuts, 0.65, cmin[1], /Normal, StrTrim(Fix(self.minThresh),2), Color=252, Alignment=0.0, Font=0

   PlotS, [!X.CRange[0], !X.CRange[1]], [self.maxThresh, self.maxThresh], $
      Color=251, Thick=3
   cmax = Convert_Coord(0, self.maxThresh, /Data, /To_Normal)
   PolyFill, [0.1, 0.15, 0.1], [cmax[1]-0.025, cmax[1], cmax[1]+0.025], /Normal, Fill=1, Color=251
   ;XYOuts, 0.65, cmax[1], /Normal, StrTrim(Fix(self.maxThresh),2), Color=251, Alignment=0.0, Font=0

   PlotS, [!X.CRange[0], !X.CRange[1]], [self.cenThresh, self.cenThresh], $
      Color=250, Thick=3
   cen = Convert_Coord(0, self.cenThresh, /Data, /To_Normal)
   PolyFill, [0.1, 0.15, 0.1], [cen[1]-0.025, cen[1], cen[1]+0.025], /Normal, Fill=1, Color=250
   ;XYOuts, 0.65, cen[1], /Normal, StrTrim(Fix(self.cenThresh),2), Color=250, Alignment=0.0, Font=0

   ; Set the decomposed state back the way it was.
   Device, Decomposed=theState

   ; Update all the text widgets.
   self.minTextID -> SetProperty, Value=String(self.minThresh, Format=self.format)
   self.maxTextID -> SetProperty, Value=String(self.maxThresh, Format=self.format)
   ;self.cenTextID -> SetProperty, Value=String(self.cenThresh, Format=self.format)

   ; Done
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CW_HISTOSTRETCH::DRAWPLOT
;
; PURPOSE:
;
;       This method draws the image histogram plot, both in the display window
;       and in a pixmap window for faster copying as the cursor moves the threshold
;       lines.
;
; SYNTAX:
;
;       histoStretch -> DrawPlot
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO CW_HistoStretch::DrawPlot

   @cat_pro_error_handler

   ; Set the window, drawing colors, and decomposed state. Do *not* draw the
   ; coordinates, because you want the plot to do this. The coordinates will
   ; be saved *after* the plot is drawn.
   self.pixmapID -> SetWindow
   self.colors -> Draw
   Device, Decomposed=0, Get_Decomposed=theState

   ; Draw the plot in the pixmap window.
   Plot, *self.histoplot, *self.xdata, YSTYLE=1, XSTYLE=1, XTICKFORMAT='(A1)', YTICKFORMAT='(A1)', $
      BACKGROUND=255, COLOR=253, /NODATA, POSITION=[0.15, 0.1, 0.9, 0.9]
   OPlot, *self.histoplot, *self.xdata, COLOR=254, THICK=2

   ; Save the plot coordinates.
   self.coords -> SaveCoords

   ; Copy the pixmap into the display window.
   self.drawID -> SetWindow
   self.pixmapID -> Draw

   ; Set decomposed state back to entry state.
   Device, Decomposed=theState

   ; Done.
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CW_HISTOSTRETCH::EVENTHANDLER
;
; PURPOSE:
;
;       This is the event handler method for the compound widget object.
;       All internal events are handled here.
;
; SYNTAX:
;
;       Called by the Catalyst system when an event for one of the compound
;       object's widgets is generated.
;
; ARGUMENTS:
;
;     event:    The event object from the system.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO CW_HistoStretch::EventHandler, event

   @cat_pro_error_handler

   Forward_Function scale_vector

   ; What class of object created this event.
   ; We expect events from DRAWWIDGETs and BUTTONWIDGETS.
   thisClass = Obj_Class(event.id)

   ; All draw widget events handled here.
   IF thisClass EQ 'DRAWWIDGET' THEN BEGIN

      ; Make the pixmap window the current window. And make sure coordinate system
      ; and colors are established.
      self.pixmapID -> SetWindow
      self.colors -> Draw
      self.coords -> Draw

      ; What kind of event is this?
      possibleEvents = ['DOWN', 'UP', 'MOTION', 'SCROLL', 'EXPOSE', 'CH_CHAR', 'KEY_CHAR']
      thisEvent = possibleEvents[event.type]
      CASE thisEvent OF

         'DOWN': BEGIN

               ; Determine if you are near any three of the possible lines. If you
               ; are, mark the line and turn motion events on for the draw widget.

               coords = Convert_Coord(event.x, event.y, 0, /Device, /To_Normal)
               IF (coords[0] GT 0.65) THEN RETURN
               coords = Convert_Coord(event.x, event.y, 0, /Device, /To_Data)
               self.lineToMove = ""
               IF Abs(self.cenThresh - coords[1]) LE self.closeTo THEN self.lineToMove = 'CENTER'
               IF Abs(self.minThresh - coords[1]) LE self.closeTo THEN self.lineToMove = 'MIN'
               IF Abs(self.maxThresh - coords[1]) LE self.closeTo THEN self.lineToMove = 'MAX'
               IF self.lineToMove NE "" THEN self.drawID -> SetProperty, Motion_Events=1

            ENDCASE ; DOWN Case

         'UP': BEGIN

               ; Turn motion events off, and clear any queued events.
               ; Reset the close line variable.
               self.drawID -> SetProperty, Motion_Events=0
               self.drawID -> SetProperty, /Clear_Events
               self.lineToMove = ""

;               ; Send a message, if needed.
;               self -> SendMessage, 'HISTOSTRETCH_CHANGE', $
;                  DATA={maxThresh:self.maxThresh, minThresh:self.minThresh}
            ENDCASE ; UP Case

         'MOTION': BEGIN

               ; Convert device coordinates to data coordinates.
               coords = Convert_Coord(event.x, event.y, 0, /Device, /To_Data)

               ; Branch depending upon which line you are close to.
               CASE self.lineToMove OF

                  'MAX': BEGIN

                        ; The max line has to be between the min line and the far range.
                        IF (coords[1] LE self.minThresh) THEN RETURN
                        IF (coords[1] GT self.range[1]) THEN coords[1] = self.range[1]

                        ; Recalculate the max and center threshold levels.
                        self.maxThresh = coords[1]
                        self.cenThresh = ((self.maxThresh - self.minThresh) / 2.0) + self.minThresh

                     ENDCASE ; MAX case

                  'MIN': BEGIN

                        ; The min line has to be between the max line and the near range.
                        IF (coords[1] GE self.maxThresh) THEN RETURN
                        IF (coords[1] LT self.range[0]) THEN coords[1] = self.range[0]

                        ; Recalculate the min and center threshold levels.
                        self.minThresh = coords[1]
                        self.cenThresh = ((self.maxThresh - self.minThresh) / 2.0) + self.minThresh

                     ENDCASE ; MIN case

                  'CENTER': BEGIN

                        ; The center line is constrained by not wanting the max or min lines to go
                        ; out of range. Which direction are we headed.
                        diff = Abs(self.maxthresh - self.cenThresh)
                        IF (diff + coords[1]) LE self.range[1] THEN BEGIN

                           self.cenThresh = coords[1]
                           self.maxThresh = self.cenThresh + diff
                           self.minThresh = self.cenThresh - diff
                           IF self.minThresh LT self.range[0] THEN BEGIN
                              self.minThresh = self.range[0]
                              self.cenThresh = self.minThresh + diff
                              self.maxThresh = self.cenThresh + diff
                           ENDIF

                        ENDIF ELSE BEGIN
                           self.maxThresh = self.range[1]
                           self.cenThresh = self.maxThresh - diff
                           self.minThresh = self.cenThresh - diff
                        ENDELSE

                     ENDCASE ; CENTER case

                  ELSE:

               ENDCASE ; LINETOMOVE case

               ; Draw the plot.
               self.drawID -> SetWindow
               self -> Draw

               ; Send a message, if needed.
               self -> SendMessage, 'HISTOSTRETCH_MOTION', $
                  DATA={maxThresh:self.maxThresh, minThresh:self.minThresh}
;      self -> SendMessage, 'HISTOSTRETCH_CHANGE', $
;         DATA={maxThresh:self.maxThresh, minThresh:self.minThresh}

            ENDCASE ; MOTION case

         ELSE:

      ENDCASE ; Which EVENT case

   ENDIF ; Draw Widget Events

   ; All text widget events handled here.

   IF thisClass EQ 'TEXTWIDGET' THEN BEGIN

      ; Which button caused the event? What is the button value?
      event.id -> GetProperty, Name=eventName, Value=theValue
      theValue = Float(theValue[0])

      CASE eventName OF

         'MINTHRESH': BEGIN

                 ; The min line has to be between the max line and the near range.
                 IF (theValue LT self.range[0]) THEN theValue = self.range[0]
                 IF (theValue GT self.maxThresh) THEN theValue = self.maxThresh

                 ; Recalculate the min and center threshold levels.
                 self.minThresh = theValue
                 self.cenThresh = ((self.maxThresh - self.minThresh) / 2.0) + self.minThresh

              ENDCASE ; of MINTHRESH

         'MAXTHRESH': BEGIN

                 ; The max line has to be between the min line and the far range.
                 IF (theValue GT self.range[1]) THEN theValue = self.range[1]
                 IF (theValue LT self.minThresh) THEN theValue = self.minThresh

                 ; Recalculate the min and center threshold levels.
                 self.maxThresh = theValue
                 self.cenThresh = ((self.maxThresh - self.minThresh) / 2.0) + self.minThresh

              ENDCASE ; of MINTHRESH


          'CENTHRESH': BEGIN

                 ; The center line is constrained by not wanting the max or min lines to go
                 ; out of range. Which direction are we headed.
;                 diff = Abs(self.maxthresh - self.cenThresh)
;                 IF (diff + theValue) LE self.range[1] THEN BEGIN
;
;                    self.cenThresh = theValue
;                    self.maxThresh = self.cenThresh + diff
;                    self.minThresh = self.cenThresh - diff
;                    IF self.minThresh LT self.range[0] THEN BEGIN
;                       self.minThresh = self.range[0]
;                       self.cenThresh = self.minThresh + diff
;                       self.maxThresh = self.cenThresh + diff
;                    ENDIF
;
;                 ENDIF ELSE BEGIN
;
;                    self.maxThresh = self.range[1]
;                    self.cenThresh = self.maxThresh - diff
;                    self.minThresh = self.cenThresh - diff
;
;                 ENDELSE

              ENDCASE ; of CENNTHRESH

      ENDCASE ; of eventName case

      ; Draw the plot and the lines.
      self -> Draw

      ; Send a message, if needed.
      self -> SendMessage, 'HISTOSTRETCH_CHANGE', $
         DATA={maxThresh:self.maxThresh, minThresh:self.minThresh}

   ENDIF ; Text Widget Events

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CW_HISTOSTRETCH::GETPROPERTY
;
; PURPOSE:
;
;       This method enables the getting of the CW_HISTOSTRETCH properties.
;
; SYNTAX:
;
;       histoStretch -> GetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     _REF_EXTRA:    Any keywords appropriate for the superclass GetProperty" method.
;-
;*****************************************************************************************************
PRO CW_HistoStretch::GetProperty,    $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> BASEWIDGET::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CW_HISTOSTRETCH::MESSAGEHANDLER
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
PRO CW_HistoStretch::MessageHandler, title, SENDER=sender, DATA=data

   ; Initialise the error handler
   @cat_pro_error_handler

   CASE title OF

      'CATIMAGE_NEWIMAGE': self -> Reset

      ELSE: self -> CATDATAATOM::MessageHandler, title, SENDER=sender, DATA=data


   ENDCASE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CW_HISTOSTRETCH::NOTIFY_REALIZE
;
; PURPOSE:
;
;       When the compound object widget is realized, it should draw the histogram plot in its
;       display window.
;
; SYNTAX:
;
;       self -> Notify_Realize, object
;
; ARGUMENTS:
;
;       object:    The object that was realized (often the self object).
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO CW_HistoStretch::Notify_Realize, theObject

   @cat_pro_error_handler

   ; Draw the histogram plot in the display window.

   self -> DrawPlot
   self -> DrawLines

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CW_HISTOSTRETCH::RESET
;
; PURPOSE:
;
;       This method resets the histogram GUI, based on the display image current in the image object.
;
; SYNTAX:
;
;       histoStretch -> DrawPlot
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
PRO CW_HistoStretch::Reset

   @cat_pro_error_handler

   ; Get the data range of the diplay image.
   self.image -> GetProperty, Display_Range=range

   ;Set the threshold values based on the data range.
   self.minThresh = range[0]
   self.maxThresh = range[1]
   self.cenThresh = (self.maxThresh - self.minThresh) / 2.0 + self.minThresh

   ; Create a delta for being "close" to a line
   self.closeto = (range[1] - range[0]) * 0.05
   self.lineToMove = ''
   self.range = range

   ; Create a new histoplot.
   self.histoplot = Ptr_New(Histogram(self.image -> GetData(), Min=range[0], Max=range[1], NBINS=nbins))
   self.xdata = Ptr_New(Findgen(N_Elements(*self.histoplot)))
   *self.xdata = Scale_Vector(*self.xdata, range[0], range[1])

   ; Draw the histogram plot in the display window.

   self -> DrawPlot
   self -> DrawLines

   ; Send a message, if needed.
   self -> SendMessage, 'HISTOSTRETCH_CHANGE', $
      DATA={maxThresh:self.maxThresh, minThresh:self.minThresh}

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CW_HISTOSTRETCH::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the CW_HISTOSTRETCH properties.
;
; SYNTAX:
;
;       histoStretch -> SetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     C_AXES:        The name of the axes color for the histogram plot. "Wheat" by default.
;
;     C_BG:          The name of the background color for the histogram plot. "Charcoal" by default.
;
;     C_CENTHRESH:   The name of the center theshold color for the histogram plot. "Sky Blue" by default.
;
;     C_DATA:        The name of the data color for the histogram plot. "White" by default.
;
;     C_MAXTHRESH:   The name of the maximum theshold color for the histogram plot. "Green" by default.
;
;     C_MINTHRESH:   The name of the minimum theshold color for the histogram plot. "Yellow" by default.
;
;     XSIZE:         The X size of the histostretch draw widget.
;
;     YSIZE:         The Y size of the histostretch draw widget.
;
;     _EXTRA:        Any keywords appropriate for the superclass SetProperty" method.
;-
;*****************************************************************************************************
PRO CW_HistoStretch::SetProperty,    $
   C_AXES=c_axes, $
   C_BG=c_bg, $
   C_CENTHRESH=c_centhresh, $
   C_DATA=c_data, $
   C_MAXTHRESH=c_maxthresh, $
   C_MINTHRESH=c_minthresh, $
   XSIZE=xsize, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(c_axes) NE 0 THEN self.colors -> LoadColor, c_axes, INDEX=253
   IF N_Elements(c_bg) NE 0 THEN self.colors -> LoadColor, c_bg, INDEX=255
   IF N_Elements(c_data) NE 0 THEN self.colors -> LoadColor, c_data, INDEX=254
   IF N_Elements(c_minthresh) NE 0 THEN self.colors -> LoadColor, c_minthresh, INDEX=252
   IF N_Elements(c_maxthresh) NE 0 THEN self.colors -> LoadColor, c_maxthresh, INDEX=251
   IF N_Elements(c_centhresh) NE 0 THEN self.colors -> LoadColor, c_centhresh, INDEX=250
   IF (N_Elements(xsize) NE 0) OR (N_Elements(ysize) NE 0) THEN self.drawID -> GetProperty, XSize=xs, YSize=ys
   IF (N_Elements(xsize) NE 0) THEN self.drawID -> SetProperty, XSize=xsize, YSize=ys
   IF (N_Elements(ysize) NE 0) THEN self.drawID -> SetProperty, YSize=xs, YSize=ysize

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> BASEWIDGET::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CW_HISTOSTRETCH::CLEANUP
;
; PURPOSE:
;
;       This is the CW_HISTOSTRETCH object class destructor method.
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
PRO CW_HistoStretch::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self.drawID
   Obj_Destroy, self.pixmapID
   Obj_Destroy, self.colors
   Obj_Destroy, self.coords
   Ptr_Free, self.xdata
   Ptr_Free, self.histoplot

   self -> BASEWIDGET::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CW_HISTOSTRETCH::INIT
;
; PURPOSE:
;
;       This is the CW_HISTOSTRETCH object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     parent:        The parent object widget for this compound widget. A BASEWIDGET.
;
;     image:         The image object that will be controlled by this compound widget object.
;                    Data for the histogram will be obtained via the image object's GETDATA
;                    method. The MESSAGEHANDLER method of th image object will be called when
;                    the user causes the histogram threshold values to change.
;
; KEYWORDS:
;
;     C_AXES:        The name of the axes color for the histogram plot. "Wheat" by default.
;
;     C_BG:          The name of the background color for the histogram plot. "Charcoal" by default.
;
;     C_CENTHRESH:   The name of the center theshold color for the histogram plot. "Sky Blue" by default.
;
;     C_DATA:        The name of the data color for the histogram plot. "White" by default.
;
;     C_MAXTHRESH:   The name of the maximum theshold color for the histogram plot. "Green" by default.
;
;     C_MINTHRESH:   The name of the minimum theshold color for the histogram plot. "Yellow" by default.
;
;     FORMAT:        A format specification for formatting the histogram annotations.
;
;     FRAME:         Set this keyword to put a frame around the compound widget.
;
;     MAXTHRESH:     Set this keyword to the maximum threshold value. Set to the maximum
;                    value of the image data by default.
;
;     MINTHRESH:     Set this keyword to the minimum threshold value. Set to the minimum
;                    value of the image data by default.
;
;     NBINS:         The number of bins in the histogram. Set to 100 by default.
;
;     _EXTRA:        Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION CW_HistoStretch::INIT, parent, image, $
   C_AXES=c_axes, $
   C_BG=c_bg, $
   C_CENTHRESH=c_centhresh, $
   C_DATA=c_data, $
   C_MAXTHRESH=c_maxthresh, $
   C_MINTHRESH=c_minthresh, $
   FORMAT=format, $
   FRAME=frame, $
   MAXTHRESH=maxThresh, $
   MINTHESH=minThresh, $
   NBINS=nbins, $
   _EXTRA=extraKeywords

   @cat_func_error_handler

   forward_function scale_vector

   IF N_Elements(parent) EQ 0 THEN Message, 'A parent object is required for CW_HISTOSTRETCH.'
   IF N_Elements(image) EQ 0 THEN  Message, 'An image object is required for CW_HISTOSTRETCH.'
   IF N_Elements(nbins) EQ 0 THEN nbins = 101
   IF N_Elements(c_axes) EQ 0 THEN c_axes = 'Wheat'
   IF N_Elements(c_bg) EQ 0 THEN c_bg = 'Charcoal'
   IF N_Elements(c_centhresh) EQ 0 THEN c_centhresh = 'Sky Blue'
   IF N_Elements(c_data) EQ 0 THEN c_data = 'White'
   IF N_Elements(c_maxthresh) EQ 0 THEN c_maxthresh = 'Green'
   IF N_Elements(c_minthresh) EQ 0 THEN c_minthresh = 'Yellow'

   ; Set the threshold values if they are not defined.
   image -> GetProperty, Display_Range=range
   IF N_Elements(minThresh) EQ 0 THEN minThresh = range[0]
   IF N_Elements(maxThresh) EQ 0 THEN maxThresh = range[1]
   self.minThresh = minThresh
   self.maxThresh = maxThresh
   self.cenThresh = (maxThresh - minThresh) / 2.0 + minThresh
   IF N_Elements(format) EQ 0 THEN format = '(F8.2)'
   self.format = format

   ; Create color palette for plotting.
   self.colors = Obj_New('COLORTOOL')
   self.colors -> LoadColor, c_bg, INDEX=255
   self.colors -> LoadColor, c_data, INDEX=254
   self.colors -> LoadColor, c_axes, INDEX=253
   self.colors -> LoadColor, c_minthresh, INDEX=252
   self.colors -> LoadColor, c_maxthresh, INDEX=251
   self.colors -> LoadColor, c_centhresh, INDEX=250


   ; Create a coordinates object for keeping track of coordinates.
   self.coords = Obj_New('PLOTCOORD')

   ; Create a delta for being "close" to a line
   self.closeto = (range[1] - range[0]) * 0.05
   self.lineToMove = ''
   self.range = range
   self.image = image

   ; Initialize yourself as a BASEWIDGET.
   ok = self -> BASEWIDGET::INIT(parent, _Extra=extraKeywords, Column=1, Frame=frame, /NOTIFY_REALIZE)
   IF NOT ok THEN RETURN, 0

   draw_xsize = 150
   self.drawID = Obj_New('DRAWWIDGET', self, XSIZE=draw_xsize, YSIZE=250, Button_Events=1, Name='DRAWWIDGET')

   ; Create the rest of the widgets.
   maxrowID = Obj_New('BASEWIDGET', self, Row=1)
      void = Obj_New('LABELWIDGET', maxrowID, Value='Max: ', SCR_XSIZE=draw_xsize*0.30)
      self.maxtextID = Obj_New('TEXTWIDGET', maxrowID, Value=String(maxThresh, Format=self.format), $
         SCR_XSIZE=draw_xsize*0.65, /Editable , NAME='MAXTHRESH')
;    cenrowID = Obj_New('BASEWIDGET', self, Row=1)
;      void = Obj_New('LABELWIDGET', cenrowID, Value='Center: ', SCR_XSIZE=draw_xsize*0.30)
;      self.centextID = Obj_New('TEXTWIDGET', cenrowID, Value=String((maxThresh-minThresh)/2.0, Format=self.format), $
;         SCR_XSIZE=draw_xsize*0.65, /Editable, NAME='CENTHRESH' )
   minrowID = Obj_New('BASEWIDGET', self, Row=1)
      void = Obj_New('LABELWIDGET', minrowID, Value='Min: ', SCR_XSIZE=draw_xsize*0.30)
      self.mintextID = Obj_New('TEXTWIDGET', minrowID, Value=String(minThresh, Format=self.format) , $
         SCR_XSIZE=draw_xsize*0.65, /Editable, Name='MINTHRESH')

   ; Create a pixmap widget for fast redraw of the histogram plot.
   self.pixmapID = Obj_New('PIXMAPWIDGET', XSIZE=draw_xsize, YSIZE=250, Name='PIXMAP', Retain=1)
   self.histoplot = Ptr_New(Histogram(image -> GetData(), Min=range[0], Max=range[1], NBINS=nbins))
   self.xdata = Ptr_New(Findgen(N_Elements(*self.histoplot)))
   *self.xdata = Scale_Vector(*self.xdata, range[0], range[1])

   ; Register the image for messages.
   self -> RegisterForMessage, image, 'HISTOSTRETCH_CHANGE'
   self -> RegisterForMessage, image, 'HISTOSTRETCH_MOTION'

   ; Done.
   self -> Report, /Completed

   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       CW_HISTOSTRETCH DEFINITION
;
; PURPOSE:
;
;       This is the object class definition for the CW_HISTOSTRETCH object.
;
;*****************************************************************************************************
PRO CW_HistoStretch__DEFINE, class

   class = { CW_HISTOSTRETCH, $
             image: Obj_New(), $             ; The image object we are interacting with.
             drawID: Obj_New(), $            ; The draw widget object.
             pixmapID: Obj_New(), $          ; The pixmap widget object.
             minThresh: 0.0, $               ; The minimum threshold.
             maxThresh: 0.0, $               ; The maximum threshold.
             cenThresh: 0.0, $               ; The center of the threshold range.
             minTextID: Obj_New(), $         ; The minimum Text Widget.
             maxTextID: Obj_New(), $         ; The maximum Text Widget.
             cenTextID: Obj_New(), $         ; The center Text Widget.
             format: "", $                   ; The string text format.
             colors: Obj_New(), $            ; A colortool object.
             coords: Obj_New(), $            ; A coordinates object.
             xdata: Ptr_New(), $             ; The X data for the histogram plot.
             histoplot: Ptr_New(), $         ; The histogram plot data itself.
             closeto: 0.0, $                 ; A delta for establishing if you are "close" to a line.
             lineToMove: "", $               ; A string identifying which line we should move. MIN, MAX, CENTER.
             range: FltArr(2), $             ; The range of the histogram plot.
             INHERITS BASEWIDGET $           ; Inherits base widget capability.
           }

END