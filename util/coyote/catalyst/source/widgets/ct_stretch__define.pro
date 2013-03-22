;*****************************************************************************************************
;+
; NAME:
;       CT_STRETCH__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a colortable stretch
;       widget for images. Essentially, it allows the use to window/level
;       an image with respect to its display colors. No image data is changed.
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
;       stretch = Obj_New('CT_STRETCH', tlb, Range=[0, 255])
;       tlb -> Draw
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
;   class = { CT_STRETCH, $
;             r: BytArr(256), $               ; The original red color vector.
;             g: BytArr(256), $               ; The original green color vector.
;             b: BytArr(256), $               ; The original blue color vector.
;             c_bg: "", $                     ; The name of the background color.
;             c_border: "", $                 ; The name of the border color.
;             c_centhresh: "", $              ; The name of the center threshold color.
;             c_maxthresh: "", $              ; The name of the maximum threshold color.
;             c_minthresh: "", $              ; The name of the minimum threshold color.
;             cenThresh: 0.0, $               ; The center of the threshold range.
;             closeto: 0.0, $                 ; A delta for establishing if you are "close" to a line.
;             colors: Obj_New(), $            ; A colortool object for image colors.
;             coords: Obj_New(), $            ; A coordinate object for the colorbar.
;             drawID: Obj_New(), $            ; The draw widget object.
;             format: "", $                   ; The string text format.
;             image: Obj_New(), $             ; The internal image that is displayed.
;             lineToMove: "", $               ; A string identifying which line we should move. MIN, MAX, CENTER.
;             maxThresh: 0.0, $               ; The maximum threshold.
;             minThresh: 0.0, $               ; The minimum threshold.
;             pixmapID: Obj_New(), $          ; The pixmap widget object.
;             minTextID: Obj_New(), $         ; The minimum Text Widget.
;             maxTextID: Obj_New(), $         ; The maximum Text Widget.
;             range: FltArr(2), $             ; The range of the color table.
;             INHERITS BASEWIDGET $           ; Inherits base widget capability.
;           }
;
; MESSAGES:
;
;       A message with a title of 'CT_STRETCH_CHANGE is sent whenever there is a change in
;       a histogram threshold line. The image object passed into the INIT method is automatically
;       registered to receive these messages. The DATA passed with the message contains the
;       minumum and maximum threshold values, like this:
;
;         DATA={maxThresh:self.maxThresh, minThresh:self.minThresh}
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 20 January 2004.
;       Removed RETAIN=1 keyword on PIXMAPWIDGET to allow display on UNIX machines. 4 Jan 2008. DWF.
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
;       CT_STRETCH::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the CT_STRETCH object. A
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
PRO CT_Stretch::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.
   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='CT_Stretch Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)
   self -> SetProperty, Description='CT_Stretch Properties'

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.
   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, $
      Name='CT_STRETCH PROPERTYSHEET', YSize=5, Description='CT_Stretch Properties');, $
      ;Event_Object=self, Event_Method='ControlPanel_Events')
   aproperties -> SetProperty, Event_Object=self, Event_Method='ControlPanel_Events'

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END

;*****************************************************************************************************
;+
; NAME:
;       CT_STRETCH::CONTROLPANEL_EVENTS
;
; PURPOSE:
;
;       This method handles events from the control panel.
;
; SYNTAX:
;
;       Called automatically when an event is generated.
;
; ARGUMENTS:
;
;     event:        The event structure.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO CT_Stretch::ControlPanel_Events, event

   @cat_pro_error_handler

   ; Only handle change events.
   IF event.type NE 0 THEN RETURN

   CASE StrUpCase(event.identifier) OF


      ELSE: BEGIN

         component = event.component
         identifier = event.identifier
         event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
         event.component -> SetPropertyByIdentifier, identifier, value

         ; Refresh the graphics hierarchy.
         self -> Draw


      ENDCASE

   ENDCASE

   self -> Report,  /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CT_STRETCH::DRAW
;
; PURPOSE:
;
;       This method draws the image histogram and image threshold lines in the draw widget.
;
; SYNTAX:
;
;       ct_Stretch -> Draw
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
PRO CT_Stretch::Draw, _Extra=extraKeywords

   @cat_pro_error_handler

   ; Set up the coordinate system.
   IF Obj_Valid(self.coords) THEN self.coords -> Draw

   ; Calculate indices for the stretched color table.
   minb = BytScl(self.minThresh, Min=self.range[0], Max=self.range[1])
   maxb = BytScl(self.maxThresh, Min=self.range[0], Max=self.range[1])
   ctindex = BytScl(((Findgen(256)-minb)/(maxb-minb) < 1.0 > 0.0), Min=0, Max=1)
   ;ctindx = BytScl(((Findgen(256)-minb)/(maxb-minb) < 1.0 > 0.0)^gamma, Min=0, Max=1)

   ; Create local color vectors using stretched indices. Load color tool object.
   red =   self.r[ctindex]
   green = self.g[ctindex]
   blue =  self.b[ctindex]
   self.colors -> SetProperty, Red=red, Green=green, Blue=blue

   ;self.pixmapID -> SetProperty, Map=1
   self.pixmapID -> Refresh
   self.pixmapID -> SetWindow

   self.image -> Draw
   ; Draw the border.
   self.image -> GetProperty, Location=location
   xs = location[0,1]
   ys = location[1,1]
   xd = location[2,1]
   yd = location[3,1]
   Plots, [xs, xd], [ys,ys], /Normal, Color=FSC_Color(self.c_border)
   Plots, [xs, xd], [yd,yd], /Normal, Color=FSC_Color(self.c_border)
   Plots, [xs, xs], [ys,yd], /Normal, Color=FSC_Color(self.c_border)
   Plots, [xd, xd], [ys,yd], /Normal, Color=FSC_Color(self.c_border)

   c = Convert_Coord(0, 5,  /Device, /To_Normal)
   yy = c[1,0]

   ; Draw the threshold lines.
   PlotS, [0, 1], [self.minThresh, self.minThresh], $
      Color=FSC_Color(self.c_minthresh), Thick=3
   cmin = Convert_Coord(0, self.minThresh,  /Data, /To_Normal)
   PolyFill, [0.1, 0.2, 0.1], [cmin[1]-yy, cmin[1], cmin[1]+yy], /Normal, Fill=1, Color=FSC_Color(self.c_minthresh)

   PlotS, [0, 1], [self.maxThresh, self.maxThresh], $
      Color=FSC_Color(self.c_maxthresh), Thick=3
   cmax = Convert_Coord(0, self.maxThresh, /Data, /To_Normal)
   PolyFill, [0.1, 0.2, 0.1], [cmax[1]-yy, cmax[1], cmax[1]+yy], /Normal, Fill=1, Color=FSC_Color(self.c_maxthresh)

   PlotS, [0, 1], [self.cenThresh, self.cenThresh], $
      Color=FSC_Color(self.c_centhresh), Thick=3
   cen = Convert_Coord(0, self.cenThresh, /Data, /To_Normal)
   PolyFill, [0.1, 0.2, 0.1], [cen[1]-yy, cen[1], cen[1]+yy], /Normal, Fill=1, Color=FSC_Color(self.c_centhresh)

   ; Update all the text widgets.
   self.minTextID -> SetProperty, Value=String(self.minThresh, Format=self.format)
   self.maxTextID -> SetProperty, Value=String(self.maxThresh, Format=self.format)

   ; Draw plot into the display window.
   self.drawID -> SetWindow
   self.pixmapID -> Draw

   ; Call the superclass DRAW method.
   self -> BASEWIDGET::Draw, _Extra=extraKeywords


   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CT_STRETCH::EVENTHANDLER
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
PRO CT_Stretch::EventHandler, event

   @cat_pro_error_handler


   ; What class of object created this event.
   ; We expect events from DRAWWIDGETs and BUTTONWIDGETS.
   thisClass = Obj_Class(event.id)

   ; All draw widget events handled here.
   IF thisClass EQ 'DRAWWIDGET' THEN BEGIN

      ; Make sure coordinate system is established.
      self.drawID -> SetWindow
      self.coords -> Draw

      ; What kind of event is this?
      possibleEvents = ['DOWN', 'UP', 'MOTION', 'SCROLL', 'EXPOSE', 'CH_CHAR', 'KEY_CHAR']
      thisEvent = possibleEvents[event.type]
      CASE thisEvent OF

         'DOWN': BEGIN

               ; Determine if you are near any three of the possible lines. If you
               ; are, mark the line and turn motion events on for the draw widget.

               coords = Convert_Coord(event.x, event.y, 0, /Device, /To_Data)
               self.lineToMove = ""
               IF Abs(self.minThresh - coords[1]) LE self.closeTo THEN self.lineToMove = 'MIN'
               IF Abs(self.maxThresh - coords[1]) LE self.closeTo THEN self.lineToMove = 'MAX'
               IF Abs(self.cenThresh - coords[1]) LE self.closeTo THEN self.lineToMove = 'CENTER'
               IF self.lineToMove NE "" THEN self.drawID -> SetProperty, Motion_Events=1

            ENDCASE ; DOWN Case

         'UP': BEGIN

               ; Turn motion events off, and clear any queued events.
               ; Reset the close line variable.
               self.drawID -> SetProperty, Motion_Events=0
               self.drawID -> SetProperty, /Clear_Events
               self.lineToMove = ""

;               ; Send a message, if needed.
;               self -> SendMessage, 'CT_STRETCH_CHANGE', $
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
               self -> Draw

               ; Send a message, if needed.
               self -> SendMessage, 'CT_STRETCH_CHANGE', $
                  DATA={maxThresh:self.maxThresh, minThresh:self.minThresh}

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


      ENDCASE ; of eventName case

      ; Draw the plots.
      self -> Draw

      ; Send a message, if needed.
      self -> SendMessage, 'CT_STRETCH_CHANGE', $
         DATA={maxThresh:self.maxThresh, minThresh:self.minThresh}

   ENDIF ; Text Widget Events

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CT_STRETCH::GETPROPERTY
;
; PURPOSE:
;
;       This method enables the getting of the CT_STRETCH properties.
;
; SYNTAX:
;
;       ct_Stretch -> GetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     C_BG:          The name of the background color for the histogram plot. "Charcoal" by default.
;
;     C_BORDER:      The name of the  color for the histogram plot. "Wheat" by default.
;
;     C_CENTHRESH:   The name of the center theshold color for the histogram plot. "Sky Blue" by default.
;
;     C_MAXTHRESH:   The name of the maximum theshold color for the histogram plot. "Green" by default.
;
;     C_MINTHRESH:   The name of the minimum theshold color for the histogram plot. "Yellow" by default.
;
;     COLORTABLE:    A (standard) color table index number.
;
;     COLORTOOL:     A ColorTool object.
;
;     FORMAT:        A format specification for formatting the MIN/MAX annotations.
;
;     MAXTHRESH:     Set this keyword to the maximum threshold value.
;
;     MINTHRESH:     Set this keyword to the minimum threshold value.
;
;     RANGE:         The data range of the color table.
;
;     XSIZE:         The X size of the histostretch draw widget.
;
;     YSIZE:         The Y size of the histostretch draw widget.
;
;     _REF_EXTRA:    Any keywords appropriate for the superclass GetProperty" method.
;-
;*****************************************************************************************************
PRO CT_Stretch::GetProperty,    $
   C_BORDER=c_border, $
   C_BG=c_bg, $
   C_CENTHRESH=c_centhresh, $
   C_MAXTHRESH=c_maxthresh, $
   C_MINTHRESH=c_minthresh, $
   COLORTABLE=colortable, $
   COLORTOOL=colortool, $
   FORMAT=format, $
   MAXTHRESH=maxthresh, $
   MINTHRESH=minthresh, $
   RANGE=range, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> BASEWIDGET::GetProperty, _EXTRA=extraKeywords

   IF Arg_Present(c_border) THEN c_border = self.c_border
   IF Arg_Present(c_bg) THEN c_bg = self.c_bg
   IF Arg_Present(c_centhresh) THEN c_centhresh = self.c_centhresh
   IF Arg_Present(c_maxthresh) THEN c_maxthresh = self.c_maxthresh
   IF Arg_Present(c_minthresh) THEN c_minthresh = self.c_minthresh
   IF Arg_Present(colortable) THEN self.colors -> GetProperty, CTINDEX=colortable
   IF Arg_Present(colortool) THEN colortool = self.colors
   IF Arg_Present(format) THEN format = self.format
   IF Arg_Present(maxthresh) THEN maxthresh = self.maxthresh
   IF Arg_Present(minthresh) THEN minthresh = self.minthresh
   IF Arg_Present(range) THEN range = self.range

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CT_STRETCH::RESET
;
; PURPOSE:
;
;       This method resets the histogram GUI, based on the display image current in the image object.
;
; SYNTAX:
;
;       ct_Stretch -> DrawPlot
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
PRO CT_Stretch::Reset

   @cat_pro_error_handler

   ;Set the threshold values based on the data range.
   self.minThresh = Min(self.range)
   self.maxThresh = Max(self.range)
   self.cenThresh = (self.maxThresh - self.minThresh) / 2.0 + self.minThresh

   ; Reset line to move.
   self.lineToMove = ''

   ; Display the color bar
   self -> Draw

   ; Send a message, if needed.
   self -> SendMessage, 'CT_STRETCH_CHANGE', $
      DATA={maxThresh:self.maxThresh, minThresh:self.minThresh}

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CT_STRETCH::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the CT_STRETCH properties.
;
; SYNTAX:
;
;       ct_Stretch -> SetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     C_BG:          The name of the background color for the histogram plot. "Charcoal" by default.
;
;     C_BORDER:      The name of the  color for the histogram plot. "Wheat" by default.
;
;     C_CENTHRESH:   The name of the center theshold color for the histogram plot. "Sky Blue" by default.
;
;     C_MAXTHRESH:   The name of the maximum theshold color for the histogram plot. "Green" by default.
;
;     C_MINTHRESH:   The name of the minimum theshold color for the histogram plot. "Yellow" by default.
;
;     COLORTABLE:    A (standard) color table index number.
;
;     COLORTOOL:     A ColorTool object.
;
;     DRAW:          Set this keyword if you wish to call the DRAW method when all properties have been set.
;
;     FORMAT:        A format specification for formatting the MIN/MAX annotations.
;
;     MAXTHRESH:     Set this keyword to the maximum threshold value.
;
;     MINTHRESH:     Set this keyword to the minimum threshold value.
;
;     RANGE:         The data range of the color table.
;
;     XSIZE:         The X size of the histostretch draw widget.
;
;     YSIZE:         The Y size of the histostretch draw widget.
;
;     _EXTRA:        Any keywords appropriate for the superclass SetProperty" method.
;-
;*****************************************************************************************************
PRO CT_Stretch::SetProperty,    $
   C_BORDER=c_border, $
   C_BG=c_bg, $
   C_CENTHRESH=c_centhresh, $
   C_MAXTHRESH=c_maxthresh, $
   C_MINTHRESH=c_minthresh, $
   COLORTABLE=colortable, $
   COLORTOOL=colortool, $
   DRAW=draw, $
   FORMAT=format, $
   MAXTHRESH=maxthresh, $
   MINTHRESH=minthresh, $
   RANGE=range, $
   XSIZE=xsize, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(c_border) NE 0 THEN self.c_border = c_border
   IF N_Elements(c_bg) NE 0 THEN BEGIN
      self.c_bg = c_bg
      self.pixmapID -> SetProperty, Background_Color=c_bg
   ENDIF
   IF N_Elements(c_minthresh) NE 0 THEN self.c_minthresh = c_minthresh
   IF N_Elements(c_maxthresh) NE 0 THEN self.c_maxthresh = c_maxthresh
   IF N_Elements(c_centhresh) NE 0 THEN self.c_centhresh = c_centhresh
   IF N_Elements(colortable) NE 0 THEN BEGIN
      LOADCT, 0 > colortable < 40, /Silent
      TVLCT, r, g, b, /Get
      self.r = r
      self.g = g
      self.b = b
      self.colors -> LoadCT, colortable
      self -> Draw
   ENDIF
   IF N_Elements(colortool) NE 0 THEN BEGIN
      Obj_Destroy, self.colors
      self.colors = colortool
   ENDIF
   IF N_Elements(format) NE 0 THEN self.format = format
   IF N_Elements(range) NE 0 THEN BEGIN
      self.minthresh = Min(range)
      self.maxthresh = Max(range)
      self.cenThresh = ((self.maxThresh - self.minThresh) / 2.0) + self.minThresh
      self.range = range
      self.closeto = Abs(range[1] - range[0]) * 0.025
      self.coords -> SetProperty, YRANGE=range
      self -> Draw
   ENDIF

   IF (N_Elements(xsize) NE 0) THEN BEGIN
      xsize = 50 > xsize
      self -> SetProperty, Update=0
      self.minTextID -> SetProperty, Scr_XSize=xsize
      self.maxTextID -> SetProperty, Scr_XSize=xsize
      self.drawID -> SetProperty, XSize=xsize
      self.pixmapID -> SetProperty, XSize=xsize
      self -> SetProperty, Update = 1
      self -> Draw
   ENDIF
   IF (N_Elements(ysize) NE 0) THEN BEGIN
      self.minTextID -> GetProperty, First_Parent=parent
      parent -> GetProperty, Scr_YSize=textSize
      self.drawID -> SetProperty, YSize=ysize - (2*textSize)
      self.pixmapID -> SetProperty, YSize=ysize - (2*textSize)
      self -> Draw
   ENDIF
   IF N_Elements(maxthresh) NE 0 THEN self.maxthresh = maxthresh
   IF N_Elements(minthresh) NE 0 THEN self.minthresh = minthresh
   self.cenThresh = ((self.maxThresh - self.minThresh) / 2.0) + self.minThresh

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> BASEWIDGET::SetProperty, _EXTRA=extraKeywords

   ; Need to draw?
   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CT_STRETCH::CLEANUP
;
; PURPOSE:
;
;       This is the CT_STRETCH object class destructor method.
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
PRO CT_Stretch::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self.drawID
   Obj_Destroy, self.pixmapID
   IF Obj_Valid(self.colors) THEN self.colors -> RemoveParent, self
   Obj_Destroy, self.image

   self -> BASEWIDGET::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CT_STRETCH::INIT
;
; PURPOSE:
;
;       This is the CT_STRETCH object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     parent:        The parent object widget for this compound widget. Must be a BASEWIDGET.
;
; KEYWORDS:
;
;     C_BG:          The name of the background color for the histogram plot. "Charcoal" by default.
;
;     C_BORDER:      The name of the  color for the histogram plot. "Wheat" by default.
;
;     C_CENTHRESH:   The name of the center theshold color for the histogram plot. "Sky Blue" by default.
;
;     C_MAXTHRESH:   The name of the maximum theshold color for the histogram plot. "Green" by default.
;
;     C_MINTHRESH:   The name of the minimum theshold color for the histogram plot. "Yellow" by default.
;
;     COLORTABLE:    The color table index number. This keyword is ignored if the COLORTOOL keyword is used.
;
;     COLORTOOL:     A ColorTool object can be passed into the program with this keyword. If it is, this
;                    object is used as the color object for the internal image and the COLORTABLE keyword
;                    is ignored. Also, the original R, G, and B vectors will be obtained from this object.
;
;     FORMAT:        A format specification for formatting the MIN/MAX annotations. By default, "(F8.2)".
;
;     FRAME:         Set this keyword to put a frame around the compound widget.
;
;     MAXTHRESH:     Set this keyword to the maximum threshold value. Set to the maximum
;                    value of the image data by default.
;
;     MINTHRESH:     Set this keyword to the minimum threshold value. Set to the minimum
;                    value of the image data by default.
;
;     RANGE:         A two-element array specifying the minimum and maximum range of the color bar.
;                    By default, [0, 255]. If the default range is used, the FORMAT keyword is set to '(I3)'.
;
;     XSIZE:         The X size of the compound widget in pixels. By default, 50 pixels. (The minimum size
;                    allowed is also 50 pixels.)
;
;
;     YSIZE:         The Y size of the compound widget in pixels. By default, 300 pixels.
;
;     _EXTRA:        Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION CT_Stretch::INIT, parent, $
   C_BG=c_bg, $
   C_BORDER=c_border, $
   C_CENTHRESH=c_centhresh, $
   C_MAXTHRESH=c_maxthresh, $
   C_MINTHRESH=c_minthresh, $
   COLORTABLE=colortable, $
   COLORTOOL=colortool, $
   FORMAT=format, $
   FRAME=frame, $
   MAXTHRESH=maxThresh, $
   MINTHRESH=minThresh, $
   RANGE=range, $
   XSIZE=xsize, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   @cat_func_error_handler

   ; Initialize yourself as a BASEWIDGET. Explicit sizing.
   ok = self -> BASEWIDGET::INIT(parent, _Extra=extraKeywords, Column=1, Frame=frame, $
      Base_Align_Center=1, YPad=0, XPad=0, Space=0)
   IF NOT ok THEN RETURN, 0

   ; Check parameters.
   IF N_Elements(parent) EQ 0 THEN Message, 'A parent object is required for CT_STRETCH.'
   IF N_Elements(c_bg) EQ 0 THEN c_bg = 'Charcoal'
   IF N_Elements(c_border) EQ 0 THEN c_border = 'Wheat'
   IF N_Elements(c_centhresh) EQ 0 THEN c_centhresh = 'Sky Blue'
   IF N_Elements(c_maxthresh) EQ 0 THEN c_maxthresh = 'Green'
   IF N_Elements(c_minthresh) EQ 0 THEN c_minthresh = 'Yellow'
   IF N_Elements(format) EQ 0 THEN format = '(F8.2)'
   IF N_Elements(range) EQ 0 THEN BEGIN
      range = [0, 255]
      format = '(I3)'
   ENDIF
   IF N_Elements(xsize) EQ 0 THEN xsize = 50
   IF N_Elements(ysize) EQ 0 THEN ysize = 350

   ; Enforce a minimum X size for the widget.
   xsize = 50 > xsize

   ; Set the threshold values if they are not defined.
   IF N_Elements(minThresh) EQ 0 THEN minThresh = Min(range)
   IF N_Elements(maxThresh) EQ 0 THEN maxThresh = Max(range)

   ; Was a ColorTool passed in?
   IF N_Elements(colortool) NE 0 THEN BEGIN
      self.colors = colortool
      self.colors -> AddParent, self
   ENDIF ELSE BEGIN
      self.colors = Obj_New('ColorTool', colortable)
   ENDELSE

   ; Load the original color table vectors.
   self.colors -> GetProperty, Red=r, Green=g, Blue=b
   self.r = r
   self.g = g
   self.b = b


   ; Load em up!
   self.c_bg = c_bg
   self.c_border = c_border
   self.c_centhresh = c_centhresh
   self.c_maxthresh = c_maxthresh
   self.c_minthresh = c_minthresh
   self.minThresh = minThresh
   self.maxThresh = maxThresh
   self.cenThresh = (maxThresh - minThresh) / 2.0 + minThresh
   self.format = format

   ; Position of color bar in window.
   colorPosition = [0.20, 0.05, 0.80, 0.95]

   ; Create a coordinates object for keeping track of coordinates.
   self.coords = Obj_New('CATCOORD', YRange=range, XRange=[0,1], Position=colorPosition)

   ; Create a delta for being "close" to a line
   self.closeto = Abs(range[1] - range[0]) * 0.025
   self.lineToMove = ''
   self.range = range

   ; Create the rest of the widgets.
   maxrowID = Obj_New('BASEWIDGET', self, Row=1, YPad=0, XPad=0, Space=0)
   self.maxtextID = Obj_New('TEXTWIDGET', maxrowID, Value=String(maxThresh, Format=self.format), $
      SCR_XSIZE=xsize, /Editable , NAME='MAXTHRESH')

   ; Get the size of one of the text widgets, so you can size the draw widget properly.
   maxrowID -> GetProperty, Scr_YSize=ys

   self.drawID = Obj_New('DRAWWIDGET', self, XSIZE=xsize-4, YSIZE=ysize-(2*ys)-4, Button_Events=1, $
      Name='CT_STRETCH_DRAWWIDGET', NOTIFY_REALIZE=1)

   minrowID = Obj_New('BASEWIDGET', self, Row=1, YPad=0, XPad=0, Space=0)
   self.mintextID = Obj_New('TEXTWIDGET', minrowID, Value=String(minThresh, Format=self.format) , $
      SCR_XSIZE=xsize, /Editable, Name='MINTHRESH')

  ; Create a pixmap widget for buffered redraw.
   self.pixmapID = Obj_New('PIXMAPWIDGET', self, XSIZE=xsize-4, YSIZE=ysize-(2*ys)-4, Name='CT_STRETCH_PIXMAP', $
      BackgroundColor=c_bg)

   ; Create the image.
   self.image = Obj_New('CatImage2d', Replicate(1,50) # Bindgen(256), Color_Object=self.colors, $
      Coord_Object=self.coords, Position=colorPosition, WID=self.pixmapID)

   ; Add the image to the pixmap.
   self.pixmapID -> Add, self.image

   ; Register properties for the property sheet.
   self -> RegisterProperty, 'FORMAT', 4, NAME="Format"
   self -> RegisterProperty, 'MAXTHRESH', 3, NAME="Maximum Range"
   self -> RegisterProperty, 'MINTHRESH', 3, NAME="Minimum Range"

   ; Done.
   self -> Report, /Completed

   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       CT_STRETCH DEFINITION
;
; PURPOSE:
;
;       This is the object class definition for the CT_STRETCH object.
;
;*****************************************************************************************************
PRO CT_Stretch__DEFINE, class

   class = { CT_STRETCH, $
             r: BytArr(256), $               ; The original red color vector.
             g: BytArr(256), $               ; The original green color vector.
             b: BytArr(256), $               ; The original blue color vector.
             c_bg: "", $                     ; The name of the background color.
             c_border: "", $                 ; The name of the border color.
             c_centhresh: "", $              ; The name of the center threshold color.
             c_maxthresh: "", $              ; The name of the maximum threshold color.
             c_minthresh: "", $              ; The name of the minimum threshold color.
             cenThresh: 0.0, $               ; The center of the threshold range.
             closeto: 0.0, $                 ; A delta for establishing if you are "close" to a line.
             colors: Obj_New(), $            ; A colortool object for image colors.
             coords: Obj_New(), $            ; A coordinate object for the colorbar.
             drawID: Obj_New(), $            ; The draw widget object.
             format: "", $                   ; The string text format.
             image: Obj_New(), $             ; The internal image that is displayed.
             lineToMove: "", $               ; A string identifying which line we should move. MIN, MAX, CENTER.
             maxThresh: 0.0, $               ; The maximum threshold.
             minThresh: 0.0, $               ; The minimum threshold.
             pixmapID: Obj_New(), $          ; The pixmap widget object.
             minTextID: Obj_New(), $         ; The minimum Text Widget.
             maxTextID: Obj_New(), $         ; The maximum Text Widget.
             range: FltArr(2), $             ; The range of the color table.
             INHERITS BASEWIDGET $           ; Inherits base widget capability.
           }

END



;PRO CT_Stretch_Example
;
;tlb = Obj_New('TopLevelBase', Row=1)
;colors = Obj_New('colortool', 33)
;draw = Obj_New('Drawwidget', tlb, XSize=400, YSize=300)
;coords = Obj_New('CatCoord', xrange=[0,1], yrange=[-1000, 3400])
;stretch = Obj_New('CT_Stretch', tlb, range=[-1000, 3400], Colortool=colors, YSize=200)
;draw1 = Obj_New('Drawwidget', tlb, XSize=400, YSize=300)
;image = Obj_New('CatImage2D', loaddata(5), Color_Object=colors)
;image1 = Obj_New('CatImage2D', loaddata(7), Color_Object=colors)
;draw -> Add, image
;draw1 -> Add, image1
;tlb -> Draw
;colors -> RegisterForMessage, image, 'COLORTOOL_SETPROPERTY'
;colors -> RegisterForMessage, image1, 'COLORTOOL_SETPROPERTY'
;wait, 2
;stretch -> SetProperty, Ysize=300
;wait, 2
;stretch -> SetProperty, MaxThresh=100, /Draw
;stretch -> Draw
;stretch -> ShowContents
;END