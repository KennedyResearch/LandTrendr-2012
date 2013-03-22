;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL
;
; PURPOSE:
;
;       The purpose of the ColorTool object is to allow the user to construct and manage
;       a color table. Standard color tables can be loaded, drawing colors can be
;       specified by name and value, etc. Calling the DRAW method on a Colortool object causes
;       that object's color table to be loaded in IDL's hardware color table. A fundamental
;       principle of the Catalyst Library is that every piece of data should have associated
;       with it a color table by which it can be displayed, and a coordinate system which will
;       establish a data coordinate system for the data. The COLORTOOL performs the former function.
;       It can be set and retrieved from data objects by using the COLOR_OBJECT keyword in 
;       SetProperty and GetProperty methods. ColorTool objects are often inherited by objects.
;       For example, if you add a data object to a draw widget, the data object is likely to
;       inherit the ColorTool object of the draw widget. This makes it easy to establish a
;       consistent color scheme for all objects being displayed in a single draw widget.
;
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING       BURRIDGE COMPUTING
;       1645 Sheely Drive                 18 The Green South
;       Fort Collins, CO 80526 USA        Warborough, Oxon, OX10 7DN England
;       Phone: 970-221-0438               Phone: +44 (0) 1865 858 279
;       E-mail: davidf@dfanning.com       davidb@burridgecomputing.co.uk
;
; SUPERCLASSES:
;
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; SYNTAX:
;
;       colorObject = Obj_New('COLORTOOL')
;
; CLASS_STRUCTURE:
;
;   class  = { COLORTOOL, $               ; The COLORTOOL object definition.
;              INHERITS CATATOM, $        ; Inherits the CATATOM object class.
;              _brewer: 0B, $             ; Flag that indicates a brewer color table.
;              _r: BytArr(256), $         ; The red color table vector.
;              _g: BytArr(256), $         ; The green color table vector.
;              _b: BytArr(256),  $        ; The blue color table vector.
;              _colornames: Ptr_New(), $  ; The names of "known" colors.
;              _rvalue: Ptr_New(), $      ; The red color values associated with color names.
;              _gvalue: Ptr_New(), $      ; The red color values associated with color names.
;              _bvalue: Ptr_New(), $      ; The red color values associated with color names.
;              _colorfile: "", $          ; The name of the color table file to read.
;              _ncolors: 0L, $            ; The number of colors in standard color table.
;              _bottom: 0L, $             ; The starting index of the standard color table.
;              _scTLB: Obj_New(), $       ; The SHOWCOLORS tlb object.
;              _selTLB: Obj_New(), $      ; The SELECTCOLORS tlb object.
;              _xcTLB: Obj_New(), $       ; The XCOLORS tlb object.
;              _xc_registerName: "", $    ; The name with which the XCOLORS dialog is registered.
;              _ctindex: 0L, $            ; The current color table index.
;              _r_old: BytArr(256), $     ; The old red color table vector.
;              _g_old: BytArr(256), $     ; The old green color table vector.
;              _b_old: BytArr(256) $      ; The old blue color table vector.
;             }
;
; MESSAGES:
;
;
;       COLORTOOL_TABLECHANGE: A message with this title is sent when color table vectors are
;                              changed in the object. In additon to the message, a DATA keyword is
;                              used to pass an anonymous structure containing the current R, G, and
;                              B color table vectors, along with the BOTTOM index for loading the vectors.
;
;       COLORTOOL_SETPROPERTY: A message with this title is sent any time the SetProperty method is
;                              called. No data is sent. Message recipients can call the GetPropery
;                              method on the message SENDER to obtain appropriate information.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 13 March 2003.
;       Added system color names to GETCOLOR method. DWF. 23 Jan 2004.
;       Modified the way ShowColors method works if a ShowColors window is on the display. Now
;          current colors are always displayed when ShowColors is called. DWF, 26 Jan 2004.
;       Added Stretch method to object. DWF, 26 Jan 2004.
;       Added support for 8-bit displays to GETCOLOR method. DWF. 25 Aug 2004.
;       Added support for BREWER color tables. 25 June 2008. DWF.
;       Added PICKCOLORNAME method. 7 July 2008. DWF.
;       Additional changes to BREWER support. 25 October 2008. DWF.
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
;       COLORTOOL::AddNamedColor
;
; PURPOSE:
;
;       This method adds a color to the internal list of named colors.
;
; SYNTAX:
;
;       colorObject -> AddNamedColor, color, colorName
; or
;       colorObject -> AddNamedColor, r, g, b, colorName
;
; ARGUMENTS:
;
;       color:     A long integer (or integer array) represeting a 24-bit color.
;
;       colorname: The name of the color. The color *must* have a name!
;
;       r:         The red component (or array of components) of the color.
;
;       g:         The green component (or array of components) of the color.
;
;       b:         The blue component (or array of components) of the color.
;
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO ColorTool::AddNamedColor, rr, gg, bb, name

   @cat_pro_error_handler

   CASE N_Params() OF
      0: Message, 'Must pass method arguments.'

      1: Message, 'A color name is required.'

      2: BEGIN
         IF N_Elements(rr) NE 3 THEN Message, 'First argument must be a three-element array.'
         IF Size(gg, /TNAME) NE 'STRING' THEN Message, 'Second argument must be a color name.'
         b = rr[2]
         g = rr[1]
         r = rr[0]
         name = gg
         END
      3: Message, 'A color name is required.'
      4: BEGIN
         r = 0 > rr < 255
         g = 0 > gg < 255
         b = 0 > bb < 255
         IF Size(name, /TNAME) NE 'STRING' THEN Message, 'Fourth argument must be a color name.'

         END
   ENDCASE

      ; Add the color to the named color palette

   *self._colornames = [*self._colornames, StrUpCase(StrCompress(StrTrim(name,2), /Remove_All))]
   *self._rvalue = [*self._rvalue, r]
   *self._gvalue = [*self._gvalue, g]
   *self._bvalue = [*self._bvalue, b]

END



;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::COLOR8to24
;
; PURPOSE:
;
;       This method converts a color triple to an equivalent 24-bit integer value.
;
; SYNTAX:
;
;       color24 = colorObject -> Color8to24(color)
;
; ARGUMENTS:
;
;       color:    A [red, green, blue] color triple that describes a particular color.
;                 The triple can be either a row or column vector of 3 elements or it
;                 can be an N-by-3 array of color triples.
;
; RETURN VALUE:
;
;       color24:  A 24-bit value, representing the red, green, and blue components
;                 of the 8-bit starting values.
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
FUNCTION ColorTool::Color8to24, color

   @cat_func_error_handler

   s = Size(color)

   IF s[0] EQ 1 THEN BEGIN
      IF s[1] NE 3 THEN Message, 'Input color parameter must be a 3-element vector.'
      RETURN, color[0] + (color[1] * 2L^8) + (color[2] * 2L^16)
   ENDIF ELSE BEGIN
      IF s[2] GT 3 THEN Message, 'Input color parameter must be an N-by-3 array.'
      RETURN, color[*,0] + (color[*,1] * 2L^8) + (color[*,2] * 2L^16)
   ENDELSE

END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::COLOR24to8
;
; PURPOSE:
;
;       This method converts a 24-bit integer value representing a color to a
;       three-element array representing the equivalent RGB values of the 24-bit integer.
;       In other words, it is the inverse operation to Color8to24.
;
; SYNTAX:
;
;       rgbcolor = colorObject -> Color24to8(color24)
;
; ARGUMENTS:
;
;       color:     A 24-bit long integer value representing a 24-bit color.
;
;
; RETURN VALUE:
;
;       rgbcolor:  A three-element array, representing the red, green, and blue components
;                  of the 24-bit starting value. Or, if an array is passed in, a 3-by-n array
;                  of color values.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
FUNCTION ColorTool::Color24to8, color

   @cat_func_error_handler

   type = Size(color, /TNAME)
   IF type NE 'LONG' THEN color = Long(color)

   num = N_Elements(color)
   retValue = BytArr(3, num)
   FOR j = 0, num - 1 DO BEGIN
      retValue[0,j] = Byte(color[j])
      retValue[1,j] = Byte(ISHFT(color[j],  -8))
      retValue[2,j] = Byte(ISHFT(color[j], -16))
   ENDFOR

   RETURN, retValue
END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::CONGRID
;
; PURPOSE:
;
;       This method is a slight variation of IDL's Congrid routine. Specifically,
;       it checks for a divide by zero that can occur occasionally in XCOLORS.
;
; SYNTAX:
;
;       newArray = colorObject -> Congrid(array, x, y, z)
;
; ARGUMENTS:
;
;       array:  A 1, 2, or 3 dimensional array to resize.
;               Data Type : Any type except string or structure.
;
;       x:      The new X dimension of the resized array.
;               Data Type : Int or Long (greater than or equal to 2).
;
;       y:      The new Y dimension of the resized array. If the original
;               array has only 1 dimension then y is ignored. If the original
;               array has 2 or 3 dimensions then y MUST be present.
;
;       z:      The new Z dimension of the resized array. If the original
;               array has only 1 or 2 dimensions then z is ignored. If the
;               original array has 3 dimensions then z MUST be present.
;
; KEYWORDS:
;
;       CENTER: If this keyword is set, shift the interpolation so that points
;               in the input and output arrays are assumed to lie at the midpoint
;               of their coordinates rather than at their lower-left corner.
;
;       CUBIC:  If specified and non-zero, "Cubic convolution"
;               interpolation is used.  This is a more
;               accurate, but more time-consuming, form of interpolation.
;               CUBIC has no effect when used with 3 dimensional arrays.
;               If this parameter is negative and non-zero, it specifies the
;               value of the cubic interpolation parameter as described
;               in the INTERPOLATE function.  Valid ranges are -1 <= Cubic < 0.
;               Positive non-zero values of CUBIC (e.g. specifying /CUBIC)
;               produce the default value of the interpolation parameter
;               which is -1.0.
;
;       INTERP: If set, causes linear interpolation to be used.
;               Otherwise, the nearest-neighbor method is used.
;
;
;       MINUS_ONE: If set, will prevent CONGRID from extrapolating one row or
;               column beyond the bounds of the input array.   For example,
;               If the input array has the dimensions (i, j) and the
;               output array has the dimensions (x, y), then by
;               default the array is resampled by a factor of (i/x)
;               in the X direction and (j/y) in the Y direction.
;               If MINUS_ONE is present (AND IS NON-ZERO) then the array
;               will be resampled by the factors (i-1)/(x-1) and (j-1)/(y-1).
;
;-
;*****************************************************************************************************
Function ColorTool::Congrid, arr, x, y, z, INTERP=int, MINUS_ONE=m1, CUBIC = cubic

    ON_ERROR, 2      ;Return to caller if error
    s = Size(arr)

    if ((s[0] eq 0) or (s[0] gt 3)) then $
      Message, 'Array must have 1, 2, or 3 dimensions.'

    ;;  Supply defaults = no interpolate, and no minus_one.
    if (N_ELEMENTS(int) le 0) then int = 0 else int = KEYWORD_SET(int)
    if (N_ELEMENTS(m1) le 0) then m1 = 0 else m1 = KEYWORD_SET(m1)
    if (N_ELEMENTS(cubic) eq 0) then cubic = 0
    if (cubic ne 0) then int = 1 ;Cubic implies interpolate


    case s[0] of
        1: begin                ; *** ONE DIMENSIONAL ARRAY
            ; DWF modified: Check divide by zero.
            srx = float(s[1] - m1)/((x-m1) > 1e-6) * findgen(x) ;subscripts
            if (int) then $
              return, INTERPOLATE(arr, srx, CUBIC = cubic) else $
              return, arr[ROUND(srx)]
        endcase
        2: begin                ; *** TWO DIMENSIONAL ARRAY
            if (int) then begin
                srx = float(s[1] - m1) / ((x-m1) > 1e-6) * findgen(x)
                sry = float(s[2] - m1) / ((y-m1) > 1e-6) * findgen(y)
                return, INTERPOLATE(arr, srx, sry, /GRID, CUBIC=cubic)
            endif else $
              return, POLY_2D(arr, $
                              [[0,0],[(s[1]-m1)/(float(x-m1) > 1e-6),0]], $ ;Use poly_2d
                              [[0,(s[2]-m1)/(float(y-m1) > 1e-6)],[0,0]],int,x,y)

        endcase
        3: begin                ; *** THREE DIMENSIONAL ARRAY
            srx = float(s[1] - m1) / ((x-m1) > 1e-6) * findgen(x)
            sry = float(s[2] - m1) / ((y-m1) > 1e-6) * findgen(y)
            srz = float(s[3] - m1) / ((z-m1) > 1e-6) * findgen(z)
            return, interpolate(arr, srx, sry, srz, /GRID)
        endcase
    endcase

    return, arr_r
END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::DRAW
;
; PURPOSE:
;
;       This method loads the color table vectors in the hardware color table.
;
; SYNTAX:
;
;       colorObject -> Draw
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
PRO ColorTool::Draw, _Extra=extra

   @cat_pro_error_handler

   IF self._ncolors LT 256 THEN BEGIN

      TVLCT, Congrid(self._r, self._ncolors), $
             Congrid(self._g, self._ncolors), $
             Congrid(self._b, self._ncolors), self._bottom

   ENDIF ELSE BEGIN

      TVLCT, self._r, self._g, self._b

   ENDELSE

END



;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::EVENTHANDLER
;
; PURPOSE:
;
;       An event handler method to handle widget events from the various graphical user interfaces
;       associated with the ColorTool Object.
;
; SYNTAX:
;
;       Typically called from the EVENTDISPATCHER utility routine. All event objects are named,
;       and the CASE statement branches on the object's name.
;
; ARGUMENTS:
;
;       Event:  The event structure from a widget event.
;
;       Object: The object reference of the widget object that generated the event.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO ColorTool::EventHandler, event

   @cat_pro_error_handler

   currentWindow = !D.Window
   sendmessage = 0

   CASE StrUpCase(event.name) OF
   
      'XCOLORS_SWITCH_COLORTYPE': BEGIN

           topObject = CatGetTopObject(event.id)
           topObject -> GetProperty, UValue=info
           event.id -> GetProperty, UVALUE=colortypes
           thisType = colortypes[event.index]
           
           ; Find the current list widget.
           oldListWidget = topObject -> Get('XCOLORS_COLORTABLE_SELECT', /RECURSIVE_SEARCH)
           oldListWidget -> GetProperty, PARENT=parent
           
           CASE thisType OF
           
                'IDL': BEGIN
                   self._colorfile = Filepath(SubDir=['resource','colors'], 'colors1.tbl')
                   END
                   
                'BREWER': BEGIN
                   self._colorfile = File_Which('fsc_brewer.tbl')
                   END
           ENDCASE
           
            colorNames=''
            LoadCT, Get_Names=colorNames, File=self._colorfile
            colorNamesIndex = StrArr(N_Elements(colorNames))
            FOR j=0,N_Elements(colorNames)-1 DO $
               colorNamesIndex[j] = StrTrim(j,2) + ' - ' + colorNames[j]
            list = Obj_New('LISTWIDGET', parent, Value=colorNamesIndex, YSize=15, Scr_XSize=256, $
               Event_Object=self, Name='XCOLORS_COLORTABLE_SELECT', $
               Index=info._ctIndex, UVALUE=colorNamesIndex)

            Obj_Destroy, oldListWidget

         ENDCASE

   

      'XCOLORS_REVERSE_COLORS': BEGIN

            topObject = CatGetTopObject(event.id)
            topObject -> GetProperty, UValue=info
            self -> Reverse
            info._rorig = self._r
            info._gorig = self._g
            info._borig = self._b
            self -> XColors_Update, info
            topObject -> SetProperty, UValue=info

         ENDCASE

      'XCOLORS_BOTTOM_SLIDER': BEGIN

            topObject = CatGetTopObject(event.id)
            topObject -> GetProperty, UValue=info

            ; Update the current bottom value of the slider.
            currentBottom = event.value + self._bottom
            info._topSlider -> GetProperty, Value=currentTop
            currentTop = currentTop + self._bottom

            ; Bottom cannot be larger than top.
            IF currentBottom GE (currentTop-1) THEN $
               info._bottomSlider -> SetProperty, Value=((currentTop-1) - self._bottom)

            self -> XColors_Update, info
            topObject -> SetProperty, UValue=info

            sendmessage = 1

         ENDCASE

      'XCOLORS_TOP_SLIDER': BEGIN

           topObject = CatGetTopObject(event.id)
           topObject -> GetProperty, UValue=info

           ; Update the current top value of the slider.
           currentTop = event.value + self._bottom
           info._bottomSlider -> GetProperty, Value=currentBottom
           currentBottom = currentBottom + self._bottom

           ; Top cannot be smaller than bottom.
           IF currentTop LE (currentBottom) THEN $
              info._topSlider -> SetProperty, Value=((currentBottom+1) - self._bottom)

           self -> XColors_Update, info
           topObject -> SetProperty, UValue=info
           sendmessage = 1

         ENDCASE

      'XCOLORS_GAMMA_SLIDER': BEGIN

            topObject = CatGetTopObject(event.id)
            topObject -> GetProperty, UValue=info

            realgamma = 10^((event.value/50.0) - 1)
            info._gammaLabel -> SetProperty, Value=String(realgamma, Format='(F7.3)')

            self -> XColors_Update, info
            topObject -> SetProperty, UValue=info
            sendmessage = 1

         ENDCASE

      'XCOLORS_COLORTABLE_SELECT': BEGIN

            topObject = CatGetTopObject(event.id)
            topObject -> GetProperty, UValue=info

            ; Load the new color table.
            self -> LoadCT, (*event.index)[0]

            ; Update the slider positions and values.
            info._bottomSlider -> SetProperty, Value=0
            info._topSlider -> SetProperty, Value=self._ncolors-1
            info._gammaSlider -> SetProperty, Value=50
            info._gammaLabel -> SetProperty, Value=String(1.0, Format='(F7.3)')

         ENDCASE

      'XCOLORS_CANCEL_BUTTON': BEGIN

            topObject = CatGetTopObject(event.id)
            topObject -> GetProperty, UValue=info

            self._r[self._bottom:self._bottom+self._ncolors-1] = info._rcancel
            self._g[self._bottom:self._bottom+self._ncolors-1] = info._gcancel
            self._b[self._bottom:self._bottom+self._ncolors-1] = info._bcancel
            self._ctindex = info._cancelIndex
            self -> Draw
            Obj_Destroy, self._xcTLB
            self._xc_registerName = ""
            self -> GUI_Update
            self -> SendMessage, 'COLORTOOL_TABLECHANGE', $
               DATA={r:info._rcancel, g:info._gcancel, b:info._bcancel, bottom:self._bottom}

         ENDCASE

      'XCOLORS_ACCEPT_BUTTON': BEGIN

            topObject = CatGetTopObject(event.id)
            topObject -> GetProperty, UValue=info

            self._r_old = self._r
            self._g_old = self._g
            self._b_old = self._b
            self._r[self._bottom:self._bottom+self._ncolors-1] = info._rcurrent
            self._g[self._bottom:self._bottom+self._ncolors-1] = info._gcurrent
            self._b[self._bottom:self._bottom+self._ncolors-1] = info._bcurrent
            self -> Draw
            Obj_Destroy, self._xcTLB
            self._xc_registerName = ""
            ;self -> GUI_Update
;            sendmessage = 1

         ENDCASE

      'XCOLORS_TLB': BEGIN
;         print, 'Received Kill Request Event'
;         tlb = event.id -> GetProperty(/ID)
;         Widget_Control, tlb, /Destroy
         ENDCASE

      'PICKCOLORNAME_CANCEL': BEGIN
         topObject = CatGetTopObject(event.id)
         topObject -> GetProperty, UValue=info
         event.id -> GetProperty, UValue=cancelColorValue
         *info.ptr = cancelColorValue
         Obj_Destroy, self._selTLB
         ENDCASE

      'PICKCOLORNAME_ACCEPT': BEGIN
         topObject = CatGetTopObject(event.id)
         topObject -> GetProperty, UValue=info
         Obj_Destroy, self._selTLB
         ENDCASE
         
      'PICKCOLORNAME': BEGIN

         topObject = CatGetTopObject(event.id)
         topObject -> GetProperty, UValue=info
         event.id -> GetProperty, UValue=colorName
         info.currentColorID -> SetWindow
         theColor = self ->GetColor(colorName, Decomposed=1)
         Device, Decomposed=1, Get_Decomposed=theState
         Erase, Color=theColor
         PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=self->GetColor('black')
         *info.ptr = colorName
         info.colorLabelID -> SetProperty, Value=colorName
         Device, Decomposed=theState
      
         ENDCASE

      'SELECTCOLOR_CANCEL': BEGIN
         topObject = CatGetTopObject(event.id)
         topObject -> GetProperty, UValue=info
         event.id -> GetProperty, UValue=cancelColorValue
         *info.ptr = cancelColorValue
         Obj_Destroy, self._selTLB
         ENDCASE

      'SELECTCOLOR_ACCEPT': BEGIN
         topObject = CatGetTopObject(event.id)
         topObject -> GetProperty, UValue=info
         Obj_Destroy, self._selTLB
         ENDCASE

      'SELECTCOLOR_SLIDER': BEGIN

         topObject = CatGetTopObject(event.id)
         topObject -> GetProperty, UValue=info
         info.redID -> GetProperty, Value=redValue
         info.greenID -> GetProperty, Value=greenValue
         info.blueID -> GetProperty, Value=blueValue
         *info.ptr = [redValue, greenValue, blueValue]
         info.currentColorID -> SetWindow
         Device, Decomposed=1, Get_Decomposed=theState
         theColor = self -> Color8to24(*info.ptr)
         Erase, Color=theColor
         PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=self->GetColor('black')
         Device, Decomposed=theState
         ENDCASE

      'SELECTCOLOR_PICKCOLOR': BEGIN

         topObject = CatGetTopObject(event.id)
         topObject -> GetProperty, UValue=info
         event.id -> GetProperty, UValue=colorName
         info.currentColorID -> SetWindow
         theColor = self ->GetColor(colorName, Decomposed=1)
         Device, Decomposed=1, Get_Decomposed=theState
         Erase, Color=theColor
         PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=self->GetColor('black')
         *info.ptr = colorName
         Device, Decomposed=theState
         ENDCASE

   ENDCASE

      IF WindowAvailable(currentWindow) THEN WSet, currentWindow

      ; Send a message, if necessary that colors have changed.

      IF sendmessage THEN BEGIN
         IF N_Elements(info) EQ 0 THEN BEGIN
            topObject = CatGetTopObject(event.id)
            topObject -> GetProperty, UValue=info
         ENDIF
         self -> SendMessage, 'COLORTOOL_TABLECHANGE', $
            DATA={r:info._rcurrent, g:info._gcurrent, b:info._bcurrent, bottom:self._bottom}
      ENDIF
END



;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::GETCOLOR
;
; PURPOSE:
;
;       The purpose of this method is to obtain a drawing color
;       by name and in a device-decomposition independent way. To
;       see a list of color names available, type:
;
;         colorNames = colorObject -> GetColor(/Names) & Print, colorNames
;
; SYNTAX:
;
;      result = colorObject -> GetColor(theColor, colorIndex)
;
; EXAMPLE:
;
;      axisColor = colorObject -> GetColor("Green", !D.Table_Size-2)
;      backColor = colorObject -> GetColor("Charcoal", !D.Table_Size-3)
;      dataColor = colorObject -> GetColor("Yellow", !D.Table_Size-4)
;      Plot, Findgen(11), Color=axisColor, Background=backColor, /NoData
;      OPlot, Findgen(11), Color=dataColor

;
; ARGUMENTS:
;
;       theColor: A string with the "name" of the color. To see a list
;           of the color names available set the NAMES keyword. This may
;           also be a vector of color names. Colors available are these:
;
;                Almond   Antique White      Aquamarine           Beige          Bisque           Black
;                  Blue     Blue Violet           Brown       Burlywood        Charcoal      Chartreuse
;             Chocolate           Coral        Cornsilk            Cyan  Dark Goldenrod       Dark Gray
;            Dark Green      Dark Khaki     Dark Orchid     Dark Salmon       Deep Pink     Dodger Blue
;             Firebrick    Forest Green            Gold       Goldenrod            Gray           Green
;          Green Yellow        Honeydew        Hot Pink      Indian Red           Ivory           Khaki
;              Lavender      Lawn Green     Light Coral      Light Cyan      Light Gray    Light Salmon
;          Light Yellow      Lime Green           Linen         Magenta          Maroon     Medium Gray
;         Medium Orchid        Moccasin            Navy           Olive      Olive Drab          Orange
;            Orange Red          Orchid  Pale Goldenrod      Pale Green          Papaya            Peru
;                  Pink            Plum     Powder Blue          Purple             Red            Rose
;            Rosy Brown      Royal Blue    Saddle Brown          Salmon     Sandy Brown       Sea Green
;              Seashell          Sienna        Sky Blue      Slate Gray            Snow    Spring Green
;            Steel Blue             Tan         Thistle          Tomato       Turquoise          Violet
;            Violet Red           Wheat           White          Yellow
;
;           Also, these system color names are available in IDL 5.6 and higher:
;
;              Frame, Text, Active, Shadow, Highlight, Edge, Selected, Face.
;
;           The color WHITE is used if this parameter is absent or a color name is mis-spelled. To see a list
;           of the color names available in the program, type this:
;
;              Print, self -> GetColor(/Names), Format='(6A15)'
;
;       theColorIndex: The color table index (or vector of indices the same length
;           as the color name vector) where the specified color is loaded. The color table
;           index parameter should always be used if you wish to obtain a color value in a
;           color-decomposition-independent way in your code. See the NORMAL CALLING
;           SEQUENCE for details. If theColor is a vector, and theColorIndex is a scalar,
;           then the colors will be loaded starting at theColorIndex.
;
; RETURN VALUE:
;
;       The value that is returned by this method depends upon the keywords
;       used to call it and on the version of IDL you are using. In general,
;       the return value will be either a color index number where the specified
;       color is loaded by the program, or a 24-bit color value that can be
;       decomposed into the specified color on true-color systems. (Or a vector
;       of such numbers.)
;
;       If you are running IDL 5.2 or higher, the program will determine which
;       return value to use, based on the color decomposition state at the time
;       the program is called. If you are running a version of IDL before IDL 5.2,
;       then the program will return the color index number. This behavior can
;       be overruled in all versions of IDL by setting the DECOMPOSED keyword.
;       If this keyword is 0, the program always returns a color index number. If
;       the keyword is 1, the program always returns a 24-bit color value.
;
;       If the TRIPLE keyword is set, the program always returns the color triple,
;       no matter what the current decomposition state or the value of the DECOMPOSED
;       keyword. Normally, the color triple is returned as a 1 by 3 column vector.
;       This is appropriate for loading into a color index with TVLCT:
;
;          IDL> TVLCT, FSC_Color('Yellow', /Triple), !P.Color
;
;       But sometimes (e.g, in object graphics applications) you want the color
;       returned as a row vector. In this case, you should set the ROW keyword
;       as well as the TRIPLE keyword:
;
;          viewobj= Obj_New('IDLgrView', Color=FSC_Color('charcoal', /Triple, /Row))
;
;       If the ALLCOLORS keyword is used, then instead of a single value, modified
;       as described above, then all the color values are returned in an array. In
;       other words, the return value will be either an NCOLORS-element vector of color
;       table index numbers, an NCOLORS-element vector of 24-bit color values, or
;       an NCOLORS-by-3 array of color triples.
;
;       If the NAMES keyword is set, the program returns a vector of
;       color names known to the program.
;
; INPUT KEYWORD PARAMETERS:
;
;       ALLCOLORS:  Set this keyword to return indices, or 24-bit values, or color
;                   triples, for all the known colors, instead of for a single color.
;
;       DECOMPOSED: Set this keyword to 0 or 1 to force the return value to be
;                   a color table index or a 24-bit color value, respectively.
;
;       NAMES:      If this keyword is set, the return value of the function is
;                   a ncolors-element string array containing the names of the colors.
;                   These names would be appropriate, for example, in building
;                   a list widget with the names of the colors. If the NAMES
;                   keyword is set, the COLOR and INDEX parameters are ignored.
;
;                   listID = Widget_List(baseID, Value=GetColor(/Names), YSize=16)
;
;       ROW:        If this keyword is set, the return value of the function when the TRIPLE
;                   keyword is set is returned as a row vector, rather than as the default
;                   column vector. This is required, for example, when you are trying to
;                   use the return value to set the color for object graphics objects. This
;                   keyword is completely ignored, except when used in combination with the
;                   TRIPLE keyword.
;
;       TRIPLE:     Setting this keyword will force the return value of the function to
;                   *always* be a color triple, regardless of color decomposition state or
;                   visual depth of the machine. The value will be a three-element column
;                   vector unless the ROW keyword is also set.
;
; OUTPUT KEYWORD PARAMETERS:
;
;       CANCEL:     This keyword is always set to 0, unless that SELECTCOLOR keyword is used.
;                   Then it will correspond to the value of the CANCEL output keyword in PICKCOLORNAME.
;
;       NCOLORS:    The number of colors recognized by the program. It will be 88 by default.
;-
;*****************************************************************************************************
FUNCTION ColorTool::GetColor, theColor, colorIndex, $
   AllColors=allcolors, $
   Cancel=cancelled, $
   Decomposed=decomposedState, $
   _Extra=extra, $
   Names=names, $
   Row=row, $
   Triple=triple, $
   _Extra=extraKeywords

   @cat_func_error_handler

   ; Get depth of visual display.
   ncolors = N_Elements(self._colorNames)
   IF (!D.Flags AND 256) NE 0 THEN Device, Get_Visual_Depth=theDepth ELSE theDepth = 8
   IF theDepth EQ 8 THEN offset = !D.Table_Size - ncolors - 2 ELSE offset = 0

   ; Make sure you have a color name and color index.
   CASE N_Elements(theColor) OF
      0: BEGIN
            theColor = 'White'
            IF N_Elements(colorIndex) EQ 0 THEN BEGIN
               IF theDepth GT 8 THEN BEGIN
                  colorIndex = !P.Color < (!D.Table_Size - 1)
               ENDIF ELSE BEGIN
                  colorIndex = Where(StrUpCase(*self._colornames) EQ StrUpCase(theColor), count) + offset
                  colorIndex = colorIndex[0]
                  IF count EQ 0 THEN Message, 'Cannot find color: ' + StrUpCase(theColor), /NoName
               ENDELSE
            ENDIF ELSE colorIndex = 0 > colorIndex < (!D.Table_Size - 1)
         ENDCASE

      1: BEGIN
            type = Size(theColor, /TNAME)
            IF type NE 'STRING' THEN Message, 'The color must be expressed as a color name.'
            theColor = theColor[0] ; Make it a scalar or suffer wrath of the WHERE function!
            IF N_Elements(colorIndex) EQ 0 THEN BEGIN
               IF theDepth GT 8 THEN BEGIN
                  colorIndex = !P.Color < (!D.Table_Size - 1)
               ENDIF ELSE BEGIN
                  colorIndex = Where(StrUpCase(*self._colornames) EQ StrUpCase(theColor), count) + offset
                  colorIndex = colorIndex[0]
                  IF count EQ 0 THEN Message, 'Cannot find color: ' + StrUpCase(theColor), /NoName
               ENDELSE
            ENDIF ELSE colorIndex = 0 > colorIndex < (!D.Table_Size - 1)
            ENDCASE

      ELSE: BEGIN
            type = Size(theColor, /TNAME)
            IF type NE 'STRING' THEN Message, 'The colors must be expressed as color names.'
            ncolors = N_Elements(theColor)
            CASE N_Elements(colorIndex) OF
               0: colorIndex = Indgen(ncolors) + (!D.Table_Size - (ncolors + 1))
               1: colorIndex = Indgen(ncolors) + colorIndex
               ELSE: IF N_Elements(colorIndex) NE ncolors THEN $
                  Message, 'Index vector must be the same length as color name vector.'
            ENDCASE

               ; Did the user want color triples?

            IF Keyword_Set(triple) THEN BEGIN
               colors = LonArr(ncolors, 3)
               FOR j=0,ncolors-1 DO colors[j,*] = FSC_Color(theColor[j], colorIndex[j], Filename=filename, $
                  Decomposed=decomposedState, /Triple)
               RETURN, colors
            ENDIF ELSE BEGIN
               colors = LonArr(ncolors)
               FOR j=0,ncolors-1 DO colors[j] = FSC_Color(theColor[j], colorIndex[j], Filename=filename, $
                  Decomposed=decomposedState)
               RETURN, colors
           ENDELSE
         END
   ENDCASE

      ; Make sure the color parameter is an uppercase string.

   varInfo = Size(theColor)
   IF varInfo[varInfo[0] + 1] NE 7 THEN $
      Message, 'The color name parameter must be a string.', /NoName
   theColor = StrUpCase(StrCompress(StrTrim(theColor,2), /Remove_All))

      ; Check synonyms of color names.

   IF StrUpCase(theColor) EQ 'GREY' THEN theColor = 'GRAY'
   IF StrUpCase(theColor) EQ 'LIGHTGREY' THEN theColor = 'LIGHTGRAY'
   IF StrUpCase(theColor) EQ 'MEDIUMGREY' THEN theColor = 'MEDIUMGRAY'
   IF StrUpCase(theColor) EQ 'SLATEGREY' THEN theColor = 'SLATEGRAY'
   IF StrUpCase(theColor) EQ 'DARKGREY' THEN theColor = 'DARKGRAY'
   IF StrUpCase(theColor) EQ 'AQUA' THEN theColor = 'AQUAMARINE'
   IF StrUpCase(theColor) EQ 'SKY' THEN theColor = 'SKYBLUE'
   IF StrUpCase(theColor) EQ 'NAVY BLUE' THEN theColor = 'NAVY'
   IF StrUpCase(theColor) EQ 'NAVYBLUE' THEN theColor = 'NAVY'

      ; How many colors do we have?

   ncolors = N_Elements(*self._colorNames)

      ; Did the user ask for the color names? If so, return them now.

   IF Keyword_Set(names) THEN RETURN, Reform(*self._colorNames, 1, ncolors)

      ; Process the color names.

   theNames = StrUpCase( StrCompress( StrTrim( *self._colorNames, 2 ), /Remove_All ) )

      ; Find the asked-for color in the color names array.

   theIndex = Where(theNames EQ theColor, foundIt)
   theIndex = theIndex[0]

      ; If the color can't be found, report it and continue with
      ; the first color in the color names array.

   IF foundIt EQ 0 THEN BEGIN
      Message, "Can't find color " + theColor + ". Substituting " + StrUpCase((*self._colornames)[0]) + ".", /Informational
      theColor = theNames[0]
      theIndex = 0
   ENDIF

      ; Get the color triple for this color.

   r = (*self._rvalue)[theIndex]
   g = (*self._gvalue)[theIndex]
   b = (*self._bvalue)[theIndex]

      ; Did the user want a color triple? If so, return it now.

   IF Keyword_Set(triple) THEN BEGIN
      IF Keyword_Set(allcolors) THEN BEGIN
         IF Keyword_Set(row) THEN $
            RETURN, Transpose([[*self._rvalue], [*self._gvalue], [*self._bvalue]]) ELSE $
            RETURN, [[*self._rvalue], [*self._gvalue], [*self._bvalue]]
      ENDIF ELSE BEGIN
         IF Keyword_Set(row) THEN RETURN, [r, g, b] ELSE RETURN, [[r], [g], [b]]
      ENDELSE
   ENDIF

      ; Otherwise, we are going to return either an index
      ; number where the color has been loaded, or a 24-bit
      ; value that can be decomposed into the proper color.

   IF N_Elements(decomposedState) EQ 0 THEN BEGIN
      IF Float(!Version.Release) GE 5.2 THEN BEGIN
         IF (!D.Name EQ 'X' OR !D.Name EQ 'WIN' OR !D.Name EQ 'MAC') THEN BEGIN
            Device, Get_Decomposed=decomposedState
         ENDIF ELSE decomposedState = 0
      ENDIF ELSE decomposedState = 0
   ENDIF ELSE decomposedState = Keyword_Set(decomposedState)

      ; Return the color value or values.

   IF decomposedState THEN BEGIN

      IF Keyword_Set(allcolors) THEN BEGIN
         RETURN,  self -> Color8to24([[*self._rvalue], [*self._gvalue], [*self._bvalue]])
      ENDIF ELSE BEGIN
         RETURN,  self -> Color8to24([r, g, b])
      ENDELSE

   ENDIF ELSE BEGIN

      IF Keyword_Set(allcolors) THEN BEGIN

         IF N_Elements(colorIndex) EQ 0 THEN colorIndex = !D.Table_Size - ncolors - 1
         IF colorIndex LT 0 THEN $
            Message, 'Number of colors exceeds available color table values. Returning.', /NoName
         IF !D.Name NE 'PRINTER' THEN TVLCT, *self._rvalue, *self._gvalue, *self._bvalue, colorIndex
         RETURN, IndGen(ncolors) + colorIndex

      ENDIF ELSE BEGIN

         IF !D.Name NE 'PRINTER' THEN $
            TVLCT, (*self._rvalue)[theIndex], (*self._gvalue)[theIndex], (*self._bvalue)[theIndex], colorIndex
         RETURN, colorIndex

      ENDELSE


   ENDELSE

END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::GETPROPERTY
;
; PURPOSE:
;
;       This method obtains properties from the object.
;
; SYNTAX:
;
;       colorObject -> GetProperty, RED=r, GREEN=g, BLUE=b
;       TVLCT, r, g, b
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       BLUE:       The blue color vector.
;       
;       BREWER:      Is set, using Brewer Color Tables.
;
;       COLORPALETTE:  A n-by-3 array containing the current color table vectors
;
;       CTINDEX:    The current color table index number.
;
;       GREEN:      The green color vector.
;       
;       NCOLORS:    The number of colors.
;
;       RED:        The red color vector.
;
;       _REF_EXTRA: Any keyword appropriate for the GETPROPERTY method of the superclass object.
;
;-
;*****************************************************************************************************
PRO ColorTool::GetProperty, $
                RED=red, $
                GREEN=green, $
                BLUE=blue, $
                BOTTOM=bottom, $
                BREWER=brewer, $
                COLORPALETTE=colorPalette, $
                CTINDEX=ctindex, $
                NCOLORS=ncolors, $
                _Ref_Extra=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(bottom) THEN bottom = self._bottom
   IF Arg_Present(brewer) THEN brewer = self._brewer
   IF Arg_Present(ncolors) THEN ncolors = self._ncolors
   IF Arg_Present(red) THEN red = self._r
   IF Arg_Present(green) THEN green = self._g
   IF Arg_Present(blue) THEN blue = self._b
   IF Arg_Present(ctindex) THEN ctindex = self._ctindex
   IF Arg_Present(colorPalette) THEN BEGIN

      colorPalette = [[Congrid(self._r, self._ncolors)], $
                      [Congrid(self._g, self._ncolors)], $
                      [Congrid(self._b, self._ncolors)]]

   ENDIF


   self -> CATATOM::GetProperty, _Extra=extraKeywords

END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::GUI_UPDATE
;
; PURPOSE:
;
;       This method updates the colors in any COLORTOOL GUI that happens to
;       be open on the display.
;
; SYNTAX:
;
;       colorObject -> GUI_Update
;
; ARGUMENTS:
;
;       INFO:  The information structure used by the XCOLORS method.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO ColorTool::GUI_Update, info

   @cat_pro_error_handler

   ; Save the current graphics window.

   currentWindow = !D.Window

   ; If XCOLORS is open, load current vectors there, as well.

   IF Obj_Valid(self._xcTLB) THEN $
   BEGIN

      IF N_Elements(info) EQ 0 THEN self._xcTLB -> GetProperty, UValue=info

      ; Load the color table and display the color bar.
      TVLCT, info._rcurrent, info._gcurrent, info._bcurrent, self._bottom
      info._ctDraw -> SetWindow
      Device, Get_Decomposed=theState, Decomposed=0
      TV, info._ctbar
      Device, Decomposed=theState


   END

   ; If SHOWCOLORS is open, update it.

   IF Obj_Valid(self._scTLB) THEN $
   BEGIN
      self._scTLB -> GetProperty, UValue=scinfo
      scinfo._scdraw -> SetWindow
      Device, Get_Decomposed=theState, Decomposed=0
      TV, scinfo._image
      Device, Decomposed=theState
   ENDIF

   ; Restore the current graphics windows.

   IF WindowAvailable(currentWindow) THEN WSet, currentWindow

END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::LOADCOLOR
;
; PURPOSE:
;
;       This method loads a color value or values into the color table vectors.
;       All color vectors must be the same size.
;
; SYNTAX:
;
;       colorObject -> LoadColor, colorName
;
;       or,
;
;       colorObject -> LoadColor, r, g, b
;
; ARGUMENTS:
;
;       colorName: A scalar or vector of color names.
;
;       Or,
;
;       r:         A scalar or vector of red values.
;
;       g:         A scalar or vector of green values.
;
;       b:         A scalar or vector of blue values.
;
; KEYWORDS:
;
;       DRAW:  If this keyword is set, the DRAW method is called after the colors
;              have been loaded.
;
;       INDEX: If a scalar, the starting index for loading the specified color vectors.
;              IF a vector, the indices of the specified colors. Must be the same size
;              as the color vectors. If absent, then set to !D.Table_Size - (N_Elements(r) + 1).
;
; EXAMPLE:
;
;       colorObject -> LoadColor, 'red', Index=10
;       colorObject -> LoadColor, ['red','yellow'], Index=20
;       colorObject -> LoadColor, ['red','yellow'], Index=[30,35]
;       colorObject -> ShowColors
;
;       colorObject -> LoadColor, 255, 255, 0, Index=10
;       colorObject -> LoadColor, [255, 0], [255,255], [0,0], Index=20
;       colorObject -> LoadColor, [255, 0], [255,255], [0,0], Index=[30,35]
;       colorObject -> ShowColors
;
;-
;*****************************************************************************************************
PRO ColorTool::LoadColor, r, g, b, INDEX=index

   @cat_pro_error_handler

   IF N_Params() EQ 0 THEN Message, 'Positional parameters are required.'

      ; Store the current color table vectors for later restoration.

   self._r_old = self._r
   self._g_old = self._g
   self._b_old = self._b

   CASE N_Params() OF

      1: BEGIN

            ; Check color index.

            s = N_Elements(r)
            type = Size(r, /TNAME)
            IF type NE 'STRING' THEN Message, 'Augument must be a color name or an array of color names.'
            CASE N_Elements(index) OF
               0: index = Indgen(s) + (!D.Table_Size - (s + 1))
               1: index = Indgen(s) + index
               ELSE: IF N_Elements(index) NE s THEN $
                  Message, 'Index vector must be the same length as color name vector.'
            ENDCASE

            ; Load the colors.

            FOR j=0,s-1 DO BEGIN
               triple = self -> GetColor(r[j], /Triple)
               self._r[index[j]] = triple[0]
               self._g[index[j]] = triple[1]
               self._b[index[j]] = triple[2]
            ENDFOR
         ENDCASE

      2: Message, 'You may use 1 or 3 positional parameters, but not 2. Sorry.'

      3: BEGIN

            ; Make sure all color vectors are the same size.

            s = N_Elements(r)
            IF N_Elements(g) NE s THEN Message, 'Color vectors must be the same size.'
            IF N_Elements(b) NE s THEN Message, 'Color vectors must be the same size.'
            CASE N_Elements(index) OF
               0: index = Indgen(s) + (!D.Table_Size - (s + 1))
               1: index = Indgen(s) + index
               ELSE: IF N_Elements(index) NE s THEN $
                  Message, 'Index vector must be the same length as color vectors.'
            ENDCASE

            ; Load the colors.

            FOR j=0,s-1 DO BEGIN
               self._r[index[j]] = r[j]
               self._g[index[j]] = g[j]
               self._b[index[j]] = b[j]
            ENDFOR
         ENDCASE

   ENDCASE

   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::LOADCT
;
; PURPOSE:
;
;       This method loads a pre-determined color table.
;
; SYNTAX:
;
;       colorObject -> LoadCT, colorIndex
;
; ARGUMENTS:
;
;       colorIndex: The color table index number to load. If absent, 0 is assumed
;
; KEYWORDS:
;
;       DRAW:       If this keyword is set, the DRAW method is called after the colors
;                   have been loaded.
;-
;*****************************************************************************************************
PRO ColorTool::LoadCT, colorIndex, DRAW=draw


   @cat_pro_error_handler

   ; Check parameters.

   IF N_Elements(colorIndex) EQ 0 THEN colorIndex = 0

      ; Store the current color table vectors for later restoration.

   self._r_old = self._r
   self._g_old = self._g
   self._b_old = self._b
   self._ctindex = colorIndex

   ; Open the file.

   OpenR, lun, self._colorFile, /Get_Lun

   ; Number of tables in this file.

   ntables = 0B
   ReadU, lun, ntables
   colorIndex = 0 > colorIndex < (ntables-1)

   ; Read the correct table.

   tables = Assoc(lun, BytArr(256), 1)
   r = tables[colorIndex*3  ]
   g = tables[colorIndex*3+1]
   b = tables[colorIndex*3+2]
   Free_Lun, lun

   ; Do we need to interpolate?

   IF self._ncolors LT 256 THEN BEGIN
      cindex = (Lindgen(self._ncolors) * 255) / (self._ncolors-1)
      r = r[cindex]
      g = g[cindex]
      b = b[cindex]
   ENDIF

   ; Load color vectors.

   self._r[self._bottom:self._bottom+self._ncolors-1] = r
   self._g[self._bottom:self._bottom+self._ncolors-1] = g
   self._b[self._bottom:self._bottom+self._ncolors-1] = b
   IF Obj_Valid(self._xcTLB) THEN $
   BEGIN

      self._xcTLB -> GetProperty, UValue=info
      info._rcurrent = r
      info._gcurrent = g
      info._bcurrent = b

      ; Update the original colors.
      info._rorig = self._r
      info._gorig = self._g
      info._borig = self._b

      ; Changed info, so set it back for subsequent GUI_UPDATE method.
      self._xcTLB -> SetProperty, UValue=info

  ENDIF

   ; Do any of the GUIs need updating?

   self->GUI_Update

   self -> SendMessage, 'COLORTOOL_TABLECHANGE', DATA={r:r, g:g, b:b, bottom:self._bottom}

   ; Draw the colors?

   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed
END


PRO ColorTool::MessageHandler, title, SENDER=sender, DATA=data

   ; Initialise the error handler
   @cat_pro_error_handler

   CASE title OF

      'COLORTOOL_TABLECHANGE': BEGIN

            ; If the sender is not the self, then update the self's colors.
            IF sender NE self THEN $
               self -> SetProperty, Red=data.r, Green=data.g, Blue=data.b, Bottom=data.bottom
               
            ; Redraw the colors.
            self -> Draw

         ENDCASE


      'COLORTOOL_SETPROPERTY': BEGIN

            ; If the sender is not the self, then update the self's colors.
            IF sender NE self THEN BEGIN
               sender -> GetProperty, COLORPALETTE=colorpalette
               self -> SetProperty, COLORPALETTE=colorpalette
               
               ; Redraw the colors.
               self -> Draw
            ENDIF
            
         ENDCASE

      ELSE: self -> CATDATAATOM::MessageHandler, title, SENDER=sender, DATA=data


   ENDCASE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::PICKCOLORNAME
;
; PURPOSE:
;
;       This method allows the user to select a color name interactively.
;
; SYNTAX:
;
;       theColor = colorObject -> PickColorName()
;
; ARGUMENTS:
;
;       defaultColor: The name of the default color to be set to the "current color"
;                     If not supplied, the color "white" is used.
;
; RETURN_VALUE:
;
;       theColor:     The name of the selected color.
;
; KEYWORDS:
;
;       GROUP_LEADER: The group leader object or widget reference. The group leader MUST
;                     be passed to make this a modal widget. If absent, the program will
;                     try to block the IDL command line. But this is not guaranteed and
;                     is not recommended.
;
;       INDEX:        If this keyword is set to a color index, the selected color will
;                     be loaded at that location in the internal color table.
;
;       TITLE:        The title of the pop-up dialog widget.
;-
;*****************************************************************************************************

FUNCTION ColorTool::PickColorName, defaultColor, GROUP_LEADER=group_leader, INDEX=index, Title=title

   @cat_func_error_handler

   ; Default values.
   IF N_Elements(defaultColor) EQ 0 THEN defaultColor = 'White'
   IF N_Elements(title) EQ 0 THEN title = 'Select a Color'

      ; Create the widgets. TLB is MODAL or BLOCKING.

   self._selTLB = Obj_New('TOPLEVELBASE', Title=title, Column=1, /Base_Align_Center,  $
      Modal=Keyword_Set(group_leader), Group_Leader=group_leader, /CENTER)

   colorbaseID = Obj_New('BASEWIDGET', self._selTLB, Column=11)
   ncolors = N_Elements(*self._colorNames)
   drawID = ObjArr(ncolors)
   FOR j=0,ncolors-1 DO BEGIN
      drawID[j] = Obj_New('DRAWWIDGET', colorbaseID, XSize=20, YSize=15,  Event_Obj=self, $
         UValue=(*self._colornames)[j], Button_Events=1, Name='PICKCOLORNAME')
   ENDFOR
   currentID = Obj_New('BASEWIDGET', self._selTLB, Column=1, Base_Align_Center=1)
   colorlabelID = Obj_New('LABELWIDGET', currentID, Value=defaultColor, /Dynamic_Resize)
   currentColorID = Obj_New('DRAWWIDGET', currentID, XSize=60, YSize=15)

   buttonbase = Obj_New('BASEWIDGET', self._selTLB, ROW=1, Align_Center=1)
   cancelID = Obj_New('BUTTONWIDGET', buttonbase, VALUE='Cancel', Name='PICKCOLORNAME_CANCEL', $
      Event_Obj=self, UValue=defaultColor)
   acceptID = Obj_New('BUTTONWIDGET', buttonbase, VALUE='Accept', Name='PICKCOLORNAME_ACCEPT', $
      Event_Obj=self)

      ; Realize the TLB.

   self._selTLB -> Realize

      ; Load the drawing colors.

   Device, Decomposed=1, Get_Decomposed=theState
   colors = self -> GetColor(/All)
   FOR j=0, ncolors-1 DO BEGIN
      drawID[j] -> SetWindow
      Erase, Color=colors[j]
      PlotS, [0,0,19,19,0], [0,14,14,0,0], /Device, Color=self->GetColor('black')
   ENDFOR

      ; Load the current color.

   currentColorID -> SetWindow
   Erase, Color=self->GetColor(defaultColor)
   PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=self->GetColor('black')
   Device, Decomposed=theState

   ptr = Ptr_New(defaultColor)

      ; Get things rolling. Will block here. When destroyed, will return color.
      ; To insure block, use GROUP_LEADER and make a modal widget.

   self._selTLB -> SetProperty, UValue={currentColorID:currentColorID, ptr:ptr, $
        drawID:drawID, colorLabelID:colorLabelID}
   self._selTLB -> Draw, /Block

   selectedColor = *ptr
   Ptr_Free, ptr
   IF N_Elements(index) NE 0 THEN BEGIN
      thisColor = self -> GetColor(selectedColor, index, /TRIPLE)
      self._r[index] = thisColor[0]
      self._g[index] = thisColor[1]
      self._b[index] = thisColor[2]
      self -> Draw
      self -> GUI_Update
   ENDIF
   RETURN, selectedColor
END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::RESET
;
; PURPOSE:
;
;       This method allows resets the current color vectors to the old color vectors.
;
; SYNTAX:
;
;       colorObject -> Reset
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
PRO ColorTool::Reset

   @cat_pro_error_handler

   temp_r = self._r
   temp_g = self._g
   temp_b = self._b

   self._r = self._r_old
   self._g = self._g_old
   self._b = self._b_old

   self._r_old = temp_r
   self._g_old = temp_g
   self._b_old = temp_b

   self -> Draw

   ; Need a message sent?

   self -> SendMessage, 'COLORTOOL_TABLECHANGE', $
           DATA={r:self._r, g:self._g, b:self._b, bottom:self._bottom}
END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::REVERSE
;
; PURPOSE:
;
;       This method allows the current color vectors to be reversed.
;
; SYNTAX:
;
;       colorObject -> Reverse
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DRAW:  If this keyword is set, the DRAW method is called after the colors have been loaded.
;-
;*****************************************************************************************************

PRO ColorTool::Reverse,  DRAW=draw

   @cat_pro_error_handler

   ; Check parameters.

   IF N_Elements(bottom) EQ 0 THEN bottom = 0
   IF N_Elements(ncolors) EQ 0 THEN ncolors = !D.Table_Size

      ; Store the current color table vectors for later restoration.

   self._r_old = self._r
   self._g_old = self._g
   self._b_old = self._b

   ; Load color vectors.

   r = Reverse((self._r)[self._bottom:self._bottom+self._ncolors-1])
   g = Reverse((self._g)[self._bottom:self._bottom+self._ncolors-1])
   b = Reverse((self._b)[self._bottom:self._bottom+self._ncolors-1])
   self._r[self._bottom:self._bottom+self._ncolors-1] = r
   self._g[self._bottom:self._bottom+self._ncolors-1] = g
   self._b[self._bottom:self._bottom+self._ncolors-1] = b

   ; Draw the colors?

   IF Keyword_Set(draw) THEN self -> Draw

   ; Need a message sent?

   self -> SendMessage, 'COLORTOOL_TABLECHANGE', DATA={r:r, g:g, b:b, bottom:self._bottom}

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::SELECTCOLOR
;
; PURPOSE:
;
;       This method allows the user to select a color interactively.
;
; SYNTAX:
;
;       theColor = colorObject -> SelectColor()
;
; ARGUMENTS:
;
;       defaultColor: The name of the default color to be set to the "current color",
;                     or the RGB value (three-element array or 24-bit value) of the
;                     default color. If not supplied, the color "white" is used.
;
; RETURN_VALUE:
;
;       theColor:     A three-element array, representing the RGB values of the selected color.
;
; KEYWORDS:
;
;       GROUP_LEADER: The group leader object or widget reference. The group leader MUST
;                     be passed to make this a modal widget. If absent, the program will
;                     try to block the IDL command line. But this is not guaranteed and
;                     is not recommended.
;
;       INDEX:        If this keyword is set to a color index, the selected color will
;                     be loaded at that location in the internal color table.
;
;       TITLE:        The title of the pop-up dialog widget.
;-
;*****************************************************************************************************

FUNCTION ColorTool::SelectColor, defaultColor, GROUP_LEADER=group_leader, INDEX=index, Title=title

   @cat_func_error_handler

      ; Sort out defaultColor. Can be a string, a 3-element array, or a 24-bit value.

   IF N_Elements(defaultColor) EQ 0 THEN BEGIN
      defaultColor = 'White'
      defaultColorValue = self -> GetColor( defaultColor, /Triple, Row=1)
   ENDIF ELSE BEGIN
      type = Size(defaultColor, /TName)
      IF type EQ 'STRING' THEN defaultColorValue = self -> GetColor( defaultColor, /Triple, Row=1) ELSE BEGIN
         IF N_Elements(defaultColor) EQ 3 THEN defaultColorValue = defaultColor ELSE BEGIN
            defaultColorValue = [Byte(defaultColor), Byte(ISHFT(defaultColor, -8)), Byte(ISHFT(defaultColor, -16))]
         ENDELSE
      ENDELSE
   ENDELSE

   IF N_Elements(title) EQ 0 THEN title = 'Select a Color'

      ; Create the widgets. TLB is MODAL or BLOCKING.

   self._selTLB = Obj_New('TOPLEVELBASE', Title=title, Column=1, /Base_Align_Center,  $
      Modal=Keyword_Set(group_leader), Group_Leader=group_leader, /CENTER)

   colorbaseID = Obj_New('BASEWIDGET', self._selTLB, Column=11)
   ncolors = N_Elements(*self._colorNames)
   drawID = ObjArr(ncolors)
   FOR j=0,ncolors-1 DO BEGIN
      drawID[j] = Obj_New('DRAWWIDGET', colorbaseID, XSize=20, YSize=15,  Event_Obj=self, $
         UValue=(*self._colornames)[j], Button_Events=1, Name='SELECTCOLOR_PICKCOLOR')
   ENDFOR
   currentID = Obj_New('BASEWIDGET', self._selTLB, Column=1, Base_Align_Center=1)
   labelID = Obj_New('LABELWIDGET', currentID, Value='Current Color', /Dynamic_Resize)
   currentColorID = Obj_New('DRAWWIDGET', currentID, XSize=60, YSize=15)

   sliderbase = Obj_New('BASEWIDGET', self._selTLB, COLUMN=1, FRAME=1, BASE_ALIGN_CENTER=1, $
      EVENT_METHOD='PickColor_Sliders')
   label = Obj_New('LABELWIDGET', sliderbase, Value='Specify a Color')

      ; Set the current color values in sliders.

   redID = Obj_New('SLIDERWIDGET', sliderbase, Scr_XSize=200, Value=defaultColorValue[0], $
      Max=255, Min=0, Title='Red', Name='SELECTCOLOR_SLIDER', Event_Obj=self)
   greenID = Obj_New('SLIDERWIDGET', sliderbase, Scr_XSize=200, Value=defaultColorValue[1], $
      Max=255, Min=0, Title='Green', Name='SELECTCOLOR_SLIDER', Event_Obj=self)
   blueID = Obj_New('SLIDERWIDGET', sliderbase, Scr_XSize=200, Value=defaultColorValue[2], $
         Max=255, Min=0, Title='Blue', Name='SELECTCOLOR_SLIDER', Event_Obj=self)

   buttonbase = Obj_New('BASEWIDGET', self._selTLB, ROW=1, Align_Center=1)
   cancelID = Obj_New('BUTTONWIDGET', buttonbase, VALUE='Cancel', Name='SELECTCOLOR_CANCEL', $
      Event_Obj=self, UValue=defaultColorValue)
   acceptID = Obj_New('BUTTONWIDGET', buttonbase, VALUE='Accept', Name='SELECTCOLOR_ACCEPT', $
      Event_Obj=self)

      ; Realize the TLB.

   self._selTLB -> Realize

      ; Load the drawing colors.

   Device, Decomposed=1, Get_Decomposed=theState
   colors = self ->GetColor(/All)
   FOR j=0, ncolors-1 DO BEGIN
      drawID[j] -> SetWindow
      Erase, Color=colors[j]
      PlotS, [0,0,19,19,0], [0,14,14,0,0], /Device, Color=self->GetColor('black')
   ENDFOR

      ; Load the current color.

   currentColorID -> SetWindow
   Erase, Color=self->Color8to24(defaultColorValue)
   PlotS, [0,0,59,59,0], [0,14,14,0,0], /Device, Color=self->GetColor('black')
   Device, Decomposed=theState

   ptr = Ptr_New(defaultColorValue)

      ; Get things rolling. Will block here. When destroyed, will return color.
      ; To insure block, use GROUP_LEADER and make a modal widget.

   self._selTLB -> SetProperty, UValue={redID:redID, greenID:greenID, blueID:blueID, $
      currentColorID:currentColorID, ptr:ptr, drawID:drawID}
   self._selTLB -> Draw, /Block

   selectedColorValue = *ptr
   Ptr_Free, ptr
   IF N_Elements(index) NE 0 THEN BEGIN
      self._r[index] = selectedColorValue[0]
      self._g[index] = selectedColorValue[1]
      self._b[index] = selectedColorValue[2]
      self -> Draw
      self -> GUI_Update
   ENDIF
   RETURN, selectedColorValue
END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::SETPROPERTY
;
; PURPOSE:
;
;       This method allows properties of the object to be set.
;
; SYNTAX:
;
;       TVLCT, r, g, b, /GET
;       colorObject -> SetProperty, RED=r, GREEN=g, BLUE=b
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       BLUE:       The blue color vector.
;       
;       BREWER:     Use the brewer color tables.
;
;       BOTTOM:     The bottom color index.
;
;       COLORFILE:  The fully-qualified name of a color table file to open and display.
;
;       COLORPALETTE:  A n-by-3 array containing the current color table vectors
;
;       GREEN:      The green color vector.
;
;       INDEX:      The color table index number.
;
;       NCOLORS:    The number of colors in the color table.
;
;       RED:        The red color vector.
;
;       _EXTRA:     Any keyword appropriate for the SETPROPERTY method of the CATATOM object.
;
;-
;*****************************************************************************************************
PRO ColorTool::SetProperty, $
   BLUE=blue, $
   BREWER=brewer, $
   BOTTOM=bottom, $
   COLORFILE=colorFile, $
   COLORPALETTE=colorPalette, $
   GREEN=green, $
   INDEX=index, $
   NCOLORS=ncolors, $
   RED=red, $
    _Extra=extraKeywords

   @cat_pro_error_handler

   sendmessage = 0
   IF N_Elements(red) NE 0 THEN BEGIN
        self._r_old = self._r
        self._r = red
        sendmessage = 1
   ENDIF
   IF N_Elements(green) NE 0 THEN BEGIN
        self._g_old = self._g
        self._g = green
        sendmessage = 1
   ENDIF
   IF N_Elements(blue) NE 0 THEN BEGIN
        self._b_old = self._b
        self._b = blue
        sendmessage = 1
   ENDIF

   IF N_Elements(colorFile) NE 0 THEN BEGIN
      self._colorFile = colorFile
      self -> LoadCT, index
   ENDIF

   IF N_Elements(index) NE 0 AND N_Elements(colorFile) EQ 0 THEN self -> LoadCT, index
   IF N_Elements(brewer) NE 0 THEN self._brewer = brewer
   IF N_Elements(bottom) NE 0 THEN self._bottom = bottom
   IF N_Elements(ncolors) NE 0 THEN self._ncolors = ncolors
   IF (self._ncolors + self._bottom) GT 256 THEN self._ncolors = 256 - self._bottom

   IF N_Elements(colorPalette) NE 0 THEN BEGIN
      self._r = colorPalette[*,0]
      self._g = colorPalette[*,1]
      self._b = colorPalette[*,2]
      sendmessage = 1
   ENDIF

   IF N_Elements(extraKeywords) NE 0 THEN self -> CATATOM::SetProperty, _Extra=extraKeywords

   ; Need a message sent?
   IF sendmessage THEN self -> SendMessage, 'COLORTOOL_SETPROPERTY'

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::SHOWCOLORS
;
; PURPOSE:
;
;       The purpose of this program is to allow the user to see the colors currently
;       loaded in the color table vectors. A window with the current colors will open
;       on the display.
;
; SYNTAX:
;
;       colorObject -> ShowColors
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       TITLE: The title of the window.
;
;-
;*****************************************************************************************************
PRO ColorTool::ShowColors, Title=title

   @cat_pro_error_handler

   currentWindow = !D.Window
   IF N_Elements(title) EQ 0 THEN title = 'Color Table Indices'

      ; Make sure the colors are current.

   self -> Draw

      ; Create the color table display in the Z buffer.

   oldWindowID = !D.Window
   thisDevice = !D.Name
   Set_Plot, 'Z'
   Device, Set_Resolution=[496,400]

      ; Set the starting index for the polygons.

   xindex = 0
   yindex = 0

      ; Start drawing. There are 16 rows and 16 columns of colors.

   FOR i=0,15 DO BEGIN

       y = [yindex, yindex+25, yindex+25, yindex, yindex]
       yindex = yindex+25
       xindex = 0

       FOR j=0,15 DO BEGIN

           x = [xindex, xindex, xindex+31, xindex+31, xindex]
           color = j+(i*16)

              ; Draw the polygon in a specfic color.

           TV, Replicate(color,31,25), j*31, i*25
           output = StrTrim(j+(i*16), 2)

              ; Draw the index number in the "opposite" color.

           XYOutS, xindex+8, yindex-15, output, Color=Byte(255-color), $
              /Device, Charsize=0.75

              ; Reset the xindex number.

           xindex = xindex+31

       ENDFOR

   ENDFOR

      ; Take a snapshot of the Z-Buffer.

   snap = TVRD()

   Set_Plot, thisDevice

      ; If it is already on the display, just return.

   IF Obj_Valid(self._scTLB) THEN BEGIN
      self._scTLB ->GetProperty, UValue=info
      info._scDraw -> SetProperty, SHOW=1
      info._scDraw -> SetWindow
      Device, Decomposed=0, Get_Decomposed=theState
      TV, snap
      Device, Decomposed=theState
      info._image = snap
      self._scTLB ->SetProperty, UValue=info
   ENDIF ELSE BEGIN
      tlb = Obj_New('TOPLEVELBASE', Title=title, TLB_Frame_Attr=1)
      scdraw = Obj_New('DRAWWIDGET', tlb, XSize=496, YSize=400)

      tlb -> Draw, /Center
      scdraw -> SetWindow
      Device, Decomposed=0, Get_Decomposed=theState
      TV, snap
      Device, Decomposed=theState

      self._scTLB = tlb
      self._scTLB -> SetProperty, UValue={_scdraw:scdraw, _image:snap}
   ENDELSE

   IF WindowAvailable(currentWindow) THEN WSet, currentWindow

END



;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::STRETCH
;
; PURPOSE:
;
;       This method stretches the current colortable between minValue and maxValue.
;
; SYNTAX:
;
;       colorObject -> Stretch, minValue, maxValue
;
; ARGUMENTS:
;
;       minValue:    The minimum value of the stretch. 0 > minValue < maxValue < 256 .
;
;       maxValue:    The maximum value of the stretch. 0 > minValue > maxValue < 256.
;
; KEYWORDS:
;
;       DRAW:       If this keyword is set, the DRAW method is called after the colors have been loaded.
;-
;*****************************************************************************************************

PRO ColorTool::Stretch, minValue, maxValue,  DRAW=draw

   @cat_pro_error_handler

   ; Check parameters.

   IF N_Elements(bottom) EQ 0 THEN bottom = 0
   IF N_Elements(ncolors) EQ 0 THEN ncolors = !D.Table_Size

      ; Store the current color table vectors for later restoration.

   self._r_old = self._r
   self._g_old = self._g
   self._b_old = self._b

   ; Load color vectors.
   f = Scale_Vector([minValue, maxValue], MinValue=0, MaxValue=255, 0, 255)
   minv = f[0]
   maxv = f[1]

   rs = (self._r)[self._bottom:self._bottom+self._ncolors-1]
   gs = (self._g)[self._bottom:self._bottom+self._ncolors-1]
   bs = (self._b)[self._bottom:self._bottom+self._ncolors-1]

   rs[0:minv] = rs[0]
   rs[minv:maxv] = Congrid(rs, maxv-minv+1)
   rs[maxv:*] = rs[N_Elements(rs)-1]

   gs[0:minv] = gs[0]
   gs[minv:maxv] = Congrid(gs, maxv-minv+1)
   gs[maxv:*] = gs[N_Elements(rs)-1]

   bs[0:minv] = bs[0]
   bs[minv:maxv] = Congrid(bs, maxv-minv+1)
   bs[maxv:*] = bs[N_Elements(bs)-1]

   self._r[self._bottom:self._bottom+self._ncolors-1] = rs
   self._g[self._bottom:self._bottom+self._ncolors-1] = gs
   self._b[self._bottom:self._bottom+self._ncolors-1] = bs

   ; Draw the colors?

   IF Keyword_Set(draw) THEN self -> Draw

   ; Need a message sent?

   self -> SendMessage, 'COLORTOOL_TABLECHANGE', DATA={r:rs, g:gs, b:bs, bottom:self._bottom}

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::XCOLORS
;
; PURPOSE:
;
;       The purpose of this method is to interactively change color tables.
;
; SYNTAX:
;
;       colorObject -> XColors
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
; 
;       BREWER:        Set this keyword if you wish to use the Brewer Colors, as
;                      implemented by Mike Galloy in the file brewer.tbl, and implemented
;                      here as fsc_brewer.tbl. See these references:
;
;                      Brewer Colors: http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer_intro.html
;                      Mike Galloy Implementation: http://michaelgalloy.com/2007/10/30/colorbrewer.html
;
;                      This program will look first in the $IDL_DIR/resource/colors directory for 
;                      the color table file, and failing to find it there will look in the same 
;                      directory that the source code of this program is located, then in your IDL path. 
;                      Finally, if it still can't find the file, it will ask you to locate it.
;                      If you can't find it, the program will simply return without loading a color table.
;
;                      NOTE: YOU WILL HAVE TO DOWNLOAD THE FSC_BREWER.TBL FILE FROM THE COYOTE LIBRARY AND
;                      PLACE IT IN ONE OF THE THREE PLACES OUTLINED ABOVE:
;
;                      http://www.dfanning.com/programs/fsc_brewer.tbl
;
;       DRAG: Set this keyword if you want colors loaded as you drag
;          the sliders. Default is to update colors only when you release
;          the sliders. Applies only to UNIX computers.
;
;       GROUP_LEADER: The group leader for this program. When the group
;          leader is destroyed, this program will be destroyed.
;
;       MODAL: Set this keyword (along with the GROUP_LEADER keyword) to
;          make the XCOLORS dialog a modal widget dialog. Note that NO
;          other events can occur until the XCOLORS program is destroyed
;          when in modal mode.
;
;       TITLE: This is the window title. It is "Load Color Tables" by
;          default.
;
;       XOFFSET: This is the X offset of the program on the display. The
;          program will be placed approximately in the middle of the display
;          by default.
;
;       YOFFSET: This is the Y offset of the program on the display. The
;          program will be placed approximately in the middle of the display
;          by default.
;
;-
;*****************************************************************************************************

PRO ColorTool::XColors, $
    BREWER=brewer, $
    Drag=drag, $
    Group_Leader=groupLeader, $
    Modal=modal, $
    NColors=ncolors, $
    Title=title, $
    XOffset=xoffset, $
    YOffset=yoffset

   @cat_pro_error_handler

   currentWindow = !D.Window

      ; Check keywords.

   IF N_Elements(title) EQ 0 THEN title = 'Load Color Table'
   IF N_Elements(drag) EQ 0 THEN drag = 0
   IF N_Elements(ncolors) EQ 0 THEN ncolors = self._ncolors
   IF N_Elements(brewer) EQ 0 THEN brewer = self._brewer

   IF self._xc_registerName EQ "" THEN BEGIN
      self._xc_registerName = 'XCOLORS:' + StrCompress(Systime())
   ENDIF

      ; Only one XCOLORS with this title.

   IF XRegistered(self._xc_registerName) GT 0 THEN RETURN
   
   ; Try to locate the brewer file. Check resource/colors directory, then directory
   ; containing CTLOAD source code, then current directory, then ask, then give up.
   ; Look for either FSC_BREWER.TBL or BREWER.TBL.
    brewerfile = Filepath(SubDir=['resource','colors'], 'fsc_brewer.tbl')
    IF File_Test(brewerfile, /READ) EQ 0 THEN brewerfile = File_Which('fsc_brewer.tbl', INCLUDE_CURRENT_DIR=1)
    IF brewerfile EQ "" THEN BEGIN
        locatedBrewerFile = 0 
    ENDIF ELSE BEGIN
        locatedBrewerFile = 1
        IF Keyword_Set(brewer) THEN self._colorfile = brewerfile
    ENDELSE

   info = {   _ctbar: BytArr(256, 40), $     ; The color table image of the XCOLORS dialog.
              _ctDraw: Obj_New(), $          ; The draw widget of the XCOLORS dialog.
              _bottomSlider: Obj_New(), $    ; The bottom slider of the XCOLORS dialog.
              _topSlider: Obj_New(), $       ; The top slider of the XCOLORS dialog.
              _gammaSlider: Obj_New(), $     ; The gamma slider of the XCOLORS dialog.
              _gammaLabel: Obj_New(), $      ; The gamma label widget.
              _ctIndex: self._ctindex, $     ; The current color table index.
              _rorig: self._r, $             ; The original color vectors.
              _gorig: self._g, $             ; The original color vectors.
              _borig: self._b, $             ; The original color vectors.
              _cancelIndex: self._ctindex, $ ; The cancel index.
              _brewer: Keyword_Set(brewer), $
              _modal: Keyword_Set(modal), $
;              _ncolors: ncolors, $
              _title: title, $
              _drag: drag, $
              _rcancel: self._r[self._bottom:self._bottom + self._ncolors-1], $ ; The red color table vector to use for a XCOLORS cancel button.
              _gcancel: self._g[self._bottom:self._bottom + self._ncolors-1], $ ; The green color table vector to use for a XCOLORS cancel button.
              _bcancel: self._b[self._bottom:self._bottom + self._ncolors-1], $ ; The blue color table vector to use for a XCOLORS cancel button.
              _rcurrent: self._r[self._bottom:self._bottom + self._ncolors-1],$ ; The red color table vector to use for a XCOLORS accept button.
              _gcurrent: self._g[self._bottom:self._bottom + self._ncolors-1],$ ; The green color table vector to use for a XCOLORS accept button.
              _bcurrent: self._b[self._bottom:self._bottom + self._ncolors-1] $ ; The blue color table vector to use for a XCOLORS accept button.
          }

      ; Load the color table vectors so we start up with the current color table.

   self -> Draw

      ; Create the top-level base. No resizing.

   IF Keyword_Set(modal) AND N_Elements(groupLeader) NE 0 THEN BEGIN
      tlb = Obj_New('TOPLEVELBASE', Column=1, Title=title, TLB_Frame_Attr=1, $
         XOffSet=xoffset, YOffSet=yoffset, Base_Align_Center=1, Event_Object=self, $
         Modal=1, Group_Leader=groupLeader, NAME='XCOLORS_TLB')
   ENDIF ELSE BEGIN
      tlb = Obj_New('TOPLEVELBASE', Column=1, Title=title, TLB_Frame_Attr=1, NAME='XCOLORS_TLB', $
         XOffSet=xoffset, YOffSet=yoffset, Base_Align_Center=1, Group_Leader=groupLeader, $
         Event_Object=self, UVALUE=groupLeader)
   ENDELSE

      ; Create a draw widget to display the current colors.

   drawbase = Obj_New('BASEWIDGET', tlb, Column=1, Frame=1)
   info._ctDraw = Obj_New('DRAWWIDGET', drawbase, XSIZE=256, YSize=40)

      ; Create sliders to control stretchs and gamma correction.

   sliderbase = Obj_New('BASEWIDGET', tlb, Column=1, Frame=1)
   info._bottomSlider = Obj_New('SLIDERWIDGET', sliderbase, Value=0, Min=0, $
      Max=self._ncolors-1, XSize=256, Event_Object=self, Name='XCOLORS_BOTTOM_SLIDER', $
      Title='Stretch Bottom', Drag=drag)
   info._topSlider = Obj_New('SLIDERWIDGET', sliderbase, Value=self._ncolors-1, Min=0, $
      Max=self._ncolors-1, XSize=256, Event_Object=self, Name='XCOLORS_TOP_SLIDER', $
      Title='Stretch Top', Drag=drag)
   info._gammaLabel = Obj_New('LABELWIDGET', sliderbase, Value=String(1.0, Format='(F6.3)'), /DYNAMIC_RESIZE)
   info._gammaSlider = Obj_New('SLIDERWIDGET', sliderbase, Value=50.0, Min=0, Max=100, $
      Drag=drag, XSize=256, /Suppress_Value, Event_Object=self, Name='XCOLORS_GAMMA_SLIDER', $
      Title='Gamma Correction')

   button = Obj_New('BUTTONWIDGET', sliderbase, Value='Reverse Colors', Event_Object=self, $
      Name='XCOLORS_REVERSE_COLORS')

   ; A row for additional control.
   IF locatedBrewerFile THEN BEGIN
        colorTypeBase = Obj_New('BASEWIDGET', tlb, ROW=1, BASE_ALIGN_CENTER=1, NAME='COLORTYPEBASE')
        colorType = Obj_New('DROPLISTWIDGET', colorTypeBase, Value=[' IDL Colors ', ' Brewer Colors '], $
            /DYNAMIC_RESIZE, UVALUE=['IDL','BREWER'], NAME='XCOLORS_SWITCH_COLORTYPE') 
            IF Keyword_Set(Brewer) THEN colorType -> SetProperty, Select=' Brewer Colors '
    ENDIF 

   
      ; Get the colortable names for the list widget.

   colorNames=''
   LoadCT, Get_Names=colorNames, File=self._colorfile
   colorNamesIndex = StrArr(N_Elements(colorNames))
   FOR j=0,N_Elements(colorNames)-1 DO $
      colorNamesIndex[j] = StrTrim(j,2) + ' - ' + colorNames[j]
   tableListBase = Obj_New('BASEWIDGET', tlb, XPAD=0, YPAD=0)
   list = Obj_New('LISTWIDGET', tableListBase, Value=colorNamesIndex, YSize=15, Scr_XSize=256, $
      Event_Object=self, Name='XCOLORS_COLORTABLE_SELECT', $
      Index=info._ctIndex, UVALUE=colorNamesIndex)
    list -> SetProperty, SELECT=info._ctIndex  
    

      ; Dialog Buttons

   dialogbase = Obj_New('BASEWIDGET', tlb, Row=1)
   cancel = Obj_New('BUTTONWIDGET', dialogbase, Value='Cancel', $
      Event_Object=self, Name='XCOLORS_CANCEL_BUTTON')
   dismiss = Obj_New('BUTTONWIDGET', dialogbase, Value='Accept', $
      Event_Object=self, Name='XCOLORS_ACCEPT_BUTTON')

      ; Display the dialog.

   Device, Get_Screen_Size=theScreen
   IF (Keyword_Set(xoffset) OR Keyword_Set(yoffset)) THEN $
   BEGIN
      IF N_Elements(xoffset) EQ 0 THEN xoffset = 100
      IF N_Elements(yoffset) EQ 0 THEN yoffset = 100
      tlb -> Position, xoffset, yoffset, NoCenter=1, /DEVICE
   ENDIF $
   ELSE tlb -> Position

   tlb -> Draw, REGISTER_NAME=self._xc_registerName
   self._xcTLB = tlb

      ; Put a picture of the color table in the window.

   bar = BINDGEN(ncolors) # REPLICATE(1B, 10)
   bar = BYTSCL(bar, TOP=ncolors-1) + self._bottom
   info._ctbar = Congrid(bar, 256, 40, /INTERP)
   info._ctDraw -> SetWindow
   Device, Get_Decomposed=theState, Decomposed=0
   TV, info._ctbar
   Device, Decomposed=theState
   self._name = 'COLORTOOL'
   tlb -> SetProperty, UValue=info
   IF WindowAvailable(currentWindow) THEN WSet, currentWindow

END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::XCOLORS_UPDATE
;
; PURPOSE:
;
;       This method updates the color tables for the XCOLORS method.
;
; SYNTAX:
;
;       Called from the EVENT_HANDLER method when events occur in the XCOLORS method interface.
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
PRO ColorTool::XColors_Update, info

   @cat_pro_error_handler

   ; Stretch color tables.
   top = self._ncolors-1+self._bottom
   info._rcurrent = info._rorig[self._bottom:top]
   info._gcurrent = info._gorig[self._bottom:top]
   info._bcurrent = info._borig[self._bottom:top]
   info._bottomSlider -> GetProperty, Value=sliderBottom
   info._topSlider -> GetProperty, Value=sliderTop

   ; Set bottom and top colors.
   info._rcurrent[0:sliderBottom] = info._rcurrent[0]
   info._gcurrent[0:sliderBottom] = info._gcurrent[0]
   info._bcurrent[0:sliderBottom] = info._bcurrent[0]
   top = N_Elements(info._rcurrent)-1
   info._rcurrent[sliderTop:top] = info._rcurrent[top]
   info._gcurrent[sliderTop:top] = info._gcurrent[top]
   info._bcurrent[sliderTop:top] = info._bcurrent[top]

   ; Get original vectors.
   red   = info._rorig
   green = info._gorig
   blue  = info._borig

   ; Calculate the current gamma function.
   info._gammaSlider -> GetProperty, Value=gamma
   realgamma = 10^((gamma/50.0) - 1)
   index = Findgen(self._ncolors)
   distribution = index^realgamma > 10e-6
   index = Round(distribution * (self._ncolors-1) / (Max(distribution) > 10e-6))
   number = sliderTop - sliderBottom + 1
   info._rcurrent[sliderBottom:sliderTop] = self -> Congrid(red[index], number, /Minus_One)
   info._gcurrent[sliderBottom:sliderTop] = self -> Congrid(green[index], number, /Minus_One)
   info._bcurrent[sliderBottom:sliderTop] = self -> Congrid(blue[index], number, /Minus_One)

   ; Update the original color vectors.
   self._r[self._bottom:self._bottom+self._ncolors-1] = info._rcurrent
   self._g[self._bottom:self._bottom+self._ncolors-1] = info._gcurrent
   self._b[self._bottom:self._bottom+self._ncolors-1] = info._bcurrent

   self -> GUI_Update, info

END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::CLEANUP
;
; PURPOSE:
;
;       This is the COLORTOOL object class destructor method.
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
;       None.
;-
;*****************************************************************************************************
PRO ColorTool::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self._colornames
   Ptr_Free, self._rvalue
   Ptr_Free, self._gvalue
   Ptr_Free, self._bvalue
   self -> CATATOM::CLEANUP

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       COLORTOOL::INIT
;
; PURPOSE:
;
;       This method initiates the COLORTOOL object.
;
; SYNTAX:
;
;       colorObject Obj_New('ColorTool')
;
; ARGUMENTS:
;
;       colorIndex: The color table index number to load. A number between 0 and 40. If
;                   missing, 0 is assumed.
;
; KEYWORDS:
;
;       BREWER:       Set this keyword to load the Brewer color tables. 
;
;                     This program will look first in the $IDL_DIR/resource/colors directory for 
;                     the color table file, and failing to find it there will look in the same 
;                     directory that the source code of this program is located, then in the current 
;                     directory. Finally, if it still can't find the file, it will ask you to locate it.
;                     If you can't find it, the program will simply return without loading a color table.
;
;                     NOTE: YOU WILL HAVE TO DOWNLOAD THE FSC_BREWER.TBL FILE FROM THE COYOTE LIBRARY AND
;                     PLACE IT IN ONE OF THE THREE PLACES OUTLINED ABOVE:
;
;                     http://www.dfanning.com/programs/fsc_brewer.tbl
;
;       BOTTOM:       The starting location where the color table is loaded.
;
;       COLORFILE:    The fully qualified file name of the file containing color tables. By default,
;                     colorFile = Filepath(SubDirectory=['resource','colors'], 'colors1.tbl')
;
;       COLORPALETTE: An n-by-3 byte array containing the color palette to load.
;
;       NCOLORS:      The number of colors in the color palette.
;
;-
;*****************************************************************************************************
FUNCTION ColorTool::INIT, colorIndex, $
   Brewer=brewer, $
   Bottom=bottom,$
   ColorFile=colorFile, $
   ColorPalette=colorPalette, $
   NColors=ncolors, $
   _Extra=extrakeywords

   @cat_func_error_handler
   
   ; Try to locate the brewer file. Check resource/colors directory, then directory
   ; containing CTLOAD source code, then current directory, then ask, then give up.
   ; Look for either FSC_BREWER.TBL or BREWER.TBL.
   ; Try to locate the brewer file. Check resource/colors directory, then directory
   ; containing CTLOAD source code, then current directory, then ask, then give up.
   ; Look for either FSC_BREWER.TBL or BREWER.TBL.
    brewerfile = Filepath(SubDir=['resource','colors'], 'fsc_brewer.tbl')
    IF File_Test(brewerfile, /READ) EQ 0 THEN brewerfile = File_Which('fsc_brewer.tbl', INCLUDE_CURRENT_DIR=1)
    IF brewerfile EQ "" THEN BEGIN
        locatedBrewerFile = 0 
    ENDIF ELSE BEGIN
        locatedBrewerFile = 1
        IF Keyword_Set(brewer) THEN file = brewerfile
    ENDELSE
   IF locatedBrewerFile AND Keyword_Set(brewer) THEN colorFile = file
   
   ; Check color table index parameter.
   IF N_Elements(index) EQ 0 THEN index = 0
   IF N_Elements(colorFile) EQ 0 THEN colorFile = Filepath(SubDirectory=['resource','colors'], 'colors1.tbl')
   IF N_Elements(bottom) EQ 0 THEN bottom = 0
   IF N_Elements(ncolors) EQ 0 THEN ncolors = (256 < !D.Table_Size) - bottom
   IF (ncolors + bottom) GT 256 THEN ncolors = 256 - bottom

      ; Fill current colors.

   self._r = Bindgen(256)
   self._g = Bindgen(256)
   self._b = Bindgen(256)

      ; Set up the known color vectors and names.

   ; Set up the color vectors.
   colors= ['White']
   rvalue = [ 255]
   gvalue = [ 255]
   bvalue = [ 255]
   colors = [ colors,   'Snow',     'Ivory','Light Yellow', 'Cornsilk',     'Beige',  'Seashell' ]
   rvalue = [ rvalue,     255,         255,       255,          255,          245,        255 ]
   gvalue = [ gvalue,     250,         255,       255,          248,          245,        245 ]
   bvalue = [ bvalue,     250,         240,       224,          220,          220,        238 ]
   colors = [ colors,   'Linen','Antique White','Papaya',     'Almond',     'Bisque',  'Moccasin' ]
   rvalue = [ rvalue,     250,        250,        255,          255,          255,          255 ]
   gvalue = [ gvalue,     240,        235,        239,          235,          228,          228 ]
   bvalue = [ bvalue,     230,        215,        213,          205,          196,          181 ]
   colors = [ colors,   'Wheat',  'Burlywood',    'Tan', 'Light Gray',   'Lavender','Medium Gray' ]
   rvalue = [ rvalue,     245,        222,          210,      230,          230,         210 ]
   gvalue = [ gvalue,     222,        184,          180,      230,          230,         210 ]
   bvalue = [ bvalue,     179,        135,          140,      230,          250,         210 ]
   colors = [ colors,  'Gray', 'Slate Gray',  'Dark Gray',  'Charcoal',   'Black',  'Honeydew', 'Light Cyan' ]
   rvalue = [ rvalue,      190,      112,          110,          70,         0,         240,          224 ]
   gvalue = [ gvalue,      190,      128,          110,          70,         0,         255,          255 ]
   bvalue = [ bvalue,      190,      144,          110,          70,         0,         255,          240 ]
   colors = [ colors,'Powder Blue',  'Sky Blue', 'Cornflower Blue', 'Cadet Blue', 'Steel Blue','Dodger Blue', 'Royal Blue',  'Blue' ]
   rvalue = [ rvalue,     176,          135,          100,              95,            70,           30,           65,            0 ]
   gvalue = [ gvalue,     224,          206,          149,             158,           130,          144,          105,            0 ]
   bvalue = [ bvalue,     230,          235,          237,             160,           180,          255,          225,          255 ]
   colors = [ colors,  'Navy', 'Pale Green','Aquamarine','Spring Green',  'Cyan' ]
   rvalue = [ rvalue,        0,     152,          127,          0,            0 ]
   gvalue = [ gvalue,        0,     251,          255,        250,          255 ]
   bvalue = [ bvalue,      128,     152,          212,        154,          255 ]
   colors = [ colors, 'Turquoise', 'Light Sea Green', 'Sea Green','Forest Green',  'Teal','Green Yellow','Chartreuse', 'Lawn Green' ]
   rvalue = [ rvalue,      64,          143,               46,          34,             0,      173,           127,         124 ]
   gvalue = [ gvalue,     224,          188,              139,         139,           128,      255,           255,         252 ]
   bvalue = [ bvalue,     208,          143,               87,          34,           128,       47,             0,           0 ]
   colors = [ colors, 'Green', 'Lime Green', 'Olive Drab',  'Olive','Dark Green','Pale Goldenrod']
   rvalue = [ rvalue,      0,        50,          107,        85,            0,          238 ]
   gvalue = [ gvalue,    255,       205,          142,       107,          100,          232 ]
   bvalue = [ bvalue,      0,        50,           35,        47,            0,          170 ]
   colors = [ colors,     'Khaki', 'Dark Khaki', 'Yellow',  'Gold', 'Goldenrod','Dark Goldenrod']
   rvalue = [ rvalue,        240,       189,        255,      255,      218,          184 ]
   gvalue = [ gvalue,        230,       183,        255,      215,      165,          134 ]
   bvalue = [ bvalue,        140,       107,          0,        0,       32,           11 ]
   colors = [ colors,'Saddle Brown',  'Rose',   'Pink', 'Rosy Brown','Sandy Brown', 'Peru']
   rvalue = [ rvalue,     139,          255,      255,        188,        244,        205 ]
   gvalue = [ gvalue,      69,          228,      192,        143,        164,        133 ]
   bvalue = [ bvalue,      19,          225,      203,        143,         96,         63 ]
   colors = [ colors,'Indian Red',  'Chocolate',  'Sienna','Dark Salmon',   'Salmon','Light Salmon' ]
   rvalue = [ rvalue,    205,          210,          160,        233,          250,       255 ]
   gvalue = [ gvalue,     92,          105,           82,        150,          128,       160 ]
   bvalue = [ bvalue,     92,           30,           45,        122,          114,       122 ]
   colors = [ colors,  'Orange',      'Coral', 'Light Coral',  'Firebrick', 'Dark Red', 'Brown',  'Hot Pink' ]
   rvalue = [ rvalue,       255,         255,        240,          178,        139,       165,        255 ]
   gvalue = [ gvalue,       165,         127,        128,           34,          0,        42,        105 ]
   bvalue = [ bvalue,         0,          80,        128,           34,          0,        42,        180 ]
   colors = [ colors, 'Deep Pink',    'Magenta',   'Tomato', 'Orange Red',   'Red', 'Crimson', 'Violet Red' ]
   rvalue = [ rvalue,      255,          255,        255,        255,          255,      220,        208 ]
   gvalue = [ gvalue,       20,            0,         99,         69,            0,       20,         32 ]
   bvalue = [ bvalue,      147,          255,         71,          0,            0,       60,        144 ]
   colors = [ colors,    'Maroon',    'Thistle',       'Plum',     'Violet',    'Orchid','Medium Orchid']
   rvalue = [ rvalue,       176,          216,          221,          238,         218,        186 ]
   gvalue = [ gvalue,        48,          191,          160,          130,         112,         85 ]
   bvalue = [ bvalue,        96,          216,          221,          238,         214,        211 ]
   colors = [ colors,'Dark Orchid','Blue Violet',  'Purple']
   rvalue = [ rvalue,      153,          138,       160]
   gvalue = [ gvalue,       50,           43,        32]
   bvalue = [ bvalue,      204,          226,       240]
   colors = [ colors, 'Slate Blue',  'Dark Slate Blue']
   rvalue = [ rvalue,      106,            72]
   gvalue = [ gvalue,       90,            61]
   bvalue = [ bvalue,      205,           139]
           colors = [ colors, 'WT1', 'WT2', 'WT3', 'WT4', 'WT5', 'WT6', 'WT7', 'WT8']
           rvalue = [ rvalue,  255,   255,   255,   255,   255,   245,   255,   250 ]
           gvalue = [ gvalue,  255,   250,   255,   255,   248,   245,   245,   240 ]
           bvalue = [ bvalue,  255,   250,   240,   224,   220,   220,   238,   230 ]
           colors = [ colors, 'TAN1', 'TAN2', 'TAN3', 'TAN4', 'TAN5', 'TAN6', 'TAN7', 'TAN8']
           rvalue = [ rvalue,   250,   255,    255,    255,    255,    245,    222,    210 ]
           gvalue = [ gvalue,   235,   239,    235,    228,    228,    222,    184,    180 ]
           bvalue = [ bvalue,   215,   213,    205,    196,    181,    179,    135,    140 ]
           colors = [ colors, 'BLK1', 'BLK2', 'BLK3', 'BLK4', 'BLK5', 'BLK6', 'BLK7', 'BLK8']
           rvalue = [ rvalue,   250,   230,    210,    190,    112,     110,    70,       0 ]
           gvalue = [ gvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
           bvalue = [ bvalue,   250,   230,    210,    190,    128,     110,    70,       0 ]
           colors = [ colors, 'GRN1', 'GRN2', 'GRN3', 'GRN4', 'GRN5', 'GRN6', 'GRN7', 'GRN8']
           rvalue = [ rvalue,   250,   223,    173,    109,     53,     35,      0,       0 ]
           gvalue = [ gvalue,   253,   242,    221,    193,    156,     132,    97,      69 ]
           bvalue = [ bvalue,   202,   167,    142,    115,     83,      67,    52,      41 ]
           colors = [ colors, 'BLU1', 'BLU2', 'BLU3', 'BLU4', 'BLU5', 'BLU6', 'BLU7', 'BLU8']
           rvalue = [ rvalue,   232,   202,    158,     99,     53,     33,      8,       8 ]
           gvalue = [ gvalue,   241,   222,    202,    168,    133,    113,     75,      48 ]
           bvalue = [ bvalue,   250,   240,    225,    211,    191,    181,    147,     107 ]
           colors = [ colors, 'ORG1', 'ORG2', 'ORG3', 'ORG4', 'ORG5', 'ORG6', 'ORG7', 'ORG8']
           rvalue = [ rvalue,   254,    253,    253,    250,    231,    217,    159,    127 ]
           gvalue = [ gvalue,   236,    212,    174,    134,     92,     72,     51,     39 ]
           bvalue = [ bvalue,   217,    171,    107,     52,     12,      1,      3,      4 ]
           colors = [ colors, 'RED1', 'RED2', 'RED3', 'RED4', 'RED5', 'RED6', 'RED7', 'RED8']
           rvalue = [ rvalue,   254,    252,    252,    248,    225,    203,    154,    103 ]
           gvalue = [ gvalue,   232,    194,    146,     97,     45,     24,     12,      0 ]
           bvalue = [ bvalue,   222,    171,    114,     68,     38,     29,     19,     13 ]
           colors = [ colors, 'PUR1', 'PUR2', 'PUR3', 'PUR4', 'PUR5', 'PUR6', 'PUR7', 'PUR8']
           rvalue = [ rvalue,   244,    222,    188,    152,    119,    106,     80,     63 ]
           gvalue = [ gvalue,   242,    221,    189,    148,    108,     82,     32,      0 ]
           bvalue = [ bvalue,   248,    237,    220,    197,    177,    163,    139,    125 ]
           colors = [ colors, 'PBG1', 'PBG2', 'PBG3', 'PBG4', 'PBG5', 'PBG6', 'PBG7', 'PBG8']
           rvalue = [ rvalue,   243,    213,    166,     94,     34,      3,      1,      1 ]
           gvalue = [ gvalue,   234,    212,    189,    164,    138,    129,    101,     70 ]
           bvalue = [ bvalue,   244,    232,    219,    204,    171,    139,     82,     54 ]
           colors = [ colors, 'YGB1', 'YGB2', 'YGB3', 'YGB4', 'YGB5', 'YGB6', 'YGB7', 'YGB8']
           rvalue = [ rvalue,   244,    206,    127,     58,     30,     33,     32,      8 ]
           gvalue = [ gvalue,   250,    236,    205,    175,    125,     95,     48,     29 ]
           bvalue = [ bvalue,   193,    179,    186,    195,    182,    168,    137,     88 ]
           colors = [ colors, 'RYB1', 'RYB2', 'RYB3', 'RYB4', 'RYB5', 'RYB6', 'RYB7', 'RYB8']
           rvalue = [ rvalue,   201,    245,    253,    251,    228,    193,    114,     59 ]
           gvalue = [ gvalue,    35,    121,    206,    253,    244,    228,    171,     85 ]
           bvalue = [ bvalue,    38,    72,     127,    197,    239,    239,    207,    164 ]
           colors = [ colors, 'TG1', 'TG2', 'TG3', 'TG4', 'TG5', 'TG6', 'TG7', 'TG8']
           rvalue = [ rvalue,  84,    163,   197,   220,   105,    51,    13,     0 ]
           gvalue = [ gvalue,  48,    103,   141,   188,   188,   149,   113,    81 ]
           bvalue = [ bvalue,   5,     26,    60,   118,   177,   141,   105,    71 ]

      ; Add system color names for IDL version 5.6 and higher.

   IF Float(!Version.Release) GE 5.6 THEN BEGIN

      tlb = Widget_Base()
      sc = Widget_Info(tlb, /System_Colors)
      Widget_Control, tlb, /Destroy
      frame = sc.window_frame
      text = sc.window_text
      active = sc.active_border
      shadow = sc.shadow_3d
      highlight = sc.light_3d
      edge = sc.light_edge_3d
      selected = sc.highlight
      face = sc.face_3d
      colors  = [colors,  'Frame',  'Text',  'Active',  'Shadow']
      rvalue =  [rvalue,   frame[0], text[0], active[0], shadow[0]]
      gvalue =  [gvalue,   frame[1], text[1], active[1], shadow[1]]
      bvalue =  [bvalue,   frame[2], text[2], active[2], shadow[2]]
      colors  = [colors,  'Highlight',  'Edge',  'Selected',  'Face']
      rvalue =  [rvalue,   highlight[0], edge[0], selected[0], face[0]]
      gvalue =  [gvalue,   highlight[1], edge[1], selected[1], face[1]]
      bvalue =  [bvalue,   highlight[2], edge[2], selected[2], face[2]]

   ENDIF

   self._colornames = Ptr_New(StrUpCase(StrCompress(StrTrim(colors,2), /Remove_All)))
   self._rvalue = Ptr_New(rvalue)
   self._gvalue = Ptr_New(gvalue)
   self._bvalue = Ptr_New(bvalue)
   self._colorFile = colorFile
   self._bottom = bottom
   self._ncolors = ncolors

      ; Load a color table.

   self -> LoadCT, colorIndex
   IF locatedBrewerFile THEN self._brewer = Keyword_Set(brewer)

   ; If a color palette has been provided. Load that now.
   IF N_Elements(colorPalette) NE 0 THEN BEGIN
      self._r = colorPalette[*,0]
      self._g = colorPalette[*,1]
      self._b = colorPalette[*,2]
   ENDIF

   IF self -> CATATOM::INIT(_Extra=extrakeywords) EQ 0 THEN RETURN, 0

   self -> Report, /Completed

   RETURN, 1
END

;*****************************************************************************************************
;
; NAME:
;       COLORTOOL CLASS DEFINITION
;
; PURPOSE:
;
;       This is the COLORTOOL object's structure definition.
;
;*****************************************************************************************************
PRO ColorTool__Define, class

   class  = { COLORTOOL, $               ; The COLORTOOL object definition.
              INHERITS CATATOM, $        ; Inherits the CATATOM object class.
              _brewer: 0B, $             ; Flag that indicates a brewer color table.
              _r: BytArr(256), $         ; The red color table vector.
              _g: BytArr(256), $         ; The green color table vector.
              _b: BytArr(256),  $        ; The blue color table vector.
              _colornames: Ptr_New(), $  ; The names of "known" colors.
              _rvalue: Ptr_New(), $      ; The red color values associated with color names.
              _gvalue: Ptr_New(), $      ; The red color values associated with color names.
              _bvalue: Ptr_New(), $      ; The red color values associated with color names.
              _colorfile: "", $          ; The name of the color table file to read.
              _ncolors: 0L, $            ; The number of colors in standard color table.
              _bottom: 0L, $             ; The starting index of the standard color table.
              _scTLB: Obj_New(), $       ; The SHOWCOLORS tlb object.
              _selTLB: Obj_New(), $      ; The SELECTCOLORS tlb object.
              _xcTLB: Obj_New(), $       ; The XCOLORS tlb object.
              _xc_registerName: "", $    ; The name with which the XCOLORS dialog is registered.
              _ctindex: 0L, $            ; The current color table index.
              _r_old: BytArr(256), $     ; The old red color table vector.
              _g_old: BytArr(256), $     ; The old green color table vector.
              _b_old: BytArr(256) $      ; The old blue color table vector.
             }
END

;*****************************************************************************************************
;
; NAME:
;       COLORTOOL_Example
;
; DESCRIPTION:
;
;       This is an example program that illustrates how to use the color tool
;       to constuct a color table. The color table will have red-terperature
;       image colors in color indices 0 to 199, and the five drawing colors
;       white, charcoal, red, yellow, and green in indices 224 through 229.
;
; SYNTAX:
;
;       IDL> .compile colorpicker__define
;       IDL> colortool_example
;
;*****************************************************************************************************

PRO colortool_example

   ; Create the colortool object. Load color table 0 to start.

colortool = Obj_New('colortool', 0, NColors=200)

   ; Load image drawing colors.

colortool -> LoadCT, 3

   ; Load drawing colors.

colortool -> LoadColor, ['white', 'charcoal', 'red', 'yellow', 'green'], Index=224
colortool -> Stretch, 0, 50

   ; Show the current color table.

colortool -> ShowColors

   ; Destroy the colortool object.

Obj_Destroy, colortool

END