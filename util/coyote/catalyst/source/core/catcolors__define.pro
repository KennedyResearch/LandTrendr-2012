;*****************************************************************************************************
;+
; NAME:
;       CATCOLORS::ADDCOLOR
;
; PURPOSE:
;
;       This method adds (or replaces) a color of a given name in the object's internal named
;       color vectors.
;
; SYNTAX:
;
;       colorObject -> colorName, colorTriple
;
; ARGUMENTS:
;
;       colorName:     A string variable that indicates the new color's name.
;
;       colorTriple:   A three-element array, giving the new color's RGB values.
;
; KEYWORDS:
;
;       REPLACE:       If this keyword is set, the color with this name is replaced
;                      in the object's internal named color vectors.
;-
;*****************************************************************************************************
PRO CatColors::AddColor, colorName, colorTriple, REPLACE=replace

   @cat_pro_error_handler

   ; Check arguments.
   IF N_Params() EQ 0 THEN Message, 'Must pass arguments to AddColor method: "colorName" and "colorTriple".'
   IF N_Params() NE 2 THEN Message, 'AddColor method requires two arguments: "colorName" and "colorTriple".'
   IF Size(colorName, /TNAME) NE 'STRING' THEN Message, 'The "colorName" argument must be a string variable.'
   IF Size(colorTriple, /DIMENSIONS) NE 3 THEN Message, 'The "colorTriple" argument must be a three-element array.'

   ; Make sure the name is a unique name. If it is not, ask if the user wants
   ; to replace this color in the color vectors.
   colorIndex = Where(StrUpCase(*self._colornames) EQ StrUpCase(colorName), count)
   IF count EQ 0 THEN BEGIN

      IF Keyword_Set(replace) THEN BEGIN
       answer = Dialog_Message(/QUESTION, ['Color name "' + colorName + '" does not exist currently.', $
                                           'Do you wish to add this color to the object?'])
       IF StrUpCase(answer) EQ 'YES' THEN BEGIN
          *self._colornames = [ Temporary(*self._colornames), colorName]
          *self._rvalue = [ Temporary(*self._rvalue), colorTriple[0]]
          *self._gvalue = [ Temporary(*self._gvalue), colorTriple[1]]
          *self._bvalue = [ Temporary(*self._bvalue), colorTriple[2]]
       ENDIF

      ENDIF ELSE BEGIN
          *self._colornames = [ Temporary(*self._colornames), colorName]
          *self._rvalue = [ Temporary(*self._rvalue), colorTriple[0]]
          *self._gvalue = [ Temporary(*self._gvalue), colorTriple[1]]
          *self._bvalue = [ Temporary(*self._bvalue), colorTriple[2]]
      ENDELSE

   ENDIF ELSE BEGIN

       IF Keyword_Set(replace) THEN BEGIN
           (*self._rvalue)[colorIndex[0]] = colorTriple[0]
           (*self._gvalue)[colorIndex[0]] = colorTriple[1]
           (*self._bvalue)[colorIndex[0]] = colorTriple[2]
       ENDIF ELSE BEGIN

           answer = Dialog_Message(/QUESTION, ['Color name "' + colorName + '" already exists. Do you', $
                                               'wish to replace this color name with new color values?'])
           IF StrUpCase(answer) NE 'NO' THEN BEGIN
                (*self._rvalue)[colorIndex[0]] = colorTriple[0]
                (*self._gvalue)[colorIndex[0]] = colorTriple[1]
                (*self._bvalue)[colorIndex[0]] = colorTriple[2]
           ENDIF
       ENDELSE

   ENDELSE
END



;*****************************************************************************************************
;+
; NAME:
;       CATCOLORS::CONGRID
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
Function CatColors::Congrid, arr, x, y, z, INTERP=int, MINUS_ONE=m1, CUBIC = cubic

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
;       CATCOLORS::DRAW
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
PRO CatColors::Draw

   @cat_pro_error_handler

    TVLCT, self._r, self._g, self._b

END



;*****************************************************************************************************
;+
; NAME:
;       CATCOLORS::EVENTHANDLER
;
; PURPOSE:
;
;       An event handler method to handle widget events associated with the CatColors object.
;
; SYNTAX:
;
;       Typically called from the EVENTDISPATCHER utility routine. All event objects are named,
;       and the CASE statement branches on the object's name.
;
; ARGUMENTS:
;
;       event:  The event structure from a widget event.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO CatColors::EventHandler, event

   @cat_pro_error_handler

   currentWindow = !D.Window
   sendmessage = 0

   CASE StrUpCase(event.name) OF

      'XCOLORS_SWITCH_COLORTYPE': BEGIN

           topObject = CatGetTopObject(event.id)
           topObject -> GetProperty, UValue=info
           event.id -> GetProperty, UVALUE=colortypes
           thisType = colortypes[event.index]

           ; Find the current list widget._
           oldListWidget = topObject -> Get('XCOLORS_COLORTABLE_SELECT', /RECURSIVE_SEARCH)
           oldListWidget -> GetProperty, PARENT=parent

           CASE thisType OF

                'IDL': BEGIN
                   self._colorfile = Filepath(SubDir=['resource','colors'], 'colors1.tbl')
                   self._brewer = 0
                   info._brewer = 0
                   info._colorfile = self._colorfile
                   END

                'BREWER': BEGIN
                   self._colorfile = File_Which('fsc_brewer.tbl')
                   self._brewer = 1
                   info._brewer = 1
                   info._colorfile = self._colorfile
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
            info._list = list
            Obj_Destroy, oldListWidget
            topObject -> SetProperty, UValue=info

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
            currentBottom = event.value
            info._topSlider -> GetProperty, Value=currentTop
            currentTop = currentTop

            ; Bottom cannot be larger than top.
            IF currentBottom GE (currentTop-1) THEN $
               info._bottomSlider -> SetProperty, Value=(currentTop-1)

            self -> XColors_Update, info
            topObject -> SetProperty, UValue=info

            sendmessage = 1

         ENDCASE

      'XCOLORS_TOP_SLIDER': BEGIN

           topObject = CatGetTopObject(event.id)
           topObject -> GetProperty, UValue=info

           ; Update the current top value of the slider.
           currentTop = event.value
           info._bottomSlider -> GetProperty, Value=currentBottom

           ; Top cannot be smaller than bottom.
           IF currentTop LE (currentBottom) THEN $
              info._topSlider -> SetProperty, Value=(currentBottom+1)

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
            self._colorfile = info._colorfile
            self._cindex = info._ctindex
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

            self._r[0:self._ncolors-1] = info._rcancel
            self._g[0:self._ncolors-1] = info._gcancel
            self._b[0:self._ncolors-1] = info._bcancel
            self._cindex = info._cancelIndex
            self._colorfile = info._cancelfile

            self -> Draw

            Obj_Destroy, self._xcTLB
            self._xc_registerName = ""

            self -> SendMessage, 'COLORTOOL_TABLECHANGE', $
               DATA={r:info._rcancel, g:info._gcancel, b:info._bcancel, bottom:0}

         ENDCASE

      'XCOLORS_ACCEPT_BUTTON': BEGIN

            topObject = CatGetTopObject(event.id)
            topObject -> GetProperty, UValue=info

            self._r_old = self._r
            self._g_old = self._g
            self._b_old = self._b
            self._r[0:self._ncolors-1] = info._rcurrent
            self._g[0:self._ncolors-1] = info._gcurrent
            self._b[0:self._ncolors-1] = info._bcurrent
            self -> Draw
            Obj_Destroy, self._xcTLB
            self._xc_registerName = ""

         ENDCASE

      'XCOLORS_TLB':

   ENDCASE

   IF WindowAvailable(currentWindow) THEN WSet, currentWindow

   ; Send a message, if necessary that colors have changed.
   IF sendmessage THEN BEGIN
      IF N_Elements(info) EQ 0 THEN BEGIN
         topObject = CatGetTopObject(event.id)
         topObject -> GetProperty, UValue=info
      ENDIF
      self -> SendMessage, 'COLORTOOL_TABLECHANGE', $
         DATA={r:info._rcurrent, g:info._gcurrent, b:info._bcurrent, bottom:0}
   ENDIF

END



;*****************************************************************************************************
;+
; NAME:
;       CATCOLORS::GETPROPERTY
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
;       BLUE:          The blue color vector.
;
;       BREWER:        If set, using Brewer Color Tables.
;
;       COLORPALETTE:  A 256-by-3 array containing the current color table vectors
;
;       INDEX:         The current color table index number.
;
;       GREEN:         The green color vector.
;
;       NCOLORS:       The number of colors in the "changable" portion of the color table.
;
;       RED:           The red color vector.
;
;       _REF_EXTRA:    Any keyword appropriate for the GETPROPERTY method of the superclass object.
;
;-
;*****************************************************************************************************
PRO CatColors::GetProperty, $
               BLUE=blue, $
               BOTTOM=bottom, $ ; Here for compatibility with ColorTool object.
               BREWER=brewer, $
               COLORPALETTE=colorPalette, $
               GREEN=green, $
               INDEX=ctindex, $
               NCOLORS=ncolors, $
               RED=red, $
               _Ref_Extra=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(bottom) THEN bottom = 0
   IF Arg_Present(brewer) THEN brewer = self._brewer
   IF Arg_Present(ctindex) THEN ctindex = self._cindex
   IF Arg_Present(colorPalette) THEN colorPalette = [[self._r],[self._g],[self._b]]
   IF Arg_Present(ncolors) THEN ncolors = self._ncolors
   IF Arg_Present(red) THEN red = self._r
   IF Arg_Present(green) THEN green = self._g
   IF Arg_Present(blue) THEN blue = self._b

   self -> CATATOM::GetProperty, _Extra=extraKeywords

END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORS::LOADCOLOR
;
; PURPOSE:
;
;       This method loads a color at a particular index in the color table.
;
; SYNTAX:
;
;       colorObject -> LoadColor, theColorName, theColorIndex
;
; ARGUMENTS:
;
;       theColorName:   The name of the color to load. The color name must be loaded into the
;                       the object. If it is not, add the color to the object with the AddColor
;                       method prior to loading it.
;
;       theColorIndex:  The color table index number where the color is to be loaded. An integer
;                       between 0 and 255. By default, 255.
;
; KEYWORDS:
;
;       DRAW:           If this keyword is set, the DRAW method is called after the color
;                       has been loaded.
;-
;*****************************************************************************************************
PRO CatColors::LoadColor, theColorName, theColorIndex, DRAW=draw

   @cat_pro_error_handler

   IF N_Elements(theColorName) EQ 0 THEN Message, 'A color name argument is required.'
   IF Size(theColorName, /TNAME) NE 'STRING' THEN Message, 'The "colorName" argument must be a string.'
   IF N_Elements(theColorIndex) EQ 0 THEN theColorIndex = 255 ELSE theColorIndex = Long(theColorIndex[0])
   theColorIndex = 0 > theColorIndex < 255

   ; Does the color name exist?
   vectorIndex = Where(StrUpCase(*self._colornames) EQ StrUpCase(StrCompress(theColorName, /REMOVE_ALL)), count)
   IF count EQ 0 THEN BEGIN
        Message, 'Cannot find color name "' + theColorName + '" in the color name vector. Returning...'
   ENDIF

   ; Load the color.
   self._r[theColorIndex] = (*self._rvalue)[vectorIndex[0]]
   self._g[theColorIndex] = (*self._gvalue)[vectorIndex[0]]
   self._b[theColorIndex] = (*self._bvalue)[vectorIndex[0]]

   ; Draw the colors?
   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORS::LOADCT
;
; PURPOSE:
;
;       This method loads a color table, with a specific index number, from a color table file.
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
PRO CatColors::LoadCT, colorIndex, BREWER=brewer, DRAW=draw

   @cat_pro_error_handler

   ; Check parameters.
   IF N_Elements(colorIndex) EQ 0 THEN colorIndex = 0

   ; Try to locate the brewer file. Check resource/colors directory, then any
   ; directory on the IDL path.
    brewerfile = Filepath(SubDir=['resource','colors'], 'fsc_brewer.tbl')
    IF File_Test(brewerfile, /READ) EQ 0 THEN brewerfile = File_Which('fsc_brewer.tbl', INCLUDE_CURRENT_DIR=1)
    IF brewerfile EQ "" THEN BEGIN
        locatedBrewerFile = 0
    ENDIF ELSE BEGIN
        locatedBrewerFile = 1
        IF Keyword_Set(brewer) THEN self._colorfile = brewerfile
    ENDELSE

   ; Store the current color table vectors for later restoration.
   self._r_old = self._r
   self._g_old = self._g
   self._b_old = self._b

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

   ; Close the file.
   Free_Lun, lun

   ; Set the index permanently.
   self._cindex = colorIndex

   ; Do we need to interpolate?
   IF self._ncolors LT 256 THEN BEGIN
      ctindex = (Lindgen(self._ncolors) * 255) / (self._ncolors-1)
      r = r[ctindex]
      g = g[ctindex]
      b = b[ctindex]
   ENDIF

   ; Load color vectors.
   self._r[0:self._ncolors-1] = r
   self._g[0:self._ncolors-1] = g
   self._b[0:self._ncolors-1] = b

   ; If we have colors displayed, update them.
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

      ; Load the color table and display the color bar.
      currentWindow = !D.Window
      TVLCT, info._rcurrent, info._gcurrent, info._bcurrent
      info._ctDraw -> SetWindow
      Device, Get_Decomposed=theState, Decomposed=0
      TV, info._ctbar
      Device, Decomposed=theState
      IF WindowAvailable(currentWindow) THEN WSet, currentWindow

      info._list -> SetProperty, SELECT=colorIndex


      ; Changed info, so set it back for subsequent GUI_UPDATE method.
      self._xcTLB -> SetProperty, UValue=info

   ENDIF

   ; Send a color table change message.
   self -> SendMessage, 'COLORTOOL_TABLECHANGE', DATA={r:r, g:g, b:b, bottom:0}

   ; Draw the colors?
   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed
END


PRO CatColors::MessageHandler, title, SENDER=sender, DATA=data

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
;       CATCOL)RS::REVERSE
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
PRO CatColors::Reverse,  DRAW=draw

   @cat_pro_error_handler

   ; Check parameters.

   IF N_Elements(bottom) EQ 0 THEN bottom = 0
   IF N_Elements(ncolors) EQ 0 THEN ncolors = !D.Table_Size

      ; Store the current color table vectors for later restoration.

   self._r_old = self._r
   self._g_old = self._g
   self._b_old = self._b

   ; Load color vectors.

   r = Reverse((self._r)[0:self._ncolors-1])
   g = Reverse((self._g)[0:self._ncolors-1])
   b = Reverse((self._b)[0:self._ncolors-1])
   self._r[0:self._ncolors-1] = r
   self._g[0:self._ncolors-1] = g
   self._b[0:self._ncolors-1] = b

   ; Draw the colors?

   IF Keyword_Set(draw) THEN self -> Draw

   ; Need a message sent?

   self -> SendMessage, 'COLORTOOL_TABLECHANGE', DATA={r:r, g:g, b:b, bottom:0}

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORS::SETPROPERTY
;
; PURPOSE:
;
;       This method allows properties of the object to be set. A COLORTOOL_SETPROPERTY message is
;       send only if the RED, GREEN, or BLUE keywords are used.
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
;       BLUE:         The blue color vector.
;
;       BREWER:       Use the brewer color tables.
;
;       COLORFILE:    The fully-qualified name of a color table file to open and display.
;
;       COLORPALETTE:  A n-by-3 array containing the current color table vectors
;
;       GREEN:        The green color vector.
;
;       INDEX:        The color table index number.
;
;       NCOLORS:      The number of colors in the color table.
;
;       RED:          The red color vector.
;
;       _EXTRA:       Any keyword appropriate for the SETPROPERTY method of the CATATOM object.
;
;-
;*****************************************************************************************************
PRO CatColors::SetProperty, $
   BLUE=blue, $
   BOTTOM=bottom, $ ; For compatibility with COLORTOOL
   BREWER=brewer, $
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

   IF N_Elements(brewer) NE 0 THEN BEGIN
        file = self._colorFile
        brewerfile = Filepath(SubDir=['resource','colors'], 'fsc_brewer.tbl')
        IF File_Test(brewerfile, /READ) EQ 0 THEN brewerfile = File_Which('fsc_brewer.tbl', INCLUDE_CURRENT_DIR=1)
        IF brewerfile EQ "" THEN BEGIN
            locatedBrewerFile = 0
        ENDIF ELSE BEGIN
            locatedBrewerFile = 1
            IF Keyword_Set(brewer) THEN file = brewerfile
        ENDELSE
       IF locatedBrewerFile THEN BEGIN
            self._colorFile = file
            self._brewer = brewer
       ENDIF ELSE Message, 'Cannot locate Brewer Color Table file.'
   ENDIF
   IF N_Elements(ncolors) NE 0 THEN self._ncolors = ncolors
   IF N_Elements(index) NE 0 THEN self._cindex = index

   IF N_Elements(colorFile) NE 0 THEN BEGIN
      self._colorFile = colorFile
      self -> LoadCT, self._cindex, BREWER=self._brewer
   ENDIF


   IF N_Elements(colorPalette) NE 0 THEN BEGIN
      self._r = colorPalette[*,0]
      self._g = colorPalette[*,1]
      self._b = colorPalette[*,2]
   ENDIF

   IF N_Elements(extraKeywords) NE 0 THEN self -> CATATOM::SetProperty, _Extra=extraKeywords

   ; Need a message sent?
   IF sendmessage THEN self -> SendMessage, 'COLORTOOL_SETPROPERTY'

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORS::XCOLORS
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

PRO CatColors::XColors, $
    Group_Leader=groupLeader, $
    Modal=modal, $
    Title=title, $
    XOffset=xoffset, $
    YOffset=yoffset

   @cat_pro_error_handler

   currentWindow = !D.Window

   ; Check keywords.
   IF N_Elements(title) EQ 0 THEN title = 'Load Color Table'

   IF self._xc_registerName EQ "" THEN BEGIN
      self._xc_registerName = 'XCOLORS:' + StrCompress(Systime())
   ENDIF

   ; Only one XCOLORS with this title.
   IF XRegistered(self._xc_registerName) GT 0 THEN RETURN

   ; Try to locate the brewer file. Check resource/colors directory, then any
   ; directory on the IDL path.
    brewerfile = Filepath(SubDir=['resource','colors'], 'fsc_brewer.tbl')
    IF File_Test(brewerfile, /READ) EQ 0 THEN brewerfile = File_Which('fsc_brewer.tbl', INCLUDE_CURRENT_DIR=1)
    IF brewerfile EQ "" THEN BEGIN
        locatedBrewerFile = 0
    ENDIF ELSE BEGIN
        locatedBrewerFile = 1
    ENDELSE

   info = {   _ctbar: BytArr(256, 40), $     ; The color table image of the XCOLORS dialog.
              _ctDraw: Obj_New(), $          ; The draw widget of the XCOLORS dialog.
              _bottomSlider: Obj_New(), $    ; The bottom slider of the XCOLORS dialog.
              _topSlider: Obj_New(), $       ; The top slider of the XCOLORS dialog.
              _gammaSlider: Obj_New(), $     ; The gamma slider of the XCOLORS dialog.
              _gammaLabel: Obj_New(), $      ; The gamma label widget.
              _list: Obj_New(), $            ; The color table list widget.
              _ctIndex: self._cindex, $      ; The current color table index.
              _rorig: self._r, $             ; The original color vectors.
              _gorig: self._g, $             ; The original color vectors.
              _borig: self._b, $             ; The original color vectors.
              _cancelIndex: self._cindex, $  ; The cancel index.
              _brewer: self._brewer, $
              _colorfile: self._colorFile, $
              _modal: Keyword_Set(modal), $
              _ncolors: self._ncolors, $
              _title: title, $
              _cancelFile: self._colorFile, $
              _rcancel: self._r[0:self._ncolors-1], $ ; The red color table vector to use for a XCOLORS cancel button.
              _gcancel: self._g[0:self._ncolors-1], $ ; The green color table vector to use for a XCOLORS cancel button.
              _bcancel: self._b[0:self._ncolors-1], $ ; The blue color table vector to use for a XCOLORS cancel button.
              _rcurrent: self._r[0:self._ncolors-1],$ ; The red color table vector to use for a XCOLORS accept button.
              _gcurrent: self._g[0:self._ncolors-1],$ ; The green color table vector to use for a XCOLORS accept button.
              _bcurrent: self._b[0:self._ncolors-1] $ ; The blue color table vector to use for a XCOLORS accept button.
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
            IF Keyword_Set(self._brewer) THEN colorType -> SetProperty, Select=' Brewer Colors '
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
   info._list = list


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
   currentWindow = !D.Window
   bar = BINDGEN(self._ncolors) # REPLICATE(1B, 10)
   bar = BYTSCL(bar, TOP=self._ncolors-1)
   info._ctbar = Congrid(bar, 256, 40, /INTERP)
   info._ctDraw -> SetWindow
   Device, Get_Decomposed=theState, Decomposed=0
   TV, info._ctbar
   Device, Decomposed=theState
   self._name = 'CATCOLORS'
   tlb -> SetProperty, UValue=info
   IF WindowAvailable(currentWindow) THEN WSet, currentWindow

END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORS::XCOLORS_UPDATE
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
PRO CatColors::XColors_Update, info

   @cat_pro_error_handler

   ; Stretch color tables.
   top = self._ncolors-1
   info._rcurrent = info._rorig[0:top]
   info._gcurrent = info._gorig[0:top]
   info._bcurrent = info._borig[0:top]
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
   self._r[0:self._ncolors-1] = info._rcurrent
   self._g[0:self._ncolors-1] = info._gcurrent
   self._b[0:self._ncolors-1] = info._bcurrent

   ; Load the color table and display the color bar.
   currentWindow = !D.Window
   TVLCT, info._rcurrent, info._gcurrent, info._bcurrent
   info._ctDraw -> SetWindow
   Device, Get_Decomposed=theState, Decomposed=0
   TV, info._ctbar
   Device, Decomposed=theState
   IF WindowAvailable(currentWindow) THEN WSet, currentWindow
END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORS::CLEANUP
;
; PURPOSE:
;
;       This is the CATCOLORS object class destructor method.
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
PRO CatColors::CLEANUP

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
;       CATCOLOR::INIT
;
; PURPOSE:
;
;       This method initiates the CATCOLOR object.
;
; SYNTAX:
;
;       colorObject Obj_New('CatColor')
;
; ARGUMENTS:
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
;       COLORFILE:    The fully qualified file name of the file containing color tables. By default,
;                     colorFile = Filepath(SubDirectory=['resource','colors'], 'colors1.tbl')
;
;       COLORPALETTE: An 256-by-3 byte array containing a color palette to load into the object.
;                     Loading a color palette does NOT send a COLORTOOL_TABLECHANGE message.
;
;       INDEX:        The index number of the color table to load. By default, 0.
;
;       NCOLORS:      The number of colors in the color palette that can be changed with the LOADCT
;                     or XCOLORS methods. These colors are always loaded from the bottom of the color
;                     palette and start at index 0. By default, 256.
;
;-
;*****************************************************************************************************
FUNCTION CatColors::INIT, $
   BREWER=brewer, $
   COLORFILE=colorFile, $
   COLORPALETTE=colorPalette, $
   INDEX=cindex, $
   NCOLORS=ncolors, $
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
   IF N_Elements(cindex) EQ 0 THEN cindex = 0
   IF N_Elements(colorFile) EQ 0 THEN colorFile = Filepath(SubDirectory=['resource','colors'], 'colors1.tbl')
   IF N_Elements(ncolors) EQ 0 THEN ncolors = 256

   ; Default values in current color vectors.
   self._r = Bindgen(256)
   self._g = Bindgen(256)
   self._b = Bindgen(256)

   ; Set up the color names.
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

   ; Brewer colors.
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

   ; Populate the objects.
   self._colornames = Ptr_New(StrUpCase(StrCompress(StrTrim(colors,2), /Remove_All)))
   self._rvalue = Ptr_New(rvalue)
   self._gvalue = Ptr_New(gvalue)
   self._bvalue = Ptr_New(bvalue)
   self._colorFile = colorFile
   self._ncolors = ncolors
   self._cindex = cindex
   IF locatedBrewerFile THEN self._brewer = Keyword_Set(brewer)

   ; Load a color table. The LOADCT method will check to be sure
   ; the color table index is within range for the particular color
   ; table file in use.
   self -> LoadCT, cindex

   ; If a color palette has been provided. Load that now.
   IF N_Elements(colorPalette) NE 0 THEN BEGIN
      self._r = colorPalette[*,0]
      self._g = colorPalette[*,1]
      self._b = colorPalette[*,2]
   ENDIF

   ; Call the Superclass to initialize.
   IF self -> CATATOM::INIT(_Extra=extrakeywords) EQ 0 THEN RETURN, 0

   self -> Report, /Completed

   RETURN, 1
END

;
;
;*****************************************************************************************************
;
; NAME:
;       CATCOLOR CLASS DEFINITION
;
; PURPOSE:
;
;       This is the CATCOLOR object's structure definition.
;
;*****************************************************************************************************
PRO CatColors__Define, class

    class = { CATCOLORS, $
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
              _ncolors: 0L, $            ; The number of colors allowed to change in color table.
              _cindex: 0L, $             ; The current color table index.
              _xcTLB: Obj_New(), $       ; The XCOLORS tlb object.
              _xc_registerName: "", $    ; The name with which the XCOLORS dialog is registered.
              _r_old: BytArr(256), $     ; The old red color table vector.
              _g_old: BytArr(256), $     ; The old green color table vector.
              _b_old: BytArr(256) $      ; The old blue color table vector.
             }

END