;*****************************************************************************************************
;+
; NAME:
;       PLOTCOORD
;
; PURPOSE:
;
;       The purpose of this object is to create a coordinate system for generic data objects.
;       It does this by saving IDL system variables. Typically, the PLOTCOORD object is updated 
;       *after* a plot is drawn in a window and acts to save the plotting parameters for that
;       particular plot. This is useful, for example, when you wish to overplot data on the plot
;       in question. You can call the draw method of the PLOTCOORD object to re-establish the plot
;       parameters for overplotting.
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
;       Programming.
;
; CALLING SEQUENCE:
;
;       theCoordObj = Obj_New('PlotCoord')
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 15 September 2003.
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
;       PLOTCOORD::DRAW
;
; PURPOSE:
;
;       This method establishes the coordinate system of the object by
;       replacing the !Map, !P, !X, !Y, and !Z system variables with their
;       stored equivalents. The current system variables are saved so they
;       can be restored with the RESTORE method later.
;
; SYNTAX:
;
;       coordObj -> Draw
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
PRO PlotCoord::Draw, Extra=extrakeywords

   @cat_pro_error_handler

   ; Save the current system variables so they can (potentially) be restored later.

   *self._mapsysvar_old = !Map
   self._psysvar_old = !P
   self._xsysvar_old = !X
   self._ysysvar_old = !Y
   self._zsysvar_old = !Z

   ; Set up the current system variables with stored coordinate space.
   !MAP = *self._mapsysvar
   !P = self._psysvar
   !X = self._xsysvar
   !Y = self._ysysvar
   !Z = self._zsysvar

   self -> Report, /Completed

END




;*****************************************************************************************************
;+
; NAME:
;       PLOTCOORD::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to get the stored system variables in the object
;
;
; SYNTAX:
;
;       coordObj -> GetProperty, Mapsysvar=mapsysvar
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       MAPSYSVAR:   The !Map system variable stored in the object.
;
;       POSITION:    The !P.Position value.
;
;       PSYSVAR:     The !P system variable stored in the object.
;
;       XSYSVAR:     The !X system variable stored in the object.
;
;       YSYSVAR:     The !Y system variable stored in the object.
;
;       ZSYSVAR:     The !Z system variable stored in the object.
;
;       _REF_EXTRA:  Any keyword appropriate for the superclass method.
;-
;*****************************************************************************************************
PRO PlotCoord::GetProperty, $
   MAPSYSVAR=mapsysvar, $
   POSITION=position, $
   PSYSVAR=psysvar, $
   XSYSVAR=xsysvar, $
   YSYSVAR=ysysvar, $
   ZSYSVAR=zsysvar, $
   _Ref_Extra=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(mapsysvar) THEN mapsysvar = *self._mapsysvar
   IF Arg_Present(psysvar) THEN psysvar = self._psysvar
   IF Arg_Present(xsysvar) THEN xsysvar = self._xsysvar
   IF Arg_Present(ysysvar) THEN ysysvar = self._ysysvar
   IF Arg_Present(zsysvar) THEN zsysvar = self._zsysvar
   IF Arg_Present(position) THEN position = (self._psysvar).position

   ; Need superclass properties?
   IF N_Elements(extraKeywords) NE 0 THEN self -> CATATOM::GetProperty, _Extra=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       PLOTCOORD::RESTORE
;
; PURPOSE:
;
;       This method restores the coordinate system that was in place at the
;       time of the last DRAW method invocation.
;
; SYNTAX:
;
;       coordObj -> Restore
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
PRO PlotCoord::Restore

   @cat_pro_error_handler

   ; Set system variable to their old pre-DRAW values.
   !Map = *self._mapsysvar_old
   !P = self._psysvar_old
   !X = self._xsysvar_old
   !Y = self._ysysvar_old
   !Z = self._zsysvar_old

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       PLOTCOORD::SAVECOORDS
;
; PURPOSE:
;
;       This method allows the object to save the currently established coordinate system.
;       The system variables !MAP, !P, !X, !Y, and !Z are saved.
;
; SYNTAX:
;
;       coordObj -> SaveCoords
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
PRO PlotCoord::SaveCoords

   @cat_pro_error_handler

   ; Save the system variables.
   *self._mapsysvar = !Map
   self._psysvar = !P
   self._xsysvar = !X
   self._ysysvar = !Y
   self._zsysvar = !Z


   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       PLOTCOORD::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the stored system variables in the object.
;
;
; SYNTAX:
;
;       coordObj -> SetProperty, Mapsysvar=!Map
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       MAPSYSVAR:   The !Map system variable to store in the object.
;
;       PSYSVAR:     The !P system variable to store in the object.
;
;       XSYSVAR:     The !X system variable to store in the object.
;
;       YSYSVAR:     The !Y system variable to store in the object.
;
;       ZSYSVAR:     The !Z system variable to store in the object.
;
;       _EXTRA:      Any keyword appropriate for the superclass method.
;-
;*****************************************************************************************************
PRO PlotCoord::SetProperty, $
   MAPSYSVAR=mapsysvar, $
   PSYSVAR=psysvar, $
   XSYSVAR=xsysvar, $
   YSYSVAR=ysysvar, $
   ZSYSVAR=zsysvar, $
   _Extra=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(mapsysvar) NE 0 THEN *self._mapsysvar = mapsysvar
   IF N_Elements(psysvar) NE 0 THEN self._psysvar = psysvar
   IF N_Elements(xsysvar) NE 0 THEN self._xsysvar = xsysvar
   IF N_Elements(ysysvar) NE 0 THEN self._ysysvar = ysysvar
   IF N_Elements(zsysvar) NE 0 THEN self._zsysvar = zsysvar

   ; Need superclass properties?
   IF N_Elements(extraKeywords) NE 0 THEN self -> CATATOM::SetProperty, _Extra=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       PLOTCOORD::SETCOORDINATES
;
; PURPOSE:
;
;       This method allows user to set up a 2D data coordinate system as used by the
;       IDL PLOT, CONTOUR, etc. commands.
;
;
; SYNTAX:
;
;       coordObj -> SetCoordinates, Position=[0.1, 0.1, 0.5, 0.75], XRange=[1,10], YRange=[3,9]
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       AUTOSCALE:    Set this keyword if you wish the axis plot ranges to be autoscaled. The
;                     default is to set exact axis ranges based on the [XYZ]Range keywords.
;
;       ASPECT_RATIO: If this keyword is set to a floating point value indicating the aspect
;                     ratio (height/width) of the resulting data coordinate system, the POSITION
;                     keyword will be modified to represent this aspect ratio. This is primarily
;                     for the convenience of CatImageData, which can display its image in a
;                     resizeable graphics window with the correct image aspect ratio. If you use
;                     this keyword, you can find the new POSITION by using the GetProperty method:
;
;                          coordObj -> GetProperty, Position=adjustedPosition
;
;       POSITION:     A four-element array representing the position of the plot in the window.
;
;       WINDEX:       The window index number of the window for which the data coordinate
;                     system is being established. By default, !D.Window is used.
;
;       XRANGE:       A two-element array representing the range for the X data axis. Default is [0,1].
;
;       XSIZE:        The X size of the window for which data coordinates are to be set. Used only if
;                     WINDEX is not provided.
;
;       YRANGE:       A two-element array representing the range for the Y data axis. Default is [0,1].
;
;       YSIZE:        The Y size of the window for which data coordinates are to be set. Used only if
;                     WINDEX is not provided.
;-
;*****************************************************************************************************
PRO PlotCoord::SetCoordinates, $
   AUTOSCALE=autoscale, $
   ASPECT_RATIO=aspect_ratio, $
   POSITION=position, $
   WINDEX=windex, $
   XRANGE=xrange, $
   XSIZE=xsize, $
   YRANGE=yrange, $
   YSIZE=ysize

   @cat_pro_error_handler

   ; Check parameters.
   currentWindow = !D.Window
   IF N_Elements(windex) EQ 0 THEN BEGIN
      IF (N_Elements(xsize) EQ 0) THEN BEGIN
         windex = !D.Window
         xsize = !D.X_Size
      ENDIF
   ENDIF ELSE BEGIN
      WSet, windex
      xsize = !D.X_Size
   ENDELSE
   IF N_Elements(windex) EQ 0 THEN BEGIN
      IF (N_Elements(ysize) EQ 0) THEN BEGIN
         windex = !D.Window
         ysize = !D.Y_Size
      ENDIF
   ENDIF ELSE BEGIN
      WSet, windex
      ysize = !D.Y_Size
   ENDELSE

   autoscale = Keyword_Set(autoscale)
   IF N_Elements(position) EQ 0 THEN BEGIN
      position = (self._psysvar).position
      IF Total(position) EQ 0.0 THEN position = [0.15, 0.125, 0.95, 0.95]
   ENDIF
   IF N_Elements(xrange) EQ 0 THEN xrange = [0,1]
   IF N_Elements(yrange) EQ 0 THEN yrange = [0,1]

   ; Set up conditions for the plot.
   IF autoscale THEN BEGIN
      xstyle = 0
      ystyle = 0
   ENDIF ELSE BEGIN
      xstyle = 1
      ystyle = 1
   ENDELSE

   IF N_Elements(aspect_ratio) NE 0 THEN BEGIN

         ; Find the proposed size of the image in pixels without aspect
         ; considerations.

      xpixSize = (position[2] - position[0]) * !D.X_VSize
      ypixSize = (position[3] - position[1]) * !D.Y_VSize

         ; Try to fit the image width. If you can't maintain
         ; the aspect ratio, fit the image height.

      trialX = xpixSize
      trialY = trialX * aspect_ratio
      IF trialY GT ypixSize THEN BEGIN
         trialY = ypixSize
         trialX = trialY / aspect_ratio
      ENDIF

         ; Recalculate the position of the image in the window.

      newPosition = FltArr(4)
      newPosition[0] = (((xpixSize - trialX) / 2.0) / !D.X_VSize) + position[0]
      newPosition[2] = newPosition[0] + (trialX/FLOAT(!D.X_VSize))
      newPosition[1] = (((ypixSize - trialY) / 2.0) / !D.Y_VSize)  + position[1]
      newPosition[3] = newPosition[1] + (trialY/FLOAT(!D.Y_VSize))
      position = newPosition
   ENDIF

   ; Set up data coordinate space.

   Window, /Pixmap, /Free, XSize=xsize, YSize=ysize
   x = Scale_Vector(Findgen(10), xrange[0], xrange[1])
   y = Scale_Vector(Findgen(10), yrange[0], yrange[1])
   currentPos = !P.Position
   !P.Position = position
   Plot, x, y, /NoData, XStyle=xstyle, YStyle=ystyle, XRange=xrange, YRange=yrange

   ; Save the system variables.
   self -> SaveCoords
   WDelete, !D.Window
   IF currentWindow GE 0 THEN WSet, currentWindow

   ; Done.
   !P.Position = currentPos
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       PLOTCOORD::SETSYSTEMDEFAULTS
;
; PURPOSE:
;
;       This method allows the object to set the stored system variables to the normal
;       system default values.
;
;
; SYNTAX:
;
;       coordObj -> SetSystemDefaults
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
PRO PlotCoord::SetSystemDefaults

   @cat_pro_error_handler

   ; Some variables will depend on current decomposition state.
   Device, Get_Decomposed=theState

   ; !P System Variable

   !P.BACKGROUND = 0
   !P.CHARSIZE = 0.000000
   !P.CHARTHICK = 0.000000
   !P.CLIP = LonArr(6)
   IF theState EQ 1 THEN !P.COLOR = 16777215 ELSE !P.COLOR = !D.Table_Size-1
   !P.FONT = -1L
   !P.LINESTYLE = 0L
   !P.MULTI = LonArr(5)
   !P.NOCLIP = 0L
   !P.NOERASE = 0L
   !P.NSUM =  0L
   !P.POSITION = FltArr(4)
   !P.PSYM = 0L
   !P.REGION = FltArr(4)
   !P.SUBTITLE = ''
   !P.SYMSIZE = 0.000000
   !P.T = DblArr(4,4)
   !P.T3D = 0L
   !P.THICK = 0.000000
   !P.TITLE = ''
   !P.TICKLEN = 0.0200000
   !P.CHANNEL = 0L

   !X.TITLE = ''
   !X.TYPE = 0L
   !X.STYLE = 0L
   !X.TICKS = 0L
   !X.TICKLEN = 0.0
   !X.THICK = 0.0
   !X.RANGE = DblArr(2)
   !X.CRANGE = DblArr(2)
   !X.S = DblArr(2)
   !X.MARGIN = [10.0, 3.0]
   !X.OMARGIN = FltArr(2)
   !X.WINDOW = FltArr(2)
   !X.REGION = FltArr(2)
   !X.CHARSIZE = 0.0
   !X.MINOR = 0L
   !X.TICKV = DblArr(60)
   !X.TICKNAME = StrArr(60)
   !X.GRIDSTYLE = 0L
   !X.TICKFORMAT = StrArr(10)
   !X.TICKLAYOUT = 0L
   !X.TICKUNITS = StrArr(10)

   ; !Y System Variable

   !Y.TITLE = ''
   !Y.TYPE = 0L
   !Y.STYLE = 0L
   !Y.TICKS = 0L
   !Y.TICKLEN = 0.0
   !Y.THICK = 0.0
   !Y.RANGE = DblArr(2)
   !Y.CRANGE = DblArr(2)
   !Y.S = DblArr(2)
   !Y.MARGIN = [4.0, 2.0]
   !Y.OMARGIN = FltArr(2)
   !Y.WINDOW = FltArr(2)
   !Y.REGION = FltArr(2)
   !Y.CHARSIZE = 0.0
   !Y.MINOR = 0L
   !Y.TICKV = DblArr(60)
   !Y.TICKNAME = StrArr(60)
   !Y.GRIDSTYLE = 0L
   !Y.TICKFORMAT = StrArr(10)
   !Y.TICKLAYOUT = 0L
   !Y.TICKUNITS = StrArr(10)

   ;!Z System Variable

   !Z.TITLE = ''
   !Z.TYPE = 0L
   !Z.STYLE = 0L
   !Z.TICKS = 0L
   !Z.TICKLEN = 0.0
   !Z.THICK = 0.0
   !Z.RANGE = DblArr(2)
   !Z.CRANGE = DblArr(2)
   !Z.S = DblArr(2)
   !Z.MARGIN = FltArr(2)
   !Z.OMARGIN = FltArr(2)
   !Z.WINDOW = FltArr(2)
   !Z.REGION = FltArr(2)
   !Z.CHARSIZE = 0.0
   !Z.MINOR = 0L
   !Z.TICKV = DblArr(60)
   !Z.TICKNAME = StrArr(60)
   !Z.GRIDSTYLE = 0L
   !Z.TICKFORMAT = StrArr(10)
   !Z.TICKLAYOUT = 0L
   !Z.TICKUNITS = StrArr(10)

   ; !Map System Variable

   !MAP.PROJECTION = 0L
   !MAP.SIMPLE = 0L
   !MAP.FILL_METHOD = 0L
   !MAP.UP_FLAGS = 0L
   !MAP.UP_NAME = ""
   !MAP.P0LON =  0.0D
   !MAP.P0LAT =  0.0D
   !MAP.U0 =  0.0D
   !MAP.V0 =  0.0D
   !MAP.SINO =  0.0D
   !MAP.COSO =  0.0D
   !MAP.ROTATION =  0.0D
   !MAP.SINR =  0.0D
   !MAP.COSR =  0.0D
   !MAP.A =  0.0D
   !MAP.E2 =  0.0D
   !MAP.UV = DblArr(2)
   !MAP.POLE = DblArr(7)
   !MAP.UV_BOX = DblArr(4)
   !MAP.LL_BOX = DblArr(4)
   !MAP.SEGMENT_LENGTH =  0.0D
   !MAP.P = DblArr(16)
   !MAP.PIPELINE = DblArr(8,12)

   ; Save the current system variables.
   self -> SaveCoords

   ; Done.
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       PLOTCOORD::CLEANUP
;
; PURPOSE:
;
;       This is the PLOTCOORD object class destructor method.
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
PRO PlotCoord::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self._mapsysvar
   Ptr_Free, self._mapsysvar_old

   self -> CATATOM::CLEANUP

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       PLOTCOORD::INIT
;
; PURPOSE:
;
;       This is the PLOTCOORD object class creator method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       parent:      The parent object to which this coordinate object will be added.
;
; KEYWORDS:
;
;       MAPSYSVAR:   The !Map system variable to store in the object.
;
;       PSYSVAR:     The !P system variable to store in the object.
;
;       XSYSVAR:     The !X system variable to store in the object.
;
;       YSYSVAR:     The !Y system variable to store in the object.
;
;       XSYSVAR:     The !Z system variable to store in the object.
;
;       _EXTRA:      Any keyword appropriate for the superclass method.
;-
;*****************************************************************************************************
FUNCTION PlotCoord::INIT, parent, $
   MAPSYSVAR=mapsysvar, $
   PSYSVAR=psysvar, $
   XSYSVAR=xsysvar, $
   YSYSVAR=ysysvar, $
   ZSYSVAR=zsysvar, $
   _Extra=extraKeywords

   @cat_func_error_handler

   ; Define keywords.
   IF N_Elements(mapsysvar) EQ 0 THEN mapsysvar = !Map
   IF N_Elements(psysvar) EQ 0 THEN psysvar = !P
   IF N_Elements(xsysvar) EQ 0 THEN xsysvar = !X
   IF N_Elements(ysysvar) EQ 0 THEN ysysvar = !Y
   IF N_Elements(zsysvar) EQ 0 THEN zsysvar = !Z

   ; Populate object.
   self._mapsysvar = Ptr_New(mapsysvar)
   self._psysvar = psysvar
   self._xsysvar = xsysvar
   self._ysysvar = ysysvar
   self._zsysvar = zsysvar
   self._mapsysvar_old = Ptr_New(!Map)
   self._psysvar_old = !P
   self._xsysvar_old = !X
   self._ysysvar_old = !Y
   self._zsysvar_old = !Z

   ; Call superclass INIT method.
   ok = self -> CATATOM::INIT(parent, _Extra=extraKeywords)
   IF NOT ok THEN Message, 'Failed to initialise the system component CatAtom.'

   self -> Report, /Completed

   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       PLOTCOORD CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the PLOTCOORD object class definition.
;       The PLOTCOORD object is subclassed from the CATATOM object.
;
;*****************************************************************************************************
PRO PlotCoord__DEFINE, class

   class = { PLOTCOORD, $
             _psysvar: !P, $
             _xsysvar: !X, $
             _ysysvar: !Y, $
             _zsysvar: !Z, $
             _mapsysvar: Ptr_New(), $
             _psysvar_old: !P, $
             _xsysvar_old: !X, $
             _ysysvar_old: !Y, $
             _zsysvar_old: !Z, $
             _mapsysvar_old: Ptr_New(), $
             INHERITS CATATOM $
           }

END