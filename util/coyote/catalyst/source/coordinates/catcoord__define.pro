;*****************************************************************************************************
;+
; NAME:
;       CATCOORD
;
; PURPOSE:
;
;       The purpose of this object is to create a coordinate system for data objects.
;       The object is meant to reside in the _COORDS field of CATDATAATOM objects.
;       The CATCOORD object creates a coordinate system at the time it is drawn by setting 
;       certain plotting system variables based on the position and range stored internally. 
;       Thus, for the coordinate system to be current, the DRAW method must be called prior 
;       to any action that relies on the coordinate system being established. (This is different,
;       in general, from the PLOTCOORD object).) It is assumed that at the time the DRAW method
;       is called, the current window (!D.Window) is the one for which data coordinates are desired.
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
;       theCoordObj = Obj_New('CatCoord')
;
; CLASS_STRUCTURE:
;
;   class = { CATCOORD, $
;             _position: DblArr(4), $  ; A four-element array representing the position of the "axes" in the window.
;             _xrange: DblArr(2), $    ; A two-element array representing the range of the X axis.
;             _xs: DblArr(2), $        ; A two-element array representing the scaling factors for the X axis.
;             _yrange: DblArr(2), $    ; A two-element array representing the range of the Y axis.
;             _ys: DblArr(2), $        ; A two-element array representing the scaling factors for the Y axis.
;             INHERITS CATATOM $
;           }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 15 September 2003.
;       Modified to *not* use PLOT command to set data coordinate system. 23 March 2004. DWF.
;       Changed the default postion to [0.0, 0.0, 1.0, 1.0] after spending *another* two hours
;          chasing a coordinate problem for the umpteenth time! 27 Dec 2004. DWF.
;       Added a modification to !P.CLIP in the DRAW method to affect proper clipping. 6 Oct 2006. DWF.
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
;       CATCOORD::DRAW
;
; PURPOSE:
;
;       This method establishes the coordinate system of the object by
;       setting parameters in the !X and !Y system variables.
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
PRO CatCoord::Draw, Extra=extrakeywords

   @cat_pro_error_handler

   ; Set up the data coordinate space by drawing a plot.
   self._xs = FSC_Normalize(self._xrange, Position=[self._position[0], self._position[2]])
   self._ys = FSC_Normalize(self._yrange, Position=[self._position[1], self._position[3]])

   !X.S = self._xs
   !Y.S = self._ys
   !X.Window = [self._position[0], self._position[2]]
   !Y.Window = [self._position[1], self._position[3]]

   ; Set the clipping rectangle.
   d = Convert_Coord(!X.Window, !Y.Window, /Normal, /To_Device)
   !P.Clip = [d[0,0], d[1,0], d[0,1], d[1,1], 0, 0]

   ; Set the calculated ranges.
   !X.CRange = self._xrange
   !Y.CRange = self._yrange

   ; Done.
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATCOORD::GETPROPERTY
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
;       POSITION:     A four-element array representing the position of the plot in the window.
;
;       XRANGE:       A two-element array representing the range for the X data axis.
;
;       XSCALE:       A two-element array representing the scaling factors for the X data axis.
;
;       YRANGE:       A two-element array representing the range for the Y data axis.
;
;       YSCALE:       A two-element array representing the scaling factors for the Y data axis.
;
;       _REF_EXTRA:   Any keywords appropriate for superclass GetProperty methods.
;-
;*****************************************************************************************************
PRO CatCoord::GetProperty, $
   POSITION=position, $
   XRANGE=xrange, $
   XSCALE=xscale, $
   YRANGE=yrange, $
   YSCALE=yscale, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Object properties.
   IF Arg_Present(autoscale) THEN autoscale = self._autoscale
   IF Arg_Present(position) THEN position = self._position
   IF Arg_Present(xrange) THEN xrange = self._xrange
   IF Arg_Present(xscale) THEN xscale = self._xs
   IF Arg_Present(yrange) THEN yrange = self._yrange
   IF Arg_Present(yscale) THEN yscale = self._ys

   ; Need superclass properties?
   IF N_Elements(extraKeywords) NE 0 THEN self -> CATATOM::GetProperty, _Extra=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATCOORD::SETPROPERTY
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
;       PARENT:       An object reference of the parent object. If provided, the CATCOORDS object
;                     will add itself to the parent with the COORDS_OBJECT keyword to SETPROPERTY.
;                     The parent should be a subclassed CATDATAATOM object.
;
;       POSITION:     A four-element array representing the position of the plot in the window.
;                     Use normalized coordinates (0 to 1) in this order: [x0, y0, x1, y1].
;
;       XRANGE:       A two-element array representing the range for the X data axis. Default is [0,1].
;
;       YRANGE:       A two-element array representing the range for the Y data axis. Default is [0,1].
;
;       _EXTRA:       Any keywords appropriate for superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO CatCoord::SetProperty, $
   PARENT=parent, $
   POSITION=position, $
   XRANGE=xrange, $
   YRANGE=yrange, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Object properties.
   IF N_Elements(position) NE 0 THEN self._position = position
   IF N_Elements(xrange) NE 0 THEN self._xrange = xrange
   IF N_Elements(yrange) NE 0 THEN self._yrange = yrange
   IF N_Elements(parent) NE 0 THEN BEGIN
      IF Obj_Isa_Valid(parent, 'CATDATAATOM') THEN parent -> SetProperty, Coords_Object=self ELSE $
         Message, 'Coordinate objects can only be added to sub-classed CATDATAATOM objects.'
   ENDIF

   ; Need superclass properties?
   IF N_Elements(extraKeywords) NE 0 THEN self -> CATATOM::SetProperty, _Extra=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATCOORD::CLEANUP
;
; PURPOSE:
;
;       This is the CATCOORD object class destructor method.
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
PRO CatCoord::CLEANUP

   @cat_pro_error_handler

   self -> CATATOM::CLEANUP

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATCOORD::INIT
;
; PURPOSE:
;
;       This is the CATCOORD object class creator method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       PARENT:       An object reference of the parent object. If provided, the CATCOORDS object
;                     will add itself to the parent with the COORDS_OBJECT keyword to SETPROPERTY.
;                     The parent should be a subclassed CATDATAATOM object.
;
;       POSITION:     A four-element array representing the position of the plot in the window.
;                     Use normalized coordinates (0 to 1) in this order: [x0, y0, x1, y1]. The
;                     default is [0,0,1,1].
;
;       XRANGE:       A two-element array representing the range for the X data axis. Default is [0,1].
;
;       YRANGE:       A two-element array representing the range for the Y data axis. Default is [0,1].
;
;       _REF_EXTRA:   Any keywords appropriate for superclass INIT methods.
;-
;*****************************************************************************************************
FUNCTION CatCoord::INIT, $
   PARENT=parent, $
   POSITION=position, $
   XRANGE=xrange, $
   YRANGE=yrange, $
   _REF_EXTRA=extraKeywords

   @cat_func_error_handler

   ; Check parameters.
   IF N_Elements(position) EQ 0 THEN position = [0,0,1,1]; [0.15, 0.125, 0.95, 0.95]
   IF N_Elements(xrange) EQ 0 THEN xrange = [0,1]
   IF N_Elements(yrange) EQ 0 THEN yrange = [0,1]

   ; Call the superclass INIT method.
   ok = self->CATATOM::INIT(_Extra=extraKeywords)
   IF NOT ok THEN Message, 'Failed to initialise the system component CatAtom.'

   ; Load the object.
   self._position = position
   self._xrange = xrange
   self._yrange = yrange
   IF N_Elements(parent) NE 0 THEN BEGIN
      IF Obj_Isa_Valid(parent, 'CATDATAATOM') THEN parent -> SetProperty, Coords_Object=self ELSE $
         Message, 'Coordinate objects can only be added to sub-classed CATDATAATOM objects.'
   ENDIF

   ; Done.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       CATCOORD CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the CATCOORD object class definition.
;       The CATCOORD object is subclassed from the CATATOM object.
;
;*****************************************************************************************************
PRO CatCoord__DEFINE, class

   class = { CATCOORD, $
             _position: DblArr(4), $     ; A four-element array representing the position of the axes in the window.
             _xrange: DblArr(2), $       ; A two-element array representing the range of the X axis.
             _xs: DblArr(2), $           ; A two-element array representing the scaling factors for the X axis.
             _yrange: DblArr(2), $       ; A two-element array representing the range of the Y axis.
             _ys: DblArr(2), $           ; A two-element array representing the scaling factors for the Y axis.
             INHERITS CATATOM $
           }

END