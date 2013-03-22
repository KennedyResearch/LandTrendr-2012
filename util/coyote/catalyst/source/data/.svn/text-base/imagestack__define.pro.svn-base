;*****************************************************************************************************
;+
; NAME:
;       IMAGESTACK__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement an object for manipulating
;       an image cube or stack. An image stack is a 3D volume data set that can be
;       sliced in three orthogonal planes to produce image slices or frames.
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
;       theObject = Obj_New("IMAGESTACK")
;
; SUPERCLASSES:
;
;       CATDATAATOM
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { IMAGESTACK, $
;             xsize: 0L, $
;             ysize: 0L, $
;             zsize: 0L, $
;             INHERITS CATDATAATOM $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 25 February 2004.
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
;       IMAGESTACK::DRAW
;
; PURPOSE:
;
;       This method may or may not be needed by your object, depending
;       upon whether a graphical representation of the object is required.
;       If you wish the DRAW method to automatically propogates down to any
;       objects contained in this object's container, call the superclass DRAW
;       method.
;
; SYNTAX:
;
;       theObject -> Draw
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
PRO ImageStack::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; Draw any objects contained within this object.
   self -> CATDATAATOM::Draw, _Extra=extrakeywords

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       IMAGESTACK::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain IMAGESTACK properties. Be sure
;       you ALWAYS call the superclass GETPROPERTY method if you have extra
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
;     DIMENSIONS:     A three-element array containing the dimensions of the image stack.
;
;     IMAGESTACK:     The image stack data.
;
;     MAXVALUE:       The maximum value of the data in the stack.
;
;     MINVALUE:       The minimum value of the data in the stack.
;
;     XSIZE:          The X size of the data stack.
;
;     YSIZE:          The Y size of the data stack.
;
;     ZSIZE:          The Z size of the data stack.
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO ImageStack::GetProperty, $
   DIMENSIONS=dimensions, $
   IMAGESTACK=imagestack, $
   MAXVALUE=maxvalue, $
   MINVALUE=minvalue, $
   XSIZE=xsize, $
   YSIZE=ysize, $
   ZSIZE=zsize, $
    _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   dimensions = Size(*self._dataPtr, /Dimensions)
   IF Arg_Present(maxvalue) THEN maxvalue = Max(*self._dataPtr)
   IF Arg_Present(minvalue) THEN minvalue = Min(*self._dataPtr)
   IF Arg_Present(xsize) THEN xsize = dimensions[0]
   IF Arg_Present(ysize) THEN ysize = dimensions[1]
   IF Arg_Present(zsize) THEN zsize = dimensions[2]
   IF Arg_Present(imagestack) THEN IF Ptr_Valid(self._dataPtr) THEN imagestack = *self._dataPtr

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATDATAATOM::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       IMAGESTACK::GETFRAME
;
; PURPOSE:
;
;       This function method returns the 2D image with the corresponding frame or slice number.
;       By default, the slice is taken in the XY plane of the image stack, but slices in other
;       planes can be returned by setting the appropriate keywords.
;
; SYNTAX:
;
;       theFrame = theObject -> GetFrame(number)
;
; ARGUMENTS:
;
;     number:      The frame or slice number of the image desired in the image stack.
;
; KEYWORDS:
;
;     ORIENTATION: Slices can be taken in the X, Y, and Z directions. This keyword accepts
;                  a single character string that selects the directions. "Z": the slice is taken
;                  from the XY plane (the default). "X" the slice is taken from the YZ plane. "Y" the
;                  slice is taken from the XZ plane.
;
;     THICK:       Normally, a slice one image frame thick is returned. If this keyword is
;                  set to a value greater than 1, then THICK number of image frames are averaged
;                  together to produce the slice that is returned. The slice will go from number
;                  to (number + thick - 1). If there are not enough image frames to produce a slice
;                  with a particular thickness, then as many frames as possible will be averaged.
;                  In other words, if there are 10 slices and you ask for slice number 8 with a
;                  thickness of 3, frame indices 8, 9, and 10 should be averaged. In this case,
;                  there is no frame index 10, so only two frames will be used in calculating
;                  the average frame.
;-
;*****************************************************************************************************
FUNCTION ImageStack::GetFrame, number, $
   ORIENTATION=orientation, $
   THICK=thick

   @cat_func_error_handler

   IF N_Elements(number) EQ 0 THEN s_number = 0 ELSE s_number = Round(number)
   IF N_Elements(orientation) EQ 0 THEN orientation = 'Z' ELSE orientation = StrUpCase(orientation)
   IF N_Elements(thick) EQ 0 THEN thick = 1 ELSE thick = (1 > Round(thick))

   ; What kind of slice do you want?
   CASE orientation OF

      "X": $ ; X Slice.
            BEGIN
               s_number = 0 > s_number
               IF s_number GT (self.xsize-1) THEN Message, 'Slice number is outside of image stack range.'
               upperRange = (s_number + (thick-1)) < (self.xsize-1)
               slice = (*self._dataPtr)[s_number:upperRange,*,*]
               IF (upperRange-s_number) GT 0 THEN BEGIN
                  slice = Total(slice, 1) / (upperRange-s_number+1)
               ENDIF
               slice = Reform(slice, self.ysize, self.zsize)
            END

      "Y": $ ; Y Slice.
            BEGIN
               s_number = 0 > s_number
               IF s_number GT (self.ysize-1) THEN Message, 'Slice number is outside of image stack range.'
               upperRange = (s_number + (thick-1)) < (self.ysize-1)
               slice = (*self._dataPtr)[*,s_number:upperRange,*]
               IF (upperRange-s_number) GT 0 THEN BEGIN
                  slice = Total(slice, 2) / (upperRange-s_number+1)
               ENDIF
               slice = Reform(slice, self.xsize, self.zsize)
            END

      "Z": BEGIN ; Z Slice.
               s_number = 0 > s_number
               IF s_number GT (self.zsize-1) THEN Message, 'Slice number is outside of image stack range.'
               upperRange = (s_number + (thick-1)) < (self.zsize-1)
               slice = (*self._dataPtr)[*,*,s_number:upperRange]
               IF (upperRange-s_number) GT 0 THEN BEGIN
                  slice = Total(slice, 3) / (upperRange-s_number+1)
               ENDIF
               slice = Reform(slice, self.xsize, self.ysize)
            END

      ELSE: Message, 'Unknown slice orientation: ' + orientation
   ENDCASE

   self -> Report, /Completed
   RETURN, slice

END



;*****************************************************************************************************
;+
; NAME:
;       IMAGESTACK::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the IMAGESTACK object's properties.
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
;     IMAGESTACK:  A 3D data set representing an image stack.
;
;     _EXTRA:     Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO ImageStack::SetProperty, IMAGESTACK=dataStack, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; If you have a data stack, get information from it.
   IF N_Elements(dataStack) NE 0 THEN BEGIN

      IF Size(dataStack, N_Dimensions=1) NE 3 THEN Message, 'Data must be 3D.'
      dims = Size(dataStack, Dimensions=1)
      self.xsize = dims[0]
      self.ysize = dims[1]
      self.zsize = dims[2]
      self -> SetData, dataStack

   ENDIF

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATDATAATOM::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       IMAGESTACK::CLEANUP
;
; PURPOSE:
;
;       This is the IMAGESTACK object class destructor method.
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
PRO ImageStack::CLEANUP

   @cat_pro_error_handler

   self -> CATDATAATOM::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       IMAGESTACK::INIT
;
; PURPOSE:
;
;       This is the IMAGESTACK object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     dataStack:   The 3D volume data set that is the image stack.
;
; KEYWORDS:
;
;
;     _EXTRA:     Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION ImageStack::INIT, dataStack, _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   ; If you have a data stack, get information from it.
   IF N_Elements(dataStack) NE 0 THEN BEGIN

      IF Size(dataStack, N_Dimensions=1) NE 3 THEN Message, 'Data must be 3D.'
      dims = Size(dataStack, Dimensions=1)
      self.xsize = dims[0]
      self.ysize = dims[1]
      self.zsize = dims[2]

   ENDIF

   ; Initialize superclass.
   ok = self -> CATDATAATOM::INIT (dataStack, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       IMAGESTACK CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the IMAGESTACK object.
;
;*****************************************************************************************************
PRO ImageStack__DEFINE, class

   class = { IMAGESTACK, $
             xsize: 0L, $
             ysize: 0L, $
             zsize: 0L, $
             INHERITS CATDATAATOM $
           }

END


;*****************************************************************************************************
;
; NAME:
;       IMAGESTACK TEST PROGRAM
;
; PURPOSE:
;
;       This is the test program for the IMAGESTACK object. It may not
;       be required in your object.
;
;*****************************************************************************************************
PRO ImageStack_Test

   LoadCT, 0, /Silent
   images = LoadData(8)
   imagestack = Obj_New('ImageStack', images)
   imageContainer = Obj_New('ImageContainer')
   imageStack -> GetProperty, ZSize=number
   FOR j=0,number-1 DO BEGIN
      coords = Obj_New('CatCoord', XRange=[0,80], YRange=[0,100])
      frame = Obj_New('ImageFrame', imageStack, Framenumber=j, Coord_Object=coords)
      frame -> Add, Obj_New('TextLine', StrTrim(j,2), x=10, y=10, Coord_Object=coords)
      imageContainer -> Add, frame
   ENDFOR
   tlb = Obj_New('TOPLEVELBASE')
   strip = Obj_New('IMAGESTRIP', tlb, ImageContainer=imageContainer, XSIZE=80, YSize=100, Frames=8, Start_Frame=43)

   strip -> GetProperty, DrawObject=drawID
   tlb -> Draw, /Center

END