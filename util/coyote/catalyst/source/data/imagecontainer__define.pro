;*****************************************************************************************************
;+
; NAME:
;       IMAGECONTAINER__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide a container class for image
;       objects. Calling the DRAW method of a container calls the DRAW method
;       on all the items in the container. It is similar to a CatLayer object,
;       which was developed later for annotations. It was developed specifically
;       for ImageFrame objects, and still contains some unneeded dependencies
;       on that object.
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
;       theObject = Obj_New("IMAGECONTAINER")
;
; SUPERCLASSES:
;
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { IMAGECONTAINER, $
;             INHERITS CATATOM $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, May 24, 2005.
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
;       IMAGECONTAINER::ADD
;
; PURPOSE:
;
;       This method is where you can screen what kinds of objects are
;       added to this object's hierarchy. The method is not always needed.
;       If you do create it, be CERTAIN to call the superclass ADD method
;       or your program will not work correctly.
;
; SYNTAX:
;
;       theObject -> Add, object
;
; ARGUMENTS:
;
;     object:     The object to be added to this one.
;
; KEYWORDS:
;
;     _EXTRA:     Any keywords appropriate for the superclass Add method.
;-
;*****************************************************************************************************
PRO ImageContainer::Add, object, _EXTRA=extraKeywords

   @cat_pro_error_handler

   self -> CATATOM::Add, object, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       IMAGECONTAINER::DRAW
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
PRO ImageContainer::Draw, _Extra=extra

   @cat_pro_error_handler

   ; Draw any objects contained within this container.

   objects = self -> Get(/All, Count=objectCount)
   FOR j=0, objectCount-1 DO objects[j] -> Draw

   self -> CATATOM::Draw, _Extra=extra

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       IMAGECONTAINER::GETFRAME
;
; PURPOSE:
;
;       This method retrieves an image from its container, based on the image framenumber
;
; SYNTAX:
;
;       thisObject -> GetFrame, frameNumber
;
; ARGUMENTS:
;
;       framenumber: The image framenumber. If an image with this framenumber can be found, it
;                    is removed from the image container.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
FUNCTION ImageContainer::GetFrame, frameNumber

   @cat_func_error_handler

   ; Get all the frames
   objects = self -> Get(/All, Count=count)
   IF count EQ 0 THEN RETURN, Obj_New()

   ; Make sure you have a frame number.
   IF N_Elements(frameNumber) EQ 0 THEN frameNumber = 0

   ; Does one of these objects have the right framenumber?
   FOR j=0,count-1 DO BEGIN
      objects[j] -> GetProperty, FrameNumber=thisFrame
      IF thisFrame EQ frameNumber THEN BEGIN
         image = self -> Get(Position=j)
         BREAK
      ENDIF
   ENDFOR

   self -> Report, /Completed
   RETURN, image

END



;*****************************************************************************************************
;+
; NAME:
;       IMAGECONTAINER::REMOVEFRAME
;
; PURPOSE:
;
;       This method removes an image from its container, based on the image framenumber
;
; SYNTAX:
;
;       thisObject -> RemoveFrame, frameNumber
;
; ARGUMENTS:
;
;       framenumber: The image framenumber. If an image with this framenumber can be found, it
;                    is removed from the image container.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO ImageContainer::RemoveFrame, frameNumber

   @cat_pro_error_handler

   ; Get all the frames
   objects = self -> Get(/All, Count=count)
   IF count EQ 0 THEN RETURN

   ; Make sure you have a frame number.
   IF N_Elements(frameNumber) EQ 0 THEN frameNumber = 0

   ; Does one of these objects have the right framenumber?
   FOR j=0,count-1 DO BEGIN
      objects[j] -> GetProperty, FrameNumber=thisFrame
      IF thisFrame EQ frameNumber THEN BEGIN
         self -> Remove, Position=j
         BREAK
      ENDIF
   ENDFOR

   self -> Report, /Completed

END


;*****************************************************************************************************
;
; NAME:
;       IMAGECONTAINER CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the IMAGECONTAINER object.
;
;*****************************************************************************************************
PRO ImageContainer__DEFINE, class

   class = { IMAGECONTAINER, $
             INHERITS CATATOM $
           }

END

