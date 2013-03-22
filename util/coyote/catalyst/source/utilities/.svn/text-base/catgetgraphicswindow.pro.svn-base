;*****************************************************************************************************
;+
; NAME:
;       CatGetGraphicsWindow
;
; PURPOSE:
;
;       The purpose of this utility routine is to allow objects lower down in a graphics
;       hierarchy to obtain the object reference of a drawable graphics window (either
;       a PIXMAPWIDGET or a DRAWWIDGET or a subclass of one of these) in the graphics 
;       hierarchy. The FIRST_PARENT of each object in the hierarchy is traversed in the
;       search for a PIXMAPWIDGET or a DRAWWIDGET object.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;
; CATEGORY:
;
;       Programming.
;
; CALLING SEQUENCE:
;
;       graphicsWindowObject = CatGetGraphicsWindow(object)
;       
; RETURN VALUE:
;
;       graphicsWindowObject:   The first PIXMAPWIDGET or DRAWWIDGET object found in the object's hierarchy.
;                 A null object is returned if a PIXMAPWIDGET or DRAWWIDGET cannot be found.
;                 
; INPUTS:
;
;       object:   The object which is requesting information and in whose hierarchy the
;                 search is conducted. This object must be sub-classed from 'CATATOM'.
;
; KEYWORDS:
;
;      None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 2 October 2008.
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
FUNCTION CatGetGraphicsWindow, object

   @cat_func_error_handler

   ; Don't change the following line of code or "self" will turn into another
   ; object. The incoming object must NOT change!
   thisObject = object

   IF (NOT OBJ_ISA_VALID (thisObject, 'CATATOM')) THEN $
      MESSAGE, 'Requesting object must be a valid CATATOM object.'

   WHILE  (~OBJ_ISA_VALID(thisObject)) DO BEGIN

     ; Pixmaps or draw widget objects should be returned.
     IF OBJ_ISA(thisObject, "PIXMAPWIDGET") THEN RETURN, thisObject
     IF OBJ_ISA(thisObject, "DRAWWIDGET") THEN RETURN, thisObject

     thisObject -> GetProperty, First_Parent=objParent
     IF (~OBJ_VALID(objParent)) THEN BREAK ; Drop out if you are at top of hierarchy.
     thisObject = objParent

   ENDWHILE

   ; Return a PIXMAPWIDGET or DRAWWIDGET, otherwise a null object.
   IF OBJ_ISA(thisObject, "PIXMAPWIDGET") THEN RETURN, thisObject
   IF OBJ_ISA(thisObject, "DRAWWIDGET") THEN RETURN, thisObject
   RETURN, Obj_New()
END