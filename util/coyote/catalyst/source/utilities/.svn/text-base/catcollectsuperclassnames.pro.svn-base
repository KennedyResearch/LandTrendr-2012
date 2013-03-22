;*****************************************************************************************************
;+
; NAME:
;       CATCOLLECTSUPERCLASSNAMES
;
; PURPOSE:
;
;       The purpose of this utility routine is to collect all the Catalyst object's
;       superclass names and return them as the result of the function. The names
;       IDL_CONTAINTER and IDLITCOMPONENT are removed from the names array before
;       the names array is returned.
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
;       Catalyst Documentation.
;
; CALLING_SEQUENCE:
;
;       superclassNames = CatCollectSuperclassNames(object)
;
; ARGUMENTS:
;
;       object:           The object reference to check (object reference). (This may also be the object
;                         classname.)
;
; RETURN_VALUE:
;
;       superclassNames:  A string array listing the classname of the object (in position 0)
;                         and the names of all of the superclasses for that object (in subsequent positions).
;
; EXAMPLE:
;
;       IDL> theObject = Obj_New('CatImage2d', Loaddata(7))
;       IDL> Print, CatCollectSuperclassNames(theObject)
;            CATIMAGE2D CATIMAGEDATA CATDATAATOM CATATOM CATCONTAINER
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 5 January 2004.
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
FUNCTION CatCollectSuperClassNames, objectName

   compile_opt idl2

   ; Return to caller on error.
   On_Error, 1

   ; Is the argument an object reference or a string? If object reference,
   ; obtain the object class name.
   type = Size(objectName, /TName)
   IF type EQ 'OBJREF' THEN theObjectName = Obj_Class(objectName) ELSE theObjectName = objectName
   type = Size(theObjectName, /TName)
   IF type NE 'STRING' THEN Message, 'Object class name is required parameter.'

   ; Find the superclass names of this object. Create appropriate
   ; return values. If one or more superclass names, use recursion.
   superclasses = Obj_Class(theObjectName, /Superclass, Count=theCount)
   CASE theCount OF
      0: retValue = [theObjectName]
      1: retValue = [theObjectName, CatCollectSuperClassNames(superclasses)]
      ELSE: BEGIN
         FOR j=0,theCount-1 DO theObjectName = [theObjectName, CatCollectSuperClassNames(superclasses[j])]
         retValue = theObjectName
         END
   ENDCASE

   ; Remove any reference to IDL_CONTAINER or IDLITCOMPONENT from the name list.
    index = Where(retValue NE 'IDL_CONTAINER', count)
    IF count GT 0 THEN retValue = retValue[index]
    index = Where(retValue NE 'IDLITCOMPONENT', count)
    IF count GT 0 THEN retValue = retValue[index]

    RETURN, retValue
END