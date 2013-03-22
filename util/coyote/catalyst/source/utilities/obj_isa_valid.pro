;*****************************************************************************************************
;+
; NAME:
;       OBJ_ISA_VALID
;
; PURPOSE:
;
;       The purpose of this utility routine is to check to be
;       sure the object argument is valid and belongs to the
;       specified class.
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
;       check = OBJ_ISA_VALID(object, classname)
;
; ARGUMENTS:
;
;       object - The object reference to check (object reference).
;       classname - The object class name to check (string).
;
; RETURN VALUE:
;
;       check - Returns 1 if the object is valid and a member of the
;          specified class. Returns 0 otherwise.
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 12 June 2002.
;
;       Added OBJARR handling: David Burridge, 23rd January 2003.
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
FUNCTION OBJ_ISA_VALID, object, classname

   noObjs = N_ELEMENTS (object)
   Case noObjs OF
      0 : Return, 0B
      1 : ok = 1B
      ELSE : ok = BYTARR (noObjs) + 1B
   ENDCASE

   IF (N_ELEMENTS (classname) NE 1) THEN RETURN, ok * 0B

   FOR objNo = 0, noObjs - 1 DO $
   BEGIN
      IF (NOT OBJ_VALID (object[objNo])) THEN ok [objNo] = 0B $
      ELSE IF (NOT OBJ_ISA (object[objNo], classname)) THEN ok [objNo] = 0B
   ENDFOR

   RETURN, ok
END