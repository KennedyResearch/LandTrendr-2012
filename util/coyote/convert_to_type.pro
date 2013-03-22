;+
; NAME:
;       CONVERT_TO_TYPE
;
; PURPOSE:
;
;       Converts its input argument to a specified data type.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:

;       Utilities
;
; CALLING SEQUENCE:
;
;       result = Convert_To_Type(input, type)
;
; INPUT_PARAMETERS:
;
;       input:          The input data to be converted.
;       type:           The data type. Accepts values as given by Size(var, /TNAME) or Size(var, /TYPE).
;
; OUTPUT_PARAMETERS:
;
;      result:          The input data is converted to specified data type.
;
; KEYWORDS:
;
;     None.
;
; RESTRICTIONS:
;
;     Data types STRUCT, POINTER, and OBJREF are not allowed.
;
; MODIFICATION HISTORY:
;
;     Written by David W. Fanning, 19 February 2006.
;-
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
FUNCTION Convert_To_Type, input, type

   ; Return to caller on error.
   On_Error, 2

   ; Two positional parameters are required.
   IF N_Params() NE 2 THEN Message, 'Two input parameters (INPUT and TYPE) are required.'

   ; If type is a string, turn it into a number.
   IF Size(type, /TNAME) EQ 'STRING' THEN BEGIN

      type = StrUpCase(type[0])
      CASE type OF
         'BYTE': type = 1
         'INT': type = 2
         'LONG': type = 3
         'FLOAT': type = 4
         'DOUBLE': type = 5
         'COMPLEX': type = 6
         'STRING': type = 7
         'DCOMPLEX': type = 9
         'UNIT': type = 12
         'ULONG': type = 13
         'LONG64': type = 14
         'ULONG64': type = 15
         ELSE: Message, 'Unable to convert input to type: ' + StrUpCase(type)
      ENDCASE

   ENDIF ELSE BEGIN

      ; Only certain kinds of data conversions can occur.
      type = type[0]
      CASE 1 OF
         (type LT 1): Message, 'Unable to convert input to UNDEFINED data type.'
         (type EQ 8): Message, 'Unable to convert input to STRUCTURE data type.'
         (type EQ 10): Message, 'Unable to convert input to POINTER data type.'
         (type EQ 11): Message, 'Unable to convert input to OBJECT data type.'
         (type GT 15): Message, 'Unable to convert undefined data type: ', StrTrim(type) + '.'
         ELSE:
      ENDCASE
   ENDELSE

   ; Do the conversion.
   CASE type OF
      1: output = BYTE(input)
      2: output = FIX(input)
      3: output = LONG(input)
      4: output = FLOAT(input)
      5: output = DOUBLE(input)
      6: output = COMPLEX(input)
      7: output = STRING(input)
      9: output = DCOMPLEX(input)
      12: output = UINT(input)
      13: output = ULONG(input)
      14: output = LONG64(input)
      15: output = ULONG64(input)
   ENDCASE

   RETURN, output

END