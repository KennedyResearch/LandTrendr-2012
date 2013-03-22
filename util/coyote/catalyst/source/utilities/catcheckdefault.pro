;*****************************************************************************************************
;+
; NAME:
;       CATCHECKDEFAULT
;
; PURPOSE:
;
;       This function enables the caller to test system settings. The intention
;       is to hide the mechanics of default storage from the caller. Use the
;       CatSetDefault procedure to set up the default variable before attempting
;       to retrieve it.
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
; CALLING_SEQUENCE:
;
;       result = CatCheckDefault (defaultName)
;
; INPUT_ARGUMENTS:
;
;     defaultName:   The name of the system setting you are checking.
;
; OUTPUT_KEYWORDS:
;
;     VALUE:         The value of the defaultName, if it exists.
;
; RETURN_VALUE:
;
;     result:         A 1 if the system setting exists, or a 0 if it does not exist.
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 12th March 2003
;       Modified to use the CatGetDefault SUCCESS flag. 25th July 2006. DWF.
;       Added VALUE keyword. 25th July 2006. DWF.
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
FUNCTION CatCheckDefault, defaultName, VALUE=value

   COMMON __$CatDefaults$_, defaultsObj

   ; Initialise an error handler
   CATCH, error
   IF (error NE 0) THEN RETURN, 0

   ; Attempt to retrieve the default from the defaults container
   value = defaultsObj -> GetValue (defaultName, Success=success)
   RETURN, success

END

