;*****************************************************************************************************
;+
; NAME:
;       CATGETDEFAULT
;
; PURPOSE:
;
;       This function enables the caller to get system settings. The intention
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
; CALLING SEQUENCE:
;
;       result = CatGetDefault (DefaultName)
;
; ARGUMENTS:
;
;     defaultName:   The name of the system setting you are getting the value for.
;
; KEYWORDS:
;
;     SUCCESS:       An output keyword set to 1 if a system setting with this name was
;                    found and its value returned successfully. Otherwise, the value is
;                    set to 0.
;
; RETURN_VALUE:
;
;     result:        The value of the system setting.
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 6th March 2003
;       Added SUCCESS keyword 16 December 2004. DWF.
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
FUNCTION CatGetDefault, defaultName, Success=success

   COMMON __$CatDefaults$_, defaultsObj

   ; Set up error handling
   CATCH, error
   IF (error NE 0) THEN RETURN, ''
   success = 0B

   ; Attempt to retrieve the default from the defaults container
   result = defaultsObj -> GetValue (defaultName, Success=success)
   IF N_Elements(result) EQ 1 THEN RETURN, result[0] ELSE RETURN, result

END

