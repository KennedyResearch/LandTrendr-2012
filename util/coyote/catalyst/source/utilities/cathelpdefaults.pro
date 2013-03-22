;*****************************************************************************************************
;+
; NAME:
;       CATHELPDEFAULTS
;
; PURPOSE:
;
;       This procedure enables the caller to explore any defaults existing in
;       the defaults system.
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
;       CatHelpDefaults
;
; KEYWORDS:
;
;       OUTPUT:      An output keyword. If set to a named variable, returns the output in
;                    a string array instead of printing output to command log.
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 17th April 2003
;       Added OUTPUT keyword. August 18, 2005. DWF.
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
PRO CatHelpDefaults, Output=output

   COMMON __$CatDefaults$_, defaultsObj

   ; Prepare for output, if necessary.
   IF Arg_Present(output) THEN outputStr = Ptr_New()

   ; Scan the defaults container
   IF (OBJ_VALID (defaultsObj)) THEN $
   BEGIN
      IF (defaultsObj -> Count () EQ 0) THEN IF Arg_Present(output) THEN output = 'No defaults loaded.' ELSE $
         PRINT, 'No defaults loaded.'
      FOR defNo = 0, defaultsObj -> Count () - 1 DO $
      BEGIN
         default = defaultsObj -> Get (Position=defNo)
         IF (OBJ_VALID (default)) THEN $
         BEGIN
            default -> GetProperty, Name=valName
            HELP, default -> GetValue (), Output=valHelp
            IF Arg_Present(output) THEN BEGIN
               IF Ptr_Valid(outputStr) THEN *outputStr = [*outputStr, valName + STRMID (valHelp[0], 12)] ELSE $
                  outputStr = Ptr_New([valName + STRMID (valHelp[0], 12)])
            ENDIF ELSE PRINT, valName, STRMID (valHelp[0], 12)
         ENDIF
      ENDFOR
   ENDIF $
   ELSE IF Arg_Present(output) THEN output = 'No defaults exist.' ELSE PRINT, 'No defaults exist.'

  IF Ptr_Valid(outputStr) THEN BEGIN
      output = *outputStr
      Ptr_Free, outputStr
  ENDIF

END

