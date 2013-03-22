;*****************************************************************************************************
;+
; NAME:
;       CATRESETSYSVARS
;
; PURPOSE:
;
;       The program resets all plotting and mapping system variables to their default values.
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
;       rCatResetSysVars
;
; INPUT_ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 12th March 2003
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
PRO CatResetSysVars

   ; Some variables will depend on current decomposition state.
   Device, Get_Decomposed=theState

   ; !P System Variable

   !P.BACKGROUND = 0L
   !P.CHARSIZE = 0.000000
   !P.CHARTHICK = 0.000000
   !P.CLIP = LonArr(6)
   IF theState EQ 1 THEN !P.COLOR = 16777215 ELSE !P.COLOR = !D.Table_Size-1
   !P.FONT = -1L
   !P.LINESTYLE = 0L
   !P.MULTI = LonArr(5)
   !P.NOCLIP = 0L
   !P.NOERASE = 0L
   !P.NSUM =  0L
   !P.POSITION = [0.15, 0.125, 0.95, 0.95] ; Can't set to zero, for some reason. :-(
   !P.PSYM = 0L
   !P.REGION = FltArr(4)
   !P.SUBTITLE = ''
   !P.SYMSIZE = 0.000000
   !P.T = DblArr(4,4)
   !P.T3D = 0L
   !P.THICK = 0.000000
   !P.TITLE = ''
   !P.TICKLEN = 0.0200000
   !P.CHANNEL = 0L

   ;!X System Variable

   !X.TITLE = ''
   !X.TYPE = 0L
   !X.STYLE = 0L
   !X.TICKS = 0L
   !X.TICKLEN = 0.0
   !X.THICK = 0.0
   !X.RANGE = DblArr(2)
   !X.CRANGE = DblArr(2)
   !X.S = DblArr(2)
   !X.MARGIN = FltArr(2)
   !X.OMARGIN = FltArr(2)
   !X.WINDOW = FltArr(2)
   !X.REGION = FltArr(2)
   !X.CHARSIZE = 0.0
   !X.MINOR = 0L
   !X.TICKV = DblArr(60)
   !X.TICKNAME = StrArr(60)
   !X.GRIDSTYLE = 0L
   !X.TICKFORMAT =  StrArr(10)
   !X.TICKLAYOUT = 0L
   !X.TICKUNITS = StrArr(10)

   ; !Y System Variable

   !Y.TITLE = ''
   !Y.TYPE = 0L
   !Y.STYLE = 0L
   !Y.TICKS = 0L
   !Y.TICKLEN = 0.0
   !Y.THICK = 0.0
   !Y.RANGE = DblArr(2)
   !Y.CRANGE = DblArr(2)
   !Y.S = DblArr(2)
   !Y.MARGIN = FltArr(2)
   !Y.OMARGIN = FltArr(2)
   !Y.WINDOW = FltArr(2)
   !Y.REGION = FltArr(2)
   !Y.CHARSIZE = 0.0
   !Y.MINOR = 0L
   !Y.TICKV = DblArr(60)
   !Y.TICKNAME = StrArr(60)
   !Y.GRIDSTYLE = 0L
   !Y.TICKFORMAT =  StrArr(10)
   !Y.TICKLAYOUT = 0L
   !Y.TICKUNITS = StrArr(10)

   ;!Z System Variable

   !Z.TITLE = ''
   !Z.TYPE = 0L
   !Z.STYLE = 0L
   !Z.TICKS = 0L
   !Z.TICKLEN = 0.0
   !Z.THICK = 0.0
   !Z.RANGE = DblArr(2)
   !Z.CRANGE = DblArr(2)
   !Z.S = DblArr(2)
   !Z.MARGIN = FltArr(2)
   !Z.OMARGIN = FltArr(2)
   !Z.WINDOW = FltArr(2)
   !Z.REGION = FltArr(2)
   !Z.CHARSIZE = 0.0
   !Z.MINOR = 0L
   !Z.TICKV = DblArr(60)
   !Z.TICKNAME = StrArr(60)
   !Z.GRIDSTYLE = 0L
   !Z.TICKFORMAT = StrArr(10)
   !Z.TICKLAYOUT = 0L
   !Z.TICKUNITS = StrArr(10)

   ; !Map System Variable

   !MAP.PROJECTION = 0L
   !MAP.SIMPLE = 0L
   !MAP.FILL_METHOD = 0L
   !MAP.UP_FLAGS = 0L
   !MAP.UP_NAME = ""
   !MAP.P0LON =  0.0D
   !MAP.P0LAT =  0.0D
   !MAP.U0 =  0.0D
   !MAP.V0 =  0.0D
   !MAP.SINO =  0.0D
   !MAP.COSO =  0.0D
   !MAP.ROTATION =  0.0D
   !MAP.SINR =  0.0D
   !MAP.COSR =  0.0D
   !MAP.A =  0.0D
   !MAP.E2 =  0.0D
   !MAP.UV = DblArr(2)
   !MAP.POLE = DblArr(7)
   !MAP.UV_BOX = DblArr(4)
   !MAP.LL_BOX = DblArr(4)
   !MAP.SEGMENT_LENGTH =  0.0D
   !MAP.P = DblArr(16)
   !MAP.PIPELINE = DblArr(8,12)

END
