;*****************************************************************************************************
;+
; NAME:
;       CatKillNotify
;
; PURPOSE:
;
;       The purpose of this utility routine is to make sure objects in the
;       object hierarchy properly destroy themselves when the widget is
;       destroyed. All object widgets will be assigned this routine to
;       be the Kill_Notify routine. If a user-defined Kill_Notify method
;       is defined for the widget object (and the widget object is subclassed
;       from WidgetAtom), it will be called before the object is destroyed.
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
;       This routine is called by the IDL system, not by a user.
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 12 June 2002.
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
PRO CatKillNotify, id

      ; Error handling.

   @cat_pro_error_handler

      ; Get the object reference.

   WIDGET_CONTROL, id, GET_UVALUE=object

      ; Is this a valid widgetAtom? If so, call the KILL_NOTIFY method and
      ; destroy the object.

   IF (obj_isa_valid (object, 'WidgetAtom')) THEN $
   BEGIN

      object -> WidgetAtom::GetProperty, Kill_Notify=kill_notify
      IF kill_notify THEN object -> Kill_Notify

   ENDIF

      ; The object must be destroyed.

   IF Obj_Valid(object) THEN OBJ_DESTROY, object

END