;*****************************************************************************************************
;+
; NAME:
;       CATREFRESHDRAW
;
; PURPOSE:
;
;       The purpose of this utility routine is to allow objects lower down in a DRAW
;       hierarchy to request DRAW events from object higher in the hierarchy. For example,
;       if an IMGAXIS object needed to refresh itself, it would need to call the DRAW
;       method on the DRAWWIDGET at the top of the graphics hierarchy. It couldn't just
;       draw itself. There may be one or more objects between the object that requests a
;       DRAW and the one that needs to perform the DRAW.
;
;       If the "STOP_AT" keyword is not specified, the redraw request will
;       progress to the top of the object hierarchy. If the final object is
;       a DRAWWIDGET, the DRAW method is called with the ERASE_WINDOW keyword set.
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
;       CatRefreshDraw, object, STOP_AT=thisObjectClass
;
; INPUTS:
;
;       object:   The object which requires the refresh. This must be sub-
;                 classed from 'CATATOM'.
;
; KEYWORDS:
;
;      NOERASE:       If the object whose draw method is called is a DRAWWIDGET object,
;                     then the draw method is called with the ERASE_WINDOW keyword set.
;                     By setting this NOERASE keyword, you will prevent the ERASE_WINDOW
;                     keyword from being set.
;
;      TARGET_WINDOW: The target display window object. Sent to the DRAW method of any
;                     DRAWWIDGET that is found. Ignored in any other case.
;
;      REQUESTER:     Set this optional keyword equal the the object reference of the object
;                     requesting the DRAW.
;
;      STOP_AT:       Set this keyword to the object class name at
;                     which the search is to stop.
;
;                     For example, if "DrawWidget" is specified, the draw will
;                     propogate up the hierarchy until a DRAWWIDGET object
;                     (or a sub-class) is encountered. This object will have its
;                     DRAW method called just before this routine completes.
;
; NOTES:
;
;       Note that any PIXMAPWIDGET encountered during traversal will be refreshed.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 21 July 2003.
;       Added TARGET_WINDOW keyword, 11 July 2005. DWF.
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
PRO CatRefreshDraw, object, $
   NOERASE=noerase, $
   REQUESTER=requester, $
   TARGET_WINDOW=target_window, $
   STOP_AT=stop_at

   @cat_pro_error_handler

   ; Don't change the following line of code or "self" will turn into another
   ; object. The incoming object must NOT change!
   thisObject = object

   IF (~OBJ_ISA_VALID (thisObject, 'CATATOM')) THEN $
      MESSAGE, 'Object must be a valid CATATOM object.'

   WHILE (~OBJ_ISA (thisObject, stop_at)) DO BEGIN

     ; Pixmap should be refreshed.
     IF OBJ_CLASS(thisObject) EQ "PIXMAPWIDGET" THEN thisObject -> Refresh

     thisObject -> GetProperty, First_Parent=objParent
     IF (~OBJ_VALID(objParent)) THEN BREAK ; Drop out if you are at top of hierarchy.
     thisObject = objParent

   ENDWHILE

   ; Special handling for draw widgets, because you may want the window erased.
   IF Obj_Class(thisObject) EQ 'DRAWWIDGET' THEN BEGIN
      thisObject -> Refresh, TARGET_WINDOW=target_window, REQUESTER=requester, Erase_Window=(1-Keyword_Set(noerase))
   ENDIF ELSE BEGIN
      thisObject -> Draw, REQUESTER=requester, TARGET_WINDOW=target_window
   ENDELSE
END