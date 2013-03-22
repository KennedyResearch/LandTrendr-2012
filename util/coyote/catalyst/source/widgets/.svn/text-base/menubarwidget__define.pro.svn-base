;*****************************************************************************************************
;+
; NAME:
;       MENUBARWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to wrap a top-level base menubar widget identifier
;       up in a widget object. This object should never be defined by the user. Rather, it
;       is defined automatically when a TOPLEVELBASE object is created.
;
; AUTHOR:
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
;       Object widgets.
;
; SYNTAX:
;
;       None. This is called automatically from the TOPLEVELBASE INIT method.
;
; SUPERCLASSES:
;
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;    class = { MENUBARWIDGET, $       ; The MENUBARWIDGET object class name.
;              INHERITS WidgetAtom $  ; Subclassed from WIDGETATOM.
;            }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W.Fanning, 28 June 2002.
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
;+
; NAME:
;       MENUBARWIDGET::ADD
;
; PURPOSE:
;
;       This method adds an object widget to the menubase object widget. The
;       added object widget must be subclassed from the BUTTONWIDGET object.
;
; SYNTAX:
;
;       self -> Add, object
;
; ARGUMENTS:
;
;       object: The object to add to the container. The object must be
;          subclassed from the BUTTONWIDGET object. (Required)
;
; KEYWORDS:
;
;       _EXTRA: Any keyword appropriate for superclass ADD methods.
;
;-
;*****************************************************************************************************
PRO MENUBARWIDGET::Add, object, _EXTRA=extraKeywords

      ; Error handling.

   @cat_pro_error_handler

   IF N_Elements(object) EQ 0 THEN Message, 'Required object is missing.'
   IF OBJ_ISA_VALID(object, 'BUTTONWIDGET') EQ 0 THEN $
      Message, 'Supplied object is invalid or not subclassed from BUTTONWIDGET.'

      ; Set the menu flag for this buttonobject, so it can also be a parent
      ; for button children. This is similar to how menu buttons work currently
      ; in widgets.

   object -> SetProperty, MENU=1
   self -> WidgetAtom::Add, object, _EXTRA=extraKeywords
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       MENUBARWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the object's properties. It is really only a wrapper to
;       the WIDGETATOM SETPROPERTY method written to prevent XSIZE and YSIZE keywords being
;       used as these are invalid for menu widgets.
;
; SYNTAX:
;
;       self -> SetProperty, UVALUE=theValue
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       XSIZE:  This keyword is ignored by the MENUBARWIDGET object.
;
;       YSIZE:  This keyword is ignored by the MENUBARWIDGET object.
;
;       _EXTRA: Any keyword appropriate for WIDGETATOM::SETPROPERTY methods.
;
;-
;*****************************************************************************************************
PRO MenuBarWidget::SetProperty, $
               XSIZE=xsize, $
               YSIZE=ysize, $
               _EXTRA=extraKeywords

   @cat_pro_error_handler
   self -> WidgetAtom::SetProperty, _EXTRA=extraKeywords
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       MENUBARWIDGET::INIT
;
; PURPOSE:
;
;       This is the MENUBARWIDGET object class initialization method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       ID: The menubar identifier obtained from the MBAR keyword in the creation of a
;           TOPLEVELBASE object.

; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
FUNCTION MENUBARWIDGET::INIT, parent, id, _EXTRA=extraKeywords

      ; Error handling.

   @cat_func_error_handler

      ; A menubar identifier is requied.

   IF N_Elements(id) EQ 0 THEN Message, 'A menu bar identifier is required.'

      ; Call the super class INIT method.

   ok = self -> WidgetAtom::INIT (parent, id, /INVISIBLE, _EXTRA=extrakeywords)
   IF NOT ok THEN Message, 'WIDGETATOM initialization failed. Returning.'

      ; Clean up and return status.

   self -> Report, /Completed
   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       BASEWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This procedure implements the BASEWIDGET object class definition.
;       The BASEWIDGET object is subclassed from the WIDGETATOM object.
;
;*****************************************************************************************************
PRO MENUBARWIDGET__DEFINE, class

   class =  { MENUBARWIDGET, $       ; The MENUBARWIDGET object class name.
              INHERITS WidgetAtom $  ; Subclassed from WIDGETATOM.
            }
END