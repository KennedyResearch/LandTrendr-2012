;*****************************************************************************************************
;+
; NAME:
;       CATOBJECTVIEW
;
; PURPOSE:
;
;       This object implements a methodology for displaying object graphics in
;       the Catalyst Object Library. The object contains an IDLgrView or IDLgrScene
;       object, written in object graphics that can be displayed in an ODrawWidget
;       object. If fact, the ODrawWidget will *only* accept a CATOBJECTVEW object.
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
; SUPERCLASSES:
;
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; SYNTAX:
;
;      objectView = OBJ_NEW ('CATOBJECTVIEW')
;
; CLASS_STRUCTURE:
;
;   class = { CATOBJECTVIEW, $       ; The object class definition.
;             INHERITS CATATOM, $    ; Part of the Catalyst Object Library.
;             _no_destroy: 0L, $     ; A flag that governs the destroying of theView.
;             _theView: Obj_New(), $ ; The IDLgrView or IDLgrScene object to be displayed.
;           }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 30 June 2003.
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
;       CATOBJECTVIEW::CREATEVIEW
;
; PURPOSE:
;
;       This method is a dummy method. Subclassed objects should override this method
;       to construct the IDLgrView object that is stored in the object
;
; SYNTAX:
;
;       aView -> CreateView
;
; ARGUMENTS:
;
;      None.
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
PRO CatObjectView::CreateView

   @cat_pro_error_handler

   Message, 'Unhandled call to CatObjectView::CreateView'

END


;*****************************************************************************************************
;+
; NAME:
;       CATOBJECTVIEW::EVENTHANDLER
;
; PURPOSE:
;
;       A dummy event handler method to catch widget events. If events come here, they
;       are simply displayed. If you wish to use this event handler, sub-class the CatObjectView
;       object and write your own event handler.
;
; SYNTAX:
;
;       Called from the CATEVENTDISPATCHER utility routine.
;
; ARGUMENTS:
;
;       Event:  The event structure from a widget event.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO CatObjectView::EventHandler, event

      ; Set up an error handler
   @cat_pro_error_handler

   Help, event, /Structure

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CATOBJECTVIEW::GETPROPERTY
;
; PURPOSE:
;
;       This method enables the getting of the CATOBJECTVIEW object properties.
;
; SYNTAX:
;
;       CATOBJECTVIEW -> GetProperty ...
;
; ARGUMENTS:
;
;      None.
;
; KEYWORDS:
;
;     NO_DESTROY:  The current state of the NO_DESTROY flag.
;
;     VIEW:        The IDLgrView or IDLgrScene object contained in the object.
;
;     _EXTRA:      Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO CatObjectView::GetProperty, NO_DESTROY=no_destroy, VIEW=theView, _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(theView) THEN theView = self._theView
   IF Arg_Present(no_destroy) THEN no_destroy = self._no_destroy

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATATOM::GetProperty, _EXTRA=extraKeywords
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CATOBJECTVIEW::RESIZE
;
; PURPOSE:
;
;       This method resizes the object. Subclass members should override this method.
;
; SYNTAX:
;
;       self -> Resize, xsize, ysize
;
; ARGUMENTS:
;
;       xsize:   The new X size.
;
;       ysize:   The new Y size.
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
PRO CatObjectView::Resize, xsize, ysize
END



;*****************************************************************************************************
;+
; NAME:
;       CATOBJECTVIEW::SETVIEW
;
; PURPOSE:
;
;       This method is a quick way to set the IDLgrView object into the object.
; SYNTAX:
;
;       aCatObjView -> SetView, theView
;
; ARGUMENTS:
;
;     theView.
;
; KEYWORDS:
;
;     VIEW:       Set the IDLgrView or IDLgrScene object.
;
;-
;*****************************************************************************************************
PRO CatObjectView::SetView, theView

   @cat_pro_error_handler

   IF N_Elements(theView) NE 0 THEN BEGIN

      IF Obj_Valid(self._theView) THEN Obj_Destroy, self._theView

      IF Obj_Isa(theView, 'IDLgrGraphic') THEN BEGIN
         idlGraphic = theView
         theModel = Obj_New('IDLgrModel')
         theView = Obj_New('IDLgrView', Viewplane_Rect=[0, 0, 1, 1], _Extra=extraKeywords)
         idlGraphic -> GetProperty, XRange=xr, YRange=yr, ZRange=zr
         xscale = FSC_Normalize(xr, Position=[0.15, 0.85])
         yscale = FSC_Normalize(yr, Position=[0.15, 0.85])
         zscale = FSC_Normalize(zr, Position=[0.15, 0.85])
         idlGraphic -> SetProperty, XCoord_Conv=xscale, YCoord_Conv=yscale, ZCoord_Conv=zscale
         theModel -> Add, idlGraphic
         theView -> Add, theModel
      ENDIF

      IF Obj_Isa(theView, 'IDLgrModel') THEN BEGIN
         theModel = theView
         theView = Obj_New('IDLgrView', Viewplane_Rect=[0, 0, 1, 1], _Extra=extraKeywords)
         theView -> Add, theModel
      ENDIF

      self._theView = theView

   ENDIF

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATOBJECTVIEW::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the CATOBJECTVIEW object properties.
;
; SYNTAX:
;
;       CATOBJECTVIEW -> SetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     NO_DESTROY:  If this keyword is set, the view object (theView) will not be destroyed when the
;                  CatObjectView object is destroyed.
;
;     VIEW:        Set the IDLgrView or IDLgrScene object.
;
;     _EXTRA:      Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO CatObjectView::SetProperty, NO_DESTROY=no_destroy, VIEW=theView, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(theView) NE 0 THEN BEGIN

      IF Obj_Valid(self._theView) THEN Obj_Destroy, self._theView

      IF Obj_Isa(theView, 'IDLgrGraphic') THEN BEGIN
         idlGraphic = theView
         theModel = Obj_New('IDLgrModel')
         theView = Obj_New('IDLgrView', Viewplane_Rect=[0, 0, 1, 1], _Extra=extraKeywords)
         idlGraphic -> GetProperty, XRange=xr, YRange=yr, ZRange=zr
         xscale = FSC_Normalize(xr, Position=[0.15, 0.85])
         yscale = FSC_Normalize(yr, Position=[0.15, 0.85])
         zscale = FSC_Normalize(zr, Position=[0.15, 0.85])
         idlGraphic -> SetProperty, XCoord_Conv=xscale, YCoord_Conv=yscale, ZCoord_Conv=zscale
         theModel -> Add, idlGraphic
         theView -> Add, theModel
      ENDIF

      IF Obj_Isa(theView, 'IDLgrModel') THEN BEGIN
         theModel = theView
         theView = Obj_New('IDLgrView', Viewplane_Rect=[0, 0, 1, 1], _Extra=extraKeywords)
         theView -> Add, theModel
      ENDIF

      self._theView = theView

   ENDIF

   IF N_Elements(no_destroy) NE 0 THEN self._no_destroy = Keyword_Set(no_destroy)

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATATOM::SetProperty, _EXTRA=extraKeywords
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CATOBJECTVIEW::CLEANUP
;
; PURPOSE:
;
;       This is the CATOBJECTVIEW object class destructor method.
;
; SYNTAX:
;
;       Called automatically when the object is destroyed.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO CatObjectView::CLEANUP

   @cat_pro_error_handler

   IF self._no_destroy EQ 0 THEN BEGIN
      Obj_Destroy, self._theView
   ENDIF

   self -> CATATOM::Cleanup

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATOBJECTVIEW::INIT
;
; PURPOSE:
;
;       This method is called upon object creation.
;
; SYNTAX:
;
;       Called automatically when the object is created thus:
;
;           viewObject = OBJ_NEW ('CATOBJECTVIEW')
;
; ARGUMENTS:
;
;       theView:    An object of type IDLgrView, IDLgrScene, IDLgrModel, or IDLgrGraphic.
;                   If this is not a view or scene, the appropriate heirarchy will be automatically
;                   constructed. The viewplane rectangle for all objects that don't contain a view
;                   is [0,0,1,1] with the default EYE distance. The view object will be destroyed
;                   when the CatObjectView object is destroyed unless the NO_DESTROY keyword is set.
;
; KEYWORDS:
;
;      NO_DESTROY: If this keyword is set, the view object (theView) will not be destroyed when the
;                  CatObjectView object is destroyed.
;
;      _EXTRA:     Any keyword appropriate for the INIT method of the superclass object. If theView
;                  is not an IDLgrView object, this keyword can also be used to pass DLgrView keywords
;                  to the created IDLgrView object.
;
;-
;*****************************************************************************************************
FUNCTION CatObjectView::INIT, theView, NO_DESTROY=no_destroy, _Extra=extraKeywords

   ; Set up error handler.
   @cat_func_error_handler

   ; Call superclass init method
   ok = self -> CATATOM::Init (parent, _EXTRA=extraKeywords)

   IF N_Elements(theView) NE 0 THEN BEGIN
      IF Obj_Isa(theView, 'IDLgrGraphic') THEN BEGIN
         idlGraphic = theView
         theModel = Obj_New('IDLgrModel')
         theView = Obj_New('IDLgrView', Viewplane_Rect=[0, 0, 1, 1], _Extra=extraKeywords)
         idlGraphic -> GetProperty, XRange=xr, YRange=yr, ZRange=zr
         xscale = FSC_Normalize(xr, Position=[0.15, 0.85])
         yscale = FSC_Normalize(yr, Position=[0.15, 0.85])
         zscale = FSC_Normalize(zr, Position=[0.15, 0.85])
         idlGraphic -> SetProperty, XCoord_Conv=xscale, YCoord_Conv=yscale, ZCoord_Conv=zscale
         theModel -> Add, idlGraphic
         theView -> Add, theModel
      ENDIF

      IF Obj_Isa(theView, 'IDLgrModel') THEN BEGIN
         theModel = theView
         theView = Obj_New('IDLgrView', Viewplane_Rect=[0, 0, 1, 1], _Extra=extraKeywords)
         theView -> Add, theModel
      ENDIF

      self._theView = theView
   ENDIF

   self._no_destroy = Keyword_Set(no_destroy)

   ; Send report and exit.
   IF (ok) THEN self -> Report, /Completed $
   ELSE self -> Report, /Failed
   RETURN, ok

END



;*****************************************************************************************************
;
; NAME:
;       CATOBJECTVIEW CLASS DEFINITION
;
; PURPOSE:
;
;       This is the class definition code for the CATOBJECTVIEW object.
;
;*****************************************************************************************************
PRO CatObjectView__DEFINE, class

   class = { CATOBJECTVIEW, $       ; The object class definition.
             INHERITS CATATOM, $    ; Part of the Catalyst Object Library.
             _no_destroy: 0L, $     ; A flag that governs the destroying of theView.
             _theView: Obj_New() $  ; The IDLgrView or IDLgrScene object to be displayed.
           }

END