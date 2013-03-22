;*****************************************************************************************************
;+
; NAME:
;       CATTOOL
;
; PURPOSE:
;
;       This object implements a tool object class. It contains a storage pointer for
;       the data before the tool is applied. This is so the result of the tool can be
;       undone later. And it contains a target field so it knows what object was the
;       target of the tool. The tool applies an operation to the target object.
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
;       toolObject = OBJ_NEW ('CATTOOL')
;
; CLASS_STRUCTURE:
;
;   class = { CATTOOL,               $ ; The object class name.
;             INHERITS CATATOM,      $ ; Inherits CATATOM
;             _theCache: Ptr_New(),  $ ; The pre-tool data location.
;             _noCache: 0L           $ ; A flag that indicates the input data is not to be cached or stored.
;             _theTarget: Obj_New(), $ ; The tool target. The tool is APPLIED to the data in this object.
;           }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 3 April 2003.
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
;       CATTOOL::CACHE
;
; PURPOSE:
;
;       This method enables the CATTOOL storage pointer to be loaded with the pre-tool data.
;
; SYNTAX:
;
;       toolObject -> Cache, theData
;
; ARGUMENTS:
;
;       theData:    The data to be cached or saved.
;
; KEYWORDS:
;
;       NO_COPY:  Set this keyword to transfer the data to the cache pointer
;                 without making a copy.
;-
;*****************************************************************************************************
PRO CatTool::Cache, theData, NO_COPY=no_copy

   @cat_pro_error_handler

   IF Ptr_Valid(self._theCache) THEN Ptr_Free, self._theCache
   self._theCache = Ptr_New(theData, No_Copy=Keyword_Set(no_copy))

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CATTOOL::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to get properties of the CATTOOL object
;
; SYNTAX:
;
;       toolObject -> GETPROPERTY, CACHED_DATA=data
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       CACHED_DATA:   The data cached in the storage pointer.
;
;       NOCACHE:       The current setting of the noCache flag.
;
;       TARGET:        A reference to the target object.
;
;       _REF_EXTRA:    Any keyword appropriate for the  superclass GETPROPERTY method.
;
;-
;*****************************************************************************************************
PRO CatTool::GetProperty, $
   CACHED_DATA=cached_data,$
   NOCACHE=noCache, $
   TARGET=target, $
    _Ref_Extra=extraKeywords

   @cat_pro_error_handler

   ; If a variable is provided, return the image in the storage location.

   IF Arg_Present(cached_data) THEN $
   BEGIN
      IF Ptr_Valid(self._theCache) THEN cached_data = *self._theCache
   ENDIF
   noCache = self._theCache
   IF Arg_Present(target) THEN target = self._theTarget

   self -> CATATOM::GetProperty, _Extra=extraKeywords
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATTOOL::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the CATTOOL object class
;       properties.
;
; SYNTAX:
;
;       toolObject -> SETPROPERTY, CACHED_DATA=cached_data
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       CACHED_DATA:   The data stored in the cache.
;
;       NOCACHE:       Set this keyword to indicate that the input data should not be cached
;                      or stored in the storage location.
;
;       NO_COPY:       Set this keyword to transfer the data to the cache without making a copy.
;
;       TARGET:        Set this keyword to an object reference for the target object. The tool is
;                      applied to the data in this target.
;
;      _EXTRA:         Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO CatTool::SetProperty, $
   CACHED_DATA=cached_data, $
   NOCACHE=noCache, $
   NO_COPY=no_copy, $
   TARGET=targetObject, $
   _Extra=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(cached_data) NE 0 THEN self -> Cache, cached_data, No_Copy=Keyword_Set(no_copy)
   IF N_Elements(noCache) NE 0 THEN $
   BEGIN
      self._noCache = Keyword_Set(noCache)
      IF self._theCache THEN Ptr_Free, self._theCache
   ENDIF
   IF N_Elements(targetObject) NE 0 THEN $
   BEGIN
     IF Obj_IsA_Valid(targetObject, 'CATDATAATOM') THEN self._theTarget = targetObject ELSE $
        Message, 'The target object is invalid or not a sub-class of CATDATAATOM.'
   ENDIF
   self -> CATATOM::SetProperty, _Extra=extraKeywords
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATTOOL::CLEANUP
;
; PURPOSE:
;
;       This is the CATTOOL object class destructor method.
;
; SYNTAX:
;
;       Called automatically when the object is destroyed thus:
;
;           OBJ_DESTROY, toolObject
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
PRO CatTool::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self._theCache

   self -> CATATOM::CLEANUP
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATTOOL::INIT
;
; PURPOSE:
;
;       This method is called upon object creation.
;
; SYNTAX:
;
;       Called automatically when the object is created thus:
;
;           toolObject = OBJ_NEW ('CATTOOL')
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;     NOCACHE:   Set this keyword to indicate that the input data should not be cached
;                or stored in the storage location.
;
;     TARGET:    Set this keyword to an object reference for the target object. The tool is
;                applied to the data in this target.
;
;     _EXTRA:    Any keyword appropriate for the INIT method of the superclass object.
;-
;*****************************************************************************************************
FUNCTION CatTool::INIT, $
   NOCACHE=noCache, $
   TARGET=targetObject, $
   _Extra=extrakeywords

   @cat_func_error_handler

   self._noCache = Keyword_Set(noCache)
   IF N_Elements(targetObject) NE 0 THEN $
   BEGIN
     IF Obj_IsA_Valid(targetObject, 'CATDATAATOM') THEN self._theTarget = targetObject ELSE $
        Message, 'The target object is invalid or not a sub-class of CATDATAATOM.'
   ENDIF

   ok = self -> CATATOM::INIT(_Extra=extrakeywords)
   IF NOT ok THEN Message, 'Failed to initialize CATATOM superclass object.'

   self -> Report, /Completed

   RETURN, ok

END


;*****************************************************************************************************
;
; NAME:
;       CATTOOL CLASS DEFINITION
;
; PURPOSE:
;
;       This is the class definition code for the CATTOOL Image Processing object.
;
;*****************************************************************************************************
PRO CatTool__DEFINE, class

   class = { CATTOOL,               $ ; The object class name.
             INHERITS CATATOM,      $ ; Inherits CATATOM
             _theCache: Ptr_New(),  $ ; The pre-tool data location.
             _noCache: 0L,          $ ; A flag that indicates the input data is not to be cached or stored.
             _theTarget: Obj_New()  $ ; The tool target. The tool is APPLIED to the data in this object.
           }

END