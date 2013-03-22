;*****************************************************************************************************
;+
; NAME:
;       CATDATAATOM
;
; PURPOSE:
;
;       This is the most basic data object in the CATALYST Object Library. It is a subclassed
;       CATATOM object and implements a generic data class in the IDL object hierarchy.
;       All data objects in the heirarchy are subclassed from this object. The fundamental
;       principal of the Catalyst Library is that data is associated with colors (for display)
;       and a coordinate system (for interaction). Thus, there are placeholders in CATDATAATOM
;       for a COLORTOOL and a CATCOORD object (or their equivalents). (There is also a placeholder
;       for tools that work on the data, but this has been less frequently employed.)
;
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
;       dataObject = Obj_New('CATDATA')
;
; CLASS_DEFINITION:
;
;   class   = { CATDATAATOM,           $   ; The CATDATAATOM class name.
;              _dataPtr   : PTR_NEW(), $   ; A pointer to the data
;              _coords    : OBJ_NEW(), $   ; A CATCOORD object of some type.
;              _colors    : OBJ_NEW(), $   ; A COLORTOOL object for setting up color tables.
;              _toolList  : OBJ_NEW(), $   ; The TOOLLIST object.
;              INHERITS CATATOM        $   ; CATDATA is an CATATOM object.
;             }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 27th March 2003.
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
;       CATDATAATOM::AddTool
;
; PURPOSE:
;
;       This method adds an object (presumably a tool object) to the ToolList for this object.
;
; SYNTAX:
;
;       self -> AddTool, object
;
; ARGUMENTS:
;
;       OBJECT: The object to be added.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO CatDataAtom::AddTool, object

   ; Set up an error handler
   @cat_pro_error_handler

   ; Check whether this is a valid  object
   IF Obj_Valid (object) THEN self._toolList -> Add, object

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATDATAATOM::APPLYCOLORS
;
; PURPOSE:
;
;       This method sets up the colors for the data object if they exist. For this
;       reason, sub-classes *MUST* call this method at the *START* of their draw methods.
;
; SYNTAX:
;
;       self -> ApplyColors
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
PRO CatDataAtom::ApplyColors

   ; Set up an error handler
   @cat_pro_error_handler

   ; Set up the colors
   IF (Obj_IsA_Valid (self._colors, 'CATATOM')) THEN self._colors -> Draw

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATDATAATOM::APPLYCOORDS
;
; PURPOSE:
;
;       This method sets up the coordinates for the data object if they exist. For this
;       reason, sub-classes *MUST* call this method at the *START* of their draw methods.
;
; SYNTAX:
;
;       self -> ApplyCoords
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
PRO CatDataAtom::ApplyCoords

   ; Set up an error handler
   @cat_pro_error_handler

   ; Set up the colors
   IF (Obj_IsA_Valid (self._coords, 'CATATOM')) THEN self._coords -> Draw

   ; Report completion
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CATDATAATOM::GetData
;
; PURPOSE:
;
;       This function method enables data to be retreived from this object. It is written
;       for internal use and should not be called without due consideration. All data
;       handling should be done inside the object.
;
; SYNTAX:
;
;       data = imageObject -> GetData (Success=s)
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       SUCCESS: This flag shows whether the get has been successful or not.
;
;-
;*****************************************************************************************************
FUNCTION CatDataAtom::GetData, Success=success

   ; Default error handler
   @cat_func_error_handler

   ; Attempt to get data from the data pointer
   success = PTR_VALID (self._dataPtr)
   IF NOT success THEN result = 0 $
   ELSE result = *self._dataPtr

   ; Report completion and return result
   self -> Report, /Completed
   RETURN, result

END


;*****************************************************************************************************
;+
; NAME:
;       CATDATAATOM::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to get the object's properties.
;
; SYNTAX:
;
;       dataObject -> GetProperty, Data=data
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       COLOR_OBJECT: Use this keyword to get the contained COLORTOOL object.
;
;       COORD_OBJECT: Use this keyword to get the contained CATCOORD object.
;
;       TOOLLIST:     The object's toolist object.
;
;       UNDO_LIMIT:   The current undo limit for the data object.
;
;       _REF_EXTRA:   Any keyword appropriate for the GETPROPERTY method of the superclass object.
;-
;*****************************************************************************************************
PRO CatDataAtom::GetProperty, COLOR_OBJECT=color_object, $
                           COORD_OBJECT=coord_object, $
                           TOOLLIST=toollist, $
                           UNDO_LIMIT=undo_limit, $
                           _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATATOM::GetProperty, _REF_EXTRA=extraKeywords

   IF Arg_Present(color_object) THEN color_object = self._colors
   IF Arg_Present(coord_object) THEN coord_object = self._coords
   IF Arg_Present(undo_limit) THEN self._toolList -> GetProperty, Limit=undo_limit
   IF Arg_Present(toollist) THEN toollist = self._toolList

   IF N_Elements(extraKeywords) NE 0 THEN self -> CatAtom::GetProperty, _Extra=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATDATAATOM::REDO
;
; PURPOSE:
;
;       This method allows a previous UNDO to be reversed.
;
; SYNTAX:
;
;           dataObject -> REDO
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       Draw: Set this keyword to allow the object to DRAW itself at the completion of the REDO.
;
;-
;*****************************************************************************************************
PRO CatDataAtom::Redo, Draw=draw, _Extra=extrakeywords

   ; Default error handler
   @cat_pro_error_handler

   ; Call REDO on the tool list
   self._toolList -> Redo, _Extra=extrakeywords

   ; Do DRAW if requested
   IF KEYWORD_SET (draw) THEN self -> Draw

   ; Report completion
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATDATAATOM::SetData
;
; PURPOSE:
;
;       This method enables new data to be added to this object.
;
; SYNTAX:
;
;       imageObject -> SetData, Dist(40,40), /Draw
;
; ARGUMENTS:
;
;       DATA: The data to be added. Note that any existing data will be removed, so this
;             routine should be used with caution!
;
; KEYWORDS:
;
;       DRAW:    Set this keyword makes the object DRAW itself once the data is loaded.
;
;       NO_COPY: Set this keyword to "move" supplied data into the object rather than
;                make a copy. Note that this will leave the input "data" variable
;                undefined.
;
;-
;*****************************************************************************************************
PRO CatDataAtom::SetData, data, Draw=draw, No_Copy=no_copy

   ; Default error handler
   @cat_pro_error_handler

   ; Check that the incoming data is valid
   IF (N_ELEMENTS (data) EQ 0) THEN MESSAGE, 'No data supplied.'

   ; Set the data pointer
   IF (PTR_VALID (self._dataPtr)) THEN PTR_FREE, self._dataPtr
   self._dataPtr = PTR_NEW (data, No_Copy=no_copy)

   ; Do DRAW if requested
   IF KEYWORD_SET (draw) THEN self -> Draw

   ; Report completion
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATDATAATOM::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to get the object's properties.
;
; SYNTAX:
;
;       dataObject -> SetProperty, Data=data
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       COLOR_OBJECT: Use this keyword to load a color object for setting up colors
;                     for data display.
;
;       COORD_OBJECT: Use this keyword to load a coordinate object for setting up
;                     the data coordinate system for data display.
;
;       DRAW:         Set this keyword makes the object DRAW itself once the data is loaded.
;
;       UNDO_LIMIT:   The number of tools kept in the undo buffer.
;
;       _EXTRA:       Any keyword appropriate for the SETPROPERTY method of the superclass object.
;-
;*****************************************************************************************************
PRO CatDataAtom::SetProperty, $
   COLOR_OBJECT=color_object, $
   COORD_OBJECT=coord_object, $
   DRAW=draw,                 $
   UNDO_LIMIT=undo_limit,     $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Color object?
   IF N_Elements(color_object) NE 0 THEN BEGIN
      IF Obj_Valid(self._colors) THEN $
      BEGIN
         self._colors -> RemoveParent, self
         self._colors = color_object
         self._colors -> AddParent, self
      ENDIF ELSE $
      BEGIN
         self._colors = color_object
         self._colors -> AddParent, self
      ENDELSE
   ENDIF

   ; Coodinates object?
   IF N_Elements(coord_object) NE 0 THEN BEGIN
      IF Obj_Valid(self._coords) THEN $
      BEGIN
         self._coords -> RemoveParent, self
         self._coords = coord_object
         self._coords -> AddParent, self
      ENDIF ELSE BEGIN
         self._coords = coord_object
         self._coords -> AddParent, self
      ENDELSE
   ENDIF

   IF N_Elements(undo_limit) NE 0 THEN self._toolList -> SetProperty, Limit=undo_limit

   ; Call the superclass method
   self -> CATATOM::SetProperty, _STRICT_EXTRA=extraKeywords

   ; If DRAW requested, do it here
   IF (KEYWORD_SET (draw)) THEN self -> Draw

   ; Report completion
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATDATAATOM::UNDO
;
; PURPOSE:
;
;       This method performs a one-step "undo" of the object on the UNDO LIST.
;
; SYNTAX:
;
;           dataObject -> UNDO
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DRAW:   Set this keyword to immediately call the draw method of the object
;               when the UNDO is finished.
;
;       _Extra: Extra keywords appropriate for toollist UNDO method.
;-
;*****************************************************************************************************
PRO CatDataAtom::Undo, Draw=draw, _Extra=extraKeywords

   ; Initialise the error handler.
   @cat_pro_error_handler

   ; Call UNDO on the tool list
   self._toolList -> Undo, _Extra=extraKeywords

   ; Do we need to re-draw?
   IF Keyword_Set(draw) THEN self -> Draw

   ; Report completion
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATDATAATOM::CLEANUP
;
; PURPOSE:
;
;       This is the CATDATAATOM object class destructor method.
;
; SYNTAX:
;
;       Called automatically when the object is destroyed thus:
;
;           OBJ_DESTROY, dataObject
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       _EXTRA:  Any keyword appropriate for the CATATOM::CLEANUP method.
;-
;*****************************************************************************************************
PRO CatDataAtom::CLEANUP, _EXTRA=extraKeywords

   @cat_pro_error_handler

   PTR_FREE, self._dataPtr
   IF (OBJ_VALID (self._coords  )) THEN self._coords -> RemoveParent, self
   IF (OBJ_VALID (self._colors  )) THEN self._colors -> RemoveParent, self
   IF (OBJ_VALID (self._toolList)) THEN self._toolList -> RemoveParent, self
   self -> CATATOM::CLEANUP, _Extra=extraKeywords
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATDATAATOM::INIT
;
; PURPOSE:
;
;       This method is used upon object creation.
;
; SYNTAX:
;
;       Called automatically when the object is created thus:
;
;           dataObject = OBJ_NEW (CatDataAtom, Hanning (50,50))
;
; ARGUMENTS:
;
;       DATA: Some data to use in the object (optional).
;
; KEYWORDS:
;
;       COLOR_OBJECT: Use this keyword to load a color object for setting up colors
;                     for data display.
;
;       COORD_OBJECT: Use this keyword to load a coordinate object for setting up
;                     the data coordinate system for data display.
;
;       NO_COPY:      Set this keyword to "move" supplied data into the object rather than
;                     make a copy. Note that this will leave the input "data" variable
;                     undefined.
;
;       PARENT:       The object reference to the parent of this object.
;
;       UNDO_LIMIT:   The number of tools kept in the undo buffer. This is set to 5 by default.
;
;       _EXTRA:       Any keyword appropriate for the INIT method of the superclass object.
;-
;*****************************************************************************************************
FUNCTION CatDataAtom::INIT, data, $
                         COLOR_OBJECT=color_object, $
                         COORD_OBJECT=coord_object, $
                         NO_COPY=no_copy, $
                         PARENT=parent, $
                         UNDO_LIMIT=undo_limit, $
                         _EXTRA=extraKeywords

   ; Call error handler and print start message
   @cat_func_error_handler

  ; Call the superclass INIT method and check
   ok = self -> CATATOM::INIT (parent, _EXTRA=extraKeywords)
   IF (NOT ok) THEN MESSAGE, 'Atom class failed to initialise'

   ; Store any data supplied
   IF (N_ELEMENTS (data) NE 0) THEN self._dataPtr = PTR_NEW (data, No_Copy=no_copy)

   ; Load the color object if available.
   IF N_Elements(color_object) NE 0 THEN $
   BEGIN
      IF Obj_Valid(color_object) THEN BEGIN
         self._colors = color_object
         color_object -> AddParent, self
      ENDIF
   END

   ; Load the coordinates object if available.
   IF N_Elements(coord_object) NE 0 THEN $
   BEGIN
      IF Obj_Valid(coord_object) THEN BEGIN
         self._coords = coord_object
         coord_object -> AddParent, self
      ENDIF
   END

   ; Create a TOOL list
   IF (N_ELEMENTS (undo_limit) EQ 0) THEN undo_limit=5
   self._toolList = Obj_New('CatToolList', Limit=undo_limit)

   ; Report and return status
   IF (ok) THEN self -> Report, /Completed $
   ELSE self -> Report, /Failed
   RETURN, ok

END


;*****************************************************************************************************
;
; NAME:
;       CATDATAATOM CLASS DEFINITION
;
; PURPOSE:
;
;       This is the CATDATAATOM objects structure definition code.
;
;*****************************************************************************************************
PRO CatDataAtom__DEFINE, class

   class   = { CATDATAATOM,           $   ; The CATDATAATOM class name.
              _dataPtr   : PTR_NEW(), $   ; A pointer to the data
              _coords    : OBJ_NEW(), $   ; A CATCOORD object of some type.
              _colors    : OBJ_NEW(), $   ; A COLORTOOL object for setting up color tables.
              _toolList  : OBJ_NEW(), $   ; The TOOLLIST object.
              INHERITS CATATOM        $   ; CATDATA is an CATATOM object.
             }
END