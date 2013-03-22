;*****************************************************************************************************
;+
; NAME:
;       CATLISTVALUE
;
; PURPOSE:
;
;        This object is part of the Catalyst Library. It is a simple object used
;        to store a named value of any data type in a CatList object. It is
;        similar to a hash in the perl programming language.
;
;        This object is designed solely to be used by the CATLIST container
;        object and should not be called individually.
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
;       CATCONTAINER
;       IDL_CONTAINER
;
; SYNTAX:
;
;       catListValueObject = Obj_New('CatListValue', parent, name, Value=value)
;
; ARGUMENTS:
;
;        parent: The parent object - must be a CatList object (required)
;
;        name:   The name of the value in the list (optional)
;
; KEYWORDS:
;
;        VALUE:  The variable to be assigned as the value of the object. If
;                no value is supplied, a NULL string is used.
;
; CLASS_STRUCTURE:
;
;    class = { CatListValue,      $
;              _value : PTR_NEW (),  $
;              INHERITS CatContainer $
;            }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 12th March 2003.
;
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
;       CATLISTVALUE::ADD
;
; PURPOSE:
;
;        This method overrides the default ADD method to prevent child objects
;        being added.
;
; SYNTAX:
;
;        None.
;
; ARGUMENTS:
;
;        None.
;
; KEYWORDS:
;
;        SUCCESS: A flag indicating the success of the routine. If set, the
;                 routine will fail quietly.
;
;-
;*****************************************************************************************************
PRO CatListValue::Add, object, Success=success, _EXTRA=extraKeywords

      ; Set up error handling
   IF (ARG_PRESENT (success)) THEN $
   BEGIN
      CATCH, error
      IF (error NE 0) THEN RETURN
   ENDIF $
   ELSE ON_ERROR, 2
   success = 0B

   MESSAGE, 'Objects cannot be ADDed to a CatListValue.'

END


;*****************************************************************************************************
;+
; NAME:
;       CATLISTVALUE::GETVALUE
;
; PURPOSE:
;
;       This function method retrieves the data value associated with the list entry.
;
; SYNTAX:
;
;       value = valueObj -> GetValue ()
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;        SUCCESS: A flag indicating the success of the routine. If set, the
;                 routine will fail quietly.
;
;-
;*****************************************************************************************************
FUNCTION CatListValue::GetValue, Success=success

   ; Set up error handling
   IF (ARG_PRESENT (success)) THEN $
   BEGIN
      CATCH, error
      IF (error NE 0) THEN RETURN, ''
   ENDIF $
   ELSE ON_ERROR, 2
   success = 0B

   IF (PTR_VALID (self._value)) THEN retVal = *self._value $
   ELSE retVal = ''

   RETURN, retVal

END


;*****************************************************************************************************
;+
; NAME:
;       CATLISTVALUE::SETVALUE
;
; PURPOSE:
;
;       This procedure method sets the data value associated with the list entry.
;
; SYNTAX:
;
;       valueObj -> SetValue, listValue
;
; ARGUMENTS:
;
;       ListValue: The variable to be used as the value.
;
; KEYWORDS:
;
;        SUCCESS: A flag indicating the success of the routine. If set, the
;                 routine will fail quietly.
;
;-
;*****************************************************************************************************
PRO CatListValue::SetValue, value, Success=success

   ; Set up error handling
   IF (ARG_PRESENT (success)) THEN $
   BEGIN
      CATCH, error
      IF (error NE 0) THEN RETURN
   ENDIF $
   ELSE ON_ERROR, 2
   success = 0B

   IF (N_ELEMENTS (value) GT 0) THEN $
   BEGIN
      IF (PTR_VALID (self._value)) THEN *self._value = value $
      ELSE self._value = PTR_NEW (value)
   ENDIF

END

;*****************************************************************************************************
;+
; NAME:
;       CATLISTVALUE::INIT
;
; PURPOSE:
;
;        This function method initialises the object.
;
; SYNTAX:
;
;        valueObj = OBJ_NEW ('CatListValue')
;
; ARGUMENTS:
;
;        Parent: The parent object - must be a CatLists object (required)
;
;        Name:   The name of the value in the list (optional)
;
; KEYWORDS:
;
;        Value:  The variable to be assigned to the first value (input). If
;                no value is supplied, a NULL string is used.
;
;-
;*****************************************************************************************************
FUNCTION CatListValue::INIT, parent, name, Value=value

   ON_ERROR, 2

   ; Process the keywords
   IF (N_ELEMENTS (parent) EQ 0) THEN MESSAGE, 'A parent must be supplied'
   IF (N_ELEMENTS (name)   EQ 0) THEN MESSAGE, 'A name must be supplied for the list value.'

   ; Check that the parent is a valid list object
   IF (NOT Obj_IsA_Valid (parent, 'CatList')) THEN MESSAGE, 'Parent must be a valid CatList object'

   ; Call the superclass method
   ok = self -> CatContainer::INIT (parent, Name=name)
   IF (NOT ok) THEN MESSAGE, 'Parent container failed to initialise.'
   IF (N_ELEMENTS (value) EQ 0) THEN value = ''

   ; Populate the object properties
   self._name  = name
   self._value = PTR_NEW (value)

   RETURN, 1

END


;*****************************************************************************************************
;+
; NAME:
;       CATLISTVALUE::CLEANUP
;
; PURPOSE:
;
;        This function method cleans up the object upon destruction.
;
; SYNTAX:
;
;        OBJ_DESTROY, defaultValueObj
;
; ARGUMENTS:
;
;        None.
;
; KEYWORDS:
;
;        None.
;
;-
;*****************************************************************************************************
PRO CatListValue::CLEANUP

   ON_ERROR, 2

   IF (PTR_VALID (self._value)) THEN PTR_FREE, self._value
   self -> CatContainer::CLEANUP

END

;*****************************************************************************************************
;+
; NAME:
;       CATLISTVALUE STRUCTURE DEFINITION
;
; PURPOSE:
;
;        This procedure defines the CatListValue object structure.
;
; SYNTAX:
;
;        None. See syntax for INIT method
;
; ARGUMENTS:
;
;        None.
;
; KEYWORDS:
;
;        None.
;
;-
;*****************************************************************************************************
PRO CatListValue__DEFINE, class

    class = { CatListValue,         $
              _value : PTR_NEW (),  $
              INHERITS CatContainer $
            }

END
