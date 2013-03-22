;*****************************************************************************************************
;+
; NAME:
;       CatList
;
; PURPOSE:
;
;        This is object is part of the Catalyst Library. It presents a mechanism for
;        dynamically creating and retrieving lists of information of any data type.
;        It is like a hash in the perl programming language. CATLISTVALUE objects
;        are stored in the CATLIST.
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
;
; SUPERCLASSES:
;
;       CAT_CONTAINER
;       IDL_CONTAINER
;
; SYNTAX:
;
;       CatListObject = Obj_New('CatList')
;
; CLASS_STRUCTURE:
;
;    class = { CatList               $ ; The CatContainer object class name.
;              INHERITS CatContainer $ ; Sub-class of CatContainer
;            }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 12th March 2003
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
;       CatList::ADD
;
; PURPOSE:
;
;        This method overrides the default ADD method to provide two possiblities:
;        either child objects of class CatListValue can be added, or a name and
;        value can be specified which causes the CatListValue to be created.
;
; SYNTAX:
;
;        defaultsObj -> Add, name, Value=value, Success=success
;
; ARGUMENTS:
;
;        NAME:    If this is a text string, a CatListValue object of this name is
;                 created to add. If this is a CatListValue object it is added directly
;                 to this container.
;
; KEYWORDS:
;
;        VALUE:   The value to enter into the list if NAME is a string. Can be of any IDL
;                 type. If NAME is specified without VALUE, VALUE will be a NULL string.
;
;        SUCCESS: A flag indicating the success of the routine. If set, the
;                 routine will fail quietly.
;
;-
;*****************************************************************************************************
PRO CatList::Add, name, Value=value, Success=success, _EXTRA=extraKeywords

   self -> AddValue, name, Value=value, Success=success, _EXTRA=extraKeywords

END

;*****************************************************************************************************
;+
; NAME:
;       CatList::ADDVALUE
;
; PURPOSE:
;
;        This method adds child objects of class CatListValue or a name and
;        value can be specified which causes the CatListValue to be created
;        and added to the list.
;
; SYNTAX:
;
;        defaultsObj -> AddValue, name, Value=value, Success=success
;
; ARGUMENTS:
;
;        NAME:    If this is a text string, a CatListValue object of this name is
;                 created to add. If this is a CatListValue object it is added directly
;                 to this container.
;
; KEYWORDS:
;
;        VALUE:   The value to enter into the list if NAME is a string. Can be of any IDL
;                 type. If NAME is specified without VALUE, VALUE will be a NULL string.
;
;        SUCCESS: A flag indicating the success of the routine. If set, the
;                 routine will fail quietly.
;
;-
;*****************************************************************************************************
PRO CatList::AddValue, name, Value=value, Success=success

   ; Set up error handling
   IF (ARG_PRESENT (success)) THEN $
   BEGIN
      CATCH, error
      IF (error NE 0) THEN RETURN
   ENDIF $
   ELSE ON_ERROR, 2
   success = 0B

   IF N_Elements(value) EQ 0 THEN value = ""

   ; Branch on the possibilities for the first param
   IF (SIZE (name, /TName) EQ 'STRING') THEN valueObj = OBJ_NEW ('CatListValue', self, name, Value=value) $
   ELSE BEGIN

      ; Check we now have a valid object
      IF (NOT OBJ_ISA (name, 'CatListValue')) THEN $
         MESSAGE, 'CatList requires a valid CatListValue object to add.'

      self -> CatContainer::Add, name, _Extra=ExtraKeywords
    ENDELSE

    success = 1B

END


;*****************************************************************************************************
;+
; NAME:
;        CatList::GetValue
;
; PURPOSE:
;
;        This function method gets a named value from the container. Note that
;        only one value can be returned at a time. To implement multiple Gets,
;        Use the normal GET method and extract the values sequentially.
;
; SYNTAX:
;
;       Value = ListObject -> GetValue (name)
;
; ARGUMENTS:
;
;       NAME:     The name of the value to retrieve from the container. (Required)
;
; KEYWORDS:
;
;        SUCCESS: A flag indicating the success of the routine. If set, the
;                 routine will fail quietly.
;
;-
;*****************************************************************************************************
FUNCTION CatList::GetValue, name, Success=success, _Ref_Extra=extraKeywords, ALL=all

   ; Set up error handling
   IF (ARG_PRESENT (success)) THEN $
   BEGIN
      CATCH, error
      IF (error NE 0) THEN RETURN, ''
   ENDIF $
   ELSE ON_ERROR, 2
   success = 0B

   IF Keyword_Set(all) THEN BEGIN
      allObjects = self -> Get(/All)
      RETURN, allObjects
   ENDIF

   ; Check that a name is specified
   IF (SIZE (name, /TName) NE 'STRING') THEN MESSAGE, 'The name for the value must be a valid string'

   ; Get the  value object(s)
   valueObj = self -> CatContainer::Get (name, _Extra=extraKeywords) ; use the superclass method to bypass GET method override

   ; If no matching objects are found, complain
   IF (OBJ_VALID (valueObj) EQ 0) THEN BEGIN
      IF Arg_Present(success) EQ 0 THEN MESSAGE, 'Value <' + name + '> not found.' ELSE $
      BEGIN
         success = 0B
         RETURN, ""
      ENDELSE
   ENDIF

   ; Get the value from the object and return it
   success = 1B
   RETURN, valueObj -> GetValue ()

END


;*****************************************************************************************************
;+
; NAME:
;       CatList::CLEANUP
;
; PURPOSE:
;
;       This is the CatList object class destructor method.
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
PRO CatList::CLEANUP

   ON_ERROR, 2
   self -> CatContainer::CLEANUP

END


;*****************************************************************************************************
;+
; NAME:
;       CatList::INIT
;
; PURPOSE:
;
;       This is the CatList object class creator method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
FUNCTION CatList::INIT

   ok = self -> CatContainer::INIT ()
   RETURN, ok

END


;*****************************************************************************************************
;
; NAME:
;       CatList CLASS DEFINITION
;
; PURPOSE:
;
;       This is the CatList object's structure definition code.
;
;*****************************************************************************************************
PRO CatList__DEFINE, class

   class = {CatList, INHERITS CatContainer}

END