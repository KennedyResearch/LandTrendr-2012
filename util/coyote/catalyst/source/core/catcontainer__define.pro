;*****************************************************************************************************
;+
; NAME:
;       CatContainer
;
; PURPOSE:
;
;        This is the most basic object in the Catalyst Library. It is actually a simple
;        wrapper around the IDL_CONTAINER object, adding get-by-name, reference counting,
;        and memory management. Reference counting is what assures that objects in the
;        Catalyst system are not destroyed until every other object in the system is done
;        using them. For the most part, users can completely ignore this feature of
;        the program and their programs will clean themselves up miraculously. To avoid
;        memory leaks, all you have to do is make sure you destroy all pointers and objects
;        you use to hold information in your program in your CLEANUP method. (And be *absolutely*
;        sure you call the superclass CLEANUP method.) But any objectyou add to another object is 
;        destroyed automatically. This feature alone is worth the astronomical cost of the Catalyst Library!
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
;       IDL_CONTAINER
;
; SYNTAX:
;
;       catContainerObject = Obj_New('CatContainer')
;
; CLASS_STRUCTURE:
;
;   struct = { CatContainer,                   $ ; The CatContainer object class name.
;              _autoDestroy      : 0B,         $ ; A flag denoting whether the object self destructs
;                                                  when reference count reaches 0.
;              _name             : "",         $ ; The "name" of the object.
;              _parents          : OBJ_NEW (), $ ; A list (container) of parent objects.
;              _memoryManagement : 0B,         $ ; A flag to show if objects contained within this
;                                                  object know about this object.
;              _indexed          : 0B,         $ ; A flag to specify whether the list is specifically indexed.
;              INHERITS IDL_CONTAINER          $
;            }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 12th March 2003
;       Added OCHILD and OSYBLING keywords to the GetProperty method. 24 June 2005. DWF.
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
;       CatContainer::ADD
;
; PURPOSE:
;
;       This method overrides the IDL_CONTAINER Add method to allow
;       the user to add an child object to this container.
;
;       If the child is a CatContainer (or subclass of), it is
;       notified of the additional parent.
;
; SYNTAX:
;
;       container -> Add, object
;
; ARGUMENTS:
;
;       object: The object to add to the container. (Required)
;
; KEYWORDS:
;
;       POSITION: The position index where the object should be added to
;                 the container.
;
;-
;*****************************************************************************************************
PRO CatContainer::Add, object, $             ; The object to add to the container.
                       Position=position     ; The position to place the object in the container.

   ON_ERROR, 2
   IF (N_ELEMENTS (position)     EQ 0) THEN position     = self -> Count ()

   ; If this is an indexed container and the position is set, remove the object at that position
   IF (self._indexed AND (position LT self -> Count())) THEN $
   BEGIN
      self -> Remove, Position=position ; replace the existing with a null object
      self -> IDL_CONTAINER::Remove, Position=position      ; remove the null object
      self -> IDL_CONTAINER::Add, object, Position=position ; insert the new object
   ENDIF $

   ; Otherwise, simply add the object into the container.
   ELSE self -> IDL_CONTAINER::Add, object, POSITION=position

   ; If memory management is on and the object we are adding is a valid CatContainer object,
   ; then add this object to that object's parent list
   IF (Obj_IsA_Valid (object, 'CatContainer') and self._memoryManagement) THEN $
      object -> AddParent, self

END


;*****************************************************************************************************
;+
; NAME:
;       CatContainer::ADDPARENT
;
; PURPOSE:
;
;       This method is used to notify this object that it has been added to a new
;       parent object. This is used internally in the object and should not be
;       called from the outside.
;
; SYNTAX:
;
;       container -> AddParent, object
;
; ARGUMENTS:
;
;       object: The parent object to which this one has been added. (Required)
;
; KEYWORDS:
;
;       NONE.
;
;-
;*****************************************************************************************************
PRO CatContainer::AddParent, parent

   ON_ERROR, 2

   ; Add the object into the container.
   self._parents -> Add, parent

END


;*****************************************************************************************************
;+
; NAME:
;       CatContainer::FindByName
;
; PURPOSE:
;
;       This method is returns the positions (indices) of named objects in the container.
;
; SYNTAX:
;
;       locns = container -> FindByName( searchName )
;
; ARGUMENTS:
;
;       searchName:     The name of the object you are looking for in this container (Required)
;
; KEYWORDS:
;
;       CASE_SENSITIVE: Set this keyword to 1 to indicate a case-sensitive search. By default, the
;                       search is case-insensitive.
;
;       COUNT:          Set this keyword to a named variable that upon exit will contain the number
;                       of objects returned that meet the searchName description. (Output)
;
;       REGEXP:         Set this keyword to 1 to indicate the searchName is a regular expression.
;
;       _EXTRA:         Any keywords supported by STREGEX can also be used. Requires REGEXP to be set.
;
;-
;*****************************************************************************************************
FUNCTION CatContainer::FindByName, searchName, $
   Case_Sensitive=case_sensitive, $
   Count=count, $
   RegExp=regexp, $
   _Extra=extra

   ON_ERROR, 2
   count = 0

   ; Search name must be a scalar.
   IF N_Elements(searchName) NE 1 THEN Message,'Search expression must be a scalar string.'

   ; Get the names of all the child objects.
   children = self -> IDL_CONTAINER::Get (/All, IsA='CatContainer', Count=noChildren)
   IF noChildren EQ 0 THEN RETURN, -1L

   names = StrArr (noChildren)
   FOR childNo = 0L, noChildren - 1 DO $
   BEGIN
      children [childNo] -> GetProperty, Name=childName
      names [childNo] = childName
   ENDFOR

   ; Does the user want to evaluate a regular expression?
   ; If not, do a simple search for the search name.
   fold_case = KEYWORD_SET (case_sensitive) EQ 0
   IF KEYWORD_SET (regexp) THEN $
      mask = StRegex(names, searchName, FOLD_CASE=fold_case, /BOOLEAN, _Extra=extra) $
   ELSE mask = STRMATCH (names, searchName, FOLD_CASE=fold_case)

   ; Transform boolean array to index array. A side effect is that
   ; the count value will be set.
   matches = Where (mask, count)

   RETURN, matches

END


;*****************************************************************************************************
;+
; NAME:
;       CatContainer::GET
;
; PURPOSE:
;
;       This method overrides the IDL_CONTAINER GET method to allow the user to return
;       an object or objects by "name".
;
; SYNTAX:
;
;       object = self -> Get(searchName)
;
; RETURN_VALUE:
;
;       object:     The found object or an array of objects (if more than one meet the search criteria).
;                   A null object is retuned if there are no matches.
;
; ARGUMENTS:
;
;       searchName: The name of the object to find in the containment hierarchy. If absent,
;                   the superclass IDL_CONTAINER::GET method is called and the first object
;                   in the container (if there is one) is returned. Otherwise, a null object
;                   is returned. You may use a regular expression for the searchName, if the
;                   REGEXP keyword is set. For example, to find all objects with the names
;                   beginning with capital C through G, the searchName would be "[C-G.]".)
;
; KEYWORDS:
;
;       ALL:        Set this keyword to return an array of object references to all of the objects
;                   in the container.
;
;       CASE_SENSITIVE: Set this keyword to do a case-sensitive search for the objects. Searches
;                   are case-insensitive by default.
;
;       COUNT:      The number of objects found. Check this output keyword instead of the return value
;                   to indicate success. (Output)
;
;       ISA:        Set this keyword equal to a class name or vector of class names. This keyword is used
;                   in conjunction with the ALL keyword. The ISA keyword filters the array returned by the
;                   ALL keyword, returning only the objects that inherit from the class or classes specified
;                   by the ISA keyword. This keyword is ignored if the ALL keyword is not provided.
;
;       POSITION:   Set this keyword equal to a scalar or array containing the zero-based indices
;                   of the positions of the objects to return.
;
;       RECURSIVE_SEARCH: If this keyword is set, the seach is conducted on all objects
;                   in the object hierarchy below the entry point. In other words, the search
;                   is done not only on child objects, but grandchild objects, great-grandchild
;                   objects. etc.
;
;       REGEXP:     Set this keyword to indicate the searchName is a regular expression.
;
;       _EXTRA:     Any keywords supported by STREGEX can also be used. Requires REGEXP to be set.
;
;-
;*****************************************************************************************************
FUNCTION CatContainer::Get, searchName, $
                            ALL=all, $
                            CASE_SENSITIVE=case_sensitive, $
                            COUNT=count, $
                            ISA=isa, $
                            POSITION=position, $
                            RECURSIVE_SEARCH=recursive_search, $
                            REGEXP=regexp, $
                            _Extra=extra

   ; Set up the error handler
   ON_ERROR, 2

   ; Initialize return values.
   result = OBJ_NEW ()
   count = 0

   ; If this is an empty container, return a null object
   noChildren = self -> Count ()
   IF (noChildren EQ 0) THEN RETURN, result

   ; Find the positions of the requested objects in this container
   IF (N_ELEMENTS (searchName) GT 0) THEN $
   BEGIN
      matches = self -> FindByName (searchName, RegExp=regexp, Count=count, _Extra=extra)
      IF (count GT 0) THEN $
      BEGIN
         IF (NOT KEYWORD_SET (all)) THEN matches = matches [0]
         newObj = self -> IDL_CONTAINER::Get (Position=matches, Count=count)
      ENDIF
   ENDIF $
   ELSE BEGIN
;      IF N_Elements(count) NE 0 THEN print, 'Count = ', count
;      IF N_Elements(position) NE 0 THEN print, 'Position = ', position
      newObj = self -> IDL_CONTAINER::Get (All=all, IsA=isa, Position=position, Count=count)
   ENDELSE

   IF (count GT 0) THEN result = newObj

   ; If required, search recursively

   IF (KEYWORD_SET (recursive_search) AND (N_ELEMENTS (position) EQ 0)) THEN $
   BEGIN
   ;;;;;;;;;;dwf;;;;;;;;;;;;;;Need next line?
      IF Obj_Isa_Valid(self, 'CATCONTAINER') EQ 0 THEN RETURN, Obj_New()
      childObjects = self -> Get(/All)
      FOR childNo = 0, N_ELEMENTS (childObjects) - 1 DO $
      BEGIN
   ;;;;;;;;;;dwf;;;;;;;;;;;;;;Need next line?
         IF Obj_Isa_Valid(childObjects[childNo], 'CATCONTAINER') EQ 0 THEN RETURN, Obj_New()
         childResult = childObjects[childNo] -> Get (searchName, All=all, Case_Sensitive=case_sensitive, $
                                               COUNT=childCount, IsA=isa, REGEXP=regexp, /Recursive_Search)
         IF (childCount GT 0) THEN $
         BEGIN
            IF (count EQ 0) THEN result = childResult $
            ELSE result = [result, childResult]
            count = count + childcount
         ENDIF
      ENDFOR
   ENDIF

   ; Convert to scalar if result is an array of one object.
   IF N_Elements(result) EQ 1 THEN result = result[0]
   RETURN, result

END


;*****************************************************************************************************
;+
; NAME:
;       CatContainer::GETNAME
;
; PURPOSE:
;
;       This method is used to return the object's name.
;
; SYNTAX:
;
;       name = self -> GetName()
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
FUNCTION CatContainer::GetName
   RETURN, self._name
END



;*****************************************************************************************************
;+
; NAME:
;       CatContainer::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to get the object's properties.
;
; SYNTAX:
;
;       self -> GetProperty, Name=objectName
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       AUTO_DESTROY:      A flag specifying whether this object self destructs when its parent
;                          reference count reduces to zero. (Output)
;
;       OCHILD:            Returns the first child in the object container. If there are no children,
;                          a null object is returned. Used to traverse the object hierarchy.
;
;       FIRST_PARENT:      This keyword will return the first parent in the _parents container.
;                          If there are no parents, it will return a null object.
;
;       INDEXED:           A flag specifying whether the list is indexed.
;
;       MEMORY_MANAGEMENT: A flag specifying whether the children of this object know about
;                          this parent (and therefore whether their existance depends on this object)
;                          (Output)
;
;       NAME:              The "name" of the object. Used to keep track of the object in the code. (Output)
;
;       PARENTS:           An array of parent objects.
;
;       OSYBLING:          Returns the first sybling in the object container. If there are no children,
;                          a null object is returned. Used to traverse the object hierarchy.
;
;-
;*****************************************************************************************************
PRO CatContainer::GetProperty, Auto_Destroy=autoDestroy, $
                               OChild=ochild, $
                               First_Parent=first_parent, $
                               Indexed=indexed, $
                               Memory_Management=memory_management, $
                               Name=name, $
                               Parents=parents, $
                               OSybling=osybling


   ON_ERROR, 2

   autoDestroy       = self._autoDestroy
   indexed           = self._indexed
   memory_management = self._memoryManagement
   name              = self._name

   IF (ARG_PRESENT (ochild)) THEN $
   BEGIN
      children = self -> Get (/All)
      validChildren = WHERE (OBJ_VALID (children), noValidChildren)
      IF (noValidChildren EQ 0) THEN ochild = OBJ_NEW () $
      ELSE BEGIN
         children = children [validChildren]
         ochild = children[0]
      ENDELSE
   ENDIF

   IF (ARG_PRESENT (first_parent)) THEN $
   BEGIN
      parents = self._parents -> Get (/All)
      validParents = WHERE (OBJ_VALID (parents), noValidParents)
      IF (noValidParents EQ 0) THEN first_parent = OBJ_NEW () $
      ELSE BEGIN
         parents = parents [validParents]
         first_parent = parents[0]
      ENDELSE
   ENDIF

   IF (ARG_PRESENT (parents)) THEN $
   BEGIN
      parents = self._parents -> Get (/All)
      validParents = WHERE (OBJ_VALID (parents), noValidParents)
      IF (noValidParents EQ 0) THEN parents = OBJ_NEW () $
      ELSE parents = parents [validParents]
      IF (N_ELEMENTS (parents) EQ 1) THEN parents = parents [0]
   ENDIF

   IF (ARG_PRESENT (osybling)) THEN $
   BEGIN
      self -> GetProperty, First_Parent=parent
      children = parent -> Get (/All, Count=count)
      thisChildLocation = WHERE (children EQ self, noValidChildren)
      IF (thisChildLocation[0] GE (count-1)) THEN osybling = OBJ_NEW () $
      ELSE BEGIN
         osybling = (children[thisChildLocation+1])[0]
      ENDELSE
   ENDIF

END


;*****************************************************************************************************
;+
; NAME:
;       CATCONTAINER::GETPROPERTY (function)
;
; PURPOSE:
;
;       This function method is a wrapper around the GetProperty procedure method. It
;       is primarily intended for convenience in getting a single property. Note that
;       it takes about twice as long to execute as its procedure counterpart.
;
; SYNTAX:
;
;       result = self -> GetProperty (/KEYWORD)
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       KEYWORD: By setting any keyword appropriate for the GetProperty method of this
;                object, that property is returned by the function. If more than one
;                property is set, the function will fail.
;
;-
;*****************************************************************************************************
;FUNCTION CatContainer::GetProperty, _EXTRA=extraKeywords
;
;   ON_ERROR, 2
;
;   IF (N_TAGS (extraKeywords) GT 1) THEN $
;      MESSAGE, 'GetProperty function can only process one keyword. Use GetProperty procedure for more.'
;
;   keywords = TAG_NAMES (extraKeywords)
;   ok = EXECUTE ('self -> GetProperty, ' + keywords[0] + '=result')
;
;   ; Report successful completion
;   IF (ok) THEN self -> Report, /Completed $
;   ELSE self -> Report, /Failed
;
;   RETURN, result
;END

;*****************************************************************************************************
;+
; NAME:
;       CatContainer::PRINT
;
; PURPOSE:
;
;       This method is used to print a list of the objects contents.
;
; SYNTAX:
;
;       container -> Print
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       LABEL:      Any text passed through 'Label' will be prepended onto the printed lines.
;
;       RECURSIVE:  If set, this keyword prints the contents of any child objects who
;                   are also containers. (input)
;
;-
;*****************************************************************************************************
PRO CatContainer::Print, Label=label, Recursive=recursive

   ON_ERROR, 2

   ; Process the keywords
   IF (N_ELEMENTS (label) EQ 0) THEN label = '-'

   ; Print a line for this object
   PRINT, label, OBJ_CLASS (self), ', Name: [', self._name, ']'

   ; Go through the children, printing onfo about them
   children = self -> Get (/All, Count=noChildren)

   FOR c = 0, noChildren - 1 DO $
   BEGIN
      IF (Obj_IsA_Valid (children [c], 'CatContainer')) THEN $
      BEGIN
         children [c] -> GetProperty, Name=childName
         IF (KEYWORD_SET (recursive)) THEN children [c] -> Print, Label='   ' + label, /Recursive $
         ELSE PRINT, '   ', label, OBJ_CLASS (children [c]), ', Name: [', childName, ']'
      ENDIF $
      ELSE PRINT, label, OBJ_CLASS (children [c])
   ENDFOR

END


;*****************************************************************************************************
;+
; NAME:
;       CatContainer::REMOVE
;
; PURPOSE:
;
;       This method overrides the IDL_CONTAINER REMOVE method to allow the user to remove by
;       name an object with reference counting.
;
; SYNTAX:
;
;       self -> Remove, object
;
; ARGUMENTS:
;
;       object:    Either the object reference or name (searchName) of the object to remove.
;
; KEYWORDS:
;
;       ALL:       Set this keyword to remove all objects from the container. If both the object parameter and
;                  this keyword is set, all objects of the specified type/name will be removed.
;
;       POSITION:  Set this keyword equal to the zero-based index of the object to be removed. If the
;                  SearchName argument is supplied, this keyword is ignored.
;
;       REGEXP:    Set this keyword to indicate the searchName is a regular expression.
;
;       _EXTRA:    Any keywords supported by STREGEX can also be used. Requires REGEXP to be set.
;
;-
;*****************************************************************************************************
PRO CatContainer::Remove, obj, All=all, Position=position, RegExp=regexp, _Extra=extra

   ON_ERROR, 2

   ; Get a list of the children in the container and check there is at least 1!
   children = self -> CatContainer::Get (/All, Count=noChildren)
   IF (noChildren EQ 0) THEN RETURN

   ; Set up an array of deletion flags
   removeFlags = BYTARR (noChildren) ; array of removal flags - all false (zero) by default

   ; If no object ref or name is given, set up the deletion flags accordingly
   IF (N_ELEMENTS (obj) EQ 0) THEN $
   BEGIN
      IF (KEYWORD_SET (all)) THEN removeFlags [*] = 1B $ ; mark all for removal
      ELSE IF (N_ELEMENTS (position) NE 0) THEN $
      BEGIN
         IF ((position LT 0) OR (position GE noChildren)) THEN $
            MESSAGE, 'Attempt to remove from position ' + STRTRIM (position, 2) + ' is out of range.'
         removeFlags [position] = 1B
      ENDIF $
      ELSE removeFlags [0] = 1B ; mark the first for removal (the default)
   ENDIF $

   ; Otherwise, cycle through the objects, setting the flags accordingly
   ELSE BEGIN
      objType = SIZE (obj, /TName)
      CASE objType OF
         'OBJREF' : FOR c = 0, noChildren - 1 DO $
                    BEGIN
                       IF (children [c] EQ obj) THEN removeFlags [c] = 1B
                    ENDFOR
         'STRING' : BEGIN
                       matches = self -> FindByName (obj, RegExp=regexp, Count=c, _Extra=extra)
                       IF (c GT 0) THEN removeFlags [matches] = 1B
                    END
          ELSE    : MESSAGE, 'Remove parameter must be either a STRING or object reference'
      ENDCASE
   ENDELSE

   ; Work DOWN through the container (so that indices don't change), removing matching objects
   removeLocns = WHERE (removeFlags, noRemoveLocns)
   FOR i = noRemoveLocns - 1, 0, -1 DO $
   BEGIN
      pos = removeLocns [i]
      IF (OBJ_VALID (children[pos]) AND self._memoryManagement) THEN $
            IF Obj_Isa_Valid(children[pos], 'CATCONTAINER') THEN children[pos] -> RemoveParent, self
      self -> IDL_CONTAINER::Remove, Position=pos
      IF (self._indexed) THEN self -> IDL_CONTAINER::Add, OBJ_NEW (), Position=pos
   ENDFOR

END


;*****************************************************************************************************
;+
; NAME:
;       CatContainer::REMOVEPARENT
;
; PURPOSE:
;
;       This method removes a parent reference from an object list. It is used internally in
;       the container object and should not be called from outside.
;
;       If the last parent is removed and the AutoDestroy property is TRUE, the object will
;       self destruct (as it is no longer required).
;
; SYNTAX:
;
;       container -> RemoveParent, thisParent
;
; ARGUMENTS:
;
;       thisParent: The object reference of the parent to be removed.
;
; KEYWORDS:
;
;       NODESTROY:  If this keyword is set, the AUTO_DESTROY keyword is set to 0 on the object
;                   before its parent is removed, and set back to its input value on exit. The
;                   point of setting the NODESTROY keyword is to be able to remove object
;                   parents prior to SAVING the object. (If this is not done, the entire
;                   object hierarchy will be SAVED with the SAVE file.)
;
;-
;*****************************************************************************************************
PRO CatContainer::RemoveParent, thisParent, NODESTROY=nodestroy

   ON_ERROR, 2
   IF (NOT OBJ_VALID (self._parents)) THEN RETURN

   ; Save the current autoDestroy state and check to see if it should be changed.
   autoState = self._autoDestroy
   IF Keyword_Set(nodestroy) THEN self._autoDestroy = 0

   ; Remove this parent object from the parent container.
   self._parents -> Remove, thisParent

   ; If this is the last parent, and autoDestroy is turned on, destroy yourself.
   IF (self._autoDestroy) THEN $
   BEGIN
      parents = self._parents -> Get (/All)
      junk = WHERE (OBJ_VALID (parents), noValidParents)
      IF (noValidParents EQ 0) THEN OBJ_DESTROY, self
   END

   ; If the object is still valid, restore the incoming autoDestroy state.
   IF Obj_Valid(self) THEN self._autoDestroy = autoState

END


;*****************************************************************************************************
;+
; NAME:
;       CatContainer::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the object's properties.
;
; SYNTAX:
;
;       self -> SetProperty, NAME=theName
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       AUTO_DESTROY: The CATCONTAINER object keeps track of the number of parents it has (reference
;                     counting). When the reference count changes from 1 to 0, the object is automatically
;                     destroyed. Setting AUTO_DESTROY to 0 will prevent this automatic destruction.
;                     By default, this keyword is set to 1. Note that if you set AUTO_DESTROY=0
;                     you will be responsible for destroying the object when you are finished with it.
;
;
;       NAME:         The "name" of the object. Used to keep track of the object in the code. (Input)
;
;-
;*****************************************************************************************************
PRO CatContainer::SetProperty, NAME=name, AUTO_DESTROY=auto_destroy

   ON_ERROR, 2
   IF (N_ELEMENTS (name        ) NE 0) THEN self._name = name
   IF (N_ELEMENTS (auto_destroy) NE 0) THEN $
   BEGIN
      self._autoDestroy = KEYWORD_SET (auto_destroy)
   ENDIF

END


;*****************************************************************************************************
;+
; NAME:
;       CATCONTAINER::SHOWCONTENTS
;
; PURPOSE:
;
;       This method is used to graphically represent the contents of an object.
;       It does a recursive search through the object containers.
;
; SYNTAX:
;
;       self -> ShowContents
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       _EXTRA:   Any keywords appropriate for the PROPERTYPANEL object.
;-
;*****************************************************************************************************
PRO CatContainer::ShowContents, _EXTRA=extra

   void = Obj_New('PropertyPanel', self, _EXTRA=extra)

END


;*****************************************************************************************************
;+
; NAME:
;       CatContainer::CLEANUP
;
; PURPOSE:
;
;       This is the CatContainer object class destructor method.
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
PRO CatContainer::CLEANUP

   ON_ERROR, 2

   ; Remove all the child objects from the container so they don't get destroyed automatically
   self -> Remove, /ALL

   ; Remove my reference from my parent's containers, then delete the parent's container itself
   IF (OBJ_VALID (self._parents)) THEN $
   BEGIN
      parents = self._parents -> IDL_CONTAINER::Get (/All, Count=noParents)
      FOR p = 0, noParents - 1 DO $
         IF (OBJ_VALID (parents [p])) THEN parents [p] -> IDL_CONTAINER::Remove, self
      self._parents -> IDL_CONTAINER::Remove, /All ; remove the parent references so they don't get destroyed!
      OBJ_DESTROY, self._parents
   ENDIF

   ; Destroy the container and the parent's list
   self -> IDL_CONTAINER::Cleanup


END


;*****************************************************************************************************
;+
; NAME:
;       CatContainer::INIT
;
; PURPOSE:
;
;       This is the CatContainer object class creator method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       PARENT:      A parent container object. Must be subclassed from IDL_CONTAINER. The object
;                    will be added to its parent container. (Input)
;
; KEYWORDS:
;
;       AUTO_DESTROY: The CATCONTAINER object keeps track of the number of parents it has (reference
;                     counting). When the reference count changes from 1 to 0, the object is automatically
;                     destroyed. Setting AUTO_DESTROY to 0 will prevent this automatic destruction.
;                     By default, this keyword is set to 1. Note that if you set AUTO_DESTROY=0
;                     you will be responsible for destroying the object when you are finished with it.
;                     (In reference counting, when an object's reference count goes to 0, the object
;                     is automatically destroyed. Setting this keyword prevents the automatic destruction
;                     that would occur otherwise.)
;
;       INDEXED:      If set, the objects that are added to the container will always have
;                     the same index value they are assigned when they are added to the container.
;                     If objects are later removed from the container, a null object is added in that
;                     indexed space, thus assuring that all objects keep their assigned index number.
;
;       MEMORY_MANAGEMENT: Setting this keyword to 0 disables automatic memory management for
;                     this container. This means that objects added to the container will not realize they
;                     are children of this container. The default is to set this keyword to 1. (This
;                     is "reference counting".)
;
;       NAME:         The "name" of the object. Used to keep track of the object in the code.
;                     You can "GET" an object by its name, for example.  Any string is acceptible. (Input)
;
;       POSITION:     This object's position in the parent container. See the IDL_CONTAINER
;                     documentation. (Input)
;
;-
;*****************************************************************************************************
FUNCTION CatContainer::INIT, parent,                                $
                             Auto_Destroy      = auto_destroy,      $
                             Indexed           = indexed,           $
                             Memory_Management = memory_management, $
                             Name              = name,              $
                             Position          = position

   ; Error handling
   ON_ERROR, 2

   ; Call superclass INIT method
   ok = self -> IDL_CONTAINER::INIT()
   IF NOT ok THEN MESSAGE, 'Failed to initialise IDL_CONTAINER superclass.'

   ; Initialise the list of parents
   self._parents = OBJ_NEW ('IDL_CONTAINER')

   ; If the auto_destroy keyword is zero, disable ref count destruction
   IF (N_ELEMENTS (auto_destroy) EQ 0) THEN self._autoDestroy = 1B $
   ELSE self._autoDestroy = KEYWORD_SET (auto_destroy)

   ; If the memory_management keyword is zero, disable child notification
   IF (N_ELEMENTS (memory_management) EQ 0) THEN self._memoryManagement = 1B $
   ELSE self._memoryManagement = KEYWORD_SET (memory_mamangement)

   ; Process the keyword parameters
   IF (N_ELEMENTS (name) GT 0) THEN self._name = name $
   ELSE BEGIN ; Ashamedly, I'm extracting the unique object number from the help call:-p
      HELP, self, Output=hlp
      hlp   = STRSPLIT (hlp, '<(', /Extract)
      objNo = STRMID (hlp [1], 10)
      self._name = OBJ_CLASS (self) + '_' + objNo
   ENDELSE
   self._indexed = KEYWORD_SET (indexed)

   FOR parentNo = 0, N_ELEMENTS (parent) - 1 DO $ ; if no. parents = 0, this loop is skipped
   BEGIN

      IF (NOT Obj_IsA_Valid (parent[parentNo], 'IDL_CONTAINER')) THEN $
         MESSAGE, 'Parent object must be a valid IDL_CONTAINER object.' $
      ELSE parent[parentNo] -> Add, self, POSITION=position

   ENDFOR

   ; Useful for debugging purposes and finding leaking memory.
   ;Print, self._name

   ; Return successful execution flag.
   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       CatContainer CLASS DEFINITION
;
; PURPOSE:
;
;       This is the CatContainer object's structure definition code.
;
;*****************************************************************************************************
PRO CatContainer__DEFINE, class


   class  = { CatContainer,                   $ ; The CatContainer object class name.
              _autoDestroy      : 0B,         $ ; A flag denoting whether the object self destructs
              _name             : "",         $ ; The "name" of the object.
              _parents          : OBJ_NEW (), $ ; A list (container) of parent objects
              _memoryManagement : 0B,         $ ; A flag to show if children know about this object
              _indexed          : 0B,         $ ; A flag to specify whether the list is indexed
              INHERITS IDL_CONTAINER          $
            }
END