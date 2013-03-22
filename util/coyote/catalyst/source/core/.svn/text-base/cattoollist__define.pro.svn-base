;*****************************************************************************************************
;+
; NAME:
;       CatToolList
;
; PURPOSE:
;
;        This is object is part of the Catalyst Library. It presents a mechanism for
;        creating and managing lists of tools (objects that operate on others).
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
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; SYNTAX:
;
;       ToolListObject = Obj_New('CatToolList')
;
; CLASS_STRUCTURE:
;
;       struct = {CatToolList, _current:0L, _limit:0L, INHERITS CatAtom}
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 29th April 2003
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
;       CatToolList::Add
;
; PURPOSE:
;
;        This method overrides the default ADD method to ensure that objects added
;        to this list are ToolAtom objects. The new object becomes the "current"
;        tool.
;
;        Note that if the tool is not added to the end of the list (i.e. previous
;        operation was an UNDO), the remaining tools will be removed.
;
;        Since the validity of the items in the list depends on it being sequential,
;        addition of tools within the list (using the POSITION keyword) causes the
;        list to be truncated.
;
;        Note that the ADD method of the IDL_CONTAINER object is used.
;        This is to bypass all memory management of CatToolList objects. These just
;        need to be added to simple containers.
;
; SYNTAX:
;
;        toolListObj -> Add, tool
;
; ARGUMENTS:
;
;        TOOL:  The tool to be added. This must be subclasses from CATTOOL.
;
; KEYWORDS:
;
;        POSITION: The position that the tool should be added to the list. If this
;                  keyword is set, the list will be truncated at the new tool.
;
;-
;*****************************************************************************************************
PRO CatToolList::Add, tool, Position=position

   ; Set up error handling
   @cat_pro_error_handler

   ; Check we have a valid tool object
   IF (NOT OBJ_ISA (tool, 'CATTOOL')) THEN MESSAGE, 'CatToolList can only have CatTool objects added.'

   ; Set the position to append to the list, if none is supplied
   noTools = self -> Count ()
   IF (N_ELEMENTS (position) EQ 0) THEN pos = self._current + 1 $
   ELSE pos = position < noTools

   ; Truncate the list at this entry (loop is skipped if position >= noTools)
   FOR toolNo = noTools - 1, pos, -1 DO self -> Remove, Position=toolNo

   ; Add the tool to the list
   self -> CATCONTAINER::Add, tool, Position=pos

   ; If memory management is on and the object we are adding is a valid CatContainer object,
   ; then add this object to that object's parent list
   IF (Obj_IsA_Valid (tool, 'CatContainer') and self._memoryManagement) THEN $
      tool -> AddParent, self

   ; Check the limit to see if it is exceeded
   IF (self._limit GT 0) THEN $
      WHILE (self -> Count () GT self._limit) DO self -> Remove, Position=0

   ; Update the current index value
   self._current = self -> Count () - 1

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CatToolList::Get
;
; PURPOSE:
;
;        This method overrides the default GET method so that a default GET (with
;        no POSITION or ALL keyword) gets the CURRENT tool (rather than the first).
;
; SYNTAX:
;
;        currentTool = toolListObj -> Get ()
;
; ARGUMENTS:
;
;        None.
;
; KEYWORDS:
;
;        POSITION: The position that the tool should be added to the list. If this
;                  keyword is set, the list will be truncated at the new tool.
;
;        ALL:      Set this keyword to get all of the tools from the list.
;
;-
;*****************************************************************************************************
FUNCTION CatToolList::Get, Position=position, All=all, Count=count

   ; Set up error handling
   @cat_func_error_handler

   ; Set the position to the current tool if none is supplied
   noTools = self -> Count ()
   IF (N_ELEMENTS (position) EQ 0) THEN pos = self._current $
   ELSE pos = position < noTools

   ; Call the superclass GET method, to get the tool(s)
   IF (pos LT 0) THEN $
   BEGIN
      result = OBJ_NEW ()
      count  = 0L
   ENDIF $
   ELSE result = self -> CatAtom::Get (Position=position, All=all, Count=count)

   ; Report completion and return result
   self -> Report, /Completed
   RETURN, result

END


;*****************************************************************************************************
;+
; NAME:
;       CatToolList::GetProperty
;
; PURPOSE:
;
;        This method exists so that properties of the object can be retrieved.
;
; SYNTAX:
;
;        toolListObj -> Add, tool
;
; ARGUMENTS:
;
;        None.
;
; KEYWORDS:
;
;        CURRENT_INDEX: The current index into the tool list array.
;
;        CURRENT_TOOL:  The current tool in the tool list array.
;
;        LIMIT:         The current limit for the number of tools in this tool list.
;
;        _REF_EXTRA:    Any extra keywords for superclass GETPROPERTY methods.
;
;-
;*****************************************************************************************************
PRO CatToolList::GetProperty, $
   Current_Index=current_index, $
   Current_Tool=current_tool, $
   Limit=limit, $
   _Ref_Extra=extrakeywords

   ; Set up error handling
   @cat_pro_error_handler

   IF Arg_Present(current_index) THEN current_index = self._current
   IF Arg_Present(limit) THEN limit   = self._limit
   IF Arg_Present(current_tool) THEN $
   BEGIN
      IF self._current LT 0 THEN current_tool = Obj_New() ELSE current_tool = self -> Get (Position=self._current)
   ENDIF

   IF N_Elements(extraKeywords) NE 0 THEN self -> CatAtom::GetProperty, _Extra=extraKeywords

   ; Report completion
   self -> Report, /Completed

END

;*****************************************************************************************************
;+
; NAME:
;       CatToolList::Remove
;
; PURPOSE:
;
;        This method overrides the default REMOVE method. The last object in the list becomes
;        the "current" tool.
;
;        Note that the REMOVE method of the IDL_CONTAINER object is used.
;        This is to bypass all memory management of CatToolList objects. These just
;        need to be removed from simple containers.
;
; SYNTAX:
;
;        toolListObj -> Remove, Position=pos
;
; ARGUMENTS:
;
;        None.
;
; KEYWORDS:
;
;        POSITION: The position of the tool to remove from the list.
;-
;*****************************************************************************************************
PRO CatToolList::Remove, Position=position, All=all

   ; Set up error handling
   @cat_pro_error_handler

   ; If the ALL keyword is set, remove everything
   IF (KEYWORD_SET (all)) THEN $
   BEGIN
      self -> IDL_CONTAINER::Remove, /All
      self._current = -1
   ENDIF $
   ELSE BEGIN

      ; Set the position to the current tool if none is supplied
      noTools = self -> Count ()
      IF (N_ELEMENTS (position) EQ 0) THEN position = self._current

      ; Remove the object at the position.
      self -> CATCONTAINER::Remove, Position=position

      ; Update the current index value
      self._current = self -> Count () - 1

   ENDELSE

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CatToolList::Redo
;
; PURPOSE:
;
;        This method includes a previously "undone" tool back into the tool list.
; SYNTAX:
;
;        toolListObj -> Redo
;
; ARGUMENTS:
;
;        None.
;
; KEYWORDS:
;
;        ALL: Set this keyword to REDO all undone items in the list, starting from the current item.
;
;-
;*****************************************************************************************************
PRO CatToolList::Redo, All=all

   ; Set up error handling
   @cat_pro_error_handler

   ; Store the number of tools in the list
   noTools = self -> Count ()

   ; If the ALL keyword is set, move through the list REDO'ing
   IF (KEYWORD_SET (all)) THEN $
   BEGIN
      tools = self -> Get (/All)
      FOR toolNo = self._current, noTools - 1 DO tools [toolNo] -> Redo
      self._current = noTools - 1
   ENDIF $

   ; Otherwise, do a single step REDO (if possible)
   ELSE IF (self._current LT noTools - 1) THEN $
   BEGIN
      tool = self -> Get (Position=self._current+1)
      tool -> Redo
      self._current = self._current + 1
   ENDIF ELSE ok = Dialog_Message('There are no tools left to REDO.')

   ; Report completion
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CatToolList::SetProperty
;
; PURPOSE:
;
;        This method exists so that properties of the object can be set.
;
; SYNTAX:
;
;        toolListObj -> SetProperty, Limit=10
;
; ARGUMENTS:
;
;        None.
;
; KEYWORDS:
;
;        LIMIT:         The current limit for the number of tools in this tool list.
;
;        _F_EXTRA:      Any extra keywords for superclass SETPROPERTY methods.
;
;-
;*****************************************************************************************************
PRO CatToolList::SetProperty, Limit=limit, _Extra=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(limit) NE 0 THEN self._limit = limit

   IF N_Elements(extraKeywords NE 0) THEN self -> CatAtom::SetProperty, _Extra=extraKeywords

   ; Report completion
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CatToolList::Undo
;
; PURPOSE:
;
;        This method UNDOes a tool from tool list.
; SYNTAX:
;
;        toolListObj -> Undo
;
; ARGUMENTS:
;
;        None.
;
; KEYWORDS:
;
;        ALL: Set this keyword to UNDO all items in the list, starting from the current item.
;
;-
;*****************************************************************************************************
PRO CatToolList::Undo, All=all

   ; Set up error handling
   @cat_pro_error_handler

   ; Store the number of tools in the list
   noTools = self -> Count ()

   ; If the ALL keyword is set, move through the list UNDO'ing
   IF (KEYWORD_SET (all)) THEN $
   BEGIN
      tools = self -> Get (/All)
      FOR toolNo = self._current-1, 0, -1 DO tools [toolNo] -> Undo
      self._current = -1
   ENDIF $

   ; Otherwise, do a single step UNDO (if possible)
   ELSE IF (self._current GE 0) THEN $
   BEGIN
      tool = self -> Get (Position=self._current)
      tool -> Undo
      self._current = self._current - 1
   ENDIF ELSE ok = Dialog_Message(['There are no more tools to UNDO.', $
      'The tool limit for this object is ' + StrTrim(self._limit, 2) + '.'])

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CatToolList::CLEANUP
;
; PURPOSE:
;
;       This is the CatToolList object class destructor method.
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
PRO CatToolList::Cleanup

   @cat_pro_error_handler
   items = self -> Get(/All)
   FOR j=0,N_Elements(items)-1 DO Obj_Destroy, items[j]
   self -> CatAtom::Cleanup
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CatToolList::INIT
;
; PURPOSE:
;
;       This is the CatToolList object class creator method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       PARENT: The parent object that will contain the tool list.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
FUNCTION CatToolList::INIT, parent, Limit=limit, _Extra=extraKeywords

   @cat_func_error_handler

   ; Call the superclass INIT method
   ok = self -> CatAtom::Init (parent, _Strict_Extra=extraKeywords)
   IF NOT ok THEN MESSAGE, 'Failed to initialise list.'

   ; Process the input keywords
   IF (N_ELEMENTS (limit) GT 0) THEN self._limit = limit

   ; Set the current index to be -1 (no tools loaded)
   self._current = -1L

   ; Report success
   self -> Report, /Completed
   RETURN, 1

END

;*****************************************************************************************************
;
; NAME:
;       CatToolList CLASS DEFINITION
;
; PURPOSE:
;
;       This is the CatToolList object's structure definition code.
;
;*****************************************************************************************************
PRO CatToolList__Define, class

   class = {CatToolList, _current:0L, _limit:0L, INHERITS CatAtom}

END