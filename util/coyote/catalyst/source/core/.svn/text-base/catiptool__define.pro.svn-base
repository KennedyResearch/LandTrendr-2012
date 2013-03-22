;*****************************************************************************************************
;+
; NAME:
;       CATIPTOOL
;
; PURPOSE:
;
;       The purpose of this routine is to implement an image processing command
;       as a tool object, giving the command the ability to be undone. Any
;       image processing command can be used. The command will call the GETDATA
;       method on the target image object to obtain the data to processes. Any
;       number of keywords are supported in the object.
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
; CATEGORY:
;
;       Objects.
;
; SYNTAX:
;
;       aTool = Obj_New("CATIPTOOL", command, targetImageObject, Keyword=keyword)
;
; SUPERCLASSES:
;
;       CATTOOL
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { CATIPTOOL, $           ; The CATIPTOOL object class.
;             INHERITS CATTOOL, $    ; Inherits the CATTOOL object class.
;             _command: "", $        ; The IDL image processing command.
;             _p1: Ptr_New(), $      ; The first additional parameter of the command.
;             _p2: Ptr_New(), $      ; The second additional parameter of the command.
;             _p3: Ptr_New(), $      ; The third additional parameter of the command.
;             _nparams: 0L, $        ; The number of additional parameters.
;             _keywords: Ptr_New() $ ; A structure of keywords used with the command.
;           }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, July 14, 2003.
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
;       CATIPTOOL::APPLY
;
; PURPOSE:
;
;       This method applies or executes the image processing command.
;;
; SYNTAX:
;
;       thisObject -> Apply
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DRAW:     Set this keyword if you wish to call the draw method of the target object
;                 after the command has been applied.
;-
;*****************************************************************************************************
PRO CatIPTool::Apply, Draw=draw

   @cat_pro_error_handler

   ; Make sure there is a target object.

   IF Obj_Valid(self._theTarget) EQ 0 THEN $
      Message, 'The ' + StrUpCase(self._command) + ' operation requires a target object.'

   ; Cache the input image unless you have been told not to.

   IF self._noCache EQ 0 THEN self -> Cache, self._theTarget -> GetData(ORIGINAL=self._original)

   ; Apply the image processing command to the target image data.

   IF N_Elements(*self._keywords) EQ 0 THEN BEGIN
      CASE self._nparams OF
         0: self._theTarget -> SetData, $
               Call_Function(self._command, self._theTarget -> GetData(ORIGINAL=self._original)), $
               ORIGINAL=self._original
         1: self._theTarget -> SetData, Call_Function(self._command, self._theTarget -> GetData(ORIGINAL=self._original), $
               *self._p1), ORIGINAL=self._original
         2: self._theTarget -> SetData, $
               Call_Function(self._command, self._theTarget -> GetData(ORIGINAL=self._original), $
               *self._p1, *self._p2), ORIGINAL=self._original
         3: self._theTarget -> SetData, $
               Call_Function(self._command, self._theTarget -> GetData(ORIGINAL=self._original), $
               *self._p1, *self._p2, *self._p3), ORIGINAL=self._original
      ENDCASE
   ENDIF ELSE BEGIN
      CASE self._nparams OF
         0: self._theTarget -> SetData, $
               Call_Function(self._command, self._theTarget -> GetData(ORIGINAL=self._original), $
               _Extra=*self._keywords), ORIGINAL=self._original
         1: self._theTarget -> SetData, $
               Call_Function(self._command, self._theTarget -> GetData(ORIGINAL=self._original), $
               *self._p1, _Extra=*self._keywords), ORIGINAL=self._original
         2: self._theTarget -> SetData, $
               Call_Function(self._command, self._theTarget -> GetData(ORIGINAL=self._original), $
               *self._p1, *self._p2, _Extra=*self._keywords), ORIGINAL=self._original
         3: self._theTarget -> SetData, $
               Call_Function(self._command, self._theTarget -> GetData(ORIGINAL=self._original), $
               *self._p1, *self._p2, *self._p3, _Extra=*self._keywords), ORIGINAL=self._original
      ENDCASE
   ENDELSE

   ; Need a draw method?

   IF Keyword_Set(draw) THEN self._theTarget -> Draw

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATIPTOOL::GETPROPERTY
;
; PURPOSE:
;
;       This method enables the getting of the CATIPTOOL object class
;       properties.
;
; SYNTAX:
;
;       aGraphicsCmd -> GetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     _EXTRA:           Any keywords appropriate for the superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO CatIPTool::GetProperty, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATTOOL::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATIPTOOL::LoadCommand
;
; PURPOSE:
;
;       This method allows the user to load a command into the object.
;
; SYNTAX:
;
;       aGraphicsCmd -> LoadCommand, 'command', targetObject, p1, p2, p3
;
; ARGUMENTS:
;
;       command:      This is the name of the image processing "command" that is to be executed.
;                     It should be a string. The image processing command should also be a function.
;                     For example, "SOBEL" or "SMOOTH".
;
;       targetObject: The image object that will provide the image data to be processed by this
;                     image processing command object.
;
;       p1:           This is the first additional parameter of the image processing command.
;                     The *first* parameter of the image processing command is always the
;                     image data obtained from the targetObject with the GETDATA method. For
;                     example, in this case P1 will be equal to 7:
;
;                         smoothObj = Obj_New('CATIPTOOL', 'Smooth', TARGETOBJECT=targetObject, 7)
;
;                     And the command that will be executed is this:
;
;                         targetObject -> SetData, Smooth(targetObject->GetData(), 7)
;
;       p2:           This is the second additional parameter of the image processing command.
;
;       p3:           This is the third additional parameter of the image processing command.
;
; KEYWORDS:
;
;     DRAW:           Set this keyword to immediately call the Draw method when the command is executed.
;
;      TARGETOBJECT:  The image object that is the target of this command. Note that a target object
;                     is REQUIRED for the command to work correctly, although you do not have to
;                     set the target in the INIT method. You can set it via the LOADCOMMAND and SETPROPERTY
;                     methods, too. There must be a valid target at the time the APPLY method is called,
;                     or an error is generated. The targetObject must be a valid CATIMAGE2D object.
;
;     _EXTRA:         Any keywords appropriate for command may be used.
;-
;*****************************************************************************************************
PRO CatIPTool::LoadCommand, $
   command, $                   ; The image processing "command" to execute.
   p1, $                        ; The first positional parameter of the command.
   p2, $                        ; The second positional parameter of the command.
   p3, $                        ; The third positional parameter of the command.
   Draw=draw, $                 ; Set this command to immediately execute the command.
   TargetObject=targetObject, $ ; The target object of the command.
   _Extra=extraKeywords         ; And extra command keywords.

   @cat_pro_error_handler

   ; Parse the command and add parts to the object.

   IF N_Params() NE 0 THEN BEGIN

      IF Size(command, /TName) NE 'STRING' THEN $
         Message, 'First positional argument must be the name of a image processing command.'
      self._command = command
      nparams = 0

      ; Free up command pointers.
      Ptr_Free, self._p1
      Ptr_Free, self._p2
      Ptr_Free, self._p3
      Ptr_Free, self._keywords

      IF N_Elements(p1) NE 0 THEN BEGIN
         nparams = nparams + 1
         self._p1 = Ptr_New(p1)
      ENDIF
      IF N_Elements(p2) NE 0 THEN BEGIN
         nparams = nparams + 1
         self._p2 = Ptr_New(p2)
      ENDIF
      IF N_Elements(p3) NE 0 THEN BEGIN
         nparams = nparams + 1
         self._p3 = Ptr_New(p3)
      ENDIF
      self._nparams = nparams
      IF N_Elements(extraKeywords) NE 0 THEN self._keywords = Ptr_New(extraKeywords) $
         ELSE self._keywords = Ptr_New(/Allocate_Heap)

   ENDIF ELSE Message, 'At least one positional parameter is required.'

   ; Is there a targetObject?

   IF N_Elements(targetObject) NE 0 THEN self -> SetProperty, TargeObject=targetObject

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATIPTOOL::REDO
;
; PURPOSE:
;
;       This method attempts to redo the previous command operation.
;;
; SYNTAX:
;
;       thisObject -> REDO
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DRAW:     Set this keyword if you wish to call the DRAW method on the target object
;                 after the REDO operation.
;
;-
;*****************************************************************************************************
PRO CatIPTool::Redo, Draw=draw

   @cat_pro_error_handler

   self -> Apply, Draw=draw

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATIPTOOL::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the CATIPTOOL object class
;       properties.
;
; SYNTAX:
;
;       aGraphicsCmd -> SetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;      TARGETOBJECT:  The image object that is the target of this command. Note that a target object
;                     is REQUIRED for the command to work correctly, although you do not have to
;                     set the target in the this method. You can set it via the LOADCOMMAND and INIT
;                     methods, too. There must be a valid target at the time the APPLY method is called,
;                     or an error is generated. The targetObject must be a valid CATIMAGE2D object.
;
;
;     _EXTRA:         Any keywords appropriate for the superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO CatIPTool::SetProperty, $
   TARGETOBJECT=targetObject, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(targetObject) NE 0 THEN $
   BEGIN
     IF Obj_IsA_Valid(targetObject, 'CATIMAGE2D') THEN self._theTarget = targetObject ELSE $
        Message, 'The target object is invalid or not a sub-class of CATIMAGE2D.'
   ENDIF
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATTOOL::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATIPTOOL::UNDO
;
; PURPOSE:
;
;       This method attempts to undo the previous command operation by restoring
;       the cached image data.
;;
; SYNTAX:
;
;       thisObject -> UNDO
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DRAW:     Set this keyword if you wish to call the DRAW method on the target object
;                 after the UNDO operation.
;-
;*****************************************************************************************************
PRO CatIPTool::Undo, DRAW=draw

   @cat_pro_error_handler

   ; Replace the cached data in the target object.

   IF Ptr_Valid(self._theCache) EQ 0 THEN RETURN
   self._theTarget -> SetData, *self._theCache, Original = self._original

   ; Need a draw?

   IF Keyword_Set(draw) THEN self._theTarget -> Draw

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATIPTOOL::CLEANUP
;
; PURPOSE:
;
;       This is the CATIPTOOL object class destructor method.
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
;       _EXTRA:  Any keyword appropriate for the "TOOLATOM::Cleanup" method.
;-
;*****************************************************************************************************
PRO CatIPTool::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self._p1
   Ptr_Free, self._p2
   Ptr_Free, self._p3
   Ptr_Free, self._keywords

   self -> CATTOOL::CLEANUP
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATIPTOOL::INIT
;
; PURPOSE:
;
;       This is the CATIPTOOL object class initialization method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       command:      This is the name of the image processing "command" that is to be executed.
;                     It should be a string. The image processing command should also be a function.
;                     For example, "SOBEL" or "SMOOTH".
;
;       targetObject: The image object that will provide the image data to be processed by this
;                     image processing command object.
;
;       p1:           This is the first additional parameter of the image processing command.
;                     The *first* parameter of the image processing command is always the
;                     image data obtained from the targetObject with the GETDATA method. For
;                     example, in this case P1 will be equal to 7:
;
;                         smoothObj = Obj_New('CATIPTOOL', 'Smooth', targetObject, 7)
;
;                     And the command that will be executed is this:
;
;                         targetObject -> SetData, Smooth(targetObject->GetData(), 7)
;
;       p2:           This is the second additional parameter of the image processing command.
;
;       p3:           This is the third additional parameter of the image processing command.
;
; KEYWORDS:
;
;       AUTO_DESTROY: This object keeps track of the number of parents it has (reference counting). When
;                     this value changes from 1 to 0, the object will be destroyed unless Auto_Destroy
;                     has been set to zero. (Input)
;
;       EVENT_OBJECT: This is an object reference to an object that will receive the
;                     event from a specific widget event. It is the equivalent (in
;                     object terms) of specifying an EVENT_PRO or EVENT_FUNC keyword.
;                     In other words, it is a way of deflected an event from the EVENTHANDLER
;                     method of the object widget that generated the event to the EVENTHANDLER
;                     method of the object specified with this keyword.
;
;       EXCLUSIVE_EVENT_OBJECT: If this keyword is set to a vaid object, events are passed directly
;                     and only to this object, ignoring the other event objects. To disable this
;                     set this keyword to be a NULL object or zero. This keyword is designed for
;                     situations where an object wishes to hog the events for a limited period.
;
;       INDEXED:      If set, the contents of this container will remain at the same index(ie:
;                     indices will not automatically be re-used.
;
;       MEMORY_MANAGEMENT: Setting this keyword to zero disables automatic memory management for
;                     this container. This means that child objects will not realize that they
;                     are children of this container.
;
;       NAME:         The "name" of the object. Used to keep track of the object in the code.
;                     You can "GET" an object by its name, for example.  Any string is acceptible. (Input)
;
;       NO_COPY:      Set this keyword to transfer the UVALUE without copying.
;
;       NOCACHE:      Set this keyword to indicate that the input data should not be cached
;                     or stored in the storage location.
;
;       ORIGINAL:     If set to 1, act on original image data instead of image display data.
;
;       PARENT:       A parent container object. Must be subclassed from IDL_CONTAINER. The object
;                     will be added to its parent container. (Input)
;
;       POSITION:     This object's position in the parent container. See the IDL_CONTAINER
;                     documentation. (Input)
;
;       REPORTLEVEL:  The reporting level for the atom. This is honored only if the reporting
;                     level for the atom is higher than the reporting level in the !Smart system
;                     variable. It is used primarily for debugging purposes. Values between 0 and 2. (Input)
;
;       TARGETOBJECT: The image object that is the target of this command. Note that a target object
;                     is REQUIRED for the command to work correctly, although you do not have to
;                     set the target in the INIT method. You can set it via the LOADCOMMAND and SETPROPERTY
;                     methods, too. There must be a valid target at the time the APPLY method is called,
;                     or an error is generated. The targetObject must be a valid CATIMAGE2D object.
;
;       UVALUE:       A user-value pointer. Can be used to store any IDL variable type.
;
;       _EXTRA:       Any keywords appropriate for the image processing command.
;-
;*****************************************************************************************************
FUNCTION CatIPTool::INIT, $
   command, $                   ; The image processing "command" to execute.
   p1, $                        ; The first additional parameter of the command.
   p2, $                        ; The second additional parameter of the command.
   p3, $                        ; The third additional parameter of the command

   ; Must include all superclass keywords here, since I can't afford to
   ; pass undefined keywords down to CATATOM and I need to be able to pick up all
   ; command keywords without knowing what they are. If you add a SUPERCLASS
   ; keyword, be sure to also make the change here.

   Auto_Destroy=auto_destroy, $
   Event_Object=evObj, $
   Exclusive_Event_Obj=eev, $
   Indexed=indexed, $
   Memory_Management=mm, $
   Name=name, $
   No_Copy=no_copy, $
   NoCache=nocache, $
   Original=original, $
   Parent=parent, $
   Position=position, $
   ReportLevel=reportlevel, $
   TargetObject=targetObject, $
   UValue=uvalue, $

   _Extra=extraKeywords     ; Any extra command keywords


   @cat_func_error_handler

   ; Parse the command and add parts to the object.

   IF N_Params() NE 0 THEN BEGIN

      IF Size(command, /TName) NE 'STRING' THEN $
         Message, 'First positional argument must be the name of a graphics command.'
      self._command = command

      nparams = 0
      IF N_Elements(p1) NE 0 THEN BEGIN
         nparams = nparams + 1
         self._p1 = Ptr_New(p1)
      ENDIF
      IF N_Elements(p2) NE 0 THEN BEGIN
         nparams = nparams + 1
         self._p2 = Ptr_New(p2)
      ENDIF
      IF N_Elements(p3) NE 0 THEN BEGIN
         nparams = nparams + 1
         self._p3 = Ptr_New(p3)
      ENDIF
      self._nparams = nparams
      IF N_Elements(extraKeywords) NE 0 THEN self._keywords = Ptr_New(extraKeywords) $
         ELSE self._keywords = Ptr_New(/Allocate_Heap)

   ENDIF

   ; Is there a target object here?

   IF N_Elements(targetObject) NE 0 THEN self -> SetProperty, TARGETOBJECT=targetObject


   ; Call superclass INIT method
   ok = self -> CATTOOL::Init ( $
      Auto_Destroy=auto_destroy, $
      Event_Object=evObj, $
      Exclusive_Event_Obj=eev, $
      Indexed=indexed, $
      Memory_Management=mm, $
      Name=name, $
      No_Copy=no_copy, $
      NoCache=nocache, $
      Parent=parent, $
      Position=position, $
      ReportLevel=reportlevel, $
      UValue=uvalue)

   ; Set the original data flag.
   self._original = Keyword_Set(original)

   IF (ok) THEN self -> Report, /Completed $
   ELSE self -> Report, /Failed

   RETURN, ok

END


;*****************************************************************************************************
;
; NAME:
;       CATIPTOOL OBJECT DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the CATIPTOOL object.
;
;*****************************************************************************************************
PRO CatIPTool__DEFINE, class

   class = { CATIPTOOL, $           ; The CATIPTOOL object class.
             INHERITS CATTOOL, $    ; Inherits the CATTOOL object class.
             _command: "", $        ; The IDL image processing command.
             _p1: Ptr_New(), $      ; The first additional parameter of the command.
             _p2: Ptr_New(), $      ; The second additional parameter of the command.
             _p3: Ptr_New(), $      ; The third additional parameter of the command.
             _original: 0L, $       ; A flag. If set to 1, use original image data.
             _nparams: 0L, $        ; The number of additional parameters.
             _keywords: Ptr_New() $ ; A structure of keywords used with the command.
           }

END
