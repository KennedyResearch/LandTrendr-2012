;*****************************************************************************************************
;+
; NAME:
;       CATGRAPHICSCMDTOOL__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement any IDL graphics command
;       procedure as a tool object, giving the command the ability to be undone. Any
;       graphics command can be used as long as it requires no more than three
;       positional parameters. Any number of keywords are supported.
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
;       aTool = Obj_New("CATGRAPHICSCMDTOOL", command, p1, p2, p2, Keyword=keyword)
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
;   class = { CATGRAPHICSCMDTOOL, $  ; The CATGRAPHICSCMDTOOL object class.
;             INHERITS CATTOOL, $    ; Inherits the CATTOOL object class.
;             _command: "", $        ; The IDL graphics command.
;             _coords: Obj_New(), $  ; A CATCOORD coordinates object.
;             _colors: Obj_New(), $  ; A COLORTOOL color object.
;             _decomposed: 0L, $     ; The color decomposition state for the command.
;             _p1: Ptr_New(), $      ; The first positional parameter of the command.
;             _p2: Ptr_New(), $      ; The second positional parameter of the command.
;             _p3: Ptr_New(), $      ; The third positional parameter of the command.
;             _nparams: 0L, $        ; The number of parameters used in the call.
;             _keywords: Ptr_New(),  ; A structure of keywords used with the command.
;           }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, April 24, 2003.
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
;       CATGRAPHICSCMDTOOL::DRAW
;
; PURPOSE:
;
;       This method draws or executes the graphics command.
;;
; SYNTAX:
;
;       thisObject -> Draw
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
PRO CatGraphicsCmdTool::Draw, _Extra=extraKeywords

   @cat_pro_error_handler

   IF self._command EQ "" THEN Message, 'There is no graphics command to execute.'

   ; Cache the current graphics window.

   IF (!D.FLAGS AND 256) NE 0 THEN BEGIN
      IF self._noCache EQ 0 THEN self -> Cache, {wid: !D.Window, contents:TVREAD()}

      ; Set the correct color decomposition state for this command.

      Device, Get_Decomposed=theState, Decomposed=self._decomposed

   ENDIF

   ; Load the command colors, if any.

   IF Obj_Valid(self._colors) THEN self._colors -> Draw

   ; Execute the command.

   IF N_Elements(*self._keywords) EQ 0 THEN BEGIN
      CASE self._nparams OF
         0: Call_Procedure, self._command
         1: Call_Procedure, self._command, *self._p1
         2: Call_Procedure, self._command, *self._p1, *self._p2
         3: Call_Procedure, self._command, *self._p1, *self._p2, *self._p3
      ENDCASE
   ENDIF ELSE BEGIN
      CASE self._nparams OF
         0: Call_Procedure, self._command, _Extra=*self._keywords
         1: Call_Procedure, self._command, *self._p1, _Extra=*self._keywords
         2: Call_Procedure, self._command, *self._p1, *self._p2, _Extra=*self._keywords
         3: Call_Procedure, self._command, *self._p1, *self._p2, *self._p3, _Extra=*self._keywords
      ENDCASE
   ENDELSE

   ; Set the color decomposition state back.

   IF (!D.Flags AND 256) NE 0 THEN Device, Decomposed=decomposedState

   ; If the COORDS object is invalid, fill it out now. Otherwise, update it.

   IF Obj_Valid(self._coords) EQ 0 THEN $
      self._coords = Obj_New('PLOTCOORD', PSYSVAR=!P, XSYSVAR=!X, YSYSVAR=!Y, ZSYSVAR=!Z) ELSE $
      self._coords -> SaveCoords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATGRAPHICSCMDTOOL::GETPROPERTY
;
; PURPOSE:
;
;       This method enables the getting of the CATGRAPHICSCMDTOOL object class
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
;     CAT_COLOR_OBJECT: A colortool object subclassed from COLORTOOL. Sets up the color table for
;                       the graphics command.
;
;     CAT_COORD_OBJECT: A coordinates object subclassed from CATCOORD. The coordinates object is
;                       automatically updated *after* the DRAW method is completed, which keeps
;                       the data coordinate system up to date for subsequent interaction with
;                       the graphics output.
;
;     COMMAND:          The current graphics command.
;
;     KEYWORDS:         The keywords associated with the graphics command.
;
;     NPARAMS:          The number of parameters associated with the graphics command.
;
;     P1:               The data associated with the first positional parameter of the graphics command.
;
;     P2:               The data associated with the second positional parameter of the graphics command.
;
;     P3:               The data associated with the third positional parameter of the graphics command.
;
;     _EXTRA:           Any keywords appropriate for the superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO CatGraphicsCmdTool::GetProperty, $
   CAT_COLOR_OBJECT=cat_color_object, $
   CAT_COORD_OBJECT=cat_coord_object, $
   COMMAND=command, $
   KEYWORDS=keywords, $
   NPARAMS=nparams, $
   P1=p1, $
   P2=p2, $
   P3=p3, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(cat_color_object) THEN cat_color_object = self._colors
   IF Arg_Present(cat_coord_object) THEN cat_coord_object = self._coords
   IF Arg_Present(command) THEN command = self._command
   IF Arg_Present(keywords) THEN IF Ptr_Valid(keywords) THEN keywords = *self._keywords
   IF Arg_Present(nparams) THEN nparams = self._nparams
   IF Arg_Present(p1) THEN IF Ptr_Valid(self._p1) THEN p1 = *self._p1
   IF Arg_Present(p2) THEN IF Ptr_Valid(self._p2) THEN p2 = *self._p2
   IF Arg_Present(p3) THEN IF Ptr_Valid(self._p3) THEN p3 = *self._p3

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATTOOL::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATGRAPHICSCMDTOOL::LoadCommand
;
; PURPOSE:
;
;       This method allows the user to load a command into the object.
;
; SYNTAX:
;
;       aGraphicsCmd -> LoadCommand, 'command', p1, p2, p3
;
; ARGUMENTS:
;
;       Command:      This is the name of the graphics "command" that is to be executed.
;                     It should be a string. For example, "Surface" or "Contour".
;
;       P1:           This is the first positional parameter of the graphics command.
;
;       P2:           This is the second positional parameter of the graphics command.
;
;       P3:           This is the third positional parameter of the graphics command.
;
;
; KEYWORDS:
;
;     DRAW:          Set this keyword to immediately call the Draw method when the command is loaded.
;
;     _EXTRA:        Any keywords appropriate for command may be used.
;-
;*****************************************************************************************************
PRO CatGraphicsCmdTool::LoadCommand, $
   command, $                   ; The graphics "command" to execute.
   p1, $                        ; The first positional parameter of the command.
   p2, $                        ; The second positional parameter of the command.
   p3, $                        ; The third positional parameter of the command.
   Draw=draw, $                 ; Set this command to immediately execute the command.
   _Extra=extraKeywords         ; And extra command keywords.

   @cat_pro_error_handler

   ; Parse the command and add parts to the object.

   IF N_Params() NE 0 THEN BEGIN

      IF Size(command, /TName) NE 'STRING' THEN $
         Message, 'First positional argument must be the name of a graphics command.'
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

   ; Need a draw?
   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATGRAPHICSCMDTOOL::PIXEL_TO_VALUE
;
; PURPOSE:
;
;       This method calculates the data value underneath a pixel location
;       in the display window. To be accurate, the window that contains the image
;       MUST BE the current graphics window or you MUST PASS the window number in
;       with the WINDOWINDEX keyword.
;
; SYNTAX:
;
;       data_value = aGraphicsCmd -> Pixel_to_Value(x, y)
;
; RETURN_VALUE:
;
;       data_value:  A two element array of the x and y points converted to data coordinates.
;
; ARGUMENTS:
;
;       X:           The x location in the display window (pixel coordinates).
;
;       Y:           The y location in the display window (pixel coordinates).
;
; INPUT_KEYWORDS:
;
;       WINDOWINDEX: The window index number of the window containing the image object. Set
;                    to !D.Window by default.
;
; OUTPUT_KEYWORDS:
;
;       INSIDE:      This keyword is set to 1 if the (x,y) value is inside the rectangle formed
;                    by [!X.Crange[0], !Y.Crange[0], !X.Crange[1], !Y.Crange[1]]. Otherwise, it
;                    is set to 0.
;
;-
;*****************************************************************************************************
FUNCTION CatGraphicsCmdTool::Pixel_to_Value, x, y, Inside=inside, $
   WindowIndex=wid

   @cat_func_error_handler

   IF N_Elements(wid) EQ 0 THEN wid = !D.Window
   currentWindow = !D.Window
   WSet, wid

   inside = 0

   ; Get the data coordinate system.

   IF Obj_Isa_Valid(self._coords, 'PLOTCOORD') THEN BEGIN
      self._coords -> Draw
      c = Convert_Coord(x, y, /Device, /To_Data)
      retValue = [c[0,0], c[1,0]]
      IF (retValue[0] GE !X.CRange[0]) AND (retValue[0] LE !X.Crange[1]) AND $
         (retValue[1] GE !Y.CRange[0]) AND (retValue[1] LE !Y.Crange[1]) THEN inside = 1
      self._coords -> Restore
   ENDIF ELSE BEGIN
      retValue = [x, y]
   ENDELSE

   ; Don't return byte values because they make lousy strings. :-(

   IF Size(retValue, /TNAME) EQ 'BYTE' THEN retValue=Fix(retValue)

   ; Set the window back to entering value.

   IF currentWindow GE 0 THEN WSet, currentWindow

   RETURN, retValue
END


;*****************************************************************************************************
;+
; NAME:
;       CATGRAPHICSCMDTOOL::REDO
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
;       None.
;-
;*****************************************************************************************************
PRO CatGraphicsCmdTool::Redo

   @cat_pro_error_handler

   self -> Draw

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATGRAPHICSCMDTOOL::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the CATGRAPHICSCMDTOOL object class
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
;     CAT_COLOR_OBJECT: A colortool object subclassed from COLORTOOL. Sets up the color table for
;                       the graphics command.
;
;     CAT_COORD_OBJECT: A coordinates object subclassed from CATCOORD. The coordinates object is
;                       automatically updated *after* the DRAW method is completed, which keeps
;                       the data coordinate system up to date for subsequent interaction with
;                       the graphics output.
;
;     _EXTRA:           Any keywords appropriate for the superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO CatGraphicsCmdTool::SetProperty, $
   CAT_COLOR_OBJECT=cat_color_object, $
   CAT_COORD_OBJECT=cat_coord_object, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Obj_Isa_Valid(cat_coord_object, 'CATCOORD') THEN self._coords = cat_coord_object
   IF Obj_Isa_Valid(cat_color_object, 'COLORTOOL') THEN self._colors = cat_color_object

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATTOOL::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CATGRAPHICSCMDTOOL::UNDO
;
; PURPOSE:
;
;       This method attempts to undo the previous command operation by restoring
;       the cached graphics window. If the window is no longer available, or there
;       is no cache, the method fails silently.
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
;       None.
;-
;*****************************************************************************************************
PRO CatGraphicsCmdTool::Undo

   @cat_pro_error_handler

   IF Ptr_Valid(self._theCache) EQ 0 THEN RETURN
   cache = *self._theCache
   IF WindowAvailable(cache.wid) THEN $
   BEGIN
      WSet, cache.wid
      TVImage, cache.contents, /NoInterpolate
   ENDIF

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATGRAPHICSCMDTOOL::CLEANUP
;
; PURPOSE:
;
;       This is the CATGRAPHICSCMDTOOL object class destructor method.
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
PRO CatGraphicsCmdTool::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self._p1
   Ptr_Free, self._p2
   Ptr_Free, self._p3
   Ptr_Free, self._keywords
   IF Obj_Valid(self._coords) THEN self._coords -> RemoveParent
   IF Obj_Valid(self._colors) THEN self._colors -> RemoveParent

   self -> CATTOOL::CLEANUP
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATGRAPHICSCMDTOOL::INIT
;
; PURPOSE:
;
;       This is the CATGRAPHICSCMDTOOL object class initialization method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       Command:      This is the name of the graphics "command" that is to be executed.
;                     It should be a string. For example, "Surface" or "Contour".
;
;       P1:           This is the first positional parameter of the graphics command.
;
;       P2:           This is the second positional parameter of the graphics command.
;
;       P3:           This is the third positional parameter of the graphics command.
;
; KEYWORDS:
;
;       AUTO_DESTROY: This object keeps track of the number of parents it has (reference counting). When
;                     this value changes from 1 to 0, the object will be destroyed unless Auto_Destroy
;                     has been set to zero. (Input)
;
;       CAT_COLOR_OBJECT: A colortool object subclassed from COLORTOOL. Sets up the color table for
;                     the graphics command.
;
;       CAT_COORD_OBJECT: A coordinates object subclassed from CATCOORD. The coordinates object is
;                     automatically updated *after* the DRAW method is completed, which keeps
;                     the data coordinate system up to date for subsequent interaction with
;                     the graphics output.
;
;       DECOMPOSED:   Set this keyword to 0 to turn color decomposition OFF before executing
;                     the graphics command. Default is color decomposition ON (this is also
;                     known as "true-color mode".)
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
;       TARGET:       Set this keyword to an object reference for the target object. The tool is
;                     applied to the data in this target.
;
;       UVALUE:       A user-value pointer. Can be used to store any IDL variable type.
;
;       _EXTRA:       Any keywords appropriate for the graphics command.
;
;  NOTES:
;
;      Note that it is possible to run into ambiguous keyword problems when loading a graphics command
;      with the INIT method. For example, if the graphics command you are loading contains any of the
;      keywords listed above, then the keyword will be misinterpreted, since the keyword will be applied
;      to object properties and not to the graphics command per se. (The POSITION keyword is an obvious
;      conflict with many graphics commands.) Rather than prevent you from trying, I've decided to do
;      what I can with what I am given. But if you find this unsuccessful, then you can just create the
;      object with "object" type keywords, and load the graphics command with the unambiguous LOADCOMMAND
;      method.
;
;      It is *always* safer to create the object, and *then* load the command with the LOADCOMMAND method.
;-
;*****************************************************************************************************
FUNCTION CatGraphicsCmdTool::INIT, $
   command, $                   ; The graphics "command" to execute.
   p1, $                        ; The first positional parameter of the command.
   p2, $                        ; The second positional parameter of the command.
   p3, $                        ; The third positional parameter of the command.

   ; Must include all superclass keywords here, since I can't afford to
   ; pass undefined keywords down to ATOM and I need to be able to pick up all
   ; command keywords without knowing what they are. If you add a SUPERCLASS
   ; keyword, be sure to also make the change here.

   Auto_Destroy=auto_destroy, $
   Cat_Color_Object=cat_color_object, $
   Cat_Coord_Object=cat_coord_object, $
   Decomposed=decomposed, $
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
   Target=target, $
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

  IF N_Elements(decomposed) EQ 0 THEN decomposed = 1
  self._decomposed = Keyword_Set(decomposed)

   IF Obj_Isa_Valid(cat_coord_object, 'PLOTCOORD') THEN self._coords = cat_coord_object
   IF Obj_Isa_Valid(cat_color_object, 'COLORTOOL') THEN self._colors = cat_color_object

   ; Call superclass INIT method
   ok = self -> CATTOOL::Init ( $
      Auto_Destroy=auto_destroy, $
      Event_Object=evObj, $
      Exclusive_Event_Obj=eev, $
      Indexed=indexed, $
      Memory_Management=mm, $
      Name=name, $
      NO_Copy=no_copy, $
      NoCache=nocache, $
      Parent=parent, $
      Position=position, $
      ReportLevel=reportlevel, $
      Target=target, $
      UValue=uvalue)

   IF (ok) THEN self -> Report, /Completed $
   ELSE self -> Report, /Failed

   RETURN, ok

END


;*****************************************************************************************************
;
; NAME:
;       CATGRAPHICSCMDTOOL OBJECT DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the CATGRAPHICSCMDTOOL object.
;
;*****************************************************************************************************
PRO CatGraphicsCmdTool__DEFINE, class

   class = { CATGRAPHICSCMDTOOL, $  ; The CATGRAPHICSCMDTOOL object class.
             INHERITS CATTOOL, $    ; Inherits the CATTOOL object class.
             _command: "", $        ; The IDL graphics command.
             _decomposed: 0L, $     ; The color decomposition state for the command.
             _coords: Obj_New(), $  ; A CATCOORD coordinates object.
             _colors: Obj_New(), $  ; A COLORTOOL color object.
             _p1: Ptr_New(), $      ; The first positional parameter of the command.
             _p2: Ptr_New(), $      ; The second positional parameter of the command.
             _p3: Ptr_New(), $      ; The third positional parameter of the command.
             _nparams: 0L, $        ; The number of parameters used in the call.
             _keywords: Ptr_New() $ ; A structure of keywords used with the command.
           }

END
