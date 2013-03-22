;*****************************************************************************************************
;+
; NAME:
;       CATATOM
;
; PURPOSE:
;
;       This is second most basic object in the CATALYST Object Library. It is a subclassed
;       CATCONTAINER object and implements an IDL object hierarchy. All objects in
;       the CATALYST Object Library are subclassed from the CATATOM object. Object
;       error handling, object reporting and documentation, object messaging, and
;       widget event handling are all set up in this object, allowing all objects
;       in the CATALYST Object Library to partake in this functionality.
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
;       CATCONTAINER, IDLITCOMPONENT
;       IDL_CONTAINER
;
; SYNTAX:
;
;       atomObject = Obj_New('CATATOM')
;
; CLASS_STRUCTURE:
;
;   class =  { CATATOM,                          $ ; The CATATOM object class name.
;              _controlPanel       : OBJ_NEW (), $ ; The control panel for the object.
;              _errorLevel         : 0B,         $ ; The error reporting level.
;              _event_method       : "",         $ ; The name of the event method associated with this object.
;              _event_objects      : OBJ_NEW (), $ ; The object(s) to whom events for this object are passed.
;              _excl_event_object  : OBJ_NEW (), $ ; An exclusive event object.
;              _messageRecipients  : OBJ_NEW (), $ ; A list of objects to be notified of method calls.
;              _reportLevel        : 0B,         $ ; The info reporting level.
;              _trash              : OBJ_NEW(),  $ ; A trash container for destroying other objects.
;              _uvalue             : PTR_NEW (), $ ; A user value placeholder for the object.
;              INHERITS CatContainer,            $ ; All objects in the Catalyst Library are containers
;              INHERITS IDLitComponent           $ ; Inherits the IDLitComponnet class for object properties.
;            }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 13 March 2003.
;       Made sure there are no duplicated registrations in RegisterForMessage. DWF. 15 May 2004.
;       Made a change in the way EVENT_METHOD is assigned. If not passed in, will try to get EVENT_METHOD
;         from the parent first, before assigning EventHandler as method. 9 August 2004. DWF.
;       Removed TOP_OBJECT reference. It was not being used and got in the way of saving/restoring
;         object. 22 January 2005. DWF.
;       Added TRASH container and ADDTOTRASH method. 27 July 2005. DWF.
;       In the COPY method, changed the default temp directory to GETENV('IDL_TMPDIR'). 3 Nov 2008 DWF.
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
;       CATATOM::ADDTOTRASH
;
; PURPOSE:
;
;        This procedure adds a specified object to the trash container. The purpose
;        is to have a place to store objects that need to be cleaned up when another
;        object is destroyed. Every object in the Catalyst system has a trash container.
;
; SYNTAX:
;
;       object -> AddToTrash, anotherObject
;
;
; ARGUMENTS:
;
;        anotherObject: The object, or array of objects, to be added to the trash container.
;
; KEYWORDS:
;
;        None.
;
;-
;*****************************************************************************************************
PRO CatAtom::AddToTrash, object

   @cat_pro_error_handler

   FOR j=0,N_Elements(object)-1 DO BEGIN
      IF Obj_Valid(object[j]) THEN self._trash -> Add, object[j]
   ENDFOR

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CatAtom::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the object. A control
;       panel is a graphical user interface for setting object
;       properties.
;
; SYNTAX:
;
;       theObject -> ControlPanel, baseObject
;
; ARGUMENTS:
;
;       baseObject:    The object reference of a base widget for this control to
;                      be added to. If not supplied, the control panel will be in a
;                      self contained window (i.e., a TOPLEVELBASE object).
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the CatControlPanel::INIT method.
;
;-
;*****************************************************************************************************
PRO CatAtom::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Object Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)
   self -> SetProperty, Description='Object Properties'

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.
   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, $
      Name='OBJECT PROPERTYSHEET', Description='Object Properties')
   aproperties -> SetProperty, Event_Object=self

   ; Is the base object from a browser? If so, then size the property sheet
   ; according to the size of the base widget.
   IF Obj_Valid(baseObject) THEN BEGIN
      IF StrUpCase(StrMid(baseObject->GetName(), 0, 7)) EQ 'BROWSER' THEN BEGIN
         baseObject -> GetProperty, Geometry=geo
         aproperties -> SetProperty, Scr_XSize=geo.xsize, Scr_YSize=geo.ysize
      ENDIF

   ENDIF

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATATOM::COPY
;
; PURPOSE:
;
;        This procedure takes an object and returns a copy of it. Note that
;        this is a "deep" copy in so far as all the data referred to by the
;        object is copied. Also note that this is an inherently SLOW process!!
;
;        This routine uses a temporary file during execution, so ensure that
;        some disk space is available. The temporary file is created in the
;        GetEnv('IDL_TMPDIR') location, unless specified.
;
; SYNTAX:
;
;       copy = obj -> Copy, object, TempDir=tempdir
;
;
; ARGUMENTS:
;
;        OBJECT: The object to be copied. If not supplied, self is copied.
;
; KEYWORDS:
;
;        TEMPDIR: The directory to be used for the temporary file.
;
;-
;*****************************************************************************************************
FUNCTION CatAtom::Copy, object, TempDir=tempDir

   ; Set up an error handler
   @cat_func_error_handler

   IF Obj_Valid(object) EQ 0 THEN object = self

   ; Switch to a temporary directory if available
   CD, Current=origDir
   IF (N_ELEMENTS (tempDir) EQ 0) THEN tempDir = GetEnv('IDL_TMPDIR')
   IF (tempDir NE '') THEN CD, tempDir

   ; Use save and restore to make a duplicate
   newobj = object
   
   ; Get the new object's parents and remove them prior to saving
   ; the object. If this is not done, the save file is VERY big!
   ; Be sure to set the NODESTROY flag, since removing parents will
   ; destroy the object, normally, after the last parent is removed.
   newobj -> GetProperty, PARENTS=parents
   FOR j=0,N_Elements(parents)-1 DO BEGIN
        newobj -> RemoveParent, parents[j], /NODESTROY
   ENDFOR
   
   ; Save and Restore makes a deep copy of the object.
   SAVE, newobj, FILENAME='obj_copy.tmp'
   RESTORE, 'obj_copy.tmp'
   FILE_DELETE, 'obj_copy.tmp'
   
   ; Add the parents back to the original object.
   FOR j=0,N_Elements(parents)-1 DO object -> AddParent, parents[j]

   ; Switch back to the original directory, if changed
   IF (tempDir NE '') THEN CD, origDir

   ; Report success and return
   self -> Report, /Completed
   RETURN, newobj

END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::DOCUMENT
;
; PURPOSE:
;
;       This method creates an HTML file (usually files, since superclass
;       objects are also documented) for the object. If running on Windows,
;       a Internet Explorer is spawned to read the documentation files.
;
; SYNTAX:
;
;       self -> Document
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DIRECTORY: The name of directory where the HTML files should be saved.
;                  By default, the method will try to use the default variable DOCDIR,
;                  which can be set with CatSetDefault. Otherwise, the user is asked
;                  to specify a currently-existing directory. If none is specified,
;                  the current directory is used.
;-
;*****************************************************************************************************
PRO CatAtom::Document, DIRECTORY=directory

   @cat_pro_error_handler


   IF N_Elements(directory) EQ 0 THEN directory = CatGetDefault ('DOCDIR')
   IF directory EQ "" THEN BEGIN

      directory = Dialog_Pickfile(/DIRECTORY, Title='Select Catalyst HTML Documentation Directory')
      IF directory EQ "" THEN BEGIN
         CD, Current=directory
         CatSetDefault, 'DOCDIR', directory
      ENDIF

   ENDIF

      ; Need a report about which directory you are writing into.

   self -> Report, 'Writing help files into ' + directory + ' ...'

      ; Get a list of all the superclasses that you need to document.

   names = CatCollectSuperclassNames(self)
   checkNames = IntArr(N_Elements(names))

         ; Find the proper source file and create the HTML files.
   FOR j=0, N_Elements(names)-1 DO BEGIN
      theFile = File_Which(!Path, /Include_Current, StrLowCase(names[j]) + "__define.pro")
      IF theFile NE "" THEN BEGIN
         Make_CatLib_Help, theFile, Filepath(Root_Dir=directory, StrLowCase(names[j]) + ".html"), Title=names[j] + ' Documentation'
         IF N_Elements(files) EQ 0 THEN files = [Filepath(Root_Dir=directory, StrLowCase(names[j]) + ".html")] ELSE $
            files = [files, Filepath(Root_Dir=directory, StrLowCase(names[j]) + ".html")]
         checkNames[j] = 1
      ENDIF ELSE checkNames[j] = 0
   ENDFOR
   validnames = Where(checkNames EQ 1, validCount)
   IF validCount GT 0 THEN BEGIN
      names = names[validnames]
      files = files[validnames]
   ENDIF

      ; Now modify the files to add superclass links.
      ; Find the SUPERCLASSES tags.

   FOR k=0, N_Elements(files) - 1 DO BEGIN

         ; Now modify the main file to have links to the other files.

      rows = File_Lines(files[k])
      text = StrArr(rows)
      OpenR, lun, files[k], /Get_LUN
      Readf, lun, text
      Free_Lun, lun

      index = Where(StrMatch(StrCompress(text, /Remove_All), 'SUPERCLASSES:') EQ 1, count)
      IF count NE 0 THEN BEGIN

         str = 'TEXT'
         start = index + 2
         WHILE str NE "" DO BEGIN
            str = StrTrim(text[start],2)

            ; Are there any commas in the string, which would indicate multiple inheritance?
            words = StrSplit(str, ',', /Extract)

            IF N_Elements(words) EQ 1 THEN BEGIN
               IF words NE ""  THEN BEGIN
                  IF words NE "IDL_CONTAINER" THEN BEGIN
                     text[start] = String(Replicate(32B, 7)) + '<A HREF="./' + $
                        StrLowCase(words) + '.html">' + words + '</A>'
                  ENDIF
               ENDIF
            ENDIF ELSE BEGIN

               addtext = String(Replicate(32B, 7))
               FOR j=0, N_Elements(words)-1 DO BEGIN
                 IF words[j] NE ""  THEN BEGIN
                     IF (StrCompress(words[j], /Remove_All) NE "IDLITCOMPONENT") THEN BEGIN
                        addtext = addtext +  '<A HREF="./' + StrLowCase(words[j]) + '.html">' + words[j] + '</A>'
                        IF j NE N_Elements(words)-1 THEN addtext = addtext + ', '
                     ENDIF ELSE BEGIN
                        addtext = addtext  + words[j]
                        IF j NE N_Elements(words)-1 THEN addtext = addtext + ', '
                     ENDELSE
                 ENDIF
               ENDFOR
               text[start] = addtext
           ENDELSE
           start = start + 1
         ENDWHILE

            ; Rewrite the file with the tags.

        OPENW, lun, files[k], /Get_Lun
        FOR j=0,N_Elements(text)-1 DO PrintF, lun, text[j]
        FREE_LUN, lun

     ENDIF

  ENDFOR

      ; Spawn a browser to look at the file.

   ;IF !VERSION.OS_FAMILY EQ 'Windows' THEN SPAWN, 'START "" "'+ files[0]+'"', /Hide
   ONLINE_HELP, Book=files[0]

END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::DRAW
;
; PURPOSE:
;
;       This method propogates a DRAW method invocation to the children of this
;       object. We expect the method to be called automatically by all subclasses
;       of the CatAtom class. On those occasions when you do NOT want the DRAW method
;       of children invoked, set the NO_CHILDREN keyword and the DRAW method returns
;       immediately. If an invalid child object is discovered, it is removed from
;       the parent container.
;
; SYNTAX:
;
;       self -> Draw
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       REQUESTER:        This optional keyword is set to the object that requests a DRAW of the
;                         object. This is helpful sometimes when messages are received by other
;                         objects.
;
;       NO_CHILDREN:      If this keyword is set, the DRAW method is not propogated to the
;                         object children, but the DRAW method returns immediately.
;-
;*****************************************************************************************************
PRO CatAtom::Draw, REQUESTER=requester, NO_CHILDREN=no_children

   ;@cat_pro_error_handler

   ; No children, please. Return immediately.
   IF (KEYWORD_SET (no_children) OR (self -> Count() EQ 0)) THEN $
   BEGIN
      self -> Report, /Completed
      RETURN
   ENDIF

   ; Call the DRAW methods on all child objects.
   self -> Report, 'Calling DRAW methods of children of ' + self._name + "...", 3

   ; Call the DRAW method on all the child objects. If a child is not a valid
   ; object, remove it from the parent.
   FOR i = 0, self -> Count() - 1 DO $
   BEGIN
      object = self -> Get(POSITION=i)
      IF Obj_Valid (object) THEN object -> Draw, REQUESTER=requester ELSE self -> Remove, object
   ENDFOR

   ; Report completion
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CATATOM::ERROR
;
; PURPOSE:
;
;       This method implements an error report into a log file or to standard output.
;       The method should normally only be called from within a CATCH error handler.
;       It relies on !ERROR_STATE having current information about the error.
;
; SYNTAX:
;
;       self -> Error
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
PRO CatAtom::Error

   compile_opt strictarr

   ; Get the error message. Make sure there is one. If not, return.
   IF !ERROR_STATE.MSG EQ "IDL_M_SUCCESS" THEN RETURN

   ; Capture the error
   HELP, /Last_Message, Output=msg

   ; Get the name of the calling routine.
   Help, Calls=callStack
   callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]

   ; If the error has been previously handled, don't handle it here
   positions = StrPos(msg, '[cat_handled]')
   foundit = Where(positions NE -1, count)
   IF count GT 0 THEN RETURN

   ; If we are writing to a log file, open it
   logFilename = CatGetDefault ('LogFilename')
   IF (logFilename NE '') THEN $
   BEGIN
      filename = FINDFILE (logFilename, Count=filecount)
      IF (filecount GT 0) THEN OPENU, logFile, logFilename, /GET_LUN, /APPEND $
      ELSE OPENW, logFile, logFilename, /GET_LUN
   ENDIF $
   ELSE logFile = -1 ; default is the command log


   ; If this object has no valid error level (e.g. it failed during init), set a default
   IF ((self._errorLevel) NE 0) THEN errorLevel = self._errorLevel $
   ELSE BEGIN
      errorLevel = CatGetDefault ('ErrorLevel', Success=ok)
      IF (NOT ok) THEN errorLevel = 2
   ENDELSE

   ; Print the output
   CASE errorLevel OF

      1 : BEGIN
             PrintF, logfile, ''
             PrintF, logfile, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
             PrintF, logfile, ''
             FOR j=0,N_Elements(msg)-1 DO PrintF, logfile, "     " + msg[j]
          END

      2 : BEGIN
             dialog_msg = TextLineFormat(msg[0])
             junk = DIALOG_MESSAGE (dialog_msg, /Error) ; Pop up dialog
             PrintF, logfile, ''
             PrintF, logfile, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
             PrintF, logfile, ''
             FOR j=0,N_Elements(msg)-1 DO PrintF, logfile, "     " + msg[j]
          END

      ELSE : PRINT, msg[0] ; do nothing

   ENDCASE

   ; Clear the error state variable.
   !ERROR_STATE.NAME = "IDL_M_SUCCESS"
   !ERROR_STATE.CODE = 0L
   !ERROR_STATE.MSG = ""
   
   ; Close the log file if open.
   IF logfile NE -1 THEN FREE_LUN, logfile
   
END



;*****************************************************************************************************
;+
; NAME:
;       CATATOM::EVENTHANDLER
;
; PURPOSE:
;
;       A dummy event handler method to catch widget events. If events come here, they
;       are dispatched, if possible, to the first parent of this object. If there are
;       no parents, an error message is delivered.
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
PRO CatAtom::EventHandler, event

      ; Set up an error handler
   @cat_pro_error_handler

      ; If there is an event object, send the event. Otherwise, the event is unhandled
      ; and should be sent to the first parent object, if there is one.
   IF self._event_objects -> Count () GT 0 THEN $
   BEGIN
      eventObjs = self._event_objects -> Get (/All, Count=noEventObjs)
      IF noEventObjs EQ 0 THEN $ ; Check the exclusive event object.
      BEGIN
         eventObjs = self._excl_event_object
         IF Obj_Valid(eventObjs) THEN noEventObjs = 1
      ENDIF
      FOR e = 0, noEventObjs - 1 DO $
         IF (eventObjs [e] NE self) THEN eventObjs[e] -> EventHandler, event
   ENDIF $
   ELSE BEGIN

      ; No event handlers for this event. Pass the event on to the first parent.
      IF Obj_Valid(self._parents) EQ 0 THEN $
         Message, 'Unhandled event structure for object ' + Obj_Class(event.id) ELSE $
         BEGIN
            self -> CatContainer::GetProperty, First_Parent=first_parent
            IF Obj_Valid(first_parent) THEN first_parent -> EventHandler, event ELSE $
               Message, 'Unhandled event structure for object ' + Obj_Class(event.id)
         ENDELSE
   ENDELSE

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       CATATOM::GETPROPERTY
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
;       AUTO_DESTROY:  A flag indicating whether this objects lifecycle
;                      is controlled my the memory management system.
;
;       CONTROLPANEL:  The object reference of the control panel. If none exists, a null object
;                      is returned.
;
;       ERRORLEVEL:    The error reporting level for this object.
;
;       EVENT_METHOD:  The name of the current event method for this object.
;
;       EVENT_OBJECTS: This keyword returns the widget object or objects that will handle events for this
;                      particular object.
;
;       EXCLUSIVE_EVENT_OBJECT: The exclusive event object, if available.
;
;       NAME:          The name given to this object.
;
;       PARENTS:       An array of the parents of this object.
;
;       REPORTLEVEL:   The informational reporting level for this object.
;
;       TOP:           A reference to the object at the top of the object hierarchy to which this
;                      object belongs.
;
;       NO_COPY:       Set this keyword to transfer the UVALUE without copying.
;
;       UVALUE:        A user value pointer. Can be used to store any IDL variable type.
;-
;*****************************************************************************************************
PRO CatAtom::GetProperty,  $
   AUTO_DESTROY=autoDestroy, $
   CONTROLPANEL=controlPanel, $
   ERRORLEVEL=errorLevel, $
   EVENT_METHOD=event_method, $
   EVENT_OBJECTS=event_objects, $
   EXCLUSIVE_EVENT_OBJECT=exclusive_event_object, $
   NAME=name, $
   NO_COPY=no_copy, $
   PARENTS=parents, $
   REPORTLEVEL=reportLevel, $
   UVALUE=uvalue, $
   _REF_EXTRA=extraKeywords, $

   ; IDLitComponent keywords

   DESCRIPTION=description, $
   ICON=icon, $
   HELP=help, $
   IDENTIFIER=identifier


   @cat_pro_error_handler

   event_method  = self._event_method
   errorLevel    = self._errorLevel
   reportLevel   = self._reportLevel
   controlPanel  = self._controlPanel

   ; If the event objects are requested, check if we've got an exclusive event object first.
   ; This will ensure that the CatEventDispatcher (which always uses this GetProperty
   ; method to obtain the event object) will always dispatch events to the correct
   ; event object.
   IF (OBJ_VALID (self._excl_event_object)) THEN event_objects = self._excl_event_object $
   ELSE event_objects = self._event_objects -> Get (/All)

   ; Return the exclusive event object is requested.
   IF Arg_Present(exclusive_event_object) THEN exclusive_event_object = self._excl_event_object

   ;Need to get the UVALUE?
   IF (PTR_VALID (self._uvalue)) THEN $
   BEGIN
      IF Arg_Present(uvalue) THEN $
      BEGIN
         IF Keyword_Set(no_copy) THEN uvalue = Temporary(*self._uvalue) ELSE uvalue = *self._uvalue
      ENDIF
   ENDIF

   ; Set up an event handler to catch unhandled keywords specifically.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      IF !Error_State.Name EQ 'IDL_M_KEYWORD_BAD' THEN $
      BEGIN
         name = Obj_Class(self)
         pos = Strpos(!Error_State.MSG, 'not allowed in call')
         keyword = StrMid(!Error_State.MSG, 7, pos-7)
         errmsg = 'Unhandled keyword ' + keyword + ' detected in ' + StrUpCase(name) + '.'
         HELP, /Last_Message, Output=msg

         HELP, Calls=callstack
         callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
         CASE errorLevel OF

            1 : BEGIN
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(name) + ':'
                Print, ''
                Print, "     " + errmsg
                FOR j=1,N_Elements(msg)-1 DO Print, "     " + msg[j]
             END

            2 : BEGIN
                junk = Dialog_Message(errmsg)
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(name) + ':'
                Print, ''
                Print, "     " + errmsg
                FOR j=1,N_Elements(msg)-1 DO Print, "     " + msg[j]

              END

            ELSE : BEGIN
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(name) + ':'
                Print, ''
                Print, "     " + errmsg
                FOR j=1,N_Elements(msg)-1 DO Print, "     " + msg[j]
              END

          ENDCASE

      ENDIF ELSE $
      BEGIN

         ; Cancel the error handler and set up the error handling to "throw" error
         HELP, Calls=callstack
         callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
         HELP, /Last_Message, Output=msg
         ON_ERROR, 2

         ; If the error has been previously handled, don't handle it here
         positions = StrPos(msg, '[cat_handled]')
         foundit = Where(positions NE -1, count)
         IF count GT 0 THEN $
         BEGIN
         IF Routine_Names(/Level) GT 2 THEN $
            MESSAGE, msg[0] ELSE RETURN
            ;MESSAGE, msg[0]
         END
         ; Report the error
         IF (Obj_IsA_Valid (self, 'CatAtom')) THEN self -> Error $
         ELSE CASE errorLevel OF

               1 : BEGIN
                   Print, ''
                   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                   Print, ''
                   FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
                END

               2 : BEGIN
                   junk = Dialog_Message(msg[0])
                   Print, ''
                   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                   Print, ''
                   FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
                 END

               ELSE : BEGIN
                   Print, ''
                   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                   Print, ''
                   FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
                 END

             ENDCASE

         ; Throw the error, signalling that it's already been handled
         IF Routine_Names(/Level) GT 2 THEN $
            MESSAGE, msg[0] + ' [cat_handled]' ELSE RETURN
      ENDELSE

      self -> Report, /Completed
      RETURN
   ENDIF

   ; Call the superclass GetProperty methods
   self -> CatContainer::GetProperty, $
      Name=name, $
      Parents=parents, $
      Auto_Destroy=autoDestroy, $
      _STRICT_EXTRA=extraKeywords
   self -> IDLitComponent::GetProperty, $
      DESCRIPTION=description, $
      ICON=icon, $
      HELP=help, $
      IDENTIFIER=identifier

   ; Report successful completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::GETSTATE
;
; PURPOSE:
;
;       This method is used to get the object's properties as a structure. It
;       is primarily intended to be used with the corresponding SETSTATE method.
;
; SYNTAX:
;
;       state = self -> GetState ()
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       SCALARS:   Set this keyword to include scalar properties. If no type
;                  keywords are specified, this will be set. (Input)
;
;       ARRAYS:    Set this keyword to include array properties. If no type
;                  keywords are specified, this will be set. (Input)
;
;       POINTERS:  Set this keyword to include pointer properties. If no type
;                  keywords are specified, this will NOT be set. (Input)
;
;       OBJECTS:   Set this keyword to include object reference properties. If no type
;                  keywords are specified, this will NOT be set. (Input)
;
;       ALL:       Set this keyword to include the all the above types. (Input)
;
;       INHERITED: Set this keyword to include properties inherited from the
;                  superclass. (Input)
;
;-
;*****************************************************************************************************
;FUNCTION CatAtom::GetState, $
;   SCALARS   = scalars,  $
;   ARRAYS    = arrays,   $
;   POINTERS  = pointers, $
;   OBJECTS   = objects,  $
;   ALL       = all,      $
;   INHERITED = inherited
;
;   ; Set up an error handler
;   @cat_func_error_handler
;
;   ; Process the input keywords
;   scalars   = KEYWORD_SET (scalars)  > KEYWORD_SET (all)
;   arrays    = KEYWORD_SET (arrays)   > KEYWORD_SET (all)
;   pointers  = KEYWORD_SET (pointers) > KEYWORD_SET (all)
;   objects   = KEYWORD_SET (objects)  > KEYWORD_SET (all)
;   inherited = KEYWORD_SET (inherited)
;
;   IF ((scalars + arrays + pointers + objects) EQ 0) THEN $
;   BEGIN
;      scalars = 1
;      arrays  = 1
;      all     = 0
;   ENDIF $
;   ELSE all = 1 ; in case they've been set individually
;
;   ; Create a structure that duplicates the objects 'self' structure and check
;   ok = EXECUTE ("struct = {" + OBJ_CLASS (self) + "}")
;   IF (NOT ok) THEN MESSAGE, 'Unable to duplicate object structure.'
;   IF (N_TAGS (struct) EQ 0) THEN MESSAGE, 'Unable to get state of empty object'
;
;   ; If all keywords are set, return this structure
;   IF (all AND inherited) THEN $
;   BEGIN
;      self -> Report, /Completed
;      RETURN, struct
;   ENDIF
;
;   ; Get the tag names and count them
;   tags   = TAG_NAMES (struct)
;   noTags = N_TAGS    (struct)
;
;   ; If inheritance is not used, get the inherited tags and remove from the tag list
;   IF (NOT inherited) THEN $
;   BEGIN
;      supers = OBJ_CLASS (self, /SUPERCLASS)
;      FOR s = 0, N_ELEMENTS (supers) - 1 DO $
;      BEGIN
;         ok = EXECUTE ("temp = {" + supers [s] + "}")
;         tempTags = TAG_NAMES (temp)
;         FOR t = 0, N_TAGS (temp) - 1 DO $
;         BEGIN
;            locn = WHERE (tags EQ tempTags [t], noMatches)
;            IF (noMatches GT 0) THEN tags [locn] = ''
;         ENDFOR
;      ENDFOR
;   ENDIF
;
;   ; Remove the NULL tag names from the list
;   validTags = WHERE (tags NE '', noValid)
;   IF (noValid EQ 0) THEN MESSAGE, 'No valid fields in object structure to copy.'
;   tags   = tags [validTags]
;   noTags = noValid
;
;   ; Go through each tag checking to see if its type is included
;   FOR t = 0, noTags - 1 DO $
;   BEGIN
;      ok = EXECUTE ("tag = self." + tags [t])
;      tagType = SIZE (tag, /TNAME)
;      CASE tagType OF
;         'OBJREF'  : IF (NOT objects ) THEN tags [t] = ''
;         'POINTER' : IF (NOT pointers) THEN tags [t] = ''
;          ELSE     : BEGIN
;                        IF ((N_ELEMENTS (tag) EQ 1) AND (NOT scalars)) THEN tags [t] = ''
;                        IF ((N_ELEMENTS (tag) GT 1) AND (NOT arrays )) THEN tags [t] = ''
;                     END
;      ENDCASE
;   ENDFOR
;
;   ; Remove the NULL tag names from the list
;   validTags = WHERE (tags NE '', noValid)
;   IF (noValid EQ 0) THEN MESSAGE, 'No valid fields in object structure to copy.'
;   tags   = tags [validTags]
;   noTags = noValid
;
;   ; Construct the output structure
;   cmd = 'struct = {Object:"' + OBJ_CLASS (self) + '"'
;   FOR t = 0, noTags - 1 DO cmd = cmd + ',' + tags [t] + ':self.' + tags [t]
;   cmd = cmd + '}'
;   ok = EXECUTE (cmd)
;   IF (NOT ok) THEN MESSAGE, 'Unable to construct output structure copy.'
;
;   ; Report completion and return structure
;   self -> Report, /Completed
;   RETURN, struct
;
;END



;*****************************************************************************************************
;+
; NAME:
;       CATATOM::HELP
;
; PURPOSE:
;
;       This method allows you to look at the current value of the object class structure.
;       It is similar to doing a Help, object, /Structure call at the IDL command line.
;       It depends upon having defined an output parameter for the CLASSNAME__DEFINE procedure
;       of any object that uses the method. All Catalyst objects use this convention:
;
;       PRO CLASSNAME__DEFINE, class
;          class = {....}
;       END
;
; SYNTAX:
;
;       anObject = self -> Help
;
; ARGUMENTS:
;
;       theField:     The name of a particular field you would like HELP on.
;
; KEYWORDS:
;
;       FIELDS:       Set this keyword to see a list of the structure field names.
;
;-
;*****************************************************************************************************
PRO CatAtom::Help, theField, Fields=fields

   @cat_pro_error_handler


   Call_Procedure, Obj_Class(self)+'__DEFINE', structure
   theTags = Tag_Names(structure)

   IF Keyword_Set(fields) THEN BEGIN
      FOR j=0,N_Elements(theTags)-1 DO Print, theTags[j]
      RETURN
   ENDIF

   IF N_Elements(theField) EQ 0 THEN BEGIN
      FOR j=0, N_Elements(theTags)-1 DO BEGIN
         Print, theTags[j]
         Help, self.(j)
         Print, ""
      ENDFOR
   ENDIF ELSE BEGIN
      index = Where(theTags EQ StrUpCase(theField), count)
      IF count NE 0 THEN BEGIN
         Print, theTags[index]
         Help, self.(index)
      ENDIF
   ENDELSE

END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::MESSAGEHANDLER
;
; PURPOSE:
;
;       This method receives notification of a SENDMESSAGE call from another object's
;       method. This method should be overridden in any object that expects to receive
;       messages from objects. Be sure your MessageHandler methods define the arguments
;       and keywords shown here. If the message gets here, we issue an error message.
;
; SYNTAX:
;
;       thisObject -> MessageHandler, title, SENDER=sender, DATA=data
;
; ARGUMENTS:
;
;       TITLE:   The title of the message.
;
; KEYWORDS:
;
;       DATA:    A keyword that contains any information the sender wishes to pass
;                with the message. It can be empty.
;
;       SENDER:  The object that generated the message
;
;-
;*****************************************************************************************************
PRO CatAtom::MessageHandler, title, SENDER=sender, DATA=data

   @cat_pro_error_handler

   IF N_Elements(title) EQ 0 THEN Message, 'Ill-formed message received. No title.'

   ; If a message gets here, there is a problem.
   CASE title OF

      ELSE: BEGIN
         sender -> GetProperty, Name=senderName
         Message, 'Unhandled message: ' + title + ' coming from ' + senderName + '.'
         END

   ENDCASE

   ; Report success
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::PRINT
;
; PURPOSE:
;
;       This method prints a simple display showing the hierarchy rooted
;       at this object.
;
; SYNTAX:
;
;       self -> Print
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       Level: The depth in the hierarchy of this object. Note that this
;              is normally set internally as the program recurses through
;              the hierarchy.
;-
;*****************************************************************************************************
PRO CatAtom::Print, LEVEL=level

   @cat_pro_error_handler

   ; Print the line for this object
   IF (N_ELEMENTS (level) EQ 0) THEN level = 1
   txt = STRING (BYTARR (level) + Replicate(32B, 4))
   PRINT, txt + '- ' + OBJ_CLASS (self) + '(' + self._name + ')'

   ; Recurse through the rest of the hierarchy
   children = self -> Get (/ALL, COUNT=noChildren)
   FOR child = 0, noChildren - 1 DO $
   BEGIN
      IF (OBJ_VALID (children [child])) THEN children [child] -> Print, Level=level+1 $
      ELSE PRINT, txt, ' - <Null Object>'
   ENDFOR

   ; Report completion and return
   self -> report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::REGISTERFORMESSAGE
;
; PURPOSE:
;
;       This method enables an object to request messages of a specific title. These
;       messages, when generated, will be passed to the MessageHandler method of the
;       recipient object. This method can also be used to unregister for a message
;       by setting the UNREGISTER keyword.
;
; SYNTAX:
;
;       thisObject -> Register_For_Message, recipient, title
;
; ARGUMENTS:
;
;       RECIPIENT:   The object that requires notification.
;
;       TITLE:       The title of messages to pass.
;
; KEYWORDS:
;
;       UNREGISTER: Set this keyword to unregister an object that has previously
;                   registered itself for messages. The RECIPIENT and TITLE must
;                   be the same as when the RECIPIENT registered the message.
;
;-
;*****************************************************************************************************
PRO CatAtom::RegisterForMessage, recipient, title, UNREGISTER=unregister

   ; Set up an error handler
   @cat_pro_error_handler

   ; Notification targets must be subclassed from CATATOM
   IF (NOT Obj_IsA_Valid (recipient, 'CatAtom')) THEN $
      MESSAGE, 'Only CATATOM objects can be passed messages.'

   IF Keyword_Set(unregister) THEN $
   BEGIN
      IF Obj_Valid(self._messageRecipients) THEN $
      BEGIN
         success = 1
         indices = self._messageRecipients -> FindByName(title, Count=count)
         IF count GT 0 THEN indices = Reverse(indices)
         FOR j=0, count-1 DO $
         BEGIN
            cat = self._messageRecipients -> Get(Position=indices[j])
            item = cat -> GetValue()
            IF Obj_Valid(item) THEN $
            BEGIN
               IF item EQ recipient THEN self._messageRecipients -> Remove, Position=indices[j]
            ENDIF
         ENDFOR
      ENDIF
      RETURN ; If unregistering, RETURN now.
   ENDIF

   ; Create the notification list if required (it is destroyed by the cleanup method)
   IF (NOT OBJ_VALID (self._messageRecipients)) THEN self._messageRecipients = OBJ_NEW ('CatList')
   IF (NOT OBJ_VALID (self._messageRecipients)) THEN MESSAGE, 'Unable to access/create notification list.'

   ; Add the target object to the required notification list(s). Make sure this is not
   ; a duplicate message recipient.
   indices = self._messageRecipients -> FindByName(title, Count=count)
   FOR j=0,count-1 DO BEGIN
      cat = self._messageRecipients -> Get(Position=indices[j])
      item = cat -> GetValue()
      IF item EQ recipient THEN BEGIN
         self -> Report, /Completed
         RETURN
      ENDIF
   ENDFOR
   self._messageRecipients -> Add, title, Value=recipient, Success=ok, Auto_Destroy=0
   IF (NOT ok) THEN MESSAGE, 'Failed to add ' + OBJ_CLASS (recipient) + 'to recipient list.'

   ; Report completion
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATATOM::REPORT
;
; PURPOSE:
;
;       This method is used to send program reports either to a log file or to standard
;       output. If the self._reportLevel variable is less than or equal to that requested,
;       the  message is output. This allows great flexibility
;       in the kinds of reports that get reported, and in particular it allows the user to turn
;       reporting to a higher level for a specific object than for other objects. This is
;       extremely useful in debugging object programs.
;
;       Reports are sent to a log file if the LogFileName system default is not a NULL string.
;
;       An "alert" message throws up a DIALOG_MESSAGE widget that the user must repond to.
;
; SYNTAX:
;
;       self -> Report, theMessage, reportLevel
;
; ARGUMENTS:
;
;       theMessage:  The message text.
;
;       reportLevel: The type of report. 0: alert, 1: informational, 2: verbose report.
;                    All reports depend upon a comparison between the ReportLevel,
;                    the self._reportLevel and the input reportLevel values. The largest of these
;                    three values is used to set the report level.
;
; KEYWORDS:
;
;       COMPLETED:   A "completed" standard verbose message.
;
;       FAILED:      A "failed" standard verbose message.
;
;       STARTED:     A "started" standard verbose message.
;
;-
;*****************************************************************************************************
PRO CatAtom::Report, msg, reportLevel, Completed=completed, Failed=failed, Started=started

   ; Set up an error handler - do not use cat_error_handler because it calls this method
   ON_ERROR, 2

   ; Set the default reportLevel if none has been supplied
   IF (N_ELEMENTS (reportLevel) EQ 0) THEN reportLevel = 3

   ; Is this a standard message?
   IF KEYWORD_SET (started) THEN msg = 'Started.' $
   ELSE IF KEYWORD_SET (completed) THEN msg = 'Completed.' $
   ELSE IF KEYWORD_SET (failed) THEN msg = 'Failed.'

   ; Nothing to do if there is no message.
   IF (N_ELEMENTS (msg) EQ 0) THEN RETURN

   ; Check whether this message should be suppressed
   IF reportLevel GT self._reportLevel THEN RETURN

   ; Get the calling routine name and call stack depth
   Help, /Traceback, Output=callStack
   routine = (StrSplit (StrCompress (callStack[1])," ", /Extract))[1]
   callStackDepth = N_ELEMENTS (callStack)

   ; Stop a runaway process
   IF ((reportLevel EQ 3) AND (callStackDepth GT 100)) THEN STOP

   ; Set up the tabulation of the output by indenting proportional to call stack depth .... nice:-)
   indent  = STRING (BYTARR (callStackDepth) + Replicate(32B, 2))

   ; Set up the report string
   CASE reportLevel OF

      0    : msg = StrUpCase (self._name) + ' ALERT from ' + routine + ' --->   ' + msg
      1    : msg = StrUpCase (self._name) + ' information from ' + routine + ' --->   ' + msg
      2    : msg = StrUpCase (self._name) + ' debug information from ' + routine + ' --->   ' + msg
      ELSE : msg = StrUpCase (self._name) + ' message from ' + routine + ' --->   ' + msg

   ENDCASE

   ; If the report is to file, open the file
   logFilename = CatGetDefault ('LogFilename', Success=ok)
   IF (logFilename NE '') THEN $
   BEGIN
      filename = FINDFILE (logFilename, Count=filecount)
      IF (filecount GT 0) THEN OPENU, logFile, logFilename, /GET_LUN, /APPEND $
      ELSE OPENW, logFile, logFilename, /GET_LUN
   ENDIF $
   ELSE logFile = -1 ; default is the command log

   PRINTF, logFile, indent, msg

   IF (logfile NE -1) THEN FREE_LUN, logfile

END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::SELECT
;
; PURPOSE:
;
;       This method is a dummy method for a "selection" process. An object who wishes
;       to be "selected" must override this method. A "selected" object will return its
;       own object reference.
;
; SYNTAX:
;
;       thisObject -> Select, x, y
;
; ARGUMENTS:
;
;       X:         A dummy parameter. Perhaps the X location in a draw widget.
;
;       Y:         A dummy parameter. Perhaps the Y location in a draw widget.
;
; KEYWORDS:
;
;       SUCCESS:   Set to 1 if a selection is made. To 0 otherwise.
;
;       _EXTRA:    Any extra keywords.
;
;-
;*****************************************************************************************************
FUNCTION CatAtom::Select, x, y, Success=success, _Extra=extra

   @cat_func_error_handler

   success = 0
   retVal = Obj_New()

   ; Are there any children to search?
   children = self -> Get(/All, Count=count)
   FOR j=0,count-1 DO BEGIN
      retVal = children[j] -> Select(x, y, Success=success)
      IF success THEN BEGIN
         IF N_Elements(selectableObjects) EQ 0 THEN selectableObjects = [retVal] ELSE $
            selectableObjects = [selectableObjects, retVal]
      ENDIF
   ENDFOR

   ; Did you find any objects?
   IF N_Elements(selectableObjects) NE 0 THEN BEGIN
      retVal = selectableObjects
      success = 1
   ENDIF

   ; Report completion.
   self -> Report, /Completed

   ; Return selected object.
   RETURN, retVal

END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::SENDMESSAGE
;
; PURPOSE:
;
;       This method dispatches messages to objects that have registered to receive
;       them (using the RegisterForMessage method).
;
; SYNTAX:
;
;       thisObject -> SendMessage, title, DATA=data
;
; ARGUMENTS:
;
;       TITLE:   The title of the message. The message will be sent to objects that have
;                registered to receive messages with this title.
;
; KEYWORDS:
;
;       DATA:    A keyword for passing relevant data along with the message.
;
;-
;*****************************************************************************************************
PRO CatAtom::SendMessage, title, DATA=data

   @cat_pro_error_handler

   ; If the notification list is empty, return
   IF (NOT OBJ_VALID (self._messageRecipients)) THEN RETURN

   ; If a title hasn't been passed in, return
   IF (SIZE (title, /TName) NE 'STRING') THEN RETURN

   ; Get all the objects interested in the notification.
   ; The variable objs contains CATLISTVALUE objects. The "value"
   ; of such an object is the the object to be notified.
   objs = self._messageRecipients -> Get (title, /All, Count=noObjs)

   FOR i = 0, noObjs - 1 DO $
   BEGIN
      messageReceipient = objs [i] -> GetValue ()

      ; If the message receipient is invalid, remove it.
      IF Obj_Valid(messageReceipient) EQ 0 THEN BEGIN
         self._messageRecipients -> Remove, objs [i]
      ENDIF ELSE messageReceipient -> MessageHandler, title, SENDER=self, DATA=data
   ENDFOR

   ; Report success
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::SETPROPERTY
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
;       AUTO_DESTROY:   Set this keyword to use memory management to destroy the object.  (Input)
;
;       CONTROLPANEL:   A control panel object. Must be subclassed from CATCONTROLPANEL. (Input)
;
;       DRAW:           Set this keyword to call the draw method after setting properties (Input).
;
;       ERRORLEVEL:     Set this keyword to modify the behaviour when an error occurs (Input).
;
;       EVENT_METHOD:   Set this keyword to the name of the object method that will
;                       handle the events for this object. It is roughly equivalent
;                       to setting EVENT_PRO in a widget program.
;
;       EVENT_OBJECTS:  This is an object reference to an object that will receive the
;                       event from a specific widget event. It is the equivalent (in
;                       object terms) of specifying an EVENT_PRO or EVENT_FUNC keyword.
;                       In other words, it is a way of deflected an event from the EVENTHANDLER
;                       method of the object widget that generated the event to the EVENTHANDLER
;                       method of the object specified with this keyword. It may be an array of
;                       objects. Any objects currently defined as event objects will be replaced.
;
;       EXCLUSIVE_EVENT_OBJECT: If this keyword is set to a vaid object, events are passed directly
;                       and only to this object, ignoring the other event objects. To disable this
;                       set this keyword to be a NULL object or zero. This keyword is designed for
;                       situations where an object wishes to hog the events for a limited period.
;
;       GROUP_LEADER:   The group leader widget ID. Must be used for floating and
;                       modal base widgets.
;
;       NAME:           The "name" of the object. Used to keep track of the object in the code. (Input)
;
;       NO_COPY:        Set this keyword to transfer the UVALUE without copying.
;
;       REPORTLEVEL:    Set this keyword to modify the behaviour when an message is invoked (Input).
;
;       UVALUE:         A user-value pointer. Can be used to store any IDL variable type.
;-
;*****************************************************************************************************
PRO CatAtom::SetProperty, Name=name, $
       Auto_Destroy=autoDestroy, $
       Control_Panel=control_panel, $
       Draw=draw, $
       Event_Method=event_method, $
       ErrorLevel=errorLevel, $
       Event_Objects=event_objects, $
       Exclusive_Event_Object=exclusive_event_object, $
       Remove_Event_Object=remove_event_object, $
       ReportLevel=reportLevel, $
       ControlPanel=controlPanel, $
       Group_Leader=group_leader, $
       No_Copy=no_copy, $
       Notifier=notifier, $
       UValue=uvalue, $
       _EXTRA=extraKeywords, $

       ; Keywords for IDLitComponent class.

       DESCRIPTION=description, $
       ICON=icon, $
       HELP=help, $
       IDENTIFIER=identifier

   @cat_pro_error_handler

   IF N_Elements(event_method) NE 0 THEN self._event_method = event_method
   IF (N_ELEMENTS (errorLevel)   GT 0) THEN self._errorlevel  = errorLevel
   IF (N_ELEMENTS (reportLevel)  GT 0) THEN self._reportlevel = reportLevel

   IF (N_ELEMENTS (controlPanel) GT 0) THEN $
   BEGIN
      IF (Obj_IsA_Valid (controlPanel, "CatControlPanel")) THEN self._controlPanel=controlPanel $
      ELSE Message, 'Control Panel object reference is not a valid CatControlPanel.'
   ENDIF

   ; If the event object is set check it and, if valid, store it
   IF (N_ELEMENTS (event_objects) GT 0) THEN $
   BEGIN
      IF (Obj_IsA_Valid (self, 'CatAtom')) THEN BEGIN
         self._event_objects -> Remove, /All
         self._event_objects -> Add, event_objects
      ENDIF ELSE Message, 'Can only add event objects to CATATOM objects.
   ENDIF

   ; If an event object is to be removed, do it here
   IF (OBJ_VALID (remove_event_object)) THEN self._event_objects -> Remove, remove_event_object

   ; Is there an exclusive event object? If there is an exclusive event object,
   ; it will be the only event object passed to the event dispatcher. This
   ; slight of hand is performed in the CatAtom::GetProperty method.
   IF (N_ELEMENTS (exclusive_event_object) EQ 1) THEN $
   BEGIN
      IF (OBJ_VALID (exclusive_event_object)) THEN self._excl_event_object = exclusive_event_object $
      ELSE self._excl_event_object = OBJ_NEW () ; use a null object to bypass
   ENDIF

   ; Set UVALUE.
   IF (N_Elements(uvalue) NE 0) THEN BEGIN
      no_copy = Keyword_Set(no_copy)
      IF Ptr_Valid(self._uvalue) THEN BEGIN
         IF no_copy THEN BEGIN
            Ptr_Free, self._uvalue
            self._uvalue = Ptr_New(uvalue, NO_COPY=no_copy)
         ENDIF ELSE *self._uvalue = uvalue
      ENDIF ELSE BEGIN
         self._uvalue = Ptr_New(uvalue, NO_COPY=no_copy)
      ENDELSE
   ENDIF

   ; Set up an event handler to catch unhandled keywords specifically.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      IF !Error_State.Name EQ 'IDL_M_KEYWORD_BAD' THEN $
      BEGIN
         name = Obj_Class(self)
         pos = Strpos(!Error_State.MSG, 'not allowed in call')
         keyword = StrMid(!Error_State.MSG, 7, pos-7)
         errmsg = 'Unhandled keyword ' + keyword + ' detected in ' + StrUpCase(name) + '.'
         HELP, /Last_Message, Output=msg

         HELP, Calls=callstack
         callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
         CASE errorLevel OF

            1 : BEGIN
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(name) + ':'
                Print, ''
                Print, "     " + errmsg
                FOR j=1,N_Elements(msg)-1 DO Print, "     " + msg[j]
             END

            2 : BEGIN
                junk = Dialog_Message(errmsg)
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(name) + ':'
                Print, ''
                Print, "     " + errmsg
                FOR j=1,N_Elements(msg)-1 DO Print, "     " + msg[j]

              END

            ELSE : BEGIN
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(name) + ':'
                Print, ''
                Print, "     " + errmsg
                FOR j=1,N_Elements(msg)-1 DO Print, "     " + msg[j]
              END

          ENDCASE

      ENDIF ELSE $
      BEGIN

         ; Cancel the error handler and set up the error handling to "throw" error
         HELP, Calls=callstack
         callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
         HELP, /Last_Message, Output=msg
         ON_ERROR, 2

         ; If the error has been previously handled, don't handle it here
         positions = StrPos(msg, '[cat_handled]')
         foundit = Where(positions NE -1, count)
         IF count GT 0 THEN $
         BEGIN
         IF Routine_Names(/Level) GT 2 THEN $
            MESSAGE, msg[0] ELSE RETURN
            ;MESSAGE, msg[0]
         END
         ; Report the error
         IF (Obj_IsA_Valid (self, 'CatAtom')) THEN self -> Error $
         ELSE CASE errorLevel OF

               1 : BEGIN
                   Print, ''
                   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                   Print, ''
                   FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
                END

               2 : BEGIN
                   junk = Dialog_Message(msg[0])
                   Print, ''
                   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                   Print, ''
                   FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
                 END

               ELSE : BEGIN
                   Print, ''
                   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                   Print, ''
                   FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
                 END

             ENDCASE

         ; Throw the error, signalling that it's already been handled
         IF Routine_Names(/Level) GT 2 THEN $
            MESSAGE, msg[0] + ' [cat_handled]' ELSE RETURN
      ENDELSE

      self -> Report, /Completed
      RETURN
   ENDIF

   ; Call the superclass SETPROPERTY methods
   self -> CatContainer::SetProperty, Name=name, Auto_Destroy=autoDestroy, _STRICT_EXTRA=extraKeywords
   self -> IDLitComponent::SetProperty, $
           DESCRIPTION=description, $
           ICON=icon, $
           HELP=help, $
           IDENTIFIER=identifier

   ; If the DRAW keyword is set, call the draw method
   IF (KEYWORD_SET (draw)) THEN self -> Draw

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::SETSTATE
;
; PURPOSE:
;
;       This method is used to set the object's properties using a structure. It
;       is primarily intended to be used with the corresponding GETSTATE method.
;
; SYNTAX:
;
;       self -> SetState (state)
;
; ARGUMENTS:
;
;       STATE:        A structure whose tag names match some/all of the object properties.
;                     Normally this structure would be from a GETSTATE method invocation
;                     for this object or another of the same class.
;
; KEYWORDS:
;
;       IGNORE_CLASS: The state structure returned by the GETSTATE method has a name
;                     identifying it as the state structure for a partucular class. If
;                     an attempt is made to apply it to another class, the call fails.
;                     To override this behaviour, set the UNMATCHED keyword. (Input)
;
;-
;*****************************************************************************************************
;PRO CatAtom::SetState, state, IGNORE_CLASS=ignore_class
;
;   ; Set up an error handler
;   @cat_pro_error_handler
;
;   ; Get the input tag names and count them
;   tags   = TAG_NAMES (state)
;   noTags = N_TAGS    (state)
;
;   ; Check to see if the first tag is OBJECT
;   IF (tags [0] NE 'OBJECT') THEN MESSAGE, 'SetState must be passed a valid state structure'
;
;   ; Check that the originator object is of the same class
;   IF (NOT KEYWORD_SET (ignore_class)) THEN $
;   BEGIN
;      IF (NOT OBJ_ISA (self, state.(0))) THEN $
;         MESSAGE, 'Object state structure does not match object class.'
;   ENDIF
;
;   ; Go through each input tag copying it to the self object
;   FOR t = 1, noTags - 1 DO $
;   BEGIN
;      ok = EXECUTE ("self." + tags [t] + " = state." + tags[t])
;      IF (NOT ok) THEN PRINT, 'Tag ', tags [t], ' not found and ignored'
;   ENDFOR
;
;   ; Report completion
;   self -> Report, /Completed
;END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::WHOMESSAGERECIPIENTS
;
; PURPOSE:
;
;       This method prints out the name of all current message recipients and
;       which messages they are regestered for. This is a debugging method.
;
; SYNTAX:
;
;       thisObject -> WhoMessageRecipients
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None
;
;-
;*****************************************************************************************************
PRO CatAtom::WhoMessageRecipients

   @cat_pro_error_handler

   ; If the notification list is empty, return
   IF (NOT OBJ_VALID (self._messageRecipients)) THEN BEGIN
        Print, 'No message recipients have registered with this object.'
        RETURN
   ENDIF
   
   ; Get all the objects interested in the notification.
   ; The variable objs contains CATLISTVALUE objects. The "value"
   ; of such an object is the the object to be notified.
   objs = self._messageRecipients -> Get (/All, Count=noObjs)
   IF noObjs EQ 0 THEN Print, 'There are no message recipients.' ELSE Print, 'List of Message Recipients:'

   FOR i = 0, noObjs - 1 DO $
   BEGIN
      messageRecipient = objs [i] -> GetValue ()

      ; If the message receipient is invalid, remove it.
      IF Obj_Valid(messageRecipient) EQ 0 THEN BEGIN
         self._messageRecipients -> Remove, objs [i]
      ENDIF ELSE BEGIN
      
      Print, '    ' + messageRecipient -> GetName() + ': "' +  objs[i] -> GetName()+ '"'
      ENDELSE
   ENDFOR

   ; Report success
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::CLEANUP
;
; PURPOSE:
;
;       This is the CATATOM object class destructor method.
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
;
; MODIFICATIONS:
;
;       I have commented out the code that destroys the control panel, if it exits. While
;       programs work fine with this code in the IDLDE, the code causes errors on program
;       exit in run-time programs. I believe this is because of the order in which objects
;       are destroyed. In any case, control panels always destroy themselves in my experience
;       and commenting this code out here has not resulted in any memory leakage in any of
;       the programs I have tested. DWF. 10 Sept 2003.
;
;-
;*****************************************************************************************************
PRO CatAtom::CLEANUP, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Destroy UVALUE pointer.
   IF (PTR_VALID (self._uvalue)) THEN PTR_FREE, self._uvalue

   ; If a control panel exists, destroy it
   IF (OBJ_VALID (self._controlPanel)) THEN OBJ_DESTROY, self._controlPanel

   ; Clean up the notification list if necessary
   IF (OBJ_VALID (self._MessageRecipients)) THEN OBJ_DESTROY, self._MessageRecipients

   ; Clean up the list of event objects
   IF (OBJ_VALID (self._event_objects)) THEN OBJ_DESTROY, self._event_objects

   ; Take out the trash.
   OBJ_DESTROY, self._trash

   ; If any keywords have been passed, complain about them
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      MESSAGE, 'Unhandled keywords ('+TAG_NAMES (extraKeywords)+') in cleanup of '+OBJ_CLASS (self)+' object.'

   ; Call the superclass cleanup method
   self -> CatContainer::CLEANUP
   self -> IDLitComponent::CLEANUP

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATATOM::INIT
;
; PURPOSE:
;
;       This is the CATATOM object class creator method.
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
;       ERRORLEVEL:     The error level for this object. The default value is 2. Values can be:
;
;                       0: Silently ignore the error. No visual output to the user.
;                       1: Notify the user of the error by writing traceback information in the command log
;                          or journal file. No graphical user interface notification.
;                       2: Default. Notify the user with a graphical dialog and also write traceback
;                          information in the command log or journal file.
;
;       EVENT_METHOD:   Set this keyword to the name of the event method that should be
;                       called when an event is generated for this widget. If undefined,
;                       the event method is "EventHandler".
;
;       EVENT_OBJECT:   This is an object reference to an object that will receive the
;                       event from a specific widget event. It is the equivalent (in
;                       widget terms) of specifying an EVENT_PRO or EVENT_FUNC keyword.
;                       In other words, it is a way of deflected an event from the EVENTHANDLER
;                       method of the object that generated the event to the EVENTHANDLER
;                       method of the object specified with this keyword.
;
;       EXCLUSIVE_EVENT_OBJECT: If this keyword is set to a vaid object, events are passed directly
;                       and only to this object, ignoring the other event objects. To disable this,
;                       set this keyword to be a NULL object or zero. This keyword is designed for
;                       situations where an object wishes to hog the events for a limited period.
;
;       NO_COPY:        Set this keyword to transfer the UVALUE without copying.
;
;       PARENT:         A parent container object. Must be subclassed from IDL_CONTAINER. The object
;                       will be added to its parent container. (Input)
;
;       REPORTLEVEL:    This sets the report level for the object. Reports are sent with the REPORT
;                       method with a ReportLevel value. If the ReportLevel value is less than the
;                       REPORTLEVEL value set here, then the report is suppressed. The default REPORTLEVEL
;                       value is 2, allowing normal and debug messages to be recorded while suppressing
;                       more verbose reporting. Report levels correspond to these values:
;
;                       0: An "alert" message is sent.
;                       1: An "informational" message is sent.
;                       2: A "debug informational" message is sent.
;                       3: A "normal" message is sent. (This is the default for the REPORT method.)
;
;                       Reports are sent to a log file (if the Catalyst Library global variable
;                       LOGFILENAME is defined) or or the command log window.
;
;       UVALUE:         A user-value pointer. Can be used to store any IDL variable type.
;
;       _EXTRA:         Any keyword appropriate for the IDL_CONTAINER::INIT method.
;-
;*****************************************************************************************************
FUNCTION CatAtom::INIT, parent,                   $ ; parent object - passed to superclass
                     Event_Method=event_method,   $ ; The event handler method.
                     Event_Object = event_object, $ ; The "event handler" for this object.
                     ErrorLevel   = errorLevel  , $ ; error reporting level for this object
                     Exclusive_Event_Object = exclusive_event_object, $
                     No_Copy      = no_copy     , $ ; Used to set the UVALUE keyword.
                     Parent       = parentKwd,    $ ; Alternative to the parent param above.
                     ReportLevel  = reportLevel , $ ; info reporting level for this object
                     UValue       = uvalue ,      $ ; The user value.
                     _EXTRA=extraKeywords, $        ; Any extra keywords for CatContainer class.

                     ; IDLitComponent keywords

                     DESCRIPTION=description, $
                     ICON=icon, $
                     HELP=help, $
                     IDENTIFIER=identifier


   ; Enable error handling.
   @cat_func_error_handler

   IF ((N_ELEMENTS (parent) EQ 0) AND (KEYWORD_SET (parentKwd))) THEN parent = parentKwd

   ; Set the user value.
   IF (N_Elements(uvalue) NE 0) THEN $
      self._uvalue = Ptr_New(uvalue, NO_COPY=Keyword_Set(no_copy))

   ; Is an event method specified? If so, assign it. Otherwise,
   ; try to find one from the parent.
   IF N_Elements(event_method) NE 0 THEN BEGIN
      self._event_method = event_method
   ENDIF ELSE BEGIN
      IF Obj_Valid(parent) THEN parent -> GetProperty, Event_Method=event_method ELSE $
         event_method = 'EventHandler'
      self._event_method = event_method
   ENDELSE

   ; If no error level is specified, try and get a default value
   IF (N_ELEMENTS (errorLevel  ) GT 0) THEN self._errorLevel = errorLevel $
   ELSE BEGIN
      self._errorLevel = CatGetDefault ('ErrorLevel', Success=ok)
      IF (NOT ok) THEN $ ; no default exists - create one
      BEGIN
         ;CatSetDefault, 'ErrorLevel', 2
         self._errorLevel = 2
      ENDIF
   END

   ; If no reporting level is specified, try and get a default value
   IF (N_ELEMENTS (reportLevel) GT 0) THEN self._reportLevel = reportLevel $
   ELSE BEGIN
      reportLevel = CatGetDefault ('ReportLevel', Success=ok)
      IF (NOT ok) THEN $  ; no default exists - create one
      BEGIN
         ;CatSetDefault, 'ReportLevel', 2
         reportLevel = 2
      ENDIF
   END

   ; Set up an event handler to catch unhandled keywords specifically.
   Catch, theError
   IF theError NE 0 THEN $
   BEGIN
      Catch, /Cancel
      IF !Error_State.Name EQ 'IDL_M_KEYWORD_BAD' THEN $
      BEGIN
         name = Obj_Class(self)
         pos = Strpos(!Error_State.MSG, 'not allowed in call')
         keyword = StrMid(!Error_State.MSG, 7, pos-7)
         errmsg = 'Unhandled keyword ' + keyword + ' detected in ' + StrUpCase(name) + '.'
         HELP, /Last_Message, Output=msg

         HELP, Calls=callstack
         callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
         CASE errorLevel OF

            1 : BEGIN
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(name) + ':'
                Print, ''
                Print, "     " + errmsg
                FOR j=1,N_Elements(msg)-1 DO Print, "     " + msg[j]
             END

            2 : BEGIN
                junk = Dialog_Message(errmsg)
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(name) + ':'
                Print, ''
                Print, "     " + errmsg
                FOR j=1,N_Elements(msg)-1 DO Print, "     " + msg[j]

              END

            ELSE : BEGIN
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(name) + ':'
                Print, ''
                Print, "     " + errmsg
                FOR j=1,N_Elements(msg)-1 DO Print, "     " + msg[j]
              END

          ENDCASE

      ENDIF ELSE $
      BEGIN

         ; Cancel the error handler and set up the error handling to "throw" error
         HELP, Calls=callstack
         callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
         HELP, /Last_Message, Output=msg
         ON_ERROR, 2

         ; If the error has been previously handled, don't handle it here
         positions = StrPos(msg, '[cat_handled]')
         foundit = Where(positions NE -1, count)
         IF count GT 0 THEN $
         BEGIN
         IF Routine_Names(/Level) GT 2 THEN $
            MESSAGE, msg[0] ELSE RETURN, 0
         END

         ; Report the error
         IF (Obj_IsA_Valid (self, 'CatAtom')) THEN self -> Error $
         ELSE CASE errorLevel OF

               1 : BEGIN
                   Print, ''
                   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                   Print, ''
                   FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
                END

               2 : BEGIN
                   junk = Dialog_Message(msg[0])
                   Print, ''
                   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                   Print, ''
                   FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
                 END

               ELSE : BEGIN
                   Print, ''
                   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                   Print, ''
                   FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
                 END

             ENDCASE

         ; Throw the error, signalling that it's already been handled
         IF Routine_Names(/Level) GT 2 THEN $
            MESSAGE, msg[0] + ' [cat_handled]' ELSE RETURN, 0
      ENDELSE

      self -> Report, /Completed
      RETURN,  0
   ENDIF

   ; Call superclass INIT method.
   ok = self -> CatContainer::INIT (parent, _STRICT_EXTRA=extraKeywords)

   IF NOT ok THEN Message, 'Failed to initialise system component (CatContainer).'

   ; Initialize component class.
   ok = self->IDLitComponent::Init(DESCRIPTION=description, $
                     ICON=icon, $
                     HELP=help, $
                     IDENTIFIER=identifier)
   IF NOT ok THEN Message, 'Failed to initialise system component (IDLitComponent).'

   ; Set up an new CATCH error handler.
   Catch, theError
   IF theError NE 0 THEN $
   BEGIN

      ; Cancel the error handler and set up the error handling to "throw" error
      CATCH, /Cancel
      HELP, Calls=callstack
      callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
      HELP, /Last_Message, Output=msg
      ON_ERROR, 2

      ; If the error has been previously handled, don't handle it here
      positions = StrPos(msg, '[cat_handled]')
      foundit = Where(positions NE -1, count)
      IF count GT 0 THEN $
      BEGIN
      IF Routine_Names(/Level) GT 2 THEN $
         MESSAGE, msg[0] ELSE RETURN, 0
      END
      ; Report the error
      IF (Obj_IsA_Valid (self, 'CatAtom')) THEN self -> Error $
      ELSE CASE errorLevel OF

            1 : BEGIN
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                Print, ''
                FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
             END

            2 : BEGIN
                junk = Dialog_Message(msg[0])
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                Print, ''
                FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
              END

            ELSE : BEGIN
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                Print, ''
                FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
              END

          ENDCASE

      ; Throw the error, signalling that it's already been handled
      IF Routine_Names(/Level) GT 2 THEN $
         MESSAGE, msg[0] + ' [cat_handled]' ELSE RETURN, 0
   ENDIF

   ; Get the parent event object, if there is one.
   IF Obj_Isa_Valid(parent, 'CatAtom') THEN parent -> CatAtom::GetProperty, Event_Object=parent_event_object

   ; Memory management is off for event objects. They will clean themselves up
   ; when they are destroyed. We only want to know about them.
   self._event_objects = OBJ_NEW ('CatContainer', Memory_Management=0)
   IF Obj_Valid(event_object) THEN self._event_objects -> Add, event_object $
   ELSE BEGIN

      ; Is there an event handler procedure for this object or for any of its
      ; superclasses? Exclude the CATATOM class, which always has an EventHandler.
      procs     = ROUTINE_NAMES ()
      classname = (OBJ_CLASS (self))[0]
      REPEAT BEGIN
         junk        = WHERE (procs EQ (classname + '::EVENTHANDLER'), found)
         classname   = (OBJ_CLASS (classname, /Superclass))[0]
      ENDREP UNTIL (found EQ 1) OR (classname EQ 'CATATOM') OR (classname EQ 'WIDGETATOM')

      IF (found GT 0) THEN BEGIN

         eventObj = self

      ENDIF ELSE BEGIN

         ; If not, can we get it from the parent of this object?
         IF (Obj_Valid (parent_event_object)) THEN eventObj = parent_event_object

      ENDELSE

      IF (OBJ_VALID (eventObj)) THEN self._event_objects -> Add, eventObj

   ENDELSE

   ; Is there an exclusive event object? If there is an exclusive event object,
   ; it will be the only event object passed to the event dispatcher. This
   ; slight of hand is performed in the CatAtom::GetProperty method.
   IF (OBJ_VALID (exclusive_event_object)) THEN self._excl_event_object = exclusive_event_object

   ; Set up the trash container.
   self._trash = Obj_New('IDL_Container')

   ; Print status report. Return successful execution flag.
   self -> Report, /Completed
   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       CATATOM CLASS DEFINITION
;
; PURPOSE:
;
;       This is the CATATOM object's structure definition code.
;
;*****************************************************************************************************

PRO CatAtom__DEFINE, class

   class =  { CATATOM,                          $ ; The CATATOM object class name.
              _controlPanel       : OBJ_NEW (), $ ; The control panel for the object.
              _errorLevel         : 0B,         $ ; The error reporting level.
              _event_method       : "",         $ ; The name of the event method associated with this object.
              _event_objects      : OBJ_NEW (), $ ; The object(s) to whom events for this object are passed.
              _excl_event_object  : OBJ_NEW (), $ ; An exclusive event object.
              _messageRecipients  : OBJ_NEW (), $ ; A list of objects to be notified of method calls.
              _reportLevel        : 0B,         $ ; The info reporting level.
              _trash              : OBJ_NEW(),  $ ; A trash container for destroying other objects.
              _uvalue             : PTR_NEW (), $ ; A user value placeholder for the object.
              INHERITS CatContainer,            $ ; All objects in the Catalyst Library are containers
              INHERITS IDLitComponent           $ ; Inherits the IDLitComponnet class for object properties.
            }
END