;*****************************************************************************************************
;+
; NAME:
;       CatEventDispatcher
;
; PURPOSE:
;
;       The purpose of this utility routine is to intercept the event
;       callbacks from widgets and pass the "event" on to an object
;       EventHandler method. The EventHandler method must be written
;       with one positional parameter: the event structure that comes
;       from the widget. Note that the "name" of the object is added
;       as a field in every event structure.
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
;       Programming.
;
; CALLING SEQUENCE:
;
;       This routine is called by the IDL system, not by a user.
;
; MODIFICATION_HISTORY:
;
;       Written by: David Burridge, 13th March 2003.
;       Added EVENT_NAME field to each event object, which contains the name of the widget event. 6 Feb 2004, DWF.
;       Made a change in which the Event_Method is retrieved from the object causing the event, rather
;         than from the event object itself. 19 July 2004. DWF.
;       Removed reference to TOP_OBJECT. 22 January 2005. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008, jointly by Fanning Software Consulting, Inc.                        ;
;  and Burridge Computing. All rights reserved.                                            ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;      ; Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      ; Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      ; Neither the name of Fanning Software Consulting, Inc. or Burridge Computing       ;
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

PRO CatEventDispatcher, event

   ; Error handling. Returning from here will go directly to
   ; the RSI widget event dispatcher and cause problems with
   ; run-time programs. So we do not let errors propogate to
   ; that level.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      HELP, /Last_Message, Output=msg
      Catch, /Cancel

      ON_Error, 1

      ; If the error has been previously handled, don't report it again
      positions = StrPos(msg, '[cat_handled]')
      foundit = Where(positions NE -1, count)
      IF count EQ 0 THEN BEGIN
         void = Error_Message()
      END

      ; Clear the error state variable.
      !ERROR_STATE.NAME = "IDL_M_SUCCESS"
      !ERROR_STATE.CODE = 0L
      !ERROR_STATE.MSG = ""
      RETURN
   ENDIF

   ; Make sure you have a valid widget ID.
   IF Widget_Info(event.id, /Valid_ID) EQ 0 THEN RETURN

   ; Get the object reference of the object that generated the event and check
   ; to be sure it is a valid CATALYST object.
   WIDGET_CONTROL, event.id, Get_UValue=object

   ;object -> GetProperty, name=n & Print, n

   ; It is possible that real widget events can get into this event handler.
   ; For example, from CW_ANIMATE. If so, check to see if the TLB has a valid
   ; object and use that as the object if it does.
   IF (NOT Obj_IsA_Valid (object, 'CatAtom')) THEN BEGIN

      ; Check the TLB for an object.
      Widget_Control, event.top, Get_UValue=object

      IF (NOT Obj_IsA_Valid (object, 'CatAtom')) THEN $
         MESSAGE, 'Unable to dispatch the event to EventHandler method.'
   ENDIF

   ; Get the ID, TOP and HANDLER objects from the one that generated the event
   object -> CATATOM::GetProperty, Event_Objects=eventObjs, Name=objName

   ; Modify the event structure such that the ID and HANDLER fields
   ; are object references

   structName = TAG_NAMES (event, /Structure_Name)
   IF structName EQ "" THEN structName = 'ANONYMOUS'


   CASE structName OF

      ; Explicitly convert WIDGET_DRAW for maximum speed
      'WIDGET_DRAW' : objEvent = {ID        : object,          $
                                  HANDLER   : eventObjs[0],    $ ; Only the first of the event objects in HANDLER field.
                                  EVENT_NAME: structName,      $
                                  NAME      : objName,         $
                                  TYPE      : event.type,      $
                                  X         : event.x,         $
                                  Y         : event.y,         $
                                  PRESS     : event.press,     $
                                  RELEASE   : event.release,   $
                                  CLICKS    : event.clicks,    $
                                  MODIFIERS : event.modifiers, $
                                  CH        : event.ch,        $
                                  KEY       : event.key        $
                                  }

      ; A generic way of copying event struct with replacement ID and HANDLER. All Catalyst
      ; Library events also have EVENT_NAME and NAME fields.
      ELSE : BEGIN
             tags  = TAG_NAMES (event)
             objEvent = Create_Struct('ID', object, 'HANDLER', eventObjs[0], $
               'EVENT_NAME', structName, 'NAME', objName)
             FOR j=3,N_Elements(tags)-1 DO objEvent = Create_Struct(objEvent, tags[j], event.(j))
             ENDCASE

   ENDCASE

   ; Check that there is at least one valid event object, otherwise complain
   IF (MAX (OBJ_VALID (eventObjs)) EQ 0) THEN $
      MESSAGE, 'No event handler for object ' + OBJ_CLASS (object) + ' named ' + objName

   ; Find out the target object for the event, get the event method from
   ; the target object, and send the event to this method.
   FOR e = 0, N_ELEMENTS (eventObjs) - 1 DO $
   BEGIN
      IF OBJ_VALID (eventObjs [e]) THEN $
      BEGIN
         objEvent.handler = eventObjs [e]
         object -> CatAtom::GetProperty, Event_Method=event_method

         ; Handy debugging code.
;         object -> GetProperty, Name=generator
;         Print, 'Object Generating Event is : ', generator
;         eventObjs[e] -> GetProperty, Name=receiver
;         Print, 'Object Receiving Event is : ', receiver
;         Print, 'Event handler method is: ', event_method
;         Print, ""

         Call_Method, event_method, eventObjs[e], objEvent
      ENDIF
   ENDFOR

END