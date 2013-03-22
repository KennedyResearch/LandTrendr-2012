;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEGROUP__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to group members of a SelectableObject class together.
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
;       textObject = Obj_New("SELECTABLEGROUP", theMembers)
;
; SUPERCLASSES:
;
;       SELECTABLEOBJECT
;       CATDATAATOM
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { SELECTABLEGROUP, $
;             theMembers: Ptr_New(), $  ; A pointer to the group members.
;             INHERITS SELECTABLEOBJECT $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 12 August 2004.
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
;       SELECTABLEGROUP::CALCULATEBOUNDARYBOX
;
; PURPOSE:
;
;       This method calculates the boundary box around the entire group of selectable objects.
;       The box is always in normalized coordinates.
;
; SYNTAX:
;
;       theObject -> CalculateBoundaryBox
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
PRO SelectableGroup::CalculateBoundaryBox

   @cat_pro_error_handler

   g_max_x = 0.0
   g_min_x = 1.0
   g_max_y = 0.0
   g_min_y = 1.0
   IF Ptr_Valid(self.theMembers) THEN BEGIN
      FOR j=0, N_Elements(*self.theMembers)-1 DO BEGIN
         theObject = (*self.theMembers)[j]
         box = theObject -> Outline()
         max_x = Max(box[0,*], Min=min_x)
         max_y = Max(box[1,*], Min=min_y)
         g_max_x = g_max_x > max_x
         g_min_x = g_min_x < min_x
         g_max_y = g_max_y > max_y
         g_min_y = g_min_y < min_y
      ENDFOR
   ENDIF
   self.box[0,*] = [g_min_x, g_min_x, g_max_x, g_max_x, g_min_x]
   self.box[1,*] = [g_min_y, g_max_y, g_max_y, g_min_y, g_min_y]

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEGROUP::DRAW
;
; PURPOSE:
;
;       This method draws the box around the selectable objects.
;
; SYNTAX:
;
;       theObject -> Draw
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       COLOR:    The name of a color to draw the box in. By default, 'white'
;
;-
;*****************************************************************************************************
PRO SelectableGroup::Draw, Color=color, _Extra=extrakeywords

   @cat_pro_error_handler

   IF Ptr_Valid(self.theMembers) THEN BEGIN
      FOR j=0,N_Elements(*self.theMembers)-1 DO (*self.theMembers)[j] -> Draw, _Extra=extrakeywords
   ENDIF

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEGROUP::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain SELECTABLEGROUP properties. Be sure
;       you ALWAYS call the superclass GETPROPERTY method if you have extra
;       keywords!
;
; SYNTAX:
;
;       theObject -> GetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     MEMBERS:       The members of the group.
;
;     _REF_EXTRA:    Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO SelectableGroup::GetProperty, $
   BOUNDARY_BOX=box, $
   MEMBERS=theMembers, $
   VISIBLE=visible, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(box) THEN box = self.box
   IF Arg_Present(theMembers) THEN IF Ptr_Valid(self.theMembers) THEN theMembers = *self.theMembers
   IF Arg_Present(visible) THEN visible = self.visible

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       BOX::INTERACTIONEVENTS
;
; PURPOSE:
;
;       This method accepts events from an interaction object of some type. The interaction
;       may pre-process events, or send them directly here.
;
; SYNTAX:
;
;       theObject -> INTERACTIONEVENTS
;
; ARGUMENTS:
;
;     event:          The widget event that is generated by the draw widget and handled by the Interaction
;                     object.
;
; KEYWORDS:
;
;     INTERACTION:    The object reference to a Interaction object that is receiving events.
;                     This is a *required* parameter, but is written as a keyword for programming clarity.
;
;-
;*****************************************************************************************************
PRO SelectableGroup::InteractionEvents, event, Interaction=interaction

   @cat_pro_error_handler

   ; Get information from the INTERACTION object. In particular, you want to know the MODE, and
   ; the object references of the draw widget and pixmap.
   IF Obj_Valid(interaction) EQ 0 THEN Message, 'An "interaction" object is a required keyword parameter.'
   interaction -> GetProperty, MODE=mode, DRAWWIDGET=drawID, PIXMAP=pixmap

   ; What kind of event is this?
   eventTypes = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSED', 'CHARACTER', 'KEYPRESS']
   thisEvent = eventTypes[event.type]

   ; Action is based on the current mode of the interaction.
   CASE mode OF

      'SELECT': BEGIN

         END ; of SELECT mode


         ELSE:

  ENDCASE

   self -> Report, /Completed


END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEGROUP::MOVE
;
; PURPOSE:
;
;       This method moves the group of selectable objects in a graphics window.
;
; SYNTAX:
;
;       theObject -> Move, x, y
;
; ARGUMENTS:
;
;       X:        The number of pixels to move in the X direction.
;
;       Y:        The number of pixels to move in the Y direction.
;
; KEYWORDS:
;
;       NODRAW:   If this keyword is set, only the coordinates are updated. No drawing occurs.
;
;       PIXMAP:   A pixmap that can be supplied for fast re-draw.
;-
;*****************************************************************************************************
PRO SelectableGroup::Move, x, y, NODRAW=nodraw, PIXMAP=pixmap

   @cat_pro_error_handler

   ; Move each object.
   IF Ptr_Valid(self.theMembers) THEN BEGIN

      FOR j=0,N_Elements(*self.theMembers)-1 DO BEGIN
         (*self.theMembers)[j] -> Move, x, y, /NoDraw
      ENDFOR

      ; Update the boundary box coordinates.
      self -> CalculateBoundaryBox

   ENDIF

   ; Need to refresh?
   IF ~Keyword_Set(nodraw) THEN BEGIN

      ; Redraw, fast if you have a pixmap.
      IF Obj_Isa_Valid(pixmap, 'PIXMAPWIDGET') THEN BEGIN
         self -> CopyParameters, drawID, Destination=d, Extent=e
         pixmap -> Copy, Destination=d, Extent=e, Origin=d
         self -> Draw
      ENDIF ELSE BEGIN
         CatRefreshDraw, self, Stop_At='DRAWWIDGET', /NoErase
      ENDELSE

   ENDIF

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEGROUP::SELECT
;
; PURPOSE:
;
;       This method returns the object reference if the requested point is inside
;       the text bounding box.
;
; SYNTAX:
;
;       selectedObject = theObject -> Select, x, y
;
; ARGUMENTS:
;
;       X:   The X location of a point in device or window coordinates.
;
;       Y:   The Y location of a point in device or window coordinates.
;
; KEYWORDS:
;
;       SUCCESS:   Set to 1 if a selection is made. To 0 otherwise.
;-
;*****************************************************************************************************
FUNCTION SelectableGroup::Select, x, y, SUCCESS=success

   @cat_func_error_handler

   ; Set up return values.
   retval = Obj_New()
   success = 0

   ; No selection possible, if invisible.
   IF self.visible EQ 0 THEN RETURN, retval

   ; If you belong to a group, you cannot be selected individually.
   IF Obj_Valid(self.mygroup) THEN RETURN, Obj_New()

   ; Convert the point from device to normalized coordinates.
   c = Convert_Coord(x, y, /Device, /To_Normal)
   xx = c[0,0]
   yy = c[1,0]

   ; Update the box coordinates.
   self -> CalculateBoundaryBox

   ; Are you inside?
   isInside = Inside(xx, yy, Reform(self.box[0,0:3]), Reform(self.box[1,0:3]))
   IF isInside THEN BEGIN
      retVal = self
      success = 1
   ENDIF

   self -> Report, /Completed
   RETURN, retval

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEGROUP::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the SELECTABLEGROUP object's properties. Be sure
;       you ALWAYS call the superclass SETPROPERTY method if you have extra keywords!
;
;
; SYNTAX:
;
;       theObject -> SetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     MEMBERS:       The members of the group.
;
;     NOREFRESH:     Set this keyword if immediate refreshing of the object on the display is not required.
;
;     VISIBLE:       Set this keyword to turn all the members of the group on or off.
;
;     _EXTRA:        Any keywords appropriate for the superclass object.
;-
;*****************************************************************************************************
PRO SelectableGroup::SetProperty, $
   MEMBERS=theMembers, $
   NOREFRESH=norefresh, $
   VISIBLE=visible, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(theMembers) NE 0 THEN BEGIN
      IF Ptr_Valid(self.theMembers) THEN BEGIN
            FOR j=0,N_Elements(*self.theMembers)-1 DO (*self.theMembers)[j] -> SetGroup, Obj_New()
            *self.theMembers = theMembers
         ENDIF ELSE BEGIN
            self.theMembers = Ptr_New(theMembers)
         ENDELSE
      FOR j=0,N_Elements(*self.theMembers)-1 DO BEGIN
         (*self.theMembers)[j] -> SetGroup, self
      ENDFOR
   ENDIF

   IF N_Elements(visible) NE 0 THEN BEGIN
      IF Ptr_Valid(self.theMembers) THEN $
         FOR j=0,N_Elements(*self.theMembers)-1 DO $
            (*self.theMembers)[j] -> SetProperty, VISIBLE=Keyword_Set(visible), NOREFRESH=Keyword_Set(norefresh)
      self.visible = Keyword_Set(visible)
   ENDIF
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEGROUP::UPGROUP
;
; PURPOSE:
;
;       This method disassociates the grouped objects
;
; SYNTAX:
;
;       selectedObject = Ungroup, ptrToMembers
;
; ARGUMENTS:
;
;       ptrToMembers:   If present and a pointer, member objects are stored in this output variable.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO SelectableGroup::Ungroup, ptrToMembers

   @cat_pro_error_handler

   FOR j=0,N_Elements(*self.theMembers)-1 DO BEGIN
      (*self.theMembers)[j] -> SetGroup, Obj_New()
   ENDFOR
   IF Ptr_Valid(ptrToMembers) THEN BEGIN
      *ptrToMembers = *self.theMembers
   ENDIF

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEGROUP::CLEANUP
;
; PURPOSE:
;
;       This is the SELECTABLEGROUP object class destructor method.
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
;      None.
;-
;*****************************************************************************************************
PRO SelectableGroup::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self.theMembers
   self -> SELECTABLEOBJECT::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEGROUP::INIT
;
; PURPOSE:
;
;       This is the SELECTABLEGROUP object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     MEMBERS:       An object array of the members of the group.
;
;     _EXTRA:        Any keywords appropriate for the CATDATAATOM INIT method.
;-
;*****************************************************************************************************
FUNCTION SelectableGroup::INIT, $
   MEMBERS=theMembers, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   ok = self -> SELECTABLEOBJECT::INIT(_EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   IF N_Elements(theMembers) NE 0 THEN BEGIN
      self.theMembers = Ptr_New(theMembers)
      FOR j=0,N_Elements(*self.theMembers)-1 DO (*self.theMembers)[j] -> SetGroup, self
   ENDIF

   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       SELECTABLEGROUP CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the SELECTABLEGROUP object.
;
;*****************************************************************************************************
PRO SelectableGroup__Define, class

   class = { SELECTABLEGROUP, $
             theMembers: Ptr_New(), $  ; The selectable objects that comprise the members of the group.
             INHERITS SelectableObject $
           }

END
