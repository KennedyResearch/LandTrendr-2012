;*****************************************************************************************************
;+
; NAME:
;       CATLAYER__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a container object that can be used
;       for layering or overlaying graphic elements in a draw widget, on an image, etc.
;       Essentially, it is a means of grouping graphic elements (e.g., annotations) in
;       a way that make them easy to draw and easy to turn on/off.
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
;       theObject = Obj_New("CATLAYER")
;
; SUPERCLASSES:
;
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { CATLAYER, $
;              _coords    : OBJ_NEW(), $   ; A CATCOORD object of some type.
;              _colors    : OBJ_NEW(), $   ; A COLORTOOL object for setting up color tables.
;              _visible   : 0L, $          ; A flag that indicates the layer is visible (1), or not (0).
;              _selectable: 0L, $          ; A flag that indicates if the layer will allow selection on contained objects.
;             INHERITS CATATOM $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 18 January 2005.
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
;       CATLAYER::APPLYCOLORS
;
; PURPOSE:
;
;       This method sets up the colors for the layer object if they exist. For this
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
PRO CatLayer::ApplyColors

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
;       CATLAYER::APPLYCOORDS
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
PRO CatLayer::ApplyCoords

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
;       CATLAYER::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the CATLAYER object. A
;       control panel is a graphical user interface for setting object
;       properties. If you create a control panel, the events are typically
;       sent to the EVENTHANDLER method.
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
PRO CatLayer::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Object Control Panel', _EXTRA=extraKeywords)

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.

   base = Obj_New('BASEWIDGET', cp, Column=1, Frame=1)

   ; Display the control panel if it created its own TLB.

   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATLAYER::DRAW
;
; PURPOSE:
;
;       This method may or may not be needed by your object, depending
;       upon whether a graphical representation of the object is required.
;       If you wish the DRAW method to automatically propogates down to any
;       objects contained in this object's container, call the superclass DRAW
;       method.
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
;       None.
;
;-
;*****************************************************************************************************
PRO CatLayer::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; If invisible, RETURN immediately.
   IF ~self._visible THEN RETURN

   ; Set up colors and coordinate system, if appropriate.
   self -> ApplyColors
   self -> ApplyCoords

   ; Draw any objects contained within this object.
   self -> CATATOM::Draw, _Extra=extrakeywords

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;        CATLAYER::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the CATLAYER object. It will typically
;        be used to respond to events from widget objects created in the CONTROLPANEL
;        method.
;
; SYNTAX:
;
;        This method is called automatically by the event handling mechanism.
;
; ARGUMENTS:
;
;       event: The event structure as described in the IDL help files, except
;              that the ID, TOP and HANDLER tags will be object references.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO CatLayer::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   CASE event.name OF
      ELSE: Print, 'Received an event from: ', event.name, ' in CatLayer EventHandler.'
   ENDCASE

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATLAYER::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain CATLAYER properties. Be sure
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
;      COLOR_OBJECT:   Use this keyword to get the contained COLORTOOL object.
;
;      COORD_OBJECT:   Use this keyword to get the contained CATCOORD object.
;
;      SELECTABLE:     Set to 1 if the layer is selectable, otherwise to 0.
;
;      VISIBLE:        Set to 1 if the layer is visible, otherwise to 0.
;
;      _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO CatLayer::GetProperty, $
   COLOR_OBJECT=color_object, $
   COORD_OBJECT=coord_object, $
   SELECTABLE=selectable, $
   VISIBLE=visible, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(color_object) THEN color_object = self._colors
   IF Arg_Present(coord_object) THEN coord_object = self._coords
   IF Arg_Present(selectable) THEN selectable = self._selectable
   IF Arg_Present(visible) THEN visible = self._visible

   ; Any other keywords?
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATATOM::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATLAYER::SELECT
;
; PURPOSE:
;
;       This method passes on a selection event to its SELECTABLEOBJECT children, and returns
;       all possible selected objects.
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
FUNCTION CatLayer::Select, x, y, SUCCESS=success

   @cat_func_error_handler

   ; Set up return values.
   retval = Obj_New()
   success = 0

   ; No selection possible, if invisible.
   IF ~self._visible THEN RETURN, retval

   ; Is the object selectable?
   IF ~self._selectable THEN RETURN, retval

   ; Get all the SELECTABLEOBJECT children of this object. If no children, RETURN.
   children = self -> Get(/All, ISA='SELECTABLEOBJECT', Count=count)
   IF count EQ 0 THEN RETURN, retval

   ; Call the SELECT method on each child in turn, accumulate selectable objects.
   FOR j=0,N_Elements(children)-1 DO BEGIN
      selectedObject = children[j] -> Select(x, y, Success=success)
      IF success THEN BEGIN
         IF N_Elements(ptr) EQ 0 THEN ptr = Ptr_New(selectedObject) ELSE *ptr = [*ptr, selectedObject]
      ENDIF
   ENDFOR

   ; If you have found any objects, return them.
   IF Ptr_Valid(ptr) THEN BEGIN
      retval = *ptr
      Ptr_Free, ptr
      success = 1
   ENDIF ELSE success = 0

   self -> Report, /Completed
   RETURN, retval

END



;*****************************************************************************************************
;+
; NAME:
;       CATLAYER::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the CATLAYER object's properties. Be sure
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
;     SELECTABLE:  Set this keyword to 0 to turn layer selectability OFF and to 1 to turn selectability ON.
;                  Selectability is set to 1 by default.
;
;      VISIBLE:    Set this keyword to 0 to turn layer visibilty OFF and to 1 to turn visibility ON.
;
;     _EXTRA:      Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO CatLayer::SetProperty, $
   COLOR_OBJECT=color_object, $
   COORD_OBJECT=coord_object, $
   SELECTABLE=selectable, $
   VISIBLE=visible, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATATOM::SetProperty, _EXTRA=extraKeywords

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
         IF Obj_Valid(self._colors) THEN self._colors -> AddParent, self
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
         IF Obj_Valid(self._coords) THEN self._coords -> AddParent, self
      ENDELSE
   ENDIF

   IF N_Elements(selectable) NE 0 THEN self._selectable = Keyword_Set(selectable)
   IF N_Elements(visible) NE 0 THEN self._visible = Keyword_Set(visible)

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATLAYER::CLEANUP
;
; PURPOSE:
;
;       This is the CATLAYER object class destructor method.
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
PRO CatLayer::CLEANUP

   @cat_pro_error_handler

   IF OBJ_VALID(self._coords) THEN self._coords -> RemoveParent, self
   IF OBJ_VALID(self._colors) THEN self._colors -> RemoveParent, self
   self -> CATATOM::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATLAYER::INIT
;
; PURPOSE:
;
;       This is the CATLAYER object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     parent:     The parent object reference.
;
; KEYWORDS:
;
;     COLOR_OBJECT: Use this keyword to load a color object for setting up colors for data display.
;
;     COORD_OBJECT: Use this keyword to load a coordinate object for setting up the data coordinate
;                   system for data display.
;
;     SELECTABLE:   Set this keyword to 0 to turn layer selectability OFF and to 1 to turn selectability ON.
;                   Selectability is set to 1 by default.
;
;     VISIBLE:      Set this keyword to 0 to turn layer visibilty OFF and to 1 to turn visibility ON.
;                   Visibility is set to 1 by default.
;
;     _EXTRA:       Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION CatLayer::INIT, parent, $
   COLOR_OBJECT=color_object, $
   COORD_OBJECT=coord_object, $
   SELECTABLE=selectable, $
   VISIBLE=visible, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   ok = self -> CATATOM::INIT (parent, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

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

   ; Set keyword defaults.
   IF N_Elements(selectable) EQ 0 THEN selectable = 1
   IF N_Elements(visible) EQ 0 THEN visible = 1

   ; Load the object.
   self._selectable = Keyword_Set(selectable)
   self._visible = Keyword_Set(visible)

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       CATLAYER CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the CATLAYER object.
;
;*****************************************************************************************************
PRO CatLayer__DEFINE, class

   class = { CATLAYER, $
              _coords    : OBJ_NEW(), $   ; A CATCOORD object of some type.
              _colors    : OBJ_NEW(), $   ; A COLORTOOL object for setting up color tables.
              _visible   : 0L, $          ; A flag that indicates the layer is visible (1), or not (0)
              _selectable: 0L, $          ; A flag that indicates if the layer will allow selection on contained objects.
             INHERITS CATATOM $
           }

END
