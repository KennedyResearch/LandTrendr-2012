;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a selectable object for user interaction.
;       This is essentially an abstract class for selectable objects.
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
;       textObject = Obj_New("SELECTABLEOBJECT")
;
; SUPERCLASSES:
;
;       CATDATAATOM
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { SELECTABLEOBJECT, $
;             background: 0L, $            ; A flag to indicate if a background box is drawn.
;             bg_color: "", $              ; The name of the background color.
;             box: DblArr(2,5), $          ; The box around the selectable object is ALWAYS in normal coordinates.
;             color: "", $                 ; The name of a color to draw the object in.
;             contextmenu: Obj_New(), $    ; A holder for a context menu object.
;             internalCoord : Obj_New(), $ ; A holder for an internal coordinate object.
;             mygroup: Obj_New(), $        ; A SELECTABLEGROUP object, of which this object is a member.
;             selectable: 0L, $            ; A flag to indicate the object is selectable.
;             visible: 0L, $               ; A flag to indicate if the object is visible (should be drawn).
;             INHERITS CatDataAtom $
;           }
;
; MESSAGES:
;
;        When a selectable object is deleted from the SetProperty method, the message
;        OBJECT_DELETED is broadcast to interested parties just before the object is
;        destroyed. The DATA field will be set to the object reference of the object
;        just about to be destoyed.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 9 August 2004.
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
;       SELECTABLEOBJECT::AddToEventStructure
;
; PURPOSE:
;
;       This method calculates the boundary box around the selectable object. The box
;       is always in normalized coordinates. This method should be overridden by subclassed
;       objects. This method will need to be overridden in superclass objects.
;
; SYNTAX:
;
;       theObject -> AddToEventStructure
;
; ARGUMENTS:
;
;       theEvent:    The event structure to which you are to add object-specific information.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
FUNCTION SelectableObject::AddToEventStructure, theEvent

    RETURN, theEvent

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::CALCULATEBOUNDARYBOX
;
; PURPOSE:
;
;       This method calculates the boundary box around the selectable object. The box
;       is always in normalized coordinates. This method should be overridden by subclassed
;       objects.
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
PRO SelectableObject::CalculateBoundaryBox

   @cat_pro_error_handler

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::COPYPARAMETERS
;
; PURPOSE:
;
;       This method returns the lower-left corner of the bounary box
;       in the DESTINATION keyword, and the number of columns and rows
;       in the boundary box in the EXTENT keyword, all in window or pixel
;       coordinates. It's purpose is to return a section of a pixmap, for
;       example, so that only that section can be copied.
;
; SYNTAX:
;
;       theObject -> CopyParameters, drawid, DESTINATION=destination, EXTENT=extent
;
; ARGUMENTS:
;
;       drawID:         The identifier of a draw widget object whose extent will
;                       provide the size of the window for calculating device coordinates.
;                       This parameter is required.
;
; KEYWORDS:
;
;       DESTINATION:    A two-element array containing the lower-left corner
;                       of the boundary box in device coordinates. An output keyword.
;
;       EXTENT:         A two-element array containing the number of columns and
;                       rows in the boundary box in device coordinates. An output keyword.
;
;-
;*****************************************************************************************************
PRO SelectableObject::CopyParameters, drawid, DESTINATION=destination, EXTENT=extent

   @cat_pro_error_handler

   IF N_Elements(drawid) EQ 0 THEN Message, 'The DRAWID argument is required.'

   ; Make sure the draw widget is the current graphics window.
   drawID -> SetWindow

   minx = Min(self.box[0,*], Max=maxx)
   miny = Min(self.box[1,*], Max=maxy)
   c = Convert_Coord([minx, maxx-minx], [miny, maxy-miny], /Normal, /To_Device)

   destination = [c[0,0]-12,c[1,0]-12]
   extent = [c[0,1]+24, c[1,1]+24]

   ; Make sure you are not out of the window. (Primarily for X Windows devices.)
   IF destination[0] LT 0 THEN destination[0] = 0.0
   IF destination[0] GT ((!D.X_Size-1) - extent[0]) THEN destination[0] = (!D.X_Size) - extent[0] + 1
   IF destination[1] LT 0 THEN destination[1] = 0.0
   IF destination[1] GT ((!D.Y_Size-1) - extent[1]) THEN destination[1] = (!D.Y_Size) - extent[1] + 1

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::DRAW
;
; PURPOSE:
;
;       This method draws the box around the selectable object.
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
;-
;*****************************************************************************************************
PRO SelectableObject::Draw, _Extra=extrakeywords

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::DRAWSELECTIONBOX
;
; PURPOSE:
;
;       This method draws a selection box around the boundary box of the text
;
; SYNTAX:
;
;       theObject -> DrawSelectionBox
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       COLOR:    The name of a color to draw the box in. By default, the color of the text.
;
;-
;*****************************************************************************************************
PRO SelectableObject::DrawSelectionBox, Color=color

   @cat_pro_error_handler

   ; If the text is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   IF N_Elements(color) EQ 0 THEN color = 'White'

   ; Update the boundary box.
   self -> CalculateBoundaryBox
   IF (self.box[0,0] EQ self.box[0,2]) OR (self.box[1,0] EQ self.box[1,1]) THEN RETURN

   ; Draw the box.
   Plots, self.box[0,*], self.box[1,*], Color=FSC_Color(color), Thick=2, /Normal

   ; Draw the handles on the box.
   phi = Findgen(32) * (!PI * 2 / 32.)
   phi = [ phi, phi[0] ]
   UserSym, Cos(phi), Sin(phi), /Fill
   x1 = self.box[0,0]
   y1 = self.box[1,0]
   x2 = self.box[0,1]
   y2 = self.box[1,1]
   x3 = self.box[0,2]
   y3 = self.box[1,2]
   x4 = self.box[0,3]
   y4 = self.box[1,3]

   PLOTS, x1, y1, PSYM=8, Color=FSC_Color(color), /Normal
   PLOTS, x2, y2, PSYM=8, Color=FSC_Color(color), /Normal
   PLOTS, x3, y3, PSYM=8, Color=FSC_Color(color), /Normal
   PLOTS, x4, y4, PSYM=8, Color=FSC_Color(color), /Normal

   maxx = Max([x1,x4], Min=minx)
   maxy = Max([y1,y4], Min=miny)
   lmidx = (maxx - minx) / 2.0 + minx
   lmidy = (maxy - miny) / 2.0 + miny
   PLOTS, lmidx, lmidy, PSYM=8, Color=FSC_Color(color), /Normal

   maxx = Max([x2,x3], Min=minx)
   maxy = Max([y2,y3], Min=miny)
   umidx = (maxx - minx) / 2.0 + minx
   umidy = (maxy - miny) / 2.0 + miny
   PLOTS, umidx, umidy, PSYM=8, Color=FSC_Color(color), /Normal

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::EVENTHANDLER
;
; PURPOSE:
;
;       This method is an event handler for the Control Panel.
;
; SYNTAX:
;
;       Called automatically by the event handling system
;
; ARGUMENTS:
;
;       event:  The event structure.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO SelectableObject::EventHandler, event

   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF


      'SEND_FRONT': self -> SetProperty, Bring_To_Front=1

      'SEND_BACK': self -> SetProperty, Send_To_Back=1

      'FOREGROUND_COLOR': BEGIN
                  self -> GetProperty, Color=color
                  color = PickColorName(color, Group_Leader=self.contextmenu->GetID())
                  self -> SetProperty, Color=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, REQUESTER=self

                    END

      'BACKGROUND_COLOR': BEGIN
                  self -> GetProperty, BG_Color=color
                  color = PickColorName(color, Group_Leader=self.contextmenu->GetID())
                  self -> SetProperty, BG_Color=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, REQUESTER=self

                    END

      'BACKGROUND_VISIBLE': self -> SetProperty, Background=1

      'BACKGROUND_INVISIBLE': self -> SetProperty, Background=0

      'MOVE_FORWARD': self -> SetProperty, Move_Forward=1

      'MOVE_BACKWARD': self -> SetProperty, Move_Backward=1

      'OTHER_PROPERTIES': BEGIN
                    event.id -> GetProperty, UValue=drawID
                    self -> ControlPanel, Group_Leader=drawID
                    END

      'DELETE_OBJECT': self -> SetProperty, Delete=1


   ENDCASE

   IF Obj_Valid(self) THEN self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::OUTLINE
;
; PURPOSE:
;
;       This method returns a box in normalized coordinates that contains the selectable object
;       and all of its parts (labels, text, etc). It is similar to the boundary box of the
;       selectable object and, in fact, is often just the boundary box. Like the boundary box,
;       the last element in the 2x5 array is the same as the first element.
;
; SYNTAX:
;
;       outline = theObject -> Outline()
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
; RETURN_VALUE:
;
;     outline:    A 2x5 array containing the XY point pairs in normalized coordinates of a rectangle
;                 big enough to contain all the parts of the object.
;
;     None.
;-
;*****************************************************************************************************
FUNCTION SelectableObject::Outline

   self -> GetProperty, Boundary_Box=box
   theOutline = box

   RETURN, theOutline

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain SELECTABLEOBJECT properties. Be sure
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
;     BACKGROUND:       Set to 1 if drawing a background for the text.
;
;     BG_COLOR:         The name of the background color.
;
;     BOUNDARY_BOX:     The current boundary box for the text. A 2x5 double array.
;
;     COLOR:            Set this keyword to the name of a color for the text. By default, "white".
;
;     MEMBERSHIP_GROUP: The group this selectable object belongs to.
;
;     SELECTABLE:       A flag that indicates if the object is selectable (1) or not (0).
;
;     VISIBLE:          If text is visible, this keyword is set to 1. O otherwise.
;
;     _REF_EXTRA:       Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO SelectableObject::GetProperty, $
   BACKGROUND=background, $
   BG_COLOR=bg_color, $
   BOUNDARY_BOX=box, $
   COLOR=color, $
   DELETE=delete, $
   MEMBERSHIP_GROUP=mygroup, $
   SELECTABLE=selectable, $
   VISIBLE=visible, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(background) THEN background = self.background
   IF Arg_Present(bg_color) THEN bg_color = self.bg_color
   IF Arg_Present(box) THEN box = self.box
   IF Arg_Present(color) THEN color = self.color
   IF Arg_Present(delete) THEN delete = 0 ; Dummy value for REGISTERPROPERTY functionality.
   IF Arg_Present(mygroup) THEN mygroup = self.mygroup
   IF Arg_Present(visible) THEN visible = self.visible


   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATDATAATOM::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::INITIALIZE
;
; PURPOSE:
;
;       This method allows the user to initialize the object, if necessary. It is
;       expected that user-defined objects will overwrite this object method.
;
; SYNTAX:
;
;       theObject -> Initialize
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
PRO SelectableObject::Initialize
END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::MOVE
;
; PURPOSE:
;
;       This method moves the selectable in a graphics window.
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
;       NODRAW:   If this keyword is set, then no drawing of the object takes place.
;
;       PIXMAP:   A pixmap that can be supplied for fast re-draw.
;-
;*****************************************************************************************************
PRO SelectableObject::Move, x, y, Pixmap=pixmap, NoDraw=nodraw

   @cat_pro_error_handler

   ; Convert the device pixels into normalized coordinates.
   self -> ApplyCoords
   self._coords -> GetProperty, XRANGE=xr, YRANGE=yr
   xx = (xr[1] - xr[0]) / !D.X_Size * x
   yy = (yr[1] - yr[0]) / !D.Y_Size * y
   c = Convert_Coord(xx, yy, /Device, /To_Normal)

   ; Update the boundary box coordinates.
   self -> CalculateBoundaryBox

   ; Add to the boundary box.
   self.box[0,*] = self.box[0,*] + c[0,0]
   self.box[1,*] = self.box[1,*] + c[1,0]

   IF ~Keyword_Set(nodraw) THEN BEGIN

      ; Redraw, fast if you have a pixmap.
      IF Obj_Isa_Valid(pixmap, 'PIXMAPWIDGET') THEN BEGIN
         pixmap -> Copy
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
;       SELECTABLEOBJECT::SELECT
;
; PURPOSE:
;
;       This method returns the object reference if the requested point is inside
;       the bounding box.
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
FUNCTION SelectableObject::Select, x, y, SUCCESS=success

   @cat_func_error_handler

   ; Set up return values.
   retval = Obj_New()
   success = 0

   ; No selection is possible if the object is invisible.
   IF self.visible EQ 0 THEN RETURN, retval

   ; If you belong to a group, you cannot be selected individually.
   IF Obj_Valid(self.mygroup) THEN RETURN, retval

   ; No selection is possible, if the object is currently unselectable.
   IF ~self.selectable THEN RETURN, retval

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
;       SELECTABLEOBJECT::SELECTPANEL
;
; PURPOSE:
;
;       Similar to a Control Panel, it gives context menu access to properties
;       of selectable objects.
;
; SYNTAX:
;
;       selectedObject = theObject -> SelectPanel, x, y, drawID
;
; ARGUMENTS:
;
;       X:       The X location of a point in device or window coordinates.
;
;       Y:       The Y location of a point in device or window coordinates.
;
;       DRAWID:  The identifer of the draw widget object in which the selection is taking place.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO SelectableObject::SelectPanel, x, y, drawID

   @cat_pro_error_handler

   IF N_Params() NE 3 THEN Message, 'Incorrect number of positonal parameters.'

   IF Obj_Valid(self.contextmenu) THEN Obj_Destroy, self.contextMenu
   self.contextMenu = Obj_New('ContextMenuBase', drawID, Column=1, Event_Object=self)
   button = Obj_New('ButtonWidget', self.contextMenu, Value='Bring to Front', Name='SEND_FRONT')
   button = Obj_New('ButtonWidget', self.contextMenu, Value='Send to Back', Name='SEND_BACK')
   button = Obj_New('ButtonWidget', self.contextMenu, Value='Move Forward', Name='MOVE_FORWARD')
   button = Obj_New('ButtonWidget', self.contextMenu, Value='Move Backward', Name='MOVE_BACKWARD')
   self -> GetProperty, Background=background

   button = Obj_New('ButtonWidget', self.contextMenu, Value='Foreground Color...', Name='FOREGROUND_COLOR', /Separator)
   IF background THEN $
      button = Obj_New('ButtonWidget', self.contextMenu, Value='Background Off.', Name='BACKGROUND_INVISIBLE') ELSE $
      button = Obj_New('ButtonWidget', self.contextMenu, Value='Background On', Name='BACKGROUND_VISIBLE')
   backgroundID = Obj_New('ButtonWidget', self.contextMenu, Value='Background Color...', Name='BACKGROUND_COLOR')
   IF ~background THEN backgroundID -> SetProperty, Sensitive=0
   button = Obj_New('ButtonWidget', self.contextMenu, Value='Other Properties...', Name='OTHER_PROPERTIES', UValue=drawID)
   button = Obj_New('ButtonWidget', self.contextMenu, Value='Delete', Name='DELETE_OBJECT', /Separator)

   Widget_DisplayContextMenu, drawID -> GetID(), x+10, y-5, self.contextMenu->GetID()

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::SETGROUP
;
; PURPOSE:
;
;       This method allows the user to set the group specified for this selectable object.
;       An object can belong to a single group.
;
; SYNTAX:
;
;       selectedObject = SetGroup, groupObject
;
; ARGUMENTS:
;
;       groupObject:    The SELECTABLEGROUP object to which this selectable object belongs.
;                       Set this to a null object (OBJ_NEW()) to remove the object from the group.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO SelectableObject::SetGroup, groupObject

   @cat_pro_error_handler

   self.mygroup = groupObject

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the SELECTABLEOBJECT object's properties. Be sure
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
;     BACKGROUND:     Set this keyword to draw a background for the text.
;
;     BG_COLOR:       The name of the background color.
;
;     BOUNDARY_BOX:   The current boundary box for the text. A 2x5 double array.
;
;     BRING_TO_FRONT: Set this keyword to move this selectable object to the front of all selectable objects.
;
;     COLOR:          Set this keyword to the name of a color for the text.
;
;     DELETE:         Set this keyword if you wish to delete the object.
;
;     MOVE_BACKWARD:  Set this keyword to move this selectable object to the back of all selectable objects.
;
;     MOVE_FORWARD:   Set this keyword to move this selectable object to the front of all selectable objects.
;
;     NOMESSAGE:      Set this keyword to suppress any messaging as a result of going through the
;                     SetProperty method. Messaging is essential for PropertySheet widget capability,
;                     but causes too many draw methods on occasion. This will prevent going through DRAW
;                     methods needlessly. Defined here for capatibility with other selectable objects.
;                     The value is NOT used!
;
;     NOREFRESH:      Set this keyword if immediate refreshing of the object on the display is not required.
;
;     SELECTABLE:     Set this keyword to 1 to make the object selectable, to 0 to make the object unselectable.
;
;     SEND_TO_BACK:   Set this keyword to move this selectable object to the back of all selectable objects.
;
;     SENDMESSAGE:    An output keyword that indicates if a message needs to be sent.
;
;     VISIBLE:        Set this keyword to 1 (the default) to see text. To 0 to turn text drawing off. An
;                     invisible object is not selectable, although the SELECTABLE flag is NOT set.
;
;     _EXTRA:         Any keywords appropriate for the superclass object.
;-
;*****************************************************************************************************
PRO SelectableObject::SetProperty, $
   BACKGROUND=background, $
   BG_COLOR=bg_color, $
   BRING_TO_FRONT=bring_to_front, $
   BOUNDARY_BOX=box, $
   COLOR=color, $
   DELETE=delete, $
   MOVE_BACKWARD=move_backward, $
   MOVE_FORWARD=move_forward, $
   NOMESSAGE=nomessage, $
   NOREFRESH=norefresh, $
   SELECTABLE=selectable, $
   SEND_TO_BACK=send_to_back, $
   SENDMESSAGE=sendMessage, $
   VISIBLE=visible, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(background) NE 0 THEN BEGIN
      sendMessage = 1
      self.background = background
      IF ~Keyword_Set(norefresh) THEN CatRefreshDraw, self, Stop_At='DRAWWIDGET'
   ENDIF
   IF N_Elements(bg_color) NE 0 THEN BEGIN
      sendMessage = 1
      self.bg_color = bg_color
      self -> SetPropertyAttribute, 'BG_COLOR', USERDEF=CapFirstLetter(self.bg_color)
      IF Obj_Valid(self._controlpanel) THEN self._controlpanel -> Refresh_Properties, Properties='BG_COLOR'
   ENDIF
   IF N_Elements(bring_to_front) NE 0 THEN BEGIN
      sendMessage = 1
      self -> GetProperty, Parent=parents

      ; Have to do the move for all parents.
      FOR j=0,N_Elements(parents)-1 DO BEGIN
         selectableObjects = parents[j] -> Get(/All, ISA='SELECTABLEOBJECT')
         children = parents[j] -> Get(/All)
         index = Where(children EQ self, count)
         last = Where(children EQ selectableObjects[N_Elements(selectableObjects)-1])
         parents[j] -> Move, index, last
      ENDFOR

      IF ~Keyword_Set(norefresh) THEN CatRefreshDraw, self, Stop_At='DRAWWIDGET'
   ENDIF
   IF N_Elements(box) NE 0 THEN self.box = box
   IF N_Elements(color) NE 0 THEN BEGIN
      sendMessage = 1
      self.color = color
      self -> SetPropertyAttribute, 'COLOR', USERDEF=CapFirstLetter(self.color)
      IF Obj_Valid(self._controlpanel) THEN self._controlpanel -> Refresh_Properties, Properties='COLOR'
   ENDIF
   IF Keyword_Set(delete) THEN BEGIN
      self -> SetProperty, Visible=0
      IF ~Keyword_Set(norefresh) THEN CatRefreshDraw, self, Stop_At='DRAWWIDGET'
      Obj_Destroy, self._controlPanel
      self -> GetProperty, Parent=parents
      self -> SendMessage, 'OBJECT_DELETED', DATA=self
      FOR j=0,N_Elements(parents)-1 DO parents[j] -> Remove, self
      Obj_Destroy, self
      RETURN
   ENDIF
   IF N_Elements(move_forward) NE 0 THEN BEGIN
      sendMessage = 1
      self -> GetProperty, Parent=parents

      ; Have to do the move for all parents.
      FOR j=0,N_Elements(parents)-1 DO BEGIN
         selectableObjects = parents[j] -> Get(/All, ISA='SELECTABLEOBJECT')
         children = parents[j] -> Get(/All)
         index = Where(children EQ self, count)
         last = Where(children EQ selectableObjects[N_Elements(selectableObjects)-1])
         parents[j] -> Move, index, (index + 1) < last
      ENDFOR

      IF ~Keyword_Set(norefresh) THEN CatRefreshDraw, self, Stop_At='DRAWWIDGET'
   ENDIF
   IF N_Elements(move_backward) NE 0 THEN BEGIN
      sendMessage = 1
      self -> GetProperty, Parent=parents

      ; Have to do the move for all parents.
      FOR j=0,N_Elements(parents)-1 DO BEGIN
         selectableObjects = parents[j] -> Get(/All, ISA='SELECTABLEOBJECT')
         children = parents[j] -> Get(/All)
         index = Where(children EQ self, count)
         first = Where(children EQ selectableObjects[0])
         parents[j] -> Move, index, (index - 1) > first
      ENDFOR

      IF ~Keyword_Set(norefresh) THEN CatRefreshDraw, self, Stop_At='DRAWWIDGET'
   ENDIF
   IF N_Elements(send_to_back) NE 0 THEN BEGIN
      sendMessage = 1
      self -> GetProperty, Parent=parents

      ; Have to do the move for all parents.
      FOR j=0,N_Elements(parents)-1 DO BEGIN
         selectableObjects = parents[j] -> Get(/All, ISA='SELECTABLEOBJECT')
         children = parents[j] -> Get(/All)
         index = Where(children EQ self, count)
         first = Where(children EQ selectableObjects[0])
         parents[j] -> Move, index, first
      ENDFOR

      IF ~Keyword_Set(norefresh) THEN CatRefreshDraw, self, Stop_At='DRAWWIDGET'
   ENDIF

   IF N_Elements(selectable) NE 0 THEN self.selectable = Keyword_Set(selectable)

   IF N_Elements(visible) NE 0 THEN BEGIN
      sendMessage = 1
      self.visible = visible
   ENDIF

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATDATAATOM::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::CLEANUP
;
; PURPOSE:
;
;       This is the SELECTABLEOBJECT object class destructor method.
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
PRO SelectableObject::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self.contextmenu
   self -> CATDATAATOM::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLEOBJECT::INIT
;
; PURPOSE:
;
;       This is the SELECTABLEOBJECT object class initialization method
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
;     BACKGROUND:     Set this keyword to draw a background for the text.
;
;     BG_COLOR:       The name of the background color. By default, "black".
;
;     COLOR:          Set this keyword to the name of a color for the text. By default, "white".
;
;     BOUNDARY_BOX:   The current boundary box for the text. A 2x5 double array.
;
;     SELECTABLE:     Set this flag to 1 to make the object selectable (set by default). Set to 0
;                     to make the object unselectable. In general, this keyword should not be used
;                     by any object *except* a subclassed object.
;
;     VISIBLE:        Set this keyword to 0 if you wish the object to be invisible. An
;                     invisible object is not selectable, although the SELECTABLE flag is NOT set.
;
;     _EXTRA:         Any keywords appropriate for the CATDATAATOM INIT method.
;-
;*****************************************************************************************************
FUNCTION SelectableObject::INIT, $
   BACKGROUND=background, $
   BG_COLOR=bg_color, $
   BOUNDARY_BOX=box, $
   COLOR=color, $
   SELECTABLE=selectable, $
   VISIBLE=visible, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   ok = self -> CATDATAATOM::INIT(_EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   background = Keyword_Set(background)
   IF N_Elements(bg_color) EQ 0 THEN bg_color = 'black'
   IF N_Elements(box) NE 0 THEN self.box = box
   IF N_Elements(color) EQ 0 THEN color = 'white'
   IF N_Elements(selectable) EQ 0 THEN selectable = 1
   IF N_Elements(visible) EQ 0 THEN visible = 1

   ; Load object.
   self.background = background
   self.bg_color = bg_color
   self.color = color

   self.selectable = selectable
   self.visible = visible

   currentVisible = self.visible
   self.visible = 0
   self -> RegisterProperty, 'DELETE', 1, USERDEF='Delete Object', NAME="Delete Object"
   self -> RegisterProperty, 'COLOR', 0, USERDEF=CapFirstLetter(self.color), NAME="Color (Foreground)"
   self.visible = currentVisible

   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       SELECTABLEOBJECT CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the SELECTABLEOBJECT object.
;
;*****************************************************************************************************
PRO SelectableObject__Define, class

   class = { SELECTABLEOBJECT, $
             background: 0L, $            ; A flag to indicate if a background box is drawn.
             bg_color: "", $              ; The name of the background color.
             box: DblArr(2,5), $          ; The box around the selectable object is ALWAYS in normal coordinates.
             color: "", $                 ; The name of a color to draw the object in.
             contextmenu: Obj_New(), $    ; A holder for a context menu object.
             mygroup: Obj_New(), $        ; A SELECTABLEGROUP object, of which this object is a member.
             selectable: 0L, $            ; A flag to indicate the object is selectable.
             visible: 0L, $               ; A flag to indicate if the object is visible (should be drawn).
             INHERITS CatDataAtom $
           }

END
