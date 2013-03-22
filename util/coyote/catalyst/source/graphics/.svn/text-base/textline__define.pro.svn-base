;*****************************************************************************************************
;+
; NAME:
;       TEXTLINE__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide a line of text that can
;       be displayed in a direct graphics draw widget. As a subclassed
;       CATDATAATOM, the text is the "data" and can be manipulated with
;       the GetData and SetData methods. The coordinate system of the
;       TextLine object is either passed to it (a CatCoord object) or is
;       a normalized coordinate system by default.
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
;       textObject = Obj_New("TEXTLINE", "This is a line of text")
;       drawObject -> Add, textObject
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
;   class = { TEXTLINE, $
;             alignment: 0.0, $           ; The alignment of the text.
;             background: 0L, $           ; A flag to indicate if a background box is drawn.
;             bg_color: "", $             ; The background color.
;             charsize: 0.0, $            ; The character size.
;             color: "", $                ; The name of a color to draw text in.
;             font: 0L, $                 ; The font type (0-Hershey, 1-Hardware, 2-True-Type).
;             orientation: 0.0, $         ; The orientation of the text
;             thickness: 0.0, $           ; The CHARTHICK ness of the text.
;             x: 0.0, $                   ; The X location for drawing text.
;             y: 0.0, $                   ; The Y location for drawing text.
;             width: 0.0, $               ; The width of the text line. (Set when drawn.)
;             INHERITS SelectableObject $
;           }
;;
; MESSAGES:
;
;   TEXTLINE_CHANGED:   This message is sent whenever SetProperty method is called and the NOMESSAGE
;                       keyword is NOT set.
;
; NOTES_AND_RESTRICTIONS:
;
;       If the font field is set to 0 (Hershey fonts) when drawn in PostScript output,
;       the font type is switched to 1 (Hardware fonts).
;
;       Rotation of text can only occur when the ALIGNMENT of the test is "centered". If you
;       try to rotate the text, the alignment is changed. Hardware fonts cannot be rotated on
;       the display.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 25 Jan 2004.
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
;       TEXTLINE::CALCULATEBOUNDARYBOX
;
; PURPOSE:
;
;       This method calculates the extent of a box about the text.
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
;       None.
;
;-
;*****************************************************************************************************
PRO TextLine::CalculateBoundaryBox

   @cat_pro_error_handler

   ; Create the internal data coordinate system.
   self -> ApplyCoords

   CASE self.alignment OF

      0: BEGIN
         x1 = self.x
         x2 = self.x + self.width
         END

      2: BEGIN
         x1 = self.x - self.width
         x2 = self.x
         END

      ELSE: BEGIN
         x1 = self.x - self.width/2.0
         x2 = self.x + self.width/2.0
         END

   ENDCASE

   ; Charsize does not apply for hardware fonts, except in devices that support scalable pixels.
   IF (self.font EQ 0) AND ((!D.Flags AND 1) EQ 0) THEN charsize = 1.0 ELSE charsize = self.charsize

   ; Calculate the coordinate of a text box.
   n = Convert_Coord([x1,x2], [self.y,self.y], /Data, /To_Normal)
   y1 = n[1,0] - (Round(!D.Y_CH_SIZE * charsize * 0.5) / Float(!D.Y_Size))
   y2 = n[1,0] + (self.charsize * !D.Y_CH_SIZE / !D.Y_Size)

   ; Add a bit of padding.
   c = Convert_Coord([12,12], [10,17], /Device, /To_Normal)
   xskosh =  c[0, 0]
   yskosh1 = c[1, 0]
   yskosh2 = c[1,1]

   ; Box laid out normally.
   CASE self.alignment OF

      0: BEGIN ; Left aligned.
         self.box[0,*] = [n[0,0]-xskosh, n[0,0]-xskosh, n[0,1], n[0,1], n[0,0]-xskosh]
         self.box[1,*] = [y1-yskosh1, y2+yskosh2, y2+yskosh2, y1-yskosh1, y1-yskosh1]
         END

      1: BEGIN ; Center aligned.
         self.box[0,*] = [n[0,0]-xskosh, n[0,0]-xskosh, n[0,1]+xskosh, n[0,1]+xskosh, n[0,0]-xskosh]
         self.box[1,*] = [y1-yskosh1, y2+yskosh2, y2+yskosh2, y1-yskosh1, y1-yskosh1]
         END

      2: BEGIN ; Right aligned.
         self.box[0,*] = [n[0,0], n[0,0], n[0,1]+xskosh, n[0,1]+xskosh, n[0,0]]
         self.box[1,*] = [y1-yskosh1, y2+yskosh2, y2+yskosh2, y1-yskosh1, y1-yskosh1]
         END

   ENDCASE

   ; Rotate the box, if required.
   IF self.orientation NE 0.0 THEN BEGIN

      ; Convert box coordinates to pixel values for rotation so that size
      ; is invariant with respect to window size.
      c = Convert_Coord([self.box[0,0],self.box[0,2]], [self.box[1,0], self.box[1,1]], $
         /Normal, /To_Device)

      ; Find the corners and midpoint of the box.
      x1 = c[0,0]
      x2 = c[0,1]
      y1 = c[1,0]
      y2 = c[1,1]
      midx = (x2 - x1)/2.0 + x1
      midy = (y2 - y1)/2.0 + y1

      CASE self.alignment OF
         0: BEGIN ; Left aligned.
            T3D, /Reset, Translate=[-x1, -y2, 0]
            T3D, Rotate=[0, 0, self.orientation], Matrix=ctm
            p1 = Transpose([x1, y1, 0, 1])
            p2 = Transpose([x1, y2, 0, 1])
            p3 = Transpose([x2, y2, 0, 1])
            p4 = Transpose([x2, y1, 0, 1])
            v1 = ctm ## p1
            v2 = ctm ## p2
            v3 = ctm ## p3
            v4 = ctm ## p4

            ; Translate back to where you found the box.
            T3D, /Reset, Translate=[x1, y2, 0], Matrix=ctm
            v1 = ctm ## v1
            v2 = ctm ## v2
            v3 = ctm ## v3
            v4 = ctm ## v4
            END
         1: BEGIN ; Center aligned.
            ; Translate to origin and rotate about the Z axis.
            T3D, /Reset, Translate=[-midx, -midy, 0]
            T3D, Rotate=[0, 0, self.orientation], Matrix=ctm
            p1 = Transpose([x1, y1, 0, 1])
            p2 = Transpose([x1, y2, 0, 1])
            p3 = Transpose([x2, y2, 0, 1])
            p4 = Transpose([x2, y1, 0, 1])
            v1 = ctm ## p1
            v2 = ctm ## p2
            v3 = ctm ## p3
            v4 = ctm ## p4

            ; Translate back to where you found the box.
            T3D, /Reset, Translate=[midx, midy, 0], Matrix=ctm
            v1 = ctm ## v1
            v2 = ctm ## v2
            v3 = ctm ## v3
            v4 = ctm ## v4
            END

         2: BEGIN ; Right aligned
            T3D, /Reset, Translate=[-x2, -y1, 0]
            T3D, Rotate=[0, 0, self.orientation], Matrix=ctm
            p1 = Transpose([x1, y1, 0, 1])
            p2 = Transpose([x1, y2, 0, 1])
            p3 = Transpose([x2, y2, 0, 1])
            p4 = Transpose([x2, y1, 0, 1])
            v1 = ctm ## p1
            v2 = ctm ## p2
            v3 = ctm ## p3
            v4 = ctm ## p4

            ; Translate back to where you found the box.
            T3D, /Reset, Translate=[x2, y1, 0], Matrix=ctm
            v1 = ctm ## v1
            v2 = ctm ## v2
            v3 = ctm ## v3
            v4 = ctm ## v4
            END
      ENDCASE

      ; Convert the points back to Normalized coordinates.

      c = Convert_Coord([v1[0], v2[0], v3[0], v4[0]], $
                        [v1[1], v2[1], v3[1], v4[1]], /Device, /To_Normal)

      ; Set the rotated points.
      self.box[0,*] = [c[0,0], c[0,1], c[0,2], c[0,3], c[0,0]]
      self.box[1,*] = [c[1,0], c[1,1], c[1,2], c[1,3], c[1,0]]
   ENDIF

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TEXTLINE::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the TEXTLINE object.
;
; SYNTAX:
;
;       textObject -> ControlPanel, baseObject
;
; ARGUMENTS:
;
;       baseObject:    The object reference of a base widget for this control to
;                      be added to. If not supplied, the control panel will be in a
;                      self contained window.
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the CATCONTROLPANEL::INIT method.
;-
;*****************************************************************************************************
PRO TextLine::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Create a new control panel

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='TextLine Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, Name='TEXTLINE PROPERTYSHEET', YSize=14)
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TEXTLINE::CREATENEWOBJECT
;
; PURPOSE:
;
;       This method creates a new object and adds it to both a draw widget and pixmap container.
;       If the object contains an annotation layer, the new object is instead added to the layer,
;       and the layer is added to the draw widget and pixmap container.
;
; SYNTAX:
;
;       theObject -> CreateNewObject, drawID, pixmapID
;
; ARGUMENTS:
;
;       drawID:    The draw widget which will contain the new arrow object. Required in normal operation.
;
;       pixmapID:  The pixmap which will contain the new arrow object. Optional.
;
; KEYWORDS:
;
;       NEWOBJECT: An output keyword containing the new arrow object that gets created.
;
;-
;*****************************************************************************************************
PRO TextLine::CreateNewObject, drawID, pixmapID, NEWOBJECT=newObject

   @cat_pro_error_handler

   self -> GetProperty, $
      ALIGNMENT=alignment, $
      BACKGROUND=background, $
      BG_COLOR=bg_color, $
      CHARSIZE=charsize, $,
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      FONT=font, $
      ORIENTATION=orientation, $
      TEXT=text, $
      THICKNESS=thickness, $
      VISIBLE=visible, $
      X=x, $
      Y=y

   ; If this is a null text object, use the new text.
   IF text EQ "" THEN text = self.newText

   newObject = Obj_New('TEXTLINE', text, $
      ALIGNMENT=alignment, $
      BACKGROUND=background, $
      BG_COLOR=bg_color, $
      CHARSIZE=charsize, $,
      COLOR=color, $
      COORD_OBJECT=coord_object, $
      FONT=font, $
      ORIENTATION=orientation, $
      THICKNESS=thickness, $
      VISIBLE=visible, $
      X=x, $
      Y=y)

   ; Make sure you have at least one positional parameter to proceed.
   IF N_Params() LT 1 THEN RETURN

   ; If you have a layer object, add the newObject to it instead of directly
   ; to the draw widget and pixmap.
   IF Obj_Valid(self.layerObject) THEN BEGIN

      ; Add new object to the layer object.
      self.layerObject -> Add, newObject

      ; If the annotation object is visible, draw the new object.
      self.layerObject -> GetProperty, Visible=visible
      IF visible THEN BEGIN

         ; Draw the new object.
         newObject -> Draw

         ; Refresh the pixmap, or it will think it is ready to draw.
         IF Obj_Valid(pixmapID) THEN pixmapID -> Refresh

      ENDIF

   ENDIF ELSE BEGIN

      ; Add the object to the draw widget and pixmap.
      drawID -> Add, newObject
      IF Obj_Valid(pixmapID) THEN pixmapID -> Add, newObject

      ; Draw the new arrow.
      newObject -> Draw

   ENDELSE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TEXTLINE::DRAW
;
; PURPOSE:
;
;       This method draws the line of text in the current direct graphics display window.
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
PRO TextLine::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; If there is no text, then return immediately.
   IF Ptr_Valid(self._dataPtr) EQ 0 THEN RETURN
   IF *self._dataPtr EQ "" THEN RETURN

   ; If the text is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   ; Make sure you have a string and not something else.
   IF Size(*self._dataPtr, /TName) NE 'STRING' THEN $
      text = StrTrim(*self._dataPtr, 2) ELSE $
      text = *self._dataPtr

   ; Apply the coordinate system, if you have one.
   self -> ApplyCoords

   ; Always to font substitution for PostScript or Printer output.
   theFont = self.font
   IF (!D.Name EQ 'PS' OR !D.Name EQ 'PRINTER') AND (theFont EQ 0) THEN theFont = 1

   ; Output the string without actually drawing it.
   IF (!D.Name EQ 'PS' OR !D.Name EQ 'PRINTER') THEN BEGIN
       theWidth = self.width
   ENDIF ELSE BEGIN
      XYOuts, self.x, self.y, text, Alignment=self.alignment/2.0, Orientation=self.orientation, $
         Font=theFont-1, Charsize=-self.charsize, Color=FSC_Color(self.color), Width=theWidth, $
         CharThick=self.thickness
   ENDELSE

   ; Assign the width. Start by making it slightly larger than it is.
   theWidth = theWidth + 0.04
   self._coords -> GetProperty, XRANGE=xr
   self.width = theWidth * (xr[1] - xr[0])

   ; Calculate the text box.
   self -> CalculateBoundaryBox

   ;Draw the background if required.
   IF self.background THEN $
      PolyFill, self.box[0,*], self.box[1,*], Fill=1, Color=FSC_Color(self.bg_color), /Normal

   ; Apply the coordinate system, if you have one.
   self -> ApplyCoords

   ; Output the string.
   IF (!D.Name EQ 'PS' OR !D.Name EQ 'PRINTER') THEN BEGIN
      XYOuts, self.x, self.y, text, Alignment=self.alignment/2.0, Orientation=self.orientation, $
         Font=theFont-1, Charsize=self.charsize, Color=FSC_Color(self.color), CharThick=self.thickness
   ENDIF ELSE BEGIN
      XYOuts, self.x, self.y, text, Alignment=self.alignment/2.0, Orientation=self.orientation, $
         Font=theFont-1, Charsize=self.charsize, Color=FSC_Color(self.color), $
         CharThick=self.thickness
   ENDELSE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TEXTLINE::DRAWSELECTIONBOX
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
PRO TextLine::DrawSelectionBox, Color=color

   @cat_pro_error_handler

   ; If there is no text, then return immediately.
   IF *self._dataPtr EQ "" THEN RETURN

   ; If the text is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   IF N_Elements(color) EQ 0 THEN color = self.color

   self -> ApplyCoords
   self -> SELECTABLEOBJECT::DrawSelectionBox, Color=color

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       TEXTLINE::EVENTHANDLER
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
PRO TextLine::EventHandler, event

   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF


      'TEXTLINE PROPERTYSHEET': BEGIN

         IF event.type EQ 0 THEN BEGIN ; Button DOWN event.

            CASE StrUpCase(event.identifier) OF

               'COLOR': BEGIN

                  event.component -> GetProperty, Color=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, Color=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, REQUESTER=self

               ENDCASE

               'BG_COLOR': BEGIN

                  event.component -> GetProperty, BG_Color=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, BG_Color=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DRAWWIDGET', /NoErase, REQUESTER=self

                  END

               ELSE: BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value

                  ; Refresh the graphics hierarchy.
                  IF Obj_Valid(self) THEN CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, REQUESTER=self ELSE RETURN


               ENDCASE

            ENDCASE

         ENDIF ; of BUTTON DOWN EVENT

         ENDCASE ; of TEXTLINE PROPERYSHEET events

        ; If you can't handle the event here. Pass it along to superclass EventHandler
        ELSE: self -> SelectableObject::EventHandler, event

   ENDCASE

   IF Obj_Valid(self) THEN self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       TEXTLINE::GetData
;
; PURPOSE:
;
;       This function method overrides the CATDATAATOM function of the same name, only
;       so that a null result comes back with a null string rather than a -1.
;
; SYNTAX:
;
;       data = imageObject -> GetData (Success=s)
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       SUCCESS: This flag shows whether the get has been successful or not.
;
;-
;*****************************************************************************************************
FUNCTION TextLine::GetData, Success=success

   ; Default error handler
   @cat_func_error_handler

   ; Attempt to get data from the data pointer
   success = PTR_VALID (self._dataPtr)
   IF NOT success THEN result = "" $
   ELSE result = *self._dataPtr

   ; Report completion and return result
   self -> Report, /Completed
   RETURN, result

END


;*****************************************************************************************************
;+
; NAME:
;       TEXTLINE::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain TEXTLINE properties. Be sure
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
;     ALIGNMENT:   Set the keyword to 0.0 for left-aligned text, 0.5 (the default) for
;                  centered text, and 1.0 for right-aligned text.
;
;     CHARSIZE:    Set this keyword to the character size desired. By default, 1.0.
;
;     FONT:        Set this to the type of font desired. Hershey: 0, Hardware: 1 (default), True-Type: 2.
;
;     HEIGHT:      The height of the text boundary box in normalized coordinates.
;
;     LAYER:       The annotation layer associated with this object.
;
;     ORIENTATION: Set this keyword to the orientation of the font. By default, 0.0.
;
;     TEXT:        A single line of text for the TextLine object.
;
;     THICKNESS:   The current thickness of the text.
;
;     WIDTH:       The width of the text boundary box in normalized coordinates.
;
;     X:           The X location of the text for alignment purposes. By default, half the coordinate X range.
;
;     Y:           The Y location of the text for alignment purposes. By default, half the coordinate Y range.
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO TextLine::GetProperty, $
   ALIGNMENT=alignment, $
   CHARSIZE=charsize, $,
   FONT=font, $
   HEIGHT=height, $
   LAYER=layer, $
   ORIENTATION=orientation, $
   TEXT=text, $
   THICKNESS=thickness, $
   WIDTH=width, $
   X=x, $
   Y=y, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(alignment) THEN alignment = self.alignment
   IF Arg_Present(bring_to_front) THEN bring_to_front = 0
   IF Arg_Present(charsize) THEN charsize = self.charsize
   IF Arg_Present(font) THEN font = self.font
   IF Arg_Present(height) THEN BEGIN
      maxy = Max(self.box[1,*], Min=miny)
      height = maxy - miny
   ENDIF
   IF Arg_Present(layer) THEN layer = self.layerObject
   IF Arg_Present(orientation) THEN orientation = self.orientation
   IF Arg_Present(send_to_back) THEN send_to_back = 0
   IF Arg_Present(thickness) THEN thickness = self.thickness
   IF Arg_Present(text) THEN text = self -> GetData()
   IF Arg_Present(width) THEN BEGIN
      maxx = Max(self.box[0,*], Min=minx)
      width = maxx - minx
   ENDIF
   IF Arg_Present(x) THEN x = self.x
   IF Arg_Present(y) THEN y = self.y

   delete = 1 - self.visible

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       TEXTLINE::INTERACTIONEVENTS
;
; PURPOSE:
;
;       This method accepts events from the SelectableInteraction object based on the "mode"
;       of the SelectableInteraction object. The SelectableInteraction object can process "SELECT"
;       and "MOVE" mode events. All other mode events are sent here for processing by a SELECTABLEOBJECT.
;
;
; SYNTAX:
;
;       theObject -> INTERACTIONEVENTS
;
; ARGUMENTS:
;
;     event:          The widget event that is generated by the draw widget and handled by the SelectableInteraction
;                     interaction.
;
; KEYWORDS:
;
;     INTERACTION:    The object reference to a SELECTABLEINTERACTION object that is receiving events.
;                     This is a *required* parameter, but is written as a keyword for programming clarity.
;
;-
;*****************************************************************************************************
PRO TextLine::InteractionEvents, event, Interaction=interaction

   @cat_pro_error_handler

   ; Get information from the INTERACTION object.
   IF Obj_Valid(interaction) EQ 0 THEN Message, 'An "interaction" object is a required keyword parameter.'
   interaction -> GetProperty, MODE=mode, DRAWWIDGET=drawID, PIXMAP=pixmap

   CASE mode OF

      'INSERT': BEGIN

         ; Set yourself up to start writing text in WRITE  mode.
         self.sx = event.x
         self.sy = event.y
         self.newText = "|"
         drawID -> SetProperty, Keyboard_Events=1
         interaction -> SetProperty, Mode='WRITE'
         self -> ApplyCoords
         c = Convert_Coord(event.x, event.y, /Device, /To_Data)
         self.x = c[0]
         self.y = c[1]

         ; Change cursor shape.
         IF !D.Name EQ 'WIN' THEN BEGIN
            Device, CURSOR_STANDARD=32512L
         ENDIF ELSE BEGIN
            Device, CURSOR_STANDARD=2
         ENDELSE

         ; Update the pixmap
         drawID -> GetProperty, XSize=xsize, YSize=ysize
         pixmap -> GetProperty, XSize=pxsize, YSize=pysize
         IF (pxsize NE xsize) OR (pysize NE ysize) THEN BEGIN
            pixmap -> SetProperty, XSize=xsize, YSize=ysize
         ENDIF

         ; Copy window contents to pixmap.
         pixmap -> SetWindow
         drawID -> Copy
         drawID -> SetWindow

         ; Get the default properties for writing the text.
         self -> GetProperty, $
            ALIGNMENT=alignment, $
            BACKGROUND=background, $
            BG_COLOR=bg_color, $
            CHARSIZE=charsize, $,
            COLOR=color, $
            COORD_OBJECT=coord_object, $
            FONT=font, $
            ORIENTATION=orientation, $
            THICKNESS=thickness

         XYOUTS, self.sx, self.sy, self.newText, $
            ALIGNMENT=alignment/2.0, $
            CHARSIZE=charsize, $,
            COLOR=FSC_Color(color), $
            FONT=font-1, $
            ORIENTATION=orientation, $
            CHARTHICK=thickness, $
            /DEVICE


      END ; of INSERT mode

      'WRITE': BEGIN

            ; What kind of event is this?
            eventTypes = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSED', 'CHARACTER', 'KEYPRESS']
            thisEvent = eventTypes[event.type]

            CASE thisEvent OF

              'CHARACTER': BEGIN

                  ; If this is not a press event, ignore it
                  IF (event.press) EQ 0 THEN RETURN

                  ; Get the default properties for writing the text.
                  self -> GetProperty, $
                     ALIGNMENT=alignment, $
                     BACKGROUND=background, $
                     BG_COLOR=bg_color, $
                     CHARSIZE=charsize, $,
                     COLOR=color, $
                     COORD_OBJECT=coord_object, $
                     FONT=font, $
                     ORIENTATION=orientation, $
                     THICKNESS=thickness

                  ; If this is a valid key click, write the text
                  IF (self.validchars [event.ch]) THEN BEGIN

                     CASE event.ch OF

                        ; Is this a Carriage Return? If so, process the text.
                        13: BEGIN

                           ; Remove insertion mark.
                           self.newText = StrMid(self.newText, 0, StrLen(self.newText)-1)

                           ; Start with a blank slate.
                           drawID -> SetWindow
                           pixmap -> Copy

                           ; Create a text object.
                           self -> CreateNewObject, drawID, pixmap
                           interaction -> SetProperty, Mode='SELECT'

                           ; Change cursor shape.
                           IF !D.Name EQ 'WIN' THEN BEGIN
                              Device, CURSOR_STANDARD=2
                           ENDIF ELSE BEGIN
                              Device, CURSOR_STANDARD=32512L
                           ENDELSE

                           ; Erase the previous text and write the new text.
                           drawID -> SetWindow
                           self -> CalculateBoundaryBox
                           self -> CopyParameters, drawID, Destination=d, Extent=e
                           pixmap -> Copy, Destination=d, Extent=e,  Origin=d

                           ; Don't want to draw with XYOUTS any longer, so exit.
                           RETURN
                           END

                        ; Is this a backspace?
                        8: BEGIN

                           IF (STRMID (self.newText, 0, /Reverse)) EQ '!' THEN rem = 3 ELSE rem = 2
                           self.newText = STRMID (self.newText, 0, STRLEN (self.newText) - rem) + "|"

                           END

                        ELSE: BEGIN

                           ; Special processing of exclamation marks in Hershey fonts.
                           IF (event.ch EQ 33) THEN self.newText = StrMid(self.newText,0, StrLen(self.newText)-1) $
                              + '!' + StrMid(self.newText,StrLen(self.newText)-1,1)

                           self.newText = StrMid(self.newText,0, StrLen(self.newText)-1) + String(event.ch) + $
                              StrMid(self.newText,StrLen(self.newText)-1,1)

                           END

                     ENDCASE

                     ; Erase the previous text and write the new text.
                     drawID -> SetWindow
                     pixmap -> Copy

                     ; Get the default properties for writing the text.
                     self -> GetProperty, $
                        ALIGNMENT=alignment, $
                        BACKGROUND=background, $
                        BG_COLOR=bg_color, $
                        CHARSIZE=charsize, $,
                        COLOR=color, $
                        COORD_OBJECT=coord_object, $
                        FONT=font, $
                        ORIENTATION=orientation, $
                        THICKNESS=thickness

                     XYOUTS, self.sx, self.sy, self.newText, $
                        ALIGNMENT=alignment/2.0, $
                        CHARSIZE=charsize, $,
                        COLOR=FSC_Color(color), $
                        FONT=font-1, $
                        ORIENTATION=orientation, $
                        CHARTHICK=thickness, $
                        /DEVICE

                 ENDIF

                 END

              'DOWN': BEGIN

                 drawID -> SetWindow
                 pixmap -> Copy
                 interaction -> SetProperty, Mode='SELECT'
                 drawID -> SetProperty, Keyboard_Events=0

                 END

              ELSE:

            ENDCASE ; of thisEvent in WRITE


      END

     'SELECT': ; Nothing to do.


  ENDCASE

   self -> Report, /Completed


END



;*****************************************************************************************************
;+
; NAME:
;       TEXTLINE::MOVE
;
; PURPOSE:
;
;       This method moves the text in a graphics window.
;
; SYNTAX:
;
;       theObject -> Move, x, y
;
; ARGUMENTS:
;
;       X:          The number of pixels to move in the X direction.
;
;       Y:          The number of pixels to move in the Y direction.
;
; KEYWORDS:
;
;       NODRAW:     If this keyword is set, only the coordinates are updated. No drawing occurs.
;
;       PIXMAP:     Set this keyword to a pixmap that can be used to erase the previous
;                   contents of the window.
;-
;*****************************************************************************************************
PRO TextLine::Move, x, y, PIXMAP=pixmap, NODRAW=noDraw

   @cat_pro_error_handler

   ; Convert the device pixels into data coordinates.
   self -> ApplyCoords
   self._coords -> GetProperty, XRANGE=xr, YRANGE=yr, POSITION=pos
   xx = Abs(xr[1] - xr[0]) / (!D.X_Size * Abs(pos[2]-pos[0])) * x
   yy = Abs(yr[1] - yr[0]) / (!D.Y_Size * Abs(pos[3]-pos[1])) * y
   self.x = self.x + xx
   self.y = self.y + yy
   IF Obj_Valid(self._controlpanel) THEN self._controlpanel -> Refresh_Properties, Properties=['X','Y']

   ; Do you need to draw?
   IF ~Keyword_Set(nodraw) THEN BEGIN
      IF Obj_Isa_Valid(pixmap, 'PIXMAPWIDGET') THEN BEGIN
         self -> CopyParameters, pixmap, Destination=d, Extent=e
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
;       TEXTLINE::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the TEXTLINE object's properties. Be sure
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
;     ALIGNMENT:      Set the keyword to 0.0 for left-aligned text, 1.0 (the default) for
;                     centered text, and 2.0 for right-aligned text.
;
;     CHARSIZE:       Set this keyword to the character size desired.
;
;     FONT:           Set this to the type of font desired. Hershey: 0, Hardware: 1 (default), True-Type: 2.
;
;     LAYER:          The annotation layer for the object.
;
;     NOMESSAGE:      Set this keyword to suppress any messaging as a result of going through the
;                     SetProperty method. Messaging is essential for PropertySheet widget capability,
;                     but causes too many draw methods on occasion. This will prevent going through DRAW
;                     methods needlessly.
;
;     NOREFRESH:      Set this keyword if immediate refreshing of the object on the display is not required.
;
;     ORIENTATION:    Set this keyword to the orientation of the font.
;
;     TEXT:           A single line of text for the TextLine object.
;
;     THICKNESS:      Set this to the thickness of the output.
;
;     X:              The X location of the text for alignment purposes.
;
;     Y:              The Y location of the text for alignment purposes.
;-
;*****************************************************************************************************
PRO TextLine::SetProperty, $
   ALIGNMENT=alignment, $
   CHARSIZE=charsize, $,
   DRAW=draw, $
   FONT=font, $
   LAYER=layer, $
   NOMESSAGE=nomessage, $
   NOREFRESH=norefresh, $
   ORIENTATION=orientation, $
   TEXT=text, $
   THICKNESS=thickness, $
   X=x, $
   Y=y, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   sendMessage = 0

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> SELECTABLEOBJECT::SetProperty, NOREFRESH=norefresh, SENDMESSAGE=sendMessage,_EXTRA=extraKeywords

   ; The object could have been deleted. If so, RETURN.
   IF ~Obj_Valid(self) THEN RETURN

   IF N_Elements(alignment) NE 0 THEN BEGIN
      sendMessage = 1
      self.alignment = alignment
   ENDIF
   IF N_Elements(charsize) NE 0 THEN BEGIN
      sendMessage = 1
      self.charsize = charsize
   ENDIF
   IF N_Elements(orientation) NE 0 THEN BEGIN
      sendMessage = 1
      self.orientation = orientation
      IF self.orientation NE 0 THEN BEGIN
         self.alignment = 1.0
         self -> SetPropertyAttribute, 'ALIGNMENT', Sensitive=0
         IF Obj_Valid(self._controlpanel) THEN BEGIN
            self._controlpanel -> Refresh_Properties, Properties=['ALIGNMENT', 'X', 'Y']
         ENDIF
      ENDIF ELSE BEGIN
         self -> SetPropertyAttribute, 'ALIGNMENT', Sensitive=1
         IF Obj_Valid(self._controlpanel) THEN $
            self._controlpanel -> Refresh_Properties, Properties='ALIGNMENT'
      ENDELSE
   ENDIF
   IF N_Elements(font) NE 0 THEN BEGIN
      sendMessage = 1
      self.font = font
      IF self.font EQ 1 THEN BEGIN
         self -> SetPropertyAttribute, 'ORIENTATION', Sensitive=0
         self -> SetPropertyAttribute, 'ALIGNMENT', Sensitive=1
         self -> SetPropertyAttribute, 'CHARSIZE', Sensitive=0
         self.orientation = 0.0
         self.charsize = 1.0
         IF Obj_Valid(self._controlpanel) THEN $
            self._controlpanel -> Refresh_Properties, Properties=['ALIGNMENT', 'ORIENTATION', 'CHARSIZE']
      ENDIF ELSE BEGIN
         IF self.orientation EQ 0.0 THEN BEGIN
            self -> SetPropertyAttribute, 'ALIGNMENT', Sensitive=1
            IF Obj_Valid(self._controlpanel) THEN $
               self._controlpanel -> Refresh_Properties, Properties=['ALIGNMENT']
         ENDIF
         self -> SetPropertyAttribute, 'ORIENTATION', Sensitive=1
         self -> SetPropertyAttribute, 'CHARSIZE', Sensitive=1
         IF Obj_Valid(self._controlpanel) THEN $
            self._controlpanel -> Refresh_Properties, Properties=['ORIENTATION', 'CHARSIZE']
      ENDELSE
      IF (self.font EQ 2) OR (self.font EQ 1) THEN BEGIN
         self -> SetPropertyAttribute, 'THICKNESS', Sensitive=0
      ENDIF ELSE BEGIN
         self -> SetPropertyAttribute, 'THICKNESS', Sensitive=1
      ENDELSE
      IF Obj_Valid(self._controlpanel) THEN $
         self._controlpanel -> Refresh_Properties, Properties=['THICKNESS']
      IF (self.font EQ 0) OR (self.font EQ 2) THEN BEGIN
         self -> SetPropertyAttribute, 'CHARSIZE', Sensitive=1
      IF Obj_Valid(self._controlpanel) THEN $
         self._controlpanel -> Refresh_Properties, Properties=['CHARSIZE']
      ENDIF
      IF Obj_Valid(self._controlpanel) THEN self._controlpanel -> Refresh_Properties, Properties=['ORIENTATION', 'THICKNESS']
   ENDIF
   IF N_Elements(layer) NE 0 THEN BEGIN
      IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self
      self.layerObject = layer
   ENDIF
   IF N_Elements(thickness) NE 0 THEN BEGIN
      sendMessage = 1
      self.thickness = thickness
   ENDIF
   IF N_Elements(x) NE 0 THEN BEGIN
      sendMessage = 1
      self.x = x
   ENDIF
   IF N_Elements(y) NE 0 THEN BEGIN
      sendMessage = 1
      self.y = y
   ENDIF
   IF N_Elements(text) NE 0 THEN BEGIN
      sendMessage = 1
      self -> SetData, text
      IF ~Keyword_Set(norefresh) THEN BEGIN
         CatRefreshDraw, self, Stop_At='DRAWWIDGET'
      ENDIF
   ENDIF

   ; The object could have been deleted.
   IF ~Obj_Valid(self) THEN RETURN

   ; Send message?
   IF sendMessage AND ~Keyword_Set(nomessage) THEN self -> SendMessage, 'TEXTLINE_CHANGED'

   ; Need to draw?
   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       TEXTLINE::CLEANUP
;
; PURPOSE:
;
;       This is the TEXTLINE object class destructor method.
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
PRO TextLine::CLEANUP

   @cat_pro_error_handler

   IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self

   self -> SELECTABLEOBJECT::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       TEXTLINE::INIT
;
; PURPOSE:
;
;       This is the TEXTLINE object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     text:        The single line of text to display.
;
; KEYWORDS:
;
;     ALIGNMENT:    Set the keyword to 0.0 for left-aligned text, 1.0 (the default) for
;                   centered text, and 2.0 for right-aligned text.
;
;     CHARSIZE:     Set this keyword to the character size desired. By default, 1.0.
;
;     FONT:         Set this to the type of font desired. Hershey: 0, Hardware: 1 (default), True-Type: 2.
;
;     LAYER:        A CATLAYER object for holding annotations.
;
;     ORIENTATION:  Set this keyword to the orientation of the font. By default, 0.0.
;
;     THICKNESS:    Set this to the thickness of the output. By default, 1.0.
;
;     X:            The X location of the text for alignment purposes. By default, half the coordinate X range.
;
;     Y:            The Y location of the text for alignment purposes. By default, half the coordinate Y range.
;
;     _EXTRA:       Any keywords appropriate for the SELECTABLEOBJECT INIT method.
;-
;*****************************************************************************************************
FUNCTION TextLine::INIT, text, $
   ALIGNMENT=alignment, $
   CHARSIZE=charsize, $,
   FONT=font, $
   LAYER=layer, $
   ORIENTATION=orientation, $
   THICKNESS=thickness, $
   X=x, $
   Y=y, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   ok = self -> SELECTABLEOBJECT::INIT (DESCRIPTION='TextLine Properties', _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0
   IF N_Elements(text) NE 0 THEN self -> SetData, text

   ; Check keywords.
   IF N_Elements(alignment) EQ 0 THEN alignment = 1.0
   background = Keyword_Set(background)
   IF N_Elements(bg_color) EQ 0 THEN bg_color = 'white'
   IF N_Elements(charsize) EQ 0 THEN charsize = 1.0
   IF N_Elements(color) EQ 0 THEN color = 'black'
   IF N_Elements(font) EQ 0 THEN font = 1.0
   IF Obj_Isa_Valid(layer, "CATLAYER") NE 0 THEN BEGIN
      self.layerObject = layer
      self.layerObject -> AddParent, self
   ENDIF
   IF N_Elements(orientation) EQ 0 THEN orientation = 0.0
   IF N_Elements(thickness) EQ 0 THEN thickness = 1.0

   ; If a coordinate object wasn't provided, try to copy one from the
   ; parent. If this isn't available, create a normalized one.
   IF Obj_Valid(self._coords) EQ 0 THEN BEGIN
      self -> GetProperty, First_Parent=parent
      IF Obj_Valid(parent) THEN parent -> GetProperty, Coord_Object=coords
      IF Obj_Valid(coords) THEN self._coords = coords ELSE $
         self._coords = Obj_New('CatCoord', XRange=[0,1], YRange=[0,1], Position=[0,0,1,1])
   ENDIF

   IF N_Elements(x) EQ 0 THEN x = 0.5
   IF N_Elements(y) EQ 0 THEN y = 0.5

   ; Set up valid character array.
   validchars = BYTARR (128)
   validchars [ 8 ]       = 1B ; Backspace
   validchars [ 13 ]      = 1B ; Carriage Return
   validchars [ 32 :  47] = 1B ; various chars
   validchars [ 48 :  57] = 1B ; 0 - 9
   validchars [ 58 :  64] = 1B ; Std keyboard characters.
   validchars [ 65 :  90] = 1B ; A - Z
   validchars [ 91 :  96] = 1B ; Std keyboard characters.
   validchars [ 97 : 122] = 1B ; a - z
   validchars [123 : 126] = 1B ; Std keyboard characters.
   self.validchars = validchars

   ; Load object.
   self.alignment = alignment
   self.charsize = charsize
   self.font = font
   self.orientation = orientation
   self.thickness = thickness
   self.x = x
   self.y = y

   ; Register properties for the property sheet. Turn visibility off, since some properties
   ; cause the object to refresh and draw prematurely.
   currentVisible = self.visible
   self.visible = 0
   self -> RegisterProperty, 'BG_COLOR', 0, USERDEF=CapFirstLetter(self.bg_color), NAME="Color (Background)"
   self -> RegisterProperty, 'BACKGROUND', 1, NAME="Background On"
   self -> RegisterProperty, 'ALIGNMENT', 9, NAME="Alignment", ENUMLIST=['Left', 'Center', 'Right']
   self -> SetPropertyByIdentifier, 'ALIGNMENT', self.alignment
   self -> RegisterProperty, 'CHARSIZE', 3, NAME="Character Size", VALID_RANGE=[0.1, 10.0]
   self -> SetPropertyByIdentifier, 'BACKGROUND', self.background
   self -> RegisterProperty, 'FONT', 9, NAME="Font Type", ENUMLIST=['Hershey', 'Hardware', 'True-Type']
   self -> RegisterProperty, 'ORIENTATION', 3, NAME="Orientation", VALID_RANGE=[0.0,360.0]
   self -> RegisterProperty, 'TEXT', 4, NAME="Text"
   self -> RegisterProperty, 'THICKNESS', 2, NAME="Thickness", VALID_RANGE=[1, 10]
   self -> RegisterProperty, 'X', 3, NAME="X Location"
   self -> RegisterProperty, 'Y', 3, NAME="Y Location"

   self -> SetPropertyByIdentifier, 'FONT', self.font
   IF self.font EQ 1 THEN self -> SetPropertyAttribute, "ORIENTATION", Sensitive=0

   self.visible = currentVisible

   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       TEXTLINE CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the TEXTLINE object.
;
;*****************************************************************************************************
PRO TextLine__Define, class

   class = { TEXTLINE, $
             alignment: 0.0, $           ; The alignment of the text.
             charsize: 0.0, $            ; The character size.
             font: 0L, $                 ; The font type (0-Hershey, 1-Hardware, 2-True-Type).
             layerObject: Obj_New(), $   ; A CATLAYER object for holding the annotation.
             newText: "", $              ; The new text a user is writing.
             orientation: 0.0, $         ; The orientation of the text
             thickness: 0.0, $           ; The CHARTHICK ness of the text.
             x: 0.0, $                   ; The X location for drawing text.
             y: 0.0, $                   ; The Y location for drawing text.
             sx: 0L, $                   ; The starting X coordinate (device) of text writing.
             sy: 0L, $                   ; The starting Y coordinate (device) of text writing.
             width: 0.0, $               ; The width of the text line. (Set when drawn.)
             validchars: BytArr(128), $  ; An array that indicates valid characters for writing.
             INHERITS SelectableObject $
           }

END
