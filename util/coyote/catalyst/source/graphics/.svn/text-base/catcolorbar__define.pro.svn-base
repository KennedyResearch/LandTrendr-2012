;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR
;
; PURPOSE:
;
;       The purpose of this routine is to implement a colorbar object as a selectable object.
;       The colors for the color bar can be passed into the program as a COLORTOOL object
;       with the COLOR_OBJECT keyword. A gray-scale color table with 256 colors is loaded
;       by default.
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
;       newObject = Obj_New("CATCOLORBAR", ...)
;       drawObject -> Add, newObject
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
;   class = { CATCOLORBAR, $
;             bar: Obj_New(), $            ; The colorbar image.
;             charsize: 0.0, $             ; The character size of the annotation.
;             divisions: 0L, $             ; The number of major tick marks.
;             extra: Ptr_New(), $          ; Placeholder for extra keywords.
;             font: 0L, $                  ; The font to use for annotations.
;             format: "", $                ; The annotation formating.
;             layerObject: Obj_New(), $    ; An optional CATLAYER object for holding the inserted selectable object.
;             minor: 0L, $                 ; The number of minor tick marks.
;             ncolors: 0L, $               ; The number of colors in the colorbar.
;             range: FltArr(2), $          ; The range of values displayed on the colorbar.
;             right: 0L, $                 ; The flag for text on RIGHT of vertical colorbars.
;             thickness: 0.0, $            ; The thickness of the selectable object.
;             ticklen: 0.0, $              ; The length of the tick marks.
;             tickv: Ptr_New(), $          ; Values for the tick marks.
;             title: "", $                 ; The title string for the colorbar.
;             top: 0L, $                   ; The flag for text on TOP of horizontal colorbars.
;             vertical: 0L, $              ; The flag for a vertical colorbar.
;             INHERITS SELECTABLEOBJECT $
;           }

;
; MESSAGES:
;
;   CATCOLORBAR_CHANGED:   This message is sent whenever SetProperty method is called and the NOMESSAGE keyword
;                          is NOT set.
;
; MODIFICATION_HISTORY:
;
;    Written by: David W. Fanning, April 17, 2005.
;    Added automatic True-Type fonts for PostScript output, if Hershey fonts would otherwise
;         be selected. 1 August 2006. DWF.
;    Added MATCHPARENT keyword to INIT method. 2 August 2006. DWF.
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
;       CATCOLORBAR::CALCULATEBOUNDARYBOX
;
; PURPOSE:
;
;       This method is here for completeness. The boundary box is actually
;       calculated at the end of the DRAW method.
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
PRO CatColorBar::CalculateBoundaryBox
END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the selectable object.
;
; SYNTAX:
;
;       boxObject -> ControlPanel, baseObject
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
PRO CatColorBar::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Create a new control panel

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Colorbar Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, Name='CATCOLORBAR PROPERTYSHEET', YSize=20)
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::CREATENEWOBJECT
;
; PURPOSE:
;
;       This method creates a new object and adds it to both a draw widget and pixmap container.
;       If the object contains a layer object, the new object is instead added to the layer,
;       and the layer is added to the draw widget and pixmap containers.
;
; SYNTAX:
;
;       theObject -> CreateNewObject, drawID
;
; ARGUMENTS:
;
;       drawID:    The draw widget which will contain the newly created object. Required in normal operation.
;
;       pixmapID:  The pixmap which will contain the newly created object. Optional.
;
;
; KEYWORDS:
;
;       NEWOBJECT: An output keyword containing the new box object that gets created. If the
;                  method is called with just this keyword and no other arguments, the method
;                  functions as a object COPY.
;
;-
;*****************************************************************************************************
PRO CatColorBar::CreateNewObject, drawID, pixmapID, NEWOBJECT=newObject

   @cat_pro_error_handler

   ;self -> GetProperty, .... ; Whatever properties are required.

   ;newObject = Obj_New('CATCOLORBAR',  ) ; Create new object.

   ; Make sure you have at least one positional parameter to proceed.
   IF N_Params() LT 1 THEN RETURN

   ; If you have a layer object, add the newObject to it instead of directly
   ; to the draw widget and pixmap.
   IF Obj_Valid(self.layerObject) THEN BEGIN

      ; Add new object to the layer object.
      self.layerObject -> Add, newObject
      self.insertedObject = newObject

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
      IF Obj_Valid(drawID) THEN drawID -> Add, newObject
      IF Obj_Valid(pixmapID) THEN pixmapID -> Add, newObject
      self.insertedObject = newObject

      ; Draw the new object.
      newObject -> Draw

   ENDELSE


   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::DRAW
;
; PURPOSE:
;
;       This method draws the box in the current direct graphics display window.
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
PRO CatColorBar::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; If the box is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   ; Draw the background if required.
   IF self.background THEN BEGIN
      PolyFill, self.box, Fill=1, Color=FSC_Color(self.bg_color), /Normal
   ENDIF

   IF self.matchparent THEN BEGIN
      CASE 1 OF
         Keyword_Set(self.vertical): BEGIN
            self -> GetProperty, First_Parent=parent
            IF Obj_ISA_Valid(parent, 'CATIMAGE') THEN BEGIN
               parent -> GetProperty, Location=ipos
               self -> GetProperty, Position=p
               p[1] = ipos[1,1]
               p[3] = ipos[3,1]
               self -> SetProperty, Position=p
            ENDIF
            END
         ELSE:  BEGIN
            self -> GetProperty, First_Parent=parent
            IF Obj_ISA_Valid(parent, 'CATIMAGE') THEN BEGIN
               parent -> GetProperty, Location=ipos
               self -> GetProperty, Position=p
               p[0] = ipos[0,1]
               p[2] = ipos[2,1]
               self -> SetProperty, Position=p
            ENDIF
            END
      ENDCASE
   ENDIF

   ; Draw the object here.
   self.bar -> Draw
   self.bar -> GetProperty, Position=position

   ; Would prefer True-Type fonts in PostScript files.
   IF self.font EQ 0 AND !D.Name EQ 'PS' THEN font = 2 ELSE font = self.font

   ; Annotate the color bar.
   IF KEYWORD_SET(self.vertical) THEN BEGIN

      IF KEYWORD_SET(self.right) THEN BEGIN


         PLOT, self.range, /NODATA, XTICKS=1, $
            YTICKS=self.divisions, XSTYLE=1, YSTYLE=9, $
            POSITION=position, COLOR=FSC_Color(self.color), CHARSIZE=self.charsize, /NOERASE, $
            YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', YTICKLEN=self.ticklen, XTHICK=self.thickness, $
            YRANGE=self.range, FONT=font-1, YMINOR=self.minor, _EXTRA=*self.extra, YTHICK=self.thickness

         AXIS, YAXIS=1, YRANGE=self.range, YTICKFORMAT=self.format, YTICKS=self.divisions, YTHICK=self.thickness, $
            YTICKLEN=self.ticklen, YSTYLE=1, COLOR=FSC_Color(self.color), CHARSIZE=self.charsize, $
            FONT=font-1, YMINOR=self.minor, YTICKNAME=*self.tickv, YTITLE=self.title, _EXTRA=*self.extra

      ENDIF ELSE BEGIN

         PLOT, self.range, /NODATA, XTICKS=1, $
            YTICKS=self.divisions, XSTYLE=1, YSTYLE=9, YMINOR=self.minor, $
            POSITION=position, COLOR=FSC_Color(self.color), CHARSIZE=self.charsize, /NOERASE, $
            YTICKFORMAT=self.format, XTICKFORMAT='(A1)', YTICKLEN=self.ticklen , $
            YRANGE=self.range, FONT=font-1, YTITLE=self.title, _EXTRA=*self.extra, $
            YTICKNAME=*self.tickv, YTHICK=self.thickness, XTHICK=self.thickness

         AXIS, YAXIS=1, YRANGE=self.range, YTICKFORMAT='(A1)', YTICKS=self.divisions, $
            YTICKLEN=self.ticklen, YSTYLE=1, COLOR=FSC_Color(self.color), CHARSIZE=self.charsize, $
            FONT=font-1, YMINOR=self.minor, YTHICK=self.thickness

      ENDELSE

   ENDIF ELSE BEGIN

      IF KEYWORD_SET(self.top) THEN BEGIN

         PLOT, self.range, /NODATA, XTICKS=self.divisions, $
            YTICKS=1, XSTYLE=9, YSTYLE=1, XTHICK=self.thickness, YTHICK=self.thickness, $
            POSITION=position, COLOR=FSC_Color(self.color), CHARSIZE=self.charsize, /NOERASE, $
            YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', XTICKLEN=self.ticklen, $
            XRANGE=self.range, FONT=font-1, XMINOR=self.minor

         AXIS, XTICKS=self.divisions, XSTYLE=1, COLOR=FSC_Color(self.color), CHARSIZE=self.charsize, $
            XTICKFORMAT=self.format, XTICKLEN=self.ticklen, XRANGE=self.range, XAXIS=1, $
            FONT=font-1, XCHARSIZE=self.charsize, XMINOR=self.minor, XTHICK=self.thickness, $
            XTICKNAME=*self.tickv, _EXTRA=*self.extra, XTITLE=self.title

      ENDIF ELSE BEGIN

         PLOT, self.range, /NODATA, XTICKS=self.divisions, $
            YTICKS=1, XSTYLE=1, YSTYLE=1, XTITLE=self.title, XTHICK=self.thickness, YTHICK=self.thickness, $
            POSITION=position, COLOR=FSC_Color(self.color), CHARSIZE=self.charsize, /NOERASE, $
            YTICKFORMAT='(A1)', XTICKFORMAT=self.format, XTICKLEN=self.ticklen, $
            XRANGE=self.range, FONT=font-1, XMinor=self.minor, $
            XTICKNAME=*self.tickv, _EXTRA=*self.extra

       ENDELSE

   ENDELSE

   ; Update boundary box for the colorbar.
   self.box[0,*] = [!X.Region[0], !X.Region[0], !X.Region[1], !X.Region[1], !X.Region[0]]
   self.box[1,*] = [!Y.Region[0], !Y.Region[1], !Y.Region[1], !Y.Region[0], !Y.Region[0]]

   ; Region doesn't get it right if you are vertical and right.
   IF self.vertical AND self.right THEN self.box[0,*] = self.box[0,*] + (0.075*self.charsize)

   ; Region doesn't get it right if you are horizontal and top.
   IF ~self.vertical AND self.top THEN self.box[1,*] = self.box[1,*] + (0.025*self.charsize)

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::DRAWSELECTIONBOX
;
; PURPOSE:
;
;       This method draws a selection box around the colorbar object
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
PRO CatColorBar::DrawSelectionBox, Color=color

   @cat_pro_error_handler

   ; If the text is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   IF N_Elements(color) EQ 0 THEN color = self.color

   ; Get the position of the colorbar in the window.
   self.bar -> GetProperty, Position=p

   ; Draw the box.
   Plots, [p[0], p[0], p[2], p[2], p[0]], $
          [p[1], p[3], p[3], p[1], p[1]], /Normal, Color=FSC_Color(color)

   ; Find midpoint of box.
   xl = Min([p[0], p[2]], Max=xr)
   midx = (xr - xl) / 2.0 + xl
   yl = Min([p[1], p[3]], Max=yr)
   midy = (yr - yl) / 2.0 + yl

   ; Draw the handles on the box.
   PLOTS, p[0], p[1], PSYM=6, Color=FSC_Color(color), Symsize=1.25, /Normal
   PLOTS, p[0], p[3], PSYM=6, Color=FSC_Color(color), Symsize=1.25, /Normal
   PLOTS, p[2], p[1], PSYM=6, Color=FSC_Color(color), Symsize=1.25, /Normal
   PLOTS, p[2], p[3], PSYM=6, Color=FSC_Color(color), Symsize=1.25, /Normal
   PLOTS, p[0], midy, PSYM=6, Color=FSC_Color(color), Symsize=1.25, /Normal
   PLOTS, p[2], midy, PSYM=6, Color=FSC_Color(color), Symsize=1.25, /Normal
   PLOTS, midx, p[1], PSYM=6, Color=FSC_Color(color), Symsize=1.25, /Normal
   PLOTS, midx, p[3], PSYM=6, Color=FSC_Color(color), Symsize=1.25, /Normal

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::EVENTHANDLER
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
PRO CatColorBar::EventHandler, event

   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF


      'CATCOLORBAR PROPERTYSHEET': BEGIN

         IF event.type EQ 0 THEN BEGIN
            CASE StrUpCase(event.identifier) OF

               'COLOR': BEGIN

                  event.component -> GetProperty, Color=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, Color=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase, REQUESTER=self

               ENDCASE

               'POSITION': BEGIN
                  event.component -> GetProperty, Position=pos
                  event.id -> GetProperty, ID=group_leader
                  position = AdjustPosition(pos, Group_Leader=group_leader)
                  event.component -> SetProperty, Position=position
                  CatRefreshDraw, self, Stop_At='DrawWidget', REQUESTER=self

               ENDCASE

               'CTINDEX': BEGIN
                  event.component -> GetProperty, CTINDEX=ctindex
                  self._colors -> XColors, Group_Leader=event.id
                  self._colors -> GetProperty, CTINDEX=ctindex
                  event.component -> SetProperty, CTINDEX=ctindex
                  CatRefreshDraw, self, Stop_At='DrawWidget', REQUESTER=self

               ENDCASE
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

         ENDCASE ; of CATCOLORBAR PROPERYSHEET events

        ; If you can't handle the event here. Pass it along to superclass EventHandler
        ELSE: self -> SelectableObject::EventHandler, event

   ENDCASE

   IF Obj_Valid(self) THEN self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain CATCOLORBAR properties. Be sure
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
;     CHARSIZE:      The character size of the color bar annotations.
;
;     COLOR:         The name of the color to use for drawing the borders and annotations
;                    of the colorbar.
;;
;     CTINDEX:       The color table index number currently used in the colorbar.
;
;     DIVISIONS:     The number of divisions to divide the bar into.
;
;     FONT:          The type of font used for annotation. Hershey: 0, Hardware:1, True-Type: 2.
;
;     FORMAT:        The format of the bar annotations.
;
;     LAYER:         A CATLAYER object for holding other objects.
;
;     MINOR:         The number of minor tick divisions.
;
;     NCOLORS:       This is the number of colors in the color bar.
;
;     POSITION:      A four-element array of normalized coordinates in the same
;                    form as the POSITION keyword on a plot.
;
;     RANGE:         A two-element vector of the form [min, max].
;
;     RIGHT:         Set if the title is on the right of the colorbar.
;
;     THICKNESS:     Set this to the thickness of the line around the colorbar. By default, 1.0.
;
;     TICKNAMES:     A string array of names or values for the tick marks.
;
;     TITLE:         A string used as the title of the colorbar. Set to "" by default.
;
;     TOP:           Set if the title is on the top of the colorbar.
;
;     VERTICAL:      Set if the colorbar is vertical rather than horizontal.
;
;     _REF_EXTRA:    Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO CatColorBar::GetProperty, $
   CHARSIZE=charsize, $
   CTINDEX=ctindex, $
   DIVISIONS=divisions, $
   FONT=font, $
   FORMAT=format, $
   LAYER=layer, $
   MINRANGE=minrange, $
   MAXRANGE=maxrange, $
   MINOR=minor, $
   NCOLORS=ncolors, $
   POSITION=position, $
   RANGE=range, $
   RIGHT=right, $
   THICKNESS=thickness, $
   TICKLEN=ticklen, $
   TICKNAMES=ticknames, $
   TITLE=title, $
   TOP=top, $
   VERTICAL=vertical, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(charsize) THEN charsize = self.charsize
   IF Arg_Present(ctindex) THEN self._colors -> GetProperty, CTINDEX=ctindex
   IF Arg_Present(divisions) THEN divisions = self.divisions
   IF Arg_Present(font) THEN font = self.font
   IF Arg_Present(format) THEN format = self.format
   IF Arg_Present(layer) THEN layer = self.layerObject
   IF Arg_Present(minor) THEN minor = self.minor
   IF Arg_Present(minrange) THEN minrange = self.range[0]
   IF Arg_Present(maxrange) THEN maxrange = self.range[1]
   IF Arg_Present(ncolors) THEN ncolors = self.ncolors
   IF Arg_Present(position) THEN self.bar -> GetProperty, Position=position
   IF Arg_Present(range) THEN range = self.range
   IF Arg_Present(right) THEN right = self.right
   IF Arg_Present(thickness) THEN thickness = self.thickness
   IF Arg_Present(title) THEN title = self.title
   IF Arg_Present(ticklen) THEN ticklen = self.ticklen
   IF Arg_Present(ticknames) THEN IF Ptr_Valid(self.ticknames) THEN $
      ticknames = *self.ticknames ELSE ticknames = ""
   IF Arg_Present(top) THEN top = self.top
   IF Arg_Present(vertical) THEN vertical = self.vertical


   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::INTERACTIONEVENTS
;
; PURPOSE:
;
;       This method accepts events from an interaction object of some type. The interaction
;       may pre-process events, or send them directly here. You are required to have the
;       following modes: SELECT, INSERT, and DRAW. Other modes are optional and are left
;       to the programmer to interpret.
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
PRO CatColorBar::InteractionEvents, event, Interaction=interaction

   @cat_pro_error_handler

   ; Get information from the INTERACTION object. In particular, you want to know the MODE, and
   ; the object references of the draw widget and pixmap.
   IF Obj_Valid(interaction) EQ 0 THEN Message, 'An "interaction" object is a required keyword parameter.'
   interaction -> GetProperty, MODE=mode, DRAWWIDGET=drawID, PIXMAP=pixmap

   ; Action is based on the current mode of the interaction.
   CASE mode OF

      'SELECT': BEGIN

         ; If you are close to a selection circle, then you are moving
         ; an edge or edges of the colorbar and not the colorbar itself.
         theMode = self -> SelectMode(event.x, event.y, DRAWID=drawID)
         IF theMode NE 9 THEN BEGIN
            self.moveend = theMode
            interaction -> SetProperty, Mode='MOVE_EDGE'
         ENDIF
         END

      'INSERT':
      'DRAW':

            'MOVE_EDGE': BEGIN

         CASE thisEvent OF


           'UP': BEGIN
                 drawID -> SetProperty, Motion_Events=0, /Clear_Events
                 drawID -> SetWindow
                 interaction -> SetProperty, Mode='FINISHED_MOVE_EDGE'
                 pixmap -> Refresh
                 self -> Draw
              END



           'MOTION': BEGIN

                 drawID -> SetWindow
                 self -> CopyParameters, drawID, Destination=d, Extent=e
                 pixmap -> Copy, Destination=d, Extent=e, Origin=d
                 self -> ApplyCoords
                 c = Convert_Coord(event.x, event.y, /Device, /To_Data)

                 CASE self.moveend OF

                     0: BEGIN ; Outside Box
                        END

                     1: BEGIN ; Lower-left corner handle.
                        self -> SetProperty, X1=c[0,0], Y1=c[1,0], /Draw
                        END

                     2: BEGIN ; Upper-left corner handle.
                        self -> SetProperty, X1=c[0,0], Y2=c[1,0], /Draw
                        END

                     3: BEGIN ; Lower-right corner handle.
                        self -> SetProperty, X2=c[0,0], Y1=c[1,0], /Draw
                        END

                     4: BEGIN ; Upper-right corner handle.
                        self -> SetProperty, X2=c[0,0], Y2=c[1,0], /Draw
                        END

                     5: BEGIN ; Left side handle.
                        self -> SetProperty, X1=c[0,0], /Draw
                        END

                     6: BEGIN ; Right side handle.
                        self -> SetProperty, X2=c[0,0], /Draw
                        END

                     7: BEGIN ; Bottom handle.
                        self -> SetProperty, Y1=c[1,0], /Draw
                        END

                     8: BEGIN ; Top handle.
                        self -> SetProperty, Y2=c[1,0], /Draw
                        END

                     ELSE:

                 ENDCASE

              END ; Of MOTION events.

           ELSE:

         ENDCASE ; of thisEvent in MOVE_EDGE

         END ; of MOVE_EDGE

      ELSE: BEGIN
         ok = Dialog_Message('Unknown MODE [ ' + mode + ' ] in CATCOLORBAR::InteractionEvents method.')
         END

   ENDCASE

   self -> Report, /Completed


END



;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::MESSAGEHANDLER
;
; PURPOSE:
;
;       This method responds to "messages" sent from other objects. It is called
;       automatically by other objects. To receive messages, it is necessary to
;       "register" with the messaging object.
;
; SYNTAX:
;
;       None. Called by other objects.
;
; ARGUMENTS:
;
;       TITLE:  The message title. This is the "name" of the message indicated when
;               the object registered for messages with the messaging object.
;
; KEYWORDS:
;
;       DATA:   Information from the SENDER that may be relevant in processing the message.
;               Typically, an anonymous structure variable, although it could be anything at all.
;
;       SENDER: An output keyword. This is the object reference of the object that is sending
;               the message.
;-
;*****************************************************************************************************
PRO CatColorbar::MessageHandler, title, SENDER=sender, DATA=data

   ; Initialise the error handler
   @cat_pro_error_handler

   CASE title OF

      'COLORTOOL_TABLECHANGE': BEGIN

            ; If the sender is not the image's colortool object, then update image colors.
            IF sender NE self._colors THEN $
               self._colors -> SetProperty, Red=data.r, Green=data.g, Blue=data.b, Bottom=data.bottom

            ; Redraw the image without erasing.
            self -> Draw


         ENDCASE

      'COLORTOOL_SETPROPERTY': BEGIN

            ; If the sender is not the image's colortool object, then update image colors.
            IF sender NE self._colors THEN BEGIN
               sender -> GetProperty, Red=red, Green=green, Blue=blue
               self._colors -> SetProperty, Red=red, Green=green, Blue=blue
            ENDIF

            ; Redraw the the axes.
            self -> Draw


         ENDCASE


      ELSE: self -> CATDATAATOM::MessageHandler, title, SENDER=sender, DATA=data


   ENDCASE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::MOVE
;
; PURPOSE:
;
;       This method moves the selectable object in a graphics window.
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
PRO CatColorBar::Move, x, y, PIXMAP=pixmap, NODRAW=noDraw

   @cat_pro_error_handler

   ; Convert the points to normal coordinates.
   c = Convert_Coord(x, y, /Device, /To_Normal)

   ; Move the position of the colorbar. Don't let the
   ; bar go outside the window.
   self.bar -> GetProperty, Position=pos
   pos[0] = pos[0] + c[0,0]
   pos[2] = pos[2] + c[0,0]
   xdistance = pos[2]-pos[0]
   IF pos[0] LT 0 THEN BEGIN
      pos[2] = xdistance
      pos[0] = 0.0
   ENDIF
   IF pos[2] GT 1.0 THEN BEGIN
      pos[0] = 1.0 - xdistance
      pos[2] = 1.0
   ENDIF

   pos[1] = pos[1] + c[1,0]
   pos[3] = pos[3] + c[1,0]
   ydistance = pos[3] - pos[1]
   IF pos[1] LT 0 THEN BEGIN
      pos[3] = ydistance
      pos[1] = 0.0
   ENDIF
   IF pos[3] GT 1.0 THEN BEGIN
      pos[1] = 1.0 - ydistance
      pos[3] = 1.0
   ENDIF

   self.bar -> SetProperty, Position=pos

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
;       CATCOLORBAR::SELECT
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
FUNCTION CatColorBar::Select, x, y, SUCCESS=success

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

   ; Obtain the colorbar position.
   self.bar -> GetProperty, Position=p

   ; Are you inside?
   isInside = Inside(xx, yy, [ p[0], p[0], p[2], p[2] ], [ p[1], p[3], p[3], p[1] ])
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
;       CATCOLORBAR::SELECTMODE
;
; PURPOSE:
;
;       This method selects the movement mode based on which which handle is closest
;       to the selection point. The selection point is a location in a draw widget in
;       device coordinates.
;
; SYNTAX:
;
;       theObject -> SelectMode, x, y
;
; ARGUMENTS:
;
;       X:          The X location of the selection.
;
;       Y:          The Y location of the selection.
;
; KEYWORDS:
;
;       DrawID:     A window object that contains the box. Required for converting to
;                   the proper device coodinate system.
;-
;*****************************************************************************************************
FUNCTION CatColorBar::SelectMode, x, y, DRAWID=drawID

   @cat_func_error_handler

   ; Update the mode map
   self -> Update_Modemap
   self.modemap -> GetProperty, XSize=xsize, YSize=ysize


   ; Normalize the location with respect to the box.
   IF N_Elements(drawID) NE 0 THEN drawID -> SetWindow
   self.bar -> GetProperty, Position=p
   b = Convert_Coord([p[0], p[2]], [p[1], p[3]], /Normal, /To_Device)
   xx = 0 > (x - Round(b[0,0])) < (xsize-1)
   yy = 0 > (y - Round(b[1,0])) < (ysize-1)

   ; Take a snapshot of the modemap.
   self.modemap -> SetWindow
   map = TVRD()

   theMode = map[xx,yy]

   self -> Report, /Completed
   RETURN, theMode
END



;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the CATCOLORBAR object's properties. Be sure
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
;     CHARSIZE:      The character size of the color bar annotations. Default is 1.0.
;
;     COLOR:         The name of the color to use for drawing the borders and annotations
;                    of the colorbar. By default, "white".
;
;     COLOR_OBJECT:  Use this keyword to load a color object for setting up colors
;                    for data display.
;
;     CTINDEX:       The color table index number of the colors to load. Used only if a
;                    COLOR_OBJECT is not passed in. Set to 0 (gray-scale) by default.
;
;     DIVISIONS:     The number of divisions to divide the bar into. There will
;                    be (divisions + 1) annotations. The default is 6.
;
;     DRAW:          Set this keyword if you wish to call the DRAW method on SetProperty completion.
;
;     FONT:          Sets the font of the annotation. Hershey: 0, Hardware:1, True-Type: 2.
;
;     FORMAT:        The format of the bar annotations. Default is '(I5)'.
;
;     INVERTCOLORS:  Setting this keyword inverts the colors in the color bar.
;
;     LAYER:         A CATLAYER object for holding other objects. Used here only when there is an UP
;                    event in INSERT mode. At that time a copy of this object is made and inserted
;                    the layer object and this is then inserted into the DrawWidget and/or Pixmap object.
;
;
;     MINOR:         The number of minor tick divisions. Default is 2.
;
;     NCOLORS:       This is the number of colors in the color bar.
;
;     NOINTERP:      Normally the colors are interpolated in the colorbar. Setting this
;                    keyword will ensure the colors are replicated by nearest neighbor sampling.
;
;     NOMESSAGE:     Set this keyword to suppress normal message sending.
;
;     POSITION:      A four-element array of normalized coordinates in the same
;                    form as the POSITION keyword on a plot. Default is
;                    [0.88, 0.10, 0.95, 0.90] for a vertical bar and
;                    [0.10, 0.88, 0.90, 0.95] for a horizontal bar.
;;
;     RANGE:         A two-element vector of the form [min, max]. Provides an
;                    alternative way of setting the MINRANGE and MAXRANGE keywords.
;
;     RIGHT:         This puts the labels on the right-hand side of a vertical
;                    color bar. It applies only to vertical color bars.
;
;     THICKNESS:     Set this to the thickness of the line around the colorbar. By default, 1.0.
;
;     TICKNAMES:     A string array of names or values for the tick marks.
;
;     TITLE:         A string used as the title of the colorbar. Set to "" by default.
;
;     TOP:           This puts the labels on top of the bar rather than under it.
;                    The keyword only applies if a horizontal color bar is rendered.
;
;     VERTICAL:      Setting this keyword give a vertical color bar. The default
;                    is a horizontal color bar.
;-
;*****************************************************************************************************
PRO CatColorBar::SetProperty, $
   CHARSIZE=charsize, $
   CTINDEX=ctindex, $
   DIVISIONS=divisions, $
   DRAW=draw, $
   FONT=font, $
   FORMAT=format, $
   INVERTCOLORS=invertcolors, $
   LAYER=layer, $
   MINOR=minor, $
   MINRANGE=minrange, $
   MAXRANGE=maxrange, $
   NCOLORS=ncolors, $
   NOINTERP=nointerp, $
   NOMESSAGE=nomessage, $
   POSITION=position, $
   RANGE=range, $
   RIGHT=right, $
   THICKNESS=thickness, $
   TICKLEN=ticklen, $
   TICKNAMES=ticknames, $
   TITLE=title, $
   TOP=top, $
   VERTICAL=vertical, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   sendMessage = 1

   IF N_Elements(charsize) NE 0 THEN self.charsize = charsize
   IF N_Elements(ctindex) NE 0 THEN self._colors -> SetProperty, INDEX=ctindex
   IF N_Elements(divisions) NE 0 THEN self.divisions = divisions
   IF N_Elements(font) NE 0 THEN self.font = font
   IF N_Elements(format) NE 0 THEN self.format = format
   IF N_Elements(invertcolors) NE 0 THEN IF Keyword_Set(invertcolors) THEN self._colors -> Reverse
   IF N_Elements(layer) NE 0 THEN BEGIN
      self.layer -> RemoveParent, self
      self.layer = layer
      self.layer -> AddParent, self
   ENDIF
   IF N_Elements(minrange) NE 0 THEN self.range[0] = minrange
   IF N_Elements(maxrange) NE 0 THEN self.range[1] = maxrange
   IF N_Elements(minor) NE 0 THEN self.minor = minor
   IF N_Elements(nointerp) NE 0 THEN self.bar -> SetProperty, NOINTERP=Keyword_Set(nointerp)
   IF N_Elements(position) NE 0 THEN self.bar -> SetProperty, Position = position
   IF N_Elements(range) NE 0 THEN self.range = range
   IF N_Elements(right) NE 0 THEN self.right = right
   IF N_Elements(thickness) NE 0 THEN self.thickness = thickness
   IF N_Elements(ticklen) NE 0 THEN self.ticklen = ticklen
   IF N_Elements(ticknames) NE 0 THEN *self.ticknames = ticknames
   IF N_Elements(title) NE 0 THEN self.title = title
   IF N_Elements(top) NE 0 THEN self.top = top
   IF N_Elements(vertical) NE 0 THEN BEGIN
      self.bar -> GetProperty, Position=p
         pp = Fltarr(4)
         pp[0] = p[1]
         pp[1] = p[0]
         pp[2] = p[3]
         pp[3] = p[2]
      self.bar -> SetProperty, Position=pp
      self.vertical = Keyword_Set(vertical)
      IF self.vertical THEN BEGIN
         bar = REPLICATE(1B,20) # BINDGEN(self.ncolors)
         IF Keyword_Set(self.invertcolors) THEN bar = Reverse(bar, 2)
      ENDIF ELSE BEGIN
         bar = BINDGEN(self.ncolors) # REPLICATE(1B,20)
         IF Keyword_Set(self.invertcolors) THEN bar = Reverse(bar, 2)
      ENDELSE
      self.bar -> SetProperty, Image=bar
      CatRefreshDraw, self, STOP_AT='DRAWWIDGET'
      draw = 0
   ENDIF


   IF (N_ELEMENTS (extraKeywords) GT 0) THEN $
      self -> SELECTABLEOBJECT::SetProperty, NOREFRESH=norefresh, SENDMESSAGE=sendmessage, _EXTRA=extraKeywords

   ; The object could have been deleted. If so, RETURN.
   IF ~Obj_Valid(self) THEN RETURN

   ; Send message?
   IF sendMessage AND ~Keyword_Set(nomessage) THEN self -> SendMessage, 'CATCOLORBAR_CHANGED'

   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed

END





;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::UPDATE_MODEMAP
;
; PURPOSE:
;
;       This method updates the modemap with the latest information.
;
; SYNTAX:
;
;       self -> Update_Modemap
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;      CLEAR:     If this keyword is set, the modemap is cleared of all information.
;-
;*****************************************************************************************************
PRO CatColorBar::Update_Modemap, CLEAR=clear

   @cat_pro_error_handler

   IF Keyword_Set(clear) THEN BEGIN
      self.modemap -> Refresh
      RETURN
   ENDIF

   ; The boundary box must be five pixels larger to allow handle selection.
   c = Convert_Coord(5, 5, /Device, /To_Normal)

   ; Convert the boundary box from normal to device coordinates.
   self.bar -> GetProperty, Position=p
   px = [ p[0], p[2] ] + c[0,0]
   py = [ p[1], p[3] ] + c[1,0]
   b = Convert_Coord(px, py, /Normal, /To_Device)
   x1 = b[0,0]
   x2 = b[0,1]
   y1 = b[1,0]
   y2 = b[1,1]

   ; Resize the modemap and refresh it.
   self.modeMap -> SetProperty, XSize=x2-x1+1, YSize=y2-y1+1
   self.modemap -> Refresh

   ; Find midpoint of box.
   xl = Min([x1, x2], Max=xr)
   midx = (xr - xl) / 2.0 + xl
   yl = Min([y1, y2], Max=yr)
   midy = (yr - yl) / 2.0 + yl

   ; Draw the handles on the box.
   phi = Findgen(32) * (!PI * 2 / 32.)
   phi = [ phi, phi[0] ]
   UserSym, Cos(phi), Sin(phi), /Fill

   self.modemap -> SetWindow
   Device, Decomposed=0, Get_Decomposed=theState
   TVLCT, r, g, b, /Get
   LoadCT, 0, /Silent
   POLYFILL, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=9
   PLOTS, x1, y1,   PSYM=8, Color=1, /Device, Symsize=1.5
   PLOTS, x1, y2,   PSYM=8, Color=2, /Device, Symsize=1.5
   PLOTS, x2, y1,   PSYM=8, Color=3, /Device, Symsize=1.5
   PLOTS, x2, y2,   PSYM=8, Color=4, /Device, Symsize=1.5
   PLOTS, x1, midy, PSYM=8, Color=5, /Device, Symsize=1.5
   PLOTS, x2, midy, PSYM=8, Color=6, /Device, Symsize=1.5
   PLOTS, midx, y1, PSYM=8, Color=7, /Device, Symsize=1.5
   PLOTS, midx, y2, PSYM=8, Color=8, /Device, Symsize=1.5

   Device, Decomposed=theState
   TVLCT, r, g, b

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::CLEANUP
;
; PURPOSE:
;
;       This is the CATCOLORBAR object class destructor method.
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
PRO CatColorBar::CLEANUP

   @cat_pro_error_handler


   IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self
   Obj_Destroy, self.bar
   Ptr_Free, self.tickv
   Ptr_Free, self.extra
   Obj_Destroy, self.modemap

   self -> SELECTABLEOBJECT::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATCOLORBAR::INIT
;
; PURPOSE:
;
;       This is the CATCOLORBAR object class initialization method
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
;     CHARSIZE:      The character size of the color bar annotations. Default is 1.0.
;
;     COLOR:         The name of the color to use for drawing the borders and annotations
;                    of the colorbar. By default, "white".
;
;     COLOR_OBJECT:  Use this keyword to load a color object for setting up colors
;                    for data display.
;
;     CTINDEX:       The color table index number of the colors to load. Used only if a
;                    COLOR_OBJECT is not passed in. Set to 0 (gray-scale) by default.
;
;     DIVISIONS:     The number of divisions to divide the bar into. There will
;                    be (divisions + 1) annotations. The default is 6.
;
;     FONT:          Sets the font of the annotation. Hershey: 0, Hardware:1, True-Type: 2.
;
;     FORMAT:        The format of the bar annotations. Default is '(I5)'.
;
;     INVERTCOLORS:  Setting this keyword inverts the colors in the color bar.
;
;     LAYER:         A CATLAYER object for holding other objects. Used here only when there is an UP
;                    event in INSERT mode. At that time a copy of this object is made and inserted
;                    the layer object and this is then inserted into the DrawWidget and/or Pixmap object.
;
;     MATCHPARENT:   If this keyword is set, and the parent of the CATCOLORBAR is a CATIMAGE, then
;                    the position of the colorbar will be matched to the actual position of the image
;                    in the window.
;
;     MINOR:         The number of minor tick divisions. Default is 2.
;
;     NCOLORS:       This is the number of colors in the color bar.
;
;     NOINTERP:      Normally the colors are interpolated in the colorbar. Setting this
;                    keyword will ensure the colors are replicated by nearest neighbor sampling.
;
;     POSITION:      A four-element array of normalized coordinates in the same
;                    form as the POSITION keyword on a plot. Default is
;                    [0.88, 0.10, 0.95, 0.90] for a vertical bar and
;                    [0.10, 0.88, 0.90, 0.95] for a horizontal bar.
;;
;     RANGE:         A two-element vector of the form [min, max]. Provides an
;                    alternative way of setting the MINRANGE and MAXRANGE keywords.
;
;     RIGHT:         This puts the labels on the right-hand side of a vertical
;                    color bar. It applies only to vertical color bars.
;
;     THICKNESS:     Set this to the thickness of the line around the colorbar. By default, 1.0.
;
;     TICKNAMES:     A string array of names or values for the tick marks.
;
;     TITLE:         A string used as the title of the colorbar. Set to "" by default.
;
;     TOP:           This puts the labels on top of the bar rather than under it.
;                    The keyword only applies if a horizontal color bar is rendered.
;
;     VERTICAL:      Setting this keyword give a vertical color bar. The default
;                    is a horizontal color bar.
;
;     _EXTRA:        Any keywords appropriate for the SELECTABLEOBJECT INIT method or the PLOT
;                    procedure for drawing the box around the colorbar image.
;-
;*****************************************************************************************************
FUNCTION CatColorBar::INIT, $
   CHARSIZE=charsize, $
   COLOR=color, $
   CTINDEX=ctindex, $
   DIVISIONS=divisions, $
   FONT=font, $
   FORMAT=format, $
   INVERTCOLORS=invertcolors, $
   LAYER=layer, $
   MATCHPARENT=matchparent, $
   MINOR=minor, $
   NCOLORS=ncolors, $
   NOINTERP=nointerp, $
   POSITION=position, $
   RANGE=range, $
   RIGHT=right, $
   THICKNESS=thickness, $
   TICKLEN=ticklen, $
   TICKNAMES=ticknames, $
   TITLE=title, $
   TOP=top, $
   VERTICAL=vertical, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   IF N_Elements(color) EQ 0 THEN color = 'White'
   IF N_Elements(ctindex) EQ 0 THEN ctindex = 0

   ok = self -> SELECTABLEOBJECT::INIT(DESCRIPTION='CatColorBar Properties', Color=color, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   IF Obj_Valid(self._colors) EQ 0 THEN self -> SetProperty, Color_Object = Obj_New('ColorTool', ctindex)

   ; Check keywords.
   IF N_Elements(charsize) EQ 0 THEN charsize = 1.0
   IF N_Elements(divisions) EQ 0 THEN divisions = 6
   IF N_Elements(font) EQ 0 THEN font = 0 ELSE font = 0 > font < 2
   IF N_ELEMENTS(format) EQ 0 THEN format = '(I5)'
   IF Obj_Isa_Valid(layer, "CATLAYER") NE 0 THEN BEGIN
      self.layerObject = layer
      self.layerObject -> AddParent, self
   ENDIF
   IF N_ELEMENTS(minor) EQ 0 THEN minor = 2
   IF N_ELEMENTS(ncolors) EQ 0 THEN ncolors = 256
   IF N_ELEMENTS(range) EQ 0 THEN range = [0, 255]
   IF N_ELEMENTS(thickness) EQ 0 THEN thickness = 1.0
   IF N_ELEMENTS(ticklen) EQ 0 THEN ticklen = 0.2
   IF N_ELEMENTS(title) EQ 0 THEN title = ""

   ; You can't have a format set *and* use ticknames.
   IF N_ELEMENTS(ticknames) NE 0 THEN format = ""

   IF KEYWORD_SET(vertical) THEN BEGIN
      bar = REPLICATE(1B,20) # BINDGEN(ncolors)
      IF Keyword_Set(invertcolors) THEN bar = Reverse(bar, 2)
      IF N_ELEMENTS(position) EQ 0 THEN BEGIN
         position = [0.88D, 0.1, 0.95, 0.9]
      ENDIF ELSE BEGIN
         IF position[2]-position[0] GT position[3]-position[1] THEN BEGIN
            position = [position[1], position[0], position[3], position[2]]
         ENDIF
         IF position[0] GE position[2] THEN Message, "Position coordinates can't be reconciled."
         IF position[1] GE position[3] THEN Message, "Position coordinates can't be reconciled."
      ENDELSE
   ENDIF ELSE BEGIN
      bar = BINDGEN(ncolors) # REPLICATE(1B, 20)
      IF Keyword_Set(invertcolors) THEN bar = Reverse(bar, 1)
      IF N_ELEMENTS(position) EQ 0 THEN BEGIN
         position = [0.1D, 0.88, 0.9, 0.95]
      ENDIF ELSE BEGIN
         IF position[3]-position[1] GT position[2]-position[0] THEN BEGIN
            position = [position[1], position[0], position[3], position[2]]
         ENDIF
         IF position[0] GE position[2] THEN Message, "Position coordinates can't be reconciled."
         IF position[1] GE position[3] THEN Message, "Position coordinates can't be reconciled."
      ENDELSE
   ENDELSE

    ; Set the number of colors in the colortool.
    self._colors -> SetProperty, NColors=ncolors
    self._colors -> RegisterForMessage, self, 'COLORTOOL_TABLECHANGE'

   ; Load object.
   self.bar = OBJ_NEW('CatImage', bar, $
      Color_Object=self._colors, $
      NoInterp=Keyword_Set(nointerp), $
      Position=position)


   self._colors -> RegisterForMessage, self.bar, 'COLORTOOL_TABLECHANGE', /Unregister
   self.charsize = charsize
   self.divisions = divisions
   self.font = font
   self.format = format
   IF N_Elements(extrakeywords) NE 0 THEN self.extra = Ptr_New(extrakeyords) ELSE $
      self.extra = Ptr_New(/Allocate_Heap)
   self.invertcolors = Keyword_Set(invertcolors)
   self.minor = minor
   self.matchparent = Keyword_Set(matchparent)
   self.ncolors = ncolors
   self.range = range
   self.right = Keyword_Set(right)
   self.thickness = thickness
   self.ticklen = ticklen
   IF N_Elements(ticknames) NE 0 THEN self.tickv = Ptr_New(ticknames) ELSE self.tickv = Ptr_New(/Allocate_Heap)
   self.title = title
   self.top = Keyword_Set(top)
   self.vertical = Keyword_Set(vertical)

   ; Create a modemap for resizing the colorbar
   self.modemap = Obj_New('Pixmapwidget')


   ; Register properties for the property sheet. Turn visibility off, since some properties
   ; cause the object to refresh and draw prematurely.
   currentVisible = self.visible
   self.visible = 0
   self -> RegisterProperty, 'CHARSIZE', 3, NAME="Character Size", VALID_RANGE=[0.1, 10.0]
   self -> RegisterProperty, 'CTINDEX', 0, NAME="Color Table", USERDEF='Load Color Table'
   self -> RegisterProperty, 'FONT', 9, NAME="Font Type", ENUMLIST=['Hershey', 'Hardware', 'True-Type']
   self -> SetPropertyByIdentifier, 'FONT', self.font
   self -> RegisterProperty, 'FORMAT', 4, NAME="Format"
   self -> RegisterProperty, 'MAXRANGE', 3, NAME="Maximum Range"
   self -> RegisterProperty, 'MINRANGE', 3, NAME="Minimum Range"
   self -> RegisterProperty, 'DIVISIONS', 2, NAME="Major Tick Divisions", VALID_RANGE=[0,30]
   self -> RegisterProperty, 'MINOR', 2, NAME="Minor Tick Divisions", VALID_RANGE=[0, 10]
   self -> RegisterProperty, 'NCOLORS', 2, NAME="Number of Colors", VALID_RANGE=[1,256]
   self -> RegisterProperty, 'POSITION', 0, NAME="Position", USERDEF='Colorbar Position'
   self -> RegisterProperty, 'RIGHT', 1, NAME="Title on Right"
   self -> RegisterProperty, 'THICKNESS', 3, NAME="Thickness", VALID_RANGE=[1.0, 10.0]
   self -> RegisterProperty, 'TICKLEN', 3, NAME="Tick Length", VALID_RANGE=[0.005, 1.0]
   self -> RegisterProperty, 'TITLE', 4, NAME="Title"
   self -> RegisterProperty, 'TOP', 1, NAME="Title on Top"
   self -> RegisterProperty, 'VERTICAL', 1, NAME="Vertical Colorbar"
   self -> RegisterProperty, 'DELETE', 1, NAME="Delete Colorbar"
   self.visible = currentVisible

   self -> Report, /Completed

   RETURN, 1

END

;*****************************************************************************************************
;
; NAME:
;       CATCOLORBAR CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the CATCOLORBAR object.
;
;*****************************************************************************************************
PRO CatColorBar__Define, class

   class = { CATCOLORBAR, $
             bar: Obj_New(), $            ; The colorbar image.
             charsize: 0.0, $             ; The character size of the annotation.
             divisions: 0L, $             ; The number of major tick marks.
             extra: Ptr_New(), $          ; Placeholder for extra keywords.
             invertcolors: 0L, $          ; Flag for creating bars with inverted colors.
             font: 0L, $                  ; The font to use for annotations.
             format: "", $                ; The annotation formating.
             layerObject: Obj_New(), $    ; An optional CATLAYER object for holding the inserted selectable object.
             matchparent: 0L, $
             minor: 0L, $                 ; The number of minor tick marks.
             modemap: Obj_New(), $        ; A pixmap for resizing the colorbar.
             moveend: 0L, $               ; A storage for which end of the colorbar is moving.
             ncolors: 0L, $               ; The number of colors in the colorbar.
             range: FltArr(2), $          ; The range of values displayed on the colorbar.
             right: 0L, $                 ; The flag for text on RIGHT of vertical colorbars.
             thickness: 0.0, $            ; The thickness of the selectable object.
             ticklen: 0.0, $              ; The length of the tick marks.
             tickv: Ptr_New(), $          ; Values for the tick marks.
             title: "", $                 ; The title string for the colorbar.
             top: 0L, $                   ; The flag for text on TOP of horizontal colorbars.
             vertical: 0L, $              ; The flag for a vertical colorbar.
             INHERITS SELECTABLEOBJECT $
           }

END

