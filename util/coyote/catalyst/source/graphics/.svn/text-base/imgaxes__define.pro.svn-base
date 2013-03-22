;*****************************************************************************************************
;+
; NAME:
;       IMGAXES__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to display axes on an image object.
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
;       theAxes = Obj_New("IMGAXES")
;       imageObj -> Add, theAxes
;
; SUPERCLASSES:
;
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { IMGAXES, $                ; The object class name.
;             INHERITS CATATOM, $
;             box: DblArr(2,5), $       ; The boundary box for the axes.
;             charsize: 0.0, $          ; The character size.
;             color: "" , $             ; The axes color.
;             coords: Obj_New(), $      ; A coordinate object.
;             font: 0L, $               ; The font to use. (Like !P.Font.)
;             position: FltArr(4), $    ; The normalized position in the window for drawing axes.
;             tickdir: 0B, $            ; The tick direction.
;             title: "", $              ; A title for the axis.
;             visible: 0L, $            ; A flag that indicats the axes should be drawn.
;             xrange:FltArr(2), $       ; The X axis range.
;             xcharsize: 0.0, $         ; The X axis character size.
;             xtickformat: "", $        ; The format for the X axis.
;             xticklen: 0.0, $          ; The X tick length.
;             xtitle: "", $             ; The title or label for the X axis.
;             ycharsize: 0.0, $         ; The Y axis character size.
;             ytickformat: "", $        ; The format for the Y axis.
;             yticklen: 0.0,            ; The Y tick length.
;             yrange:FltArr(2), $       ; The Y axis range.
;             ytitle: "" $              ; The title or label for the Y axis.
;           }
;
; NOTES:
;
;    The image objects will automatically update the IMGAXES object position when the image is
;    drawn if the IMGAXES object has a name of "IMGAXES*" where the "*" can be anything you like.
;    This is the default name given to this object, so it is perhaps best NOT to name the object
;    unless there is good reason to do so. Appending the letters "IMGAXES" to your name will
;    prevent problems.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, July 17, 2003.
;       Removed SCR_XSIZE call on PropertySheet widgets. 12 October 2004. DWF.
;       Changed ON keyword to VISIBLE for Catalyst consistency. 7 July 2005. DWF.
;       Added boundary box calculations at end of DRAW method. 7 July 2005. DWF.
;       Added automatic True-Type fonts for PostScript output, if Hershey fonts would otherwise
;         be selected. 1 August 2006. DWF.
;       Added the ability to specify tick length and direction. 12 Oct 2008. DWF.
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
;+
; NAME:
;       IMGAXES::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the IMGAXES object.
;
; SYNTAX:
;
;       axisObject -> ControlPanel, baseObject
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
PRO ImgAxes::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Create a new control panel

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Axis Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, Name='AXIS PROPERTYSHEET', YSize=20)
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       IMGAXES::DRAW
;
; PURPOSE:
;
;       Draw the axes.
;
; SYNTAX:
;
;       theAxes -> Draw
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
PRO ImgAxes::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; Draw only if the axes are suppose to be on.
   IF self.visible THEN BEGIN

      ; Would prefer True-Type fonts in PostScript files.
      IF self.font EQ 0 AND !D.Name EQ 'PS' THEN font = 2 ELSE font = self.font
      
      xticklen = self.xticklen
      yticklen = self.yticklen
      IF self.tickdir EQ 0 THEN BEGIN
            xticklen = (-1) * xticklen
            yticklen = (-1) * yticklen
      ENDIF
      
      ; If you have a coordinate object, use that to get the range values.
      ; If the range values are different, they you are going to have to
      ; request a redraw of your parent draw widget, because the axes
      ; are out of date and require a higher level re-draw.
      IF Obj_Valid(self.coords) THEN BEGIN
          needUpdate = 0
          self.coords -> GetProperty, XRANGE=xrange, YRANGE=yrange
          IF Floats_Equal(self.xrange, xrange) EQ 0 THEN BEGIN
                self.xrange = xrange
                needUpdate = 1
          ENDIF
          IF Floats_Equal(self.yrange, yrange) EQ 0 THEN BEGIN
                self.yrange = yrange
                needUpdate = 1
          ENDIF
      ENDIF 
      
      IF Keyword_Set(needUpdate) THEN BEGIN
      
         CatRefreshDraw, self, Stop_At='DrawWidget'
         
      ENDIF ELSE BEGIN
      
          ; Draw the axes
          PLOT, self.xrange, self.yrange, XStyle=1, YStyle=1, /NoErase, /NoData, $
             Position=self.position, FONT=font-1, XTickformat=self.xtickformat, YTickformat=self.ytickformat, $
             XCharsize=self.xcharsize, YCharsize=self.ycharsize, Color=FSC_Color(self.color, !P.Color), $
             XRange=self.xrange, YRange=self.yrange, XTitle=self.xtitle, YTitle=self.ytitle, $
             Charsize=self.charsize, Title=self.title, XTICKLEN=xticklen, YTICKLEN=yticklen
      ENDELSE
      
   ENDIF

   ; Update boundary box for the colorbar.
   self.box[0,*] = [!X.Region[0], !X.Region[0], !X.Region[1], !X.Region[1], !X.Region[0]]
   self.box[1,*] = [!Y.Region[0], !Y.Region[1], !Y.Region[1], !Y.Region[0], !Y.Region[0]]


   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       IMGAXES::EVENTHANDLER
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
PRO ImgAxes::EventHandler, event

   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.

   event.ID -> GetProperty, Name=eventName
   CASE eventName OF


      'AXIS PROPERTYSHEET': BEGIN

         IF event.type EQ 0 THEN BEGIN

            CASE StrUpCase(event.identifier) OF

               'COLOR': BEGIN

                  event.component -> GetProperty, Color=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, Color=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget'

               ENDCASE

               'MAX_XRANGE': BEGIN

                  event.component -> GetProperty, XRANGE=xrange
                  event.id -> GetProperty, VALUE=max_xrange, Component=event.component, Property_Value=event.identifier
                  event.component -> SetProperty, XRANGE=[xrange[0],max_xrange]

                  ; Refresh the graphics hierarchy.
                  ;CatRefreshDraw, self, Stop_At='DrawWidget'

               ENDCASE

              'MIN_XRANGE': BEGIN

                  event.component -> GetProperty, XRANGE=xrange
                  event.id -> GetProperty, VALUE=min_xrange, Component=event.component, Property_Value=event.identifier
                  event.component -> SetProperty, XRANGE=[min_xrange,xrange[1]]

                  ; Refresh the graphics hierarchy.
                  ;CatRefreshDraw, self, Stop_At='DrawWidget'

               ENDCASE

               'MAX_YRANGE': BEGIN

                  event.component -> GetProperty, YRANGE=yrange
                  event.id -> GetProperty, VALUE=max_yrange, Component=event.component, Property_Value=event.identifier
                  event.component -> SetProperty, YRANGE=[yrange[0],max_yrange]

                  ; Refresh the graphics hierarchy.
                  ;CatRefreshDraw, self, Stop_At='DrawWidget'

               ENDCASE

              'MIN_YRANGE': BEGIN

                  event.component -> GetProperty, YRANGE=yrange
                  event.id -> GetProperty, VALUE=min_yrange, Component=event.component, Property_Value=event.identifier
                  event.component -> SetProperty, YRANGE=[min_yrange,yrange[1]]

                  ; Refresh the graphics hierarchy.
                  ;CatRefreshDraw, self, Stop_At='DrawWidget'

               ENDCASE

               "TICKFORMAT": BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=tickformat, Component=component, Property_Value=identifier
                  IF tickformat NE "" THEN BEGIN
                     firstChar = StrMid(tickformat, 0, 1)
                     IF  firstChar EQ '"' OR firstChar EQ "'" THEN tickformat = StrMid(tickformat, 1)
                     firstChar = StrMid(tickformat, 0, 1)
                     IF  firstChar NE '(' THEN tickformat = '(' + tickformat
                     tickformat = String(Reverse(Byte(tickformat)))
                     firstChar = StrMid(tickformat, 0, 1)
                     IF  firstChar EQ '"' OR firstChar EQ "'" THEN tickformat = StrMid(tickformat, 1)
                     firstChar = StrMid(tickformat, 0, 1)
                     IF  firstChar NE ')' THEN tickformat = ')' + tickformat
                     tickformat = String(Reverse(Byte(tickformat)))
                     event.component -> SetProperty, Tickformat=tickformat
                     ENDIF ELSE event.component -> SetProperty, Tickformat=""
                     event.id -> SetProperty, Refresh_Property=1

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DRAWWIDGET'

                  END

               ELSE: BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget'


               ENDCASE

            ENDCASE
         ENDIF

         ENDCASE

   ENDCASE

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       IMGAXES::GETPROPERTY
;
; PURPOSE:
;
;       This method enables the getting of the IMGAXES object class
;       properties.
;
; SYNTAX:
;
;       aIMGAXES -> GetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     CHARSIZE:      The character size to display the axes.
;
;     COLOR:         The name of a color to use for the axes (e.g., "yellow").
;
;     COORD_OBJECT:  The coordinate object (usually of the parent) stored here for access.
;
;     BOUNDARY_BOX:  The boundary box that holds the axes.
;
;     FONT:          The font used to draw the axes. Note that the value returned here is one more than
;                    the value used to define the FONT in the INIT method. That is, if you have Hershey
;                    fonts selected, this keyword will return 0, not the expected -1. This is to accomodate
;                    the property sheet widget properties, used in the Control Panel.
;
;     POSITION:      The position in the window in normalized coordinates [x0, y0, x1, y1].
;
;     MAX_XRANGE:    This is the same as xrange[1]. It is used exclusively by the Control Panel in setting properties.
;
;     MAX_YRANGE:    This is the same as yrange[1]. It is used exclusively by the Control Panel in setting properties.
;
;     MIN_XRANGE:    This is the same as xrange[0]. It is used exclusively by the Control Panel in setting properties.
;
;     MIN_YRANGE:    This is the same as yrange[0]. It is used exclusively by the Control Panel in setting properties.
;     
;     TICKDIR:       The tick direction.
;     
;     TITLE:         The current title for the axes.
;
;     VISIBLE:       The current setting of the "draw visible" flag.
;
;     XCHARSIZE:     The current X axis character size.
;
;     XRANGE:        The X axis range.
;     
;     XTICKLEN:      The X tick length.
;
;     XTICKFORMAT:   The format to be used for the X axis annotation.
;
;     XTITLE:        The X axis title.
;
;     YCHARSIZE:     The current Y axis character size.
;
;     YRANGE:        The Y axis range.
;
;     YTICKLEN:      The Y tick length.
;     
;     YTICKFORMAT:   The format to be used for the Y axis annotation.
;
;     YTITLE:        The Y axis title.
;
;     _REF_EXTRA:    Extra keywords for the superclass GETPROPERTY methods.
;-
;*****************************************************************************************************
PRO ImgAxes::GetProperty, $
   BOUNDARY_BOX=boundary_box, $
   CHARSIZE=charsize, $
   COLOR=color, $
   COORD_OBJECT=coords, $
   FONT=font, $
   MAX_XRANGE=max_xrange, $ ; The following four keywords are use exclusively by
   MAX_YRANGE=max_yrange, $ ; the IMGAXIS control panel. Don't remove these keywords.
   MIN_XRANGE=min_xrange, $ ; But don't use them yourself.
   MIN_YRANGE=min_yrange, $
   VISIBLE=visible, $
   POSITION=position, $
   TICKDIR=tickdir, $
   TITLE=title, $
   XCHARSIZE=xcharsize, $
   XTICKFORMAT=xtickformat, $
   XTICKLEN=xticklen, $
   XRANGE=xrange, $
   XTITLE=xtitle, $
   YCHARSIZE=ycharsize, $
   YRANGE=yrange, $
   YTICKFORMAT=ytickformat, $
   YTICKLEN=yticklen, $
   YTITLE=ytitle, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   boundary_box = self.box
   charsize = self.charsize
   color = self.color
   coords = self.coords
   font = self.font
   visible = self.visible
   position = self.position
   tickdir = self.tickdir
   title = self.title
   xcharsize = self.xcharsize
   xrange = self.xrange
   xtickformat = self.xtickformat
   xticklen = self.xticklen
   xtitle = self.xtitle
   ycharsize = self.ycharsize
   yrange = self.yrange
   ytickformat = self.ytickformat
   yticklen = self.yticklen
   ytitle = self.ytitle
   max_xrange = self.xrange[1]
   min_xrange = self.xrange[0]
   max_yrange = self.yrange[1]
   min_yrange = self.yrange[0]

   IF N_Elements(extraKeywords) NE 0 THEN self -> CATATOM::GetProperty, _Extra=extraKeywords

   self -> Report, /Completed
END

;*****************************************************************************************************
;+
; NAME:
;       IMGAXES::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the IMGAXES object class
;       properties.
;
; SYNTAX:
;
;       aIMGAXES -> SetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     CHARSIZE:      Set this keyword to the character size to display the axes.
;
;     COLOR:         Set this keyword to the name of a color to use for the axes (e.g., "yellow").
;
;     COORD_OBJECT:  A coordinate object. This is really a storage location for a coordinate object
;                    from a parent. When the X range or Y range changes in the SetProperty method,
;                    the coordinate object stored here is changed appropriately. This is usually
;                    required if the axis properties are to be changed with its Control Panel.
;
;     FONT:          Set this keyword to use Hershey fonts (0), hardware fonts (1), or true-type fonts (2).
;                    Note that the values are *different* from the values used in the INIT method. This is
;                    because this keyword is being used with the ENUMLIST property in property sheets. Sorry,
;                    it can't be helped. To avoid problems, set the FONT property through the Control Panel.
;
;     TICKDIR:       The direction of the ticks. 0 is outward, 1 is inward.
;
;     TITLE:         A string that will be the title of the axis. Place over the top axis.
;
;     VISIBLE:       Set this keyword to 1 to turn axes drawing VISIBLE, set to 0 to turn OFF.
;
;     XCHARSIZE:     The character size for the X axis characters.
;
;     XRANGE:        Set this keyword to the X axis range.
;
;     XTICKFORMAT:   Set this keyword to the format to be used for the X axis annotation.
;     
;     XTICKLEN:      The length of the X ticks.
;
;     XTITLE:        The title or label for the X axis
;
;     YCHARSIZE:     The character size for the X axis characters.
;
;     YRANGE:        Set this keyword to the Y axis range.
;
;     YTICKFORMAT:   Set this keyword to the format to be used for the Y axis annotation.
;     
;     YTICKLEN:      The length of the Y ticks.
;
;     YTITLE:        The title or label for the Y axis
;
;     _EXTRA:        Extra keywords for the superclass SETPROPERY methods.
;-
;*****************************************************************************************************
PRO ImgAxes::SetProperty, $
   CHARSIZE=charsize, $
   COLOR=color, $
   COORD_OBJECT=coords, $
   FONT=font, $
   VISIBLE=visible, $
   POSITION=position, $
   TICKDIR=tickdir, $
   TITLE=title, $
   XCHARSIZE=xcharsize, $
   XTICKFORMAT=xtickformat, $
   XTICKLEN=xticklen, $
   XRANGE=xrange, $
   XTITLE=xtitle, $
   YCHARSIZE=ycharsize, $
   YRANGE=yrange, $
   YTICKFORMAT=ytickformat, $
   YTICKLEN=yticklen, $
   YTITLE=ytitle, $
   EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(charsize) NE 0 THEN self.charsize = charsize
   IF N_Elements(xcharsize) NE 0 THEN self.xcharsize = xcharsize
   IF N_Elements(ycharsize) NE 0 THEN self.ycharsize = ycharsize
   IF N_Elements(color) NE 0 THEN self.color = color
   IF N_Elements(coords) NE 0 THEN self.coords = coords
   IF N_Elements(font) NE 0 THEN self.font = font
   IF N_Elements(tickdir) NE 0 THEN self.tickdir = tickdir
   IF N_Elements(title) NE 0 THEN self.title = title
   IF N_Elements(visible) NE 0 THEN self.visible = visible
   IF N_Elements(position) NE 0 THEN self.position = position
   IF N_Elements(xtickformat) NE 0 THEN BEGIN
      IF xtickformat EQ "" THEN xtickformat='I0'
      firstChar = StrMid(xtickformat, 0, 1)
      IF  firstChar EQ '"' OR firstChar EQ "'" THEN xtickformat = StrMid(xtickformat, 1)
      firstChar = StrMid(xtickformat, 0, 1)
      IF  firstChar NE '(' THEN xtickformat = '(' + xtickformat
      xtickformat = String(Reverse(Byte(xtickformat)))
      firstChar = StrMid(xtickformat, 0, 1)
      IF  firstChar EQ '"' OR firstChar EQ "'" THEN xtickformat = StrMid(xtickformat, 1)
      firstChar = StrMid(xtickformat, 0, 1)
      IF  firstChar NE ')' THEN xtickformat = ')' + xtickformat
      xtickformat = String(Reverse(Byte(xtickformat)))
      self.xtickformat = xtickformat
   ENDIF
   IF N_Elements(ytickformat) NE 0 THEN BEGIN
      IF ytickformat EQ "" THEN ytickformat='I0'
      firstChar = StrMid(ytickformat, 0, 1)
      IF  firstChar EQ '"' OR firstChar EQ "'" THEN ytickformat = StrMid(ytickformat, 1)
      firstChar = StrMid(ytickformat, 0, 1)
      IF  firstChar NE '(' THEN ytickformat = '(' + ytickformat
      ytickformat = String(Reverse(Byte(ytickformat)))
      firstChar = StrMid(ytickformat, 0, 1)
      IF  firstChar EQ '"' OR firstChar EQ "'" THEN ytickformat = StrMid(ytickformat, 1)
      firstChar = StrMid(ytickformat, 0, 1)
      IF  firstChar NE ')' THEN ytickformat = ')' + ytickformat
      ytickformat = String(Reverse(Byte(ytickformat)))
      self.ytickformat = ytickformat
   ENDIF
   IF N_Elements(xrange) NE 0 THEN self.xrange = xrange
   IF N_Elements(xticklen) NE 0 THEN self.xticklen = xticklen
   IF N_Elements(xtitle) NE 0 THEN self.xtitle = xtitle
   IF N_Elements(yrange) NE 0 THEN self.yrange = yrange
   IF N_Elements(yticklen) NE 0 THEN self.yticklen = yticklen
   IF N_Elements(ytitle) NE 0 THEN self.ytitle = ytitle

   IF Obj_Valid(self.coords) THEN BEGIN
      IF N_Elements(xrange) NE 0 THEN self.coords -> SetProperty, XRange=self.xrange
      IF N_Elements(yrange) NE 0 THEN self.coords -> SetProperty, YRange=self.yrange
      IF (N_Elements(xrange) NE 0) OR (N_Elements(yrange) NE 0) THEN BEGIN
          CatRefreshDraw, self, Stop_At='DrawWidget'
      ENDIF
   ENDIF
   
   IF N_Elements(extraKeywords) NE 0 THEN self -> CATATOM::SetProperty, _Extra=extraKeywords

   self -> Report, /Completed
END

;*****************************************************************************************************
;+
; NAME:
;       IMGAXES::CLEANUP
;
; PURPOSE:
;
;       This is the IMGAXES object class destructor method.
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
PRO ImgAxes::CLEANUP

   @cat_pro_error_handler

   self -> CATATOM::CLEANUP

   self -> report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       IMGAXES::INIT
;
; PURPOSE:
;
;       This is the IMGAXES object class initialization method
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
;     CHARSIZE:      Set this keyword to the overall character size to display the axes.
;
;     COLOR:         Set this keyword to the name of a color to use for the axes (e.g., "yellow").
;
;     COORD_OBJECT:  A coordinate object. This is really a storage location for a coordinate object
;                    from a parent. When the X range or Y range changes in the SetProperty method,
;                    the coordinate object stored here is changed appropriately. This is usually
;                    required if the axis properties are to be changed with its Control Panel.
;
;     FONT:          Set this keyword to use Hershey fonts (0), hardware fonts (1), or true-type fonts (2).
;
;     VISIBLE:       If set to 0, axes will be turned off (not drawn). If set to 1 (the default), they
;                    will be drawn.
;
;     POSITION:      The position in the window in normalized coordinates [x0, y0, x1, y1].
;     
;     TICKDIR:       The tick direction. A value of 0 (the default) indicates ticks point outside the
;                    image. A value of 1 indicates the ticks point inside the image.
;
;     TITLE:         A string that will be the title of the axis. Place over the top axis.
;
;     XCHARSIZE:     The character size for the X axis characters.
;
;     XRANGE:        Set this keyword to the X axis range.
;
;     XTICKFORMAT:   Set this keyword to the format to be used for the X axis annotation.
;     
;     XTICKLEN:      The length of the X tick marks, in normalized units. By default, 0.015.
;
;     XTITLE:        The title or label for the X axis
;
;     YCHARSIZE:     The character size for the X axis characters.
;
;     YRANGE:        Set this keyword to the Y axis range.
;
;     YTICKFORMAT:   Set this keyword to the format to be used for the Y axis annotation.
;     
;     YTICKLEN:      The length of the X tick marks, in normalized units. By default, 0.015.
;
;     YTITLE:        The title or label for the Y axis
;
;     _EXTRA:        Extra keywords for the superclass INIT methods.
;
; NOTES:
;
;    The image objects will automatically update the IMGAXES object position when the image is
;    drawn if the IMGAXES object has a name of "IMGAXES*" where the "*" can be anything you like.
;    This is the default name given to this object, so it is perhaps best NOT to name the object
;    unless there is good reason to do so. Appending the letters "IMGAXES" to your name will
;    prevent problems.
;
;-
;*****************************************************************************************************
FUNCTION ImgAxes::INIT, $
   CHARSIZE=charsize, $
   COLOR=color, $
   COORD_OBJECT=coords, $
   FONT=font, $
   VISIBLE=visible, $
   POSITION=position, $
   TICKDIR=tickdir, $
   TITLE=title, $
   XCHARSIZE=xcharsize, $
   XTICKFORMAT=xtickformat, $
   XTICKLEN=xticklen, $
   XRANGE=xrange, $
   XTITLE=xtitle, $
   YCHARSIZE=ycharsize, $
   YRANGE=yrange, $
   YTICKFORMAT=ytickformat, $
   YTICKLEN=yticklen, $
   YTITLE=ytitle, $
   _Extra=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   ; Check keywords.
   IF N_Elements(charsize) NE 0 THEN self.charsize = charsize ELSE self.charsize = 1.0
   IF N_Elements(xcharsize) NE 0 THEN self.xcharsize = xcharsize ELSE self.xcharsize = 0.8
   IF N_Elements(ycharsize) NE 0 THEN self.ycharsize = ycharsize ELSE self.ycharsize = 0.8
   IF N_Elements(coords) NE 0 THEN self.coords = coords
   IF N_Elements(font) NE 0 THEN self.font = 0 > (font + 1) < 2 ELSE self.font = 0
   IF N_Elements(on) EQ 0 THEN visible = 1 ELSE visible = Keyword_Set(on)
   self.visible = visible

   ;  Check other default keywords.
   IF N_Elements(color) EQ 0 THEN self.color = "white" ELSE self.color = color
   IF N_Elements(position) EQ 0 THEN self.position = [0.15, 0.15, 0.9, 0.9] ELSE self.position = position
   IF N_Elements(title) EQ 0 THEN self.title = ""
   IF N_Elements(xrange) EQ 0 THEN xrange = [0,1] ELSE xrange = xrange
   IF N_Elements(xtitle) EQ 0 THEN self.xtitle = "" ELSE self.xtitle = xtitle
   IF N_Elements(yrange) EQ 0 THEN yrange = [0,1] ELSE yrange = yrange
   IF N_Elements(ytitle) EQ 0 THEN self.ytitle = "" ELSE self.ytitle = ytitle
   IF N_Elements(title) EQ 0 THEN self.title = "" ELSE self.title = title
   
   ; Default tickformatting based on range.
   IF N_Elements(xtickformat) EQ 0 THEN BEGIN 
        CASE Size(xrange[0], /TNAME) OF
            'BYTE': xtickformat = '(I0)'
            'INT': xtickformat = '(I0)'
            'LONG': xtickformat = '(I0)'
            'FLOAT': xtickformat = '(F0.2)'
            'DOUBLE': xtickformat = '(D0.2)'
            ELSE: xtickformat = '(I0)'
        ENDCASE
   ENDIF
   IF N_Elements(xtickformat) NE 0 THEN BEGIN
      IF xtickformat EQ "" THEN xtickformat='I0'
      firstChar = StrMid(xtickformat, 0, 1)
      IF  firstChar EQ '"' OR firstChar EQ "'" THEN xtickformat = StrMid(xtickformat, 1)
      firstChar = StrMid(xtickformat, 0, 1)
      IF  firstChar NE '(' THEN xtickformat = '(' + xtickformat
      xtickformat = String(Reverse(Byte(xtickformat)))
      firstChar = StrMid(xtickformat, 0, 1)
      IF  firstChar EQ '"' OR firstChar EQ "'" THEN xtickformat = StrMid(xtickformat, 1)
      firstChar = StrMid(xtickformat, 0, 1)
      IF  firstChar NE ')' THEN xtickformat = ')' + xtickformat
      xtickformat = String(Reverse(Byte(xtickformat)))
      self.xtickformat = xtickformat
   ENDIF
   IF N_Elements(ytickformat) EQ 0 THEN BEGIN
        CASE Size(yrange[0], /TNAME) OF
            'BYTE': ytickformat = '(I0)'
            'INT': ytickformat = '(I0)'
            'LONG': ytickformat = '(I0)'
            'FLOAT': ytickformat = '(F0.2)'
            'DOUBLE': ytickformat = '(D0.2)'
            ELSE: ytickformat = '(I0)'
        ENDCASE
   ENDIF
   IF N_Elements(ytickformat) NE 0 THEN BEGIN
      IF ytickformat EQ "" THEN ytickformat='I0'
      firstChar = StrMid(ytickformat, 0, 1)
      IF  firstChar EQ '"' OR firstChar EQ "'" THEN ytickformat = StrMid(ytickformat, 1)
      firstChar = StrMid(ytickformat, 0, 1)
      IF  firstChar NE '(' THEN ytickformat = '(' + ytickformat
      ytickformat = String(Reverse(Byte(ytickformat)))
      firstChar = StrMid(ytickformat, 0, 1)
      IF  firstChar EQ '"' OR firstChar EQ "'" THEN ytickformat = StrMid(ytickformat, 1)
      firstChar = StrMid(ytickformat, 0, 1)
      IF  firstChar NE ')' THEN ytickformat = ')' + ytickformat
      ytickformat = String(Reverse(Byte(ytickformat)))
      self.ytickformat = ytickformat
   ENDIF
   
   ; Tick length
   IF N_Elements(xticklen) EQ 0 THEN xticklen = 0.015
   IF N_Elements(yticklen) EQ 0 THEN yticklen = 0.015
   self.xticklen = xticklen
   self.yticklen = yticklen
   self.tickdir = Keyword_Set(tickdir)
   
   ; Fill up the range values.
   self.xrange = xrange
   self.yrange = yrange
   
   ok = self -> CATATOM::INIT (parent, _EXTRA=extraKeywords, Description='Axis Properties')
   IF ~ok THEN BEGIN
      self -> Report, /Failed
      RETURN, 0
   ENDIF

   ; Register properties for the property sheet.
   self->RegisterProperty, 'COLOR', 0, NAME="Axis Color", USERDEF="Axis Color"
   self->RegisterProperty, 'CHARSIZE', 3, NAME="Character Size", VALID_RANGE=[0.1, 10.0]
   self->RegisterProperty, 'XCHARSIZE', 3, NAME="X Character Size", VALID_RANGE=[0.1, 10.0]
   self->RegisterProperty, 'YCHARSIZE', 3, NAME="Y Character Size", VALID_RANGE=[0.1, 10.0]
   self->RegisterProperty, 'FONT', 9, NAME="Font Type", ENUMLIST=['Hershey', 'Hardware', 'True-Type'], DESCRIPTION='Hershey'
   self -> SetPropertyByIdentifier, 'FONT', self.font
   self->RegisterProperty, 'XTICKFORMAT', 4, NAME="X Tick Format"
   self->RegisterProperty, 'YTICKFORMAT', 4, NAME="Y Tick Format"
   self->RegisterProperty, 'MAX_XRANGE', 3, NAME="Max X Range"
   self->RegisterProperty, 'MIN_XRANGE', 3, NAME="Min X Range"
   self->RegisterProperty, 'MAX_YRANGE', 3, NAME="Max Y Range"
   self->RegisterProperty, 'MIN_YRANGE', 3, NAME="Min Y Range"
   self->RegisterProperty, 'VISIBLE', 1, NAME="Show Axes"
   self->RegisterProperty, 'TITLE', 4, NAME="Title"
   self->RegisterProperty, 'XTITLE', 4, NAME="X Title"
   self->RegisterProperty, 'YTITLE', 4, NAME="Y Title"
   self->RegisterProperty, 'XTICKLEN', 3, NAME="X Tick Length", VALID_RANGE=[0.0, 1.0]
   self->RegisterProperty, 'YTICKLEN', 3, NAME="Y Tick Length", VALID_RANGE=[0.0, 1.0]
   self->RegisterProperty, 'TICKDIR', 1, NAME="Tick Direction In"


   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       IMGAXES OBJECT CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the IMGAXES object.
;
;*****************************************************************************************************
PRO ImgAxes__DEFINE, class

   class = { IMGAXES, $                ; The object class name.
             INHERITS CATATOM, $
             box: DblArr(2,5), $       ; The boundary box for the axes.
             charsize: 0.0, $          ; The character size.
             color: "" , $             ; The axes color.
             coords: Obj_New(), $      ; A coordinate object.
             font: 0L, $               ; The font to use. (Like !P.Font.)
             position: FltArr(4), $    ; The normalized position in the window for drawing axes.
             tickdir: 0B, $            ; The tick direction.
             title: "", $              ; A title for the axis.
             visible: 0L, $            ; A flag that indicats the axes should be drawn.
             xrange:FltArr(2), $       ; The X axis range.
             xcharsize: 0.0, $         ; The X axis character size.
             xtickformat: "", $        ; The format for the X axis.
             xticklen: 0.0, $          ; The X tick length.
             xtitle: "", $             ; The title or label for the X axis.
             ycharsize: 0.0, $         ; The Y axis character size.
             ytickformat: "", $        ; The format for the Y axis.
             yticklen: 0.0, $          ; The Y tick length.
             yrange:FltArr(2), $       ; The Y axis range.
             ytitle: "" $              ; The title or label for the Y axis.
           }

END

