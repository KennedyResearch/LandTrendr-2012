;*****************************************************************************************************
;+
; NAME:
;       PROPERTYPANEL__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to create a panel object, which has
;       a tree-widget showing the object hierarchy on the left and the
;       control panel of each selected tree-widget object on the right.
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
;       theObject = Obj_New("PROPERTYPANEL")
;
; SUPERCLASSES:
;
;       TOPLEVELBASE
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { PROPERTYPANEL, $
;             INHERITS TOPLEVELBASE, $          ; The PROPERTYPANEL is a TOPLEVELBASE.
;             controlPanel: Obj_New(), $        ; The current control panel in the display.
;             curpos: 0L, $                     ; Current position of line. Used to move the line.
;             delta: 0L, $                      ; Current distance line has moved.
;             down: 0L, $                       ; Status flag of button DOWN in line widget.
;             left_arrow: Obj_New(), $          ; The left-facing arrow.
;             left_base: Obj_New(), $           ; The left-most base widget (holds tree-widgets).
;             left_xsize: 0L, $                 ; Y size of left base.
;             line_base: Obj_New(), $           ; Holds separator widgets.
;             line_drawID: Obj_New(), $         ; Contains the line.
;             linepix: Ptr_New(), $             ; An array of values that make up the line.
;             minsize: 0L, $                    ; The minumum X size allowed for the left and right base widgets.
;             move_drawID_1: Obj_New(), $       ; Allows the line to be moved.
;             move_drawID_2: Obj_New(), $       ; Allows the line to be moved in X Windows.
;             move_base_1: Obj_New(), $         ; Allows the line to be moved.
;             move_base_2: Obj_New(), $         ; Allows the line to be moved in X Windows.
;             right_arrow: Obj_New(), $         ; The right-facing arrow.
;             right_base: Obj_New(), $          ; The right-most base widget (holds control panels).
;             right_xsize: 0L, $                ; X size of right base.
;             statusbar: Obj_New(), $           ; A statusbar widget.
;             topbase: Obj_New(), $             ; The top-most widget in the widget hierarchy.
;             treeWidget: Obj_New(), $          ; The container contents tree widget.
;             visible: 0L, $                    ; Current visibility. 1-left base only, 2-right base only, 3-both bases.
;             xspace: 0L, $                     ; Used to create the line image.
;             ysize: 0L $                       ; The Y size of the right and left base widgets.
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 1 Sept 2005.
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
;        PROPERTYPANEL::DISPLAY_SLIDER_BAR
;
; PURPOSE:
;
;        A utility routine for drawing the slider bar that is the panel separator.
;
; SYNTAX:
;
;        theObject -> Display_Slider_Bar, index
;
; ARGUMENTS:
;
;       index:    An integer specifying which view of the slider bar to display.  0 - normal view
;                 (active, gray background), 1 - inactive, 2 - highlighted.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO PropertyPanel::Display_Slider_Bar, index

   @cat_pro_error_handler

   IF N_Elements(index) EQ 0 THEN index = 0

   ; Load colors after storing current color table.
   TVLCT, rr, gg, bb, /Get
   colors = Widget_Info(self->GetID(), /System_Colors)
   face = colors.face_3d
   TVLCT,[0,face[0],0,face[0]*0.8,face[0],255], $
         [0,face[1],0,face[1]*0.8,face[1],255], $
         [0,face[2],0,face[2]*0.8,255,    255]

   ; Draw the line in the window.
   self.line_drawID -> SetWindow
   TVImage, (*self.linepix)[index*8:(index+1)*8-1,*], /TV

   ; Draw borders around the line.
   self.line_drawID -> GetProperty, Geometry=g
   ysize = g.ysize
   Device, Decomposed=0, Get_Decomposed=theState
   TV, BytArr(8)+1b,0,ysize-1
   TV, BytArr(8),0,ysize-2
   TV, BytArr(6)+5b,1,ysize-3
   TV, BytArr(8),0,5
   TVLCT, rr, gg, bb
   Device, Decomposed=theState

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;        PROPERTYPANEL::DRAW_DASHED_BAR
;
; PURPOSE:
;
;        A utility routine for drawing a dashed bar as the panel separator
;        is moved.
;
; SYNTAX:
;
;        theObject -> DrawDashedBar
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
PRO PropertyPanel::DrawDashedBar

   @cat_pro_error_handler

   ; Create a line image.
   b=(BytArr(~(self.xspace mod 2) + self.xspace, self.ysize)+ $
     (lindgen(self.xspace+1, self.ysize) mod 2))[0:self.xspace-1,0:self.ysize-1]

   ; Load colors.
   TVLCT, rr, gg, bb, /Get
   TVLCT, FSC_Color(['Black', 'White'], /Triple), 0

   ; Display line.
   self.move_drawID_1 -> SetWindow
   TVImage, b, /TV
   self.move_drawID_2 -> SetWindow
   TVImage, b, /TV

   ; Restore color table.
   TVLCT, rr, gg, bb

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;        PROPERTYPANEL::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the PROPERTYPANEL object. It will typically
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
PRO PropertyPanel::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   CASE event.name OF

      ; Events from the line draw widget. Move the line, etc.
      'LINE_DRAWID': BEGIN

         ; Tracking events.
         IF event.event_name EQ 'WIDGET_TRACKING' THEN BEGIN

            ; If both panes are visible, change the cursor to "movement".
            IF self.visible EQ 3 THEN BEGIN
               self -> Display_Slider_Bar, (event.enter ? 2 : 0)
               event.id -> SetWindow
               IF event.enter THEN BEGIN
                  Device, Cursor_Standard= (!Version.OS_Family NE 'Windows' ? 108 : 32644)
                  self.statusbar -> SetProperty, Text='Move slider to resize panels.'
               ENDIF ELSE BEGIN
                  Device, /Cursor_Crosshair
                  self.statusbar -> SetProperty, Text='Select object in LEFT panel to view its properties in RIGHT panel.'
               ENDELSE
            ENDIF ELSE BEGIN
               event.id -> SetWindow
               IF event.enter THEN BEGIN
                  Device, /Cursor_Original
                  self.statusbar -> SetProperty, Text='Use arrow buttons to view entire panel.'
               ENDIF ELSE BEGIN
                  Device, /Cursor_Crosshair
                  self.statusbar -> SetProperty, Text='Use arrow buttons to view entire panel.'
               ENDELSE
            ENDELSE
            RETURN
         ENDIF

         IF event.type EQ 4 THEN BEGIN ; Expose events
            self -> Display_Slider_Bar, (self.visible NE 3)
            RETURN
         ENDIF

         ; If both panes are visible continue, otherwise exit now.
         IF self.visible NE 3 THEN RETURN

         ; If this is button DOWN event, make move line visible. Two lines
         ; are required to handle differences between Windows and Motif.
         IF event.type EQ 0 THEN BEGIN
           self.curpos = event.x
           self.delta = 0
           self.down = 1
           self.line_base -> GetProperty, Geometry=g
           self.move_base_1 -> SetProperty, XOffset=g.xoffset, Map=1
           self.move_base_2 -> SetProperty, XOffset=g.xoffset+1, Map=1
         ENDIF

         ; If this is a button UP event, hide the move line and process new widget sizes
         ; and offsets for the side bases.
         IF event.type EQ 1 THEN BEGIN
            self.down = 0
            self.left_base -> GetProperty, Geometry=geoleft
            self.right_base -> GetProperty, Geometry=georight
            self.line_base -> GetProperty, Geometry=geoline
            self.move_base_1 -> SetProperty, XOffset=0, Map=0
            self.move_base_2 -> SetProperty, XOffset=0, Map=0
            self.left_base -> SetProperty, XSIZE=((self.left_xsize += self.delta))
            self.line_base -> SetProperty, XOFFSET = (geoline.xoffset + self.delta) > 0
            self.right_base -> SetProperty, XSIZE=((self.right_xsize -= self.delta)), $
               XOFFSET=(georight.xoffset + self.delta) > geoline.scr_xsize
            self.line_base -> GetProperty, Geometry=lineinfo
            self.right_base -> GetProperty, Geometry=rightinfo
            self.left_base -> GetProperty, Geometry=leftinfo
            self.controlPanel -> SetProperty, Scr_XSize=rightInfo.scr_xsize, Scr_YSize=self.ysize
            self.treeWidget -> SetProperty, XSize=leftInfo.scr_xsize, YSize=self.ysize
         ENDIF

         ; Motion events while button is DOWN, just move dashed line.
         IF event.type EQ 2 AND self.down EQ 1 THEN BEGIN
            self -> GetProperty, Geometry=geotop
            self.line_base -> GetProperty, Geometry=geoline
            self.left_base -> GetProperty, Geometry=geoleft
            self.right_base -> GetProperty, Geometry=georight
            self.delta = (-self.curpos-geoleft.scr_xsize+self.minsize)> $
               (event.x-self.curpos)< (georight.scr_xsize+geoline.scr_xsize-self.curpos-self.minsize)
            self.move_base_1 -> SetProperty, XOFFSET=0>(geoline.xoffset+self.delta) $
                       <(geotop.xsize-self.minsize-1)
            self.move_base_2 -> SetProperty, XOFFSET=0>(geoline.xoffset+1+self.delta)< $
                       (geotop.xsize-self.minsize)
         ENDIF

         END ; of LINE_DRAWID events.

      'ARROW_BUTTON': BEGIN

         ; Only handle select events.
         IF event.select NE 1 THEN RETURN

         ; Get the value of the button and set proper visibility.
         event.id -> GetProperty, UValue=thisButton
         visible = 3
         CASE thisButton OF
            'LEFT': IF self.visible AND 2 NE 0 THEN visible = (self.visible AND 2) + (~(self.visible AND 1))
            'RIGHT': IF self.visible AND 1 NE 0 THEN visible = (self.visible AND 1) + (~(self.visible AND 2)) * 2
         ENDCASE

         ; Get widget geometries.
         self.left_base -> GetProperty, Geometry=infoleft
         self.right_base -> GetProperty, Geometry=inforight
         self.line_base -> GetProperty, Geometry=infoline
         self.topbase -> GetProperty, Geometry=infotop

         ; Act differently depending upon visibility.
         IF visible NE 0 AND visible NE self.visible THEN BEGIN

            CASE visible OF

               1: BEGIN
                  self.left_arrow -> SetProperty, Sensitive = 0
                  self.right_arrow -> SetProperty, Sensitive = 1
                  self -> Display_Slider_Bar, 1
                  IF self.visible EQ 3 THEN BEGIN
                     self.right_base -> SetProperty, Map=0
                     self.statusbar -> SetProperty, XSIZE=infoleft.scr_xsize
                     self.topbase -> SetProperty, XSIZE=infoleft.scr_xsize + infoline.scr_xsize
                     self.right_base -> SetProperty, XSIZE=1, YSIZE=1, XOFFSET=0
                     self.line_base -> SetProperty, XOFFSET=infoleft.scr_xsize
                     self.right_base -> SetProperty, XOFFSET=0
                     self.statusbar -> Resize, self.topbase
                  ENDIF ELSE BEGIN
                     self.right_base -> SetProperty, MAP=0, XSIZE=1, YSIZE=1, XOFFSET=0
                     self.left_base -> SetProperty, XSIZE=self.left_xsize, YSIZE=self.ysize
                     self.line_base -> SetProperty, XOFFSET=infoleft.scr_xsize
                     self.left_base -> SetProperty, MAP=1
                     self.statusbar -> SetProperty, XSIZE=self.left_xsize + infoline.scr_xsize
                     self.topbase -> SetProperty, XSIZE=inforight.scr_xsize + infoline.scr_xsize + infoleft.scr_xsize
                     self.statusbar -> Resize, self.topbase
                  ENDELSE
                  self.left_arrow -> SetProperty, MAP=0
                  self.left_arrow -> SetProperty, MAP=1
                  self.right_arrow -> SetProperty, MAP=0
                  self.right_arrow -> SetProperty, MAP=1
                  END

               2: BEGIN
                  self.left_arrow -> SetProperty, Sensitive = 1
                  self.right_arrow -> SetProperty, Sensitive = 0
                  self -> Display_Slider_Bar, 1
                  self.line_base -> SetProperty, XOFFSET=0
                  self.right_base -> SetProperty, XOFFSET=infoline.scr_xsize
                  IF self.visible EQ 3 THEN BEGIN
                     self.right_base -> GetProperty, Geometry=inforight
                     self.left_base -> SetProperty, MAP=0
                     self.left_base -> SetProperty, XSIZE=1, YSIZE=1
                     self.statusbar -> SetProperty, XSIZE=1
                     self.topbase -> SetProperty, XSIZE=self.right_xsize + infoline.scr_xsize
                     self.statusbar -> Resize, self.topbase
                  ENDIF ELSE BEGIN
                     self.right_base -> SetProperty, XSIZE=self.right_xsize, YSIZE=self.ysize
                     self.right_base -> SetProperty, MAP=1
                     self.right_base -> GetProperty, Geometry=inforight
                     self.left_base -> SetProperty, MAP=0
                     self.left_base -> SetProperty, XSIZE=1, YSIZE=1
                     self.statusbar -> SetProperty, XSIZE=1
                     self.topbase -> SetProperty, XSIZE=infotop.scr_xsize - infoleft.scr_xsize + inforight.scr_xsize
                     self.statusbar -> Resize, self.topbase
                  ENDELSE
                  self.left_arrow -> SetProperty, MAP=0
                  self.left_arrow -> SetProperty, MAP=1
                  self.right_arrow -> SetProperty, MAP=0
                  self.right_arrow -> SetProperty, MAP=1
                  END

               3: BEGIN
                  self.left_arrow -> SetProperty, Sensitive = 1
                  self.right_arrow -> SetProperty, Sensitive = 1
                  self -> Display_Slider_Bar, 0
                  IF self.visible EQ 1 THEN BEGIN
                     self.right_base -> SetProperty, XSIZE=self.right_xsize, YSIZE=self.ysize
                     self.right_base -> GetProperty, Geometry=inforight
                     self.right_base -> SetProperty, XOFFSET=infoleft.scr_xsize + infoline.scr_xsize
                     self.right_base -> SetProperty, MAP=1
                     self.topbase -> SetProperty, XSIZE=infoline.scr_xsize + inforight.scr_xsize + infoleft.scr_xsize
                     self.statusbar -> Resize, self.topbase
                  ENDIF ELSE BEGIN
                     self.left_base -> SetProperty, XSIZE=self.left_xsize, YSIZE=self.ysize
                     self.left_base -> GetProperty, Geometry=infoleft
                     self.right_base -> SetProperty, XOFFSET=infoleft.scr_xsize + infoline.scr_xsize
                     self.left_base -> SetProperty, MAP=1
                     self.line_base -> SetProperty, XOFFSET=infoleft.scr_xsize
                     self.topbase -> SetProperty, XSIZE=infoleft.scr_xsize + infoline.scr_xsize + inforight.scr_xsize
                     self.statusbar -> Resize, self.topbase
                  ENDELSE
                  self.left_arrow -> SetProperty, MAP=0
                  self.left_arrow -> SetProperty, MAP=1
                  self.right_arrow -> SetProperty, MAP=0
                  self.right_arrow -> SetProperty, MAP=1
                  END

            ENDCASE
            self.visible = visible
         ENDIF

         END ; of ARROW_BUTTON events.

         'PROPERTYPANEL_TLB': BEGIN ; A resize event.

            ; Get the current geometry of the widgets you need.
            self.line_base -> GetProperty, Geometry=lineinfo
            self.statusbar -> GetProperty, Geometry=statusinfo
            self.right_base -> GetProperty, Geometry=rightinfo
            self.left_base -> GetProperty, Geometry=leftinfo
            self.topbase -> GetProperty, Geometry=topinfo

            ; Calculate new xsize and ysize.
            xsize = event.x - lineinfo.scr_xsize
            ysize = event.y - statusinfo.scr_ysize
            self.ysize = ysize


            ; Action depends on current visibility state.
            CASE self.visible OF

               1: BEGIN ; Only left pane is visible.
                  self.left_base -> SetProperty, XSIZE=xsize, YSIZE=ysize
                  self.line_base -> SetProperty, YSIZE=ysize
                  self.left_xsize = xsize
                  self.right_xsize = xsize
                  self.left_base -> GetProperty, Geometry=leftinfo
                  self.line_drawID -> SetProperty, YSIZE=ysize-22
                  self -> DrawDashedBar
                  self.statusBar -> SetProperty, XSIZE=xsize
                  self.topbase -> SetProperty, XSIZE=lineinfo.scr_xsize + leftinfo.scr_xsize, $
                     YSIZE=self.ysize
                  self.line_base -> SetProperty, XOFFSET=leftInfo.scr_xsize
                  self.right_base -> SetProperty, XOFFSET=leftinfo.scr_xsize + lineinfo.scr_xsize
                  self.statusBar -> Resize, self
                  self.controlPanel -> SetProperty, Scr_XSize=leftInfo.scr_xsize, Scr_YSize=self.ysize
                  self.treeWidget -> SetProperty, XSize=leftInfo.scr_xsize, YSize=self.ysize
                  END

               2: BEGIN ; Only right pane is visible.
                  self.right_base -> SetProperty, XSIZE=xsize, YSIZE=ysize
                  self.line_base -> SetProperty, YSIZE=ysize
                  self.right_xsize = xsize
                  self.left_xsize = xsize
                  self.right_base -> GetProperty, Geometry=rightinfo
                  self.line_drawID -> SetProperty, YSIZE=ysize-22
                  self -> DrawDashedBar
                  self.statusbar -> SetProperty, XSIZE=xsize
                  self.topbase -> SetProperty, XSIZE=rightinfo.scr_xsize + lineinfo.scr_xsize, $
                     YSIZE=self.ysize
                  self.right_base -> GetProperty, Geometry=inforight
                  self.statusBar -> Resize, self
                  self.treeWidget -> SetProperty, XSize=rightInfo.scr_xsize, YSize=self.ysize
                  self.controlPanel -> SetProperty, Scr_XSize=rightInfo.scr_xsize, Scr_YSize=self.ysize
                  END

               3: BEGIN ; Both panes are visible.
                  self.left_base -> SetProperty, XSIZE=xsize/2, YSIZE=ysize
                  self.left_xsize = xsize/2
                  self.right_base -> SetProperty, XSIZE=xsize/2, YSIZE=ysize
                  self.right_xsize = xsize/2
                  self.right_base -> GetProperty, Geometry=rightinfo
                  self.left_base -> GetProperty, Geometry=leftinfo
                  self.line_base -> SetProperty, YSIZE=ysize, XOFFSET=leftinfo.scr_xsize
                  self.right_base -> SetProperty, XOFFSET=leftinfo.scr_xsize + lineinfo.scr_xsize
                  self.line_drawID -> SetProperty, YSIZE=ysize-22
                  self -> DrawDashedBar
                  self.statusBar -> SetProperty,  XSIZE=rightinfo.scr_xsize + lineinfo.scr_xsize + leftinfo.scr_xsize
                  self.topbase -> SetProperty, XSIZE=rightinfo.scr_xsize + lineinfo.scr_xsize + leftinfo.scr_xsize, $
                     YSIZE=self.ysize
                  self.treeWidget -> SetProperty, XSize=rightInfo.scr_xsize, YSize=self.ysize
                  self.controlPanel -> SetProperty, Scr_XSize=leftInfo.scr_xsize, Scr_YSize=self.ysize
                  END

            ENDCASE

            ; Update the other draw widgets.
            self -> Display_Slider_Bar
            self.move_drawID_1 -> SetProperty, YSIZE=self.ysize
            self.move_drawID_2 -> SetProperty, YSIZE=self.ysize
            self -> DrawDashedBar

            END ; Of PROPERTYPANEL_TLB resize event.

      ELSE: BEGIN

            ; Special processing of TREE WIDGET events.
            IF StrMid(event.name, 0, 10) EQ 'TREEWIDGET' THEN BEGIN

               ; Only interested in tree widget selection events.
               IF event.event_name NE 'WIDGET_TREE_SEL' THEN RETURN

               ; Get the object out of the event.id
               event.id -> GetProperty, UValue=thisObject

               ; Find and destroy the current control panel, because you
               ; are selecting another one.
               IF Obj_Valid(self.controlPanel) THEN BEGIN
                  self.controlPanel -> GetProperty, First_Parent=cp
                  Obj_Destroy, cp
               ENDIF

               ; Create a control panel for this object.
               IF Obj_Valid(thisObject) THEN BEGIN
                  thisObject -> ControlPanel, self.right_base
               ENDIF ELSE BEGIN
                  self -> ControlPanel, self.right_base
               ENDELSE

               ; Find the property sheet of this control panel, store that.
               propertySheet = self.right_base -> Get(ISA='PROPERTYSHEETWIDGET', /All, /Recursive)
               self.controlPanel = propertySheet
               self.right_base -> GetProperty, Geometry=rightinfo
               IF Obj_Valid(self.controlPanel) THEN $
                  self.controlPanel -> SetProperty, Scr_XSize=rightInfo.scr_xsize, Scr_YSize=self.ysize
               self.right_base -> Draw

            ENDIF ELSE Print, 'Received an event from ' + event.name
            END
   ENDCASE

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       PROPERTYPANEL::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain PROPERTYPANEL properties. Be sure
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
;     MINSIZE:        The minimum X size of either the left or right panel.
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO PropertyPanel::GetProperty, MINSIZE=minsize, _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF ARG_PRESENT(minsize) THEN minsize = self.minsize
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> TOPLEVELBASE::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       PROPERTYPANEL::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the PROPERTYPANEL object's properties. Be sure
;       you ALWAYS call the superclass SETPROPERTY method if you have extra keywords!
;
;
; SYNTAX:
;
;       theObject -> SetProperty ...
;
; ARGUMENTS:
;
;     MINSIZE:        The minimum X size of either the left or right panel.
;
; KEYWORDS:
;
;     _EXTRA:         Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO PropertyPanel::SetProperty, MINSIZE=minsize, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(minsize) NE 0 THEN self.minsize = minsize

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> TOPLEVELBASE::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       PROPERTYPANEL::CLEANUP
;
; PURPOSE:
;
;       This is the PROPERTYPANEL object class destructor method.
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
PRO PropertyPanel::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self.linepix

   self -> TOPLEVELBASE::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       PROPERTYPANEL::INIT
;
; PURPOSE:
;
;       This is the PROPERTYPANEL object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       theObject:     An object reference to the objects whose contents you wish to view.
;
; KEYWORDS:
;
;     MINSIZE:        The minimum X size of either the left or right panel. By default 200 pixels.
;
;     LEFT_XSIZE:     The pixel size of the left-most panel containing the tree widgets. By default, 2*MINSIZE.
;
;     RIGHT_XSIZE:    The pixel size of the right-most panel containing the property sheet of the
;                     currently selected tree-widget. By defaul,t 2*MINSIZE.
;
;     TITLE:          The title of the PropertyPanel top-level base.
;
;     YSIZE:          The Y pixel size of the panels. By default, 2*MINSIZE.
;
;     _EXTRA:         Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION PropertyPanel::INIT, theObject, $
   MINSIZE=minsize, $
   LEFT_XSIZE=left_xsize, $
   RIGHT_XSIZE=right_xsize, $
   TITLE=title, $
   YSIZE=ysize, $

   ; Following TOPLEVELBASE keywords duplicated to prevent unauthorized use.
   COLUMN=column, $
   ROW=row, $
   MODAL=modal, $
   NO_MBAR=no_mbar, $

   _EXTRA=extraKeywords

   ; Set up error handler.
   @cat_func_error_handler

   ; Check parameters.
   IF N_Elements(minsize) EQ 0 THEN minsize = 150
   IF N_Elements(right_xsize) EQ 0 THEN right_xsize = 2 * minsize
   IF N_Elements(left_xsize) EQ 0 THEN left_xsize = 2 * minsize
   IF N_Elements(title) EQ 0 THEN title = 'Object Property Browser'
   IF N_Elements(ysize) EQ 0 THEN ysize = 2 * minsize

   ; Create the widgets.
   ok = self -> TOPLEVELBASE::INIT (parent, COLUMN=1, TITLE=title, /NO_MBAR, _EXTRA=extraKeywords, $
      MAP=0, NAME='PROPERTYPANEL_TLB', YPAD=0, XPAD=0, SPACE=0)
   IF ~ok THEN RETURN, 0
   IF OBJ_ISA_VALID(theObject, 'WIDGETATOM') THEN self -> SetProperty, GROUP_LEADER=theObject
   topbase = Obj_New('BASEWIDGET', self, YPAD=0, XPAD=0, SPACE=0)

   ; Left base widget.
   left_base = Obj_New('BASEWIDGET', topbase, YSIZE=ysize, XSIZE=left_xsize, YPAD=2, XPAD=2, SPACE=0)
   left_base -> GetProperty, Geometry=g
   offset = g.xoffset + g.scr_xsize + g.xpad + g.margin + g.space

   ; Create moving slider bar # 1.
   move_base_1 = Obj_New('BASEWIDGET', topbase, XOFFSET=0, XPAD=2, YPAD=2, MAP=0)
   move_drawID_1 = Obj_New('DRAWWIDGET', move_base_1, XSIZE=2, YSIZE=ysize)

   ; Create base to hold arrows and slider.
   line_base = Obj_New('BASEWIDGET', topbase, XOFFSET=offset, XPAD=2, YPAD=2, SPACE=0, YSIZE=ysize, COLUMN=1)

   ; Create arrows.
   left_arrow = Obj_New('BUTTONWIDGET', line_base, XSIZE=8, YSIZE=11, UVALUE='LEFT', $
      VALUE=Transpose(Byte([8,12,14,15,14,12,8])), X_BITMAP_EXTRA=4, NAME='ARROW_BUTTON')
   right_arrow = Obj_New('BUTTONWIDGET', line_base, XSIZE=8, YSIZE=11, UVALUE='RIGHT', $
      VALUE=Transpose(Byte([1,3,7,15,7,3,1])), X_BITMAP_EXTRA=4, NAME='ARROW_BUTTON')

   ; Create slider bar.
   line_drawID = Obj_New('DRAWWIDGET', line_base, XSIZE=8, YSIZE=ysize-22, EXPOSE_EVENTS=1, $
      TRACKING_EVENTS=1, MOTION_EVENTS=1, BUTTON_EVENTS=1, NAME='LINE_DRAWID')

   line_base -> GetProperty, Geometry=g
   offset = g.xoffset + g.scr_xsize + g.xpad + g.margin + g.space

   ; Left base is estroyed and recreated, so it is positioned in the correct visibility order.
   Obj_Destroy, left_base
   left_base = Obj_New('BASEWIDGET', topbase, YSIZE=ysize, XSIZE=left_xsize, YPAD=2, XPAD=2, SPACE=0, $
      NAME='BROWSER_LEFT BASE')

   right_base = Obj_New('BASEWIDGET', topbase, YSIZE=ysize, XSIZE=right_xsize, XOFFSET=offset, $
      YPAD=2, XPAD=2, SPACE=0, NAME='BROWSER_RIGHT_BASE')

   ; Create moving slider bar #2
   move_base_2 = Obj_New('BASEWIDGET', topbase, XOFFSET=offset, XPAD=2, YPAD=2, SPACE=0, YSIZE=ysize, MAP=0)
   move_drawID_2 = Obj_New('DRAWWIDGET', move_base_2, XSIZE=2, YSIZE=ysize)

   ; Create a line for the slider bar.
   self.linepix = Ptr_New(BytArr(24,(Get_Screen_Size())[1])+1b)
   (*self.linepix)[16:23,*] = 4b
   (*self.linepix)[[0,7,8,15,16,23],*] = 0b
   (*self.linepix)[[1,9,17],*] = 5b
   (*self.linepix)[[3,11,19],3:*:3] = 5b
   (*self.linepix)[[4,20],2:*:3] = 2b

   ; Load the object
   self.left_arrow = left_arrow
   self.left_base = left_base
   self.left_xsize = left_xsize
   self.line_base = line_base
   self.line_drawID = line_drawID
   self.minsize = minsize
   self.move_drawID_1 = move_drawID_1
   self.move_drawID_2 = move_drawID_2
   self.move_base_1 = move_base_1
   self.move_base_2 = move_base_2
   self.right_arrow = right_arrow
   self.right_base = right_base
   self.right_xsize = right_xsize
   self.topbase = topbase
   self.visible = 3 ; Both panels visible on start-up.
   self.xspace = 2
   self.ysize = ysize

   ; Create a statusbar object.
   self.statusbar = OBJ_NEW('STATUSBAR', self, Name='Statusbar', /Align_Left)

   ; Create the tree-widgets for this object's contents.
   IF Obj_Valid(theObject) THEN CatViewContents, theObject, Parent=left_base
   left_base -> GetProperty, OChild=treeWidget
   self.treeWidget = treeWidget

   ; Draw the TLB now, but it is unmapped.
   self -> Draw
   self -> DrawDashedBar
   self -> Display_Slider_Bar

   ; Create the property panel for the right panel.
   IF Obj_Valid(theObject) THEN theObject -> ControlPanel, right_base
   propertySheet = right_base -> Get(ISA='PROPERTYSHEETWIDGET', /All, /Recursive)
   self.controlPanel = propertySheet

   ; Resize the left and right widgets.
   right_base -> GetProperty, Geometry=rightinfo
   left_base -> GetProperty, Geometry=leftinfo
   self.treeWidget -> SetProperty, XSize=rightInfo.scr_xsize, YSize=self.ysize
   self.controlPanel -> SetProperty, Scr_XSize=leftInfo.scr_xsize, Scr_YSize=self.ysize

   ; Alright, show everything and set resize events on TLB.
   self -> SetProperty, Map=1
   self -> SetProperty, SIZE_EVENTS=1

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       PROPERTYPANEL CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the PROPERTYPANEL object.
;
;*****************************************************************************************************
PRO PropertyPanel__DEFINE, class

   class = { PROPERTYPANEL, $
             INHERITS TOPLEVELBASE, $          ; The PROPERTYPANEL is a TOPLEVELBASE.
             controlPanel: Obj_New(), $        ; The current control panel in the display.
             curpos: 0L, $                     ; Current position of line. Used to move the line.
             delta: 0L, $                      ; Current distance line has moved.
             down: 0L, $                       ; Status flag of button DOWN in line widget.
             left_arrow: Obj_New(), $          ; The left-facing arrow.
             left_base: Obj_New(), $           ; The left-most base widget (holds tree-widgets).
             left_xsize: 0L, $                 ; Y size of left base.
             line_base: Obj_New(), $           ; Holds separator widgets.
             line_drawID: Obj_New(), $         ; Contains the line.
             linepix: Ptr_New(), $             ; An array of values that make up the line.
             minsize: 0L, $                    ; The minumum X size allowed for the left and right base widgets.
             move_drawID_1: Obj_New(), $       ; Allows the line to be moved.
             move_drawID_2: Obj_New(), $       ; Allows the line to be moved in X Windows.
             move_base_1: Obj_New(), $         ; Allows the line to be moved.
             move_base_2: Obj_New(), $         ; Allows the line to be moved in X Windows.
             right_arrow: Obj_New(), $         ; The right-facing arrow.
             right_base: Obj_New(), $          ; The right-most base widget (holds control panels).
             right_xsize: 0L, $                ; X size of right base.
             statusbar: Obj_New(), $           ; A statusbar widget.
             topbase: Obj_New(), $             ; The top-most widget in the widget hierarchy.
             treeWidget: Obj_New(), $          ; The container contents tree widget.
             visible: 0L, $                    ; Current visibility. 1-left base only, 2-right base only, 3-both bases.
             xspace: 0L, $                     ; Used to create the line image.
             ysize: 0L $                       ; The Y size of the right and left base widgets.
           }

END
