;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide an interaction for creating
;       and manipulating SELECTABLEOBJECT objects. An interaction takes over 
;       draw widget event handling for the duration of the interaction and then
;       restores it to its normal funcitoning. We spent a LOT of time implementing
;       this, and while it works, it is extremely complicated (bordering on iTools
;       complicated!). I am not happy with the complexity, and I am not sure this
;       is what is needed anyway. I think interaction functionality is too user-specific
;       to be handled like this. In practice, I seldom use interactions, but code
;       the functionality I want for a particular application in the event handlers.
;       That said, this particular interaction is amazingly useful for annotating
;       graphics windows.
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
;       theObject = Obj_New("ANNOTATEINTERACTION")
;
; SUPERCLASSES:
;
;       INTERACTION
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { ANNOTATEINTERACTION, $
;             align_gui_tlb: Obj_New(), $      ; The align GUI top-level base object.
;             align_tb: 0L, $                  ; The current align top/bottom value.
;             align_lr: 0L, $                  ; The current align left/right value.
;             distribute_gui_tlb: Obj_New(), $ ; The distrubute GUI top-level base object.
;             distribute_h: 0L, $              ; The current distribute horizontal value.
;             distribute_v: 0L, $              ; The current distribute vertical value.
;             distribute_gap_h: Obj_New(), $   ; The distribute horizonal gap field object.
;             distribute_gap_v: Obj_New(), $   ; The distribute vertical gap field object.
;             selectedObjects: Ptr_New(), $    ; The currently selected objects.
;             sx: 0L, $                        ; The static X location.
;             sy: 0L, $                        ; The static Y location.
;
;             defaultObject: Obj_New(), $      ; The default (or current) selectable object.
;             defTextObject: Obj_New(), $      ; The default TEXTLINE object.
;             defAngleObject: Obj_New(), $     ; The default ANGLETOOL object.
;             defArrowObject: Obj_New(), $     ; The default ARROW object.
;             defBoxObject: Obj_New(), $       ; The default BOX object.
;             defEllipseObject: Obj_New(), $   ; The default ELLIPSE object.
;             defPolygonObject: Obj_New(), $   ; The default POLYGON object.
;             defMeasureObject: Obj_New(), $   ; The default MEASUREMENT object.
;             layerObject: Obj_New(), $        ; A layer object for holding selectable annotation objects.
;
;             angleID: Obj_New(), $            ; The ANGLE MEASURMENT button on the ControlPanel.
;             annotateOn: Obj_New(), $         ; The ANNOTATE_LAYER_ON button on the ControlPanel.
;             annotateOff: Obj_New(), $        ; The ANNOTATE_LAYER_OFF button on the ControlPanel.
;             arrowID: Obj_New(), $            ; The ARROW button on ControlPanel.
;             boxID: Obj_New(), $              ; The BOX button on ControlPanel.
;             ellipseID: Obj_New(), $          ; The ELLIPSE button on the ControlPanel.
;             globalID: Obj_New(), $           ; The GLOBAL_PROPERTY button on ControlPanel.
;             measureID: Obj_New(), $          ; The MEASUREMENT button on the ControlPanel.
;             polygonID: Obj_New(), $          ; The POLYGON button on the ControlPanel.
;             saveID: Obj_New(), $             ; The SAVE_WINDOW button on ControlPanel.
;             selectID: Obj_New(), $           ; The SELECT button on ControlPanel.
;             textID: Obj_New(), $             ; The TEXT button on ControlPanel.
;
;             INHERITS INTERACTION $
;           }
;
; MESSAGES:
;
;   None.
;
; NOTES:
;
;   The way the interaction works is by copying the contents of the draw widget into a pixmap,
;   so they can be redrawn appropriately. This is only successful if the contents of the draw
;   widget can be drawn in the pixmap. Images, for example, might have their WID keyword set and
;   be attached to a draw widget already. (This is especially the case if they are responding to
;   color table events of some kind.) These kinds of images are not appropriate for annotation
;   interactions, because they can't be drawn in the pixmap.

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
;       ANNOTATEINTERACTION::ALIGN_EVENTS
;
; PURPOSE:
;
;       This event handling method for the ALIGNGUI widget creation method.
;
; SYNTAX:
;
;       theObject -> Align_Events, event
;
; ARGUMENTS:
;
;       event:    The event structure.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO AnnotateInteraction::Align_Events, event
   event.id -> GetProperty, Value=buttonValue, UValue=buttonUValue
   CASE buttonValue OF
      'As Is': BEGIN
         IF buttonUValue EQ 'Top/Bottom' THEN self.align_tb = 0
         IF buttonUValue EQ 'Left/Right' THEN self.align_lr = 0
         END
      'Tops': self.align_tb = 1
      'T/B Centers': self.align_tb = 2
      'Bottoms': self.align_tb = 3
      'Left Sides': self.align_lr = 1
      'L/R Centers': self.align_lr = 2
      'Right Sides': self.align_lr = 3
      'Cancel': Obj_Destroy, self.align_gui_tlb
      'Align': BEGIN
         CASE self.align_tb OF
            0:
            1: BEGIN
               maxy = 0.0D
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> GetProperty, BOUNDARY_BOX=box
                  maxy = maxy > Max(box[1,*])
               ENDFOR
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> GetProperty, BOUNDARY_BOX=box
                  deltay = maxy - Max(box[1,*])
                  self._drawID -> GetProperty, YSIZE=ys
                  (*self.selectedObjects)[j] -> Move, 0, deltay*ys, Pixmap=self._drawID_pixmap
               ENDFOR
               self._drawID_pixmap -> Refresh
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy
               END
            2: BEGIN
               maxy = 0.0D
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> GetProperty, BOUNDARY_BOX=box
                  top = Max(box[1,*], Min=bot)
                  center_y = (top-bot)/2.0 + bot
                  maxy = maxy > center_y
               ENDFOR
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> GetProperty, BOUNDARY_BOX=box
                  top = Max(box[1,*], Min=bot)
                  center_y = (top-bot)/2.0 + bot
                  deltay = maxy - center_y
                  self._drawID -> GetProperty, YSIZE=ys
                  (*self.selectedObjects)[j] -> Move, 0, deltay*ys, Pixmap=self._drawID_pixmap
               ENDFOR
               self._drawID_pixmap -> Refresh
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy
               END
            3: BEGIN
               miny = 1.0D
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> GetProperty, BOUNDARY_BOX=box
                  miny = miny < Min(box[1,*])
               ENDFOR
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> GetProperty, BOUNDARY_BOX=box
                  deltay = miny - Min(box[1,*])
                  self._drawID -> GetProperty, YSIZE=ys
                  (*self.selectedObjects)[j] -> Move, 0, deltay*ys, Pixmap=self._drawID_pixmap
               ENDFOR
               self._drawID_pixmap -> Refresh
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy
               END
         ENDCASE
          CASE self.align_lr OF
            0:
            1: BEGIN
               minx = 1.0D
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> GetProperty, BOUNDARY_BOX=box
                  minx = minx < Min(box[0,*])
               ENDFOR
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> GetProperty, BOUNDARY_BOX=box
                  deltax = minx - Min(box[0,*])
                  self._drawID -> GetProperty, XSIZE=xs
                  (*self.selectedObjects)[j] -> Move, deltax*xs, 0, Pixmap=self._drawID_pixmap
               ENDFOR
               self._drawID_pixmap -> Refresh
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy
               END
            2: BEGIN
               maxx = 0.0D
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> GetProperty, BOUNDARY_BOX=box
                  top = Max(box[0,*], Min=bot)
                  center_y = (top-bot)/2.0 + bot
                  maxx = maxx > center_y
               ENDFOR
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> GetProperty, BOUNDARY_BOX=box
                  top = Max(box[0,*], Min=bot)
                  center_y = (top-bot)/2.0 + bot
                  deltax = maxx - center_y
                  self._drawID -> GetProperty, XSIZE=xs
                  (*self.selectedObjects)[j] -> Move, deltax*xs, 0, Pixmap=self._drawID_pixmap
                  self._drawID_pixmap -> Copy
               ENDFOR
               self._drawID_pixmap -> Refresh
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy
               END
            3: BEGIN
               maxx = 0.0D
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> GetProperty, BOUNDARY_BOX=box
                  maxx = maxx > Max(box[0,*])
               ENDFOR
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> GetProperty, BOUNDARY_BOX=box
                  deltax = maxx - Max(box[0,*])
                  self._drawID -> GetProperty, XSIZE=xs
                  (*self.selectedObjects)[j] -> Move, deltax*xs, 0, Pixmap=self._drawID_pixmap
               ENDFOR
               self._drawID_pixmap -> Refresh
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy
               END
         ENDCASE
         self._drawID -> Draw
         FOR j=0,N_Elements(*self.selectedObjects)-1 DO (*self.selectedObjects)[j] -> DrawSelectionBox

         Obj_Destroy, self.align_gui_tlb
         END
   ENDCASE
END


;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::ALIGNGUI, event
;
; PURPOSE:
;
;       This method builds a modal dialog for obtaining user input for
;       how to align selected items.
;
; SYNTAX:
;
;       theObject -> AlignGUI, event
;
; ARGUMENTS:
;
;       event:    The event structure.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO AnnotateInteraction::AlignGUI, event

   @cat_pro_error_handler

   tlb = Obj_New('TopLevelBase', /Floating, Group_Leader=self._drawID, /Modal, Column=1, $
      Base_Align_Center=1, Event_Object=self, Event_Method='Align_Events', Title='Align')
   self.align_gui_tlb = tlb
   topbase = Obj_New('BaseWidget', tlb, Row=1)
   buttonBase = Obj_New('BaseWidget', tlb, Row=1)

   lefttopbase = Obj_New('BaseWidget', topbase, Column=1, Base_Align_Left=1)
   rttopbase = Obj_New('BaseWidget', topbase, Column=1, Base_Align_Left=1)

   label = Obj_New('LabelWidget', lefttopbase, Value='Top/Bottom:')
   tbbase = Obj_New('BaseWidget', lefttopbase, Column=1, /Exclusive, /Frame)
   button = Obj_New('ButtonWidget', tbbase, Value='As Is', UVALUE='Top/Bottom', /No_Release)
   button -> SetProperty, Set_Button=1
   self.align_tb = 0
   button = Obj_New('ButtonWidget', tbbase, Value='Tops', /No_Release)
   button = Obj_New('ButtonWidget', tbbase, Value='T/B Centers', /No_Release)
   button = Obj_New('ButtonWidget', tbbase, Value='Bottoms', /No_Release)

   label = Obj_New('LabelWidget', rttopbase, Value='Left/Right:')
   tbbase = Obj_New('BaseWidget', rttopbase, Column=1, /Exclusive, /Frame)
   button = Obj_New('ButtonWidget', tbbase, Value='As Is', UVALUE='Left/Right', /No_Release)
   button -> SetProperty, Set_Button=1
   self.align_lr = 0
   button = Obj_New('ButtonWidget', tbbase, Value='Left Sides', /No_Release)
   button = Obj_New('ButtonWidget', tbbase, Value='L/R Centers', /No_Release)
   button = Obj_New('ButtonWidget', tbbase, Value='Right Sides', /No_Release)

   button = Obj_New('ButtonWidget', buttonBase, Value='Cancel')
   button = Obj_New('ButtonWidget', buttonBase, Value='Align')

   tlb -> Draw, /Center

   self -> Report, /Completed
END

;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::BUILDMODEMENU
;
; PURPOSE:
;
;       This method builds a mode-select context menu.
;
; SYNTAX:
;
;       theObject -> BuildModeMenu
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
;;*****************************************************************************************************
;PRO AnnotateInteraction::BuildModeMenu
;
;    Obj_Destroy, self._contextMenu
;    self._contextMenu = Obj_New('ContextMenuBase', self._drawID, Column=1, Event_Object=self)
;    button = Obj_New('ButtonWidget', self._contextMenu, Value='Insert Mode', Name='INSERT MODE', /Checked_Menu)
;    IF self._mode EQ 'INSERT' THEN button -> SetProperty, Set_Button=1
;    button = Obj_New('ButtonWidget', self._contextMenu, Value='Select Mode', Name='SELECT MODE', /Checked_Menu)
;    IF self._mode EQ 'SELECT' THEN button -> SetProperty, Set_Button = 1
;
;END


;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::BUILDMULTISELECTMENU
;
; PURPOSE:
;
;       This method builds a multi-select context menu.
;
; SYNTAX:
;
;       theObject -> BuildMultiSelectMenu
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
PRO AnnotateInteraction::BuildMultiSelectMenu

    Obj_Destroy, self._contextMenu
    self._contextMenu = Obj_New('ContextMenuBase', self._drawID, Column=1, Event_Object=self)
    button = Obj_New('ButtonWidget', self._contextMenu, Value='Align...', Name='ALIGN')
    button = Obj_New('ButtonWidget', self._contextMenu, Value='Distribute...', Name='DISTRIBUTE')
    button = Obj_New('ButtonWidget', self._contextMenu, Value='Group', Name='GROUP')
    button = Obj_New('ButtonWidget', self._contextMenu, Value='Ungroup', Name='UNGROUP')
    button = Obj_New('ButtonWidget', self._contextMenu, Value='Color', Name='COLOR_ALL')
    button = Obj_New('ButtonWidget', self._contextMenu, Value='Delete', Name='DELETE_ALL')

END



;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the object
;
; SYNTAX:
;
;       imageObject -> ControlPanel, baseObj
;
; ARGUMENTS:
;
;       baseObject:    The object reference of a base widget for this control to
;                      be added to. If not supplied, the control panel will be a
;                      self contained window.
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the "CONTROLPANEL::INIT" method.
;
;
;-
;*****************************************************************************************************
PRO AnnotateInteraction::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel
   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, Name='AnnotateInteraction::ControlPanel', $
      TITLE='Annotate', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK, XPad=0, YPad=0, /Exclusive)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

   ; Create the rest of the widgets.
   self.selectID = Obj_New('BUTTONWIDGET', cp, $
                     VALUE=Filepath('arrow.bmp', Subdir=['resource','bitmaps']), $
                     /BitMap, Tooltip='Select Objects', $
                     Helpline ='LEFT to SELECT, DRAG (SHIFT-LEFT to ADD), RIGHT to CHANGE PROPERTIES or GROUP', $
                     NAME='SELECT_OBJECTS', Tracking_Events=1, No_Release=1)
   self.selectID -> SetProperty, Set_Button=1

   self.textID = Obj_New('BUTTONWIDGET', cp, $
                     VALUE=Filepath('text.bmp', Subdir=['resource','bitmaps']), $
                     /BitMap, Tooltip='Textual Annotation', $
                     Helpline ='Draw Text in Window. CR to ENTER. Click inside to drag.', $
                     NAME='INSERT_TEXT', Tracking_Events=1, No_Release=1)

   self.boxID = Obj_New('BUTTONWIDGET', cp, $
                     VALUE=Filepath('rectangl.bmp', Subdir=['resource','bitmaps']), $
                     /BitMap, Tooltip='Box Annotation', $
                     Helpline ='Draw Rectangle in Window. Click inside to drag.', $
                     NAME='INSERT_BOX', Tracking_Events=1, No_Release=1)

   ; Look for resources in Catalyst resource directory. If not there,
   ; look in RSI resource directory. If not there, make do with an ugly resource.
   resourcefile = Filepath('cat_arrow.bmp', Root_Dir=ProgramRootDir(/TWOUP), Subdir=['resources'])
   file = File_Search(resourcefile, Count=count)
   IF count EQ 0 THEN BEGIN
      resourcefile = Filepath('cat_arrow.bmp', Subdir=['resource'])
      file = File_Search(resourcefile, Count=count)
      IF count EQ 0 THEN resourcefile = Filepath('tab-cntr.bmp', Subdir=['resource','bitmaps'])
   ENDIF
   self.arrowID = Obj_New('BUTTONWIDGET', cp, $
                     VALUE=resourcefile, $
                     /BitMap, Tooltip='Arrow Annotation', $
                     Helpline ='Draw Arrow in Window. Click inside to drag.', $
                     NAME='INSERT_ARROW', Tracking_Events=1, No_Release=1)


   self.ellipseID = Obj_New('BUTTONWIDGET', cp, $
                     VALUE=Filepath('ellipse.bmp', Subdir=['resource','bitmaps']), $
                     /BitMap, Tooltip='Ellipse Annotation', $
                     Helpline ='Draw Ellipse in Window. Click inside to drag.', $
                     NAME='INSERT_ELLIPSE', Tracking_Events=1, No_Release=1)


   resourcefile = Filepath('cat_polygon.bmp', Root_Dir=ProgramRootDir(/TwoUp), Subdir=['resources'])
   file = File_Search(resourcefile, Count=count)
   IF count EQ 0 THEN BEGIN
      resourcefile = Filepath('cat_arrow.bmp', Subdir=['resource'])
      file = File_Search(resourcefile, Count=count)
      IF count EQ 0 THEN resourcefile = Filepath('segpoly.bmp', Subdir=['resource','bitmaps'])
   ENDIF
   self.polygonID = Obj_New('BUTTONWIDGET', cp, $
                     VALUE=resourcefile, $
                     /BitMap, Tooltip='Polygon Annotation', $
                     Helpline ='Draw Polygon in Window. Click inside to drag.', $
                     NAME='INSERT_POLYGON', Tracking_Events=1, No_Release=1)


   resourcefile = Filepath('cat_linemeasure.bmp', Root_Dir=ProgramRootDir(/TwoUp), Subdir=['resources'])
   file = File_Search(resourcefile, Count=count)
   IF count EQ 0 THEN BEGIN
      resourcefile = Filepath('cat_linemeasure.bmp', Subdir=['resource'])
      file = File_Search(resourcefile, Count=count)
   ENDIF
   IF count EQ 0 THEN BEGIN
      resourcefile =    [       $
     [000B, 000B],      $
     [000B, 112B],      $
     [000B, 080B],      $
     [000B, 112B],      $
     [000B, 008B],      $
     [000B, 004B],      $
     [000B, 002B],      $
     [000B, 001B],      $
     [128B, 000B],      $
     [064B, 000B],      $
     [032B, 000B],      $
     [016B, 000B],      $
     [014B, 000B],      $
     [010B, 000B],      $
     [014B, 000B],      $
     [000B, 000B]      $
     ]
   ENDIF
   self.measureID = Obj_New('BUTTONWIDGET', cp, $
                     VALUE=resourcefile, $
                     /BitMap, Tooltip='Line Measurement Tool', $
                     Helpline ='Click and measure distance to another point.', $
                     NAME='INSERT_MEASURE', Tracking_Events=1, No_Release=1)


   resourcefile = Filepath('cat_angle.bmp', Root_Dir=ProgramRootDir(/TwoUp), Subdir=['resources'])
   file = File_Search(resourcefile, Count=count)
   IF count EQ 0 THEN BEGIN
      resourcefile = Filepath('cat_angle.bmp', Subdir=['resource'])
      file = File_Search(resourcefile, Count=count)
   ENDIF
   IF count EQ 0 THEN BEGIN
      resourcefile = [   $
     [000B, 000B],      $
     [004B, 000B],      $
     [014B, 000B],      $
     [031B, 000B],      $
     [004B, 000B],      $
     [004B, 000B],      $
     [004B, 000B],      $
     [004B, 000B],      $
     [028B, 000B],      $
     [036B, 000B],      $
     [068B, 000B],      $
     [132B, 016B],      $
     [132B, 048B],      $
     [252B, 127B],      $
     [000B, 048B],      $
     [000B, 016B]      $
     ]
   ENDIF
   self.angleID = Obj_New('BUTTONWIDGET', cp, $
                     VALUE=resourcefile, $
                     /BitMap, Tooltip='Angle Measurement Tool', $
                     Helpline ='Click and measure angle to second point.', $
                     NAME='INSERT_ANGLE', Tracking_Events=1, No_Release=1)

  IF Float(!Version.Release) LT 6.1 THEN $
     resourcefile = Filepath('prop.bmp', Subdir=['resource','bitmaps']) ELSE $
     resourcefile = Filepath('propsheet.bmp', Subdir=['resource','bitmaps'])
  self.globalID = Obj_New('BUTTONWIDGET', cp, $
                 VALUE=resourcefile, $
                 /BitMap, Tooltip='Global Annotation Properties...', $
                 Helpline ='Access Global Annotation Properties.', $
                 NAME='GLOBAL_PROPERTIES', Tracking_Events=1, No_Release=1)

   ; If we have an annotation layer object, then include annotation ON/OFF buttons.
   IF Obj_Valid(self.layerObject) THEN BEGIN

      resourcefile = Filepath('cat_layer_on.bmp', Root_Dir=ProgramRootDir(/TwoUp), Subdir=['resources'])
      file = File_Search(resourcefile, Count=count)
      IF count EQ 0 THEN BEGIN
         resourcefile = Filepath('cat_layer_on.bmp', Subdir=['resource'])
         file = File_Search(resourcefile, Count=count)
         IF count EQ 0 THEN resourcefile = Filepath('view.bmp', Subdir=['resource','bitmaps'])
      ENDIF
      self.annotateOn = Obj_New('BUTTONWIDGET', cp, $
                        VALUE=resourcefile, $
                        /BitMap, Tooltip='Annotation Layer ON', $
                        Helpline ='This button turns the annotation layer ON.', $
                        NAME='ANNOTATE_LAYER_ON', Tracking_Events=1, No_Release=1)

      resourcefile = Filepath('cat_layer_off.bmp', Root_Dir=ProgramRootDir(/TwoUp), Subdir=['resources'])
      file = File_Search(resourcefile, Count=count)
      IF count EQ 0 THEN BEGIN
         resourcefile = Filepath('cat_layer_off.bmp', Subdir=['resource'])
         file = File_Search(resourcefile, Count=count)
         IF count EQ 0 THEN resourcefile = Filepath('window.bmp', Subdir=['resource','bitmaps'])
      ENDIF
      self.annotateOff = Obj_New('BUTTONWIDGET', cp, $
                        VALUE=resourcefile, $
                        /BitMap, Tooltip='Annotation Layer OFF', $
                        Helpline ='This button turns the annotation layer OFF.', $
                        NAME='ANNOTATE_LAYER_OFF', Tracking_Events=1, No_Release=1)


   ENDIF ; end of annotation layer buttons

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN BEGIN
      tlb -> Draw
      ;cp -> Show
   ENDIF

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::DISTRIBUTE_EVENTS
;
; PURPOSE:
;
;       This event handling method for the DISTRIBUTEGUI widget creation method.
;
; SYNTAX:
;
;       theObject -> Distribute_Events, event
;
; ARGUMENTS:
;
;       event:    The event structure.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO AnnotateInteraction::Distribute_Events, event

   CASE event.name OF
      'HORIZONTAL AS IS': BEGIN
         self.distribute_h = 0
         self.distribute_gap_h -> SetProperty, Sensitive=0, Select=0
         END
      'VERTICAL AS IS': BEGIN
         self.distribute_v = 0
         self.distribute_gap_v -> SetProperty, Sensitive=0, Select=0
         END
      'HORIZONTAL EDGE GAP': BEGIN
         self.distribute_h = 1
         self.distribute_gap_h -> SetProperty, Select=1, Sensitive=1
         END
      'VERTICAL EDGE GAP': BEGIN
         self.distribute_v = 1
         self.distribute_gap_v -> SetProperty, Select=1, Sensitive=1
         END
      'HORIZONTAL CENTERS': BEGIN
         self.distribute_h = 2
         self.distribute_gap_h -> SetProperty, Sensitive=0, Select=0
         END
      'VERTICAL CENTERS': BEGIN
         self.distribute_v = 2
         self.distribute_gap_v -> SetProperty, Sensitive=0, Select=0
         END
      'HORIZONTAL EDGES': BEGIN
         self.distribute_h = 3
         self.distribute_gap_h -> SetProperty, Sensitive=0, Select=0
         END
      'VERTICAL EDGES': BEGIN
         self.distribute_v = 3
         self.distribute_gap_v -> SetProperty, Sensitive=0, Select=0
         END
      'CANCEL': Obj_Destroy, self.distribute_gui_tlb

      'DISTRIBUTE': BEGIN
         theObjects = *self.selectedObjects
         num = N_Elements(theObjects)
         CASE self.distribute_h OF
            0:
            1: BEGIN

               ; Get the edge gap.
               self.distribute_gap_h -> GetProperty, Value=gap

               ; Order the objects based on their centers.
               centers = FltArr(num)
               FOR j=0,num-1 DO BEGIN
                  theObjects[j] -> GetProperty, BOUNDARY_BOX=box
                  l = Min(box[0,*], Max=r)
                  centers[j] = (r-l)/2 + l
               ENDFOR
               index = Sort(centers)
               theObjects = theObjects[index]
               centers = centers[index]
               theObjects[0] -> GetProperty, WIDTH=w1
               FOR j= 1,num-1 DO BEGIN
                  theObjects[j] -> GetProperty,WIDTH=w2
                  c = Convert_Coord([gap,0], /Device, /To_Normal)
                  len = w1/2 + c[0,0] + w2/2
                  delta = len - (centers[j]-centers[j-1])
                  self._drawID -> GetProperty, XSIZE=xs
                  theObjects[j] -> MOVE, delta*xs, 0, Pixmap=self._drawID_pixmap
                  theObjects[j] -> CalculateBoundaryBox
                  theObjects[j] -> GetProperty, BOUNDARY_BOX=box
                  l = Min(box[0,*], Max=r)
                  centers[j] = (r-l)/2 + l
                  w1 = w2
               ENDFOR
               self._drawID_pixmap -> Refresh
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy
               END

            2: BEGIN

               ; If you have two or fewer objects, this doesn't make sense.
               IF num LT 3 THEN RETURN

               ; Order the objects based on their centers.
               centers = FltArr(num)
               FOR j=0,num-1 DO BEGIN
                  theObjects[j] -> GetProperty, BOUNDARY_BOX=box
                  l = Min(box[0,*], Max=r)
                  centers[j] = (r-l)/2 + l
               ENDFOR
               index = Sort(centers)
               theObjects = theObjects[index]
               centers = centers[index]
               distance = centers[1] - centers[0]
               FOR j= 2,num-1 DO BEGIN
                  newCenter = centers[j-1] + distance
                  delta = newCenter - centers[j]
                  self._drawID -> GetProperty, XSIZE=xs
                  theObjects[j] -> MOVE, delta*xs, 0, Pixmap=self._drawID_pixmap
                  theObjects[j] -> CalculateBoundaryBox
                  theObjects[j] -> GetProperty, BOUNDARY_BOX=box
                  l = Min(box[0,*], Max=r)
                  centers[j] = (r-l)/2 + l
               ENDFOR
               self._drawID_pixmap -> Refresh
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy
               END

            3: BEGIN

               ; If you have two or fewer objects, this doesn't make sense.
               IF num LT 3 THEN RETURN

               ; Order the objects based on their centers.
               centers = FltArr(num)
               FOR j=0,num-1 DO BEGIN
                  theObjects[j] -> GetProperty, BOUNDARY_BOX=box
                  l = Min(box[0,*], Max=r)
                  centers[j] = (r-l)/2 + l
               ENDFOR
               index = Sort(centers)
               theObjects = theObjects[index]
               centers = centers[index]

               ; Get the edge gap.
               theObjects[0] -> GetProperty, Boundary_Box=box
               left = Max(box[0,*])
               theObjects[num-1] -> GetProperty, Boundary_Box=box
               right = Min(box[0,*])
               gap = Abs(right - left)
               points = FltArr(num-2)
               FOR j=1,num-2 DO BEGIN
                  points[j-1] = (right < left) + (gap/(num-1) * j)
               ENDFOR

               FOR j= 1,num-2 DO BEGIN
                  delta = centers[j] - points[j-1]
                  self._drawID -> GetProperty, XSIZE=xs
                  theObjects[j] -> MOVE, delta*xs, 0, Pixmap=self._drawID_pixmap
               ENDFOR
               self._drawID_pixmap -> Refresh
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy
               END

         ENDCASE

          CASE self.distribute_v OF

            0:

            1: BEGIN

               ; Get the edge gap.
               self.distribute_gap_v -> GetProperty, Value=gap

               ; Order the objects based on their centers.
               centers = FltArr(num)
               FOR j=0,num-1 DO BEGIN
                  theObjects[j] -> GetProperty, BOUNDARY_BOX=box
                  l = Min(box[1,*], Max=r)
                  centers[j] = (r-l)/2 + l
               ENDFOR
               index = Sort(centers)
               theObjects = theObjects[index]
               centers = centers[index]
               theObjects[0] -> GetProperty, HEIGHT=h1
               FOR j= 1,num-1 DO BEGIN
                  theObjects[j] -> GetProperty, Y=y, HEIGHT=h2
                  c = Convert_Coord([0,gap], /Device, /To_Normal)
                  len = h1/2 + c[1,0] + h2/2
                  delta = len - (centers[j]-centers[j-1])
                  self._drawID -> GetProperty, YSIZE=ys
                  theObjects[j] -> MOVE, 0, delta*ys, Pixmap=self._drawID_pixmap
                  theObjects[j] -> CalculateBoundaryBox
                  theObjects[j] -> GetProperty, BOUNDARY_BOX=box
                  l = Min(box[1,*], Max=r)
                  centers[j] = (r-l)/2 + l
                  h1 = h2
               ENDFOR
               self._drawID_pixmap -> Refresh
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy
               END

            2: BEGIN

               ; If you have two or fewer objects, this doesn't make sense.
               IF num LT 3 THEN RETURN

               ; Order the objects based on their centers.
               centers = FltArr(num)
               FOR j=0,num-1 DO BEGIN
                  theObjects[j] -> GetProperty, BOUNDARY_BOX=box
                  l = Min(box[1,*], Max=r)
                  centers[j] = (r-l)/2 + l
               ENDFOR
               index = Sort(centers)
               theObjects = theObjects[index]
               centers = centers[index]
               distance = centers[1] - centers[0]
               FOR j= 2,num-1 DO BEGIN
                  theObjects[j] -> GetProperty, Y=y
                  newCenter = centers[j-1] + distance
                  delta = newCenter - centers[j]
                  self._drawID -> GetProperty, YSIZE=ys
                  theObjects[j] -> MOVE, 0, delta*ys, Pixmap=self._drawID_pixmap
                  theObjects[j] -> CalculateBoundaryBox
                  theObjects[j] -> GetProperty, BOUNDARY_BOX=box
                  l = Min(box[1,*], Max=r)
                  centers[j] = (r-l)/2 + l
               ENDFOR
               self._drawID_pixmap -> Refresh
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy
               END

            3: BEGIN

               ; If you have two or fewer objects, this doesn't make sense.
               IF num LT 3 THEN RETURN

               ; Order the objects based on their centers.
               centers = FltArr(num)
               FOR j=0,num-1 DO BEGIN
                  theObjects[j] -> GetProperty, BOUNDARY_BOX=box
                  l = Min(box[1,*], Max=r)
                  centers[j] = (r-l)/2 + l
               ENDFOR
               index = Sort(centers)
               theObjects = theObjects[index]
               centers = centers[index]

               ; Get the edge gap.
               theObjects[0] -> GetProperty, Boundary_Box=box
               left = Max(box[1,*])
               theObjects[num-1] -> GetProperty, Boundary_Box=box
               right = Min(box[1,*])
               gap = Abs(right - left)
               points = FltArr(num-2)
               FOR j=1,num-2 DO BEGIN
                  points[j-1] = (right < left) + (gap/(num-1) * j)
               ENDFOR

               FOR j= 1,num-2 DO BEGIN
                  theObjects[j] -> GetProperty, Y=y
                  delta = centers[j] - points[j-1]
                  self._drawID -> GetProperty, YSIZE=ys
                  theObjects[j] -> MOVE, 0, delta*ys, Pixmap=self._drawID_pixmap
               ENDFOR
               self._drawID_pixmap -> Refresh
               self._drawID -> SetWindow
               self._drawID_pixmap -> Copy
               END

         ENDCASE
         self._drawID -> Draw
         FOR j=0,num-1 DO theObjects[j] -> DrawSelectionBox
         Obj_Destroy, self.distribute_gui_tlb
         END
   ENDCASE
END


;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::DISTRIBUTEGUI, event
;
; PURPOSE:
;
;       This method builds a modal dialog for obtaining user input for
;       how to align selected items.
;
; SYNTAX:
;
;       theObject -> DistributeGUI, event
;
; ARGUMENTS:
;
;       event:    The event structure.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO AnnotateInteraction::DistributeGUI, event

   @cat_pro_error_handler

   tlb = Obj_New('TopLevelBase', /Floating, Group_Leader=self._drawID, /Modal, Column=1, $
      Base_Align_Center=1, Event_Object=self, Event_Method='Distribute_Events', Title='Distribute')
   self.distribute_gui_tlb = tlb
   topbase = Obj_New('BaseWidget', tlb, Row=1)
   buttonBase = Obj_New('BaseWidget', tlb, Row=1)

   lefttopbase = Obj_New('BaseWidget', topbase, Column=1, Base_Align_Left=1)
   rttopbase = Obj_New('BaseWidget', topbase, Column=1, Base_Align_Left=1)

   label = Obj_New('LabelWidget', lefttopbase, Value='Horizontal Spacing:')
   self.distribute_gap_h = Obj_New('FieldWidget', lefttopbase, Title='Gap (pixels): ', Value=0, $
      XSize=6, Sensitive=0)
   tbbase = Obj_New('BaseWidget', lefttopbase, Column=1, /Exclusive, /Frame)
   button = Obj_New('ButtonWidget', tbbase, Value='As Is', Name='HORIZONTAL AS IS', /No_Release)
   button -> SetProperty, Set_Button=1
   self.distribute_h = 0
   button = Obj_New('ButtonWidget', tbbase, Value='Edge Gap', Name='HORIZONTAL EDGE GAP', /No_Release)
   button = Obj_New('ButtonWidget', tbbase, Value='Equidistant Centers', Name='HORIZONTAL CENTERS', /No_Release)
   button = Obj_New('ButtonWidget', tbbase, Value='Equidistant Edges', Name='HORIZONTAL EDGES', /No_Release)

   label = Obj_New('LabelWidget', rttopbase, Value='Vertical Spacing:')
   self.distribute_gap_v = Obj_New('FieldWidget', rttopbase, Title='Gap (pixels): ', Value=0, $
      XSize=6, Sensitive=0)
   tbbase = Obj_New('BaseWidget', rttopbase, Column=1, /Exclusive, /Frame)
   button = Obj_New('ButtonWidget', tbbase, Value='As Is', Name='VERTICAL AS IS', /No_Release)
   button -> SetProperty, Set_Button=1
   self.distribute_v = 0
   button = Obj_New('ButtonWidget', tbbase, Value='Edge Gap', Name='VERTICAL EDGE GAP', /No_Release)
   button = Obj_New('ButtonWidget', tbbase, Value='Equidistant Centers', Name='VERTICAL CENTERS', /No_Release)
   button = Obj_New('ButtonWidget', tbbase, Value='Equidistant Edges', Name='VERTICAL EDGES', /No_Release)

   button = Obj_New('ButtonWidget', buttonBase, Value='Cancel', Name='CANCEL')
   button = Obj_New('ButtonWidget', buttonBase, Value='Distribute', Name='DISTRIBUTE')

   tlb -> Draw, /Center

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::DRAW
;
; PURPOSE:
;
;       This method draws the interaction in the display window.
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
PRO AnnotateInteraction::Draw, EXTRA=extraKeywords

   @cat_pro_error_handler

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;        ANNOTATEINTERACTION::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the ANNOTATEINTERACTION object.
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
PRO AnnotateInteraction::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Is this a tracking event? If so, handle it here and RETURN.
   IF event.event_name EQ 'WIDGET_TRACKING' THEN BEGIN
      IF Obj_Valid(self._statusbar) THEN BEGIN
         IF event.enter THEN BEGIN
            event.id -> GetProperty, Helpline=theText
            self._statusbar -> SetProperty, Text=theText
         ENDIF ELSE self._statusbar -> SetProperty, $
            Text='[SHIFT-] LEFT click to SELECT/MOVE, RIGHT click for PROPERTIES/GROUP.'
      ENDIF
      RETURN
   ENDIF

   self._drawID -> SetWindow
   
   ; If this is a WIDGET_BASE event, something was resized.
   ; Resize the draw widget and redraw.
   IF event.event_name EQ 'WIDGET_BASE' THEN BEGIN
        self._drawID -> SetProperty, XSIZE=event.x, YSIZE=event.y
        self._drawID -> Draw, /ERASE_WINDOW
        RETURN
   ENDIF

   IF event.event_name EQ 'WIDGET_BUTTON' THEN BEGIN

      CASE event.name OF

         'ALIGN': BEGIN
            self -> AlignGUI, event
            END

         'ANNOTATE_LAYER_ON': BEGIN
            IF Obj_Valid(self.layerObject) THEN BEGIN
               self.layerObject -> SetProperty, Visible=1
               self.angleID -> SetProperty, Sensitive=1
               self.selectID -> SetProperty, Sensitive=1
               self.textID -> SetProperty, Sensitive=1
               self.boxID -> SetProperty, Sensitive=1
               self.arrowID -> SetProperty, Sensitive=1
               self.ellipseID -> SetProperty, Sensitive=1
               self.measureID -> SetProperty, Sensitive=1
               self.polygonID -> SetProperty, Sensitive=1
               CatRefreshDraw, self._drawID_Pixmap, Stop_At='DrawWidget'
            ENDIF
            END

         'ANNOTATE_LAYER_OFF': BEGIN
            IF Obj_Valid(self.layerObject) THEN BEGIN
               self.layerObject -> SetProperty, Visible=0
               self.angleID -> SetProperty, Sensitive=0
               self.selectID -> SetProperty, Sensitive=0
               self.textID -> SetProperty, Sensitive=0
               self.boxID -> SetProperty, Sensitive=0
               self.arrowID -> SetProperty, Sensitive=0
               self.ellipseID -> SetProperty, Sensitive=0
               self.measureID -> SetProperty, Sensitive=0
               self.polygonID -> SetProperty, Sensitive=0
               CatRefreshDraw, self._drawID_Pixmap, Stop_At='DrawWidget'
            ENDIF
            END

         'BMP': BEGIN
            self._drawID -> SetWindow
            self._drawID -> Output, /BMP, FILENAME=self.output_filename + '.bmp'
            END

         'CANCEL':

         'COLOR_ALL': BEGIN

            ; Have to have at least one selected object.
            IF Ptr_Valid(self.selectedObjects) EQ 0 THEN RETURN

            (*self.selectedObjects)[0] -> GetProperty, Color=theColor
            theColor = PickColorName(theColor, Group_Leader=self._drawID->GetID())
            FOR j=0,N_Elements(*self.selectedObjects)-1 DO (*self.selectedObjects)[j] -> SetProperty, Color=theColor
            self._drawID -> Draw

            END

         'DELETE_ALL': BEGIN

            ; Have to have at least one selected object.
            IF Ptr_Valid(self.selectedObjects) EQ 0 THEN RETURN

            FOR j=0,N_Elements(*self.selectedObjects)-1 DO (*self.selectedObjects)[j] -> SetProperty, DELETE=1, NOREFRESH=1, NOMESSAGE=1
            self._drawID -> Draw, /Erase

            ; Copy window to the pixmap.
            self._drawID_pixmap -> SetWindow
            self._drawID -> Copy

            ; Free objects.
            Ptr_Free, self.selectedObjects

            END

         'DISTRIBUTE': BEGIN
            self -> DistributeGUI, event
            END

         'GLOBAL_PROPERTIES': BEGIN

            cp = OBJ_NEW ('CatControlPanel', event.id, COLUMN=1, $
               TITLE='Global Annotation Properties', /No_Cancel, /No_Apply, /No_OK)
            IF (NOT OBJ_VALID (cp)) THEN RETURN

            aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, Name='ANNOTATION PROPERTYSHEET', YSize=14)
            aproperties -> SetProperty, Event_Object=self

            ; Display the control panel if it created its own TLB.
            IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center
            self.selectID -> SetProperty, Set_Button=1
            END


         'GROUP': BEGIN
               IF Ptr_Valid(self.selectedObjects) EQ 0 THEN RETURN
               IF N_Elements(*self.selectedObjects) LT 2 THEN RETURN

               group = Obj_New('SelectableGroup', Member=*self.selectedObjects, $
                  NAME='SELECTABLE GROUP', MEMORY_MANAGEMENT=0)
               group -> CalculateBoundaryBox

               ; Find the parents of one of the group members. This is where we
               ; are going to add the new group object.
               (*self.selectedObjects)[0] -> GetProperty, Parents=parents
               FOR j=0, N_Elements(parents)-1 DO BEGIN
                  parents[j] -> Add, group
                  IF Obj_Isa_Valid(parents[j], "PIXMAPWIDGET") THEN parents[j] -> Refresh, Requester=self
               ENDFOR

               CatRefreshDraw, group, Stop_At='DrawWidget'
               group -> DrawSelectionBox
               *self.selectedObjects = group

            END

          'INSERT_ANGLE': BEGIN
            self.defAngleObject -> GetProperty, $
               ARROWHEAD_SIZE=arrowhead_size, $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               COLOR=color, $
               CLOCKWISE=clockwise, $
               COORD_OBJECT=coord_object, $
               RADIANS=radians, $
               STATUSBAR=statusbar, $
               LAYER=layer, $
               LINESTYLE=linestyle, $
               TEXTCOLOR=textcolor, $
               THICKNESS=thickness, $
               XPTS=xpts, $
               YPTS=ypts

            default_object = Obj_New('ANGLETOOL', $
               ARROWHEAD_SIZE=arrowhead_size, $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               COLOR=color, $
               CLOCKWISE=clockwise, $
               COORD_OBJECT=coord_object, $
               RADIANS=radians, $
               STATUSBAR=statusbar, $
               LAYER=layer, $
               LINESTYLE=linestyle, $
               TEXTCOLOR=textcolor, $
               THICKNESS=thickness, $
               XPTS=xpts, $
               YPTS=ypts)

            self -> SetProperty, Mode='INSERT', Default_Object=default_object
            END

          'INSERT_ARROW': BEGIN
            self.defArrowObject -> GetProperty, $
               ARROWHEAD=arrowhead, $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               COLOR=color, $
               COORD_OBJECT=coord_object, $
               HEADSIZE=headsize, $
               LAYER=layer, $
               THICKNESS=thickness

            default_object = Obj_New('ARROW', $
               ARROWHEAD=arrowhead, $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               COLOR=color, $
               COORD_OBJECT=coord_object, $
               HEADSIZE=headsize, $
               LAYER=layer, $
               THICKNESS=thickness)
            self -> SetProperty, Mode='INSERT', Default_Object=default_object
            END

          'INSERT_BOX': BEGIN
            self.defBoxObject -> GetProperty, $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               COLOR=color, $
               COORD_OBJECT=coord_object, $
               LAYER=layer, $
               LINESTYLE=linestyle, $
               THICKNESS=thickness
            default_object = Obj_New('BOX', $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               COLOR=color, $
               COORD_OBJECT=coord_object, $
               LAYER=layer, $
               LINESTYLE=linestyle, $
               THICKNESS=thickness)
            self -> SetProperty, Mode='INSERT', Default_Object=default_object
            END

          'INSERT_ELLIPSE': BEGIN
            self.defEllipseObject -> GetProperty, $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               COLOR=color, $
               COORD_OBJECT=coord_object, $
               LAYER=layer, $
               LINESTYLE=linestyle, $
               THICKNESS=thickness
            default_object = Obj_New('ELLIPSE', $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               COLOR=color, $
               COORD_OBJECT=coord_object, $
               LAYER=layer, $
               LINESTYLE=linestyle, $
               THICKNESS=thickness)
            self -> SetProperty, Mode='INSERT', Default_Object=default_object
            END

          'INSERT_MEASURE': BEGIN
            self.defMeasureObject -> GetProperty, $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               COORD_OBJECT=coord_object, $
               COLOR=color, $
               FORMAT=format, $
               HEADSIZE=headsize, $
               LAYER=layer, $
               THICKNESS=thickness, $
               UNITS=units
            default_object = Obj_New('TAPEMEASURE', $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               COORD_OBJECT=coord_object, $
               COLOR=color, $
               FORMAT=format, $
               HEADSIZE=headsize, $
               LAYER=layer, $
               THICKNESS=thickness, $
               UNITS=units)
            self -> SetProperty, Mode='INSERT', Default_Object=default_object
            END

          'INSERT MODE': BEGIN
            self -> SetProperty, Mode='INSERT'
            END

          'INSERT_POLYGON': BEGIN
            self.defPolygonObject -> GetProperty, $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               COLOR=color, $
               COORD_OBJECT=coord_object, $
               LAYER=layer, $
               LINESTYLE=linestyle, $
               THICKNESS=thickness
            default_object = Obj_New('POLYGON', $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               COLOR=color, $
               COORD_OBJECT=coord_object, $
               LAYER=layer, $
               LINESTYLE=linestyle, $
               THICKNESS=thickness)
            self -> SetProperty, Mode='INSERT', Default_Object=default_object
            END

          'INSERT_TEXT': BEGIN
            self.defTextObject -> GetProperty, $
               ALIGNMENT=alignment, $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               CHARSIZE=charsize, $,
               COLOR=color, $
               COORD_OBJECT=coord_object, $
               FONT=font, $
               LAYER=layer, $
               ORIENTATION=orientation, $
               THICKNESS=thickness
            default_object = Obj_New('TEXTLINE',  "", NAME='Freddy', $
               ALIGNMENT=alignment, $
               BACKGROUND=background, $
               BG_COLOR=bg_color, $
               CHARSIZE=charsize, $,
               COLOR=color, $
               COORD_OBJECT=coord_object, $
               FONT=font, $
               LAYER=layer, $
               ORIENTATION=orientation, $
               THICKNESS=thickness)

            self -> SetProperty, Mode='INSERT', Default_Object=default_object
            END

         'JPEG': BEGIN
            self._drawID -> SetWindow
            self._drawID -> Output, /JPEG, FILENAME=self.output_filename + '.jpg'
            END

         'PNG': BEGIN
            self._drawID -> SetWindow
            self._drawID -> Output, /PNG, FILENAME=self.output_filename + '.png'
            END

         'POSTSCRIPT': BEGIN
            self._drawID -> SetWindow
            thisDevice = !D.Name
            keywords = PSConfig(Cancel=cancelled, FILENAME=self.output_filename + '.ps', _Extra=PSWindow())
            IF cancelled THEN RETURN
            Set_Plot, 'PS'
            Device, _Extra=keywords
            self._drawID -> Draw
            Device, /Close
            Set_Plot, thisDevice
            END


          'SELECT MODE': BEGIN
            self -> SetProperty, Mode='SELECT'
            END

          'SELECT_OBJECTS': BEGIN
            self -> SetProperty, Mode='SELECT'
            END

          'TIFF': BEGIN
            self._drawID -> SetWindow
            self._drawID -> Output, /TIFF, FILENAME=self.output_filename + '.tif'
            END

          'UNGROUP': BEGIN

               ; You can only ungroup a group.
               IF Obj_Isa_Valid((*self.selectedObjects)[0], 'SELECTABLEGROUP') EQ 0 THEN RETURN
               theGroup = (*self.selectedObjects)[0]
               theGroup -> Ungroup, self.selectedObjects

               ; Remove the group from all its parents
               theGroup -> GetProperty, Parents=parents
               FOR j=0,N_Elements(parents)-1 DO parents[j] -> Remove, theGroup
               self._drawID_pixmap -> Refresh
               self._drawID -> Refresh
               FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                  (*self.selectedObjects)[j] -> DrawSelectionBox
               ENDFOR

            END

          ELSE: Print, 'Unknown button event in AnnotateInteraction::EventHandler.'

      ENDCASE

      RETURN ; from WIDGET_BUTTON events.

   ENDIF

   ; Handle property sheet events here.
   IF StrMid(event.event_name, 0, 16) EQ 'WIDGET_PROPSHEET' THEN BEGIN
      CASE event.event_name OF
         'WIDGET_PROPSHEET_CHANGE': BEGIN
            IF event.type EQ 0 THEN BEGIN

               CASE StrUpCase(event.identifier) OF

                  'BG_COLOR': BEGIN

                     event.component -> GetProperty, BG_Color=bg_color
                     event.id -> GetProperty, ID=group_leader
                     bg_color = PickColorName(color, Group_Leader=group_leader)
                     event.component -> SetProperty, BG_Color=bg_color
                     END

                  'COLOR': BEGIN

                     event.component -> GetProperty, Color=color
                     event.id -> GetProperty, ID=group_leader
                     color = PickColorName(color, Group_Leader=group_leader)
                     event.component -> SetProperty, Color=color
                     END

                  'DEF_ANGLE_OBJECT': BEGIN
                     event.component -> GetProperty, DEF_ANGLE_OBJECT=def_angle_object
                     def_angle_object -> ControlPanel, Group_Leader=event.id
                     END

                  'DEF_ARROW_OBJECT': BEGIN
                     event.component -> GetProperty, DEF_ARROW_OBJECT=def_arrow_object
                     def_arrow_object -> ControlPanel, Group_Leader=event.id
                     END

                  'DEF_BOX_OBJECT': BEGIN
                     event.component -> GetProperty, DEF_BOX_OBJECT=def_box_object
                     def_box_object -> ControlPanel, Group_Leader=event.id
                     END

                  'DEF_ELLIPSE_OBJECT': BEGIN
                     event.component -> GetProperty, DEF_ELLIPSE_OBJECT=def_ellipse_object
                     def_ellipse_object -> ControlPanel, Group_Leader=event.id
                     END

                  'DEF_POLYGON_OBJECT': BEGIN
                     event.component -> GetProperty, DEF_POLYGON_OBJECT=def_polygon_object
                     def_polygon_object -> ControlPanel, Group_Leader=event.id
                     END

                  'DEF_MEASURE_OBJECT': BEGIN
                     event.component -> GetProperty, DEF_MEASURE_OBJECT=def_measure_object
                     def_measure_object -> ControlPanel, Group_Leader=event.id
                     END

                  'DEF_TEXT_OBJECT': BEGIN
                     event.component -> GetProperty, DEF_TEXT_OBJECT=def_text_object
                     def_text_object -> ControlPanel, Group_Leader=event.id
                     END


                  ELSE: BEGIN

                     component = event.component
                     identifier = event.identifier
                     event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                     event.component -> SetPropertyByIdentifier, identifier, value
                     END

               ENDCASE
            ENDIF
            END
         ELSE:
      ENDCASE
      RETURN
   ENDIF

   ; What kind of event is this? Only draw widget events should get this far.
   eventTypes = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSED', 'CHARACTER', 'KEYPRESS_CH', 'KEYPRESS_KEY']
   thisEvent = eventTypes[event.type]

   ; Everything depends on the current MODE.
   CASE self._mode OF

      'SELECT': BEGIN

          CASE thisEvent OF

            'DOWN': BEGIN

               ; Did you click in a selectable object?
               objects = self._drawID -> SelectObjects(event.x, event.y, Count=count)

               ; Did you find a selectable object?
                IF count GT 0 THEN BEGIN

                  ; Found a selectable object.
                  FOR j=0,count-1 DO BEGIN

                    theObject = (Reverse(objects))[j]

                    ; Search for the first SELECTABLEOBJECT object you find.
                    IF Obj_Isa_Valid(theObject, 'SELECTABLEOBJECT') THEN BEGIN

                       ; The pixmap is interested in know if this object is ever deleted.
                       theObject -> RegisterForMessage, self._drawID_pixmap, 'OBJECT_DELETED'

                       ; Did you click the LEFT mouse button? Then we are moving the object.
                       IF event.press EQ 1 THEN BEGIN

                           ; If we didn't use a modifier key, then find out if we have
                           ; a single object or whether this object is already in a group
                           ; of objects.
                          IF event.modifiers EQ 0 THEN BEGIN
                             IF Ptr_Valid(self.selectedObjects) THEN BEGIN
                                selectedObjects = *self.selectedObjects
                                i = Where(selectedObjects EQ theObject, cnt)
                                IF cnt EQ 0 THEN BEGIN
                                    *self.selectedObjects = theObject
                                    singleObjectFlag = 1
                                ENDIF ELSE BEGIN
                                    IF N_Elements(selectedObjects) GT 1 THEN BEGIN
                                       singleObjectFlag = 0
                                    ENDIF ELSE BEGIN
                                       singleObjectFlag = 1
                                    ENDELSE
                                ENDELSE
                             ENDIF ELSE BEGIN
                                    self.selectedObjects = Ptr_New(theObject)
                                    singleObjectFlag = 1
                             ENDELSE

                            ; If this is a single object, pass the SELECT event on to its InteractionEvents handler.
                            IF singleObjectFlag THEN theObject -> InteractionEvents, event, Interaction=self

                          ENDIF

                         ; If we used a SHIFT key, then we are adding to the selection.
                         IF event.modifiers EQ 1 THEN BEGIN
                            IF Ptr_Valid(self.selectedObjects) THEN BEGIN
                               index = Where(*self.selectedObjects EQ theObject, count)
                               IF count EQ 0 THEN (*self.selectedObjects) = [*self.selectedObjects,theObject]
                            ENDIF ELSE BEGIN
                               self.selectedObjects = Ptr_New(theObject)
                            ENDELSE
                         ENDIF

                          ; New mode. Save the cursor location.
                          IF self._mode EQ 'SELECT' THEN self._mode = 'MOVE'
                          self.sx = event.x
                          self.sy = event.y

                         ; Motion events on. Make sure the pixmap is the same size
                          ; as the window. Could *create* pixmap here, but I think
                          ; this is faster.
                          self._drawID -> SetProperty, MOTION_EVENTS=1
                          self._drawID -> GetProperty, XSize=xsize, YSize=ysize
                          self._drawID_pixmap -> GetProperty, XSize=pxsize, YSize=pysize
                          IF (xsize NE pxsize) OR (ysize NE pysize) THEN BEGIN
                              self._drawID_pixmap -> SetProperty, XSize=xsize, YSize=ysize
                          ENDIF

                          ; Quick draw of pixmapwindow with this selected objects turned off. Want
                          ; this for quick re-draw during movement.
                           FOR j=0,N_Elements(*self.selectedObjects)-1 DO $
                                 (*self.selectedObjects)[j] -> SetProperty, Visible=0, /NoMessage, /NoRefresh
                          self._drawID_pixmap -> Refresh

                          ; Copy pixmap contents to window.
                          self._drawID -> SetWindow
                          self._drawID_pixmap -> Copy

                          ; Draw the selected object in the window.
                          self._drawID -> SetWindow
                          FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                             (*self.selectedObjects)[j] -> SetProperty, Visible=1, /NoMessage, /NoRefresh
                             (*self.selectedObjects)[j] -> Draw
                             (*self.selectedObjects)[j] -> DrawSelectionBox
                          ENDFOR

                          ; Can only select one at a time.
                          BREAK

                       ENDIF

                       ; User clicked the RIGHT button in selectable object. Access the
                       ; selectable object's select panel.
                       IF event.press EQ 4 THEN BEGIN
                          IF Ptr_Valid(self.selectedObjects) THEN BEGIN
                             IF N_Elements(*self.selectedObjects) GT 1 THEN BEGIN
                                  self -> BuildMultiSelectMenu
                                  Widget_DisplayContextMenu, event.id -> GetID(), event.x+10, event.y-1, $
                                    self._contextMenu->GetID()
                                  BREAK ; Only one context menu for the group.
                             ENDIF ELSE BEGIN
                               IF Obj_Isa_Valid(theObject, 'SELECTABLEGROUP') THEN BEGIN
                                  Obj_Destroy, self._contextMenu
                                   self._contextMenu = Obj_New('ContextMenuBase', self._drawID, Column=1, Event_Object=self)
                                  button = Obj_New('ButtonWidget', self._contextMenu, Value='Ungroup', Name='UNGROUP')
                                  Widget_DisplayContextMenu, event.id -> GetID(), event.x+10, event.y-1, $
                                    self._contextMenu->GetID()
                                  BREAK ; Only one context menu for the group.
                               ENDIF ELSE BEGIN
                                    theObject -> SelectPanel, event.x, event.y, self._drawID
                                  BREAK
                               ENDELSE
                             ENDELSE
                          ENDIF
                       ENDIF

                    ENDIF

                  ENDFOR

               ENDIF ELSE BEGIN

                  ; Clicked RIGHT BUTTTON outside a selectable object. Get the Annotation
                  ; ControlPanel. Nothing will appear if the control panel already exists. For
                  ; example, it might already be embedded somewhere, as in the Catalyst application.
                  ; Otherwise, the Annotation ControlPanel will pop up next to where the user has clicked.
                  IF event.press EQ 4 THEN BEGIN
                      tlb = CatGetTopObject(self._drawID)
                      tlb -> GetProperty, XOffset=xoff, YOffset=yoff
                      self -> ControlPanel, Group_Leader=self._drawID_pixmap, XOffset=xoff+event.x, YOffset=yoff+event.y, Row=1
                  ENDIF

                  ; If there is a valid current selectable object,
                  ; redraw to remove selection and remove selected object.
                  IF Ptr_Valid(self.selectedObjects) THEN BEGIN

                     ; Redraw the window.
                     self._drawID_pixmap->Refresh

                     ; Copy window contents from pixmap to window.
                     self._drawID -> SetWindow
                     self._drawID_pixmap -> Copy

                  ENDIF
                  Ptr_Free, self.selectedObjects

               ENDELSE

               END

            ELSE:

          ENDCASE ; of SELECT DOWN

          END ; of Case SELECT

      'MOVE': BEGIN

         CASE thisEvent OF

           'DOWN': BEGIN

                 IF Ptr_Valid(self.selectedObjects) THEN BEGIN
                    self.sx = event.x
                    self.sy = event.y
                    self._drawID -> SetProperty, MOTION_EVENTS=1
                 ENDIF

              END

           'UP': BEGIN

              self._drawID -> SetProperty, Motion_Events=0, /Clear_Events
              self -> SetProperty, Mode='SELECT'
              self._drawID_pixmap -> Refresh
              self._drawID -> SetWindow
              ;self._drawID_pixmap -> Copy
              IF Ptr_Valid(self.selectedObjects) THEN BEGIN
                 FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                    (*self.selectedObjects)[j] -> CopyParameters, self._drawID, Destination=d, Extent=e
                    self._drawID_pixmap -> Copy, Destination=d, Extent=e, Origin=d
                    (*self.selectedObjects)[j] -> DrawSelectionBox
                 ENDFOR
              ENDIF
              END



           'MOTION': BEGIN

                 IF Ptr_Valid(self.selectedObjects) THEN BEGIN
                    deltaX = event.x - self.sx
                    deltaY = event.y - self.sy
                    self._drawID -> SetWindow
                    ;self._drawID_pixmap -> Copy
                    FOR j=0,N_Elements(*self.selectedObjects)-1 DO BEGIN
                       (*self.selectedObjects)[j] -> CopyParameters, self._drawID, Destination=d, Extent=e
                       self._drawID_pixmap -> Copy, Destination=d, Extent=e, Origin=d
                       (*self.selectedObjects)[j] -> Move, deltaX, deltaY, /NoDraw
                       (*self.selectedObjects)[j] -> Draw
                       (*self.selectedObjects)[j] -> DrawSelectionBox
                    ENDFOR
                    self.sx = event.x
                    self.sy = event.y
                 ENDIF

              END

           ELSE:

         ENDCASE ; of thisEvent in MOVE

         END ; of case MOVE

      ; If some mode comes in that is not handled here, then the event
      ; is passed on to the INTERACTIONEVENTS method of the current interaction
      ; object.
      ELSE: BEGIN

         IF Obj_Valid(self.defaultObject) THEN BEGIN
            self.defaultObject -> InteractionEvents, event, Interaction=self
            self -> GetProperty, Mode=currentMode

            ; If the object "finishes" an operation and we get an UP event, then we have to do
            ; the appropriate thing, which in this case is go into SELECT mode.
            IF StrMid(StrUpCase(currentMode), 0, 8) EQ 'FINISHED' THEN self -> SetProperty, Mode='SELECT'

            ; If we are finished with an INSERTION event, then we should destroy the default object.
            IF StrUpCase(currentMode) EQ 'FINISHED_INSERT' THEN self -> SetProperty, Default_Object=Obj_New()

         ENDIF ELSE BEGIN
            IF Ptr_Valid(self.selectedObjects) THEN BEGIN
               IF Obj_Valid((*self.selectedObjects)[0]) THEN $
                  (*self.selectedObjects)[0] -> InteractionEvents, event, Interaction=self
               self -> GetProperty, Mode=currentMode
               IF StrMid(StrUpCase(currentMode), 0, 8) EQ 'FINISHED' THEN self -> SetProperty, Mode='SELECT'
            ENDIF
         ENDELSE

         END

   ENDCASE ; of self._mode


   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain ANNOTATEINTERACTION properties. Be sure
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
;     BACKGROUND:         Set this keyword to draw a background for the text.
;
;     BG_COLOR:           The name of the background color. By default, "white".
;
;     COLOR:              Set this keyword to the name of a color for the text. By default, "black".
;
;     DEF_ARROW_OBJECT:   A default arrow object. New objects are created with this object's properties.
;
;     DEF_BOX_OBJECT:     A default box object. New objects are created with this object's properties.
;
;     DEF_ELLIPSE_OBJECT: A default ellipse object. New objects are created with this object's properties.
;
;     DEF_MEASURE_OBJECT: A default tapemeasure object. New objects are created with this object's properties.
;
;     DEF_POLYGON_OBJECT: A default polygon object. New objects are created with this object's properties.
;
;     DEF_TEXT_OBJECT:    A default text object. New objects are created with this object's properties.
;
;     DEFAULT_OBJECT:     The current default object.
;
;     LAYER:              The current annotation CATLAYER object.
;
;     LINESTYLE:          The default linestyle for objects created with this object. By default, 0.
;
;     MODE:               The current object mode.
;
;     THICKNESS:          The default thickness of lines created by annotation objects. By default, 2.
;
;     PIXMAPWIDGET:       The object reference to any pixmap widget that is created.
;
;     SELECTEDOBJECTS:    Any objects currently selected.
;
;     _REF_EXTRA:         Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO AnnotateInteraction::GetProperty, $
    BACKGROUND=background, $
    BG_COLOR=bg_color, $
    COLOR=color, $
    DEF_ARROW_OBJECT=def_arrow_object, $
    DEF_ANGLE_OBJECT=def_angle_object, $
    DEF_BOX_OBJECT=def_box_object, $
    DEF_ELLIPSE_OBJECT=def_ellipse_object, $
    DEF_MEASURE_OBJECT=def_measure_object, $
    DEF_POLYGON_OBJECT=def_polygon_object, $
    DEF_TEXT_OBJECT=def_text_object, $
    DEFAULT_OBJECT=default_object, $
    LAYER=layer, $
    LINESTYLE=linestyle, $
    MODE=mode, $
    THICKNESS=thickness, $
    PIXMAPWIDGET=pixmap, $
    SELECTEDOBJECTS=selectedObjects, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler


   IF Arg_Present(background) THEN background = self._background
   IF Arg_Present(bg_color) THEN bg_color = self._bg_color
   IF Arg_Present(color) THEN color = self._color
   IF Arg_Present(default_object) THEN default_object = self.defaultObject
   IF Arg_Present(def_angle_object) THEN def_angle_object = self.defAngleObject
   IF Arg_Present(def_arrow_object) THEN def_arrow_object = self.defArrowObject
   IF Arg_Present(def_box_object) THEN def_box_object = self.defBoxObject
   IF Arg_Present(def_ellipse_object) THEN def_ellipse_object = self.defEllipseObject
   IF Arg_Present(def_measure_object) THEN def_measure_object = self.defMeasureObject
   IF Arg_Present(def_polygon_object) THEN def_polygon_object = self.defPolygonObject
   IF Arg_Present(def_text_object) THEN def_text_object = self.defTextObject
   IF Arg_Present(layer) THEN layer = self.layerObject
   IF Arg_Present(linestyle) THEN linestyle = self._linestyle
   IF Arg_Present(mode) THEN mode = self._mode
   IF Arg_Present(pixmap) THEN pixmap = self._drawID_pixmap
   IF Arg_Present(selectedObjects) THEN selectedObjects = self.selectedObjects
   IF Arg_Present(thickness) THEN thickness = self._thickness
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> INTERACTION::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::RESTOREDISPLAY
;
; PURPOSE:
;
;       This method restores the draw widget to its former state.
;
; SYNTAX:
;
;       theObject -> RestoreDisplay
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
PRO AnnotateInteraction::RestoreDisplay

   @cat_pro_error_handler

   ; Call superclass method.
   self -> INTERACTION::RestoreDisplay

   ; Set the cursor back to its original shape.
   Device, /Cursor_Original

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::SETDISPLAY
;
; PURPOSE:
;
;       This method takes over event handling from the draw widget and
;       sets up the object so that everything can be restored to the way
;       it was when the interaction is finished.
;
; SYNTAX:
;
;       theObject -> SetDisplay
;
; ARGUMENTS:
;
;       drawWidget:   The draw widget used in the interaction. (Optional argument.)
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO AnnotateInteraction::SetDisplay, drawWidget

   @cat_pro_error_handler

   self -> INTERACTION::SetDisplay, drawWidget

   ; Set SELECT mode with arrow cursor.
   ;self -> SetProperty, Mode='SELECT'

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the ANNOTATEINTERACTION object's properties. Be sure
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
;     The following keywords set default properties for new objects created in this interaction.
;
;     BACKGROUND:         Set this keyword to draw a background for the text.
;
;     BG_COLOR:           The name of the background color. By default, "white".
;
;     COLOR:              Set this keyword to the name of a color for the text. By default, "black".
;
;     DEF_ANGLE_OBJECT:   A default ANGLETOOL object. New objects are created with this object's properties.
;
;     DEF_ARROW_OBJECT:   A default arrow object. New objects are created with this object's properties.
;
;     DEF_BOX_OBJECT:     A default box object. New objects are created with this object's properties.
;
;     DEF_ELLIPSE_OBJECT: A default ellipse object. New objects are created with this object's properties.
;
;     DEF_MEASURE_OBJECT: A default tapemeasure object. New objects are created with this object's properties.
;
;     DEF_POLYGON_OBJECT: A default polygon object. New objects are created with this object's properties.
;
;     DEF_TEXT_OBJECT:    A default text object. New objects are created with this object's properties.
;
;     DEFAULT_OBJECT:     Set this keyword to an object reference to a SELECTABLEOBJECT object. By default,
;                         is set to a DEF_TEXT_OBJECT.
;
;     LAYER:              A CATLAYER object for holding the annotation objects. The old annotation layer
;                         and everything it contains will be destroyed.
;
;     LINESTYLE:          The default linestyle for objects created with this object. By default, 0.
;
;     MODE:               The current object mode.
;
;     THICKNESS:          The default thickness of lines created by annotation objects. By default, 2.
;
;     _EXTRA:             Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO AnnotateInteraction::SetProperty, $
   BACKGROUND=background, $
   BG_COLOR=bg_color, $
   COLOR=color, $
   DEF_ANGLE_OBJECT=def_angle_object, $
   DEF_ARROW_OBJECT=def_arrow_object, $
   DEF_BOX_OBJECT=def_box_object, $
   DEF_ELLIPSE_OBJECT=def_ellipse_object, $
   DEF_MEASURE_OBJECT=def_measure_object, $
   DEF_POLYGON_OBJECT=def_polygon_object, $
   DEF_TEXT_OBJECT=def_text_object, $
   DEFAULT_OBJECT=default_object, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   MODE=mode, $
   THICKNESS=thickness, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(default_object) NE 0 THEN BEGIN
      Obj_Destroy, self.defaultObject
      self.defaultObject = default_object
   ENDIF

   IF N_Elements(def_angle_object) NE 0 THEN BEGIN
      Obj_Destroy, self.defAngleObject
      self.defAngleObject = def_angle_object
   ENDIF

   IF N_Elements(def_arrow_object) NE 0 THEN BEGIN
      Obj_Destroy, self.defArrowObject
      self.defArrowObject = def_arrow_object
   ENDIF

   IF N_Elements(def_box_object) NE 0 THEN BEGIN
      Obj_Destroy, self.defBoxObject
      self.defBoxObject = def_box_object
   ENDIF

   IF N_Elements(def_ellipse_object) NE 0 THEN BEGIN
      Obj_Destroy, self.defEllipseObject
      self.defEllipseObject = def_ellipse_object
   ENDIF

   IF N_Elements(def_measure_object) NE 0 THEN BEGIN
      Obj_Destroy, self.defMeasureObject
      self.defMeasureObject = def_measure_object
   ENDIF

   IF N_Elements(def_polygon_object) NE 0 THEN BEGIN
      Obj_Destroy, self.defPolygonObject
      self.defPolygonObject = def_polygon_object
   ENDIF

   IF N_Elements(def_text_object) NE 0 THEN BEGIN
      Obj_Destroy, self.defTextObject
      self.defTextObject = def_text_object
   ENDIF

   IF N_Elements(layer) NE 0 THEN BEGIN


      IF Obj_Valid(self.layerObject) THEN BEGIN

         ; Remove the layer object from the draw widget and pixmap.
         self._drawID -> Remove, self.layerObject
         self._drawID_pixmap -> Remove, self.layerObject

         ; Removing all the parents of the layer object (probably destroying it, unless
         ; someone has registered specific interest by setting AUTO_DESTROY=0).
         self.layerObject -> GetProperty, Parents=parents
         FOR j=0,N_Elements(parents)-1 DO self.layerObject -> RemoveParent, parents[j]

      ENDIF

      ; Add the new layer. Register your interest in it.
      self.layerObject = layer
      self.layerObject -> AddParent, self

      ; Add the new layer to each of the default objects.
      self.defaultObject -> SetProperty, Layer=layer
      self.defAngleObject -> SetProperty, Layer=layer
      self.defArrowObject -> SetProperty, Layer=layer
      self.defBoxObject -> SetProperty, Layer=layer
      self.defEllipseObject -> SetProperty, Layer=layer
      self.defTextObject -> SetProperty, Layer=layer
      self.defMeasureObject -> SetProperty, Layer=layer
      self.defPolygonObject -> SetProperty, Layer=layer

      ; Add the layer object to the draw widget and pixmap.
      self._drawID -> Add, self.layerObject
      self._drawID_pixmap -> Add, self.layerObject

   ENDIF

   ; Global properties.
   IF N_Elements(background) NE 0 THEN BEGIN
      self.defAngleObject -> SetProperty, BACKGROUND=background, /NoRefresh, /NoMessage
      self.defArrowObject -> SetProperty, BACKGROUND=background, /NoRefresh, /NoMessage
      self.defBoxObject -> SetProperty, BACKGROUND=background, /NoRefresh, /NoMessage
      self.defEllipseObject -> SetProperty, BACKGROUND=background, /NoRefresh, /NoMessage
      self.defTextObject -> SetProperty, BACKGROUND=background, /NoRefresh, /NoMessage
      self.defMeasureObject -> SetProperty, BACKGROUND=background, /NoRefresh, /NoMessage
      self.defPolygonObject -> SetProperty, BACKGROUND=background, /NoRefresh, /NoMessage
      self._background = background
   ENDIF

   IF N_Elements(bg_color) NE 0 THEN BEGIN
      self.defAngleObject -> SetProperty, BG_COLOR=bg_color, /NoRefresh, /NoMessage
      self.defArrowObject -> SetProperty, BG_COLOR=bg_color, /NoRefresh, /NoMessage
      self.defBoxObject -> SetProperty, BG_COLOR=bg_color, /NoRefresh, /NoMessage
      self.defEllipseObject -> SetProperty, BG_COLOR=bg_color, /NoRefresh, /NoMessage
      self.defTextObject -> SetProperty, BG_COLOR=bg_color, /NoRefresh, /NoMessage
      self.defMeasureObject -> SetProperty, BG_COLOR=bg_color, /NoRefresh, /NoMessage
      self.defPolygonObject -> SetProperty, BG_COLOR=bg_color, /NoRefresh, /NoMessage
      self._bg_color = bg_color
   ENDIF

   IF N_Elements(color) NE 0 THEN BEGIN
      self.defAngleObject -> SetProperty, COLOR=color, TEXTCOLOR=color, /NoRefresh, /NoMessage
      self.defArrowObject -> SetProperty, COLOR=color, /NoRefresh, /NoMessage
      self.defBoxObject -> SetProperty, COLOR=color, /NoRefresh, /NoMessage
      self.defEllipseObject -> SetProperty, COLOR=color, /NoRefresh, /NoMessage
      self.defTextObject -> SetProperty, COLOR=color, /NoRefresh, /NoMessage
      self.defMeasureObject -> SetProperty, COLOR=color, /NoRefresh, /NoMessage
      self.defPolygonObject -> SetProperty, COLOR=color, /NoRefresh, /NoMessage
      self._color = color
   ENDIF

   IF N_Elements(linestyle) NE 0 THEN BEGIN
      self.defAngleObject -> SetProperty, LINESTYLE=linestyle, /NoRefresh, /NoMessage
      self.defArrowObject -> SetProperty, LINESTYLE=linestyle, /NoRefresh, /NoMessage
      self.defBoxObject -> SetProperty, LINESTYLE=linestyle, /NoRefresh, /NoMessage
      self.defEllipseObject -> SetProperty, LINESTYLE=linestyle, /NoRefresh, /NoMessage
      self.defMeasureObject -> SetProperty, LINESTYLE=linestyle, /NoRefresh, /NoMessage
      self.defPolygonObject -> SetProperty, LINESTYLE=linestyle, /NoRefresh, /NoMessage
      self._linestyle = linestyle
   ENDIF

   IF N_Elements(thickness) NE 0 THEN BEGIN
      self.defAngleObject -> SetProperty, THICKNESS=thickness, /NoRefresh, /NoMessage
      self.defArrowObject -> SetProperty, THICKNESS=thickness, /NoRefresh, /NoMessage
      self.defBoxObject -> SetProperty, THICKNESS=thickness, /NoRefresh, /NoMessage
      self.defEllipseObject -> SetProperty, THICKNESS=thickness, /NoRefresh, /NoMessage
      self.defMeasureObject -> SetProperty, THICKNESS=thickness, /NoRefresh, /NoMessage
      self.defPolygonObject -> SetProperty, THICKNESS=thickness, /NoRefresh, /NoMessage
      self._thickness = thickness
   ENDIF

   IF N_Elements(mode) NE 0 THEN BEGIN
      CASE StrUpCase(mode) OF

         'INSERT': BEGIN

            self._mode = 'INSERT'

            IF Obj_Isa_Valid(self.defaultObject, 'TEXTLINE') THEN BEGIN
               IF !D.Name EQ 'WIN' THEN BEGIN
                    Device, Cursor_Standard=32513L
               ENDIF ELSE BEGIN
                    Device, CURSOR_STANDARD=152
               ENDELSE
            ENDIF

            IF Obj_Isa_Valid(self.defaultObject, 'ARROW') THEN BEGIN
               IF !D.Name EQ 'WIN' THEN BEGIN
                    Device, Cursor_Standard=32515L
               ENDIF ELSE BEGIN
                    Device, CURSOR_STANDARD=30
               ENDELSE
            ENDIF

            IF Obj_Isa_Valid(self.defaultObject, 'BOX') THEN BEGIN
               IF !D.Name EQ 'WIN' THEN BEGIN
                    Device, Cursor_Standard=32515L
               ENDIF ELSE BEGIN
                    Device, CURSOR_STANDARD=30
               ENDELSE
            ENDIF

            END

         'WRITE': BEGIN

            self._mode = 'WRITE'
            IF !D.Name EQ 'WIN' THEN BEGIN
                 Device, Cursor_Standard=32513L ; Will have to modify for X windows.
            ENDIF ELSE BEGIN
                 Device, CURSOR_STANDARD=152
            ENDELSE

            END

         'SELECT': BEGIN ; Select mode

            self._mode = 'SELECT'
            IF !D.Name EQ 'WIN' THEN BEGIN
               Device, CURSOR_STANDARD=32512L
            ENDIF ELSE BEGIN
               Device, CURSOR_STANDARD=2
            ENDELSE
            IF Obj_Valid(self.selectID) THEN self.selectID->SetProperty, Set_Button=1
            END

         ELSE: self._mode = StrUpCase(mode)

      ENDCASE
   ENDIF

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> INTERACTION::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::CLEANUP
;
; PURPOSE:
;
;       This is the ANNOTATEINTERACTION object class destructor method.
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
PRO AnnotateInteraction::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self.selectedObjects
   Obj_Destroy, self._drawID_pixmap

   ; Remove interest in these objects.
   IF Obj_Valid(self.defTextObject) THEN self.defTextObject -> RemoveParent, self
   IF Obj_Valid(self.defaultObject) THEN self.defaultObject -> RemoveParent, self
   IF Obj_Valid(self.defArrowObject) THEN self.defArrowObject -> RemoveParent, self
   IF Obj_Valid(self.defAngleObject) THEN self.defAngleObject -> RemoveParent, self
   IF Obj_Valid(self.defBoxObject) THEN self.defBoxObject -> RemoveParent, self
   IF Obj_Valid(self.defEllipseObject) THEN self.defEllipseObject -> RemoveParent, self
   IF Obj_Valid(self.defPolygonObject) THEN self.defPolygonObject -> RemoveParent, self
   IF Obj_Valid(self.layerObject) THEN self.layerObject -> RemoveParent, self
   IF Obj_Valid(self.defMeasureObject) THEN self.defMeasureObject -> RemoveParent, self
   self -> INTERACTION::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       ANNOTATEINTERACTION::INIT
;
; PURPOSE:
;
;       This is the ANNOTATEINTERACTION object class initialization method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     drawObject:          The draw widget object that you will be taking over events from.
;
; KEYWORDS:
;
;     BACKGROUND:          Set this keyword to draw a background for the text.
;
;     BG_COLOR:            The name of the background color. By default, "black".
;
;     COLOR:               Set this keyword to the name of a color for the text. By default, "white".
;
;     COORD_OBJECT:        A coordinate object (CATCOORD) for establishing a coordinate system for the
;                          interaction objects. Currently, only the TAPEMEASURE uses a coordinate system.
;                          All other coordinate systems are normalized.
;
;     DEF_ANGLE_OBJECT:    An optional default ANGLETOOL object. New objects are created with this object's properties.
;
;     DEF_ARROW_OBJECT:    An optional default ARROW object. New objects are created with this object's properties.
;
;     DEF_BOX_OBJECT:      An optional default BOX object. New objects are created with this object's properties.
;
;     DEF_ELLIPSE_OBJECT:  An optional default ELLIPSE object. New objects are created with this object's properties.
;
;     DEF_MEASURE_OBJECT:  An optional default TAPEMEASURE object. New objects are created with this object's properties.
;
;     DEF_POLYGON_OBJECT:  An optional default POLYGON object. New objects are created with this object's properties.
;
;     DEF_TEXT_OBJECT:     An optional default TEXTLINE object. New objects are created with this object's properties.
;
;     DEFAULT_OBJECT:      Set this keyword to an object reference to a SELECTABLEOBJECT object. By default,
;                          is set to a DEF_TEXT_OBJECT. This object will be deleted when the ANNOTATEINTERACTION
;                          object is destroyed.
;
;     LAYER:               An optional CATLAYER object for holding the annotation objects. If a layer object is
;                          not provided, the annotations are placed directly in the drawObject container. Otherwise,
;                          the annotations are placed in the CATLAYER object, and the CATLAYER object is placed in
;                          the drawObject.
;
;     LINESTYLE:          The default linestyle for objects created with this object. By default, 0.
;     
;     OUTPUT_FILENAME:    The base name of the output filename used in SAVE AS operations. By default, "annotation".
;
;     THICKNESS:          The default thickness of lines created by annotation objects. By default, 2.
;
;     STATUSBAR:          A reference to a statusbar object. Messages can be sent from the interaction
;                         to the statusbar object, if supplied. The statusbar object is NOT destroyed by
;                         the interaction when the interaction is destroyed.
;
;     _EXTRA:              Any keywords appropriate for the superclass INIT method or DEFAULT_OBJECT INIT method.
;-
;*****************************************************************************************************
FUNCTION AnnotateInteraction::INIT, drawObject, $
   BACKGROUND=background, $
   BG_COLOR=bg_color, $
   COLOR=color, $
   COORD_OBJECT=coord_object, $
   DEF_ANGLE_OBJECT=def_angle_object, $
   DEF_ARROW_OBJECT=def_arrow_object, $
   DEF_BOX_OBJECT=def_box_object, $
   DEF_ELLIPSE_OBJECT=def_ellipse_object, $
   DEF_MEASURE_OBJECT=def_measure_object, $
   DEF_POLYGON_OBJECT=def_polygon_object, $
   DEF_TEXT_OBJECT=def_text_object, $
   DEFAULT_OBJECT=default_object, $
   LAYER=layer, $
   LINESTYLE=linestyle, $
   NAME=name, $
   OUTPUT_FILENAME=output_filename, $
   STATUSBAR=statusbar, $
   THICKNESS=thick, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   IF N_Elements(name) EQ 0 THEN name = 'Annotation'
   IF N_Elements(output_filename) EQ 0 THEN output_filename = 'annotation'

   ok = self -> INTERACTION::INIT (drawObject, STATUSBAR=statusbar, $
      DESCRIPTION='Global Annotation Properties', Name=name, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; Global properties.
   background = Keyword_Set(background)
   IF N_Elements(bg_color) EQ 0 THEN bg_color = 'Black'
   IF N_Elements(color) EQ 0 THEN color = 'White'
   IF N_Elements(linestyle) EQ 0 THEN linestyle = 0
   IF N_Elements(thick) EQ 0 THEN thick = 2

   ; Create a pixmap for fast re-drawing. Set it as the RefreshBuffer of the drawObject.
   drawObject -> GetProperty, XSize=xsize, YSize=ysize, Initial_Color=theColor
   self._drawID_pixmap = Obj_New('PixmapWidget', XSize=xsize, YSize=ysize, BackgroundColor=theColor)
   drawObject -> SetProperty, RefreshBuffer=self._drawID_pixmap

   ; Copy all the objects in the draw widget into the pixmap. This will really ONLY
   ; work if the objects that are copied into the pixmap widget will draw themselves
   ; anywhere. If they have specific internal directions (e.g., some image objects
   ; can be "attached" to specific draw widgets) for which draw widget to display
   ; themselves in, this is will not work. The symptom of a problem is something that
   ; is not drawing itself. (If it is an image, you may be getting just the white background
   ; of the pixmap.)
   objects = drawObject -> Get(/All)
   FOR j=0,N_Elements(objects)-1 DO self._drawID_pixmap -> Add, objects[j]
   self._drawID_pixmap -> Refresh

   ; If a coordinate object wasn't provided, try to copy one from the
   ; draw widget. If this isn't available, create a normalized one.
   IF N_Elements(coord_object) EQ 0 THEN BEGIN
      IF Obj_Valid(drawObject) THEN drawObject -> GetProperty, Coord_Object=coord_object
      IF Obj_Valid(coord_object) EQ 0 THEN BEGIN
         coord_object = Obj_New('CatCoord', XRange=[0,1], YRange=[0,1], Position=[0,0,1,1], Name='Default Coords')
         self -> AddToTrash, coord_object
      ENDIF
   ENDIF

   ; If you received an annotation layer object, regester your interest
   ; and add it to the drawObject and the pixmap.
   IF Obj_Isa_Valid(layer, "CatLayer") NE 0 THEN BEGIN
      self.layerObject = layer
      self.layerObject -> AddParent, self
      drawObject -> Add, layer
      self._drawID_pixmap -> Add, layer
   ENDIF

   ; Current mode.
   IF self._mode EQ '' THEN self._mode = 'INSERT'

   ; Check keywords.
   IF N_Elements(def_angle_object) EQ 0 THEN $
      def_angle_object = Obj_New('ANGLETOOL', Visible=0, Coord_Object=coord_object, Name='ANGLE TOOL', $
         Thick=thick, Color=color, Background=background, Linestyle=linestyle, BG_Color=bgcolor) ELSE $
         def_angle_object -> SetProperty, Visible=0
   def_angle_object -> SetProperty, Statusbar=statusbar

   IF N_Elements(def_arrow_object) EQ 0 THEN $
      def_arrow_object = Obj_New('ARROW', Visible=0, Coord_Object=coord_object, Name='ARROW TOOL', $
         Thick=thick, Color=color, Background=background, Linestyle=linestyle, BG_Color=bgcolor) ELSE $
         def_arrow_object -> SetProperty, Visible=0

   IF N_Elements(def_box_object) EQ 0 THEN $
      def_box_object = Obj_New('BOX', Visible=0, Coord_Object=coord_object, Name='BOX TOOL', $
         Thick=thick, Color=color, Background=background, Linestyle=linestyle, BG_Color=bgcolor) ELSE $
      def_box_object -> SetProperty, Visible=0

   IF N_Elements(def_ellipse_object) EQ 0 THEN $
      def_ellipse_object = Obj_New('ELLIPSE', Visible=0, Coord_Object=coord_object, Name='ELLIPSE TOOL', $
         Thick=thick, Color=color, Background=background, Linestyle=linestyle, BG_Color=bgcolor) ELSE $
         def_ellipse_object -> SetProperty, Visible=0

   IF N_Elements(def_measure_object) EQ 0 THEN $
      def_measure_object = Obj_New('TAPEMEASURE', Coord_Object=coord_object, Visible=0, Name='MEASURE TOOL', $
         Thick=thick, Color=color, Background=background, Linestyle=linestyle, BG_Color=bgcolor) ELSE $
         def_measure_object -> SetProperty, Visible=0

   IF N_Elements(def_polygon_object) EQ 0 THEN $
      def_polygon_object = Obj_New('POLYGON', Visible=0, Coord_Object=coord_object, Name='POLYGON TOOL', $
         Thick=thick, Color=color, Background=background, Linestyle=linestyle, BG_Color=bgcolor) ELSE $
         def_polygon_object -> SetProperty, Visible=0

   IF N_Elements(def_text_object) EQ 0 THEN $
      def_text_object = Obj_New('TEXTLINE', Visible=0, Coord_Object=coord_object, Name='TEXT TOOL', $
         Thick=thick, Color=color, Background=background, BG_Color=bgcolor) ELSE $
         def_text_object -> SetProperty, Visible=0

   ; Register for RESIZE messages from the draw widget.
   drawObject -> RegisterForMessage, self._drawID_pixmap, 'RESIZEDRAWWIDGET'

   ; Create a default object. Properties from this object
   ; will be used to create new TEXTLINE objects.
   IF N_Elements(default_object) EQ 0 THEN BEGIN
      def_text_object -> GetProperty, $
         ALIGNMENT=alignment, $
         BACKGROUND=background, $
         BG_COLOR=bg_color, $
         CHARSIZE=charsize, $,
         COLOR=color, $
         COORD_OBJECT=coord_object, $
         FONT=font, $
         ORIENTATION=orientation, $
         THICKNESS=thickness, $
         VISIBLE=visible
      default_object = Obj_New('TEXTLINE',  "", $
         ALIGNMENT=alignment, $
         BACKGROUND=background, $
         BG_COLOR=bg_color, $
         CHARSIZE=charsize, $,
         COORD_OBJECT=coord_object, $
         COLOR=color, $
         FONT=font, $
         ORIENTATION=orientation, $
         THICKNESS=thickness, $
         VISIBLE=visible)
   ENDIF

   ; Delete the normal context menu and create your own.
   self -> BuildMultiSelectMenu

   ; Load the object.
   self.defaultObject = default_object
   self.defAngleObject = def_angle_object
   self.defArrowObject = def_arrow_object
   self.defBoxObject = def_box_object
   self.defEllipseObject = def_ellipse_object
   self.defTextObject = def_text_object
   self.defPolygonObject = def_polygon_object
   self.defMeasureObject = def_measure_object
   self.output_filename = output_filename

   ; Is there a layer object?
   IF Obj_Valid(self.layerObject) THEN BEGIN
      self.defaultObject -> SetProperty, Layer=layer
      self.defAngleObject -> SetProperty, Layer=layer
      self.defArrowObject  -> SetProperty, Layer=layer
      self.defBoxObject  -> SetProperty, Layer=layer
      self.defEllipseObject  -> SetProperty, Layer=layer
      self.defTextObject  -> SetProperty, Layer=layer
      self.defPolygonObject  -> SetProperty, Layer=layer
      self.defMeasureObject -> SetProperty, Layer=layer
   ENDIF

   ; Register your interest in these objects.
   self.defaultObject -> AddParent, self
   self.defAngleObject -> AddParent, self
   self.defArrowObject -> AddParent, self
   self.defBoxObject -> AddParent, self
   self.defEllipseObject -> AddParent, self
   self.defTextObject -> AddParent, self
   self.defPolygonObject -> AddParent, self
   self.defMeasureObject -> AddParent, self

   ; Load global properties.
   self._background = background
   self._bg_color = bg_color
   self._color = color
   self._linestyle = linestyle
   self._thickness = thickness

   ; Register global properties.
   self -> RegisterProperty, 'BACKGROUND', 1, NAME="Background On"
   self -> RegisterProperty, 'BG_COLOR', 0, USERDEF="Background Color", NAME="Background Color"
   self -> RegisterProperty, 'COLOR', 0, USERDEF="Foreground Color", NAME="Color"
   self -> RegisterProperty, 'LINESTYLE', 2, NAME="Linestyle", VALID_RANGE=[0, 5]
   self -> RegisterProperty, 'THICKNESS', 2, NAME="Thickness", VALID_RANGE=[1, 10]
   self -> RegisterProperty, 'DEF_ANGLE_OBJECT', 0, USERDEF='Angle Tool Properties', NAME='Angle Tool'
   self -> RegisterProperty, 'DEF_ARROW_OBJECT', 0, USERDEF='Arrow Tool Properties', NAME='Arrow Tool'
   self -> RegisterProperty, 'DEF_BOX_OBJECT', 0, USERDEF='Box Tool Properties', NAME='Box Tool'
   self -> RegisterProperty, 'DEF_ELLIPSE_OBJECT', 0, USERDEF='Ellipse Tool Properties', NAME='Ellipse Tool'
   self -> RegisterProperty, 'DEF_MEASURE_OBJECT', 0, USERDEF='Measure Tool Properties', NAME='Measure Tool'
   self -> RegisterProperty, 'DEF_POLYGON_OBJECT', 0, USERDEF='Polygon Tool Properties', NAME='Polygon Tool'
   self -> RegisterProperty, 'DEF_TEXT_OBJECT', 0, USERDEF='Text Tool Properties', NAME='Text Tool'

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       ANNOTATEINTERACTION CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the ANNOTATEINTERACTION object.
;
;*****************************************************************************************************
PRO AnnotateInteraction__DEFINE, class

   class = { ANNOTATEINTERACTION, $
             align_gui_tlb: Obj_New(), $      ; The align GUI top-level base object.
             align_tb: 0L, $                  ; The current align top/bottom value.
             align_lr: 0L, $                  ; The current align left/right value.
             distribute_gui_tlb: Obj_New(), $ ; The distrubute GUI top-level base object.
             distribute_h: 0L, $              ; The current distribute horizontal value.
             distribute_v: 0L, $              ; The current distribute vertical value.
             distribute_gap_h: Obj_New(), $   ; The distribute horizonal gap field object.
             distribute_gap_v: Obj_New(), $   ; The distribute vertical gap field object.
             selectedObjects: Ptr_New(), $    ; The currently selected objects.
             sx: 0L, $                        ; The static X location.
             sy: 0L, $                        ; The static Y location.
             output_filename: "", $           ; Base name of output filename used in SAVE AS operations.

             defaultObject: Obj_New(), $      ; The default (or current) selectable object.
             defTextObject: Obj_New(), $      ; The default TEXTLINE object.
             defAngleObject: Obj_New(), $     ; The default ANGLETOOL object.
             defArrowObject: Obj_New(), $     ; The default ARROW object.
             defBoxObject: Obj_New(), $       ; The default BOX object.
             defEllipseObject: Obj_New(), $   ; The default ELLIPSE object.
             defPolygonObject: Obj_New(), $   ; The default POLYGON object.
             defMeasureObject: Obj_New(), $   ; The default MEASUREMENT object.
             layerObject: Obj_New(), $        ; A layer object for holding selectable annotation objects.

             angleID: Obj_New(), $            ; The ANGLE MEASURMENT button on the ControlPanel.
             annotateOn: Obj_New(), $         ; The ANNOTATE_LAYER_ON button on the ControlPanel.
             annotateOff: Obj_New(), $        ; The ANNOTATE_LAYER_OFF button on the ControlPanel.
             arrowID: Obj_New(), $            ; The ARROW button on ControlPanel.
             boxID: Obj_New(), $              ; The BOX button on ControlPanel.
             ellipseID: Obj_New(), $          ; The ELLIPSE button on the ControlPanel.
             globalID: Obj_New(), $           ; The GLOBAL_PROPERTY button on ControlPanel.
             measureID: Obj_New(), $          ; The MEASUREMENT button on the ControlPanel.
             polygonID: Obj_New(), $          ; The POLYGON button on the ControlPanel.
             saveID: Obj_New(), $             ; The SAVE_WINDOW button on ControlPanel.
             selectID: Obj_New(), $           ; The SELECT button on ControlPanel.
             textID: Obj_New(), $             ; The TEXT button on ControlPanel.

             ; Global properties.
             _background: 0L, $               ; A background ON/OFF flag.
             _bg_color: "", $                 ; The background color.
             _color: "", $                    ; The foreground color.
             _linestyle: 0L, $                ; The linestyle.
             _thickness: 0L, $                ; The thickness of the line.

             INHERITS INTERACTION $
           }

END


