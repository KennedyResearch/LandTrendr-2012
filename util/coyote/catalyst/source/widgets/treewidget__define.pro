;*****************************************************************************************************
;+
; NAME:
;       TREEWIDGET__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a tree widget as an object.
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
;       aTreeWidget = Obj_New("TREEWIDGET", parent)
;
; SUPERCLASSES:
;
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { TREEWIDGET, $          ; Tree widget object class
;             INHERITS WIDGETATOM $  ; Inherits WidgetAtom object class.
;           }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 20 April 2003.
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
;       TREEWIDGET::ADD
;
; PURPOSE:
;
;       This method overrides the superclass Add methods to make sure tree widgets are only
;       added to other tree widgets.
;
; SYNTAX:
;
;       thisTreeObj -> Add, thatTreeObj
;
; ARGUMENTS:
;
;       object: The object to add to the container. Only objects subclassed from TREEWIDGET can be added.
;
; KEYWORDS:
;
;       _EXTRA: Any keyword appropriate for the superclass ADD methods.
;
;-
;*****************************************************************************************************
PRO TreeWidget::Add, object, _Extra=extraKeywords

   @cat_pro_error_handler

   IF Obj_Valid(object) EQ 0 THEN Message, 'Only valid objects can be added to this container.'
   IF (OBJ_ISA (object, 'TREEWIDGET')) EQ 0 THEN $
      Message, 'Only sub-classed TreeWidget objects can be added to TreeWidget objects.'
   self -> WidgetAtom::Add, object, _Extra=extraKeywords

   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       TREEWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method enables the getting of the TREEWIDGET object class
;       properties.
;
; SYNTAX:
;
;       aTreeWidget -> GetProperty, CURRENT=tree_current
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     CONTEXT_EVENTS: Returns a 1 if context events are on, a 0 otherwise.
;
;     EXPANDED:   If 1, the current node is expanded, if 0 then it is collapsed.
;
;     ROOT:       Returns the object reference of the root node of the tree widget.
;
;     SELECT:     This keyword acts differently depending upon whether the specified tree widget
;                 idenifier is a root node or just a normal node in a tree.
;
;                 If this node is a root node, then this keyword returns an object reference, or
;                 an array of object references (if in MULTIPLE selection mode), that are currently
;                 selected. If no nodes are selected, a null object is returned.
;
;                 If this node is a normal node, this keyword returns a 1 if the node is selected
;                 and a zero otherwise.
;
;     VALUE:      The text string used to label the tree widget.
;
;     _REF_EXTRA:  Any keywords appropriate for the WIDGETATOM::GetProperty method.
;-
;*****************************************************************************************************
PRO TreeWidget::GetProperty, $
   CONTEXT_EVENTS=context_events, $
   EXPANDED=expanded, $
   ROOT=root, $
   SELECT=select, $
   VALUE=value, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Arg_Present(context_events) THEN context_events = Widget_Info(self._ID, /Context_Events)
   IF Arg_Present(expanded) THEN expanded = Widget_Info(self._ID, /TREE_EXPANDED)
   IF Arg_Present(root) THEN $
   BEGIN
      rootID = Widget_Info(self._ID, /TREE_ROOT)
      Widget_Control, rootID, Get_UValue=root
   ENDIF
   IF Arg_Present(select) THEN $
   BEGIN

      ; Is this a root node?
      rootID = Widget_Info(self._ID, /TREE_ROOT)
      Widget_Control, rootID, Get_UValue=root
      IF self EQ root THEN rootnode = 1 ELSE rootnode = 0

      ; If it is a root node, then you have to obtain object references.
      ; Otherwise, just return the normal values.
      IF rootnode THEN $
      BEGIN
         selectIDs = Widget_Info(self._ID, /TREE_SELECT)
         select = ObjArr(N_Elements(selectIDs))
         FOR j=0, N_Elements(selectIDs)-1 DO $
         BEGIN
            Widget_Control, selectIDs[j], Get_UValue=theObject
            select[j] = theObject
         ENDFOR
      ENDIF ELSE select = Widget_Info(self._ID, /TREE_SELECT)
   END

   IF Arg_Present(value) THEN Widget_Control, self._ID, GET_VALUE=value
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> WidgetAtom::GetProperty, _EXTRA=extraKeywords
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       TREEWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the TREEWIDGET object class
;       properties.
;
; SYNTAX:
;
;       aTreeWidget -> SetProperty, CURRENT=4
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     BITMAP:     Set this keyword equal to a 16x16x3 array representing an RGB image that will
;                 be displayed next to the node in the tree widget.
;
;     CONTEXT_EVENTS:    Set to 1 to turn context events on for the base widget.
;
;     EXPANDED:   Set this keyword to expand the current folder node.
;
;     SELECT:     This keyword acts differently depending upon whether the specified tree widget
;                 idenifier is a root node or just a normal node in a tree.
;
;                 If a root node, then set this keyword to an object reference, or an array of
;                 object references (if in MULTIPLE selection mode), that should be selected.
;                 To clear *all* selections, set this keyword to 0. If the tree widget is in
;                 MULTIPLE selection mode, the selection changes made to the tree widget with
;                 this keyword are additive, not exclusive.
;
;                 If a normal node, set this keyword to select this node. Set the keyword
;                 to zero to deselect this node.
;
;     VALUE:      Set this keyword equal to a string containing the text that will be
;                 displayed next to the tree node.
;
;     VISIBLE:    Set his keyword to make the current node visible to the user. This does not affect
;                 the current selection status of the node.
;
;     _EXTRA:     Any keywords appropriate for the WIDGETATOM::SetProperty method.
;-
;*****************************************************************************************************
PRO TreeWidget::SetProperty, $
   BITMAP=bitmap, $
   CONTEXT_EVENTS=context_events, $
   EXPANDED=expanded, $
   SELECT=select, $
   VALUE=value, $
   VISIBLE=visible, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Set the keywords, if available.

   IF N_Elements(bitmap) NE 0 THEN Widget_Control, self._ID, SET_TREE_BITMAP=bitmap
   IF N_Elements(context_events) NE 0 THEN $
      WIDGET_CONTROL, self._id, CONTEXT_EVENTS=Keyword_Set(context_events)
   IF N_Elements(expanded) NE 0 THEN Widget_Control, self._ID, SET_TREE_EXPANDED=Keyword_Set(expanded)
   IF N_Elements(value) NE 0 THEN Widget_Control, self._ID, SET_VALUE=value
   IF N_Elements(visible) NE 0 THEN Widget_Control, self._ID, SET_TREE_VISIBLE=Keyword_Set(visible)

   ; Selections could come in as object references. Must convert to widget IDs.
   IF N_Elements(select) NE 0 THEN $
   BEGIN
      IF Size(select, /TNAME) EQ 'OBJREF' THEN $
      BEGIN
         id_array = LonArr(N_Elements(select))
         FOR j=0, N_Elements(select)-1 DO id_array[j] = select[j] -> GetProperty(/ID)
         IF N_Elements(id_array) EQ 1 THEN id_array = id_array[0]
         Widget_Control, self._ID, SET_TREE_SELECT=id_array
      ENDIF ELSE Widget_Control, self._ID, SET_TREE_SELECT=select
   ENDIF

   ; Call the superclass method.
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> WIDGETATOM::SetProperty, _EXTRA=extraKeywords
   self -> Report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       TREEWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the TREEWIDGET object class destructor method.
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
;-
;*****************************************************************************************************
PRO TreeWidget::CLEANUP

   @cat_pro_error_handler
   self -> WIDGETATOM::CLEANUP
   self -> report, /Completed
END



;*****************************************************************************************************
;+
; NAME:
;       TREEWIDGET::INIT
;
; PURPOSE:
;
;       This is the TREEWIDGET object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     parent          The parent object reference for this object widget.
;
; KEYWORDS:
;
;     ALIGN_BOTTOM:    Set this keyword to align the base widget with the bottom of the parent base.
;
;     ALIGN_CENTER:    Set this keyword to align the base widget with the center of the parent base.
;
;     ALIGN_LEFT:      Set this keyword to align the base widget with the left of the parent base.
;
;     ALIGN_RIGHT:     Set this keyword to align the base widget with the right of the parent base.
;
;     ALIGN_TOP:       Set this keyword to align the base widget with the top of the parent base.
;
;     BITMAP:          Set this keyword equal to a 16x16x3 array representing an RGB image that will
;                      be displayed next to the node in the tree widget.
;
;     CONTEXT_EVENTS:  Set this keyword to turn context events on for this widget object..
;
;     EXPANDED:        If the tree node being created is a FOLDER, set this keyword to cause the folder
;                      to be initially displayed expanded. Be default, folders are initially displayed
;                      collapsed.
;
;     FOLDER:          Set this keyword to cause the tree node being created to act as a folder (that is,
;                      as a branch of the tree, rather than as a leaf. Only tree widgets that are set
;                      as folders can act as parents to other tree widgets (with the exception of the
;                      root node, whose parent is a base widget).
;
;     MULTIPLE:        Set this keyword to enable multiple selection operations in the tree widget. If
;                      enabled, multiple elements in the tree widget can be selected at one time by holding
;                      down the Control or Shift key while clicking the left mouse button. The keyword is
;                      valid only if the parent of the tree widget is a base widget.
;
;     SCR_XSIZE:       Set the screen X size of the base to this many pixels. (Use discouraged.)
;
;     SCR_YSIZE:       Set the screen Y size of the base to this many pixels. (Use discouraged.)
;
;     TOP:             Set this keyword to cause the tree node being created to be inserted as the parent
;                      node's top entry. By default, new nodes are created as the parent node's bottom entry.
;
;     UNITS:           The units for measurments. The default is 0 for pixels. Other values are
;                      1 for inches, and 2 for centimeters.
;
;     VALUE:           Set this keyword equal to a string containing the text that will be
;                      displayed next to the tree node. If this keyword is not set, the
;                      default value "Tree" is used.
;
;     XOFFSET:         The horizontal space (pixels) from upper left corner of the display.
;
;     XSIZE:           The X size of the widget.
;
;     YOFFSET:         The vertical space (pixels) from upper left corner of the display.
;
;     YSIZE:           The Y size of the widget.
;
;     _EXTRA:          Any keywords appropriate for the WIDGETATOM::INIT method.
;-
;*****************************************************************************************************
FUNCTION TreeWidget::INIT, parent, $
   ALIGN_BOTTOM=align_bottom,$
   ALIGN_CENTER=align_center, $
   ALIGN_LEFT=align_left, $
   ALIGN_RIGHT=align_right, $
   ALIGN_TOP=align_top, $
   BITMAP=bitmap, $
   CONTEXT_EVENTS=context_events, $
   EXPANDED=expanded, $
   FOLDER=folder, $
   MULTIPLE=multiple, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   TOP=top, $
   UNITS=units, $
   VALUE=value, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

     ; Create the tree widget.

   parent -> GetProperty, ID=parentID
   id = WIDGET_TREE (parentID, $
         ALIGN_BOTTOM=align_bottom,$
         ALIGN_CENTER=align_center, $
         ALIGN_LEFT=align_left, $
         ALIGN_RIGHT=align_right, $
         ALIGN_TOP=align_top, $
         BITMAP=bitmap, $
         CONTEXT_EVENTS=context_events, $
         EXPANDED=expanded, $
         FOLDER=folder, $
         MULTIPLE=multiple, $
         SCR_XSIZE=scr_xsize, $
         SCR_YSIZE=scr_ysize, $
         TOP=top, $
         UNITS=units, $
         VALUE=value, $
         XOFFSET=xoffset, $
         XSIZE=xsize, $
         YOFFSET=yoffset, $
         YSIZE=ysize)

      ; Call the superclass INIT method.

   ok = self -> WidgetAtom::INIT (parent, id, _EXTRA=extraKeywords)
   IF NOT ok THEN Message, 'WidgetAtom Initialization failed.'

   self -> Report, /Completed

   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       TREEWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the TREEWIDGET object.
;
;*****************************************************************************************************
PRO TreeWidget__DEFINE, class

   class = { TREEWIDGET, $          ; Tree widget object class
             INHERITS WIDGETATOM $  ; Inherits WidgetAtom object class.
           }
END
