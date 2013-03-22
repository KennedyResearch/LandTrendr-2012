;*****************************************************************************************************
;+
; NAME:
;       CATVIEWCONTENTS
;
; PURPOSE:
;
;       This function allows the user to view the contents of an object container.
;       The contents of the object are shown in a TreeWidget view, with the object's
;       classname, followed by the object's name in parentheses.
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
;       Utility.
;
; CALLING SEQUENCE:
;
;       CatViewContents, theObject
;
; ARGUMENTS:
;
;     theObject:       The object reference of the object whose contents you wish to view.
;
; KEYWORDS:
;
;     GROUP_LEADER:    An object reference for a WIDGETATOM-type object.
;
;     PARENT:          The identifier of a base widget object that will hold the tree widget
;                      contents created by this program. If not supplied, a top-level base
;                      object is created.
;
;     TITLE:           If a top-level base widget is created, this is used as the title.
;
;     XSIZE:           The final X size of the output tree widget. By default, 600 pixels.
;
;     YSIZE:           The final Y size of the output tree widget. By default, 600 pixels.
;
; INTERNAL_KEYWORDS:   The internal keywords FLAG and TREE should not be used.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 18 August 2005.
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
PRO CatViewContents, object, $
   FLAG=flag, $
   GROUP_LEADER=group_leader, $
   PARENT=parent, $
   TITLE=title, $
   XSIZE=xsize, $
   YSIZE=ysize, $
   TREE=tree

   @cat_pro_error_handler

   IF Obj_Valid(object) THEN BEGIN

      ; If this is the first time this is being called.
      IF N_Elements(flag) EQ 0 THEN BEGIN

         IF N_Elements(xsize) EQ 0 THEN xsize = 600
         IF N_Elements(ysize) EQ 0 THEN ysize = 600
         IF N_Elements(title) EQ 0 THEN title = 'Contents of ' + object->GetName()

         ; Do you need to create a base widget?
         IF N_Elements(parent) EQ 0 THEN BEGIN
            parent = Obj_New('VIEWCONTENTS', Column=1, Title=title, $
                  GROUP_LEADER=group_leader )
            createdTLB = 1
         ENDIF ELSE createdTLB = 0

         ; Create the root node of the tree.
         rootnode = Obj_New('TreeWidget', parent, /Folder, /Expanded, Value=Obj_Class(object), $
             /Top, YSize=ysize, XSize=xsize, NAME='Root Node')

         ; Create the first tree folder, which is the parent of the tree.
         tree = Obj_New('TreeWidget', rootnode, /Folder, /Expanded, $
               Value=Obj_Class(object)+ ' (' + object->GetName() + ')', UVALUE=object)

         IF createdTLB THEN parent -> Draw, /Center

      ENDIF

      ; Does this object have children?
      children = object -> Get(/All, Count=count)
      IF count EQ 0 THEN RETURN

      ; If there are children, traverse each child's container for leafs.
      FOR j=0, count-1 DO BEGIN
         child = children[j]
         IF Obj_Valid(child) THEN BEGIN

            value = Obj_Class(child) + ' (' + child->GetName() + ')'
            child -> GetProperty, OChild=hasChild
            IF Obj_Valid(hasChild) THEN BEGIN
               newTree = Obj_New('TreeWidget', tree, /Folder, /Expanded, Value=value, UValue=child)
            ENDIF ELSE BEGIN
               newTree = Obj_New('TreeWidget', tree, Value=value, UValue=child)
            ENDELSE
            CatViewContents, child, FLAG=1, TREE=newTree
         ENDIF
      ENDFOR
   ENDIF ELSE Message, 'Must pass a valid object reference.'

END