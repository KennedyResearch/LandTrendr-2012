;*****************************************************************************************************
;+
; NAME:
;       TEXTWIDGET
;
; PURPOSE:
;
;       The purpose of this routine is to implement a text widget as an object.
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
;       Object widgets.
;
; SYNTAX:
;
;       textWidget = Obj_New("TextWidget", theParent, Value='The Text')
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
;    class = { TEXTWIDGET, INHERITS WidgetAtom }
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 24 July 2002.
;       Modified keywords in the SetProperty method that started with SET_, dropping this prefix
;          to keep them consistent with the same keywords in the GetProperty method. 18 July 2004. DWF.
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
;
; NAME:
;       TEXTWIDGET::ADD
;
; PURPOSE:
;
;       This method overrides the superclass Add method to prevent addition of
;       other widget objects to text objects.
;
; SYNTAX:
;
;       textObject -> Add, object
;
; ARGUMENTS:
;
;       object: The object to add to the container. Only objects that are NOT subclassed
;               from WIDGETATOM objects can be added.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO TextWidget::Add, object

   @cat_pro_error_handler

   IF OBJ_ISA_VALID(object, 'WIDGETATOM') THEN Message, 'Cannot add widget objects to TextWidgets.'

   self -> Add::WIDGETATOM, object
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       TEXTWIDGET::GETPROPERTY
;
; PURPOSE:
;
;       This method is used to get the object's properties.
;
; SYNTAX:
;
;       textObject -> GetProperty, VALUE=theText
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       ALL_EVENTS:  Returns a 1 if ALL_EVENTS is turned on for text widget. Returns a 0 otherwise.
;
;       CONTEXT_EVENTS: Returns a 1 if context events are on, a 0 otherwise.
;
;       EDITABLE:    Returns a 1 if EDITABLE is turned on for text widget. Returns a 0 otherwise.
;
;       NUMBER:      Returns the number of characters currently in the text widget.
;
;       TEXT_OFFSET_TO_XY: Use this keyword to translate a text widget character offset
;                    into column and line form. The value of this keyword should
;                    be set to the character offset (an integer) to be translated.
;                    A two-element integer array is returned, giving the column
;                    (element 0) and line (element 1) corresponding to the offset.
;                    If the offset specified is out of range, the array [-1,-1]
;                    is returned.
;
;       TEXT_SELECT: Set this keyword to return the starting character offset
;                    and length (in characters) of the selected (highlighted)
;                    text in the specified text widget. A two-element integer
;                    array is returned containing the starting position of
;                    the highlighted text as an offset from character zero
;                    of the text in the widget (element 0), and length of
;                    the current selection (element 1).
;
;       TEXT_TOP_LINE: Set this keyword to return the zero-based line
;                    number of the line currently at the top of a text widget's
;                    display viewport. Note that this value is different from
;                    the zero-based character offset of the characters in the
;                    line. The character offset can be calculated from the line
;                    offset via the TEXT_XY_TO_OFFSET keyword.
;
;       TEXT_XY_TO_OFFSET: Use this keyword to translate a text widget position given in
;                    line and column form into a character offset. The value of this
;                    keyword should be set to a two-element integer array specifying
;                    the column (element 0) and line (element 1) position. The character
;                    offset (as a longword integer) is returned corresponding to the
;                    position. If the position specified is out of range, -1 is returned.
;
;       VALUE:       The current value of the text widget.
;
;       _REF_EXTRA:  Any keyword appropriate for the "WidgetAtom::GetProperty" object method.
;
;-
;*****************************************************************************************************
PRO TextWidget::GetProperty, $
   ALL_EVENTS=all_events, $
   CONTEXT_EVENTS=context_events, $
   EDITABLE=editable, $
   NUMBER=number, $
   TEXT_OFFSET_TO_XY=text_offset_to_xy, $
   TEXT_SELECT=text_select, $
   TEXT_TOP_LINE=text_top_line, $
   TEXT_XY_TO_OFFSET=text_xy_to_offset, $
   VALUE=value, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Get any superclass properties.

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> WidgetAtom::GetProperty, _EXTRA=extraKeywords

      ; To continue, you need a valid widget ID.

   IF WIDGET_INFO (self._id, /VALID_ID) NE 1 THEN BEGIN
      self -> Report, /Completed
      RETURN
   ENDIF


   WIDGET_CONTROL, self._id, GET_VALUE=value
   all_events = Widget_Info(self._id, /TEXT_ALL_EVENTS)
   IF Arg_Present(context_events) THEN context_events = Widget_Info(self._ID, /Context_Events)
   ;editable = Widget_Info(self._id, /EDITABLE)
   number = Widget_Info(self._id, /TEXT_NUMBER)
   text_offset_to_xy = Widget_Info(self._id, /TEXT_OFFSET_TO_XY)
   text_select = Widget_Info(self._id, /TEXT_SELECT)
   text_top_line = Widget_Info(self._id, /TEXT_TOP_LINE)
   text_xy_to_offset = Widget_Info(self._id, TEXT_XY_TO_OFFSET=text_xy_to_offset)

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       TEXTWIDGET::SETPROPERTY
;
; PURPOSE:
;
;       This method is used to set the object's properties.
;
; SYNTAX:
;
;       textObject -> SetProperty, VALUE='Test'
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       ALL_EVENTS:  Set this keyword to enable all possible text events to be generated.
;                    Events depend upon the interaction of the ALL_EVENTS and EDITABLE keywords.
;                    See the IDL On-Line Help for WIDGET_TEXT for details.
;
;       APPEND:      If this keyword is set, and you set the VALUE of the text widget, then
;                    the text is appended to the text already in the text widget, rather than
;                    replacing it.
;
;       CONTEXT_EVENTS:    Set to 1 to turn context events on for the base widget.
;
;       EDITABLE:    Set this keyword to allow user editing of text widget contents.
;
;       INPUT_FOCUS: Set this keyword to configure the text widget to receive keyboard focus.
;
;       KBRD_FOCUS_EVENTS: Set this keyword to allow keyboard focus events be generated.
;
;       NO_NEWLINE:  Set this character to prevent new lines of text from being added when text
;                    is appended to the text already available in the text widget.
;
;       TEXT_SELECT: Use this keyword to clear any current selection in the specified table
;                    cell or text widget, and either set a new selection, or simply set the text
;                    insertion point. To set a selection, specify a two-element integer array
;                    containing the starting position and the length of the selection. For example,
;                    to set a selection covering characters 3 though 23:
;
;                    textWidget -> SetProperty, SET_TEXT_SELECT=[3, 20]
;
;                    To move the text insertion point without setting a selection, omit the
;                    second element, or set it to zero.
;
;       TEXT_TOP_LINE: Set this keyword to the zero-based line number of the line to
;                    be positioned on the topmost visible line in the text widget's
;                    viewport. No horizontal scrolling is performed. Note that this
;                    is a line number, not a character offset.
;
;       USE_TEXT_SELECT: Set this keyword to modify the behavior of the VALUE keyword.
;                    If USE_TEXT_SELECT is set, the VALUE replaces only the text in the
;                    current text selection, not the entire text.
;
;       VALUE:       The text that goes into the text widget.
;
;       _EXTRA:  Any keyword appropriate for the "WidgetAtom::SetProperty" object method.
;
;-
;*****************************************************************************************************
PRO TextWidget::SetProperty, $
   ALL_EVENTS=all_events, $
   APPEND=append, $
   CONTEXT_EVENTS=context_events, $
   EDITABLE=editable, $
   INPUT_FOCUS=input_focus, $
   KBRD_FOCUS_EVENTS=kbrd_focus_events, $
   NO_NEWLINE=no_newline, $
   TEXT_SELECT=set_text_select, $
   TEXT_TOP_LINE=set_text_top_line, $
   USE_TEXT_SELECT=use_text_select, $
   VALUE=value, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

      ; Set superclass properties.

   self -> WidgetAtom::SetProperty, _EXTRA=extraKeywords

      ; Make sure you have a valid widget ID before proceeding.

   IF Widget_Info(self._ID, /Valid_ID) NE 1 THEN BEGIN
      self -> Report, /Completed
      RETURN
   ENDIF

      ; Set the properties of the text widget.

   IF (N_ELEMENTS (all_events) GT 0) THEN WIDGET_CONTROL, self._id, ALL_TEXT_EVENTS=Keyword_Set(all_events)
   IF N_Elements(context_events) NE 0 THEN $
      WIDGET_CONTROL, self._id, CONTEXT_EVENTS=Keyword_Set(context_events)
   IF (N_ELEMENTS (editable) GT 0) THEN WIDGET_CONTROL, self._id, EDITABLE=Keyword_Set(all_events)
   IF (N_ELEMENTS (input_focus) GT 0) THEN WIDGET_CONTROL, self._id, INPUT_FOCUS=Keyword_Set(input_focus)
   IF (N_ELEMENTS (kbrd_focus_events) GT 0) THEN WIDGET_CONTROL, self._id, KBRD_FOCUS_EVENTS=Keyword_Set(kbrd_focus_events)
   IF (N_ELEMENTS (no_newline) GT 0) THEN WIDGET_CONTROL, self._id, NO_NEWLINE=Keyword_Set(no_newline)
   IF (N_ELEMENTS (set_text_top_line) GT 0) THEN WIDGET_CONTROL, self._id, SET_TEXT_TOP_LINE=set_text_top_line
   IF (N_ELEMENTS (use_text_select) GT 0) THEN WIDGET_CONTROL, self._id, USE_TEXT_SELECT=Keyword_Set(use_text_select)
   IF (N_ELEMENTS (value) GT 0) THEN WIDGET_CONTROL, self._id, SET_VALUE=value, APPEND=Keyword_Set(append)

   ; The following line MUST go after setting the value, or it has no effect if VALUE and TEXT_SELECT
   ; are used together. Go figure.
   IF (N_ELEMENTS (set_text_select) GT 0) THEN WIDGET_CONTROL, self._id, SET_TEXT_SELECT=set_text_select

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       TEXTWIDGET::CLEANUP
;
; PURPOSE:
;
;       This is the TEXTWIDGET object class destructor method.
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
;       _EXTRA:  Any keyword appropriate for the "WidgetAtom::Cleanup" method.
;-
;*****************************************************************************************************
PRO TextWidget::CLEANUP

   @cat_pro_error_handler
   self -> WidgetAtom::CLEANUP
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       TEXTWIDGET::INIT
;
; PURPOSE:
;
;       This is the TEXTWIDGET object class initialization method.

; ARGUMENTS:
;
;       theParent - The parent object reference. This is the same as using the PARENT keyword.
;
; KEYWORDS:
;
;       ALL_EVENTS:  Set this keyword to enable all possible text events to be generated.
;                    Events depend upon the interaction of the ALL_EVENTS and EDITABLE keywords.
;                    See the IDL On-Line Help for WIDGET_TEXT for details.
;
;       CONTEXT_EVENTS:    Set this keyword to turn context events on for this widget object..
;
;       EDITABLE:    Set this keyword to allow user editing of text widget contents.
;
;       FONT:        The name of a font to be used by the widget.
;
;       FRAME:       Create a frame this many pixels wide around base.
;
;       KBRD_FOCUS_EVENTS: Set this keyword to allow keyboard focus events be generated.
;
;       NO_NEWLINE:  Set this character to prevent new lines of text from being added when text
;                    is appended to the text already available in the text widget.
;
;       SCR_XSIZE:   Set the screen X size of the widget to this many pixels. (Use discouraged.)
;
;       SCR_YSIZE:   Set the screen Y size of the widget to this many pixels. (Use discouraged.)
;
;       SCROLL:      Set this keyword to allow scroll bars to be placed on the text widget.
;
;       UNITS:       The units for measurments. The default is 0 for pixels. Other values are
;                    1 for inches, and 2 for centimeters.
;
;       VALUE:       The text to be placed in the text widget. Must be a string or string array.
;
;       WRAP:        Set this keyword to allow text wrapping in the text widget.
;
;       XOFFSET:     The horizontal space (pixels) from upper left corner of the parent.
;
;       XSIZE:       The X size of the widget.
;
;       YOFFSET:     The vertical space (pixels) from upper left corner of the parent.
;
;       YSIZE:       The Y size of the widget.
;
;       _EXTRA:      Any keyword appropriate for the "WidgetAtom::Init" method.
;-
;*****************************************************************************************************
FUNCTION TextWidget::INIT, parent, $
   ALL_EVENTS=all_events, $
   CONTEXT_EVENTS=context_events, $
   EDITABLE=editable, $
   FONT=font, $
   FRAME=frame, $
   KBRD_FOCUS_EVENTS=kbrd_focus_events, $
   NO_NEWLINE=no_newline, $
   SCR_XSIZE=scr_xsize, $
   SCR_YSIZE=scr_ysize, $
   SCROLL=scroll, $
   UNITS=units, $
   VALUE=value, $
   WRAP=wrap, $
   XOFFSET=xoffset, $
   XSIZE=xsize, $
   YOFFSET=yoffset, $
   YSIZE=ysize, $
   _EXTRA=extraKeywords

   @cat_func_error_handler

      ; If there is a parent, make sure it is a valid BASEWIDGET class object.

   IF OBJ_ISA_VALID(parent, "BASEWIDGET") EQ 0 THEN Message, 'Parent object invalid or not of type BASEWIDGET.'

   ; Create the new text widget.
   parent -> GetProperty, ID=parentID
   id = WIDGET_TEXT (parentID, $
      ALL_EVENTS=all_events, $
      CONTEXT_EVENTS=context_events, $
      EDITABLE=editable, $
      FONT=font, $
      FRAME=frame, $
      KBRD_FOCUS_EVENTS=kbrd_focus_events, $
      NO_NEWLINE=no_newline, $
      SCR_XSIZE=scr_xsize, $
      SCR_YSIZE=scr_ysize, $
      SCROLL=scroll, $
      UNITS=units, $
      VALUE=value, $
      WRAP=wrap, $
      XOFFSET=xoffset, $
      XSIZE=xsize, $
      YOFFSET=yoffset, $
      YSIZE=ysize)

   ; Call the superclass INIT method
   ok = self -> WidgetAtom::INIT (parent, id, _EXTRA=extraKeywords)
   IF (NOT ok) THEN self -> Message, 'Superclass failed to initialize properly.'

   ; Print and return status report
   IF (ok) THEN self -> Report, /Completed $
   ELSE self -> Report, /Failed
   RETURN, ok
END


;*****************************************************************************************************
;
; NAME:
;       TEXTWIDGET CLASS DEFINITION
;
; PURPOSE:
;
;       This is the TEXTWIDGET object's structure definition code.
;
;*****************************************************************************************************

PRO TextWidget__DEFINE, class

   class = {TEXTWIDGET, INHERITS widgetAtom}

END
