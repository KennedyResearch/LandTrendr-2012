;*****************************************************************************************************
;+
; NAME:
;       CATGRAPHICSWINDOW__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a draw widget inside a top-level base widget.
;       Objects with DRAW methods can be added to the CatGraphicsWindow object and their graphics
;       will be displayed in the window.
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
;       aGraphicsWindow = Obj_New("CatGraphicsWindow")
;
;
; SUPERCLASSES:
;
;       TOPLEVELBASE
;       BASEWIDGET
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER
;
; CLASS_STRUCTURE:
;
;   class = {CatGraphicsWindow, _draw:OBJ_NEW (), INHERITS TopLevelBase}
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 6 July 2003.
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
;       CatGraphicsWindow::ADD
;
; PURPOSE:
;
;       This method adds the object to the draw widget of the graphics window. To
;       be displayed in the graphics window, the added object will require a DRAW method.
;
; SYNTAX:
;
;       aGraphicsWindow -> Add, theObject
;
; ARGUMENTS:
;
;     theObject:     The object to be added to this object's draw widget object. Must be either
;                    a CATDATAATOM (direct graphics) with a DRAW method or a CATOBJECTVIEW
;                    (object graphics) object. If a CATOBJECTVIEW object, the object will be
;                    expected to handle all draw widget events. Button events and expose events
;                    are turned on by default.
;
; KEYWORDS:
;
;     _EXTRA:        Extra keywords for the TOPLEVELBASE::ADD method or the object graphics
;                    oDrawWidget::SetProperty method.
;-
;*****************************************************************************************************

PRO CatGraphicsWindow::Add, theObject, _Extra=extraKeywords

   @cat_pro_error_handler


   IF (Obj_IsA_Valid (theObject, 'WIDGETATOM')) THEN $
   BEGIN
      self -> TopLevelBase::Add, theObject, _EXTRA=extraKeywords
   ENDIF $
   ELSE BEGIN
      IF (Obj_IsA_Valid(theObject, 'CATDATAATOM')) OR (Obj_IsA_Valid(theObject, 'CATOBJECTVIEW')) THEN $
      BEGIN
         IF Obj_IsA_Valid(theObject, 'CATOBJECTVIEW') THEN BEGIN
            self._draw -> Add, theObject, /Handle_Events, /Exclusive, _Extra=extraKeywords
            self._draw -> SetProperty, Button_Events=1, Expose_Events=1, _Extra=extraKeywords
         ENDIF
         IF Obj_IsA_Valid(theObject, 'CATDATAATOM') THEN BEGIN
            self._draw -> Add, theObject, _Extra=extraKeywords
         ENDIF

      ENDIF ELSE self._draw -> Add, theObject, _Extra=extraKeywords
   ENDELSE

   IF Obj_ISA(theObject,'CATIMAGEDATA') THEN self -> SetProperty, Erase_Window=1

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;        CatGraphicsWindow::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the CatGraphicsWindow interface. It
;        exists purely to handle resize events which are passed to the
;        child object.
;
; SYNTAX:
;
;        This method is called automatically by the event handling mechanism.
;
; ARGUMENTS:
;
;       event:    The event structure as described in the IDL help files.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO CatGraphicsWindow::EventHandler, event

   ; Set up the event handler
   @cat_pro_error_handler

   ; If this is a resize event, resize the draw widget
   IF (event.id EQ self) THEN $
   BEGIN
      self._draw -> SetProperty, XSIZE=event.x, YSIZE=event.y
      self._draw -> Draw, /Erase_Window
   ENDIF ELSE BEGIN

      CASE StrUpCase(event.name) OF
         'EXIT': Obj_Destroy, self
         ELSE: BEGIN
            HELP, event, /Structure
            Message, 'Unrecognized event in CatGraphicsWindow EventHandler.'
            END
      ENDCASE

   ENDELSE

   ; Report completion
   IF Obj_Valid(self) THEN self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;        CatGraphicsWindow::DRAWWIDGETEVENTHANDLER
;
; PURPOSE:
;
;        This method is the event handler for an events being generated by the
;        draw widget.
;
; SYNTAX:
;
;        This method is called automatically by the event handling mechanism.
;
; ARGUMENTS:
;
;       event:    The event structure as described in the IDL help files.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO CatGraphicsWindow::DrawWidgetEventHandler, event

   ; Set up the event handler
   @cat_pro_error_handler

   self._draw -> SetWindow
   ERASE
   self._draw -> Draw

   ; Report completion
   IF Obj_Valid(self) THEN self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CatGraphicsWindow::GETPROPERTY
;
; PURPOSE:
;
;       This method enables the getting of the CatGraphicsWindow object class
;       properties.
;
; SYNTAX:
;
;       aCatGraphicsWindow -> GetProperty
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     DRAWID:     The draw widget object.
;
;     WINDOWID:   The window index number of the draw widget (direct graphics) or the
;                 IDLgrWindow object (object graphics).
;
;     _EXTRA:     Any keywords appropriate for the "DrawWidget::SetProperty" method.
;-
;*****************************************************************************************************
PRO CatGraphicsWindow::GetProperty, DRAWID=drawid, WindowID=windowid, _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF Obj_Valid(self._draw) THEN BEGIN
      self._draw -> GetProperty, WindowID=windowID
      drawid = self._draw
   ENDIF

   self -> TOPLEVELBASE::GetProperty, _Extra=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CatGraphicsWindow::SAVEAS
;
; PURPOSE:
;
;       This method allows the draw widget to save its contents in hardcopy. It is an
;       event handler method.
;
; SYNTAX:
;
;       Called by a widget event occurring.
;
; ARGUMENTS:
;
;     event:      The Save As... button event structure.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO CatGraphicsWindow::SaveAs, event

   @cat_pro_error_handler

   CASE event.name OF
      'JPEG': self._draw -> Output, /JPEG, Filename=self._output_filename + '.jpg'
      'TIFF': self._draw -> Output, /TIFF, Filename=self._output_filename + '.tif'
      'BMP': self._draw -> Output, /BMP, Filename=self._output_filename + '.bmp'
      'PNG': self._draw -> Output, /PNG, Filename=self._output_filename + '.png'
      'PS': self._draw -> Output, /PostScript, Filename=self._output_filename + '.ps'

   ENDCASE

END


;*****************************************************************************************************
;+
; NAME:
;       CatGraphicsWindow::SETWINDOW
;
; PURPOSE:
;
;       This method sets the window index number of the draw widget to be the
;       current graphics window.
;
; SYNTAX:
;
;       thisWindow -> SetWindow
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None
;-
;*****************************************************************************************************
PRO CatGraphicsWindow::SetWindow
   self._draw -> SetWindow
END


;*****************************************************************************************************
;+
; NAME:
;       CatGraphicsWindow::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the CatGraphicsWindow object class
;       properties.
;
; SYNTAX:
;
;       aCatGraphicsWindow -> SetProperty, XSIZE=500, YSIZE=300
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     BACKGROUND:   The name of the background color. Normally, this keyword will require ERASE_WINDOW=1, too.
;
;     ERASE_WINDOW: Set this keyword to 1 to have the draw widget erase before drawing. Set
;                   to 0 to skip the erasing.
;
;     _EXTRA:       Any keywords appropriate for the  TopLevelBase SetProperty" method.
;-
;*****************************************************************************************************
PRO CatGraphicsWindow::SetProperty, BACKGROUND=background, ERASE_WINDOW=erase_window, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(erase_window) NE 0 THEN self._draw -> SetProperty, ERASE_WINDOW=Keyword_Set(erase_window)
   IF N_Elements(background) NE 0 THEN self._draw -> SetProperty, INITIAL_COLOR=background

   self -> TOPLEVELBASE::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed
END

;*****************************************************************************************************
;+
; NAME:
;       CatGraphicsWindow::CLEANUP
;
; PURPOSE:
;
;       This is the CatGraphicsWindow object class destructor method.
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
PRO CatGraphicsWindow::CLEANUP

   @cat_pro_error_handler

   Obj_Destroy, self._draw
   self -> TOPLEVELBASE::Cleanup

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CatGraphicsWindow::INIT
;
; PURPOSE:
;
;       This is the CatGraphicsWindow object class initialization method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     parent:          The parent object of this object.
;
; KEYWORDS:
; 
;    BACKGROUND_COLOR: Set this to the name of a background color. Used only when the
;                      keyword ERASE_WINDOW is set. Set to "BLACK" by default.
;
;     ERASE_WINDOW:    Set this keyword to 1 to have the draw widget erase before drawing. Set
;                      to 0 to skip the erasing.
;
;     OBJECT_GRAPHICS: Set this keyword to make the draw widget an ODRAWWIDGET instead of a DRAWWIDGET object.
;     
;     OUTPUT_FILENAME: The base name for the output filename. This will be given the proper extension
;                      by the program.
;
;     TITLE:           The title of the top-level base widget.
;     
;     XSIZE:           The X size of the enclosed draw widget in pixels.
;
;     YSIZE:           The Y size of the enclosed draw widget in pixels.
;
;     _EXTRA:          Any keywords appropriate for TopLevelBase::INIT method.
;-
;*****************************************************************************************************
FUNCTION CatGraphicsWindow::INIT, parent, $
   BACKGROUND_COLOR=background_color, $
   COLOR_OBJECT=color_object, $
   ERASE_WINDOW=erase_window, $
   OBJECT_GRAPHICS=object_graphics, $
   OUTPUT_FILENAME=output_filename, $
   TITLE=title, $
   XSIZE=xsize, $
   YSize=ysize, $
   _EXTRA=extraKeywords

   ; Set up error handler.
   @cat_func_error_handler
   
   IF N_Elements(background_color) EQ 0 THEN background_color = 'BLACK'
   IF N_Elements(output_filename) EQ 0 THEN output_filename = 'catalyst'
   IF N_Elements(title) EQ 0 THEN title = 'Catalyst Graphics Window'
   self._output_filename = output_filename
   
   ; Initialize top-level base widget object.
   ok = self -> TopLevelBase::Init (parent, /SIZE_EVENTS, Column=1, _EXTRA=extraKeywords, $
        MBAR=menuBarID, XPAD=0, YPAD=0, TITLE=title)

   ; Create a Quit button in the menu bar.
   fileMenu = OBJ_NEW ('ButtonWidget', menuBarID ,  Value='File', /MENU)
   saveasID = OBJ_NEW ('ButtonWidget', fileMenu,  Value='Save As...', /MENU, $
      Name='Save As', Event_Method='SaveAs')
   button = OBJ_NEW ('ButtonWidget', saveasID,  Name='JPEG', Value='JPEG file')
   button = OBJ_NEW ('ButtonWidget', saveasID,  Name='TIFF', Value='TIFF File')
   button = OBJ_NEW ('ButtonWidget', saveasID,  Name='BMP', Value='BMP File')
   button = OBJ_NEW ('ButtonWidget', saveasID,  Name='PNG', Value='PNG File')
   button = OBJ_NEW ('ButtonWidget', saveasID,  Name='PS', Value='PostScript File')
   exitBttn = OBJ_NEW ('ButtonWidget', fileMenu,  Name='Exit', Value='Exit', /Separator)

   ; Add a draw widget
   IF Keyword_Set(object_graphics) THEN $
      self._draw = OBJ_NEW ('ODrawWidget', self, XSIZE=xsize, YSize=ysize, $
         Erase_Window=erase_window) ELSE $
      self._draw = OBJ_NEW ('SelectableDrawWidget', self, XSIZE=xsize, YSize=ysize, $
         Erase_Window=erase_window, INITIAL_COLOR=background_color, COLOR_OBJECT=color_object, $
         /Select_Events, /Button_Event, Name='DRAWWIDGET', $
         Event_Method='DrawWidgetEventHandler', /Notify_Realize)

   ; Report completion and return
   self -> Report, /Completed
   RETURN, ok
END


;*****************************************************************************************************
;
; NAME:
;       CatGraphicsWindow CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the CatGraphicsWindow object.
;
;*****************************************************************************************************
PRO CatGraphicsWindow__DEFINE, class

   class = { CatGraphicsWindow, $
             _draw:OBJ_NEW (), $     ; The draw widget object.
             _output_filename: "", $ ; The base name of the output file for the SAVE AS functionality.
             INHERITS TopLevelBase }

END



;*****************************************************************************************************
;
; NAME:
;       CatGraphicsWindow TEST
;
; PURPOSE:
;
;       This is a test program for the CatGraphicsWindow object.
;
;*****************************************************************************************************
PRO CatGraphicsWindowTest

   gDirectWindow = Obj_New('CatGraphicsWindow', XSize=400, YSize=400, XOFFSET=100, YOFFSET=200)
   image = Obj_New('CatImage', Loaddata(7))
   gDirectWindow -> Add, image
   gDirectWindow -> Draw

   gObjectWindow = Obj_New('CatGraphicsWindow', XSize=500, YSize=500, /Object_Graphics, XOFFSET=600, YOFFSET=200)
   surface = Obj_New('CatSurface', Loaddata(2))
   gObjectWindow -> Add, surface
   gObjectWindow -> Draw

END
