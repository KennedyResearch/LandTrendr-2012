;*****************************************************************************************************
;+
; NAME:
;       SYSTEMCOLORS
;
; PURPOSE:
;
;       The program creates an object to manipulate system colors.
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
;       Utility Objects.
;
; CALLING_SEQUENCE:
;
;       sysColorObject = Obj_New('SystemColors')
;
; INPUT_ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 12th March 2003
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
FUNCTION SystemColors::GetColors

   colors = { frame:self.frame, text:self.text, active:self.active, shadow:self.shadow, $
              highlight:self.highlight, edge:self.edge, selected:self.selected, face:self.face }

   RETURN, colors

END


PRO SystemColors::Show

   @cat_pro_error_handler

   tlb = Obj_New('TopLevelBase', Row=1, XPad=0, YPad=0, Space=0)
   draw = Obj_New('DrawWidget', tlb, XSize=175, YSize=400)
   tlb -> Draw, /Center

   draw -> SetWindow
   Device, Decomposed=1, Get_Decomposed=theState
   x = [0, 50, 50, 0, 0]
   Polyfill, x, [0, 0, 50, 50, 0] + (50)*0, /Device, Color=self.face
   Plots, x, [0, 0, 50, 50, 0] + (50)*0, /Device, Color=FSC_Color('white')
   XYOuts, 75, 25, 'Face', /Device, Font=0, Color=FSC_Color('white')
   Polyfill, x, [0, 0, 50, 50, 0] + (50)*1, /Device, Color=self.selected
   Plots, x, [0, 0, 50, 50, 0] + (50)*1, /Device, Color=FSC_Color('white')
   XYOuts, 75, 75, 'Selected', /Device, Font=0, Color=FSC_Color('white')
   Polyfill, x, [0, 0, 50, 50, 0] + (50)*2, /Device, Color=self.edge
   Plots, x, [0, 0, 50, 50, 0] + (50)*2, /Device, Color=FSC_Color('white')
   XYOuts, 75, 125, 'Edge', /Device, Font=0, Color=FSC_Color('white')
   Polyfill, x, [0, 0, 50, 50, 0] + (50)*3, /Device, Color=self.highlight
   Plots, x, [0, 0, 50, 50, 0] + (50)*3, /Device, Color=FSC_Color('white')
   XYOuts, 75, 175, 'Highlight', /Device, Font=0, Color=FSC_Color('white')
   Polyfill, x, [0, 0, 50, 50, 0] + (50)*4, /Device, Color=self.shadow
   Plots, x, [0, 0, 50, 50, 0] + (50)*4, /Device, Color=FSC_Color('white')
   XYOuts, 75, 225, 'Shadow', /Device, Font=0, Color=FSC_Color('white')
   Polyfill, x, [0, 0, 50, 50, 0] + (50)*5, /Device, Color=self.active
   Plots, x, [0, 0, 50, 50, 0] + (50)*5, /Device, Color=FSC_Color('white')
   XYOuts, 75, 275, 'Active', /Device, Font=0, Color=FSC_Color('white')
   Polyfill, x, [0, 0, 50, 50, 0] + (50)*6, /Device, Color=self.text
   Plots, x, [0, 0, 50, 50, 0] + (50)*6, /Device, Color=FSC_Color('white')
   XYOuts, 75, 325, 'Text', /Device, Font=0, Color=FSC_Color('white')
   Polyfill, x, [0, 0, 50, 50, 0] + (50)*7, /Device, Color=self.frame
   Plots, x, [0, 0, 50, 50, 0] + (50)*7, /Device, Color=FSC_Color('white')
   XYOuts, 75, 375, 'Frame', /Device, Font=0, Color=FSC_Color('white')
   Device, Decomposed=theState

   self -> Report, /Completed
END


PRO SystemColors::CLEANUP

   @cat_pro_error_handler

   self -> CatAtom::CLEANUP

   self -> Report, /Completed

END


FUNCTION SystemColors::INIT, _Extra=extraKeywords

   @cat_func_error_handler

   ; Create a widget and obtain sytem colors from it.
   tlb = Obj_New('TopLevelBase')
   tlb -> GetProperty, ID=id
   colors = Widget_Info(id, /System_Colors)
   Obj_Destroy, tlb

   ; Call the superclass INIT method.
   ok = self->CatAtom::INIT(_Extra=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; Fill the object fields.
   self.face = Color24(colors.face_3d)
   self.selected = Color24(colors.highlight)
   self.edge = Color24(colors.light_edge_3d)
   self.highlight = Color24(colors.light_3d)
   self.shadow = Color24(colors.shadow_3d)
   self.active = Color24(colors.active_border)
   self.text = Color24(colors.button_text)
   self.frame = color24(colors.window_frame)

   self -> Report, /Completed

   RETURN, 1

END

PRO SystemColors__Define, class

   class = { SYSTEMCOLORS, $
             face: 0L, $
             edge: 0L, $
             highlight: 0L, $
             shadow: 0L, $
             active: 0L, $
             text: 0L, $
             selected: 0L, $
             background: 0L, $
             frame: 0L, $
             INHERITS CatAtom $
           }

END

