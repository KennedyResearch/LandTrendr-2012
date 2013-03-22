;**************************************************************************** 
;Copyright © 2008-2011 Oregon State University                                
;All Rights Reserved.                                                         
;                                                                             
;                                                                             
;Permission to use, copy, modify, and distribute this software and its        
;documentation for educational, research and non-profit purposes, without     
;fee, and without a written agreement is hereby granted, provided that the    
;above copyright notice, this paragraph and the following three paragraphs    
;appear in all copies.                                                        
;                                                                             
;                                                                             
;Permission to incorporate this software into commercial products may be      
;obtained by contacting Oregon State University Office of Technology Transfer.
;                                                                             
;                                                                             
;This software program and documentation are copyrighted by Oregon State      
;University. The software program and documentation are supplied "as is",     
;without any accompanying services from Oregon State University. OSU does not 
;warrant that the operation of the program will be uninterrupted or           
;error-free. The end-user understands that the program was developed for      
;research purposes and is advised not to rely exclusively on the program for  
;any reason.                                                                  
;                                                                             
;                                                                             
;IN NO EVENT SHALL OREGON STATE UNIVERSITY BE LIABLE TO ANY PARTY FOR DIRECT, 
;INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST      
;PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
;IF OREGON STATE UNIVERSITYHAS BEEN ADVISED OF THE POSSIBILITY OF SUCH        
;DAMAGE. OREGON STATE UNIVERSITY SPECIFICALLY DISCLAIMS ANY WARRANTIES,       
;INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
;FITNESS FOR A PARTICULAR PURPOSE AND ANY STATUTORY WARRANTY OF               
;NON-INFRINGEMENT. THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS,    
;AND OREGON STATE UNIVERSITY HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE,       
;SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.                            
;                                                                             
;**************************************************************************** 

;+
; NAME:
;  PARSE_CLASS_CODES
;
; PURPOSE:
;
;  interpret landtrendr class label code
;
; AUTHOR:
;
;
; CATEGORY:
;     Landtrendr, Post-Processing
;
; CALLING SEQUENCE:
;
;
; INPUTS:
;   class_codes: class code array.
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;   09.17.2010. Intial creation by Yang
;
;
;******************************************************************************************;
;  Copyright (c) 2010, by ???                                ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;

function parse_class_codes, class_codes

  n_classes = n_elements(class_codes)
  if n_classes eq 0 then return, {ok:0, message:'No class labels supplied'}
  
  base = {class_num:0, class_name:'', n_segs:0, $
          write_details:0, $
          type:ptr_new(), $ ;thresh:ptr_new(), $
          year_rule:ptr_new(), year:ptr_new(), $
          duration_rule:ptr_new(), duration:ptr_new(), $
          precover_rule:ptr_new(), precover:ptr_new() $
         }
  
  class_structure = replicate(base, n_classes)
  
  ;now interpret each code
  for i = 0, n_classes -1 do begin
    this = strsplit(class_codes[i], '#', /extract)
    
    if n_elements(this) LT 4 then return, {ok: 0, message: 'mal-formated class code'}
    
    class_structure[i].class_num = fix(this[0])
    if class_structure[i].class_num eq 1 then return, {ok:0, message:"Class 1 is reserved for no-change"}
    if class_structure[i].class_num eq 2 then return, {ok:0, message:"Class 2 is reserved for no-match pixels"}
    
    class_structure[i].class_name = this[1]
    
    case this[2] of
      'Y': class_structure[i].write_details = 1 
      'N': class_structure[i].write_details = 0
      else: return, {ok:0, message:"Only year rules Y, N are allowed"}
    endcase
    
    ;check code definition:
    ;   1. at least one of the code is not placeholder
    ;   2. two place holders can not next to each other
    tmp_codes = this[3:*]
    test_codes = strpos(tmp_codes, 'XX')
    if total(test_codes) eq 0 then return, {ok:0, message:'At least one code is required as non-place holder'}
    
    ;check consecutive segment code
    if n_elements(tmp_codes) gt 1 then begin
        t2 = shift(test_codes, -1)+test_codes
        t2 = t2[0:n_elements(t2)-2]
        t3 = where(t2 eq 0, n0)
        if n0 gt 0 then return, {ok:0, message:'Place holders cannot next to each other'}
    endif
    
    n_segs = n_elements(this)-3
    class_structure[i].n_segs = n_segs
    
    type = strarr(n_segs)
    ;thresh = intarr(n_segs)
    year_rule = strarr(n_segs)
    year = intarr(n_segs)
    dur_lorg = strarr(n_segs) ;0: le    1: gt
    duration = bytarr(n_segs)
    cover_lorg = strarr(n_segs)
    cover = bytarr(n_segs)
    
    for j = 0, n_segs - 1 do begin
      this_one = this[j+3] ;TTRRyyyyDnnCnnW
      this_type = strupcase(strmid(this_one, 0, 2)) ;TT
      
      type[j] = this_type
      case this_type of ;the follow just serves as check for code validity
        'FD': ;First Disturbance
        'RD': ;Recent Disturbance
        'GD': ;Greatest Disturbance
        'SD': ;Second-greatest Disturbance
        'LD': ;Longest Disturbance
        'FR': ;First Recovery
        'RR': ;Recent Recovery
        'GR': ;Greatest Recovery
        'SR': ;Second-greatest Recovery
        'LR': ;Longest Recovery
        'XX': ;if j eq 0 or j eq n_segs-1 then return, {ok:0, message:"Spacer types must have actual types before and after"}
        else: return, {ok:0, message:"Only types FD, RD, GD, SD, LD, FR, RR, GR, SR, LR, or XX are allowed."}
      endcase
      
      this_type = strupcase(strmid(this_one, 2, 2)) ;RR
      year_rule[j] = this_type
      case this_type of
        'EQ':
        'LE':
        'GE':
        'XX':
        else: return, {ok:0, message:"Only year rules EQ, LE, GE, or XX are allowed"}
      endcase
    
      this_year = fix(strmid(this_one, 4, 4))
      if this_year eq 0 then year[j]=-1 else year[j] = this_year
      
      this_dur = strupcase(strmid(this_one, 8, 1))
      dur_lorg[j] = this_dur
      case this_dur of
        'G':
        'L':
        'X':
        else: return, {ok:0, message:"Only duration rules G (>), L (<=), or X are allowed"}
      endcase
      this_dur = fix(strmid(this_one, 9, 2))
      duration[j] = this_dur

      this_pre = strupcase(strmid(this_one, 11, 1))
      cover_lorg[j] = this_pre
      case this_pre of
        'G':
        'L':
        'X':
        else: return, {ok:0, message:"Only predisturbance rules G (>), L (<=), or X are allowed"}
      endcase
      ;if strupcase(strmid(this_one, 11, 1)) eq 'G' then cover_lorg[j] = 1 ; less than or equal: 0, greater than:1 (or X, but that's ignored)
      this_cover = fix(strmid(this_one, 12, 2))
      cover[j] = this_cover
    endfor
    
    class_structure[i].type = ptr_new(type, /alloc)
    class_structure[i].year_rule = ptr_new(year_rule, /alloc)
    class_structure[i].year = ptr_new(year, /alloc)
    class_structure[i].duration_rule = ptr_new(dur_lorg, /alloc)
    class_structure[i].duration = ptr_new(duration, /alloc)
    class_structure[i].precover_rule = ptr_new(cover_lorg, /alloc)
    class_structure[i].precover = ptr_new(cover, /alloc)
  end

  return, {ok:1, class_structure:class_structure}
end