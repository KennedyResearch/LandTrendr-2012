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

;
; Copyright (c) 1998, Oregon State University.
; This software is currently in beta-test condition.  It may not be altered,
;	copied, or redistributed without express written permission of
;	Robert E. Kennedy, Oregon State University (kennedyr@fsl.orst.edu).
;	This software is supplied as is, with no express or implied
;	warranties.

function blowup, image, by
on_error, 2
r = size(image, /dim)
if n_elements(r) eq 3 then nlayers = r[2] else nlayers = 1

a=size(image)
dimz = [a(1), a(2)]
if nlayers eq 1 then typenum = a[3] else typenum = a[4]
newdims = dimz* [by, by]

base = image[0,0]-image[0,0]
newimage = replicate(base, newdims[0], newdims[1], nlayers)


;case 1 of
;(typenum eq 1):	newimage=bytarr(newdims(0),newdims(1), nlayers)
;(typenum eq 2): newimage=intarr(newdims(0),newdims(1), nlayers)
;(typenum eq 3): newimage=fltarr(newdims(0),newdims(1), nlayers)
;(typenum eq 4): newimage=fltarr(newdims(0),newdims(1), nlayers)
;else:	message, 'Not recognized image type'
;endcase
for i = 0, nlayers-1 do begin

for x = 0, dimz(0)-1 do begin
  for y = 0, dimz(1) -1 do begin
  newimage(x*by:(x*by)+by-1, y*by:(y*by)+by-1, i) = image(x,y,i)
  end
end

end

newimage = reform(newimage)
return, newimage
end

