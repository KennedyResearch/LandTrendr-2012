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

function convert_256im_to_ff, im, rrr, ggg, bbb

  ;Used in make_2d_hist_image_ff.pro
  ;   Goal is to convert the 256-scale image into
  ;   its 0-16million (24bit) color coordinate
  ;   given the colors rrr, ggg, bbb, which allows
  ;   display according to the color ramps in
  ;   xloadct.  run xloadct, then do tvlct, rrr, ggg, bbb, /get

  ;   and then save those as 'some color file name.sav'
  ;   then restore that, and pass the rrr, ggg, bbb


  sz=size(im, /dim)
  if n_elements(sz) gt 2 then begin
    print, 'convert_256im_to_ff assumes a 2-d image.  fatal error. stopping'
    return, [-1]
  end
  
  
  base = im[0]-im[0]
  g = replicate(base, sz[0]*sz[1]*3)
  outim = reform(g, sz[0], sz[1], 3)
  temp=outim[*,*,0]
  
  for r=0,255 do begin
    these = where(im eq r, many)
    if many ne 0 then temp[these] = rrr[r]
  end
  outim[*,*,0] = temp
  
  
  for g=0,255 do begin
    these = where(im eq g, many)
    if many ne 0 then temp[these] = ggg[g]
  end
  outim[*,*,1] = temp
  for b=0,255 do begin
    these = where(im eq b, many)
    if many ne 0 then temp[these] =bbb[b]
  end
  outim[*,*,2] = temp
  
  
  return, outim
  
end
