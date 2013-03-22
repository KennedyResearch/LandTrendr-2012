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




;#############################################################################
function sq_rotate, inp, angle, interp=interp

  ;rotate the two images relative to each other.
  ;   angle of rotation clockwise from due north
  ;   rs- the structure including the reference image
  ;   is-   "    " 	     "     "    input     "

if n_elements(interp) ne 0 then $
    new_image = rot(inp.img, angle, /interp, missing=inp.img_info.ignore) else $
    new_image = rot(inp.img, angle, missing = inp.img_info.ignore)



  ;get new values for the corners (center stays the same)
  ;  see page 67, bottom for information on rationale for doing this.
  ;  But basically, if we rotate the original image clockwise 30 degrees,
  ;  we want to know what the new upper left corner is in the coordinate
  ;  system of the original image.  The easiest way to think of this is
  ;  to envision the upper left of the original image turning back
  ;  (counterclockwise) 30 degrees.  The way to calculate that is
  ;  just use the negative of the angle that the user gives


    ;first, get a dx, dy from center to original edge
      diff_upl = inp.img_info.centerpoint - inp.img_info.upl
      diff_lor = inp.img_info.centerpoint - inp.img_info.lor

    ;then get a new dx, dy for the rotated edges

      new_diff_upl = calc_rot_vect(-angle)##diff_upl
      new_diff_lor = calc_rot_vect(-angle)##diff_lor

    ;now add back to centerpoint

      nupl = inp.img_info.centerpoint - new_diff_upl
      nlor = inp.img_info.centerpoint - new_diff_lor

  ;return the image with new information

     img_info = {filename:inp.img_info.filename, upl:nupl, lor:nlor, $
          pixelsize:inp.img_info.pixelsize, centerpoint:inp.img_info.centerpoint, $
          layer:inp.img_info.layer, gifov:inp.img_info.gifov, $
          ignore:inp.img_info.ignore}
     ret = {img:new_image, img_info:img_info, offset:inp.offset, $
    		valid:inp.valid}

   return, ret
 end

