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
function sq_eqpixsize, refinfo, inp

  ;function to take the input image and match its pixel size to the 
  ;    reference pixel size.  We make sure that the center points of
  ;    of the two images are in the same position relative to the
  ;    center pixel --i.e. center or upper edge of pixel.

  ;how different are the pixel sizes?
    blowupby = refinfo.gifov / inp.img_info.gifov	;figure out
  					;the relationship between the
  					;the reference image and the area
  					;on the ground in reference units
  					;captured in one input pixel

  ;call on aggby function to make a resized image
  
    new_image = aggby(inp.img, blowupby, ignore=inp.img_info.ignore)
    
  
  ;figure out the new edges 
  ;The center point remains the same, but the upper left and lower right
  ;   likely will have changed.  We calculate them based on the center point
  ;   and half of the new window size (windims) 
  ;The GIFOV should now be equal to the GIFOV of the reference image.
  
    windims = dims(new_image)
    npixsize = inp.img_info.pixelsize * blowupby
    nupl = inp.img_info.centerpoint - ( npixsize*[.5, -.5]*windims)
    nlor = inp.img_info.centerpoint + ( npixsize*[.5, -.5]*windims)
    
   
    img_info = {filename:inp.img_info.filename, upl:nupl, lor:nlor, $
          pixelsize:npixsize, centerpoint:inp.img_info.centerpoint, $
          layer:inp.img_info.layer, gifov:refinfo.gifov, $
          ignore:inp.img_info.ignore}
    ret = {img:new_image, img_info:img_info, offset:inp.offset, $
    		valid:inp.valid}
  
  
return, ret
end
