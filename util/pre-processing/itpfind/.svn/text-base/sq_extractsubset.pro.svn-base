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
function sq_extractsubset, image, center, windowsize, diag_un1=diag_un1


;center is the point around which we want to extract a windowsize's worth
;    of data from 'image'.
;Note:  To make this more efficient, we have to assume that the center/windowsize
;    combination will land directly on the edges of pixels (i.e. we're
;    working with integer multiples of pixel size).  Otherwise we'd need
;    to resample just to subset, which would add too much time.


valid =1
;Note, we'll check to make sure we haven't extended outside the bounds of
;    image.

    write_diagnosis, diag_un1, 'sq_extractsubset:  getting bounds on image subset'

   bounds = [ [edgit(center, windowsize)], $
   	[edgit(center, windowsize, /lowerright)] ]

 ;check against edge

   s = dims(image)
   constraints = [ [-.5,-.5], [s(0)-(0.5), s(1)-(0.5)] ]


   write_diagnosis, diag_un1, 'sq_extractsubset:  fixing edge on subset'
   newb = fix_edge(bounds, constraints, /equalshrink)	;we tell fixedge
   						;to shrink the window an
   						;equal amount on either side,
   						;thus maintaining the center
   						;point.

   if newb.valid eq 0 then begin
    	write_diagnosis, diag_un1, 'sq_extractsubset:  subset did not work with image'
    	new_im = 0
   		valid = 0

   		goto, past
   		end


 ;grab that portion of the image.
    ;of course, we need to adjust to center points of pixels to
    ; do an array subset

    temp_sub_coords = newb.coords+ [ [.5,.5], [-.5, -.5]]


    ;get image
       write_diagnosis, diag_un1, 'sq_extractsubset:  subsetting image'

    new_im = image (temp_sub_coords(0,0):temp_sub_coords(0,1), $
    		      temp_sub_coords(1,0):temp_sub_coords(1,1) )

 past:
return, {img:new_im, valid:valid}
end

