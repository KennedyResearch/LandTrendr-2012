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

;General procedures used at multiple times in the squeeter family



;#############################################################################

function define_window, image_info,  params

;given a centerpoint from some calling program, define the boundaries of 
;   the window to use.  Currently called by sq_load_images and by 
;   sq_movecenter

;First make sure that the centerpoint, around which we're going to 
;	make a window and extract, is either a center of a pixel if
;	the window_size is odd, or the upperleft edge if the window_size
;	is even
;We know that the image_info.upl should be the upper left corner of the image
;	so we only need to adjust that if window_size is odd, which would
;	be the case if evenodd() is 1.  So we adjust by .5 pixels times evenodd,
;	which is 0 if window size is even and .5,-.5 if window size is odd.
;This ensures that from here on out we're dealing with values that will make
;	zot_img happy when it loads in the image


  oldcenterpoint = image_info.centerpoint
  centerpoint = adj_int_mult(image_info.upl, $
  		image_info.pixelsize,image_info.centerpoint, /map) + $
  		[image_info.pixelsize(*)] * [.5,-.5] * $
  		[(params.window_size[0] mod 2), (params.window_size[1] mod 2)] 
  difference = oldcenterpoint - centerpoint 
  
;Now we define the edge of the window based on the startpoint as the
;	center.  If we're off the edge of the image, we need to move the
;	center point and keep track of the offset so we can pass it back
;	to squeeter.  Squeeter needs to know, since it needs to apply it
;	to the next center point before it calls this routine again

  win_size_map = params.window_size*image_info.pixelsize
  bounds = dblarr(2,2)
  bounds(*,0) = edgit(centerpoint, win_size_map, /map)
  bounds(*,1) = edgit(centerpoint, win_size_map, /lowerright, /map)
  
  ;test edge.  Diffs holds the values that you add to bounds
  ;	to get the original value
    
    ;fixed = fix_edge(bounds, [ [image_info.upl],[image_info.lor] ], $
    ;			/map, /noshrink)
    
    
    fixed = fix_edge(bounds, [ [image_info.upl],[image_info.lor] ], $
    			/map)
    valid = fixed.valid 
    bounds = fixed.coords
         ;offset = fixed.diffs(*,0)	;since we chose /noshrink, the .diffs
    	        			;flag is the same for upl and lor, so 
    		        		;we just use the upl value
    offset = fixed.diffs
     
     
     ;Aug 25th.  commented out centerpoint adjustment.  Now define
     ;window essentially just returns the value that would be needed 
     ;to get within the image bounds
       				
    ;centerpoint = centerpoint - offset	;adjust the 
        				;start point
        				;so it's still
        				;in the center
    ;at this point, difference holds the shoving that we did to 
    ;  move the point to a center of a pixel.  Offset holds the
    ;  the value that the center point was shoved to move it to a center
    ;  of a pixel.  The total offset is the addition of both of those.
    
    ;totaloffset = offset + difference	
    
    
 ;centerpoint is the new centerpoint of the image
 ;corners = the edges of the new window
 ;offset is the offset caused by both the adjustment to pixel integer 
 ;	increments and because of running into any edges
 ;shift is simply the offset caused by adjustment to pixel integer vals
 ;valid is zero if we couldn't fix the edge -- this would only happen if
 ;      the requested image were bigger than the original image.
     

ret = {centerpoint:centerpoint, corners:bounds, offset:offset, $
		shift:difference, valid:valid}
return, ret
end

