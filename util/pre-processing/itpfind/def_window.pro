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

function def_window, centerpoint, image_info, params

;given a trial point and the image_info.upl, .lor and the params window-size
;  information, this will define the window

  ;make a copy of the centerpoint  and fix it for the distance from the
  ;   corner

    cp = centerpoint
    upperleftpixelcenter = edgit(image_info.upl, image_info.pixelsize, /map, /tocenter)	;added april 27, 2008 to see if the problem with the rotation

	cp = adj_int_mult(upperleftpixelcenter, image_info.pixelsize, cp, /map) + $
 		[image_info.pixelsize(*)] * [.5,-.5] * $
  		[1-(params.window_size[0] mod 2), 1-(params.window_size[1] mod 2)]

;    cp = adj_int_mult(image_info.upl, image_info.pixelsize, cp, /map) + $
;  		[image_info.pixelsize(*)] * [.5,-.5] * $
;  		[(params.window_size[0] mod 2), (params.window_size[1] mod 2)]
;stop



  ;Define the edge of the window based on the cp and the params.window_size

    win_size_map = params.window_size * image_info.pixelsize
    bounds = dblarr(2,2)
    bounds(*,0) = edgit(cp, win_size_map, /map)
    bounds(*,1) = edgit(cp, win_size_map, /lowerright, /map)

  ;Test edge.  Don't allow shrinking.  When it gets to an edge, it'll
  ;	move into the image.

    fixed = fix_edge(bounds, [ [image_info.upl], [image_info.lor] ], $
    			edgezone=params.edgezone*image_info.pixelsize, $
    			/noshrink, /map)

    new_centerpoint = cp-fixed.diffs[*,0]
    ;-->print, 'Image info'
    ;-->print, image_info
    ret = {centerpoint:new_centerpoint, corners:fixed.coords, offset:fixed.diffs, $
	shift:centerpoint-cp, valid:fixed.valid}

return, ret
end


