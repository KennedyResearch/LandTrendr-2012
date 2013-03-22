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

function edgit, coordinate, pixel_size, tocenter=tocenter, $
	lowerright=lowerright, map=map

;Purpose:  adjust coordinates to centers or edges of pixel, given the 
; other

;Default mode:  Given the center point and pixel size, the function returns 
;		the upper left corner of the pixel.  
;/tocenter:  the coordinate given to the program is the upper left edge, so the
;		program returns the center value.
;/lowerright:  toggles either or both of the other options so that the 
;		lower right edge is considered.  In other words, if /tocenter
;		is chosen with /lowerright, the coordinate value given by
;		user is assumed to be the lowerright of the pixel.  If 
;		/tocenter is not chosen, then the user-defined coordinate is
;		assumed to be the center and the lowerright is returned.


if n_elements(tocenter) ne 0 then adj = [-.5,.5] else adj = [.5,-.5]
if n_elements(lowerright) ne 0 then adj = adj*[-1,-1]
if n_elements(map) eq 0 then adj = adj*[1,-1] else $
	if map eq 0 then adj = adj*[1,-1] ;alter the y if not using map


return, coordinate - (pixel_size*adj)
end

;added /map command so that non-map coordinates could be calc'd.  I checked
;  all the relevant programs and added the /map command where appropriate.
;  Programs checked:  fix_edge, calc_rot_vect, covar,aggby,squeeter.function,
;			squeeter_procbundle,squeet_procbundle,squeet.pro,
;			squeet_handler
