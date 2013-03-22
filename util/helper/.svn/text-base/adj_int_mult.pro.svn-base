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

;===================================================================
;#############################################################################
function adj_int_mult, upper_left, pixel_size, candidate_value, map=map

;PURPOSE:  returns the candidate value adjusted to an integer multiple of
;     pixels away from the upperleft pixel. If upper_left is center of pixel,
;     then the returned value will also be center of pixel.  If upper_left
;     is corner of pixel, the returned value will also be corner.
;ASSUMPTIONS:  both upper_left and candidate_value are in file coordinate system
;
;RULE:  half-way points moved up and to the left
;Rules:
;map
;x: -.49 to .5 --> 0    .51 to 1.5 --> 1
;y: -.5 to .49 --> 0    -.51 to -1.5 --> -1
;regular
;x:  -.49 to .5 --> 0  .51 to 1.5 --> 1
;y:  -.49 to .5 --> 0  .51 to 1.5 --> 1


if n_elements(map) eq 0 then mapadj = [1,1] else $
	if map eq 0 then mapadj = [1,1] else mapadj = [1,-1]


pixels = ( double(candidate_value) - upper_left ) / (pixel_size*mapadj)


return, [ (ceil(pixels(0)-0.5)*pixel_size(0))+upper_left(0), $
	  (mapadj(1)*ceil(pixels(1)-0.5)*pixel_size(1))+upper_left(1) ]
end


;March 5th, 1998changed assumption to not in map coords, so you ned to set
;  /map to have it work like before.  I've checked this for the following
;  programs:  fix_edge.function, squeeter, squeeter_procbundle,
;		calc_rot_vector, edgit.function, covar, pick_gcps.pro,
;		aggby, squeet.pro, squeet_handler.pro, squeet_procbundle

