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

function steepness, var, focuspoint

;given a 2-dimensional variable, this function calculates the 
;   approximate first derivative for the four cardinal directions
;   centered on the focuspoint.  The distance option determines
;   how many pixels away from the focuspoint we should use for
;   derivative calculation
; focuspoint is a 2-element array with x,y coordinates in the 
;		var array subscript space

;print, 'RUNNING STEEPNESS.FUNCTION'
;print, 'With parameters'
;print, 'var'
;help, var
;print, 'focuspoint', focuspoint


;assign distance as 

  if n_elements(distance) eq 0 then distance = 2	;pixels
;print, 'distance', distance  

;need to size just to keep from
;going off the edge

  varsize= size(var)	

;need to stretch the variable from 0 to 1 so we're always comparing
;	slopes to a common mark

  newvar= stretch(var) / 255.0
  
  
;figure out normalized distance units, giving the full space a 10 by 10
;   grid structure

  ratio = [varsize(1),varsize(2)] / [10.,10.]

;get edges of the peak

  breaks = find_breaks(var, focuspoint)

;get steepnesses

  test = fill_arr(focuspoint,4)
  diff = var[test[0,*],test[1,*]] - var[breaks[0,*], breaks[1,*]] 
  dist = [sqrt( [test[0,*]-breaks[0,*]]^2 + [test[1,*]-breaks[1,*]]^2 ) ] / $
  		fill_arr(ratio, 4)
  
      a=where(dist eq 0, many)
      if many ne 0 then dist[a]=1	;we set denominator to 1 since 
      					;numerator will be 0 anyway
  stps = diff/dist
  
  
return, stps
end

 
   