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

function score_peaks, cov, ix, iy

;give each peak a score based on its height relative to the surrounding
;   lands

cov2 = cov

totalrange = max(cov)-min(cov)

dims = size(cov, /dimensions)
secondder = fltarr(dims[0], dims[1], 4)
a = n_elements(ix)
score = fltarr(a)
movevector = [ [1,0], [0,-1], [-1,0], [0,1] ]


;There's some interesting shifting going on here -- 
;   Basically, I want the movevector to be the vector that I use to
;   search around the peak points.  Since the 'shift' command really
;   works in the opposite direction from what I want, for a situation
;   where I'm going to search using a move-vector of 1, I need to shift
;   -1.  Note also that I need to shift separately for each direction so
;   that the approximate 1st and second derivatives are at the distant
;   point of the pair being considered;  this way, I more accurately 
;   tie the derivative to the point.  This is why I shift twice

;Create a smoothed version of the covariogram.  This is used only to
;  find the breaks, not to determine the value of the score
  cov2 = sfit(cov, 5)	;smooth for finding breaks.  

for dir = 0,3 do begin
  tempfirst = $
  	shift( (shift(cov2,(-1)* movevector[0,dir],(-1)* movevector[1,dir])-cov2), $	
  			movevector[0,dir], movevector[1,dir])	
  secondder[*,*,dir] = $
  	shift( (shift(tempfirst,(-1)* movevector[0,dir], (-1)*movevector[1,dir])-tempfirst), $
  			movevector[0,dir], movevector[1,dir])
end

;for each peak from ix,iy, determine the height from the surrounding
;   points


						
tempos= bytarr(2,4)	;for temp storage of locations
dist = fltarr(4)	;for distances from peak

  for i = 0,a-1 do begin
  edgepoint = bytarr(4)	;is this point the edge of the variogram? If
			;   so, we don't really know if this is the true
			;   base of the peak.   for dir = 0,3 do begin
  weights = fltarr(4)	;reset each time
     for dir = 0,3 do begin
        testp = [ix[i],iy[i]]
        loop:
        testp=testp+movevector[*,dir]	
        if testp[0] gt dims[0]-1 or testp[0] lt 0 or $
        	testp[1] gt dims[1]-1 or testp[1] lt 0 then begin
        			testp = testp-movevector[*,dir]
        			edgepoint(dir)=1
        			goto, done
        			end
        			
        if secondder[testp[0],testp[1], dir] lt 0 then goto, loop  
        				       	;if 2nd negative, 
        					;then we're still on a peak
        
           
        done:  
        
        tempos[*,dir] = testp	;set this point to the edge of skirt of the peak
        dist[dir] = abs(ix[i]-testp[0]) + abs(iy[i]-testp[1])	;distance
        							;without needing
        				;to square it and sqrt since it's 
        				;in cardinal directions
   end
   
   ;with all four skirt directions set, now interpolate what the cov
   ;   value should be at the point of the peak
   
    if total(dist) eq 0 then score[i] = 0 else begin
   
   
   ;Need to pick only those points that are not at the edge of 
   ;   variogram
     
     goodpoints = where(edgepoint eq 0, many)
     
     if many eq 0 then goodpoints = [0,1,2,3]	;if the peak is the
     							;whole area, then 
     							;we have to consider
     							;the edges after all
     
     if many eq 1 then weights[goodpoints] = 1 else begin
     	weights[goodpoints] = 1-(dist[goodpoints]/ (total(dist[goodpoints])))
        weights = weights / total(weights)
     end
     
     
     interpval= cov[tempos[0,0], tempos[1,0]] * weights[0] + $
   		cov[tempos[0,1], tempos[1,1]] * weights[1] + $
   		cov[tempos[0,2], tempos[1,2]] * weights[2] + $
   		cov[tempos[0,3], tempos[1,3]] * weights[3] 
   		
   ;What is the difference between that and what we find?
     
     score[i] = ( cov[ix[i],iy[i]] - interpval )   /   totalrange
   
   
   end
   
   
    next: 
     
     
  end
  



return, score

end
 
   
        
        
         