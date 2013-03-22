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

function angle_diff, xcoords, ycoords, yrange, distweightfactor=distweightfactor


;need three points -- middle point is the one that gets the score
;  the others are the ones preceding and following
;Note that ycoords needs to be scaled to the range of the whole
;  trajectory for this to really be meaningful


if n_elements(xcoords) ne 3 or n_elements(ycoords) ne 3 then return, -1

;distweightfactor helps determine how much weight is given to angles
;  that precede a disturbance.  If set to 0, the angle difference is
;  passed straighton.
if n_elements(distweightfactor) eq 0 then distweightfactor = 2


;get the slope of the prior to current points
  ydiff2 = (ycoords[2]-ycoords[1])
  ydiff1 = (ycoords[1]-ycoords[0])

  angle1 = atan(float(ydiff1)/(xcoords[1]-xcoords[0]))
  angle2 = atan(float(ydiff2)/(xcoords[2]-xcoords[1]))

;compare 'em

  mx = max([angle1, angle2], min=mn)
;
  scaler = max([0, (ydiff2*distweightfactor)/yrange])+1;+(abs(ydiff1)/yrange)+1.	;if disturbance (positive), give more weight
;;  stop
;print, scaler
  diff = (mx-mn) * scaler
;
;  ;*max([abs(ydiff1),abs(ydiff2)])  ;give more weight to vertices near a big change

 diff = max([abs(angle1), abs(angle2)])*scaler



 return, diff


end
