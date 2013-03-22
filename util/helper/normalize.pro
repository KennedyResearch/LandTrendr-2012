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

function normalize, im, ignore = ignore


on_error, 2

image=float(im)


if n_elements(ignore) eq 0 then ign_st = 0 else begin
				ign_val = ignore
				ign_st=1
				end
	

if ign_st eq 0 then begin
	t=where(image eq image)
	a=image(t) 
end else begin
	bad = where(image eq ign_val, many)
	
     ;need to have at least three good points to do std
     ;   so we check against the total number of possible
     ;   points-2	
	if many ge n_elements(image)-2 then begin	
			image(*,*)=-99
			return, image ;if the whole image is ignore values
			end
	if many eq 0 then begin
	     ign_st = 0
             t=where(image eq image)
        end else t=where(image ne ign_val, many) 
        
        a=image(t)
end
			
mom=meanstd(a)
a= (a-mom(0))/float(mom(1))

image=float(image)
image(t) = a
if ign_st eq 1 then image(bad)=-99  

return, image
end

			


		

