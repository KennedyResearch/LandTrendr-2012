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

function stringswap, string, regexp, swapto

;Take a string, swap the "swapto" string for all cases of the "regexp"
;They need not be same length

sz = size(string)
if sz[0] eq 0 then usestring = [string] else usestring = string
n = n_elements(usestring)
outstring = strarr(n)


 rlen = strlen(regexp)
 swlen =strlen(swapto)

for i = 0, n-1 do begin
    stlen = strlen(usestring[i])
    a = strpos(usestring[i], regexp)
    secondpiece = a+rlen
    if a ne -1 then $
      outstring[i] = strmid(usestring[i], 0, a)+swapto+$
   		strmid(usestring[i], secondpiece,  stlen-secondpiece+1) else $
      outstring[i] = usestring[i]
   
  
  ;Test if there are any more, if so recurse
  ;but on the off chance the regexp and swapto are the same, we need to 
  ;avoid endless recursion

    if regexp ne swapto then begin 
     a1 = strpos(outstring[i], regexp)
     if a1 gt (a+swlen) then begin	;check to see if it's past the end of the added section
	stringtemp = strmid(outstring[i],a+swlen, strlen(outstring[i])-(a+swlen)+1)	;if so, just grab this latter part to swap
	outstring[i] = strmid(outstring[i], 0, a+swlen)+stringswap(stringtemp, regexp, swapto)	;construct the new from parts
     end
    end
end ;i

return, outstring
end

   		
