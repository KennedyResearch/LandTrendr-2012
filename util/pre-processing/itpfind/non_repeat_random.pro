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

function non_repeat_random, outsize, mult, seed=seed

;Return "number" count of random numbers that are no closer
;   to any each other than 1/mult.  If you want 20 random numbers
;   between 0 and 100, then outsize is 20 and mult is 100

if outsize gt mult then begin
	print, 'There are not enough bins for the requested size'
	return, 0
	end


n_out = n_elements(outsize)
if n_out ne 1 then begin
	print, 'Cannot handle 2-d output'
	end

count = 0
cursize= outsize

if n_elements(seed) eq 0 then seed = randomseed()

loop:

  a = long(randomu(seed, cursize)*mult)
  if count eq 0 then begin
  	ret=[a[uniq(a, sort(a))]]	;if just starting out
      end else begin
        z=n_elements(a)
        t=ret
        expand_cols,t, z, newdims
        if newdims[0] eq -1 then stop


        t[count:count+z-1] = a
        ret = [t[uniq(t, sort(t))]]
      end

   count=n_elements(ret)

   cursize = fix(outsize-count)


   if count ne outsize then goto, loop


return, ret
end




