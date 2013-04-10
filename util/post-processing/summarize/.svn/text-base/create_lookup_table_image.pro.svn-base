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

function create_lookup_table_image, image

;swap all unique values in an image with new unique ordinal
;   counting values.  For example, if unique values are 3,9,13,109, the 
;   new values would be 1,2,3,4. 
;   Returns the new image and the lookup table to keep track of the original values

u = fast_unique(image)
u= u[sort(u)]

n=n_elements(u)
case 1 of 
(n lt 254): base = bindgen(n)+1
(n lt 65534):  base = uindgen(n)+1
else:   base = lindgen(n)+1
endcase
lookup = lonarr(2, n)
lookup[0,*] = base
lookup[1,*] = u

j = histogram(image, omin = omin, reverse_indices = r)
histsize = r[0]-1 ;the first in the reverse indices points to the beginning of the 
                ; lookup list, so that minus 1 is the number of elements in the 
                ; histogram
                
outimage = image


for i = 0, n-1 do begin 
  ;if the unique elements and the histogram match (and they'd better)
  ;    the unique values should point to places in the histogram 
  ;    with non-zero counts, once the omin is taken into account. 
  
  this_one = u[i]-omin
  if r[i] ne r[i+1] then outimage[r[r[this_one]:r[this_one+1]-1]] = lookup[0, i]
end


return, {ok:1, outimage:outimage, lookup:lookup}

end
  