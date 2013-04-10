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

function combine_two_images, image1, image2, image1_lookup=image1_lookup


;combine two images with same footprint and create 
;   a new image with lookup table to get back to the original values
;   image1 is always considered the "base" and image2 the add-on
;   returned is the lookup table with
;   [number_code_in_returned_image, value_in_image1, value_in_image2]
;   if image1 was itself the product of this routine, then 
;   the "value_in_image1" is actually expanded to include as many columns
;   as needed to have the image1_lookup array work right. 



sz1=size(image1, /dim)
u1 = fast_unique(image1)
u1 = u1[sort(u1)]
n1 = n_elements(u1)

sz2=size(image2, /dim)
u2 = fast_unique(image2)
u2 = u2[sort(u2)]
n2 = n_elements(u2)

if sz1[0] ne sz2[0] or sz1[1] ne sz2[1] then begin
      print, 'image1 and image2 are not of the same size'
      print, 'image1: '+string(sz1)
      print, 'image2: '+string(sz2)
      return, {ok:-1}
 end

outimage = image1-image1

counter = 1l

if n_elements(image1_lookup) ne 0 then begin 
  prior_exists = 1
  sz = size(image1_lookup)
  column_end_prior = sz[0]
end


for i =0,  n1-1 do begin
  for j = 0, n2-1 do begin 
      combo = (image1 eq u1[i]) * (image2 eq u2[j])
      
      these = where (combo ne 0, many)
      
      if many ne 0 then begin 
           ;set the combo image appropriately
           combo = combo * counter    ;set the pixels, zeros remain zero
           outimage = outimage + combo
           
           ;set the lookup
           if prior_exists eq 0 then p = u1[i] else p = image1_lookup[1:column_end_prior-1, i] 
                 
           
           thiscombo = [counter,p, u2[j]]
           if counter gt 1 then expand_rows, lookup, 1 else lookup = thiscombo
           lookup[*,counter-1] = thiscombo
           counter=counter+1
           
      end
      
   end
end



return, {ok:1, outimage:outimage, lookup:lookup}

end
  