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

function ascribe_2d_hist_vals, original_axis1_vals, original_axis2_vals, $
			histvals, min1, min2, max1, max2, bin1, bin2


;given a 2-d array with counts derived from the hist_2d routine,
;  ascribe the counts to the original values

n = n_elements(original_axis1_vals)

if n_elements(original_axis2_vals) ne n then message, 'original axis vals do not match'

counts = replicate(histvals[0]-histvals[0], n)	;make a blank

sz = size(histvals, /dim)

for x = 0,sz[0]-1 do begin
  for y = 0, sz[1]-1 do begin

     lowcutoff_axis1 = min1+(x*bin1)
     highcutoff_axis1 = lowcutoff_axis1+bin1
     lowcutoff_axis2 = min2+(y*bin2)
     highcutoff_axis2 = lowcutoff_axis2+bin2

     tot = (original_axis1_vals ge lowcutoff_axis1) * $
     		(original_axis1_vals lt highcutoff_axis1) * $
     		(original_axis2_vals ge lowcutoff_axis2) * $
     		(original_axis2_vals lt highcutoff_axis2)
     these = where(tot ne 0, many)
     if many ne 0 then  $
         counts[these] = histvals[x,y]
   end
end

return, counts


end
