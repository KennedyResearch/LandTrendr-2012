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

function c_prob, array, min = min, max=max, vals = vals, $
				bin_count_min = bin_count_min, omin=omin, omax=omax


;Cumulative probability of an array.  Probability runs from 0 to 1.0
;Use _extra to pass commands to the histogram routine


if n_elements(min) eq 0 then minval = min(array) else minval = min
if n_elements(max) eq 0 then maxval = max(array) else maxval = max

;figure out bin size if necessary

if n_elements(bin_count_min) ne 0 then begin
   ;default is 1. 0

   bin_count = round(maxval - minval)
   if bin_count lt bin_count_min then $
   		binsize =  float(maxval-minval)/bin_count_min else $
   		binsize = 1

   artype = size(array, /type)
   if (artype le 2 or artype eq 12) and binsize lt 1 then array = float(array)	;do this to handle byte and integer arrarys.



   h = histogram(array, min=minval, max=maxval, locations=vals, binsize = binsize)



end else h = histogram(array, min=minval, max=maxval, locations=vals)

omin=minval
omax=maxval

c = cumulate(h)	;Lefsky's program


return,  ( c / float(max(c)))
end
