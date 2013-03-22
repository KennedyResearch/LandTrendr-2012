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

function find_correction2, vals
  n=n_elements(vals)
	out = vals
  diff_2 = shift(abs(vals-shift(vals,-2)),1)

  diff_minus1 = vals-shift(vals,-1)
  diff_plus1 = vals-shift(vals, 1)

  correction = vals-vals
  prop_correction = float(correction)

  correction[0] = 0
  correction[n-1]=0		;no corrections on th eend

  for i = 1,n-2 do begin
    md = max(abs([diff_minus1[i], diff_plus1[i]]))
    if md eq 0 then md =diff_2[i]	;set this to result in correction of 0
	  prop_correction[i] = (1.-(diff_2[i] / float(md)))

		;now determine if the correction centered on this "i" is greater than
		;  previous i's

		;first, calculate the correction for this one based on the
		;    mean difference between the two adjacent points
		;    Make sure that the direction of correction is opposite

		test = replicate(prop_correction[i] * 0.3 * (((vals[i-1]+vals[i+1])/2)-(vals[i])), 3) * $
						[-1,1,-1]

		;then compare with existing correction, make
		;  a 0,1 array to index

		resolve = [abs(test[0]) lt abs(correction[i-1]), $
						   abs(test[1]) lt abs(correction[i]), $
						   abs(test[2]) lt abs(correction[i+1])]

		;then use the index to assign either the new value or the existing one

	  correction[i-1] = ([test[0],correction[i-1]])[resolve[0]]
	  correction[i] = ([test[1],correction[i]])[resolve[1]]
	  correction[i+1] = ([test[2],correction[i+1]])[resolve[2]]


	end


return, {correction:correction, prop_correction:prop_correction}


end




function desawtooth2, vals, stopat=stopat
  v=vals
  ;plot, vals
  if n_elements(stopat) eq 0 then stopat = .5
  prop = 1.0
  maxcount = n_elements(vals)

  count = 1

  while (prop gt stopat) and (count lt maxcount) do begin
     c= find_correction2(v)
     prop = max(c.prop_correction)
		 v = v + c.correction
		 count = count + 1

  end


;print, count


return, v

end
