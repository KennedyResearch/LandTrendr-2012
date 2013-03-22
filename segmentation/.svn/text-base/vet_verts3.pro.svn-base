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

function vet_verts3, x, y, vertices, desired_count, distweightfactor


v=vertices
n_verts = n_elements(v)
n_to_remove = n_verts-desired_count

if n_to_remove lt 0 or n_verts le 3 then return, vertices


;if we don't have enough, just return what was given
;get initial slope ratios across all points

	n_t = n_verts-2

	slope_ratios = fltarr(n_t)

	;for angles, need to make it scaled in y like we see in display
	yr = range(y)
	yscale = ((y-min(y))/yr)  *range(x)	;make a square
    sc_yr = range(yscale)

	for i = 1, n_verts- 2 do $	;i is referenced off of the vertices array

		slope_ratios[i-1] = angle_diff(x[v[i-1:i+1]], $
								yscale[v[i-1:i+1]], sc_yr, $
								distweightfactor=distweightfactor)
;	plot, x, y, /ynoz
;	oplot, x[v], y[v], color = '00ffff'xl
;    print, 'end of full'
;



	m = max(slope_ratios)	;this one sticks and is used to mask out ones taken out

;now go through and iteratively remove them.

	usable = lindgen(n_verts)	;start with all possible usable, then take away
    count = n_verts		;keeps track how many good ones are left

	for i = 0, n_to_remove-1 do begin
		;pick from the slope diffs in play (not the ones at the end, which are
		;      shifted there from prior iterations)
;	   print, 'slope_diffs'
;	   print, slope_ratios[0:count-1-2]
;
	   worst = where(slope_ratios eq min(slope_ratios[0:count-1-2]), n) ;worst is in terms of triangle
	   																		; area array

	   worst = worst[0]
	   worst = worst + 1		;now increment so it's in terms of the vertex array
	;	print, 'worst is '+string(x[v[worst]])


	   usable[worst:n_verts-2]=usable[worst+1:n_verts-1]	;shift down to take out bad one
	   ;as long as we're not at the end of the triangle array, then shift down as well.  Note
	   ;   that the array index for worst is in terms of the vertex array, so the test
	   ;   of position is vs. n_t instead of n_t-1

	   if worst ne n_t then slope_ratios[worst-1:n_t-2]=slope_ratios[worst:n_t-1]	;same


	   ;recalculate the ones around the one taken out.
   		if worst ne 1 then slope_ratios[worst-2] = $		;if we're not at the beginning, need to recalculate the one to the left
   				angle_diff(x[ v[[usable[worst-2], usable[worst-1], usable[worst]]] ], $
   	   							yscale[v[[usable[worst-2], usable[worst-1], usable[worst]]] ], sc_yr)


   		if worst ne n_t then slope_ratios[worst-1] = $	;if we're not at the end, need to recalc the one to the right
				angle_diff(x[ v[[usable[worst-1], usable[worst], usable[worst+1]]] ], $
   	   							yscale[v[[usable[worst-1], usable[worst], usable[worst+1]]] ], sc_yr)

  ;print, 'after recalculating'

;
;		print, 'vertices after removing'
;		print, v[usable[0:count-2]]
;
;		print, 'slope diffs after removing'
;
;		print, slope_ratios[0:count-2-2]
;;
		count = count -1
   	end


;oplot, x[v[usable[0:count-1]]], y[v[usable[0:count-1]]], color = '00ff00'xl
;;
;stop

return, v[usable[0:count-1]]

end



