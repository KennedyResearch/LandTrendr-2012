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


;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;X
;X                Begin main module
;X
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


function apply_fitted_trajectory_v1, all_years, goods, vvals,  $
		  vertices, desawtooth_val


;February 28, 2008
;Given a set of vertices found with TBCD (passed here as "vertices"
;  apply the best fitting trajectory through the vvals (this is
;  presumably a different index or spectral value than the original.


;Years:  the year values that are to be used in the fitting.
; 		These should have the same number of elements as the vvals
;		array.  These should not include years that have clouds
;		or any other filtered values.
;VVals:  The values (spectral, biomass, etc.) for the Years.
;Allyears:  The years for the entire sequence of images, including
;		the years that have been masked out for the fitting.
;		This allows us to build a true fitted image even for
;		years where there's a cloud, etc.



;get the offset of years.  add back in at the end

  ;minimum_x_year = min(years)
  minimum_x_year = min(all_years)

  all_x = all_years - minimum_x_year
  fixed_v = vertices-minimum_x_year



   													  ;avoid little dribs and drabs

n_all_yrs = n_elements(all_years)		;how many years are there in the whole archive?
n_yrs = n_elements(vvals)   ;how many years are to be used in this fitting (i.e. minus clouds, etc.)


;Do prep work

;Take out spikes that start and end at same value (to get rid of weird years
;			left over after cloud filtering)

	if desawtooth_val lt 1.0 then $
		vals = desawtooth(vvals, stopat=desawtooth_val) else vals = vvals

;	if n_elements(max_segments) ne 0 then max_count = max_segments+1 else $
;			max_count = min([round(float(n_yrs)/2.5), 7])	;maximum number of vertices is 7

  ;  vals = vals * modifier  ;this sets everything so disturbance is always positive

max_count = n_elements(fixed_v)	;just need this to set segment_mse


;set up blank
  mean_vvals = mean(vvals)

  model = {   vertices:fixed_v, $
  				   vertvals:fixed_v-fixed_v, $
  				   yfit:fltarr(n_all_yrs)+mean_vvals, $
  				   n_segments:0, $
                  		;1 = autocorr, 2, = find_segments6, 3=findsegments 7
                   segment_mse:intarr(max_count-1)-1}


;in the special case where there were not enough to fit,
;   all of the vertices will be -1, and the fixed-vertices -1986.
;  Rather than trip up ftv_v1 with this problem, we just return
;   mean value and

   if vertices[0] eq -1 then begin
		model.vertices[*] = 0
		model.vertices = [0, max(all_x)] + minimum_x_year
		model.vertvals[0:1] = [mean_vvals, mean_vvals]
		model.n_segments = 1
		if n_elements(vvals) gt 2 then $
			model.segment_mse =  fix(stdev(vvals) < 32767) else model.segment_mse = fix(total(mean_vvals^2) < 32767)	;just square it to make it look bad.
        ;fyit is already set right
        return, {ok:1, model:model}
   end






;****************************
;Use the fixed vertices to find the best route
; through these vals


	fitted = ftv_v1(all_x, goods, vals,fixed_v)	;max_count is the number of segments + 1


;************************


;***********
;  Now assign the vert valsvals.
	 model = {   vertices:all_x[fitted.vertices[0:fitted.n_segments]]+minimum_x_year, $
  				   vertvals:fitted.vertvals[0:fitted.n_segments], $
  				   yfit:fitted.yfit, $
  				   n_segments:fitted.n_segments, $
             segment_mse:fitted.segment_mse}

;need to use the vertvals to do this.



return, {ok:1, model:model}
end




