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


function fit_trajectory_v2, all_years, goods, vvals,  $
			minneeded, background, $
			modifier, seed, $
			desawtooth_val, pval, $
			max_segments, recovery_threshold, $
			distweightfactor,  vertexcountovershoot, $
   			bestmodelproportion


;February 28, 2008
;given a set of years and observed values,
;   fit the trajectory using segmentation.
;  Use a simple segmentation approach where the segments
;      vertices are assigned in order from early to latest
;      year.
;  If that doesn't produce a reasonable fit at pval threshold,
;	   then try a segmentation that allows all vertex values
;	   to float independently (more timeconsuming)
;Ouputs:
;	 A structure with enough room for information on the
;		year and the value of 6 vertices (which is currently the
;		most that are reasonable in the landsat record).

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


   													  ;avoid little dribs and drabs

n_all_yrs = n_elements(all_years)		;how many years are there in the whole archive?
n_yrs = n_elements(vvals)   ;how many years are to be used in this fitting (i.e. minus clouds, etc.)

;check if a bunch of zeros -- this indicates off the edge
    zeroes = where(vvals eq background*modifier, n_zeroes)
    if n_zeroes gt (0.3*n_yrs) then return, {ok:0}





;Do prep work

;Take out spikes that start and end at same value (to get rid of weird years
;			left over after cloud filtering)

	if desawtooth_val lt 1.0 then $
		vals = desawtooth(vvals, stopat=desawtooth_val) else vals = vvals


	if n_elements(max_segments) ne 0 then max_count = max_segments+1 else $
			max_count = min([round(float(n_yrs)/2.5), 7])	;maximum number of vertices is 7

    vals = vals * modifier  ;this sets everything so disturbance is always positive

;set up blank

  best_model = {   vertices:intarr(max_count), $
  				   vertvals:intarr(max_count), $
  				   yfit:fltarr(n_all_yrs)+mean(vvals), $
  				   n_segments:0, $
                   p_of_f:1.0, $
                   f_stat:0., $
                   ms_regr:0., $
                   ms_resid:0., $		;1 = autocorr, 2, = find_segments6, 3=findsegments 7
                   segment_mse:intarr(max_count-1)-1}	;set to negative one as flag


  ;not enought data to run the fitting
  if n_elements(goods) lt minneeded then begin
    best_model.n_segments = 1
    return, {ok:1, best_model:best_model} 
  end

;****************************
;Do the trajectory fitting!  This uses one of two approaches
;   to identify the best fit of the


	best = tbcd_v2(all_x, goods, vals, max_count, seed, pval, $
				recovery_threshold, modifier, distweightfactor, $
				 vertexcountovershoot,   bestmodelproportion )	;max_count is the number of segments + 1

;************************


;***********
;  Now assign the vert valsvals.

   best_model.yfit = best.yfit * modifier
   best_model.f_stat = best.f_stat
   best_model.p_of_f = best.p_of_f
   best_model.ms_regr = best.ms_regr
   best_model.ms_resid = best.ms_resid
   ;best_model.source = best.source

   best_model.n_segments = best.n_segments
   best_model.vertices[0:best.n_segments] = all_x[best.vertices[0:best.n_segments]]+minimum_x_year
   best_model.vertvals[0:best.n_segments] = best.vertvals[0:best.n_segments]
   best_model.segment_mse[0:best.n_segments-1] = fix(best.segment_mse[0:best.n_segments-1] < 32767)



;need to use the vertvals to do this.



return, {ok:1, best_model:best_model}
end




