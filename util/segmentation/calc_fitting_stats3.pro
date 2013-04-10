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

function calc_fitting_stats3, obs, pred, n_predictors, resid=resid


;   a count for the number of predictor variables used
;   to get the predicted vals, return stuff
;  Assumes equal weights


observed = float(obs)
predicted = float(pred)

;first, take out the points in the record
;   where the observed and predicted are identical -- these
;   are nodes in the fitting that should not be allowed
;   to inflate the fit


exacts = where(abs(observed-predicted) lt 0.00001, n_exacts)		;change to machine precision some day

;observed = observed[goods]
;predicted = predicted[goods]

	points = n_elements(observed)
	if points ne n_elements(predicted) then begin
			print, "calc_fitting_stats:  observed and predicted have different number of elements"
			return, {ok:0}
			end


  b = reform(observed, points)
  yfit = reform(predicted, points)
  varnum = n_predictors
  w = fltarr(points)+1.0

	mean_y = total(b) / points
	ss = total(W * (B - mean_y)^2)		;sum of squares

	resid = b - yfit
	abs_diff = total(abs(resid))

	ss_resid = total(resid^2 * W)		;sum of squares of the residuals
  if ss_resid gt ss then ss_resid = ss ;to handle small rounding error when no trend
	ss_regr = ss- ss_resid					;sum of squares of the regression



	df_regr = varnum
	df_resid = points-varnum-1;-n_exacts		;take away the number of times we have exact matches at vertices


  if df_resid le 0 then out = $
  			 {  ok: 0, $
   					mean_y:mean_y, $
   					sum_of_squares: ss, $
   					sum_of_squares_resid: ss_resid, $
   					sum_of_squares_regression: ss_regr, $
   					df_regr: df_regr, $
   					df_resid:  df_resid, $
						residual_variance:  0, $
						total_variance:  0, $

						adjusted_rsquare:  0, $
						f_stat:  0, $
						p_of_f: 1.0, $
						yfit: yfit, $
						ms_regr:0, $				;added these two 6/14/06
						ms_resid:0, $
						aicc:0,$
						abs_diff:abs_diff}	else $

	begin


				residual_variance = ss_resid / df_resid
				total_variance = ss / (points-1)
				adjusted_rsquare = 1- (residual_variance / total_variance)	;terms from Jongman et al. pg 37


			  ms_regr = ss_regr / df_regr			;mean square error of regression
			     ms_resid = ss_resid / df_resid		;mean square error of resids

			     if ms_regr lt .00001 then f_regr=.00001 else $
			     	;because of glitch in f_test1, a zero mistakenly gets f score of 1, so need to override
			     			f_regr = ms_regr / ms_resid

			     p_of_f = 1-f_test1(f_regr, df_regr, df_resid)

			;calc the AIC
					AIC = (2*n_predictors) + (points * alog(ss_resid/points))
					AICc = AIC + ( (2*n_predictors*(n_predictors+1)) / (points-n_predictors-1) )
				if finite(aicc) eq 0 then aicc = -1



			    out = {  ok: 1, $
			   					mean_y:mean_y, $
			   					sum_of_squares: ss, $
			   					sum_of_squares_resid: ss_resid, $
			   					sum_of_squares_regression: ss_regr, $
			   					df_regr: df_regr, $
			   					df_resid:  df_resid, $
									residual_variance:  residual_variance, $
									total_variance:  total_variance, $

									adjusted_rsquare:  adjusted_rsquare, $
									f_stat:  f_regr, $
									p_of_f: p_of_f, $
									yfit: yfit, $
									ms_regr:ms_regr, $				;added these two 6/14/06
									ms_resid:ms_resid, $
									abs_diff:abs_diff, $	;added these two 6/14/06
									AICc:aicc}						;added july 29 2007
   end



return, out
end




