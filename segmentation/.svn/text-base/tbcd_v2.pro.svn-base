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


function fill_line, x, x_endpoints, y_endpoints, slope=slope
	;each is a 2-element array
	;  start and end of the interval
	;  this calculates the y-vals in between

  span = x_endpoints[1]-x_endpoints[0]
 ; xvals = lindgen(span+1)

 ;in cases where there are mulitiple image in a year
 minidx = where(x eq x_endpoints[0])
 maxidx = where(x eq x_endpoints[1])

  ;xvals = x[minidx[0]:maxidx[0]]
  xvals = x[minidx:maxidx]

  slope = float(y_endpoints[1]-y_endpoints[0])/span


;return the fitted y vals

  return, ((xvals-xvals[0]) * slope) + y_endpoints[0]
end

function fill_line2, x, x_endpoints, y_endpoints, slope=slope

;called by fill_verts, fill verts2, which are only in f7

;differs from the version 1 in find_segments6 in that
;  this one assumes the x_endpoints are x indices, not
;  the actual x values

	;each is a 2-element array
	;  start and end of the interval
	;  this calculates the y-vals in between

  span = x[x_endpoints[1]]-x[x_endpoints[0]]
 ; xvals = lindgen(span+1)
 ; xvals = x[where(x eq x_endpoints[0]):where(x eq x_endpoints[1])]
  xvals = x[x_endpoints[0]:x_endpoints[1]]


  slope = float(y_endpoints[1]-y_endpoints[0])/span


;return the fitted y vals

  return, ((xvals-xvals[0]) * slope) + y_endpoints[0]
end



function fill_verts2, X, P, vertices=vertices, n_yrs=n_yrs, num_params=num_params

;Only in 7
;called by find_best_trace3

;		num_params = n_elements(p) / 2
;		n_yrs = n_elements(x)
;
    ymod = fltarr(n_yrs)

    for i = 0, num_params-2 do $
    		ymod[vertices[i]:vertices[i+1]] = fill_line2(x, [vertices[i], vertices[i+1]], [p[i],p[i+1]])


 return, ymod
 end


function pick_better_fit, y, yfit1, yfit2, choice = choice

;this assumes same number of parameters to ahve created the yfits
   diff1 = total((y-yfit1)^2)
   diff2 = total((y-yfit2)^2)
   choice = diff1 gt diff2

   if choice then return, yfit2 else return, yfit1
end





function split_series, x, y, endsegment, firstsegment, disttest

;given an x and y  split the series into two
;   smaller segments
;However, invoke a rule where there can be no series where the value
;   decreases (implication of recovery) for only 1 or 2 years --
;   this will help with the overfitting, since this is not
;   really a prominent type of phenomenon, and if it's minor
;   anyway a coarser fit would make more sense.
;endsegment is a flag set to 1 if this segment is at the end
;  of the time period, when we don't invoke the
;  recovery restriction rule -- it will get thrown out
;  later if it's really extreme



   r = regress(x,y,yfit=yfit)
  diff = abs(yfit - y)
  n = n_elements(y)
  diff[0]=(diff[n-1]=0)  ;endpoints are already vertices, so take them out of consideration


 ;the first segment test was causing so many problems that I just took it out 1/2/09
 ;    if it causes a lot of false positives, then look at comparing the
 ;    slope of y[1]-y[0]/x[1]-x[0] vs. the slope of the regression (r) -- if the
 ;    regression is greater (in the opposite direction) than the this one,then
 ;    let it go.

 ; if (disttest) and (firstsegment eq 1) then diff[1]=diff[1]*(y[1] ge y[0])	;we accept vertices near edge if disturbance, but not if recovery
  if (disttest) and (endsegment eq 1) then diff[n-2]=diff[n-2]*(y[n-1] gt y[n-2])	;we know there are three elements at least, because
  																				; assured in calling program
;  diff[2]=diff[2]*(y[2] ge y[0])  ;same if separated by two occasions
;
;  if n gt 3 then  diff[n-3]=diff[n-3]*(y[n-1] gt y[n-3])	;same at other end
;


  maxdiff = where(diff eq max(diff), many)
  maxdiff=maxdiff[0]


  return, {delta: max(diff), vertex:maxdiff, ok:(maxdiff gt 0)}
   																						;if maxdiff is 0, then we
  																						; know there were no segments
  																						; that met the rule of no 1-yr recovery

end ;split series

function score_segments, x, y, vertices, vertex_count

   segment_scores = fltarr(vertex_count-1)  	;# segments always # vertices -1

   for i = 0, vertex_count- 2 do begin
      span = float(vertices[i+1]-vertices[i]+1)
      if span gt 2 then begin
          yvals = y[vertices[i]:vertices[i+1]]
          ;if we've done desawtooth, it's possible that all of the
          ;  values in a segment have same value, in which case regress
          ;  would choke, so deal with that.

          if range(yvals) gt 0 then r = regress(x[vertices[i]:vertices[i+1]], $
      						y[vertices[i]:vertices[i+1]], yfit=yfit) else yfit = yvals

          segment_scores[i] = total((y[vertices[i]:vertices[i+1]]-yfit)^2) / span

					;then check to make sure that we don't have 3 or 4 element with in
			end

   end



   return, segment_scores
 end




function find_vertices, x, y, max_count, distweightfactor

   n=n_elements(y)
   m = min([max_count, n-2])		;just in case

	 vertex_flags = bytarr(n)
	 vertex_flags[0] = (vertex_flags[n-1]=1)	;set ends to vertices

;grab a few with big changes in convolve value

   vertices = where(vertex_flags eq 1, vertex_count)
   count = 0

;plot, x, y

 	 while vertex_count lt m do begin 		;while vertex count less than max
      mses = score_segments(x, y, vertices, vertex_count)		;# segments always vertex_count - 1
     ok= 0
;oplot, x[vertices], y[vertices], color = 'ff00ff'xl

		 insidecount = 0
     ;print, mses

     while ok eq 0 do begin
         max_mse = max(mses)	;find the segment with the most residual variation
         if max_mse eq 0 then return, vertices		;bail if we can't get high enough without breaking recovery rule

        s = where(mses eq max_mse)
        s=s[0]
        endsegment = (s eq vertex_count-2)
        firstsegment = (s eq 0)



        v = split_series(x[vertices[s]:vertices[s+1]], $
      								 y[vertices[s]:vertices[s+1]], $
      								 endsegment, firstsegment, $
      								 (distweightfactor ne 0))	;just use distweightfactor to determine if disturbance should be considered in initial segments


				insidecount = insidecount+1


        ok = v.ok
        if ok ne 1 then mses[s] = 0		;reset
      end		;finding the segments that are legit in terms of recovery rule
;	print, 'insidecount'+string(insidecount)

      vertex_flags[v.vertex+vertices[s]] = 1 		;the vertex picked by split series, but modified by the
      																					;   count indicating the beginning of the piece
      																	 				;   of the series that was sent to split_series

      vertices = where(vertex_flags eq 1, vertex_count)
  ;print, verticesprint, vert

      count = count +1
   ;print, 'count'+string(count)
      if count gt 20 then return, vertices



   end


   return, vertices
end

function find_vertices2, x, y, max_count

   n=n_elements(y)
   m = min([max_count, n-2])		;just in case
   vertices = vet_verts3(x,y,lindgen(n),m)
return, vertices

end






;	 vertex_flags = bytarr(n)
;	 vertex_flags[0] = (vertex_flags[n-1]=1)	;set ends to vertices
;   if m lt 3 then return, where(vertex_flags eq 1)   ;if only 2 vertices are possible, they're the front and back ends
;
;   vertices = where(vertex_flags eq 1, vertex_count)
;
;   count = 0
;
; 	 while vertex_count lt m do begin 		;while vertex count less than max
;      mses = score_segments(x, y, vertices, vertex_count)		;# segments always vertex_count - 1
;     ok= 0
;;oplot, x[vertices], y[vertices], color = 'ff00ff'xl
;
;		 insidecount = 0
;
;     while ok eq 0 do begin
;         max_mse = max(mses)	;find the segment with the most residual variation
;         if max_mse eq 0 then return, vertices		;bail if we can't get high enough without breaking recovery rule
;
;        s = where(mses eq max_mse)
;        s=s[0]
;        endsegment = (s eq vertex_count-2)
;        firstsegment = (s eq 0)
;
;
;
;        v = split_series(x[vertices[s]:vertices[s+1]], $
;      								 y[vertices[s]:vertices[s+1]], $
;      								 endsegment, firstsegment)
;
;				insidecount = insidecount+1
;
;
;        ok = v.ok
;        if ok ne 1 then mses[s] = 0		;reset
;      end		;finding the segments that are legit in terms of recovery rule
;;	print, 'insidecount'+string(insidecount)
;
;      vertex_flags[v.vertex+vertices[s]] = 1 		;the vertex picked by split series, but modified by the
;      																					;   count indicating the beginning of the piece
;      																	 				;   of the series that was sent to split_series
;
;      vertices = where(vertex_flags eq 1, vertex_count)
;  ;print, verticesprint, vert
;
;      count = count +1
;   ;print, 'count'+string(count)
;      if count gt 20 then goto, past
;   end
;
;
;
;past:
;
;
;  ;convol test -- compare actual trajectory against convolved to find
;  ;  biggest changes from local trend.
;
;	ycon = convol(y, [0.333, 0.333, 0.333])
;	ycon[0] = y[0]
;	ycon[n-1] = y[n-1]
;	ydiff = abs(ycon-y)
;
;	ycon2 = convol(y, [0.2, 0.2, 0.2, 0.2, 0.2])
;	ycon2[0:1] = ycon[0:1]
;	ycon2[n-2:n-1] = ycon[n-2:n-1]
;	ydiff2 = abs(ycon2-y)
;
;sv = 1./7
;
;	ycon3 = convol(y, [sv, sv, sv, sv, sv, sv, sv])
;	ycon3[0:2] = ycon[0:2]
;	ycon3[n-3:n-1] = ycon2[n-3:n-1]
;	ydiff3 = abs(ycon3-y)
;
;;plot, y
;;oplot, ycon, color = '00ff00'xl
;;oplot, ycon2, color = 'ffff00'xl
;;oplot, ycon3, color = 'ff00ff'xl, thick = 2
;;
;;
;;stop
;
;  ;use ydiff2, sorted, to get the biggest vertices
;
;   ydiff3sort = reverse(sort(ydiff2))
;   vertex_flags[ydiff3sort[0:m-1]]=1	;take the first group vertices using convol test
;
;   vertices = where(vertex_flags eq 1, vertex_count)
;
;;   ;now cull out back to the desired number
;;   ;use new calc of m
;;;   stop
;;
;;   ;w,0
;;;
;;;	plot, x, y
;;;	oplot, x, ycon, color = '00ff00'xl
;;;	oplot, x, ycon2, color = '00ffff'xl
;;;
;;;	w,1
;;;	plot, x, ydiff, color = '00ff00'xl
;;;	oplot, x, ydiff2, color = '00ffff'xl
;;stop
;;
;;     mm = min([max_count, n-2])		;just in case
;;     fstats = fltarr(vertex_count)
;;     usable_verts = intarr(vertex_count)	;flag to keep track of good ones
;;     usable_verts[0] = (usable_verts[vertex_count-1] = 1)
;;
;;	 prior_start = 0
;;	 prior_end = 1
;;
;;  while prior_start lt vertex_count-1 do begin 		;as long as we haven't traversed whole thing, keep going
;;     fstats = fltarr(vertex_count)
;;
;;	 r1 = regress(x[vertices[prior_start]:vertices[prior_end]], $
;;	  				y[vertices[prior_start]:vertices[prior_end]], yfit=yfit)
;;	 ok = calc_fitting_stats3(y[vertices[prior_start]:vertices[prior_end]],$
;;    	 			 yfit, 1, resid=resid)
;;     prior_f = ok.f_stat
;;
;;
;;
;;	 for i = prior_end+1, vertex_count-1 do begin
;;
;;;	   r1 = regress(x[vertices[i-1]:vertices[i]], $
;;;	   				y[vertices[i-1]:vertices[i]], yfit=yfit)
;;;
;;;	   r2 = regress(x[vertices[i]:vertices[i+1]], $
;;;	   				y[vertices[i]:vertices[i+1]], yfit=yfit)
;;
;;		r1 = regress(x[vertices[prior_start]:vertices[i]], $
;;	   				y[vertices[prior_start]:vertices[i]], yfit=yfit)
;;
;;    	 ok = calc_fitting_stats3(y[vertices[prior_start]:vertices[i]],$
;;    	 			 yfit, 2, resid=resid)
;;    	 fstats[i] = ok.f_stat
;;     end
;;     ;find the best f_stat, call it the usable
;;
;;
;;		prior_start = where(fstats eq max(fstats))
;;
;;		usable_verts[prior_start]=1
;;	    prior_end =prior_start+1
;;	endwhile
;;
;;
;;	stop
;;
;;
;;
;;
;;
;;	   ;takeaway[i] = total(abs(yfit-y[vertices[i-1]:vertices[i+1]])/(mean(yfit))/(vertices[i+1]-vertices[i-1]))
;;	   ;takeaway[i]= abs(r1-r2)
;;	  ; takeaway[i] = ok.p_of_f		;the fit of the immediate segments without this vertex
;;
;;
;;    ; end
;;
;;stop
;;
;;	takeaway[0]=(takeaway[vertex_count-1]=min(takeaway))
;;    s = reverse(sort(takeaway))
;;
;
;
;ch = vet_verts2(x,y,lindgen(n),m)
;plot, x, y
;oplot, x[ch], y[ch], color = '00ffff'xl
;
;stop
;
;    vertices = vertices[s[0:mm-1]]	;take only the highest ones,
;    								;which are those whose taking away results
;    vertices = vertices[sort(vertices)]							; in biggest residual on the regression of that segment
;
;
;      return, vertices
;end


function find_best_trace, x, y, v, n_segments

		;for a given set of vertices (x-vals), find the
		;   the combo of vertex y-vals that results
		;   in best fit for each segment
		;x and y are the original values
		;  v is the list of vertices (in terms of array position, not the x-value
		;  n_segments is the number of segments -- passed just
		;     to save calc time
		;This is used only on the first run, with all of the
		;   segments.  From here on out, we just eliminate
		;   each one and calc the vals


    yfit_final = y		;set up array of fitted vals
	  vx = x[v]
    vv = y[v]				;initially, the vertex vals are the y-values at the x-vals
    									;these will be modified as we regress our way through
    									; the series

    slope = fltarr(n_segments)	;set up array to capture slopes


		;TODO:
    s= 0
    dotway = fill_line(x, vx[s:s+1], vv[s:s+1], slope=dot_slope)
		float_slope = regress(x[v[s]:v[s+1]], y[v[s]:v[s+1]], yfit=floatway)

	  yfit_better = pick_better_fit(y[v[s]:v[s+1]], dotway, floatway, choice=choice)
		vv[s]=yfit_better[0]
		vv[s+1]=yfit_better[v[s+1]]
		slope[s] = ([dot_slope, float_slope])[choice]
		yfit_final[v[s]:v[s+1]] 	= yfit_better

		for s = 1, n_segments-1 do begin
		  dotway = fill_line(x, vx[s:s+1], vv[s:s+1], slope=dot_slope)
			anch = anchored_regression(x[v[s]:v[s+1]], y[v[s]:v[s+1]], vv[s] )
		  anchway=anch.yfit
		  anch_slope=anch.slope
		  yfit_better = pick_better_fit(y[v[s]:v[s+1]], dotway, anchway, choice=choice)
			vv[s]=yfit_better[0]
			vv[s+1]=yfit_better[v[s+1]-v[s]]
			slope[s] = ([dot_slope, anch_slope])[choice]
			yfit_final[v[s]:v[s+1]] 	= yfit_better
    end

    return, {yfit:yfit_final, slopes:slope, vertvals:vv}

end


function find_best_trace3, x, y, v, n_vertices

		;for a given set of vertices (x-vals), find the
		;   the combo of vertex y-vals that results
		;   in best fit for each segment
		;x and y are the original values
		;  v is the list of vertices (in terms of array position, not the x-value
		;  n_segments is the number of segments -- passed just
		;     to save calc time

	;use mpfitfun to get this right.


		n_yrs = n_elements(x)
		num_params = n_vertices


   if num_params eq 0 then stop

;   	base_info = {fixed:0, limited:[0,0], limits:[0., 0.]}
;		parinfo = replicate(base_info, num_params)
    m=moment(y, sdev=sdev)
		if sdev eq 0 then sdev = 1		;if it's a bunch of zeros, just set error to 1 so it'll pass through


		vals_sterr = replicate(sdev, n_yrs )

		p_in = fltarr(num_params)+m[0]

		functargs = {n_yrs: n_elements(x), num_params:num_params, vertices:v}
		vertvals = mpfitfun('fill_verts2', x, y, vals_sterr, p_in, $
    							errmsg=e, functargs=functargs, /quiet)


    yfit= fill_verts2(x, vertvals, num_params=num_params, vertices=v, n_yrs=n_yrs)


		;calc the slopes
		spans = shift(v,-1)-v

		ydiff = shift(vertvals,-1) - vertvals

		return, {yfit:yfit, slopes:ydiff[0:n_vertices-2]/spans[0:n_vertices-2], vertvals:vertvals}
end

function fill_from_vertices, x, v, vv, n_segments, n_obs

    yfit = fltarr(n_obs)	;set up
    slopes = fltarr(n_segments)

    for i = 0, n_segments-1 do begin



     k = fill_line(x, [x[v[i]], x[v[i+1]]], [vv[i], vv[i+1]], slope=slope)

     yfit[v[i]:v[i+1]] = k
     slopes[i] = slope
    end
  return, {yfit:yfit, slopes:slopes}
end

function take_out_weakest, x, y, v, vertvals, n_vertices, n_obs
   use = bytarr(n_vertices)+1
   mse = fltarr(n_vertices)

   for i = 1, n_vertices-2 do begin 			;for each vertex
   																				;look at mse for region defined by prior and subsequent vertex
   																				;  the one with least is the least important

     yfit = fill_line( x, [x[v[i-1]], x[v[i+1]]], [vertvals[i-1], vertvals[i+1]])
     mse[i] = total((yfit-y[v[i-1]:v[i+1]])^2) / float(x[v[i+1]]-x[v[i-1]])

  end

   weakest = where(mse[1:n_vertices-2] eq min(mse[1:n_vertices-2]))+1

   use[weakest[0]] = 0
   these = where(use eq 1)

   ok = fill_from_vertices(x, v[these], vertvals[these], n_vertices-2, n_obs)




return, {v:v[these], vertvals:vertvals[these], $
					yfit:ok.yfit, slopes:ok.slopes}


end
function take_out_weakest2, info, threshold, x, y, v, vertvals, n_vertices, n_obs

	use = bytarr(n_vertices)+1

	;first, check to see if there are any segments that
	;break the slope criterion.  If so, take them out first.


	ok = 1

	n_slopes = info.n_segments

	;we operate under the knowledge that
	;  disturbance is always considered to have a positive
	;  slope, and recovery a negative slope (based on band5 type indicators).

		 negatives = where(info.slope lt 0 and info.slope ne -1, n_negatives)

	   	 range_of_vals = range(info.yfit[0:info.n_obs-1])
		run_mse = 1

		if n_negatives gt 0 then $
				scaled_slope = abs( info.slope[negatives]) / range_of_vals else $
				scaled_slope = threshold-1		;set so it won't be gt threshold



		 if max(scaled_slope) gt threshold then begin 	;if we have one that violates, take it out

		    	violator = negatives[where(scaled_slope eq max(scaled_slope), n_scaledslopes)]
				violator = violator[0]

			;the violator is a segment -- which vertex to remove?   Since we
			;   are tracking through time, we assume that it is the latter
			;   vertex that is causing the problem and take it out. This will
			;   be violated only if there are spikes in brightness that are
			;   not fixed by desawtooth, but that situation would be no better
			;   removing the first vertex anyway, so we stick with this approach
			;   since it will take out more shadow problems. the only



			;now interpolate to get rid of this point, so it doesn't mess
			;   up the fits later
			if violator+1 eq n_vertices-1 then begin
				y[v[violator+1]] = y[v[violator+1]-1]
				vertvals[n_vertices-1] = y[v[violator+1]]

				run_mse = 1		;since the violating point was at end, need to run mse instead after fixing
			end else begin	;just set eq to prior
			    use[violator+1]=0
				lefty=y[v[violator+1]-1]
				righty=y[v[violator+1]+1]
				leftx=x[v[violator+1]-1]
				rightx=x[v[violator+1]+1]
				thisx = x[v[violator+1]]
				slope = float(righty-lefty)/(rightx-leftx)
				interpy = ((thisx-leftx)*slope)+lefty
				y[v[violator+1]] = interpy
				run_mse = 0
			end



		end
		if run_mse then begin	;take out the vertex whose elimination results in the least penalty


			mse = fltarr(n_vertices)

			for i = 1, n_vertices-2 do begin 			;for each vertex
																						;look at mse for region defined by prior and subsequent vertex
																						;  the one with least is the least important

			 yfit = fill_line( x, [x[v[i-1]], x[v[i+1]]], [vertvals[i-1], vertvals[i+1]])
			 mse[i] = total((yfit-y[v[i-1]:v[i+1]])^2) / float(x[v[i+1]]-x[v[i-1]])

			end

			weakest = where(mse[1:n_vertices-2] eq min(mse[1:n_vertices-2]))+1

			use[weakest[0]] = 0

        end

   these = where(use eq 1)

   ok = fill_from_vertices(x, v[these], vertvals[these], n_vertices-2, n_obs)



return, {v:v[these], vertvals:vertvals[these], $
					yfit:ok.yfit, slopes:ok.slopes}


end


function pick_best_model6, info, pval, bestmodelproportion, use_fstat=use_fstat

        if N_ELEMENTS(use_fstat) eq 0 then use_fstat = 0	;rek changed to 0 10/17/09

				  ;now pick the best one
				   max_non_zero_segment = max(where(info.n_segments ne 0, n_nonzero))
				   if n_nonzero eq 0 then stop


				;   n_tests = n_elements(info)
				;   right =info.f_stat-shift(info.f_stat, -1)
				;   right[max_non_zero_segment] = 0.0001	;just set to positive so it will stay in consideration
				;
				; 	 left = info.f_stat-shift(info.f_stat, 1)
				;	 left[0] = 0.0001	;just set to positive so it will stay in consideration
				;
				;  ;first, look at places where a model is a local max in terms
				;  ;    of the f_stat -- better than both the one below and the one
				;  ;    above
				;
				;	 potential = where((left gt 0) * (right gt 0) eq 1, n)
				;
				;     ;pick the simplest example from those
				;     if n eq 0 then best_relative = n_tests-1	else best_relative = max(potential) ;take the simplest version that meet criteria
				;
				;	 ;then separately, look at places where the jump from
				;	 ;   simpler to more complex is big and much bigger
				;	 ;   than move up to the next more complex
				;
				;
				;   score = left + right
				;   best_jump = where(score eq max(score), n_best)
				;   best_jump = best_jump[n_best-1]		;pick the simplest one, in case two tie
				;
				;   best = max([best_jump, best_relative])	;then find the simplest example from those two
				;   ; i seem to miss disturbances this way.
				;
				;  colors = [ 'ff9922'xl, '99ff22'xl, '2299ff'xl, 'aaaaff'xl, 'ffaaff'xl, 'ddaadd'xl, '22dddd'xl]

				;
				;
				;;
				;  best = best_jump[0]

;try just doing the max:

;  z = info.aicc
;
;  notzeros = where(info.aicc ne 0, ngoods)
;  if ngoods ne 0 then $
;  		minval = min(info[notzeros].aicc)  else minval = 0
;


;	best_overall = where(info.aicc eq minval)
;   best = best_overall[0]

  if use_fstat eq 0 then begin
    mx = min(info.p_of_f)
    best_overall = where(info.p_of_f le (2-bestmodelproportion)*mx)
    best = best_overall[0]
    return, best
  endif else begin
    pass_the_test = where(info.p_of_f le pval, npass)
    if npass eq 0 then return, -1

    ;best = -1 else best = min(pass_the_test)		;pick the lowest one, which is
    							;the one with the most vertices since we go in reverse order

    ;check to see whether any with p < 0.001
    ; pass_the_stringent = where(info.p_of_f le 0.001, npass)
    ; if npass ne 0 then return, pass_the_stringent[0]	;pick the lowest one, which means most detailed

     ;otherwise, find the best f-stat

     mx = max(info.f_stat)


     best_overall = where(info.f_stat ge bestmodelproportion * mx)

     best = best_overall[0]		;take the most descriptive one if a tie
     return, best
  end
end

function check_slopes, info, threshold

	;given one model, look at its slopes
	;filter out if recovery happens quicker than quickest disturbance --
	;  a value-free way to get unreasonable things out.
	; but of course don't do if all we have is recovery.  no way to tell for sure then.


		ok = 1

		n_slopes = info.n_segments

	;all of these operate under the knowledge that
	;  disturbance is always considered to have a positive
	;  slope, and recovery a negative slope (based on band5 type indicators).
	;  Always make sure that he adjustment factor that happens
	;   upstream of find_segments6 ensures that recovery is negative

		 negatives = where(info.slope lt 0, n_negatives)
		 range_of_vals = range(info.yfit[0:info.n_obs-1])


		if n_negatives gt 0 then begin
			 scaled_slope = abs( info.slope[negatives]) / range_of_vals
			if max(scaled_slope) gt threshold then return, 0
		end


return, ok

end



;*******************************************************************
;
;MAIN ROUTINE:   TBCD_V1
;
;*******************************************************************


function tbcd_v2, all_x, goods, y, max_count, seed, pval, $
		recovery_threshold, modifier, distweightfactor, $
		 vertexcountovershoot, bestmodelproportion


;February 28, 2007. REK.
;Derived from find_segments6 and 7.

  x = all_x[goods]
 n_obs = n_elements(x)

;*********************************
;FIND ALL POTENTIAL VERTICES FIRST
;
	;given a series of vals y, find the logical segments of
	;  straight lines.  Use the actual values of the
	;  curve for the fits

;stop

	 v1 = find_vertices(x, y, max_count+vertexcountovershoot, distweightfactor) ;MAKE INTO A PARAMETER
	 v = vet_verts3(x,y, v1, max_count, distweightfactor)

	 ;stop


	 orig_v = v		;keep 'em in case need to try other way
	 orig_n_segments = n_elements(orig_v)-1		;use this at the end to constrain to the right number


	;first, find the trace through the x-vals of the vertices
	;   that minimizes the squared difference between the
	;   fit and the observed values for each segment separately.
	;   this can be done using either a floating regression
	;   or a dot-to-dot regression

	  n_vertices = n_elements(v)
	  n_segments = n_vertices-1



	;catch the case where all values are same -- happens
	;  if the mask isn't quite right and the background value
	;  is set wrong.


;	  if n_vertices eq 2 then return, {f_stat:0., p_of_f:1., ms_regr:0., ms_resid:0., $
;		  					vertices:v-v, vertvals:v-v, yfit:y-y, $
;		  					slope:fltarr(n_segments), n_segments:1, $
;		  					aicc:0., f_stat_random:0., p_of_f_random:0.}

    ;match model run output
 ;   if n_vertices eq 2 then return, {f_stat:0., p_of_f:1., ms_regr:0., ms_resid:0., $
 ;             vertices:v-v, vertvals:new_vertvals-new_vertvals, yfit:all_x-all_x, $
 ;             slope:fltarr(n_segments), n_segments:1, $
 ;             aicc:0., n_obs:n_obs, segment_mse:fltarr(n_segments)}

    if n_vertices eq 2 then return, {f_stat:0., p_of_f:1., ms_regr:0., ms_resid:0., $
              vertices:v-v, vertvals:v-v, yfit:all_x-all_x, $
              slope:fltarr(n_segments), n_segments:1, $
              aicc:0., n_obs:n_obs, segment_mse:fltarr(n_segments)}

;********************************
;FIND BEST TRACE WITH ALL THOSE VERTICES
;
;The best trace is the set of y-values that best
;    works with those vertices.  This approach
;    uses find_best_trace, which uses a simple
;    and relatively quick way to do this.  Starting
;    at the left of the sequence, it allows segments
;    to either track the two vertices directly,
;    or to use a simple linear least-squares value
;    anchored on the left vertex to connect to the
;    next vertex.  This forces the sequence in order
;    from left to right (oldest to newest).

  best_fit = find_best_trace(x, y, v, n_segments)
  new_vertvals = best_fit.vertvals




;********************************
;THEN SEQUENTIALLY REMOVE VERTICES
;
;now go through each vertex, take it out, and
; see what the fit is.

  ;SET UP
    blanks = intarr(max_count)
	base =  {f_stat:0., p_of_f:1., ms_regr:0., ms_resid:0., $
	  					vertices:blanks, vertvals:blanks, yfit:all_x-all_x, $	;changed yfit 2/29/08 so all years involved
	  					slope:fltarr(max_count-1), n_segments:1, $
	  					aicc:0., n_obs:n_obs, segment_mse:fltarr(max_count-1)}		;added segment_mse 12/27/08

	if (n_vertices eq 2) then return, base  ;the special case where all values are same (a problem with the mask, but don't hang up if it happens)

	;info = replicate(base, n_vertices-2)
	info = replicate(base, n_vertices-1)


  ;GO THROUGH IT
  ;Each time you take on out, you use the same
  ;   "find_best_trace" approach that uses
  ;   the anchored regression stuff.


	;for the first one, use all vertices

	i = 0
		ok = calc_fitting_stats3(y, best_fit.yfit, ((n_vertices-i)*2)-2, resid=resid)

		info[i].f_stat = ok.f_stat
		info[i].p_of_f = ok.p_of_f
		info[i].ms_regr = ok.ms_regr
		info[i].ms_resid = ok.ms_resid
		info[i].vertices[0:n_vertices-i-1] = v		;vertices are the actual index in the array, not the year
		info[i].vertvals[0:n_vertices-i-1] = best_fit.vertvals
		info[i].yfit[0:n_obs-1] = best_fit.yfit	;added [0:n_obs-1] 2/29 to allow for space in
		info[i].slope[0:n_vertices-i-2] = best_fit.slopes
		info[i].n_segments = n_vertices-i-1
		info[i].aicc = ok.aicc





	;for i = 1, n_vertices-2 do begin 		;n_vertices-2 because always need the first and last
	for i = 1, n_vertices-2 do begin
		;take out all vertices, check them out, pick the
		;   one that results best overall fit

		rr = take_out_weakest2(info[i-1], recovery_threshold, x, y, v, new_vertvals, n_vertices-(i-1), n_obs)

		new_vertinfo = find_best_trace(x,y,rr.v, n_segments-i)

		ok = calc_fitting_stats3(y, new_vertinfo.yfit, ((n_vertices-i)*2)-2, resid=resid)

		info[i].f_stat = ok.f_stat
		info[i].p_of_f = ok.p_of_f
		info[i].ms_regr = ok.ms_regr
		info[i].ms_resid = ok.ms_resid
		info[i].vertices[0:n_vertices-i-1] = rr.v		;vertices are the actual index in the array, not the year
		info[i].vertvals[0:n_vertices-i-1] = new_vertinfo.vertvals
		info[i].yfit[0:n_obs-1] = new_vertinfo.yfit	;added [0:n_obs-1] 2/29 to allow for space in
		info[i].slope[0:n_vertices-i-2] = new_vertinfo.slopes
		info[i].n_segments = n_vertices-i-1
		info[i].aicc = ok.aicc


        v = rr.v
        new_vertvals = new_vertinfo.vertvals

	end


;************************************
;PICK THE BEST ONE
;
;Along the way, though, we check to make
;   sure that the best one doesn't violate
;   the slopes  rule (where recovery can't
;   be faster than a prescribed speed)
;Note:  This section requires that recovery
;   always result in a decrease in the y-values
;   If the y-values are of a type where recovery
;   actually results in an increase (e.g. biomass),
;   you need to set the modifier value to -1 before
;   running tbcd, so this recovery criterion will
;   be applied appropriately.


   notdone = 1
   fstats = info.f_stat
   increment = 0

	 while notdone do begin
	    increment = increment+1
	    best = pick_best_model6(info, pval, bestmodelproportion)

			if best ne -1 then begin
		   ;check on the slopes


		  	ok = check_slopes(info[best], recovery_threshold)
		  	if ok eq 0 then info[best].p_of_f = 1
			  info[best].f_stat = info[best].f_stat * ok
				info[best].aicc = info[best].aicc * ok


			  notdone = 1-ok- (increment gt n_vertices)	;once ok = 1,
			  				;or if we've gone through this as many times as there are vertices, we move on
		  end else begin
				;if we get here, it means none of the
				;  aicc's were valid and/or that
				;  none of the segments worked.

				best = where(fstats eq min(fstats))
				best=best[0]
				notdone = 0
		end


	end



;*******************************
;IF NO GOOD FIT FOUND, TRY THE MPFITFUN APPROACH


  if info[best].p_of_f gt pval then begin
  ;	print, 'using f7 on '+string(i)

	    v=orig_v	;restore the vertices

		 n_vertices = n_elements(v)
	     n_segments = n_vertices-1


		;*********
		;Find the best fit using the marquardt approach (F7)


		  best_fit = find_best_trace3(x, y, v, n_vertices)	;NOTE THAT N_VERTICES IS DIFFERENT THAN CALL TO FIND_BEST_TRACE (THE OLDER VERSION)
		  new_vertvals = best_fit.vertvals


		;********
		;F7: Set up for marquardt approach

		    info = replicate(base, n_vertices-1)

		;set up the first one with max number of vertices
			i = 0
			ok = calc_fitting_stats3(y, best_fit.yfit, ((n_vertices-i)*2)-2, resid=resid)

			info[i].f_stat = ok.f_stat
			info[i].p_of_f = ok.p_of_f
			info[i].ms_regr = ok.ms_regr
			info[i].ms_resid = ok.ms_resid
			info[i].vertices[0:n_vertices-i-1] = v		;vertices are the actual index in the array, not the year
			info[i].vertvals[0:n_vertices-i-1] = best_fit.vertvals
			info[i].yfit[0:n_obs-1] = best_fit.yfit	;added [0:n_obs-1] 2/29 to allow for space in
			info[i].slope[0:n_vertices-i-2] = best_fit.slopes
			info[i].n_segments = n_vertices-i-1
			info[i].aicc = ok.aicc




		  ;********
		  ;F7: TAKE OUT WORST ONE ITERATIVELY

		;	for i = 1, n_vertices-2 do begin 		;n_vertices-2 because always need the first and last

			for i = 1, n_vertices-2 do begin

				;take out all vertices, check them out, pick the
				;   one that results best overall fit

				rr = take_out_weakest(x, y, v, new_vertvals, n_vertices-(i-1), n_obs)

				new_vertinfo = find_best_trace3(x,y,rr.v, n_vertices-i)


				ok = calc_fitting_stats3(y, new_vertinfo.yfit, ((n_vertices-i)*2)-2, resid=resid)

				info[i].f_stat = ok.f_stat
				info[i].p_of_f = ok.p_of_f
				info[i].ms_regr = ok.ms_regr
				info[i].ms_resid = ok.ms_resid
				info[i].vertices[0:n_vertices-i-1] = rr.v		;vertices are the actual index in the array, not the year
				info[i].vertvals[0:n_vertices-i-1] = new_vertinfo.vertvals
				info[i].yfit[0:n_obs-1] = new_vertinfo.yfit
				info[i].slope[0:n_vertices-i-2] = new_vertinfo.slopes
				info[i].n_segments = n_vertices-i-1
				info[i].aicc = ok.aicc

				v = rr.v
		        new_vertvals = new_vertinfo.vertvals

			end


		  ;*********************
		  ;F7:  Pick the best one
				notdone = 1
				increment = 0
				fstats = info.f_stat


				 while notdone do begin
			    best = pick_best_model6(info, pval, bestmodelproportion)
					 increment = increment + 1

					   ;check on the slopes
		      if best ne -1 then begin
				  	ok = check_slopes(info[best], recovery_threshold)
					  if ok eq 0 then info[best].p_of_f = 1
					  info[best].f_stat = info[best].f_stat * ok
						info[best].aicc = info[best].aicc*ok

					  notdone = 1-ok-(increment gt n_vertices)		;once ok = 1, we move on
					end else begin
					  			;if we get here, it means none of the
							;  aicc's were valid and/or that
							;  none of the segments worked, for
							;  either this approach or the prior one
							;  In this case, just reset things and use
							;  a single segment.
						best = where(info.n_segments eq 1)
						info[best].p_of_f = 1.0
						info[best].slope = 0
						info[best].vertvals[0:1] = mean(y)



					   notdone=0
					end


				end
end ; doing F7 if F6 didn't work

;**************************************************************
;Calculate the MSE of each segment, for use in masking ag later


  info[best].segment_mse =  score_segments(x,y,info[best].vertices, info[best].n_segments+1)

;
;w, 0
;   colors = [ 'ff9922'xl, '99ff22'xl, '2299ff'xl, 'aaaaff'xl, 'ffaaff'xl, 'ddaadd'xl, '22dddd'xl]
;   startyear = 1984
;   plot, x+startyear, y, /ynoz
;   for i = 0, n_elements(info)-1 do begin & oplot, x+startyear, info[i].yfit, thick = 2, color = colors[i]  &  end
;
;   oplot, x+startyear, info[best].yfit, color = '0000ff'xl, thick = 2
;   oplot, x+startyear, info[best].yfit, color = '0000ff'xl, psym = 4
;
;stop
;;
;;;
;
;print, 'vertices
;print, info[best].vertices

;************************
;First, fill in the vertices. right now, the vertex values in the info structure are based
;  on the x values passed to the routine (the ones that have been
;  filtered), so we need to extend in case those first or last years are
;  missing for this pixel.




	;first the middles


	info[best].vertices[0:info[best].n_segments] = goods[info[best].vertices[0:info[best].n_segments]]


	;front end
	if all_x[0] ne x[0] then begin
		;originally, I had extended the segment forward. But this causes
		;  problems for disturbances that occur in year 3 (where year 1 has a cloud, year 2
		;   is pre-disturbance, and year 3 is the disturbance).  In those cases
		;   the disturbance is propagated back, doubling the intensity and shifting
		;    the year back one.  Therefore, the safer thing to do is to just
		;    add a new vertex in front, make it the same y-value.  For the
		;    long disturbance or recovery situations, this shouldn't add too
		;    much error, and it seems likely that they will get collapseed later
		;The one hitch is if there are already 6 segments, so we have to
		;   handle that

	    new_n_segments = info[best].n_segments+1	;we're going to increment by 1
		new_n_vertices = new_n_segments+1

	    new_vertices = lonarr(new_n_vertices)
	   	new_vertices[1:new_n_vertices-1] = info[best].vertices[0:info[best].n_segments]
		new_vertices[0] = 0


	    new_vertvals = fltarr(new_n_vertices)
	    new_vertvals[1:new_n_vertices-1] = info[best].vertvals[0:info[best].n_segments]
	 	new_vertvals[0] = new_vertvals[1]	;just copy the same y-val


	    new_slope = fltarr(new_n_segments)
	    new_slope[0] = 	0   ;it's flat!
	 	new_slope[1:new_n_segments-1] = info[best].slope[0:info[best].n_segments-1] ;THIS COULD CAUSE POTENTIAL ERROR IN THE SLOPE!!!!

    	new_segment_mse = [0, info[best].segment_mse[0:info[best].n_segments-1]]

	;check for number of segments, and take out if too many

		;if new_n_segments gt orig_n_segments then begin
		if new_n_segments gt max_count-1 then begin
		 	;need to find out which one is least problematic to take out
			slope_ratios = fltarr(new_n_vertices-2)
			sc_yr = range(new_vertvals)

			for i = 1, new_n_vertices-2 do $
				slope_ratios[i-1]=angle_diff(new_vertices[i-1:i+1], $
									new_vertvals[i-1:i+1], sc_yr, distweightfactor=2)
			;find the vertex that has the least "bend"
			minv = where (slope_ratios eq min(slope_ratios), nminv)
			if nminv gt 1 then minv=minv[0]
			minv = minv+1		;because the zeroth is not in slope_ratios

			;take out the worst one
			flag = intarr(new_n_vertices)
			flag[minv] = 1
			use = where(flag eq 0)
			new_vertvals = new_vertvals[use]
			new_vertices = new_vertices[use]

      mseflag = intarr(new_n_vertices-1)
      mseflag[minv] = 1
      mseuse = where(mseflag eq 0)

      new_segment_mse = new_segment_mse[mseuse]


			;calculate slope
			dy = shift(new_vertvals, -1)-new_vertvals
			dx = shift(new_vertices, -1)-new_vertices
			new_slope = (dy/dx)[0:new_n_segments-2]

			new_n_vertices=new_n_vertices-1
			new_n_segments=new_n_segments-1
	  end


   	 ;at the end, re-assign the values to the info structure

;    info[best] = {f_stat: info[best].f_stat, p_of_f: info[best].p_of_f, $
;                  ms_regr: info[best].ms_regr, $
;                  vertices:new_vertices, vertvals:new_vertvals, $
;                  yfit:all_x-all_x, slope:new_slope, n_segments:new_n_segments, $
;                  aicc: info[best].aicc, n_obs: info[best].n_obs, $
;                  segment_mse:new_segment_mse}

		info[best].n_segments = new_n_segments
		info[best].vertices[0:new_n_vertices-1] = new_vertices
		info[best].vertvals[0:new_n_vertices-1] = new_vertvals
		info[best].slope[0:new_n_segments-1]= new_slope
		info[best].segment_mse[0:new_n_segments-1] = new_segment_mse




;;OBSOLETE code
;
;	   ;first, handle the X position of the vertices.  The vertices
;	   ;  are in index values, not years, so we have to figure out
;	   ;  the index of the first one and then add that
;;
;;		index_offset = where(all_x eq x[0], n_index_offset)
;;		if n_index_offset eq 0 then message, 'index mismatch error.'
;;
;;		not_zeros = where(info[best].vertices ne 0)	;we know there's at least 1
;;
;;		info[best].vertices[not_zeros] = info[best].vertices[not_zeros] + index_offset[0] ;shift all of the vertices up
;;
;		info[best].vertices[0] = 0	;set to the start
;
;		;now all of the vertices are relative to the correct year
;
;
;	   ;then handle the y offset
;	   ; First, need to see what the number of years are between
;	   ;   the beginning of all x and x, and use that to extend the slope
;	   ;   of the first segment
;
;	   	extendlength = x[0]-all_x[0]
;		dy = extendlength * info[best].slope[0] * (-1) 	;-1 because we're going backwards
;		info[best].vertvals[0] = info[best].vertvals[0] + dy

	end		;confirmed 2/29/08 that this works.


	;now do the other end
	n_all_x = n_elements(all_x)		;n_obs is the number of filtered x's


  if all_x[n_all_x-1] ne x[n_obs-1] then begin  ;if the last year is not the same

    new_n_segments = info[best].n_segments + 1
    new_n_vertices = new_n_segments + 1

    new_vertices = [info[best].vertices[0:info[best].n_segments], n_all_x-1]
    new_vertvals = [info[best].vertvals[0:info[best].n_segments], info[best].vertvals[info[best].n_segments]]

    new_slope = [info[best].slope[0:info[best].n_segments-1],0]
    new_segment_mse = [info[best].segment_mse[0:info[best].n_segments-1], 0]

    ;Assuming on
    if new_n_segments gt max_count-1 then begin

      slope_ratios = fltarr(new_n_vertices-2)
      sc_yr = range(new_vertvals)

      for i=1, new_n_vertices-2 do begin
        slope_ratios[i-1] = angle_diff(new_vertices[i-1:i+1], new_vertvals[i-1:i+1], sc_yr, distweightfactor=2)
      end

      minv = where (slope_ratios eq min(slope_ratios), nminv)
      if nminv gt 1 then minv=minv[0]
      minv = minv+1   ;because the zeroth is not in slope_ratios

      ;take out the worst one
      flag = intarr(new_n_vertices)
      flag[minv] = 1
      use = where(flag eq 0)
      new_vertvals = new_vertvals[use]
      new_vertices = new_vertices[use]

      mseflag = intarr(new_n_vertices-1)
      mseflag[minv] = 1
      mseuse = where(mseflag eq 0)

      new_segment_mse = new_segment_mse[mseuse]
      ;calculate slope
      dy = shift(new_vertvals, -1)-new_vertvals
      dx = shift(new_vertices, -1)-new_vertices
      new_slope = (dy/dx)[0:new_n_segments-2]

      new_n_vertices=new_n_vertices-1
      new_n_segments=new_n_segments-1
    end


    info[best].n_segments = new_n_segments
    info[best].vertices[0:new_n_vertices-1] = new_vertices
    info[best].vertvals[0:new_n_vertices-1] = new_vertvals
    info[best].slope[0:new_n_segments-1]= new_slope
	info[best].segment_mse[0:new_n_segments-1] = new_segment_mse

;    info[best] =  {vertices:new_vertices, vertvals:new_vertvals, $
;      yfit:all_x-all_x, $ ;changed yfit 2/29/08 so all years involved
;      slope:new_slope, n_segments:new_n_segments, $
;      segment_mse:new_segment_mse}

  end

;	if all_x[n_all_x-1] ne x[n_obs-1] then begin 	;if the last year is not the same
;
;	   ;first, handle the X position of the vertices.  The vertices
;	   ;  are in index values, not years.   so the last vertex should
;	   ;  simply point to the last element in the all_x array.
;	   ;  However, in the special case where the year just before the last
;	   ;  year is the one that has a vertex (i.e. 0, 19, 20 if 20 is the last year),
;	   ;  then we actually need to make a new vertex because the last one
;	   ;  serves dual duty as the end of the sequence and as a catch of a
;	   ;  real disturbance
;
;		if info[best].vertices[(info[best].n_segments)-1] eq (n_obs-2) and $
;			(info[best].n_segments ne max_count-1) then begin
;			;update everything to handel new stuff
;			info[best].n_segments = info[best].n_segments+1	;add to the count.  this will force the add on of the vals below
;			info[best].slope[info[best].n_segments-1] = 0	;make last segment flat, by default
;
;		end
;
;		info[best].vertices[info[best].n_segments] = n_all_x-1
;
;
;
;	   ;then handle the y offset
;	   ; First, need to see what the number of years are between
;	   ;   the beginning of all x and x, and use that to extend the slope
;	   ;   of the first segment
;
;	   	extendlength = all_x[n_all_x-1]-x[n_obs-1]
;		dy = extendlength * info[best].slope[info[best].n_segments-1]
;		info[best].vertvals[info[best].n_segments] = $
;				info[best].vertvals[info[best].n_segments] + dy
;
;	end		;did not have a test available to confirm this works!


;***********************
;Now fill in the yfit values for the "allyears" range, to get a
;  true yfit even for years that were missing from this pixel


   ;should be able to use "fill_from_vertices"
   ;   need to pass the allyears, but that needs to be in
   ;    the same year units as the x vals, so that means that I need
   ;    to subtract the min of the allyears from both.
;
;print, 'verts after'
;print, info[best].vertices
;

	all_y = all_x-all_x	;set to zero
	if info[best].p_of_f gt pval then begin 	;if not good, then set all to flat line
	 got = mean(y)
	 all_y = got
	 info[best].vertvals[*]=0
	 info[best].vertvals[0:1]=got
	 maxvert = max(info[best].vertices)	;get the biggest year

	 info[best].vertices[*]=0
	 info[best].vertices[1]=maxvert
	info[best].n_segments = 1

    info[best].slope[*] = 0

	thismse = mean((all_y-y)^2)
	info[best].segment_mse[*]=0
	info[best].segment_mse[0] = thismse


	 end  else begin

		ok = fill_from_vertices(all_x, info[best].vertices, $
					info[best].vertvals, $
					info[best].n_segments, n_all_x)
		all_y = ok.yfit

    end

info[best].yfit = all_y			;because we made .yfit have right size up front, this should fit in fine.

;;



return, info[best]


end