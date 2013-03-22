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


;*******************************************************************
;
;MAIN ROUTINE:   ftv_v1
;
;*******************************************************************


function ftv_v1, all_x, goods, y, vertices

;print, vertices
;given vertices (the x vals where the trajectory is to
;   bend) found using tbcd separately, apply
;   the best trajectory through those points
;   Since this is presumably based on the same
 ;  image as the one used to find the vertices originally,
 ;  the goods values should be the same and we shouldn't
 ;   expect a vertex to show up in a place where
 ;   this image doesn't have valid values

;February 28, 2007. REK.


  x = all_x[goods]
 n_obs = n_elements(x)
 n_verts = n_elements(vertices)

;*********************************
;CHECK THE VERTICES.
; Sometimes it seems that we get
; vertices in places where there
; is a cloud masked out, so we
; need to fix those by moving front or back one
;And then cull out any dupes
;
mismatch=0
n_vertices = n_elements(vertices)
n_segments = n_vertices-1
v = intarr(n_verts)

for i = 0, n_verts-1 do begin
  v[i] = where(x eq vertices[i], n_matches)

  if n_matches eq 0 then begin
	mismatch=1
  	v[i] = max(where(x lt vertices[i], n_matches2))
  	if (n_matches2 eq 0) then v[i] = min(where(x gt vertices[i])) else $
  	if i gt 0  then begin
	  if v[i] eq v[i-1] and v[i] ne n_obs-1 then $		;changed to v[i] ne n_obs-1
  	     v[i] =min(where(x gt vertices[i]))
  	end
  end

 end
if min(v) eq -1 then stop		;at this point, v is referenced to the "goods", so
								;not the absolute x year


if mismatch then begin

	q = fast_unique(v)
	v = q[sort(q)]
	n_vertices = n_elements(v)
	n_segments = n_vertices-1
end

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



	n_all_x = n_elements(all_x)		;n_obs is the number of filtered x's


if n_segments eq 0 then begin		;this is an error catch -- need to see why some verts are all -1986.

	n_segments =1
	mny = mean(y)
	best_fit = {slopes:0, $
				n_segments:1, $
				vertvals:[mny,mny], $
				yfit:fix(replicate(mny, n_obs))}
	;v = [x[0], x[n_obs-1]]
	v = [0, n_all_x-1]		;changed 3/26/08

;TODO <YANG> why v is not the same size as yfit

end else  best_fit = find_best_trace(x, y, v, n_segments)	;NOTE THAT N_VERTICES IS DIFFERENT THAN CALL TO FIND_BEST_TRACE (THE OLDER VERSION)

;get the mse

segment_mse = sqrt(score_segments(x,y,v, n_segments+1))

;the v is based on the goods index, not the all_x index, so
;   if there is a cloud year in the middleof the sequence,
;   it'll compress the end of the fitted outputs. So first we
;   have to convert the v to the all_x index
vv = v
for iv = 0, n_vertices-1 do vv[iv] = where(all_x eq x[v[iv]])
v = vv



new_vertvals = best_fit.vertvals

info =  {vertices:v, vertvals:best_fit.vertvals, $
		 	yfit:all_x-all_x, $	;changed yfit 2/29/08 so all years involved
			slope:best_fit.slopes, n_segments:n_segments, $
			segment_mse:segment_mse}
;oi = info	;remove this once problem resolved.




	;front end
	if all_x[0] ne x[0] then begin

	  new_n_segments = info.n_segments + 1
	  new_n_vertices = new_n_segments + 1

	  new_vertices = lonarr(new_n_vertices)
	  new_vertices[1:new_n_vertices-1] = info.vertices[0:info.n_segments]
    new_vertices[0] = 0

    new_vertvals = fltarr(new_n_vertices)
    new_vertvals[1:new_n_vertices-1] = info.vertvals[0:info.n_segments]
    new_vertvals[0] = new_vertvals[1]

    new_slope = fltarr(new_n_segments)
    new_slope[0] = 0
    new_slope[1:new_n_segments-1] = info.slope[0:info.n_segments-1]

    new_segment_mse = [0, info.segment_mse]

;    info =  {vertices:new_vertices, vertvals:new_vertvals, $
;		 	yfit:all_x-all_x, $	;changed yfit 2/29/08 so all years involved
;			slope:new_slope, n_segments:new_n_segments, $
;			segment_mse:new_segment_mse}

    ;NO NEED TO TAKE OUT ANY SEGMENTS ADDED
    if new_n_segments gt n_elements(vertices)-1 then begin
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

    info =  {vertices:new_vertices, vertvals:new_vertvals, $
		 	yfit:all_x-all_x, $	;changed yfit 2/29/08 so all years involved
			slope:new_slope, n_segments:new_n_segments, $
			segment_mse:new_segment_mse}

;    info.n_segments = new_n_segments
;    info.vertices = new_vertices
;    info.vertvals = new_vertvals
;    info.slope = new_slope
    ;YANG mse is not updated!

	   ;first, handle the X position of the vertices.  The vertices
	   ;  are in index values, not years, so we have to figure out
	   ;  the index of the first one and then add that
;
;		index_offset = where(all_x eq x[0], n_index_offset)
;		if n_index_offset eq 0 then message, 'index mismatch error.'
;
;		not_zeros = where(info[best].vertices ne 0)	;we know there's at least 1
;
;		info[best].vertices[not_zeros] = info[best].vertices[not_zeros] + index_offset[0] ;shift all of the vertices up
;
;		info.vertices[0] = 0	;set to the start
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
;		dy = extendlength * info.slope[0] * (-1) 	;-1 because we're going backwards
;		info.vertvals[0] = info.vertvals[0] + dy

	end		;confirmed 2/29/08 that this works.





	if all_x[n_all_x-1] ne x[n_obs-1] then begin 	;if the last year is not the same

		new_n_segments = info.n_segments + 1
	  new_n_vertices = new_n_segments + 1

	  new_vertices = [info.vertices, n_all_x-1]
    new_vertvals = [info.vertvals, info.vertvals[info.n_segments]]
    new_slope = [info.slope,0]
    new_segment_mse = [info.segment_mse, 0]

    if new_n_segments gt n_elements(vertices)-1 then begin

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

    info =  {vertices:new_vertices, vertvals:new_vertvals, $
		 	yfit:all_x-all_x, $	;changed yfit 2/29/08 so all years involved
			slope:new_slope, n_segments:new_n_segments, $
			segment_mse:new_segment_mse}

;    info.n_segments = new_n_segments
;    info.vertices = new_vertices
;    info.vertvals = new_vertvals
;    info.slope = new_slope
    ;YANG mse is not updated!


;    info =  {vertices:new_vertices, vertvals:new_vertvals, $
;		 	yfit:all_x-all_x, $
;			slope:new_slope, n_segments:new_n_segments, $
;			segment_mse:new_segment_mse}



	   ;first, handle the X position of the vertices.  The vertices
	   ;  are in index values, not years.   so the last vertex should
	   ;  simply point to the last element in the all_x array


;		info.vertices[info.n_segments] = n_all_x-1

	   ;then handle the y offset
	   ; First, need to see what the number of years are between
	   ;   the beginning of all x and x, and use that to extend the slope
	   ;   of the first segment

;	   	extendlength = all_x[n_all_x-1]-x[n_obs-1]
;		dy = extendlength * info.slope[info.n_segments-1]
;		info.vertvals[info.n_segments] = $
;				info.vertvals[info.n_segments] + dy

	end		;did not have a test available to confirm this works!


;***********************
;Now fill in the yfit values for the "allyears" range, to get a
;  true yfit even for years that were missing from this pixel

;
;if max(info.vertices) gt n_all_x-1 then stop

		ok = fill_from_vertices(all_x, info.vertices, $
					info.vertvals, $
					info.n_segments, n_all_x)
		info.yfit = ok.yfit

;    end

;info.yfit = all_y			;because we made .yfit have right size up front, this should fit in fine.

;
;w, 0
;   colors = [ 'ff9922'xl, '99ff22'xl, '2299ff'xl, 'aaaaff'xl, 'ffaaff'xl, 'ddaadd'xl, '22dddd'xl]
;   plot, x+1985, y, /ynoz
;   for i = 0, n_elements(info)-1 do begin & oplot, x+1985, info[i].yfit, thick = 2, color = colors[i] & end
;
;   oplot, x+1985, info.yfit, color = '0000ff'xl, thick = 2
;
;
;;stop



return, info


end



