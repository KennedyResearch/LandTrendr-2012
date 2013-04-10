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

;
;
; distyear: the disturbance of interest, this may not be the landtrendr vertex, use together with fix_vertex
; intervals: number of years after disturbance
; vertex_years: landtrendr vertexes
; vertex_vals: vertex values corresponding to vertex_years
; all_years: all the years used in segmentation
; fix_vertex: used when distyear is not landtrendr vertex
;
; return the rate at year distyear + duration + interval
;
function calculate_recovery_slice, distyear, intervals, vertex_years, vertex_vals, all_years, fix_vertex=fix_vertex, use_delta=use_delta
  ;this is the disturbance start vertex year
  dist_vertex_year = distyear
  
  ;if distyear is not a vertex, but a calculated disturbance from the vertex and image used,
  ;then find the disturbance vertex.
  ;this is an issue for multi-year duration disturbance.
  if keyword_set(fix_vertex) ne 0 then begin
    this = where(all_years eq distyear, n)
    if n eq 0 then return, {ok: 0, message: 'this should not happen!'}
    dist_vertex_year = all_years[this-1]
    dist_vertex_year = dist_vertex_year[0]
    
    ;is this a vertex
    check = where(vertex_years eq dist_vertex_year, n)
    
    ;this is not a vertex, most likely this is due to cloudy image
    ;now need to find the actual vertex
    if n le 0 then begin
      check = where(vertex_years lt dist_vertex_year, n)
      if n eq 0 then return, {ok: 0, message: 'this should not happen!'}
      dist_vertex_year = max(vertex_years[check])
    endif
  endif
  
  rate = intarr(n_elements(intervals))
  
  ;the vertex following disturbance start vertex year is the start of recovery
  good = where(vertex_years gt 0, n)
  
  ;if there is no vertex for this disturbance, most likely this is a filled in disturbance pixel
  if n eq 0 then return, {ok:1, rate:rate}
  
  v_yrs = vertex_years[good]
  v_vals = vertex_vals[good]
  seg_durs = shift(v_yrs, -1)-v_yrs ;the last element of seg_durs is not valid
  seg_mags = shift(v_vals, -1)-v_vals ;the last element of seg_mags is not valid
  
  n_segments = n_elements(seg_durs)-1
  seg_durs = seg_durs[0:n_segments-1]
  seg_mags = seg_mags[0:n_segments-1]
  
  ;calculate rate of the segment
  ;TODO: should we use relative rate?
  seg_rate = seg_mags / seg_durs
  
  ;find the start year of recover segment
  this = where(v_yrs eq dist_vertex_year[0])
  recovery_start_year = v_yrs[this[0] + 1]
  
  ;find the start value of recover segment
  recovery_start_val = v_vals[this[0] + 1]
  
  for i = 0, n_elements(intervals)-1 do begin
    target_year = recovery_start_year + intervals[i]
    
    ;if the year requested is pass the last year of image stack, then no information is available.
    if target_year gt max(v_yrs) then rate[i] = 0
    
    ;find which segment recovery_start_year + interval belongs to
    for this_idx = this[0]+1, n_elements(v_yrs)-2 do begin
      if v_yrs[this_idx] lt target_year and v_yrs[this_idx+1] ge target_year then begin
        if keyword_set(use_delta) then begin ;if asked for delta
          rate[i] = fix((v_vals[this_idx+1] - v_vals[this_idx]) / (v_yrs[this_idx+1] - v_yrs[this_idx]) * (target_year - v_yrs[this_idx])) + v_vals[this_idx]
        endif else begin
          rate[i] = seg_rate[this_idx]
        endelse
        break
      endif
    endfor
  end
  return,  {ok:1, rate:rate}
end