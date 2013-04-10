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
; distyear: the disturbance of interest, this may not be the landtrendr vertex, use together with fix_vertex
; vertex_years: landtrendr vertexes
; vertex_vals: vertex values corresponding to vertex_years
; all_years: all the years used in segmentation
; fix_vertex: used when distyear is not landtrendr vertex
;
; return magnitude, duration, and rate
; 
; Feb. 21, 2011, add disturbance information in extracted values
;
function calculate_recovery_rate, distyear, vertex_years, vertex_vals, all_years, fix_vertex=fix_vertex

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

    distvals = [0,0,0,0] ;year, preval, mag, dur
    mag = [0,0,0]
    dur = [0,0,0]
    rate = [0,0,0]
    prev = [0,0,0]

    ;the vertex following disturbance start vertex year is the start of recovery
    good = where(vertex_years gt 0, n)
    
    ;if there is no vertex for this disturbance, most likely this is a filled in disturbance pixel
    if n eq 0 then return, {ok:1, distvals:distvals, mag:mag, dur:dur, rate:rate, prev:prev}
    
    v_yrs = vertex_years[good]
    v_vals = vertex_vals[good]
    seg_durs = shift(v_yrs, -1)-v_yrs ;the last element of seg_durs is not valid
    seg_mags = shift(v_vals, -1)-v_vals ;the last element of seg_mags is not valid
    
    n_segments = n_elements(seg_durs)-1
    seg_durs = seg_durs[0:n_segments-1]
    seg_mags = seg_mags[0:n_segments-1]
    seg_prev = v_vals[0:n_segments-1]
    
    ;calculate rate of the segment
    ;TODO: should we use relative rate?
    seg_rate = seg_mags / seg_durs
    
    ;find the start of recover segment vertex
    ; there are some strange cases where the vertex are the same across band but has a disturbance year.
    ; e.g. vertex_years = [1984, 1984, 0, 0...], and disturbance year is 2008.
    this = where(v_yrs eq dist_vertex_year[0], n)
    if n gt 1 then begin ; this is some strange pixels
      print, "Warning: multiple vertex has the same year"
      this = this[0]
    endif
    
    recovery_start = this[0] + 1
    
    distvals[0] = distyear
    distvals[1] = v_vals[this]
    distvals[2] = seg_mags[this]
    distvals[3] = seg_durs[this]
    
    ;if the recover start at the last year, then no recovery information is available yet.
    ;TODO: should this be made as a flag, e.g. -9999
    if recovery_start eq n_elements(v_yrs)-1 then return,  {ok:1, distvals:distvals, mag:mag, dur:dur, rate:rate, prev:prev}
    
    rec_durs = seg_durs[recovery_start:n_segments-1]
    rec_mags = seg_mags[recovery_start:n_segments-1]
    rec_rates = seg_rate[recovery_start:n_segments-1]
    rec_prev = seg_prev[recovery_start:n_segments-1]
    
    n = n_elements(rec_durs)
    if n ge 3 then begin
        mag[0:2] = rec_mags[0:2]
        dur[0:2] = rec_durs[0:2]
        rate[0:2] = rec_rates[0:2]
        prev[0:2] = rec_prev[0:2]
    endif else begin
        mag[0:n-1] = rec_mags[0:n-1]
        dur[0:n-1] = rec_durs[0:n-1]
        rate[0:n-1] = rec_rates[0:n-1]
        prev[0:n-1] = rec_prev[0:n-1]
    endelse
    
    return,  {ok:1, distvals:distvals, mag:mag, dur:dur, rate:rate, prev:prev}

end