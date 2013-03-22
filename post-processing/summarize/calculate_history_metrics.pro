;+
; NAME:
;   CALCULATE_HISTORY_METRICS
;
; PURPOSE:
;   calculate historical metrics for a single pixel for [start_year, end_year]
;
; AUTHOR:
;
; CATEGORY:
;   Post processing
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
;  March 2013.  REK updated take out unnecessary history variables. 

;-
function calculate_history_metrics, all_years, vertexes, vertvals, modifier, b_stack, g_stack, w_stack, start_year, end_year, dist_thresh=dist_thresh, rec_thresh=rec_thresh
  
    if n_elements(dist_thresh) eq 0 then dist_thresh = 15
    if n_elements(rec_thresh) eq 0 then rec_thresh = 15
    
    ok = collapse_segments(vertexes, vertvals, modifier, dist_thresh, rec_thresh)
    
    ;fix requested year range if necessary
    start_year = start_year > min(all_years)
    end_year = end_year < max(all_years)
    
    n = end_year - start_year + 1
    
    b_t = intarr(n)
    g_t = intarr(n)
    w_t = intarr(n)
    
    ;delta of B, G, W at t
    db_t = intarr(n)
    dg_t = intarr(n)
    dw_t = intarr(n)
    
    	;recent vertex B, G, W
    	;rv_b = intarr(n)
    	;rv_g = intarr(n)
    	;rv_w = intarr(n)
    
    ;time since recent vertex
    ts_v = intarr(n)
    
   	 ;previous vertex B, G, W
   	; pv_b = intarr(n)
    	;pv_g = intarr(n)
    	;pv_w = intarr(n)
    
    	;p_dur = intarr(n)
    
    
    valids = where(vertexes gt 0, n_valid)
    if n_valid lt 2 then begin
      return, {b_t:b_t, g_t:g_t, w_t:w_t, db_t:db_t, dg_t:dg_t, dw_t:dw_t, ts_v:ts_v}	;shortened march 2013; others not used. 
      
    		;return, {b_t:b_t, g_t:g_t, w_t:w_t, db_t:db_t, dg_t:dg_t, dw_t:dw_t, rv_b:rv_b, rv_g:rv_g, rv_w:rv_w, ts_v:ts_v, pv_b:pv_b, pv_g:pv_g, pv_w:pv_w, p_dur:p_dur}
    endif
    
    ;filter out non-valid vertex for later use
    ;vertexes = vertexes[valids]
    ;vertvals = vertvals[valids]
    
    bgw_index = intarr(n_valid)
    for i=0, n_valid-1 do begin
      bgw_index[i] = where(all_years eq vertexes[valids[i]])
    endfor
    
;    out_vertexes = indgen(n) + start_year
;    b_t = interpol(b_stack[bgw_index], vertexes[valids], out_vertexes)
;    g_t = interpol(g_stack[bgw_index], vertexes[valids], out_vertexes)
;    w_t = interpol(w_stack[bgw_index], vertexes[valids], out_vertexes)
    
    used_vertexes = all_years[bgw_index]
    used_durs = shift(used_vertexes, -1) - used_vertexes > 0
    
    for this_year = start_year, end_year do begin
      this_index = this_year - start_year
            
      ;is this a missing year
      this = where(all_years eq this_year, n)
      if n eq 0 then begin
        off = abs(all_years - this_year)
        before = where(off eq min(off))
        before = before[0]
        
        b_t[this_index] = b_stack[before] + fix((b_stack[before+1]-b_stack[before])*(this_year-all_years[before])/(all_years[before+1]-all_years[before]))
        g_t[this_index] = g_stack[before] + fix((g_stack[before+1]-g_stack[before])*(this_year-all_years[before])/(all_years[before+1]-all_years[before]))
        w_t[this_index] = w_stack[before] + fix((w_stack[before+1]-w_stack[before])*(this_year-all_years[before])/(all_years[before+1]-all_years[before]))
      endif else begin 
        b_t[this_index] = b_stack[this]
        g_t[this_index] = g_stack[this]
        w_t[this_index] = w_stack[this]
      endelse
    
    
      ;find the segment this year belongs to.
      diff = abs(used_vertexes - this_year)
      seg_start = where(diff eq min(diff))
      seg_start = seg_start[0] ; in case multile match
      
      ;if it is the beginning of a segment, it is considered as end of the previous segment firest.
      if this_year le vertexes[seg_start] then seg_start = seg_start - 1
      
      ;this is the start of time series
      if seg_start lt 0 then continue; return, {b_t:b_t, g_t:g_t, w_t:w_t, db_t:db_t, dg_t:dg_t, dw_t:dw_t, rv_b:rv_b, rv_g:rv_g, rv_w:rv_w, ts_v:ts_v, pv_b:pv_b, pv_g:pv_g, pv_w:pv_w, p_dur:p_dur}
      
            
      		;recent vertex value
      		;rv_b[this_index] = b_stack[bgw_index[seg_start]]
      		;rv_g[this_index] = g_stack[bgw_index[seg_start]]
      		;rv_w[this_index] = w_stack[bgw_index[seg_start]]
      
      ts_v[this_index] = this_year - used_vertexes[seg_start]
      
      db_t[this_index] = b_t[this_year-start_year] - b_stack[bgw_index[seg_start]]
      dg_t[this_index] = g_t[this_year-start_year] - g_stack[bgw_index[seg_start]]
      dw_t[this_index] = w_t[this_year-start_year] - w_stack[bgw_index[seg_start]]
      
      		;if there is a previous segment
      		;if seg_start gt 0 then begin
      		;  pv_b[this_index] = b_stack[bgw_index[seg_start-1]]
      		;  pv_g[this_index] = g_stack[bgw_index[seg_start-1]]
      		;  pv_w[this_index] = w_stack[bgw_index[seg_start-1]]
        
       		; p_dur[this_index] = used_durs[seg_start-1]
      		;endif
    endfor
    return, {b_t:b_t, g_t:g_t, w_t:w_t, db_t:db_t, dg_t:dg_t, dw_t:dw_t,ts_v:ts_v}
  
    
    		;return, {b_t:b_t, g_t:g_t, w_t:w_t, db_t:db_t, dg_t:dg_t, dw_t:dw_t, rv_b:rv_b, rv_g:rv_g, rv_w:rv_w, ts_v:ts_v, pv_b:pv_b, pv_g:pv_g, pv_w:pv_w, p_dur:p_dur}
  end
  
  
;test script
; logic test
;     all_years, vertexes, vertvals, b_stack, g_stack, w_stack, start_year, end_year
;    vertexes = [1984, 1988, 1997, 2008, 0, 0, 0]
;    vertvals = [10, 12, -50, 9, 0, 0, 0]
;    all_years = indgen(25)+1984
;    valids = where(vertexes gt 0)
;    b_stack = interpol(vertvals[valids], vertexes[valids], all_years)
;    g_stack = b_stack + 15
;    w_stack = b_stack - 20
;
;    ok = calculate_history_metrics(all_years, vertexes, vertvals, b_stack, g_stack, w_stack, 1988, 1990)
  
