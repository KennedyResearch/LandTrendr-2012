;+
; Name:
;   calculate_precursor
;
; Purpose:
;   Given an image with year of interest, extract the information for previous and current segment
;   previous_segment: B, G, W, duration, ΔB, ΔG, ΔW
;   current_segment: B, G, W, offset, ΔB, ΔG, ΔW
;-
function calculate_precursor, yoi, vertex_years, all_years, b_stack, g_stack, w_stack
    preimg = intarr(7)
    curimg = intarr(7)
    
    good = where(vertex_years gt 0, n)
    ;if there is no vertex for this disturbance, most likely this is a filled in disturbance pixel
    if n eq 0 then return, {ok:1, preimg:preimg, curimg:curimg}
    
    v_yrs = vertex_years[good]
    seg_durs = shift(v_yrs, -1) - v_yrs
    n_segments = n_elements(seg_durs) - 1
    seg_durs = seg_durs[0:n_segments-1]
    
    ;where is current segment
    ydiff = v_yrs - yoi
    vic = where(ydiff gt 0, n)
    if n gt 0 then ydiff[vic] = 9999
    ydiff = abs(ydiff)
    cur = where(ydiff eq min(ydiff))
    
    cur_year = v_yrs[cur]
    vic = where(all_years eq cur_year[0])
    curimg[0] = b_stack[vic]
    curimg[1] = g_stack[vic]
    curimg[2] = w_stack[vic]

    curimg[3] = yoi - cur_year
    
    ;use absolute difference just in case yoi is not in the stack
    ;this should be fixed when all years are included
    ydiff = all_years - yoi
    vic = where(ydiff gt 0, n)
    if n gt 0 then ydiff[vic] = 9999
    ydiff = abs(ydiff)
    yoi_idx = where(ydiff eq min(ydiff))
    curimg[4] = b_stack[yoi_idx] - curimg[0]
    curimg[5] = g_stack[yoi_idx] - curimg[1]
    curimg[6] = b_stack[yoi_idx] - curimg[2]
    
    if cur[0] gt 0 then begin
        vic = where(all_years eq v_yrs[cur[0]-1])
        preimg[0] = b_stack[vic]
        preimg[1] = g_stack[vic]
        preimg[2] = w_stack[vic]

        preimg[3] = v_yrs[cur[0]] - v_yrs[cur[0]-1]
        
        preimg[4] = curimg[0] - preimg[0]
        preimg[5] = curimg[1] - preimg[1]
        preimg[6] = curimg[2] - preimg[2]
    end

    return, {ok:1, preimg:preimg, curimg:curimg}
 end