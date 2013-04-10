;
; iterativel filter segments based on certain rules for detrending
; 
;Step 1. Define disturbance and recovery filter rules.
;            For both disturbance and recovery segments, passing the filter rule mean the segment is real, and will not be considered for detrending.
;
;
;
;Step 2. Identify segments for detrending
;            Check each segment with filter rule defined in step 1, if a segment does not pass filter rule, marked it as Case 1 for decreasing segment or Case 2  for increasing segment. If a segment pass filter rule, marked them as Case 0. All segments marked as 0 will stay as they are. No further evaluation will be done for segment marked as 0.
;
;
;
;Step 3: Detrending
;            for each segment marked as Case 1 or Case 2 in step 2   
;
;           
;
;            if there is no immediate segment neighbor of the same trend direction (Case 1 or Case 2), make the segment flat.
;
;           
;
;            else if there are immediate neighbors of the same trend direction, all consecutive segments of the same trend direction are merged together for evaluation in step 3.1.
;
;           
;
;      step 3.1:
;            if merged segment passes filter rule, keep them as they are
;
;            else make the merged segment flat.
;
;
;
;   This function return {vertvals:vertvals, vertices:vertices, flag: [0, 1, 1]}
;   where flag is the same length of number of segments, with 0 indicate to remove the segment.
;


function detrending, best_vertices, best_vertvals, postprocess_params
  ;function lt_filter_bycover, best_vertices, best_vertvals, postprocess_params, tocover=tocover, use_relative_mag=use_relative_mag

  if n_elements(tocover) eq 0 then tocover = 0
  if n_elements(use_relative_mag) eq 0 then use_relative_mag = 1

  vertices = best_vertices
  vertvals = best_vertvals
  change_model = postprocess_params.change_model
  static_model = postprocess_params.static_model
  collapse_dist_angle = postprocess_params.collapse_dist_angle
  collapse_recv_angle = postprocess_params.collapse_recv_angle


  goods = where(vertices gt 0, n_verts)
  if n_verts eq 0 then return, {ok:1, vertvals:vertvals, vertices:vertices, flags:intarr(n_elements(vertices)-1)}

  ; No need to merge segment    
  ;preserve original values
  original_vertices = vertices
  original_vertvals = vertvals

  ;TODO: check existence of cover model
  if tocover eq 1 then begin
    
    if static_model eq '' or static_model eq 'none' then begin
      print, "At least static model function is needed to convert to cover."
      return, {ok:-1}
    endif
    ;if change_model eq '' or change_model eq 'none' then print, "Info: static model differencing is used for change model."
     
 endif

;clip off the .pro if the model was passed that way:

  if strmid(static_model, strlen(static_model)-4, 4) eq '.pro' then $
		thismodel = strmid(static_model, 0, strlen(static_model)-4) else $
		thismodel = static_model

  pct_tree_loss1 = postprocess_params.pct_tree_loss1
  pct_tree_loss20 = postprocess_params.pct_tree_loss20
  pre_dist_cover = postprocess_params.pre_dist_cover
  pct_tree_gain = postprocess_params.pct_tree_gain

  ;find meaningful ones
  nodata = where(vertices eq 0, n_nodata)
  n_segs = n_verts-1

  ;find disturbances
  vv = vertvals[goods]
  vt = vertices[goods]

  mags = shift(vv,-1) - vv  ;this has "n_verts" indices
  mags[n_segs] = 0

  ; construct flag, it has the same number of elements as vertices -1: number of segments
  flags = intarr(n_elements(vertvals))

  durs = shift(vt,-1) - vt
  durs[n_segs] = 0

  ; convert to cover if requested
  if change_model ne '' and change_model ne 'none' then begin
    command = 'magcov = fix('+change_model+'(mags))'
    ok = execute(command)

    ;fix last segment magnitude, just to make sure it does not have magnitude from change model
    magcov[where(durs eq 0)] = 0

    command = 'precov = fix(' + thismodel + '(vv))'
    ok = execute(command)
    
    if n_nodata gt 0 then precov[nodata] = 0 

    if use_relative_mag ne 0 then magcov = magcov/(precov+0.000001)*100
  endif else begin
    endvertval = vv + mags
    command = 'endcov = fix(' + thismodel + '(endvertval))'
    ok = execute(command)
    ; preevent cover
    command = 'precov = fix(' + thismodel + '(vv))'
    ok = execute(command)

    if n_nodata gt 0 then begin 
      precov[nodata] = 0
      endcov[nodata] = 0
    endif
    
    if use_relative_mag eq 0 then begin
      magcov = endcov - precov
    endif else begin
      magcov = (endcov - precov)/(precov+0.000001)*100
    endelse
  endelse

  ;first check predict cover
  idx = where(precov lt pre_dist_cover and durs ne 0 and magcov lt 0, n)
  ;reset all variables
  if n gt 0 then begin 
    mags[idx] = 0
    magcov[idx] = 0
    flags[idx] = 1 ;
  endif
  ;second check the cover loss in disturbance
  covlimit = pct_tree_loss1 + (pct_tree_loss20 - pct_tree_loss1) / 19.0 * durs
  idx = where(magcov lt 0 and abs(magcov) lt covlimit, n)
  if n gt 0 then begin 
    mags[idx] = 0
    magcov[idx] = 0
    flags[idx] = 1 ; 
  endif

  ;third check for recovery
  idx = where(magcov gt 0 and magcov lt pct_tree_gain, n)
  if n gt 0 then begin 
    mags[idx] = 0
    magcov[idx] = 0
    flags[idx] = 2 ; 
  endif

  return, {ok:1, vertvals:vertvals, vertices:vertices, flags:flags}

end

