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

;+
; NAME:
;   landtrendrv04_filter
;
; PURPOSE:
;   Due to the way collapse angles is handled, change_model does is not being used at all even when it is specified.
; CATEGORY:
;   post filtering
;
; CALLING SEQUENCE:
;
;
; RETURN VALUE:
;
;
;
;-

function lt_filter_bycover, best_vertices, best_vertvals, p2_params, tocover=tocover, use_relative_mag=use_relative_mag

  if n_elements(tocover) eq 0 then tocover = 0
  if n_elements(use_relative_mag) eq 0 then use_relative_mag = 1

  vertices = best_vertices
  vertvals = best_vertvals
  change_model = p2_params.change_model
  static_model = p2_params.static_model
  collapse_dist_angle = p2_params.collapse_dist_angle
  collapse_recv_angle = p2_params.collapse_recv_angle


  goods = where(vertices gt 0, n_verts)
  if n_verts eq 0 then return, {ok:1, vertvals:vertvals, vertices:vertices, mags:intarr(n_elements(vertices)-1), magcover:intarr(n_elements(vertices)-1), original_vertvals:vertvals, original_vertices:vertices}

  ; now collapse segment based on threshold
  ok = lt_collapse_segments(vertices, vertvals, static_model, collapse_dist_angle, collapse_recv_angle) 
  if ok.ok ne 1 then return, {ok:-1, message: 'collapse segment error.'}

  vertices = ok.vertices
  vertvals = ok.vertvals
  
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

  pct_tree_loss1 = p2_params.pct_tree_loss1
  pct_tree_loss20 = p2_params.pct_tree_loss20
  pre_dist_cover = p2_params.pre_dist_cover
  pct_tree_gain = p2_params.pct_tree_gain

  ;find meaningful ones
  nodata = where(vertices eq 0, n_nodata)
  goods = where(vertices gt 0, n_verts)
  n_segs = n_verts-1

  ;if n_verts eq 0 then return, {ok:1, vertvals:vertvals, vertices:vertices}
  if n_verts eq 0 then return, {ok:1, vertvals:vertvals, vertices:vertices, mags:intarr(n_elements(vertices)-1), magcover:intarr(n_elements(vertices)-1), original_vertvals:vertvals, original_vertices:vertices}
  ;find disturbances
  vv = vertvals[goods]
  vt = vertices[goods]

  mags = shift(vv,-1) - vv  ;this has "n_verts" indices
  mags[n_segs] = 0

  durs = shift(vt,-1) - vt
  durs[n_segs] = 0

  if change_model ne '' and change_model ne 'none' then begin
    command = 'magcov = fix('+change_model+'(mags))'
    ok = execute(command)

    ;fix last segment magnitude, just to make sure it does not have magnitude from change model
    magcov[where(durs eq 0)] = 0

    command = 'precov = fix(' + static_model + '(vv))'
    ok = execute(command)
    
    if n_nodata gt 0 then precov[nodata] = 0 

    if use_relative_mag ne 0 then magcov = magcov/(precov+0.000001)*100
  endif else begin
    endvertval = vv + mags
    command = 'endcov = fix(' + static_model + '(endvertval))'
    ok = execute(command)
    ; preevent cover
    command = 'precov = fix(' + static_model + '(vv))'
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
    ;original_vertvals[idx] = 9999
  endif
  ;second check the cover loss in disturbance
  covlimit = pct_tree_loss1 + (pct_tree_loss20 - pct_tree_loss1) / 19.0 * durs
  idx = where(magcov lt 0 and abs(magcov) lt covlimit, n)
  if n gt 0 then begin 
    mags[idx] = 0
    magcov[idx] = 0
    ;original_vertvals[idx] = 9999
  endif

  ;third check for recovery
  idx = where(magcov gt 0 and magcov lt pct_tree_gain, n)

  if n gt 0 then begin 
    mags[idx] = 0
    magcov[idx] = 0
    ;original_vertvals[idx] = 9999
  endif

  cum_mags = total(mags, /cumulative)

  vertvals[1:n_verts-1] = vv[0] + cum_mags[0:n_verts-2]

  if tocover eq 1 then begin
    command = 'vertvals = fix(' + static_model + '(vertvals))'
    ok = execute(command)
  end

  return, {ok:1, vertvals:vertvals, vertices:vt, mags:mags, magcover: magcov, original_vertvals:original_vertvals, original_vertices:original_vertices}
end