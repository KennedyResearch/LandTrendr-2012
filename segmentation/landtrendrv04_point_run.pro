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
;   landtrendrv04_point_run
;
; PURPOSE:
;   Run landtrendr on a single location
;
; CATEGORY:
;   Calibration
;
; CALLING SEQUENCE
;
; RETURN VALUE:
;    return, {ok:1, best_model:modelstack, modifier:modifier, point_stack:pointstack}
;    where
;      best_model is an array of
;        best_model = {vertices:intarr(max_count), $
;                      vertvals:intarr(max_count), $
;                      yfit:fltarr(n_all_yrs)+mean(vvals), $
;                      n_segments:0, $
;                      p_of_f:1.0, $
;                      f_stat:0., $
;                      ms_regr:0., $
;                      ms_resid:0., $   ;1 = autocorr, 2, = find_segments6, 3=findsegments 7
;                      segment_mse:intarr(max_count-1)-1} ;set to negative one as flag
;
;      pointstack is an array of
;        {distrec_mag:distrec_mag, distrec_preval:distrec_preval, distrec_dur:distrec_dur}
;
;
; CHANGE HISTORY:
;   1. based on previous graph rountine for point mode
;   2. reformat
;   3. remove graphing
;   4. remove slow vs abrupt disturbance
;-
function landtrendrv04_point_run, run_params, post_process_params,$
  point_coords, point_name, calibration_params, file_coords=file_coords

  info = run_params.image_info
  index = run_params.index
  subset=run_params.subset
  mask_image = run_params.mask_image
  kernelsize = run_params.kernelsize
  background_val = run_params.background_val
  skipfactor = run_params.skipfactor
  desawtooth_val = run_params.desawtooth_val
  pval = run_params.pval
  max_segments = run_params.max_segments
  fix_doy_effect = run_params.fix_doy_effect
  divisor =run_params.divisor
  minneeded = run_params.minneeded
  recovery_threshold=run_params.recovery_threshold
  
  ;add new parameter
  distweightfactor = run_params.distweightfactor
  vertexcountovershoot = run_params.vertexcountovershoot
  bestmodelproportion = run_params.bestmodelproportion
  
  
  minimum_number_years_needed = 6     ;if we have fewer years than this, we can't do it.
  
  ;get the number of years we're analyzing.
  ;updating this august 21, 2008 for the multiple images per year
  ;n_yrs = n_elements(image_info)
  
  years = fast_unique(info.year)
  years = years[sort(years)]
  n_yrs = n_elements(years)
  
  n_images = n_elements(info)
  
  if n_yrs lt minimum_number_years_needed then begin
    print, 'make_figure_landtrendrv04_examine:  there are fewer than the minimum
    print, 'number of years available for disturbance/recovery extraction'
    print, 'the minimum is: '+string(minimum_number_years_needed)
    print, 'the number of files given to extract_disturbance_recovery4.pro: '+string(n_yrs)
    print, 'confirm that the information from find_image_stack_files is correct'
    return, {ok:0}
  end
  
  if divisor eq -1 then message, 'Divisor value must be set by user, not set to -1, for graphing'
  
  
  ;START THE GOOD STUFF
  
  ;now go through and build it.
  offset = (kernelsize-1)/2
  
  min_year = min(info.year)
  x_axis = years
  
  if n_elements(background_val) eq 0 then background_val = 0
  
  vals = fltarr(n_yrs)
  
  ;get the pixel size
  zot_img, info[0].image_file, hdr, blank, /hdronly
  pxsize = hdr.pixelsize
  offset_map = offset * pxsize[0]
  
  if keyword_set(file_coords) eq 1 then $ 
    subset = [ [point_coords[0]-offset_map, point_coords[1]-offset_map], $
      [point_coords[0]+offset_map, point_coords[1]+offset_map]] $     
  else $ 
    subset = [ [point_coords[0]-offset_map, point_coords[1]+offset_map],$
      [point_coords[0]+offset_map, point_coords[1]-offset_map]]

    
  
  ;make a variable to hold the values and the years
  img = intarr(kernelsize, kernelsize, n_yrs)
  cld_img = bytarr(kernelsize, kernelsize, n_yrs)		;added v4
  usedmask = intarr(kernelsize, kernelsize) ;valide values for years with multiple image
  
  ;which image was used
  idx_img = bytarr(kernelsize, kernelsize, n_yrs)
  
  ;for this point, process everything
  k=0
  
  for i = 0, n_yrs-1 do begin
    ;FIRST CHECK TO SEE HOW MANY IMAGES FOR THIS YEAR
    fileid = i+k
    this = where(info.year eq years[i], n)	;<- N IS THE KEY VARIABLE
    
    ;masks and ids for current year
    cur_mask = bytarr(kernelsize, kernelsize)
    cur_img = intarr(kernelsize, kernelsize)
    cur_idx = bytarr(kernelsize, kernelsize)
    
    ;SINGLE IMAGE FOR THIS YEAR
    if n eq 1 then begin
      tempsubset=subset
      
      landtrendr_image_read, info[fileid], hdr, img1, tempsubset, index, modifier, background_val
      
      sz = size(img1, /dim)
      
      bads = where(img1 eq background_val, n_bads)
      if n_bads ne 0 then cld_img[*,*,i] = (cld_img[*,*,i]+ (img1 eq background_val)) ne 0 	;ne 0 needed incase cloud image and background val!
      
      ;took out divisor here relative to run_tbcd_single_chunk, because will get captured later
      img[*,*,i] = img1/divisor	;added 2/7/08 this will scale to max of 1000
      idx_img[*,*,i] = replicate(fileid, kernelsize, kernelsize)	;set all pixels to this one
      
      ;now read the cloud mask
      ; if there is no cloud mask, then just skip this
      if info[fileid].cloud_file ne 'none' and info[fileid].cloud_file ne '' then begin
        tempsubset=subset
        if info[fileid].cloud_file eq 'band8' then $
          zot_img, info[fileid].image_file, clhdr, mimg, layers=[8], subset=tempsubset else $
          zot_img, info[fileid].cloud_file, clhdr, mimg, subset=tempsubset
        cld_img[*,*,i] = (cld_img[*,*,i] + (mimg gt 2300)) ne 0
      end  ;cloud image
    end		;single image for this year
    
    ;MULTIPLE IMAGES PER YEAR
    ;if multiple image exists for this year, select one and make the others masked out
    if n gt 1 then begin
      victims = info[this]
      ;sort by priority
      vicorder = sort(victims.image_priority)
      victims = victims[vicorder]
      
      ;read in the cloud images for each victim, in order of priority
      for j = 0, n-1 do begin
        tempsubset=subset
        landtrendr_image_read, victims[j], hdr, img1, tempsubset, index, modifier, background_val
        
        ;now read the cloud mask
        ; if there is no cloud mask, then just skip this
        mimg = replicate(0, kernelsize, kernelsize)
        
        if victims[j].cloud_file ne 'none' and victims[j].cloud_file ne '' then begin
          tempsubset=subset
          if victims[j].cloud_file eq 'band8' then $
            zot_img, victims[j].image_file, clhdr, mimg, layers=[8], subset=tempsubset else $
            zot_img, victims[j].cloud_file, clhdr, mimg, subset=tempsubset
        ;cld_img[*,*,fileid+j] = (cld_img[*,*,fildid+j] + (mimg gt 2300)) ne 0
        end
        
        ;identify pixels that are not background, that haven't been picked by the higher
        ;   priority image, and that are not in the cloud mask
        valid = where(img1 ne background_val and cur_mask eq 0 and mimg le 2300, n_valid)
        if n_valid ne 0 then begin
          cur_img[valid] = img1[valid]
          cur_mask[valid] = 1					;mask gets set to 1 if the pixel is chosen
          cur_idx[valid] = replicate(this[vicorder[j]], n_valid)
        end
      endfor
      k = k + n - 1
      img[*,*,i] = cur_img/divisor
      cld_img[*,*,i] = cur_mask ne 1		;any cur_mask pixels still remaining 0 were not chosen
      idx_img[*,*,i] = cur_idx
    end
  end			;accumulating the values for this point
  
  ;DO THE FITTING FOR THIS POINT
  ;  	relative to run_tbcd_single_chunk, here we don't need to re-extract the values
  ;   from the image in a loop, so just figure out usable pixels right away.
  chunk = img
  usable = cld_img eq 0	;identify all good pixels in the window around the point
  slice = total(chunk*usable,1)			;multiple the good ones by the image values (zero out bad values), and total on one dimension
  slice_usable = total(usable, 1)			;identify the usable ones in this slice
  vals = total(slice,1)/total(slice_usable, 1)	;creat the "vals" variable now by dividing by number of good pixels per year
  
  goods= where(cld_img[offset, offset, *] ne 1, ngds)		;just use the central pixel's cloud mask to determine the number of good years
  
  ; goods = where(vals[*,p] ne background_val, ngds)	;
  seed = randomseed()
  
  
  if n_elements(modifier) eq 0 then modifier = 1
  
  ;first, take out the doy effect
  if n_elements(fixdoyeffect) ne 0 then begin
    idxs = idx_img[offset, offset,*]		;use the middle pixel for the day of year -- could cause anomalies if central is weird
    
    uniques = fast_unique(info[goods].julday)
    if n_elements(uniques) gt 4 then begin
      r = poly_fit(info[idxs[goods]].julday, vals[goods],2, chisq=chisq,yfit = yfit)
      m = mean(yfit)
      w, 0
      plot, info[idxs[goods]].julday, vals[goods], psym=4, symsize= 2
      oplot, info[idxs[goods]].julday, yfit, color = 'ffff00'xl
      
      zzz = calc_fitting_stats3(vals[goods], yfit, 3, resid=resid)
      if zzz.p_of_f lt pval then outvals = m+resid else $
        outvals = vals[goods]
      oplot, info[idxs[goods]].julday, outvals, psym = 4, color = '00ff00'xl
    end else outvals = vals[goods]
  end else outvals = vals[goods]
  
  
  ;run on all the points and vary run parameters
  ; landtrendr parameter
  pvals = calibration_params.pvals
  maxsegments = calibration_params.maxsegments
  recovery_thresholds = calibration_params.recovery_thresholds
  desawtooth_thresholds = calibration_params.desawtooth_thresholds
  oververtex_thresholds = calibration_params.oververtex_thresholds
  distweight_thresholds = calibration_params.distweight_thresholds
  bestmodel_thresholds = calibration_params.bestmodel_thresholds
  
  
  ;p1 parameter
  dist_collapse_angles = calibration_params.dist_collapse_angles
  recv_collapse_angles = calibration_params.recv_collapse_angles
  
  ;p2 parameter
  pct_tree_loss1_thresholds = calibration_params.pct_tree_loss1_thresholds
  pct_tree_loss20_thresholds = calibration_params.pct_tree_loss20_thresholds
  pre_dist_cover_thresholds = calibration_params.pre_dist_cover_thresholds
  pct_tree_gain_thresholds = calibration_params.pct_tree_gain_thresholds
  
  ;total landtrendr run
  n_runs = n_elements(pvals) * n_elements(maxsegments) * n_elements(recovery_thresholds) * n_elements(desawtooth_thresholds) * $
    n_elements(oververtex_thresholds) * n_elements(distweight_thresholds) * n_elements(bestmodel_thresholds)
    
  ; total runs for p1 and p2
  n_runs_p1 = n_runs * n_elements(dist_collapse_angles) * n_elements(recv_collapse_angles)
  n_runs_p2 = n_runs_p1 * n_elements(pct_tree_loss1_thresholds) * n_elements(pct_tree_loss20_thresholds) * $
    n_elements(pre_dist_cover_thresholds) * n_elements(pct_tree_gain_thresholds)
    
  lt_run_params = {index:'', $
    pval: 0.05, $
    max_segments:6, $
    recovery_threshold:0.25, $
    desawtooth_val:0.9, $
    distweightfactor:2, $
    vertexcountovershoot: 3, $
    bestmodelproportion: 0.75}
    
  p1_run_params = {dist_collapse_angle:0, recv_collapse_angle:0}
  p2_run_params = {pct_tree_loss1:0, pct_tre_loss20:0, pre_dist_cover:0, pct_tree_gain:0}
  
  lt_runidx = {runindex:0}
  p1_runidx = {runindex:0}
  p2_runidx = {runindex:0}
  
  run_count = 0l
  p1_run_count = 0l
  p2_run_count = 0l
  
  for pvalidx = 0, n_elements(pvals)-1 do begin
    for maxsegidx = 0, n_elements(maxsegments)-1 do begin
      for recvidx = 0, n_elements(recovery_thresholds)-1 do begin
        for desawidx = 0, n_elements(desawtooth_thresholds)-1 do begin
          for oververtidx = 0, n_elements(oververtex_thresholds)-1 do begin
            for distweightidx = 0, n_elements(distweight_thresholds)-1 do begin
              for bestmodelidx = 0, n_elements(bestmodel_thresholds)-1 do begin
              
                lt_run_params.index = index
                lt_run_params.pval = pvals[pvalidx]
                lt_run_params.max_segments = maxsegments[maxsegidx]
                lt_run_params.recovery_threshold = recovery_thresholds[recvidx]
                lt_run_params.desawtooth_val = desawtooth_thresholds[desawidx]
                lt_run_params.vertexcountovershoot = oververtex_thresholds[oververtidx]
                lt_run_params.distweightfactor = distweight_thresholds[distweightidx]
                lt_run_params.bestmodelproportion = bestmodel_thresholds[bestmodelidx]
                
                pval = lt_run_params.pval;
                max_segments = lt_run_params.max_segments
                recovery_threshold=lt_run_params.recovery_threshold
                desawtooth_val = lt_run_params.desawtooth_val
                distweightfactor = lt_run_params.distweightfactor
                vertexcountovershoot = lt_run_params.vertexcountovershoot
                bestmodelproportion = lt_run_params.bestmodelproportion
                
                ; landtrendr run
                ;CHECK: ok.best_model.vertvals = actual value * modifier
                ok=fit_trajectory_v2(x_axis, goods, outvals, $
                  minneeded, background_val, $
                  modifier, seed, desawtooth_val, pval, $
                  max_segments, recovery_threshold, $
                  distweightfactor, vertexcountovershoot, $
                  bestmodelproportion)
                  
                ;CHECK: fix vertvals from modification in fit_trajectory_v2
                newvv = ok.best_model.vertvals * modifier
                
                cvinfo = landtrendrv04_output_mapping(ok.best_model.vertices, newvv, point_name, point_coords, ok.best_model.f_stat, ok.best_model.p_of_f)
                saved_cvinfo = create_struct({runidx:run_count}, cvinfo)
                cur_lt_run_params = create_struct({runidx:run_count}, lt_run_params)
                
                if run_count eq 0 then begin
                  vertinfo = replicate(saved_cvinfo, n_runs)
                  saved_lt_run_params = replicate(cur_lt_run_params, n_runs)
                end
                vertinfo[run_count] = saved_cvinfo
                saved_lt_run_params[run_count] = cur_lt_run_params
                run_count = run_count + 1
                
                ;now do post-processing
                ;to mimic the way the vertices will be on reading
                ;   swap them by modifier
                ;vv = ok.best_model.vertvals * modifier
                
                for distanglidx = 0, n_elements(dist_collapse_angles)-1 do begin
                  for recvanglidx = 0, n_elements(recv_collapse_angles)-1 do begin
                  
                    p1_run_params.dist_collapse_angle = dist_collapse_angles[distanglidx]
                    p1_run_params.recv_collapse_angle = recv_collapse_angles[recvanglidx]
                    
                    ; apply post filtering - P1
                    out = collapse_segments(ok.best_model.vertices, ok.best_model.vertvals, modifier, $
                      dist_collapse_angles[distanglidx], $
                      recv_collapse_angles[recvanglidx])
                      
                    ;CHECK: fix vertvals from modification in fit_trajectory_v2
                    newvv = out.vertvals * modifier
                    
                    cvinfo_p1 = landtrendrv04_output_mapping(out.vertices, newvv, point_name, point_coords, ok.best_model.f_stat, ok.best_model.p_of_f)
                    ;saved_cvinfo_p1 = create_struct(lt_run_params, p1_run_params, cvinfo_p1)
                    saved_cvinfo_p1 = create_struct({runidx:p1_run_count}, cvinfo_p1)
                    cur_p1_run_params = create_struct({runidx:p1_run_count}, lt_run_params, p1_run_params)
                    
                    if p1_run_count eq 0 then begin
                      p1_vertinfo = replicate(saved_cvinfo_p1, n_runs_p1)
                      saved_p1_run_params = replicate(cur_p1_run_params, n_runs_p1)
                    end
                    
                    p1_vertinfo[p1_run_count] = saved_cvinfo_p1
                    saved_p1_run_params[p1_run_count] = cur_p1_run_params
                    p1_run_count = p1_run_count + 1
                    
                    
                    for pct_tree_loss1_idx = 0, n_elements(pct_tree_loss1_thresholds)-1 do begin
                      for pct_tree_loss20_idx = 0, n_elements(pct_tree_loss20_thresholds)-1 do begin
                        for pre_dist_cover_idx = 0, n_elements(pre_dist_cover_thresholds)-1 do begin
                          for pct_tree_gain_idx = 0, n_elements(pct_tree_gain_thresholds)-1 do begin
                          
                            p2_run_params.pct_tree_loss1 = pct_tree_loss1_thresholds[pct_tree_loss1_idx]
                            p2_run_params.pct_tre_loss20 = pct_tree_loss20_thresholds[pct_tree_loss20_idx]
                            p2_run_params.pre_dist_cover = pre_dist_cover_thresholds[pre_dist_cover_idx]
                            p2_run_params.pct_tree_gain = pct_tree_gain_thresholds[pct_tree_gain_idx]
                            
                            post_process_params.pct_tree_loss1 = p2_run_params.pct_tree_loss1
                            post_process_params.pct_tree_loss20 = p2_run_params.pct_tre_loss20
                            post_process_params.pre_dist_cover = p2_run_params.pre_dist_cover
                            post_process_params.pct_tree_gain = p2_run_params.pct_tree_gain
                            
                            ; apply post filter - p2
                            ;p2_out = landtrendrv04_filter_bycover(out.vertices, newvv, post_process_params)
                            p2_out = landtrendrv04_filter_bycover(out.vertices, newvv, post_process_params, /use_relative_mag)
                            cvinfo_p2 = landtrendrv04_output_mapping(p2_out.vertices, p2_out.vertvals, point_name, point_coords, ok.best_model.f_stat, ok.best_model.p_of_f)
                            
                            ;saved_cvinfo_p2 = create_struct(lt_run_params, p1_run_params, p2_run_params, cvinfo_p2)
                            saved_cvinfo_p2 = create_struct({runidx:p2_run_count}, cvinfo_p2)
                            cur_p2_run_params = create_struct({runidx:p2_run_count}, lt_run_params, p1_run_params, p2_run_params)
                            
                            if p2_run_count eq 0 then begin
                              p2_vertinfo = replicate(saved_cvinfo_p2, n_runs_p2)
                              saved_p2_run_params = replicate(cur_p2_run_params, n_runs_p2)
                            end
                            p2_vertinfo[p2_run_count] = saved_cvinfo_p2
                            saved_p2_run_params[p2_run_count] = cur_p2_run_params
                            p2_run_count = p2_run_count + 1
                            
                          end ; pct_tree_gain_thresholds
                        end ; pre_dist_cover_thresholds
                      end ; pct_tree_loss20_thresholds
                    end ; pct_tree_loss1_thresolds
                    
                  end ; dist collapse angle
                end ; recv collapse anagle
                
              end ;bestmodel_thresholds
            end ;distweight_thresholds
          end ;oververtex_thresholds
        end ;desawtooth_thresholds
      end ;recovery_thresholds
    end ;maxsegments
  end ;pvals
  
  
  
  ;      ;now do post-processing
  ;      ;to mimic the way the vertices will be on reading
  ;      ;   swap them by modifier
  ;      vv = ok.best_model.vertvals * modifier
  ;
  ;      out = collapse_segments(ok.best_model.vertices, $
  ;        vv, modifier, $
  ;        post_process_params.collapse_dist_angle, $
  ;        post_process_params.collapse_rec_angle)
  ;
  ;      newvertices = out.vertices
  ;      newvertvals = out.vertvals
  ;
  ;      ;assign disturbance image layer props
  ;      ;to get the values right for the model, need to account for divisor too.
  ;      ;TODO: check divisor vs modifier
  ;      nv = newvertvals * divisor
  ;      point_out = extract_dist_rec_segment(newvertices, nv, modifier, years)
  
  
  ;  return, {ok:1, best_model:modelstack, modifier:modifier, point_stack:pointstack}
  return, {ok:1, vertinfo:vertinfo, p1_vertinfo:p1_vertinfo, p2_vertinfo:p2_vertinfo, $
    lt_params:saved_lt_run_params, p1_params:saved_p1_run_params, p2_params:saved_p2_run_params}
end

