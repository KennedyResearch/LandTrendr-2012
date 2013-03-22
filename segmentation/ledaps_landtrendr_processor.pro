pro ledaps_landtrendr_processor, run_params

  ;---unpack the structure---
  ppprrr=run_params.ppprrr
  path=run_params.path
  segmentation_eval=run_params.segmentation_eval
  segparamstxt=run_params.segparamstxt
  segmentation=run_params.segmentation
  useareafile=run_params.useareafile
  resume=run_params.resume
  run_ftv_doit=run_params.fit_to_vertices
  dist_rec_snapshots=run_params.dist_rec_snapshots
  dark_seg_outputs=run_params.dark_seg_outputs
  image_list=run_params.image_list
  image_list_type=run_params.image_list_type
  templatehdr=run_params.template_hdr
  progressbaryesno=run_params.progressbaryesno
  if segmentation_eval eq 1 then begin 
  	label_parameters_txt=run_params.label_parameters_txt
  	class_code_txt=run_params.class_code_txt
 end
 
;test if post_process params are there

  tn = tag_names(run_params)
 if total(strmatch(tn, strupcase('post_process_file'))) eq 0 then post_process_file = 'none' else $
     post_process_file = run_params.post_process_file
   
  ;------ identify path separator -------
  pse = path_sep()


  image_info_savefile = path+'images'+pse+'landtrendr_image_info_'+ppprrr+'.sav'
  create_image_info, path, image_info_savefile, useareafile=useareafile
  repopulate_image_info, image_info_savefile
  
  ;templatehdr = file_search(path+"images"+pse, "*ledaps.hdr")
  ;templatehdr = templatehdr[0]
  
  addyears = segmentation_eval + segmentation + run_ftv_doit  
  
  eval = 0 ;set the default value of evaluation segmentation to 0
  
  if keyword_set(label_segs) eq 0 then label_segs = 0 else label_segs = 1
  if keyword_set(segmentation) eq 0 then segmentation = 0 else segmentation = 1
  if keyword_set(segmentation_eval) eq 1 then begin
    eval=1
    segmentation = 1
    label_segs = 1
  endif
  
  

  check_image_info_prior_to_seg, image_info_savefile, /ledaps
  make_metadata_for_ledaps, path
  
  
  ;#################################################################################################################
  ;run_segmentation
  if segmentation eq 1 then begin
    if file_exists(segparamstxt) eq 0 then begin
      ;try to find the parameter file
      segparamstxt = file_search(path, "*segmentation_parameters.txt")
    endif
    ;make sure that all of the preprocessing is complete
    if image_list ne 0 then adj_img_info, image_list, image_list_type, image_info_savefile
    params = parse_seg_params(path, ppprrr, segparamstxt, image_info_savefile, mask_image=useareafile, subset=subset, eval=eval, resume=resume)
    
    ;if this is the non-eval run - find the eval files and delete them
    if keyword_set(eval) ne 1 then begin
      eval_files = file_search(file_dirname(params[0].output_base), "*eval*", count=n_eval_files)
      if n_eval_files ge 1 then file_delete, eval_files
    endif
    
    if keyword_set(eval) eq 1 then n_runs = 1 else n_runs = n_elements(params)
    
    for i=0, n_runs-1 do begin
      print, ">>> Starting Segmentation", i+1
      t1 = systime(1)

      ok= process_tbcd_chunks(params[i], progressbaryesno)
      end_time = systime()
      print, '>>> Done With Segmentation', i+1
      t2 = systime(1)
      time = float((t2-t1)/60)
      print, ">>> segmentation ", i+1, " took: ", time," minutes"
    endfor
    convert_bsq_headers_to_envi, path, templatehdr, overwrite=overwrite_hdr
  endif
  
  ;#################################################################################################################
  ;create ftv outputs
  if keyword_set(run_ftv_doit) eq 1 then begin
    print, ">>> Starting fit to vertices"
    t1 = systime(1)
    if run_ftv_doit eq 1 or run_ftv_doit eq 3 then tc_bgw = 1 else tc_bgw = 0
    if run_ftv_doit eq 2 or run_ftv_doit eq 3 then b543 = 1 else b543 = 0
    
    ;adding in steps for detrending with percent cover models
    if post_process_file eq 'none' then $
     run_ftv, path, progressbaryesno, tc_bgw=tc_bgw, b543=b543 else $
     begin 
      post_process_params = extract_post_process_params(post_process_file)
      run_ftv,path, progressbaryesno, tc_bgw=tc_bgw, b543=b543, post_process_params=post_process_params 
     end
     
    convert_bsq_headers_to_envi, path, templatehdr, overwrite=overwrite_hdr
    t2 = systime(1)
    time = float((t2-t1)/60)
    print, ">>> fit to vertices took: ", time," minutes"
  endif
  
  ;#################################################################################################################
  ;add year to fitted outputs headers
  if addyears ge 1 then begin
    print, ">>> Starting add years to fitted outputs"
    t1 = systime(1)
    convert_bsq_headers_to_envi, path, templatehdr, overwrite=overwrite_hdr
    outputs_path = path+"outputs"+pse
    diag_file = file_search(outputs_path, "*diag.sav", count=n_diag_file)
    ;find the non ftvs
    nonftv_index = where(strmatch(diag_file, "*ftv*") ne 1, n_nonftv_index, complement=ftv_index, ncomplement=n_ftv_index)
    if n_nonftv_index ge 1 then begin
      dothese = diag_file[nonftv_index]
      for i=0, n_nonftv_index-1 do add_years_to_fittedimages, outputs_path, dothese[i]
    endif
    if n_ftv_index ge 1 then begin
      dothese = diag_file[ftv_index]
      for i=0, n_ftv_index-1 do add_years_to_fittedimages, outputs_path, dothese[i], /from_ftv
    endif
    t2 = systime(1)
    time = float((t2-t1)/60)
    print, ">>> add years to fitted outputs took: ", time," minutes"
  endif
  
 ;#################################################################################################################
  ;create labled outputs
  if label_segs eq 1 then begin
    print, ">>> Starting segment labeling"
    t1 = systime(1)
    outputs_path = path+"outputs"+pse
    diag_file = file_search(outputs_path, "*diag.sav", count=n_diag_file)
    ;find the non ftvs
    nonftv_index = where(strmatch(diag_file, "*ftv*") ne 1, n_nonftv_index, complement=ftv_index, ncomplement=n_ftv_index)
    if n_nonftv_index ge 1 then begin
      dothese = diag_file[nonftv_index]
      for i=0, n_nonftv_index-1 do begin
        label_params = parse_label_params(path, dothese[i], label_parameters_txt, class_code_txt, eval=eval)
        checkit = label_existence_check(path, dothese[i], label_params)
        if checkit.ok eq 0 then continue 
        ok = lt_label(checkit.updated_label_params)
      endfor
    endif
    convert_bsq_headers_to_envi, path, templatehdr, overwrite=overwrite_hdr
    t2 = systime(1)
    time = float((t2-t1)/60)
    print, ">>> segment labeling took: ", time," minutes"
  endif
  

    ;#################################################################################################################
  ;create disturbance and recovery layers
  if keyword_set(dist_rec_snapshots) eq 1 then begin
    outputs_path = path+"outputs"+pse
    diag_file = file_search(outputs_path, "*diag.sav", count=n_diag_file)
    ;find the non ftvs
    nonftv_index = where(strmatch(diag_file, "*ftv*") ne 1, n_nonftv_index, complement=ftv_index, ncomplement=n_ftv_index)
    if n_nonftv_index ge 1 then begin
      dothese = diag_file[nonftv_index]
      for i=0, n_nonftv_index-1 do begin
        t1 = systime(1)
        ok = create_dist_rec_snapshots(path, dothese[i])
        t2 = systime(1)
        time = float((t2-t1)/60)
        print, ">>> dist_rec_snapshot ", i+1, " took: ", time," minutes"
      endfor
      convert_bsq_headers_to_envi, path, templatehdr, overwrite=overwrite_hdr
    endif
  endif
  
  ;#################################################################################################################
  ;create dark seg outputs
  if keyword_set(dark_seg_outputs) eq 1 then begin
    outputs_path = path+"outputs"+pse
    diag_file = file_search(outputs_path, "*diag.sav", count=n_diag_file)
    ;find the non ftvs
    nonftv_index = where(strmatch(diag_file, "*ftv*") ne 1, n_nonftv_index, complement=ftv_index, ncomplement=n_ftv_index)
    if n_nonftv_index ge 1 then begin
      dothese = diag_file[nonftv_index]
      for i=0, n_nonftv_index-1 do begin
        t1 = systime(1)
        ok = make_forest_mask_input_image(dothese[i]) 
        t2 = systime(1)
        time = float((t2-t1)/60)
        print, ">>> dark_seg_outputs ", i+1, " took: ", time," minutes"
      endfor
      convert_bsq_headers_to_envi, path, templatehdr, overwrite=overwrite_hdr
    endif
  endif
 
  ;#################################################################################################################

  
end  
