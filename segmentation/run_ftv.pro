pro run_ftv, path, progressbaryesno, tc_bgw=tc_bgw, b543=b543, post_process_params=post_process_params

;------ identify path separator -------
  pse = path_sep()


  ;set the path to the outputs
 ; outputs = path+"outputs"+pse
 ;changing 3/28/13 rek to reflect more specific path name approach  -- the specific outputs path is passed to run_ftv now. 

    outputs = path

 print, 'Checking outputs directory for diag files: '+outputs
  
  ;figure out what indices to fit based on passed key words
  if keyword_set(tc_bgw) eq 1 and keyword_set(b543) eq 1 then doitfor = ['brightness','greenness','wetness','band5', 'band4', 'band3']
  if keyword_set(tc_bgw) eq 1 and keyword_set(b543) eq 0 then doitfor = ['brightness','greenness','wetness']
  if keyword_set(tc_bgw) eq 0 and keyword_set(b543) eq 1 then doitfor = ['band5', 'band4', 'band3']
  if keyword_set(tc_bgw) eq 0 and keyword_set(b543) eq 0 then message, "no indices were selected for fitting, check the keywords"
  
  ;find all of the diag files
  diag_files = file_search(outputs, "*diag.sav", count=n_diag_files)
  
 

  ;if some were found do stuff with it
  if n_diag_files ge 1 then begin
    ;find the ones that are from the initial segmentation
    nonftv = where(strmatch(diag_files, "*ftv*") ne 1, n_nonftv)
    if n_nonftv ge 1 then diag_files = diag_files[nonftv] else message, "there are no segmentation diag files to create ftv outputs from"
    
    ;create a structure for holding info
    for i=0, n_elements(diag_files)-1 do begin
      restore, diag_files[i]
      vertyrs = diag_info[0].output_image_group[0].filename[0]
      output_base = strmid(vertyrs,0, strpos(vertyrs, "_vertyrs.bsq"))
      
      for f=0, n_elements(doitfor)-1 do begin
        if n_elements(post_process_params) eq 0 then $
         ftv_run_params = {vertex_image_file:vertyrs, $
          apply_to_image_info:diag_info[0].image_info, $
          apply_to_index:doitfor[f], $
          background_val:diag_info[0].background_val, $
          divisor:diag_info[0].divisor, $
          subset:diag_info[0].image_info[0].subset, $
          mask_image:diag_info[0].mask_image, $
          output_base:output_base, $
          kernelsize:diag_info[0].kernelsize, $
          fix_doy_effect:diag_info[0].fix_doy_effect, $
          max_segments:diag_info[0].max_segments, $
          skipfactor:diag_info[0].skipfactor, $
          desawtooth_val:diag_info[0].desawtooth_val} else $  ;if we give it post-roces params
          ftv_run_params = {vertex_image_file:vertyrs, $
          apply_to_image_info:diag_info[0].image_info, $
          apply_to_index:doitfor[f], $
          background_val:diag_info[0].background_val, $
          divisor:diag_info[0].divisor, $
          subset:diag_info[0].image_info[0].subset, $
          mask_image:diag_info[0].mask_image, $
          output_base:output_base, $
          kernelsize:diag_info[0].kernelsize, $
          fix_doy_effect:diag_info[0].fix_doy_effect, $
          max_segments:diag_info[0].max_segments, $
          skipfactor:diag_info[0].skipfactor, $
          desawtooth_val:diag_info[0].desawtooth_val, $
          post_process_params:post_process_params}
          
          
          
          
          
          ;check if this one has been created already
;          searchfor = strcompress(file_basename(output_base)+"*"+doitfor[f]+"*diag.sav", /rem)
 ;         already_done = file_search(outputs, searchfor, count=n_already_done)
  ;        if n_already_done ge 1 then begin
   ;         print, ""
    ;        print, ">>> FTV has already been run for: ", doitfor[f]
     ;       print, ">>> based on this diag file: ", diag_files[i]
      ;      print, ">>> skipping"
       ;     print, ""
        ;    continue
         ; endif
          print, ">>> fitting to vertices: ", doitfor[f]
          ok = process_ftv_chunks(ftv_run_params, progressbaryesno, /interpolate)
          if ok.ok eq 0 then stop
      endfor
    endfor
  endif else print, 'No diag files found'
end
