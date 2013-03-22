		
pro run_lt_label_and_filtering, diag_files, label_parameters_txt, class_code_txt, templatehdr
  
  if n_elements(diag_files) ne n_elements(label_parameters_txt) then message, "the number of diag_files must correspond and match the number of 'label_parameters_txt' files" 
    
  ;create labled outputs
  print, ">>> Starting segment labeling"
  t1 = systime(1)
  for i=0, n_elements(diag_files)-1 do begin
    testit = strmatch(diag_files[i], "*outputs*")
    if testit eq 1 then begin
      pos = strpos(diag_files[i], "outputs")
      path = strmid(diag_files[i],0,pos)
    endif else message, "trying to find path to outputs, but 'outputs' does not exist in this diag_file filename: "+diag_files[i]
    label_params = parse_label_params(path, diag_files[i], label_parameters_txt[i], class_code_txt, eval=eval)
   
    checkit = label_existence_check(path, diag_files[i], label_params)
    if checkit.ok eq 0 then continue
    ok = lt_label(checkit.updated_label_params)
    if ok.ok eq 0 then message, ok.message
    convert_bsq_headers_to_envi, path, templatehdr, overwrite=overwrite_hdr
  endfor
  t2 = systime(1)
  time = float((t2-t1)/60)
  print, ">>> segment labeling took: ", time," minutes"
  
  
  ;#################################################################################################################
  ;filter labeled outputs
  print, ">>> Starting patch aggregation and filtering"
  t1 = systime(1)
  for i=0, n_elements(diag_files)-1 do begin
    testit = strmatch(diag_files[i], "*outputs*")
    if testit eq 1 then begin
      pos = strpos(diag_files[i], "outputs")
      path = strmid(diag_files[i],0,pos)
    endif else message, "trying to find path to outputs, but 'outputs' does not exist in this diag_file filename: "+diag_files[i]
    run_label_class_filter, class_code_txt, label_parameters_txt[i], diag_files[i]
    convert_bsq_headers_to_envi, path, templatehdr, overwrite=overwrite_hdr
  endfor
  t2 = systime(1)
  time = float((t2-t1)/60)
  print, ">>> patch aggregation and filtering took: ", time," minutes"
  
end
