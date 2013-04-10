function extract_post_process_params,  label_parameters_txt

  ;strictly for use with detremnidng FTV jan 2013
  ;
  ;###################################################

  ;warning!!! can only accept one set of params as of 11/29/11
  ;structure is nearly there to allow multiples

  ;###################################################

  ;open the label_parameters_txt file
  openr, lun, label_parameters_txt, /get_lun
  file_size = file_lines(label_parameters_txt,/noexpand_path)
  file_list = strarr(file_size)
  readf, lun, file_list
  free_lun, lun
  
  ;make holders for the variables and values
  variable = strarr(n_elements(file_list))
  value = strarr(n_elements(file_list))
  
  
  
  ;make holders for the variables and values
  variable = strarr(n_elements(where(file_list ne "") eq 1))
  value = variable
  
  ;make a run params structure for each set passed
  theruns = {static_model: '',$
    change_model: '',$
    pct_tree_loss1: 10s,$
    pct_tree_loss20: 3s,$
    pre_dist_cover: 20s,$
    pct_tree_gain: 5s,$
    collapse_dist_angle: 15s,$
    collapse_recv_angle: 15s}
    
  firsttime = 1 ;set first time to true
  for j=0, n_elements(file_list)-1 do begin
    if file_list[j] eq '' then continue
    split = strsplit(file_list[j], '=', /extract)
    variable[j] = strtrim(split[0],2)
    value[j] = strtrim(split[1],2)
    if firsttime eq 1 then begin
      n_runs = n_elements(strsplit(value[j], ',', /extract))
      theruns = replicate(theruns, n_runs)
      firsttime = 0 ;set first time to false
    endif
    
    ;go through each set and package it for processing
    splitup = strtrim(strsplit(value[j], ',', /extract),2)
    if n_elements(splitup) ne n_runs then begin
      print, ">>> parsing out the segmentation parameter file..."
      print, ">>> checking to see if all variables have the same..."
      print, ">>> number of entries and it appears as though one or more..."
      print, ">>> variables does not have the same number of entries as..."
      print, ">>> the variable 'run_name' - please fix this and rerun"
      print, ""
      print, ">>> ending program"
      print, ""
      stop
    endif
    
    ;fill in the variables with their values
    for i=0, n_runs-1 do begin
      if variable[j] eq 'static_model' then theruns[i].static_model = splitup[i]
      if variable[j] eq 'change_model' then theruns[i].change_model = splitup[i]
      if variable[j] eq 'pct_tree_loss1' then theruns[i].pct_tree_loss1 = fix(splitup[i])
      if variable[j] eq 'pct_tree_loss20' then theruns[i].pct_tree_loss20 = fix(splitup[i])
      if variable[j] eq 'pre_dist_cover' then theruns[i].pre_dist_cover = fix(splitup[i])
      if variable[j] eq 'pct_tree_gain' then theruns[i].pct_tree_gain = fix(splitup[i])
      if variable[j] eq 'collapse_dist_angle' then theruns[i].collapse_dist_angle = fix(splitup[i])
      if variable[j] eq 'collapse_recv_angle' then theruns[i].collapse_recv_angle = fix(splitup[i])
     
    endfor
  endfor
  
  ;---group run parameters---
  filter_params = { $
    static_model: theruns.static_model ,$
    change_model: theruns.change_model ,$
    pct_tree_loss1: theruns.pct_tree_loss1 ,$
    pct_tree_loss20: theruns.pct_tree_loss20 ,$
    pre_dist_cover: theruns.pre_dist_cover ,$
    pct_tree_gain: theruns.pct_tree_gain ,$
    collapse_dist_angle: theruns.collapse_dist_angle ,$
    collapse_recv_angle: theruns.collapse_recv_angle}
    
  
  return, filter_params
end