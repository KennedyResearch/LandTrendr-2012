function parse_label_params, path, diag_info_savefile, label_parameters_txt, class_code_txt, eval=eval


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
  
  ;open the txt file
  ;if keyword_set(eval) eq 0 then begin
    openr, lun, class_code_txt, /get_lun
    file_size = file_lines(class_code_txt,/noexpand_path)
    code_list = strarr(file_size)
    readf, lun, code_list
    free_lun, lun
    codelistsplit = [""]
    for i=0, n_elements(code_list)-1 do begin
      pieces = strcompress(strsplit(code_list[i], ",", /extract), /rem)
      codelistsplit = [codelistsplit, pieces[0]]
    endfor
    code_list = codelistsplit[1:*]
    print
  ;endif else begin
    ;code_list = ['3#greatest_disturbance#Y#GDXX0000X00X00#XXXX0000X00X00','4#greatest_recovery#Y#GRXX0000X00X00#XXXX0000X00X00']
    ;extractem = 'no'
  ;endelse
  
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
    collapse_recv_angle: 15s,$
    run_name: '',$
    diag_file: '' ,$
    class_codes: code_list,$
    filter_params: {mock:'mock'},$
    merge_recovery: 'yes',$
    extract_tc_ftv: 'yes',$
    use_relative_mag: 'yes',$
    end_year: -1s,$
    start_year: -1s}
    
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
      if variable[j] eq 'run_name' then theruns[i].run_name = splitup[i]
      if variable[j] eq 'merge_recovery' then theruns[i].merge_recovery = splitup[i]
      if variable[j] eq 'extract_tc_ftv' then if keyword_set(eval) eq 1 then theruns[i].extract_tc_ftv = 'no' $
        else theruns[i].extract_tc_ftv = splitup[i]
      if variable[j] eq 'use_relative_mag' then theruns[i].use_relative_mag = splitup[i]
      if variable[j] eq 'end_year' then theruns[i].end_year = fix(splitup[i])
      if variable[j] eq 'start_year' then theruns[i].start_year = fix(splitup[i])
      theruns[i].diag_file = diag_info_savefile
      theruns[i].class_codes = code_list
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
    
  run_params = { $
    run_name: theruns.run_name ,$
    diag_file: theruns.diag_file ,$
    class_codes: theruns.class_codes ,$
    filter_params: filter_params ,$
    merge_recovery: theruns.merge_recovery ,$
    extract_tc_ftv: theruns.extract_tc_ftv ,$
    use_relative_mag: theruns.use_relative_mag ,$ 
    end_year: theruns.end_year ,$
    start_year: theruns.start_year}
    
  return, run_params
end