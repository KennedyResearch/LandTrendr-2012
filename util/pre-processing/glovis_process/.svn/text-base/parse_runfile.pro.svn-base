function parse_runfile, file

  ;open the label_parameters_txt file
  openr, lun, file, /get_lun
  file_size = file_lines(file,/noexpand_path)
  file_list = strarr(file_size)
  readf, lun, file_list
  free_lun, lun
  
  structure = create_struct('starter',"")
  for j=0, n_elements(file_list)-1 do begin
    if file_list[j] eq '' then continue
    split = strsplit(file_list[j], '=', /extract)
    case strtrim(split[0],2) of
      'proj_ref_file': structure = create_struct(structure, 'proj_ref_file', strtrim(split[1],2))
      'norm_method': structure = create_struct(structure, 'norm_method', fix(strtrim(split[1],2)))
      'madcal_ref_img': structure = create_struct(structure, 'madcal_ref_img', strtrim(split[1],2))
      'input_dem': structure = create_struct(structure, 'input_dem', strtrim(split[1],2))
      'nlcdimg': structure = create_struct(structure, 'nlcdimg', strtrim(split[1],2))
      'water_off': structure = create_struct(structure, 'water_off', fix(strtrim(split[1],2)))
      'snow_off': structure = create_struct(structure, 'snow_off', fix(strtrim(split[1],2)))
      'delete_files': structure = create_struct(structure, 'delete_files', fix(strtrim(split[1],2)))
      'modis_img_path': structure = create_struct(structure, 'modis_img_path', strtrim(split[1],2))
      'target_day': structure = create_struct(structure, 'target_day', fix(strtrim(split[1],2)))
      'target_year': structure = create_struct(structure, 'target_year', fix(strtrim(split[1],2)))
      'segparamstxt': structure = create_struct(structure, 'segparamstxt', strtrim(split[1],2))
      'label_parameters_txt': structure = create_struct(structure, 'label_parameters_txt', strtrim(split[1],2))
      'class_code_txt': structure = create_struct(structure, 'class_code_txt', strtrim(split[1],2))
      'dark_object_vals': structure = create_struct(structure, 'dark_object_vals', strtrim(split[1],2))
      ;'ls_madcal_ref_img': structure = create_struct(structure, 'ls_madcal_ref_img', strtrim(split[1],2))
    endcase
  endfor
  return, structure
end