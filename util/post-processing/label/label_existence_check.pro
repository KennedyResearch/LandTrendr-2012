function label_existence_check, path, diag_file, label_params
;------ identify path separator -------
  pse = path_sep()

  label_codes = label_params.class_codes  ;subset the label codes
  n_label_codes = n_elements(label_codes)
  label_struct = {label:"",base:"",exist:0}
  label_struct = replicate(label_struct,n_label_codes)
  
  for i=0, n_label_codes-1 do begin ;loop through separating the label name out
    code_split = strsplit(label_codes[i], "#", /extract) ;split up the label code
    label_single = code_split[1] ;pull out the label name
    label_struct[i].label = label_single ;put the names into a list
  endfor
  
  ;create the base names
  base = file_basename(diag_file)
  index = strsplit(base, "_")
  index = index[7]
  base = strmid(base,0,index)
  label_struct.base = base+label_struct.label+".bsq"

  ;check for base name existence
  for i=0, n_label_codes-1 do begin
    finds = file_search(path+"outputs"+pse,"*"+label_params.run_name+pse+label_struct[i].base, count=n_finds)
    if n_finds eq 0 then label_struct[i].exist = 0 else label_struct[i].exist = 1
  endfor
  
  ;subset the non-matches and set ok variable to 0 or 1 depending on whether there are any labels still needing outputs
 match = where(label_struct.exist eq 0, n_match)
  if n_match ge 1 then begin 
    updated_label_codes = label_codes[match]
    ok = 1
  endif else begin
    updated_label_codes = ""
    ok = 0
  endelse
  
  ;repackage the label_params file
  updated_label_params = {class_codes:updated_label_codes, diag_file:label_params.diag_file, $
    end_year:label_params.end_year, extract_tc_ftv:label_params.extract_tc_ftv, $
    filter_params:label_params.filter_params, merge_recovery:label_params.merge_recovery, $
    run_name:label_params.run_name, start_year:label_params.start_year, use_relative_mag:label_params.use_relative_mag}
  
  ;return the info to landtrendr_processor
  return, {updated_label_params:updated_label_params, ok:ok}
end
