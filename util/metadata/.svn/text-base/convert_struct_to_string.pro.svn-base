;+
; NAME:
;   CONVERT_STRUCT_TO_STRING
;   
; PURPOSE:
;   concatenate structure variables to string representation
;
; PARAMS:
;   params, a structure. No nested structure is supported.
;-

function convert_struct_to_string, params
  ret = string(10b)
  tags = tag_names(params)
  for i = 0, n_tags(params)-1 do begin
    this_value = params.(i)
    if n_tags(this_value) gt 0 then continue ; ignore nested structure for now until needed
    ret = ret + tags[i] + ": " + string(this_value, format='('+string(n_elements(this_value))+'A)') + string(10b)
  endfor
  return, ret
end