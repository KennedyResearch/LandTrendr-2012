;+
; NAME:
;   CONCATENATE_METADATA
;
; PURPOSE:
;   Concatenate metadata for each input files into one metadata file.
;
; PARAMETERS:
;   inputs: an array of input metadata files to be concatenated
;   output_metadata_file: combined output files
;
; TODO:
;   * error checking on file existence
;   * format other than .bsq handling
;
;-
pro concatenate_metadata, inputs, output_metadata_file, params=params
;------ identify path separator -------
  pse = path_sep()


  command_orig = "copy /Y "
  connector = "+"
  outpipe = " "
  
  if !Version.(1) ne "Win32" then begin
    command_orig = "cat "
    connector = " "
    outpipe = " > "
  endif
  
  ;check to see if parameters is set
  if keyword_set(params) then begin
    if n_tags(params) le 0 then message, "Please specify params as a structure"
  endif
  
  
  for i = 0, n_elements(inputs)-1 do begin
    this_meta = stringswap(inputs[i], ".bsq", "_meta.txt")
    if file_test(this_meta) eq 0 then message, this_meta + " does not exists!"
    if i gt 0 then command1 = command1 + connector + this_meta else command1 = command_orig + this_meta
  endfor
  commanddoit = command1 + outpipe + output_metadata_file
  
  ;check to see if this metadata file needs to be split up to get through the command prompt
  length = strlen(commanddoit)
  if length ge 8000 then begin
    half1 = round(n_elements(inputs)/2)
    half2 = n_elements(inputs)-half1
    base = file_dirname(output_metadata_file)
    out1 = base+pse+"temp1.txt"
    out2 = base+pse+"temp2.txt"
    
    for i = 0, half1-1 do begin
      this_meta = stringswap(inputs[i], ".bsq", "_meta.txt")
      if file_test(this_meta) eq 0 then message, this_meta + " does not exists!"
      if i gt 0 then command2 = command2 + connector + this_meta else command2 = command_orig + this_meta
    endfor
    commanddoit = command2 + outpipe + out1
    if !Version.(1) ne "Win32" then spawn, commanddoit else spawn, commanddoit, /hide
    
    for i = half1, n_elements(inputs)-1 do begin
      this_meta = stringswap(inputs[i], ".bsq", "_meta.txt")
      if file_test(this_meta) eq 0 then message, this_meta + " does not exists!"
      if i gt half1 then command3 = command3 + connector + this_meta else command3 = command_orig + this_meta
    endfor
    commanddoit = command3 + outpipe + out2
    if !Version.(1) ne "Win32" then spawn, commanddoit else spawn, commanddoit, /hide
    
    commanddoit = command_orig + out1 + connector + out2 + outpipe + output_metadata_file  
    if !Version.(1) ne "Win32" then spawn, commanddoit else spawn, commanddoit, /hide
    
    if file_test(output_metadata_file) eq 1 then file_delete, out1, out2
  
  endif else if !Version.(1) ne "Win32" then spawn, commanddoit else spawn, commanddoit, /hide
  
  if n_elements(params) ne 0 then begin
  openu, fun, output_metadata_file, /append, /get_lun
  printf, fun, convert_struct_to_string(params)
  free_lun, fun
  end
end
