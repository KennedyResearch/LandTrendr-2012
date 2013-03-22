pro gdal_envi_hdr_fixer, path

  hdrs = file_search(path, "*.hdr", count = n_hdrs) ;find the hdr files
  
  if n_hdrs ge 1 then begin
    for i=0, n_elements(hdrs)-1 do begin
      ;open the hdrs
      openr, lun, hdrs[i], /get_lun
      file_size = file_lines(hdrs[i],/noexpand_path)
      file_list = strarr(file_size)
      readf, lun, file_list
      free_lun, lun
      
      fixed_lines = transpose(file_list)
      
      ;check on the units
      map_info = where(strpos(fixed_lines, "map info") eq 0, count)
      if count ge 1 then begin
        ;check if it has units
        units = strpos(fixed_lines[map_info], "units")
        if units eq -1 then begin
          length = strlen(fixed_lines[map_info])
          new_map_info = strmid(fixed_lines[map_info],0,length-1)+", units=Meters}"
          fixed_lines[map_info] = new_map_info
        endif
      endif
      
      ;check on the coordinate system string
      coordsline = where(strmatch(fixed_lines, "*coordinate system string*"),  complement=goods)
      fixed_lines = transpose(fixed_lines[goods])
           
      openw, lun, hdrs[i],/get_lun
      printf, lun, fixed_lines
      free_lun, lun
      
    endfor
  endif 
end
