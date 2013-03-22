pro from_envi_hdr_fixer, path

  image_dir = strcompress(path+"images\", /rem) ;create the path to the images directory
  study_area_dir =  strcompress(path+"study_area\", /rem)
  
  hdrs = [file_search(image_dir, "*.hdr"), file_search(study_area_dir, "*.hdr")] ;find the hdr files
  print, "fixing hdrs..."
  
  ;go through all the hdrs
  for i=0, n_elements(hdrs)-1 do begin
    if hdrs[i] eq "" then goto, start_here
    ;open the hdrs
    openr, lun, hdrs[i], /get_lun
    file_size = file_lines(hdrs[i],/noexpand_path)
    file_list = strarr(file_size)
    readf, lun, file_list
    free_lun, lun
    
    ;fix hdrs that reference the ul of ul cell instead of center
    if strmid(file_list[0], 0, 5) ne 'BANDS' then begin
      map_info_line_element = where(strmid(file_list, 0, 8) eq 'map info')
      map_info_line = file_list[map_info_line_element] ;get the map info line        ;file_list[12]
      map_info_pieces = strsplit(map_info_line, ',', /extract) ;split up the ,ap info line
      check = double(strcompress(map_info_pieces[1], /rem))
      if map_info_pieces[1] lt 1.5 then begin
        map_info_pieces[1] = '1.5000' ;replace the 1.0000 (ul) with 1.500 (center) for x
        map_info_pieces[2] = '1.5000' ;replace the 1.0000 (ul) with 1.500 (center) for y
        x = double(map_info_pieces[3]) ;convert the string value to a float so we can add to it
        y = double(map_info_pieces[4]) ;convert the string value to a float so we can subtract from it
        if map_info_pieces[5] eq ' 3.0000000000e+001' or map_info_pieces[5] eq ' 30' then begin ;check that the cell size is 30
          xadjust = double(15.0)
          yadjust = double(15.0)
          xfix = x+double(15) ;adjust the ulx tie to the center
          yfix = y-double(15) ;adjust the uly tie to the center
        endif else message, "cell size must be 30m to be fixed"
        xstring = string(xfix, format='(F0.4)') ;format the value to correct string
        ystring = string(yfix, format='(F0.4)') ;format the value to correct string
        map_info_pieces[3] = xstring ;put the fixed x into the map info line
        map_info_pieces[4] = ystring ;put the fixed y into the map info line
        map_info_fixed = strcompress(strjoin(map_info_pieces, ", ")) ;put the map info line back together
        file_list[map_info_line_element] = map_info_fixed ;put the fixed map info line into the hdr
        
        from_envi_hdr = strcompress(hdrs[i]+".from_envi", /rem)
        file_copy, hdrs[i], from_envi_hdr, /overwrite
        ;write out the fixed hdr
        file_list = transpose(file_list)
        openw, lun, hdrs[i],/get_lun
        printf, lun, file_list
        free_lun, lun
      endif ;less than 1.5
    endif ;bands
    start_here: ;if hdrs[i] eq ""
  endfor
end