pro check_image_info_prior_to_seg, image_info_savefile, ledaps=ledaps

  restore, image_info_savefile
  n_images = n_elements(image_info)
  bad = 0
  
  if keyword_set(ledaps) eq 1 then begin
    checkit = {cloud_file:1 ,$
      tc_file:1 ,$
      year: 2011 ,$
      day: 300}
      
    checkit = replicate(checkit, n_images)
    
    for i=0, n_images-1 do begin
      ;check for cloudmask
      if file_test(image_info[i].cloud_file) eq 0 then checkit[i].cloud_file = 0
      ;check for a tc image
      if file_test(image_info[i].tc_file) eq 0 then checkit[i].tc_file = 0

      ;assign the julday and year so it can be reported correctly if there is a miss
      checkit[i].year = image_info[i].year
      checkit[i].day = image_info[i].julday


    endfor
    
    ;print missing cloudmasks
    for i=0, n_images-1 do begin
      if checkit[i].cloud_file eq 0 then begin
        yearday = strcompress(string(checkit[i].year)+"_"+string(checkit[i].day)+": ", /rem)
        print, ""
        print, ">>> !!!warning!!! image date: ",yearday
        print, ">>> does not have a cloudmask, please fix this and rerun segmentation"
        print, ""
        bad = bad+1
      endif
    endfor
    
    ;print missing tc images
    for i=0, n_images-1 do begin
      if checkit[i].tc_file eq 0 then begin
        yearday = strcompress(string(checkit[i].year)+"_"+string(checkit[i].day)+": ", /rem)
        print, ""
        print, ">>> !!!warning!!! image date: ",yearday
        print, ">>> does not have tasseled cap, please fix this and rerun segmentation"
        print, ""
        bad = bad+1
      endif
    endfor
  endif else begin
  

  
  
  
    checkit = {normimg:1 ,$
      cloud_file:1 ,$
      tc_file:1 ,$
      refimg:'na' ,$
      year: 2011 ,$
      day: 300}
      
    checkit = replicate(checkit, n_images)
    
    for i=0, n_images-1 do begin
      checkit[i].year = image_info[i].year
      checkit[i].day = image_info[i].julday
      ;check for madcal
      madcal = image_info[i].image_file
      good = (strmatch(madcal, "*_to_*")) + (strmatch(madcal, "*radref*"))
      if good eq 0 then begin
        checkit[i].normimg = 0
        checkit[i].refimg = 'na'
      endif else begin
        ;check that all dates have the same reference image
        meta = stringswap(madcal, ".bsq", "_meta.txt")
        if file_test(meta) eq 1 then begin
          if strmatch(madcal, "*radref*") eq 1 then checkit[i].refimg = file_basename(madcal) else begin
            openr, lun, meta, /get_lun
            file_size = file_lines(meta,/noexpand_path)
            file_list = strarr(file_size)
            readf, lun, file_list
            free_lun, lun
            for n=0, n_elements(file_list)-1 do begin
              theline = strmatch(file_list[n], "*RADIOMETRIC_REFERENCE_IMAGE*")
              if theline eq 1 then begin
                split = strsplit(file_list[n], ":", /extract)
                checkit[i].refimg = strcompress(split[1], /rem)
              endif
            endfor
          endelse
        endif else begin
          yearday = strcompress(string(checkit[i].year)+"_"+string(checkit[i].day)+": ", /rem)
          print, ""
          print, ">>> !!!warning!!! there is nometadata for this radref or normalized image date: "
          print, yearday
          print, "fix this and rerun segmentation
          print, ""
        endelse
      endelse
      ;check for cloudmask
      if file_test(image_info[i].cloud_file) eq 0 then checkit[i].cloud_file = 0
      ;check for a tc image
      if file_test(image_info[i].tc_file) eq 0 then checkit[i].tc_file = 0
    endfor
    
    ;print missing madcals
    for i=0, n_images-1 do begin
      if checkit[i].normimg eq 0 then begin
        yearday = strcompress(string(checkit[i].year)+"_"+string(checkit[i].day)+": ", /rem)
        print, ""
        print, ">>> !!!warning!!! image date: ",yearday
        print, ">>> has not been normalized, please fix this and rerun segmentation"
        print, ""
        bad = bad+1
      endif
    endfor
    
    ;print missing cloudmasks
    for i=0, n_images-1 do begin
      if checkit[i].cloud_file eq 0 then begin
        yearday = strcompress(string(checkit[i].year)+"_"+string(checkit[i].day)+": ", /rem)
        print, ""
        print, ">>> !!!warning!!! image date: ",yearday
        print, ">>> does not have a cloudmask, please fix this and rerun segmentation"
        print, ""
        bad = bad+1
      endif
    endfor
    
    ;print missing tc images
    for i=0, n_images-1 do begin
      if checkit[i].tc_file eq 0 then begin
        yearday = strcompress(string(checkit[i].year)+"_"+string(checkit[i].day)+": ", /rem)
        print, ""
        print, ">>> !!!warning!!! image date: ",yearday
        print, ">>> does not have tasseled cap, please fix this and rerun segmentation"
        print, ""
        bad = bad+1
      endif
    endfor
    
    ;print different rad refs
    unique = uniq(checkit.refimg)
    if n_elements(unique) gt 1 then begin
      yearday = strcompress(string(checkit.year)+"_"+string(checkit.day)+":", /rem)
      print, ""
      print, ">>> !!!warning!!! not all images dates used the same reference image"
      print, ">>> here is the list, fix these so that they all use the same reference image"
      print, ""
      print, transpose(yearday+checkit.refimg)
      print, ""
      bad = bad+1
    endif
    
  endelse
  if bad gt 0 then stop
  
end
