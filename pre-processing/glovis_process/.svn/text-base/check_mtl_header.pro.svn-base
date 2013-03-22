pro check_mtl_header, path, ppprrr, update=update

  ;set the search path - paths differ depending on whether it is an update or not
  if keyword_set(update) eq 1 then begin
    vct_outputs = path+"VCT\outputs\update\"
    glovis_targz = path+"glovis_targz\update\"
  endif else begin
    vct_outputs = path+"VCT\outputs\"
    glovis_targz = path+"glovis_targz\"
  endelse
  
  ;find the metadata files
  mtl_files = file_search(vct_outputs, "*MTL.txt*", count=n_mtl_files)
  
  ;create structure to hold the image metadata
  ls_metadata = {ppprrr:"ppprrr",$
    year:'"na"',$
    month:'"na"',$
    day:'"na"',$
    julian_day:'"na"',$
    product_type:'"na"',$
    elevation_source:'"na"',$
    processing_software:'"na"',$
    ephemeris_type:'"na"',$
    sensor_id:'"na"',$
    sensor_mode:'"na"',$
    striping_band1:'"na"',$
    striping_band2:'"na"',$
    striping_band3:'"na"',$
    striping_band4:'"na"',$
    striping_band5:'"na"',$
    striping_band6:'"na"',$
    striping_band7:'"na"',$
    banding:'"na"',$
    coherent_noise:'"na"',$
    memory_effect:'"na"',$
    scan_correlated_shift:'"na"',$
    inoperable_detectors:'"na"',$
    dropped_lines:'"na"'}
    
  ;replicate the structure for as many MTL files that were found
  ls_metadata = replicate(ls_metadata, n_elements(mtl_files))
  
  ;create a list of infomation to find from the metadata
  find_these = ["*product_type*","*elevation_source*","*processing_software*","*ephemeris_type*",$
    "*sensor_id*","*sensor_mode*","*striping_band1*","*striping_band2*","*striping_band3*",$
    "*striping_band4*","*striping_band5*","*striping_band6*","*striping_band7*","*banding*",$
    "*coherent_noise*","*memory_effect*","*scan_correlated_shift*","*inoperable_detectors*",$
    "*dropped_lines*"]
    
  if n_mtl_files ge 1 then begin
    for i=0, n_mtl_files-1 do begin
      print, "checking mtl metadata for: ",mtl_files[i]
      
      ;convert the mtl file date to julian date
      mtl_basename = file_basename(mtl_files[i])
      etmsensor = strmatch(mtl_basename, "*L71*")
      year = strmid(mtl_basename,10,4)
      month = strmid(mtl_basename,14,2)
      day = strmid(mtl_basename,16,2)
      julday = strtrim(string(ymd2dn(year,month,day)),2)
      
      ;fill the structure with year and julian day
      ls_metadata[i].ppprrr = ppprrr
      ls_metadata[i].year = year
      ls_metadata[i].month = month
      ls_metadata[i].day = day
      ls_metadata[i].julian_day = julday
      
      ;read in the mtl header
      openr, lun, mtl_files[i], /get_lun
      file_size = file_lines(mtl_files[i],/noexpand_path)
      file_list = strarr(file_size)
      readf, lun, file_list
      free_lun, lun
      
      for f=0, n_elements(find_these)-1 do begin
        if f eq 11 and etmsensor eq 1 then info = where(strmatch(file_list, "*striping_band61*", /fold_case) eq 1, n_info) else $
          info = where(strmatch(file_list, find_these[f], /fold_case) eq 1, n_info)
        if n_info eq 1 then result = strcompress(strsplit(file_list[info], "=", /extract), /rem) else result = strcompress(['"na"', '"na"'], /rem)
        case f of
          0: ls_metadata[i].product_type = result[1]
          1: ls_metadata[i].elevation_source = result[1]
          2: ls_metadata[i].processing_software = result[1]
          3: ls_metadata[i].ephemeris_type = result[1]
          4: ls_metadata[i].sensor_id = result[1]
          5: ls_metadata[i].sensor_mode = result[1]
          6: ls_metadata[i].striping_band1 = result[1]
          7: ls_metadata[i].striping_band2 = result[1]
          8: ls_metadata[i].striping_band3 = result[1]
          9: ls_metadata[i].striping_band4 = result[1]
          10: ls_metadata[i].striping_band5 = result[1]
          11: ls_metadata[i].striping_band6 = result[1]
          12: ls_metadata[i].striping_band7 = result[1]
          13: ls_metadata[i].banding = result[1]
          14: ls_metadata[i].coherent_noise = result[1]
          15: ls_metadata[i].memory_effect = result[1]
          16: ls_metadata[i].scan_correlated_shift = result[1]
          17: ls_metadata[i].inoperable_detectors = result[1]
          18: ls_metadata[i].dropped_lines = result[1]
        endcase
      endfor ;for f=0, n_elements(find_these)-1 do begin
    endfor ;for i=0, n_mtl_files-1 do begin
    
    ;save the information to a .sav file
    out_info_file_sav = strcompress(path+"images\"+ppprrr+"_mtl_metadata_summary.sav", /rem)
    out_info_file_csv = strcompress(path+"images\"+ppprrr+"_mtl_metadata_summary.csv", /rem)
    save, ls_metadata, filename = out_info_file_sav
    export_structure_to_file, ls_metadata, out_info_file_csv
    
    ;find and delete the bad images
    l1g = where(ls_metadata.product_type eq '"L1G"', n_l1g)
    note = "the following images were not processed:"
    outfile = path+"documentation\"+ppprrr+"_images_not_processed.txt
    if n_l1g ge 1 then begin
    
      vct_search = ls_metadata[l1g].year+ls_metadata[l1g].month+ls_metadata[l1g].day
      glovis_search = ls_metadata[l1g].year+ls_metadata[l1g].julian_day
      
      
      delete_these = ["junk"]
      
      for y=0, n_elements(vct_search)-1 do begin
        b = file_search(vct_outputs, "*"+vct_search[y]+"*", count=count)
        if count ge 1 then delete_these = [delete_these,b]
      endfor
      for y=0, n_elements(glovis_search)-1 do begin
        b = file_search(glovis_targz, "*"+glovis_search[y]+"*", count=count)
        if count ge 1 then delete_these = [delete_these,b]
      endfor
      
      if n_elements(delete_these) gt 1 then begin
        delete_these = delete_these[1:*]
        close, /all
        file_delete, delete_these, /quiet
        
        ;write out a txt file with a note about deletions
        these = where(strmatch(delete_these, "*.tar.gz*") eq 1, n_these)
        if n_these ge 1 then begin
          glovis_targz_files = file_basename(delete_these[these])+" because it is an L1G product"
          fulltext = transpose([note, glovis_targz_files])
          if file_exists(outfile) eq 1 then begin
            ;read in the file and append the new info
            openr, lun, outfile, /get_lun
            file_size = file_lines(outfile,/noexpand_path)
            file_list = strarr(file_size)
            readf, lun, file_list
            free_lun, lun
            fulltext = transpose([file_list,glovis_targz_files])
            n_lines = n_elements(fulltext)
            if n_lines gt 1 then begin
              unique = fulltext[1:*]
              unique = unique[uniq(unique, sort(unique))]
              fulltext = [fulltext[0],unique]
            endif
          endif
          openw, lun,outfile,/get_lun
          printf, lun,fulltext
          free_lun, lun
        endif
      endif
    endif else begin
      ;write an empty note if the file does not exist and there are no L1G products
      if file_exists(outfile) eq 0 then begin
        openw, lun,outfile,/get_lun
        printf, lun,note
        free_lun, lun
      endif
    endelse
  endif else begin ;if n_mtl_files ge 1 then begin
    print, ""
    print, ">>> warning!!!"
    print, ">>> cannot find mtl.txt metadata files in this folder:"
    print, ">>> ", vct_outputs
    print, ">>>"
    print, ">>> ending program"
    print, ""
    return
  endelse ;if n_mtl_files ge 1 then begin
end
