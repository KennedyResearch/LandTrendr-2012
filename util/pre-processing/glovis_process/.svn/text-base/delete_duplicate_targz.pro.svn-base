;this program will delete scenes that don't match the specified pat/row 
;and will delete duplicate dates - this can happen if the data came 
;from different receiving stations

pro delete_duplicate_targz, path, ppprrr

  ;find targz files
  lookhere = path+"\glovis_targz\"
  targz_files = file_search(lookhere, "*.tar.gz*", count=n_targz_files)
  if n_targz_files ge 1 then begin
    ;search for glovis_targzs that do not belong to the specified path/row
    delete_these = [""]
    searchfor = strcompress("*"+ppprrr+"*", /rem)
    bads = where(strmatch(file_basename(targz_files), searchfor) ne 1, n_bads, complement=goods, ncomplement=n_goods)
    if n_bads ge 1 then begin
      print, ">>> !!!warning!!! the following images do not belong to the scene path/row that you are processing:"
      print, ""
      print, transpose(targz_files[bads])
      print, ""
      print, ">>> it/they will be deleted"
      delete_these = [delete_these, targz_files[bads]]
      delete_these = delete_these[1:*]
      close, /all
      file_delete, delete_these
    endif
  endif
  
  if n_goods ge 1 then begin
    targz_files = targz_files[goods]
    
    ;search for duplicates
    ;pull out the year and date
    yearday = strmid(file_basename(targz_files), 9,7)
    
    ;find the unique ones
    unique_yearday = fast_unique(yearday)
    delete_these = [""]
    for i=0, n_elements(unique_yearday)-1 do begin
      finds = where(strmatch(targz_files, strcompress("*"+unique_yearday[i]+"*", /rem)) eq 1, n_finds)
      if n_finds gt 1 then begin
        these = indgen(n_finds)
        these = these[1:*]
        finds = finds[these]
        delete_these = [delete_these,targz_files[finds]]
      endif
    endfor
    
    ;if there are bads
    if n_elements(delete_these) ne 1 then begin
      delete_these = delete_these[1:*]
      close, /all
      file_delete, delete_these
    endif
  endif
end