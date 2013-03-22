;subset=[[ulx,uly],[lrx,lry]]
function get_subset, imagefile=imagefile, subset=subset
  
  if keyword_set(imagefile) eq 1 then begin
    if file_test(imagefile) eq 1 then begin
      hdr = gdal_get_hdr_info(imagefile)
      ulx = hdr.subset[0]
      uly = hdr.subset[1]
      lrx = hdr.subset[2]
      lry = hdr.subset[3]
    endif else begin
      print, ">>> file: ", imagefile
      print, ">>> does not exist - check the path"
      print, ""
      print, ">>> ending program"
      print, ""
      stop
    endelse
  endif
  
  if keyword_set(subset) eq 1 then begin
    ulx = subset[0,0]
    uly = subset[1,0]
    lrx = subset[0,1]
    lry = subset[1,1]
  endif
  
  return, [ulx,uly,lrx,lry]

end