;output must have extension: .prf
function gdal_get_projection, image, prf_outfile=prf_outfile

  cmd = "gdalinfo " + image
  if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
  spawn, cmd, hdr, /hide
  
  ;read through the file and pull out the projection lines
  start = where(strmatch(hdr, "*Coordinate System is:*")) + 1
  finish = where(strmatch(hdr, "*Origin*")) - 1
  thegoods = hdr[start:finish]
  
  if keyword_set(prf_outfile) eq 1 then begin
    ;write out the projection in a format that gdalwarp will accept
    openw, lun,prf_outfile,/get_lun
    printf, lun,thegoods
    free_lun, lun
  endif
  
  return, thegoods
end