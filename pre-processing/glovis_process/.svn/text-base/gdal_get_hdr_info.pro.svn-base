function gdal_get_hdr_info, image
  cmd = "gdalinfo " + image
  if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
  spawn, cmd, info, /hide
  
  ul_line = where(strmatch(info, "*Upper Left*") eq 1, n_ul_line)
  if n_ul_line ne 0 then begin
    ul_line = info[ul_line]
    ul_pieces = strsplit(ul_line,"[(),]", /extract, count=n_pieces)
    ul = double([ul_pieces[1],ul_pieces[2]])
  endif
  
  lr_line = where(strmatch(info, "*Lower Right*") eq 1, n_lr_line)
  if n_lr_line ne 0 then begin
    lr_line = info[lr_line]
    lr_pieces = strsplit(lr_line,"[(),]", /extract, count=n_pieces)
    lr = double([lr_pieces[1],lr_pieces[2]])
  endif 
  
  layer_lines = where(strmatch(info, "*Band_*") eq 1, n_layer_lines)
  
  size_line = where(strmatch(info, "*Size is*") eq 1, n_size_line)
  if n_size_line ne 0 then begin
    size_line = info[size_line]
    size_pieces = long(strsplit(size_line,"[Size is,]", /extract, count=n_pieces))
  endif 
  
  
  subset = [ul,lr]
  info = {upperleftcenter:ul,$
          lowerrightcenter:lr,$
          subset:subset,$
          n_layers:n_layer_lines,$
          filesize:size_pieces}
  
  return, info 
end