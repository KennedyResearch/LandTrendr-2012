pro watersnow_mask_for_madcal, path, ppprrr  ;, update=update

;  if keyword_set(update) eq 1 then vct_output_path = strcompress(path+"VCT\outputs\update\", /rem) else $
;    vct_output_path = strcompress(path+"VCT\outputs\", /rem)
  
  vct_output_path = strcompress(path+"VCT\outputs\", /rem)
  lt_madcal_path = strcompress(path+'madcal\', /rem)
  
  proj = file_search(path+"VCT\prep\", "*ref_img_projection.prf", count=n_proj)
  if n_proj ne 1 then message, "can't find proj ref .prf file or there are more than one"
  
  ;find the vct masks
  images = file_search(vct_output_path, "*mask.bsq")
  
  ;find the vct_usearea composite
  usearea = file_search(vct_output_path, "*vct_usearea.bsq", count=n_usearea)
  if n_usearea eq 0 then begin
    print, ">>> !!!warning!!! trying to find the vct_usearea.bsq file..."
    print, ">>> which should be inside *\ppprrr\VCT\outputs\usearea_composite\"
    print, ">>> but the search was unsuccessful, ending program"
    stop
  endif
  
  zot_img, usearea, hdr, img, /hdronly
  subset=[[hdr.upperleftcenter],[hdr.lowerrightcenter]]
  
;  ;find the intersection of all of the vct usearea files and make a subset  
;  for i=0, n_elements(images)-1 do begin
;    zot_img, images[i], hdr, img, /hdronly
;    if i eq 0 then begin
;      ulx = hdr.upperleftcenter[0]
;      uly = hdr.upperleftcenter[1]
;      lrx = hdr.lowerrightcenter[0]
;      lry = hdr.lowerrightcenter[1]
;    endif else begin
;      ulx = max([ulx, hdr.upperleftcenter[0]])
;      uly = min([uly, hdr.upperleftcenter[1]])
;      lrx = min([lrx, hdr.lowerrightcenter[0]])
;      lry = max([lry, hdr.lowerrightcenter[1]])
;    endelse
;  endfor
;  subset = [[ulx,uly],[lrx,lry]]
  
  
  print, ">>> making combined madcal mask, please wait..."
  for i=0, n_elements(images)-1 do begin
    print, ">>> working on image: "+ strcompress(string(i+1)+"/"+string(n_elements(images)),/rem)
    zot_img, images[i], hdr, img, subset=subset
    if i eq 0 then bads = (img eq 1)+(img eq 7) else $ ;bads eq gt 0  (img eq 0) > took this out because it adds in all of the SLC-off gaps
      bads = temporary(bads)+((img eq 1)+(img eq 7)) ;(img eq 0)+
  endfor
  
  bads = temporary(bads) gt 0  ;bads are 1's and everything else is 0's
  
  radius = 1
  kernel = SHIFT(DIST(2*radius+1), radius, radius) LE radius
  bads = convol(temporary(bads), kernel) gt 0
  bads = convol(temporary(bads), kernel) gt 0
  
  ;multiply by the usearea to set background pixels to 0
  zot_img, usearea, hdr, img
  bads = byte(temporary(bads) eq 0)*temporary(img) ;with this bads are now 0
  
  ;write it out temp so we can reproject
  tempout = path+"VCT\prep\temp_madcal_mask.bsq"
  
  ;write out cloudmask
  openw, un, tempout, /get_lun
  writeu, un, temporary(bads)
  free_lun, un
  hdr.pixeltype = 3
  write_im_hdr, tempout, hdr
  
  template_hdr = stringswap(images[0], "bsq", "hdr")
  convert_bsq_headers_to_envi, path+"VCT\prep\", template_hdr  
  
  ;reproject the mask
  ;time = timestamp()
  outfile = strcompress(path+"madcal\"+ppprrr+"_madcal_mask.bsq")

  ;create command to project image using gdalwarp
  s1 = "gdalwarp -t_srs " + proj
  s2 = " -of ENVI -tr 30 30 -overwrite -multi "
  cmd = s1 + s2 + tempout + " " + outfile
  ;run the command
  spawn, cmd, /hide
  
  deletethese = file_search(path+"VCT\prep\", "*temp_madcal_mask*", count=n_deletethese)
  if n_deletethese ge 1 then file_delete, deletethese  
  
end