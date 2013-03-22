pro image_difference_maker, path, image_date_list, normalized=normalized, unnormalized=unnormalized, ledaps=ledaps
 pse = path_sep()

  ;check the keywords for errors
  check = keyword_set(unnormalized) + keyword_set(normalized) + keyword_set(ledaps)
  if check eq 0 then begin
    print, "warning! you must activate either the keyword 'unnormalized' or 'normalized' or 'ledaps'..."
    print, "to define what images to search for when making a cloudmask reference image composite"
    print, "ending program"
    stop
  endif
  if check gt 1 then begin
    print, "warning! you must activate only one keyword, either 'unnormalized' or 'normalized' or 'ledaps'..."
    print, "to define what images to search for when making a cloudmask reference image composite..."
    print, "not both"
    print, "ending program"
    stop
  endif
  
  if keyword_set(unnormalized) eq 1 then searchit = "*archv"
  if keyword_set(normalized) eq 1 then searchit = "*_to_*"
  if keyword_Set(ledaps) eq 1 then searchit = "*ledaps"
  
  n_images = n_elements(image_date_list)
  
  ;find the cloud and shadow reference image
  refimgfile = file_search(path, "cloud_shadow_reference_image.bsq", count=n_refimgfile)
  if n_refimgfile eq 0 then message, "cannot find 'cloud_shadow_reference_image.bsq' file"
  
  ;find the image files
  image_date_list = strcompress(string(image_date_list), /rem)
  year = strmid(image_date_list,0,4)
  day = strmid(image_date_list,4,3)
  imgsearch = strcompress("*"+year+"_"+day+searchit+'.bsq', /rem)
  thermalsearch = strcompress("'*"+year+"_"+day+'*b6.bsq', /rem)
  
  for i=0, n_images-1 do begin
    ;find the normailized image
    find = file_search(path+"images"+pse, strcompress("*"+year[i]+"_"+day[i]+searchit+".bsq", /rem), count=n_find)
    if n_find eq 0 then message, "could not find a normalized image for date: "+strcompress(year[i]+"_"+day[i])
    if n_find gt 0 then goodone = where(strmatch(find, "*nochangepixels*") ne 1, n_goods)
    if n_goods gt 1 then message, "there is more than one normalized image for date: "+strcompress(year[i]+"_"+day[i])
    if i eq 0 then image_list = find[goodone] else image_list = [image_list,find[goodone]]
;    ;find the thermal layer
;    find = file_search(path+"images\", strcompress("*"+year[i]+"_"+day[i]+"*b6.bsq", /rem), count=n_find)
;    if n_find eq 0 then message, "could not find a thermal image for date: "+strcompress(year[i]+"_"+day[i])
;    if n_find gt 1 then message, "there is more than one thermal image for date: "+strcompress(year[i]+"_"+day[i])
;    if i eq 0 then thermal_list = find else thermal_list = [thermal_list,find]
  endfor
  
  
  
  for i=0, n_images-1 do begin
    dir = file_dirname(image_list[i])
    name = file_basename(image_list[i])
    search = strmid(name, 0, 18)+"*b6.bsq"
    
    ;find the thermal layer
    thermal = file_search(file_dirname(image_list[i]), search, count=n_thermal)
    ;if n_thermal eq 0 then message, "could not find a thermal image for date: ", strcompress(year[i]+"_"+day[i])
    ;if n_thermal gt 1 then message, "there is more than one normalized images for date: ", strcompress(year+"_"+day)
    if i eq 0 then begin
      if n_thermal eq 1 then thermal_list = thermal else thermal_list = "na"
    endif else if n_Thermal eq 1 then thermal_list = [thermal_list,thermal] else thermal_list = [thermal_list,"na"]
  endfor
  
  ;make a subset
  subsetfiles = [image_list,refimgfile]
  zot_img, subsetfiles[0], hdr, img, /hdronly
  ul = hdr.upperleftcenter
  lr = hdr.lowerrightcenter
  for i=0, n_elements(subsetfiles)-1 do begin
    zot_img, subsetfiles[i], m_hdr, m_img, /hdronly
    if m_hdr.upperleftcenter[0] gt ul[0] then ul[0] = m_hdr.upperleftcenter[0]
    if m_hdr.upperleftcenter[1] lt ul[1] then ul[1] = m_hdr.upperleftcenter[1]
    if m_hdr.lowerrightcenter[0] lt lr[0] then lr[0] = m_hdr.lowerrightcenter[0]
    if m_hdr.lowerrightcenter[1] gt lr[1] then lr[1] = m_hdr.lowerrightcenter[1]
  endfor
  
  adjed_lr_coords = adj_int_mult_for_madcal([ul[0],ul[1]], [30,30], [lr[0],lr[1]], /map)
  mastersubset = [[ul[0],ul[1]],[adjed_lr_coords]]
  
  layers = [1,4,5,1]
  bandname = ["b1","b4","b5","b6"]
  
  for i=0, n_images-1 do begin
    
    ;check for existence
    if keyword_set(normalized) eq 1 then ending = "normalized.bsq" 
    if keyword_set(unnormalized) eq 1 then ending = "unnormalized.bsq"
    if keyword_set(ledaps) eq 1 then ending = "ledaps_img.bsq"
    outfile = strcompress(file_dirname(image_list[i])+pse+strmid(file_basename(image_list[i]),0,18)+"_cld_shdw_dif_"+ending, /rem)
    if file_test(outfile) eq 1 then continue
    
    ;if the difference image doe snot exist then make one
    print, ">>> creating image difference for file: ", strcompress(string(i+1)+"/"+string(n_images), /rem) 
    subset=mastersubset
    zot_img, refimgfile, refhdr, refimg, subset=subset
    for j=0, n_elements(layers)-1 do begin
      subset=mastersubset
      no_therm = 0
      if j eq 3 then begin
        loadthis = thermal_list[i] 
        if loadthis eq "na" then begin
          loadthis = image_list[i]
          no_therm = 1
        endif
      endif else loadthis = image_list[i]
      subset=mastersubset
      zot_img, loadthis, hdr, img, subset=subset, layers=layers[j]
      if no_therm eq 1 then img[*,*,*] = 0 
      if j eq 0 then refimg[*,*,j] = refimg[*,*,j]-img else refimg[*,*,j] = img-refimg[*,*,j]
    endfor
        
    openw, unit, outfile, /get_lun
    writeu, unit, refimg
    free_lun, unit
    refhdr.n_layers = 4
    write_im_hdr, outfile, refhdr
    
    make_metadata_for_cldmsk_dif_img, outfile, image_list[i], refimgfile 
    
  endfor  
  print, ">>> done creating image difference files"
  print, ""
end
