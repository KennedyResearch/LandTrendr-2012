
pro cloud_shadow_mask_reference_image_maker, path, image_date_list, unnormalized=unnormalized, normalized=normalized, ledaps=ledaps
pse = path_sep()

  ;check for existence
  exists = file_search(path,"*cloud_shadow_reference_image.bsq", count=n_exists)
  if n_exists eq 0 then begin
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
    
    ;find the image files
    image_date_list = strcompress(string(image_date_list), /rem)
    year = strmid(image_date_list,0,4)
    day = strmid(image_date_list,4,3)
    imgsearch = strcompress("*"+year+"_"+day+searchit+'.bsq', /rem)
    thermalsearch = strcompress("'*"+year+"_"+day+'*b6.bsq', /rem)
    cloudmasksearch = strcompress("'*"+year+"_"+day+'*cloudmask.bsq', /rem)
    
    for i=0, n_images-1 do begin
      ;find the normailized image
      find = file_search(path+"images"+pse, strcompress("*"+year[i]+"_"+day[i]+searchit+".bsq", /rem), count=n_find)
      if n_find eq 0 then find = file_search(path+"images"+pse, strcompress("*"+year[i]+"_"+day[i]+"*radref*.bsq", /rem), count=n_find)
      if n_find eq 0 then message, "could not find a normalized image for date: "+ strcompress(year[i]+"_"+day[i])
      if n_find gt 0 then goodone = where(strmatch(find, "*nochangepixels*") ne 1, n_goods)
      if n_goods gt 1 then message, "there is more than one normalized image for date: "+ strcompress(year[i]+"_"+day[i])
      if i eq 0 then image_list = find[goodone] else image_list = [image_list,find[goodone]]
    ;    ;find the thermal layer
    ;    find = file_search(path+"images\", strcompress("*"+year[i]+"_"+day[i]+"*b6.bsq", /rem), count=n_find_therm)
    ;    ;if n_find_therm eq 0 then message, "could not find a thermal image for date: "+ strcompress(year[i]+"_"+day[i])
    ;    if n_find_therm eq 0 then find = "none"
    ;    if n_find_therm gt 1 then message, "there is more than one thermal image for date: "+ strcompress(year[i]+"_"+day[i])
    ;    if i eq 0 then thermal_list = find else thermal_list = [thermal_list,find]
    ;
    ;    ;find the cloudmask
    ;    find = file_search(path+"images\", strcompress("*"+year[i]+"_"+day[i]+"*cloudmask.bsq", /rem), count=n_find)
    ;    if n_find eq 0 then message, "could not find a cloudmask image for date: "+ strcompress(year[i]+"_"+day[i])
    ;    if n_find gt 1 then message, "there is more than one cloudmask image for date: "+ strcompress(year[i]+"_"+day[i])
    ;    if i eq 0 then mask_list = find else mask_list = [mask_list,find]
    endfor
    
    if file_test(path+"madcal"+pse) eq 1 then outpath = path+"madcal\" else begin
      outpath = path+"images"+pse+"cloud_ref"+pse
      file_mkdir, outpath
    endelse
    
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
      
      ;find the cloudmask
      search = strmid(name, 0, 18)+"*cloudmask.bsq"
      mask = file_search(file_dirname(image_list[i]), search, count=n_mask)
      if i eq 0 then begin
        if n_mask eq 1 then mask_list = mask else mask_list = "na"
      endif else if n_mask eq 1 then mask_list = [mask_list,mask] else mask_list = [mask_list,"na"]
    endfor
    
    missing_mask = where(mask_list eq "na", n_missing_mask)
    if n_missing_mask ge 1 then begin
      missing = image_list[missing_mask]
      print, ""
      print, ">>> warning! the following images are missing cloudmasks - please make them or remove from image list:"
      print, transpose(missing)
      print, ">>> ending program"
      print, ""
      stop
    endif
    
    ;make a subset
    zot_img, mask_list[0], hdr, img, /hdronly
    ul = hdr.upperleftcenter
    lr = hdr.lowerrightcenter
    for i=0, n_images-1 do begin
      zot_img, mask_list[i], m_hdr, m_img, /hdronly
      if m_hdr.upperleftcenter[0] gt ul[0] then ul[0] = m_hdr.upperleftcenter[0]
      if m_hdr.upperleftcenter[1] lt ul[1] then ul[1] = m_hdr.upperleftcenter[1]
      if m_hdr.lowerrightcenter[0] lt lr[0] then lr[0] = m_hdr.lowerrightcenter[0]
      if m_hdr.lowerrightcenter[1] gt lr[1] then lr[1] = m_hdr.lowerrightcenter[1]
    endfor
    
    adjed_lr_coords = adj_int_mult_for_madcal([ul[0],ul[1]], [30,30], [lr[0],lr[1]], /map)
    mastersubset = [[ul[0],ul[1]],[adjed_lr_coords]]
    
    layers = [1,4,5,1]
    bandname = ["b1","b4","b5","b6"]
    for h=0, 3 do begin
      print, ""
      print, ">>> working on: ", bandname[h]
      for i=0, n_images-1 do begin
        print, ">>>   for image: ", strcompress(string(i+1)+"/"+string(n_images), /rem)
        no_therm = 0
        if h eq 3 then begin
          loadthis = thermal_list[i]
          if loadthis eq "na" then begin
            loadthis = image_list[i]
            no_therm = 1
          endif
        endif else loadthis = image_list[i]
        subset=mastersubset
        zot_img, loadthis, imghdr, img, subset=subset, layers = layers[h]
        ;load the mask
        subset=mastersubset
        zot_img, mask_list[i], mskhdr, mskimg, subset=subset
        ;apply the mask
        if no_therm eq 1 then img[*,*,*] = 0 else img = temporary(img)*temporary(mskimg)
        
        outfile = strcompress(outpath+bandname[h]+"_temp_"+string(i+1)+".bsq", /rem)
        openw, unit, outfile, /get_lun
        writeu, unit, img
        free_lun, unit
        imghdr.n_layers = 1
        write_im_hdr, outfile, imghdr
      endfor
      
      files = file_search(outpath,"*temp*.bsq", count=n_b1_files)
      if n_b1_files ge 1 then begin
        for j=0, n_b1_files-1 do begin
          if j eq 0 then begin
            subset=mastersubset
            zot_img, files[j], stayhdr, stayimg, subset=subset, layers = 1
          endif else begin
            subset=mastersubset
            zot_img, files[j], newhdr, newimg, subset=subset, layers = 1
            zeros = where(stayimg eq 0)
            if h eq 0 then goods = where(newimg lt stayimg and newimg ne 0, n_goods) ;b1
            if h eq 1 then goods = where(newimg lt stayimg and newimg ne 0, n_goods) ;b4   and newimg ne 0, n_goods
            if h eq 2 then goods = where(newimg gt stayimg) ;b5   and newimg ne 0, n_goods
            if h eq 3 then goods = where(newimg gt stayimg) ;thermal   and newimg ne 0, n_goods
            index = [temporary(goods),temporary(zeros)]
            stayimg[index] = newimg[index]
          endelse
        endfor
        name = strcompress(outpath+bandname[h]+"_goods.bsq")
        openw, unit, name, /get_lun
        writeu, unit, temporary(stayimg)
        free_lun, unit
        stayhdr.n_layers = 1
        write_im_hdr, name, stayhdr
        
      endif
      tempfiles = file_search(outpath, "*temp*", count=n_tempfiles)
      if n_tempfiles ge 1 then file_delete, tempfiles
    endfor
    
    ;make a stack
    print, ""
    print, ">>> creating an image composite"
    goodsfiles = file_search(outpath, "*goods.bsq", count=n_goodsfiles)
    if n_goodsfiles ne 4 then message, "warning! could not find composite image bands for stacking"
    goodsfiles = goodsfiles[sort(goodsfiles)]
    zot_img, goodsfiles[0], hdr, img, /hdronly
    refimg = intarr(hdr.filesize[0],hdr.filesize[1],4)
    for i=0, n_goodsfiles-1 do begin
      zot_img, goodsfiles[i], hdr, img
      refimg[*,*,i] = img
    endfor
    
    outfile = strcompress(outpath+"cloud_shadow_reference_image.bsq")
    openw, unit, outfile, /get_lun
    writeu, unit, refimg
    free_lun, unit
    hdr.n_layers = 4
    hdr.pixeltype = 6 ;signed 16 bit
    write_im_hdr, outfile, hdr
    
    make_metadata_for_cldmsk_ref_img, image_list, mask_list, outfile
    
    tempfiles = file_search(outpath, "*goods*", count=n_tempfiles)
    if n_tempfiles ge 1 then file_delete, tempfiles
    print, ""
    print, ">>> done creating a cloud and shadow reference image"
  endif
end
