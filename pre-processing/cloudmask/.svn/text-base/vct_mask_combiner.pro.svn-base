

pro vct_mask_combiner, path, ppprrr, update=update

  print, ">>> making vct usearea mask"
  
  ;create some path variables from the given inputs
  ppp = strmid(ppprrr, 0, 3) ;'046'
  rrr = strmid(ppprrr, 3, 3) ;'026
  pr = strcompress("p"+ppp+"r"+rrr ,/rem) ;'p046r026
  if keyword_set(update) eq 1 then begin
    vct_output_path = strcompress(path+"VCT\outputs\update\", /rem)
    usearea_composite = strcompress(path+"VCT\outputs\update\usearea_composite\", /rem)
    existing_usearea_dir = strcompress(path+"VCT\outputs\usearea_composite\", /rem)
  endif else begin
    vct_output_path = strcompress(path+"VCT\outputs\", /rem)
    usearea_composite = strcompress(path+"VCT\outputs\usearea_composite\", /rem)
  endelse
  
  file_mkdir, usearea_composite
  
  ;find the vct common area masks (vct usearea)
  files = file_search(vct_output_path, "*commMask*")
  images = files[where(strmatch(files, "*.hdr") eq 0, complement=hdrs)]
  hdrs = files[hdrs]
  
  ;find the intersection of all of the vct usearea files and make a subset
  for i=0, n_elements(images)-1 do begin
    zot_img, images[i], hdr, img, /hdronly
    if i eq 0 then begin
      ulx = hdr.upperleftcenter[0]
      uly = hdr.upperleftcenter[1]
      lrx = hdr.lowerrightcenter[0]
      lry = hdr.lowerrightcenter[1]
    endif else begin
      ulx = max([ulx, hdr.upperleftcenter[0]])
      uly = min([uly, hdr.upperleftcenter[1]])
      lrx = min([lrx, hdr.lowerrightcenter[0]])
      lry = max([lry, hdr.lowerrightcenter[1]])
    endelse
  endfor
  subset = [[ulx,uly],[lrx,lry]]
  
  ;create an image holder
  zot_img, images[0], hdr, img_holder, subset=subset, /hdronly
  img_holder = bytarr(hdr.filesize[0], hdr.filesize[1])+1
  
  for i=0, n_elements(images)-1 do begin
    print, ">>>  adding: ", file_basename(images[i])
    zot_img, images[i], hdr, img, subset=subset
    img_holder = byte(img_holder*img)
  endfor
  
  ;create
  outimg = strcompress(usearea_composite + pr + "_vct_usearea.bsq", /rem)
  outhdr = strcompress(usearea_composite + pr + "_vct_usearea.hdr", /rem)
  
  ;check if a vct_usearea already exists  
    lt_delete_duplicate, outimg, /vct_usearea
  
    ;write out the file
    openw, unit, outimg, /get_lun
    writeu, unit, img_holder
    free_lun, unit
    hdr.n_layers = 1
    hdr.pixeltype = 3   ;unsigned 32 bit
    hdr.upperLeftCenter = subset[*,0]
    hdr.lowerRightCenter = subset[*,1]
    write_im_hdr, outimg, hdr
    
    ;convert the flat header to envi style
    convert_bsq_headers_to_envi, usearea_composite, hdrs[0], /overwrite
      
    ;delete the usearea files
    close, /all
    file_delete, images, hdrs
    
    ;fix the composite vct usearea file's header so that the coordinates are tied to the center of the pixel (1.5) and not the ul corner (1)
    openr, lun, outhdr, /get_lun
    file_size = file_lines(outhdr,/noexpand_path)
    file_list = strarr(file_size)
    readf, lun, file_list
    free_lun, lun
    
    map_info_line_element = where(strmid(file_list, 0, 8) eq 'map info')
    map_info_line = file_list[map_info_line_element] ;get the map info line        ;file_list[12]
    map_info_pieces = strsplit(map_info_line, ',', /extract) ;split up the ,ap info line
    check = double(strcompress(map_info_pieces[1], /rem))
    if map_info_pieces[1] lt 1.5 then begin
      map_info_pieces[1] = '1.5000' ;replace the 1.0000 (ul) with 1.500 (center) for x
      map_info_pieces[2] = '1.5000' ;replace the 1.0000 (ul) with 1.500 (center) for y
      map_info_fixed = strcompress(strjoin(map_info_pieces, ", ")) ;put the map info line back together
      file_list[map_info_line_element] = map_info_fixed ;put the fixed map info line into the hdr
      
      file_list = transpose(file_list)
      openw, lun, outhdr,/get_lun
      printf, lun, file_list
      free_lun, lun
    endif
    
    ;check the extents of the updated common area mask and the existing common area mask
    if keyword_set(update) eq 1 then begin
      new_mask = outimg
      old_mask = file_search(existing_usearea_dir, "*vct_usearea.bsq", count=count)
      if count ne 1 then begin
        if count gt 1 then begin
          print, ">>> there is more than one vct_usearea.bsq file..."
          print, ">>> there can only be one, check on it"
          stop
        endif
        if count eq 0 then begin
          print, ">>> there is no vct_usearea.bsq file in the *\VCT\outputs\usearea_composite\ folder..."
          print, ">>> one must exist"
          stop
        endif
      endif
      zot_img, new_mask, new_mask_hdr, new_mask_img, /hdronly
      zot_img, old_mask, old_mask_hdr, old_mask_img, /hdronly
      
      ul = old_mask_hdr.upperleftcenter
      lr = old_mask_hdr.lowerrightcenter
      
      old_commask_files = file_search(existing_usearea_dir, "*vct_usearea*")
      new_commask_files = file_search(usearea_composite, "*vct_usearea*")
      
      change = 0 ;starter
      if new_mask_hdr.upperleftcenter[0] gt ul[0] then change = change + 1 ;message, "new mask has a smaller dimension than the existing one"
      if new_mask_hdr.upperleftcenter[1] lt ul[1] then change = change + 1 ;message, "new mask has a smaller dimension than the existing one"
      if new_mask_hdr.lowerrightcenter[0] lt lr[0] then change = change + 1 ;message, "new mask has a smaller dimension than the existing one"
      if new_mask_hdr.lowerrightcenter[1] gt lr[1] then change = change + 1 ;message, "new mask has a smaller dimension than the existing one"
    
      if change ge 1 then begin
        file_delete, old_commask_files
        file_move, new_commask_files, existing_usearea_dir  
      endif else file_delete, usearea_composite, /recursive
    
      ;if an update, move the mask files to the no-update folder
      ;vct_output_path is OK for updates - it gets redirected up top
      ;mask = file_search(drive + vct_output_path, "*mask", count=maskcount)
      ;maskhdr = file_search(drive + vct_output_path, "*mask.hdr", count=maskhdrcount)
      ;commmask = file_search(vct_output_path, "*commMask*", count=commmaskcount)
      outdir = strcompress(path+"VCT\outputs\", /rem)
      ;update_files = file_search(vct_output_path, "*mask*", count=maskcount)
      files = file_search(vct_output_path, "*", count=maskcount)
      ;a = drive + vct_output_path
      file_move, files, outdir, /overwrite
      ;    if maskcount ge 1 then file_move, mask, outdir
      ;    if maskhdrcount ge 1 and maskcount eq maskhdrcount then file_move, maskhdr, outdir else begin
      ;      print, ">>> trying to move the mask files..."
      ;      print, ">>> from the vct\outputs\update\ folder to the..."
      ;      print, ">>> vct\outputs\ folder, but the number of mask..."
      ;      print, ">>> files does not match the number of .hdr files..."
      ;      print, ">>> go to the below location and figure out what's up"
      ;      print, "  ", drive + vct_output_path
      ;    endelse
      if keyword_set(delete_files) eq 1 then begin
        ;make sure that we are deleting the update folder contents and not the outputs contents
        close, /all, /force
        goodtogo = strmatch(drive + vct_output_path, "*update*")
        if goodtogo eq 1 then file_delete, drive + vct_output_path, /recursive
      endif
    endif
    
    ;delete the intermediate DEMs from the prep_dem program - waiting til now in case we need to diagnose a problem
    demfiles =file_search(path+"VCT\prep\", "*tif*", count=n_demfiles)
    if n_demfiles ge 1 then begin
      close, /all
      file_delete, demfiles
    endif
  end