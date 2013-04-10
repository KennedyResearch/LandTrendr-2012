pro madcal_pixel_picker_random, path, this_run_info, image_info, param_set=param_set, modis_ref=modis_ref,$
    pcaonthefly=pcaonthefly, radiometric_ref_img=radiometric_ref_img
    
  ;---unpack the run info structure---
  reffile = this_run_info.file1 ;"L:/4626/images/1992/LT4046026_1992_231_radref.bsq"
  depfile = this_run_info.file2 ;"L:/4626/images/1985/LT5046026_1985_235_archv.bsq"
  maskref = this_run_info.file1mask ;"L:/4626/images/1992/LT4046026_1992_231_madcal_mask.bsq"
  maskdep = this_run_info.file2mask ;"L:/4626/images/1985/LT5046026_1985_235_madcal_mask.bsq"
  usearea = this_run_info.footprint ;"L:/4626/study_area/046026_vct_usearea.bsq"
  run_name = this_run_info.run_name ;"L:/4626/images/landtrendr_image_info_046026.sav"
  
  ;find the landcover mask
  lcmask = file_search(path+"madcal\", "*_lc_mask.bsq", count=n_lcmask)
  if n_lcmask ne 1 then print, ">>> !!!warning!!! cannot find the landcover mask"
  
  ;---get the subset---
  zot_img, maskref, m_hdr, m_img, /hdronly
  ul = m_hdr.upperleftcenter
  lr = m_hdr.lowerrightcenter
  
  zot_img, maskdep, m_hdr, m_img, /hdronly
  if m_hdr.upperleftcenter[0] gt ul[0] then ul[0] = m_hdr.upperleftcenter[0]
  if m_hdr.upperleftcenter[1] lt ul[1] then ul[1] = m_hdr.upperleftcenter[1]
  if m_hdr.lowerrightcenter[0] lt lr[0] then lr[0] = m_hdr.lowerrightcenter[0]
  if m_hdr.lowerrightcenter[1] gt lr[1] then lr[1] = m_hdr.lowerrightcenter[1]
  
  zot_img, usearea, m_hdr, m_img, /hdronly
  if m_hdr.upperleftcenter[0] gt ul[0] then ul[0] = m_hdr.upperleftcenter[0]
  if m_hdr.upperleftcenter[1] lt ul[1] then ul[1] = m_hdr.upperleftcenter[1]
  if m_hdr.lowerrightcenter[0] lt lr[0] then lr[0] = m_hdr.lowerrightcenter[0]
  if m_hdr.lowerrightcenter[1] gt lr[1] then lr[1] = m_hdr.lowerrightcenter[1]
  
  if n_lcmask eq 1 then begin
    zot_img, lcmask, m_hdr, m_img, /hdronly
    if m_hdr.upperleftcenter[0] gt ul[0] then ul[0] = m_hdr.upperleftcenter[0]
    if m_hdr.upperleftcenter[1] lt ul[1] then ul[1] = m_hdr.upperleftcenter[1]
    if m_hdr.lowerrightcenter[0] lt lr[0] then lr[0] = m_hdr.lowerrightcenter[0]
    if m_hdr.lowerrightcenter[1] gt lr[1] then lr[1] = m_hdr.lowerrightcenter[1]
  endif
  
  ulx = ul[0];+40000
  uly = ul[1];-40000
  lrx = lr[0];-40000
  lry = lr[1];+40000
  
  adjed_lr_coords = adj_int_mult_for_madcal([ulx,uly], [30,30], [lrx,lry], /map)
  
  mastersubset = [[ulx,uly],[adjed_lr_coords]]
  
  
  ;---combine the masks---
  print, "creating a mask"
  
  ;make a 0/1 raster from the dependent file cloudmask
  subset=mastersubset
  zot_img, maskdep, maskdep_hdr, maskdep_img, subset=subset
  
  ;make a 0/1 raster from the independent file cloudmask
  subset=mastersubset
  zot_img, maskref, maskref_hdr, maskref_img, subset=subset
  
  maskdep_img = temporary(maskdep_img)*temporary(maskref_img)
  
  ;load in the usearea file
  subset=mastersubset
  zot_img, usearea, usearea_hdr, usearea_img, subset=subset
  
  maskdep_img = temporary(maskdep_img)*temporary(usearea_img)
  
  if n_lcmask eq 1 then begin
    ;load in the water and snow mask
    subset=mastersubset
    zot_img, lcmask, lcmask_hdr, lcmask_img, subset=subset
    
    maskdep_img = temporary(maskdep_img)*temporary(lcmask_img)
  endif
  
  if keyword_set(modis_ref) eq 1 then begin
    subset=mastersubset
    zot_img, reffile, refhdr, refimg, subset=subset, /hdronly
    modismask = bytarr(refhdr.filesize[0], refhdr.filesize[1])
    for b=0, refhdr.n_layers-1 do begin
      zot_img, reffile, refhdr, refimg, subset=subset, layers=(b+1)
      if b eq 0 then modismask = temporary(refimg) ne 0 else $
        modismask = temporary(modismask)*(refimg ne 0)
    endfor
    maskdep_img = temporary(maskdep_img)*temporary(modismask)
  endif
  
  mask = temporary(maskdep_img)
  
    outimg = "K:\045030\madcal\mask_test.bsq"
    openw, unit, outimg, /get_lun
    writeu, unit, mask
    free_lun, unit
    maskdep_hdr.n_layers = 1
    maskdep_hdr.pixeltype = 3
    write_im_hdr, outimg, maskdep_hdr
  
  ;---get the number of channels---
  subset=mastersubset
  zot_img, depfile, dep_hdr, dep_img, /hdronly, subset=subset
  subset=mastersubset
  zot_img, reffile, ref_hdr, ref_img, /hdronly, subset=subset
  layers_dep = dep_hdr.n_layers
  layers_ref = ref_hdr.n_layers
  
  ;---check that both images have same layers---
  if keyword_set(modis_ref) then begin
    if layers_ref ne 7 then begin
      print, ">>> the modis reference image has:"
      print, ">>> ", layers_ref, " bands..."
      print, ">>> it should have 7 bands..."
      print, ">>> ensure that you are using the NBAR..."
      print, ">>> product and that it has 7 bands..."
      print, ">>> in the native sequence"
      stop
    endif
    if layers_dep ne 6 then begin
      print, ">>> the dependant image:"
      print, "  ", depfile
      print, ">>> has ", layers_dep, "_bands..."
      print, ">>> it should have 6..."
      print, ">>> find out what the deal is"
      stop
    endif
  endif else if layers_dep ne layers_ref then message, "n layers do not match for ref and dep files"
  
  ;---check that both images have the same # columns and rows---
  if dep_hdr.filesize[0] ne ref_hdr.filesize[0] then message,$
    "n columns do not match for ref and dep files"
  if dep_hdr.filesize[1] ne ref_hdr.filesize[1] then message,$
    "n rows do not match for ref and dep files"
    
  ;---get image info--
  subset=mastersubset
  zot_img, depfile, dep_hdr, dep_img, subset=subset, /hdronly ;load the dependent image
  dim = dep_hdr.filesize
  
  ;create the no change image holder
  no_change_image = bytarr(dim[0] , dim[1])
  
  first = 0
  last = 999999
  n_nochange = 0
  
  total_no_change = 200000
  allgone = 0
  grabs = 20
  
  goods = where(mask eq 1, n_goods)
  random = randomu(seed, n_goods)
  random = sort(temporary(random))
  
  n_grabs = 1
  repeat begin
    ;deal with running out of pixels
    if last ge n_goods-1 then begin
      last = n_goods-1
      allgone = 1
    endif
    usethese = random[first:last]
    check_these = goods[usethese]
    
;    no_change_image[check_these] = 1
;  
;    outimg = "M:\025028\madcal\no_change_test.bsq"
;    openw, unit, outimg, /get_lun
;    writeu, unit, no_change_image
;    free_lun, unit
;    maskdep_hdr.n_layers = 1
;    maskdep_hdr.pixeltype = 3
;    write_im_hdr, outimg, maskdep_hdr
    
    
    first = last+1
    last = first+999999
    
    radcal_auto, reffile, depfile, check_these, 'dummy_info', mastersubset, nochange, nochangecount, modis_ref=modis_ref ;find no change pixels from the bin grabs
    n_nochange = n_nochange+nochangecount
    
    print, "allgone: ", allgone
    print, "n_nochange: ", n_nochange
    print, "grab: ", n_grabs+1
    
    no_change_image[check_these[nochange]] = 1 ;fill in the no_change_image with 1's where no change pixels were found
    
    n_grabs = n_grabs +1
    
  endrep until n_grabs eq grabs or allgone eq 1 or n_nochange ge total_no_change
  
  outfile = this_run_info.nochange
  
  openw, un, outfile, /get_lun
  writeu, un, no_change_image
  free_lun, un
  ref_hdr.n_layers = 1
  ref_hdr.pixeltype = 3
  write_im_hdr, outfile, ref_hdr
  
end
