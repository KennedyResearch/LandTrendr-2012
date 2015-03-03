
;variable "pcaimgstuct" comes from pca.pro
pro madcal_pixel_picker, path, this_run_info, image_info, param_set=param_set, modis_ref=modis_ref,$
    radiometric_ref_img=radiometric_ref_img
    
  if keyword_set(param_set) eq 1 then begin
    if param_set eq 1 then params = [2000,259999L,0] ;259999L
    if param_set eq 2 then params = [3000,259999LL,1]
  endif else params = [2000,259999L,0]
  
  ;if param_set gt 2 then params = [2500,259999L,0]
  min_in_bin = params[0]
  ;last = params[1]
  bin_adj = params[2]
  
  ;---unpack the run info structure---
  reffile = this_run_info.file1
  depfile = this_run_info.file2
  maskref = this_run_info.file1mask
  maskdep = this_run_info.file2mask
  usearea = this_run_info.footprint
  run_name = this_run_info.run_name
  results_path = this_run_info.results_path
  
  ;reffile = "L:/4626/images/1992/LT4046026_1992_231_radref.bsq"
  ;depfile = "L:/4626/images/1985/LT5046026_1985_235_archv.bsq"
  ;maskref = "L:/4626/images/1992/LT4046026_1992_231_madcal_mask.bsq"
  ;maskdep = "L:/4626/images/1985/LT5046026_1985_235_madcal_mask.bsq"
  ;usearea = "L:/4626/study_area/046026_vct_usearea.bsq"
  ;imageinfo = "L:/4626/images/landtrendr_image_info_046026.sav"
  
  ;find the landcover mask
  lcmask = file_search(path+"madcal\", "*_lc_mask.bsq", count=n_lcmask)
  if n_lcmask ne 1 then print, ">>> !!!warning!!! cannot find the landcover mask, continuing without it"
  
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
  
  ulx = ul[0]
  uly = ul[1]
  lrx = lr[0]
  lry = lr[1]
  
  ;adjust the subset size so that it will make it through on a 2gb machine
  if keyword_set(radiometric_ref_img) eq 0 then begin
    columns = ceil(lrx-ulx)/30
    rows = ceil(uly-lry)/30
    if columns gt 7000 then begin
      xadjust = round(((columns-7000)/2)*30)
      ulx = ul[0]+xadjust
      lrx = lr[0]-xadjust
    endif
    if rows gt 7000 then begin
      yadjust = round(((rows-7000)/2)*30)
      lry = lr[1]+yadjust
      uly = ul[1]-yadjust
    endif
  endif
  
  ;todo - find the pixel size from zot_img instead of hardwire to 30,30
  adjed_lr_coords = adj_int_mult_for_madcal([ulx,uly], [30,30], [lrx,lry], /map)
  
  mastersubset = [[ulx,uly],[adjed_lr_coords]]
  
  ;---combine the masks---
  print, "creating a mask"
 stop 
  ;make a 0/1 raster from the dependent file cloudmask
  subset=mastersubset
  zot_img, maskdep, maskdep_hdr, maskdep_img, subset=subset
  ;maskdep_img = maskdep_img eq 0
  
  ;make a 0/1 raster from the dependent file cloudmask
  subset=mastersubset
  zot_img, maskref, maskref_hdr, maskref_img, subset=subset
  ;maskref_img = maskref_img eq 0
  
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
  
  if keyword_set(radiometric_ref_img) eq 1 then begin
    if file_test(radiometric_ref_img) eq 1 then begin
      mask = mask lt 200 ;this turns all the mask pixels on so that the background does not limit pixels - but it should be tested without it because I don't think it is actually needed - JB 3/21/12
    endif
  endif
  
    
;  outimg = "F:\045029\madcal\mask_test.bsq"
;  openw, unit, outimg, /get_lun
;  writeu, unit, mask
;  free_lun, unit
;  maskdep_hdr.n_layers = 1
;  maskdep_hdr.pixeltype = 3
;  write_im_hdr, outimg, maskdep_hdr
    
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
  
  ;---histogram density binned image---
  subset=mastersubset
  print, ">>> running PCA"
  layers = [1,2,3]
  pc = pca1(depfile, maskdep, layers, subset=subset, othermask=mask)
  
  print, "bin the first component"
  if keyword_set(modis_ref) eq 1 then n_bins = 10 else n_bins = 10 ;for else 10 bis was pretty good
  histequal = hist_equal(temporary(pc), top=n_bins) ;10 bins plus 0 so actually 11 bins (the last one is not used though) so 10
  
  histequal = (histequal*temporary(mask))-1
  
  ;    outimg = "F:/047030/madcal/pca_masked_hist_equala.bsq"
  ;    openw, unit, outimg, /get_lun
  ;    writeu, unit, histequal
  ;    free_lun, unit
  ;    dep_hdr.n_layers = 1
  ;    dep_hdr.pixeltype = 6
  ;    write_im_hdr, outimg, dep_hdr
  
  ;create the no change image holder
  no_change_image = bytarr(dim[0] , dim[1])
  
  ;---go through the bins and find no-change pixels---
  first = 0L ;within bin grab start tracker 32bit
  last = 259999L ;within bin grab end tracker 32bit 99999L
  goods = ulonarr(n_bins) ;no-change pixel number tracker 32bit
  it = 1
  
  info = create_struct("count",0L, "n_goods",0L) ;create a structure to hold information about the no change pixels
  info = replicate(info, n_bins)  ;replicate for each bin
  done = bytarr(n_bins)  ;a holder to keep track of which bins have a set amount of nochange pixels
  
  for j=0, n_bins-1-bin_adj do info[j].count = n_elements(where(histequal eq j)) ;get the pixel count in each bin of the hist_equal
  min_n = min(info.count)
  random = randomu(seed, min_n)
  random = sort(temporary(random))
  
  if keyword_set(modis_ref) eq 1 then justendit = 3 else justendit = 3 ;set the number of iterations that will occur if other catches don't end the process first
  
  repeat begin ;cycle through the bins doing grabs until all of the bins have a set amount of no change pixels
    print, "checking pixel group: ", it
    ;---pull out a set of pixels from each bin and pass it to radcal---
    check_pixels_all = 0 ;create a holder for the pixels to be checked by radcal
    ;picks_all = ulonarr((last-first)+1, n_bins) ;create a holder to keep track of which grabbed pixels belong to which bin
    for j=0, n_bins-1-bin_adj do begin ;go through all of the bins and pull out pixels (grabs)
      if last ge min_n-1 then begin ;deal with running out of pixels to grab in a bin
        last = min_n-1
      ;picks_all = ulonarr((last-first)+1, n_bins)
      endif
      picks = where(histequal eq j, count) ;if there are enough pixels in the bin for a grab, identify these pixels
      usethese = random[first:last]
      check_pixels = picks[usethese] ;pull out the pixels based on where the first and last index are at
      check_pixels_all = [check_pixels_all, check_pixels] ;concatenate the check pixels from all bins
      start_here:
    ;picks_all[*,j] = check_pixels ;put the pixel grabs into the holder so we can check later how many no change picks belong to each bin
    endfor
    
    check_pixels_all = check_pixels_all[1:*] ;get rid of the holder starter
    
    subset=mastersubset
    radcal_auto, reffile, depfile, check_pixels_all, info, subset, nochange, nochangecount, modis_ref=modis_ref ;find no change pixels from the bin grabs
    if nochangecount eq 0 then begin
      print, ">>> !!!warning!!! radcal could not find any no-change pixels, check this image: "
      print, ">>> ", depfile
      print, ">>> and it's mask - this error has come up in the past where between the reference image..."
      print, ">>> and the dependent image, there are basically no non-masked pixels left..."
      print, ">>> if after checking the dep. image you confirm that it is a too-few pixels problem..."
      print, ">>> then delete the date - use the 'delete_lt_dates' process in the batchfile - note that..."
      print, ">>> you must turn on the process and define the date to delete in the 'deletedate' variable"
      print, ">>> ending program"
      stop
    endif
    no_change_image[check_pixels_all[nochange]] = 1 ;fill in the no_change_image with 1's where no change pixels were found
    
    for f=0, n_bins-1-bin_adj do if goods[f] ge min_in_bin then done[f] = 1 ;keep track of which bins have 1500 nochange pixels
    done_counter = ulong(total(done)) ;sum up the dones
    first = first+259999 ;track the start of next pixel group
    last = last+259999 ;track the end of the next pixel group
    it=it+1 ;track the iterations for printing
  endrep until done_counter eq n_bins or last gt (n_elements(random)-5) or it eq justendit ;get out if all the bins are done or one is empty
  
  ;  outfile = strcompress(file_dirname(this_run_info.nochange)+"\nochange_pixels_no_second_sample.bsq"   ,/rem)
  ;  openw, un, outfile, /get_lun
  ;  writeu, un, no_change_image
  ;  free_lun, un
  ;  ref_hdr.n_layers = 1
  ;  ref_hdr.pixeltype = 3
  ;  write_im_hdr, outfile, ref_hdr
  
  
  mingoods = 3000 ;min(goods)
  for i=0, n_bins-1-bin_adj do begin
    group = where(histequal eq i and no_change_image eq 1, count)
    dif1 = count-mingoods
    if dif1 ge 1 then begin
      random = randomu(seed1, dif1)
      random = sort(random)
      no_change_image[group[random]] = 0
    endif
  endfor
  
  outfile = this_run_info.nochange
  
  openw, un, outfile, /get_lun
  writeu, un, no_change_image
  free_lun, un
  ;  ref_hdr.upperleftcenter = mastersubset[*,0]
  ;  ref_hdr.lowerrightcenter = mastersubset[*,1]
  ref_hdr.n_layers = 1
  ref_hdr.pixeltype = 3
  write_im_hdr, outfile, ref_hdr
  
;    openw, unit, outimg, /get_lun
;    writeu, unit, no_change_image
;    free_lun, unit
;    dep_hdr.n_layers = 1
;    dep_hdr.pixeltype = 3   ;unsigned 32 bit
;    ;hdr.upperLeftCenter = [ulx, uly]
;    ;hdr.lowerRightCenter = [lrx, lry]
;    write_im_hdr, outimg, dep_hdr
end
