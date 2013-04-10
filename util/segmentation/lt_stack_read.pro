function lt_stack_read, image_info, index, point_coords, point_name, pval=pval

  info = image_info
  index = index
  fix_doy_effect = 1
  divisor = 1
  kernelsize = 1
  
  if n_elements(pval) eq 0 then pval = 0.15
  
  minimum_number_years_needed = 6     ;if we have fewer years than this, we can't do it.
  
  ;get the number of years we're analyzing.
  ;updating this august 21, 2008 for the multiple images per year
  ;n_yrs = n_elements(image_info)
  
  years = fast_unique(info.year)
  years = years[sort(years)]
  n_yrs = n_elements(years)
  
  n_images = n_elements(info)
  
  if n_yrs lt minimum_number_years_needed then begin
    print, 'make_figure_landtrendrv04_examine:  there are fewer than the minimum
    print, 'number of years available for disturbance/recovery extraction'
    print, 'the minimum is: '+string(minimum_number_years_needed)
    print, 'the number of files given to extract_disturbance_recovery4.pro: '+string(n_yrs)
    print, 'confirm that the information from find_image_stack_files is correct'
    return, {ok:0}
  end
  
  if divisor eq -1 then message, 'Divisor value must be set by user, not set to -1, for graphing'
  
  
  ;START THE GOOD STUFF
  
  ;now go through and build it.
  offset = (kernelsize-1)/2
  
  min_year = min(info.year)
  x_axis = years
  
  if n_elements(background_val) eq 0 then background_val = 0
  
  vals = fltarr(n_yrs)
  
  ;get the pixel size
  zot_img, info[0].image_file, hdr, blank, /hdronly
  pxsize = hdr.pixelsize
  offset_map = offset * pxsize[0]
  
  subset = [ [point_coords[0]-offset_map, point_coords[1]+offset_map], $
    [point_coords[0]+offset_map, point_coords[1]-offset_map]]
    
  ;make a variable to hold the values and the years
  img = intarr(kernelsize, kernelsize, n_yrs)
  cld_img = bytarr(kernelsize, kernelsize, n_yrs)		;added v4
  usedmask = intarr(kernelsize, kernelsize) ;valide values for years with multiple image
  
  ;which image was used
  idx_img = bytarr(kernelsize, kernelsize, n_yrs)
  
  ;for this point, process everything
  k=0
  
  for i = 0, n_yrs-1 do begin
    ;FIRST CHECK TO SEE HOW MANY IMAGES FOR THIS YEAR
    fileid = i+k
    this = where(info.year eq years[i], n)	;<- N IS THE KEY VARIABLE
    
    ;masks and ids for current year
    cur_mask = bytarr(kernelsize, kernelsize)
    cur_img = intarr(kernelsize, kernelsize)
    cur_idx = bytarr(kernelsize, kernelsize)
    
    ;SINGLE IMAGE FOR THIS YEAR
    if n eq 1 then begin
      tempsubset=subset
      
      landtrendr_image_read, info[fileid], hdr, img1, tempsubset, index, modifier, background_val
      
      sz = size(img1, /dim)
      
      bads = where(img1 eq background_val, n_bads)
      if n_bads ne 0 then cld_img[*,*,i] = (cld_img[*,*,i]+ (img1 eq background_val)) ne 0 	;ne 0 needed incase cloud image and background val!
      
      ;took out divisor here relative to run_tbcd_single_chunk, because will get captured later
      img[*,*,i] = img1/divisor	;added 2/7/08 this will scale to max of 1000
      idx_img[*,*,i] = replicate(fileid, kernelsize, kernelsize)	;set all pixels to this one
      
      ;now read the cloud mask
      ; if there is no cloud mask, then just skip this
      if info[fileid].cloud_file ne 'none' and info[fileid].cloud_file ne '' then begin
        tempsubset=subset
        if info[fileid].cloud_file eq 'band8' then $
          zot_img, info[fileid].image_file, clhdr, mimg, layers=[8], subset=tempsubset else $
          zot_img, info[fileid].cloud_file, clhdr, mimg, subset=tempsubset
        cld_img[*,*,i] = (cld_img[*,*,i] + (mimg gt 2300)) ne 0
      end  ;cloud image
    end		;single image for this year
    
    ;MULTIPLE IMAGES PER YEAR
    ;if multiple image exists for this year, select one and make the others masked out
    if n gt 1 then begin
      victims = info[this]
      ;sort by priority
      vicorder = sort(victims.image_priority)
      victims = victims[vicorder]
      
      ;read in the cloud images for each victim, in order of priority
      for j = 0, n-1 do begin
        tempsubset=subset
        landtrendr_image_read, victims[j], hdr, img1, tempsubset, index, modifier, background_val
        
        ;now read the cloud mask
        ; if there is no cloud mask, then just skip this
        mimg = replicate(0, kernelsize, kernelsize)
        
        if victims[j].cloud_file ne 'none' and victims[j].cloud_file ne '' then begin
          tempsubset=subset
          if victims[j].cloud_file eq 'band8' then $
            zot_img, victims[j].image_file, clhdr, mimg, layers=[8], subset=tempsubset else $
            zot_img, victims[j].cloud_file, clhdr, mimg, subset=tempsubset
        ;cld_img[*,*,fileid+j] = (cld_img[*,*,fildid+j] + (mimg gt 2300)) ne 0
        end
        
        ;identify pixels that are not background, that haven't been picked by the higher
        ;   priority image, and that are not in the cloud mask
        valid = where(img1 ne background_val and cur_mask eq 0 and mimg le 2300, n_valid)
        if n_valid ne 0 then begin
          cur_img[valid] = img1[valid]
          cur_mask[valid] = 1					;mask gets set to 1 if the pixel is chosen
          cur_idx[valid] = replicate(this[vicorder[j]], n_valid)
        end
      endfor
      k = k + n - 1
      img[*,*,i] = cur_img/divisor
      cld_img[*,*,i] = cur_mask ne 1		;any cur_mask pixels still remaining 0 were not chosen
      idx_img[*,*,i] = cur_idx
    end
  end			;accumulating the values for this point
  
  ;DO THE FITTING FOR THIS POINT
  ;  	relative to run_tbcd_single_chunk, here we don't need to re-extract the values
  ;   from the image in a loop, so just figure out usable pixels right away.
  chunk = img
  usable = cld_img eq 0	;identify all good pixels in the window around the point
  slice = total(chunk*usable,1)			;multiple the good ones by the image values (zero out bad values), and total on one dimension
  slice_usable = total(usable, 1)			;identify the usable ones in this slice
  vals = total(slice,1)/total(slice_usable, 1)	;creat the "vals" variable now by dividing by number of good pixels per year
  
  goods= where(cld_img[offset, offset, *] ne 1, ngds)		;just use the central pixel's cloud mask to determine the number of good years
  
  ; goods = where(vals[*,p] ne background_val, ngds)	;
  seed = randomseed()
  
  
  if n_elements(modifier) eq 0 then modifier = 1
  
  ;first, take out the doy effect
  if n_elements(fix_doy_effect) ne 0 then begin
    idxs = idx_img[offset, offset,*]		;use the middle pixel for the day of year -- could cause anomalies if central is weird
    
    uniques = fast_unique(info[goods].julday)
    if n_elements(uniques) gt 4 then begin
      r = poly_fit(info[idxs[goods]].julday, vals[goods],2, chisq=chisq,yfit = yfit)
      m = mean(yfit)
      w, 0
      plot, info[idxs[goods]].julday, vals[goods], psym=4, symsize= 2
      oplot, info[idxs[goods]].julday, yfit, color = 'ffff00'xl, psym=5
      
      zzz = calc_fitting_stats3(vals[goods], yfit, 3, resid=resid)
      if zzz.p_of_f lt pval then outvals = m+resid else $
        outvals = vals[goods]
      oplot, info[idxs[goods]].julday, outvals, psym = 4, color = '00ff00'xl
    end else outvals = vals[goods]
  end else outvals = vals[goods]
  
  return, {ok:1, x:x_axis[goods], y:outvals}
end

