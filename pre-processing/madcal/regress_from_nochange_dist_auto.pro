;function regress_from_nochange_dist_auto, nochange_info,  output_base, dfile, wfile, ignore=ignore, $
;      screen_prob = screen_prob, apply_to_entire_image=apply_to_entire_image, $
;      dist_screen = dist_screen

function regress_from_nochange_dist_auto, nochange_info,  output_base, ignore=ignore, $
    screen_prob = screen_prob, apply_to_entire_image=apply_to_entire_image, $
    dist_screen = dist_screen, depmask, eightbit=eightbit, modis_ref=modis_ref
    
  ;if there are .tifs delete them, they'll cause problems in madcal_for_tbcd_auto - these are tif used to create the madcal summary tiffs
  tiffiles = file_search(file_dirname(nochange_info.fname2), "*.tif", count=n_tiffiles)
  if n_tiffiles ge 1 then file_delete, tiffiles
  
  ;#################################################################################################
  ;find the subset of the three ref and dep images and the nochange raster
  ;first, assess the smallest area common to all
  zot_img, nochange_info.nochange_file, noch_hdr, mask_nochange, /hdronly
  zot_img, nochange_info.fname1, h1, img1, /hdronly
  zot_img, nochange_info.fname2, h2, targetimg, /hdronly
  
  subset = [ [max([noch_hdr.upperleftcenter[0], h1.upperleftcenter[0], h2.upperleftcenter[0]]), $
    min([noch_hdr.upperleftcenter[1], h1.upperleftcenter[1], h2.upperleftcenter[1]])], $
    [min([noch_hdr.lowerrightcenter[0], h1.lowerrightcenter[0], h2.lowerrightcenter[0]]), $
    max([noch_hdr.lowerrightcenter[1], h1.lowerrightcenter[1], h2.lowerrightcenter[1]])] ]
 
  ;create a master subset
  orig_subset = subset
  
  ;#################################################################################################
  
  ;create the madcal display images for the madcal tifs
  zot_img, depmask, depmaskhdr, depmaskimg, /hdronly
  zot_img, nochange_info.fname2, hdr, img, /hdronly
  
  temp_subset = [ [max([depmaskhdr.upperleftcenter[0], hdr.upperleftcenter[0]]), $
    min([depmaskhdr.upperleftcenter[1], hdr.upperleftcenter[1]])], $
    [min([depmaskhdr.lowerrightcenter[0], hdr.lowerrightcenter[0]]), $
    max([depmaskhdr.lowerrightcenter[1], hdr.lowerrightcenter[1]])] ]
    
  master_temp_subset = temp_subset
  temp_subset = master_temp_subset
  zot_img, depmask, depmaskhdr, depmaskimg, subset=temp_subset
  temp_subset = master_temp_subset
  zot_img, nochange_info.fname2, hdr, img, layers = [3,2,1], subset=temp_subset
  
  maxsize = float(max([depmaskhdr.filesize[0],depmaskhdr.filesize[1]]))
  denom = float(400/maxsize)
  xsize = round(depmaskhdr.filesize[0]*denom)
  ysize = round(depmaskhdr.filesize[1]*denom)
  
  ;reduce the size of the raw image
  img1 = reverse(hist_equal(congrid(img, xsize, ysize, 3),  minv=10, maxv=175),2)
  
  ;apply the mask to the dependent image
  temp_subset = master_temp_subset
  zot_img, nochange_info.fname2, hdr, img, layers = [3,2,1], subset=temp_subset
  for r=0, 2 do begin
    if r eq 2 then img[*,*,r] = img[*,*,r]*temporary(depmaskimg) else img[*,*,r] = img[*,*,r]*depmaskimg
  endfor
  
  ;reduce the size of teh masked image
  img = reverse(hist_equal(congrid(temporary(img), xsize, ysize, 3),  minv=10, maxv=175),2)
  
  ;display the reduced size images
  ;display the masked image
  title = file_basename(nochange_info.fname2)
  window, 0, xsize=400, ysize=400, title=title;create a window to hold all of the regression tifs
  tv, temporary(img), 0, true=3
  bbb = tvrd(/true)
  output_tiff = strcompress( output_base+'_b123mask.tif', /rem)
  write_tiff, output_tiff, reverse(temporary(bbb),3)
  
  ;display the raw image
  window, 0, xsize=400, ysize=400, title=title;create a window to hold all of the regression tifs
  tv, temporary(img1), 0, true=3
  bbb = tvrd(/true)
  output_tiff = strcompress( output_base+'_b123.tif', /rem)
  write_tiff, output_tiff, reverse(temporary(bbb),3)
  ;#################################################################################################
  
  ;set up to match with radcal patterns, for use in orthoregress
  num_channels = h2.n_layers
  aa= fltarr(num_channels) ; slope of orthogonal regression curve
  xm = fltarr(num_channels); mean of X
  ym = fltarr(num_channels); mean of Y
  
  ;make a structure to hold onto the data
  base = {slope:0., intercept:0., slope_sigma:0., intercept_sigma:0., $
    rma_slope:0., rma_intercept:0., rma_rmse:0., $
    stats_diff: fltarr(4), corr:0., n_nochng:0U}
  band_stats = replicate(base, num_channels)  ;to store information on the fits
  
  ;go through each layer, find the coefficients
  next_start = 0
  
  it = 0 ;starter
  endthis = 0.0001 ;starter
  done = 0 ;on off switch
  for i = 0, num_channels-1 do begin
    print, "processing band: ", i+1
    it = it+1
    subset = orig_subset
    zot_img, nochange_info.nochange_file, noch_hdr, mask_nochange, subset=subset
    
    mask_nochange = where(mask_nochange eq 1, num_nochange)
    
    indices1 = where(indgen(n_elements(mask_nochange)) mod 3,complement=indices2)
    mask_test = mask_nochange[indices2]
    mask_train = mask_nochange[indices1]
    n_test = n_elements(mask_test)
    n_train = n_elements(mask_train)
    n_nochange = n_elements(mask_nochange)
    
    ;make an output base
    output_file = output_base+'.bsq'
    
    if it gt 1 then free_lun, un
    if i eq 0 then openw, un, output_file, /get_lun else $  ;and it eq 1
      openu, un, output_file, /get_lun, /append
    point_lun, un, next_start    ;
    ;print, "logical_unit: ",un  ;needed this to figure out where the logical unit accumulation was coming from
        
    ;Landsat to MODIS band correspondence (from Yang 9/14/11)
    ;Landsat, MODIS, sequence in the LT stack
    ;1, 3, 1
    ;2, 4, 2
    ;3, 1, 3
    ;4, 2, 4
    ;5, 6, 5
    ;7, 7, 6
    if keyword_set(modis_ref) eq 1 then begin
      if i eq 0 then layer = 3
      if i eq 1 then layer = 4
      if i eq 2 then layer = 1
      if i eq 3 then layer = 2
      if i eq 4 then layer = 6
      if i eq 5 then layer = 7
    endif else layer = i+1
    
    subset = orig_subset
    zot_img, nochange_info.fname1, h1, img1, layers=layer, subset =subset
    img1 = transpose(img1[mask_train])  ;transpose to work in orthoregress
    
    subset = orig_subset
    zot_img, nochange_info.fname2, h2, targetimg, layers=[i+1], subset=subset
    img2 = transpose(targetimg[mask_train])
    subset=orig_subset
    
    ;screen for probability
    if n_elements(screen_prob) ne 0 then begin
      ok = screen_vals_by_prob(img1, img2, threshold = screen_prob)
      if ok.ok eq 0 then begin
        free_lun, un
        return, {ok:-1, bad_band:i+1, notes:'_screen vals caused problems_'}
      endif
      img1_screen= ok.x
      img2_screen= ok.y
    endif else begin
      img1_screen = img1
      img2_screen = img2
    endelse
    
    ;take these screened values from here forward
    ;check for ignore values
    if n_elements(ignore) ne 0 then begin
      goodsx = img2_screen ne ignore
      goodsy = img1_screen ne ignore
      goods = where( (goodsx * goodsy) eq 1)
    endif else goods=lindgen(n_train)   ;if no ignore, just set up reference to all of the elements
    
    subsetgoods = goods    ;set to the whole population now, we'll later const
    base = {slope:0., intercept:0., correlation:0., xm:0., ym:0., aa:0., sigma_aa:0., sigma_bb:0.}
    
    iter_tracker = replicate(base, 10)
    
    q=0 ;it tracker
    endthis = 0.0001  ;this a starter variable that keeps track of the correlation; if the correlation is ge .98 no more screening takes place
    repeat begin ;do this screening until a worse correlation is returned
      print, "band screening iteration: ", q+1
      ortho_regress, transpose(img2_screen[goods[subsetgoods]]), transpose(img1_screen[goods[subsetgoods]]), ai, xmi, ymi, sigma_aa, sigma_bb
      aa[i]=ai
      xm[i]=xmi
      ym[i]=ymi
      if q ge 1 then bbb = tvrd(10+i, /true)  ;this saves the previous filter
      
      window,10+i,xsize=400,ysize=400,xpos=10*i,ypos=10*i,title="Regression All No-change"
      
      ok = make_2d_hist_image_ff(img2_screen[goods[subsetgoods]], img1_screen[goods[subsetgoods]], window = 10+i, $
        nbins = [100,100], blowup=3,background = 255, $
        percent_cutoffs=[0.0, 1.0], $  ;[0.01, 0.99] - jb likes the display with 0.0 and 1.0
        xtitle="uncalibrated",ytitle="reference", $
        title="channel"+string(i+1), $
        plotrange = plotrange)
        
      Oplot,[0,plotrange[1]],[ym[i]-aa[i]*xm[i],ym[i]-aa[i]*xm[i]+plotrange[1]*aa[i]],thick = 2, color='0000ff'xl
      xyouts, .7, .2, 'ORTHO_REGRESS', /norm, color = '0000ff'xl
      
      aaa = tvrd(10+i, /true) ;this saves the current filter
      
      ;get the counts associated with the values in the original arrays
      counts = ascribe_2d_hist_vals(img2_screen[goods[subsetgoods]], img1_screen[goods[subsetgoods]], ok.histvals, ok.min1, ok.min2, ok.max1, ok.max2, ok.bin1, ok.bin2)
      
      ;scale it
      count_score = (1-(float(counts) / max(counts)))+1 ;make the lowest count get the highest count_score
      ;because of penalty of low counts. range is [1:4]
      
      iter_tracker[q].slope = aa[i]
      iter_tracker[q].intercept = ym[i]-aa[i]*xm[i]
      iter_tracker[q].correlation = correlate(img1_screen[goods[subsetgoods]], img2_screen[goods[subsetgoods]]) ;correlate the subset
      iter_tracker[q].aa = aa[i]
      iter_tracker[q].xm = xm[i]
      iter_tracker[q].ym = ym[i]
      iter_tracker[q].sigma_aa = sigma_aa
      iter_tracker[q].sigma_bb = sigma_bb
      
      ;NEW WITH DIST VERSION:   Use distance from the regrssion line to help filter
      dists = perp_dist( iter_tracker[q].slope,  iter_tracker[q].intercept, img2_screen[goods], img1_screen[goods], /absolute)  ;get the distance from the whole population
      ;gives the distance, with near-zero value having high counts and near line
      ;  want to filter out the distant points
      
      ;now take the dists and multiply by the count score, to make lower counts get shoved further away
      dists = dists*count_score
      
      w = c_prob(dists, bin_count_min = 20) ;get the cumulative distribution.  we're interest in screening out the high stuff -- over, say, 98%.  whatever
      ;the screen probability is.
      subset_threshold = max(where(w lt (1.0-dist_screen), many))
      if many eq 0 then begin
        print, '>>> Screen probability is too high...'
        print, '>>> keeping the given subset'
        n_narrowed = n_elements(dists)
      endif else subsetgoods = where(dists lt subset_threshold, n_narrowed)    ;constrain for next run
      print, 'slope'+string(iter_tracker[q].slope)
      print, 'intercept'+string(iter_tracker[q].intercept)
      print, 'n post filtering = '+string(n_narrowed)
      print, 'correlation = '+string(iter_tracker.correlation)
      dif = iter_tracker[q].correlation-endthis
      endthis = iter_tracker[q].correlation
      
      if endthis ge 0.98 then bestone = q else bestone = q-1
      q=q+1 ;prepare for next iteration if needed
      if dif le 0 then done = 1 ;done means that a worse correlation was returned
    endrep until done eq 1 or endthis ge 0.98 or q eq 10 or n_narrowed lt 1000
    done = 0 ;reset for possible next iteration of the image screening
    
    if bestone eq -1 then bestone = 0
    aa[i] = iter_tracker[bestone].slope
    intercept = iter_tracker[bestone].intercept
    slope = aa[i]
    xm[i] = iter_tracker[bestone].xm
    ym[i] = iter_tracker[bestone].ym
    sigma_aa = iter_tracker[bestone].sigma_aa
    sigma_bb = iter_tracker[bestone].sigma_bb
    correlation = iter_tracker[bestone].correlation
    
    ;assign values
    band_stats[i].slope = slope
    band_stats[i].slope_sigma = sigma_aa
    band_stats[i].intercept = intercept
    band_stats[i].intercept_sigma = sigma_bb
    band_stats[i].corr = correlation
    band_stats[i].n_nochng = n_narrowed
    
    output_tiff = strcompress( output_base+'_ch'+string(i)+'.tif', /rem)
    if endthis ge 0.98 or bestone eq 0 then write_tiff, output_tiff, reverse(temporary(aaa),3) else write_tiff, output_tiff, reverse(temporary(bbb),3)
        
    ;calibrate and write out using the orthoregress coefficients
    ;first, read in the whole targetimg and then
    ;  apply it to the whole image. so just read int he
    ;  image without the subset.  7/19/05
    
    subset = orig_subset
    if n_elements(apply_to_entire_image) eq 0 then $ 
    zot_img, nochange_info.fname2, h2, targetimg, subset=subset, layers=[i+1] else $
      zot_img, nochange_info.fname2, h2, targetimg, layers=[i+1]
      
    ;identify pixels that are ignore values so we maintain them as ignore
    t_ignore = where(targetimg eq ignore, n_t_ig)
    
    if keyword_set(eightbit) eq 1 then begin
      img3 = round(intercept+targetimg*slope)
      img3[where(img3 gt 255)] = 255
      img3[where(img3 lt 0)] = 0
      img3 = byte(img3)
    endif else img3 = fix(round(intercept+targetimg*slope))
    
    if keyword_set(modis_ref) eq 1 then begin
      over = where(img3 gt 32767, overcount)
      if overcount ge 1 then img3[over] = 32767
      under = where(img3 lt 0, undercount)
      if undercount ge 1 then img3[under] = 0
    endif
    
    ;reset the pixels that had the ignore value on input
    if n_t_ig ne 0 then img3[t_ignore]=ignore
    
    writeu, un, img3
    
    ;get the pointer to the end of the file
    point_lun, -un, next_start
     
    ;do the comparison on the test pixels
    mom = moment(img1[mask_test]-img3[mask_test])
    band_stats[i].stats_diff = mom
    
    free_lun, un
    
  endfor   ;i  through layers.
  
  ;write out header for calibrated image
  h = h2
  h.n_layers = num_channels
  if keyword_set(eightbit) eq 1 then h.pixeltype = 3 else h.pixeltype = 6
  write_im_hdr, output_base+'.hdr', h
  
  ;save the info
  info = {band_stats:band_stats, calibrated_image:output_file, $
    mask_train:mask_train, mask_test:mask_test}
    
  savefile = output_base+'.sav'
  save, info, file = savefile
  
  wdelete
  wdelete
  wdelete
  wdelete
  wdelete
  wdelete
  wdelete
  
  subset=orig_subset
  return, {ok:1, savefile:savefile, band_stats:band_stats, calibrated_image:output_file, header:h, subset:subset}
  
end