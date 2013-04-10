function refpicktester, path, from_auto=from_auto, check_ref=check_ref

  ;  from_auto = 1
  ;  path = "f:\4626\"

  ;  if keyword_set(from_auto) eq 1 then begin
  ;    path = from_auto[0]
  print, "finding a cost reference image"
  lt_images = strcompress(path+'images\', /rem)
  image_info = find_landtrendr_files_for_cost(lt_images, /ignore_ref)
  maxyear = max(image_info.year)
  minyear = min(image_info.year)
  targetyear = ((maxyear-minyear)/2) + minyear
  medjulday = fix(median(image_info.julday))
  print, "  target year: ", targetyear
  print, "  median julian day: ", medjulday
  
  ;set up a structure
  base = {file: "",$
    year:1991,$
    day:365,$
    clouds:1.0,$
    sensor:"",$
    difyear:100,$
    difday:100,$
    difmincld:0.0,$
    order:0,$
    cloudorder:0.0,$
    difdayorder:0.0,$
    difyearorder:0.0,$
    cloud_file:"",$
    madcal_mask_file:""}
    
  refpicker = replicate(base, n_elements(image_info))
  
  image_info = vct_repopulate_image_info(image_info, from_auto=from_auto)
  
  ;fill in the structure
  a = n_elements(image_info)
  for i=0, n_elements(image_info)-1 do begin
    refpicker[i].file = image_info[i].image_file
    refpicker[i].difyear = abs(image_info[i].year-targetyear)
    refpicker[i].difday = abs(image_info[i].julday-medjulday)
    refpicker[i].sensor = strmid(file_basename(image_info[i].image_file), 0,3) ;'LE7', 'TM5'
    refpicker[i].year = image_info[i].year
    refpicker[i].day = image_info[i].julday
    refpicker[i].cloud_file = image_info[i].cloud_file
    refpicker[i].madcal_mask_file = image_info[i].madcal_mask_file
    
    if image_info[i].cloud_file eq 'none' then goto, start_here
    ;read in the cloudmasks and figure out how much is "cloud"
    zot_img, image_info[i].cloud_file, hdr, img
    print, "  getting percent clouds: ", file_basename(image_info[i].cloud_file)
    dummy = where(img eq 0, noncloud)
    dummy = where(img eq 2500, cloud)
    percentcloud = float(cloud)/(cloud+noncloud)
    print, " ", percentcloud *100,"%"
    refpicker[i].clouds = percentcloud
    start_here:
  endfor
  ;  endif
  
  ;narrow down the potential ref images
  refpicker = refpicker[where(refpicker.cloud_file ne 'none')]
  mincloud = min(refpicker.clouds) ;get the minimum cloud value
  refpicker.difmincld = refpicker.clouds - mincloud ;get the differene of all cloud amounts from the minimum
  
  ;get rid of the slc-off images
  ;goods1 = refpicker.sensor ne "LE7"
  ;goods2 = refpicker.year lt 2003
  ;goods3 = (goods1+goods2) gt 0
  goods3 =  refpicker.sensor ne "LE7" ;get rid of ETM+ images as potential cost ref images
  refpicks = refpicker[where(goods3 eq 1)]
  refpicks = refpicks[where(refpicks.difmincld lt 0.1)] ;pull out files that have cloud amounts within 10% of the minimum value
  
  ;if 10% does not grab 4 images then loosen the threshold to 20% then finally 30%
  n_refpicks = n_elements(refpicks)
  if n_refpicks lt 4 then refpicks = refpicks[where(refpicks.difmincld lt 0.2)]
  n_refpicks = n_elements(refpicks)
  if n_refpicks lt 4 then refpicks = refpicks[where(refpicks.difmincld lt 0.3)]
  
  ;for the found files go through and rank them by nearness to median julday, middle of the time series, and amount of cloud
  for i=0, n_elements(refpicks)-1 do refpicks[i].order = i ;mark their order so it can be referenced later
  
  cloudsort = sort(refpicks.difmincld) ;sort the difference from the min cloud amount
  difdaysort = sort(refpicks.difday) ;sort the difference from median day
  difyearsort = sort(refpicks.difyear) ;srt the difference from middle year
  
  ;the cost ref img is based on a weighted order of least to greatest for dif from median julday, median year, and dif from min cloud
  ;cloud carries the greatest weight, followed by day of year then year
  for i=0, n_elements(refpicks)-1 do begin
    theone = where(cloudsort[i] eq refpicks.order) ;from sorted dif from min cloud choices, figure out which one matches the iteration
    refpicks[theone].cloudorder = i*.10 ;once it's found, multiply it's place in the order (iteration) by a weight
    theone = where(difdaysort[i] eq refpicks.order)
    refpicks[theone].difdayorder = i*.25
    theone = where(difyearsort[i] eq refpicks.order)
    refpicks[theone].difyearorder = i*.65
  endfor
  
  imgrank = refpicks.cloudorder+refpicks.difdayorder+refpicks.difyearorder
  sorted = sort(imgrank)
  
  ;---evaluate the top 4 reference picks---
  if keyword_set(check_ref) eq 1 then begin 

    
    picks = cost_ref_display(refpicks, sorted, title, targetyear, medjulday)
    
;    
;    print, "preparing selected ref images for viewing..." 
;    picks = replicate(create_struct("f","","doy","","year",""),4)
;    for i=0,3 do begin
;      print, "  ..."
;      picks[i].f = strcompress(refpicks[where(sorted eq i)].file, /rem)
;      file = strcompress(file_basename(picks[i].f), /rem)
;      picks[i].year = strcompress(strmid(file, 10, 4), /rem)
;      picks[i].doy = strcompress(strmid(file, 15, 3), /rem)
;      zot_img, picks[i].f, hdr, img, layers = [3,2,1]
;      xsize = round(hdr.filesize[0]/20)
;      ysize = round(hdr.filesize[1]/20)
;      if i eq 0 then img1 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
;      if i eq 1 then img2 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
;      if i eq 2 then img3 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
;      if i eq 3 then img4 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
;    endfor
;    ;
;    ;  pickedfile1 = refpicks[where(sorted eq 0)].file
;    ;  pickedfile2 = refpicks[where(sorted eq 1)].file
;    ;  pickedfile3 = refpicks[where(sorted eq 2)].file
;    ;  pickedfile4 = refpicks[where(sorted eq 3)].file
;    ;
;    ;
;    ;  zot_img, pickedfile1, hdr, img, layers = [3,2,1]
;    ;  xsize = round(hdr.filesize[0]/20)
;    ;  ysize = round(hdr.filesize[1]/20)
;    ;  img1 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
;    ;  zot_img, pickedfile2, hdr, img, layers = [3,2,1]
;    ;  xsize = round(hdr.filesize[0]/20)
;    ;  ysize = round(hdr.filesize[1]/20)
;    ;  img1 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
;    ;  zot_img, pickedfile1, hdr, img, layers = [3,2,1]
;    ;  xsize = round(hdr.filesize[0]/20)
;    ;  ysize = round(hdr.filesize[1]/20)
;    ;  img1 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
;    ;  zot_img, pickedfile2, hdr, img, layers = [3,2,1]
;    ;  xsize = round(hdr.filesize[0]/20)
;    ;  ysize = round(hdr.filesize[1]/20)
;    ;  img1 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
;    
;    
;    title = strcompress("target year: "+string(targetyear)+" :: target day-of-year: "+string(medjulday)+" :: "+$
;      "top>bottom left>right... 1: "+picks[0].year+" "+picks[0].doy+" :: 2: "+picks[1].year+" "+picks[1].doy+" :: 3: "+$
;      picks[2].year+" "+picks[2].doy+" :: 4: "+picks[3].year+" "+picks[3].doy)
;    
;    print, ">>> which image is the best: 1, 2, 3, or 4?"
;    print, ">>> this order is the algorithm's top 4 picks"
;    print, ">>> the window title describes the positions"
;    window, xsize=900, ysize=900, title=title;create a window to hold all of the regression tifs
;    device, decomposed=1
;    tv, img1, 0, true=3
;    tv, img2, 1, true=3
;    tv, img3, 2, true=3
;    tv, img4, 3, true=3
;    device, decomposed=0
;    
;    print, " "
;    print, ">>> which image is the best: 1, 2, 3, or 4?"
;    print, ">>> type your answer. example: 1"
  
    
    
    print, "what image was best?"
    print, "type: 1, 2, 3 or 4"
    therefimg = get_kbrd()
    if therefimg eq '1' then reffile = picks[0].f
    if therefimg eq '2' then reffile = picks[1].f
    if therefimg eq '3' then reffile = picks[2].f
    if therefimg eq '4' then reffile = picks[3].f
    
    return, reffile
    
  ;
  ;
  ;  WINDOW, 0, XSIZE = imageSize[0], YSIZE = 3*imageSize[1], $
  ;   TITLE = 'The Channels of an RGB Image'
  ;TV, redChannel, 0, 0
  ;TV, greenChannel, 0, imageSize[1]
  ;TV, blueChannel, 0, 2*imageSize[1]
  ;
  ;
  ;
  ;
  ;  device, decomposed=0
  ;
  ;
  ;  xsize = round(hdr.filesize[0]/18)
  ;  ysize = round(hdr.filesize[1]/18)
  ;  img1 = reverse(congrid(img, xsize, ysize),2)
  ;
  ;
  ;  window, xsize=1200, ysize=900, title="", 7 ;create a window to hold all of the regression tifs
  ;  loadct,0 ;load a color table
  ;  tv, img1, order=1
  ;  TVImage,reverse(img1,3),true=1 ;display the images in the window
  ;
  ;
  ;;  band1 = img
  ;;
  ;;  zot_img, image, hdr, img, layers = 2
  ;;  band2 = img
  ;;  zot_img, image, hdr, img, layers = 3
  ;;  band3 = img
  ;;
  ;;  DEVICE, DECOMPOSED = 1
  ;;  LOADCT, 0
  ;;  WINDOW, 0, XSIZE = hdr.filesize[0], YSIZE = hdr.filesize[1], $
  ;;   TITLE = 'image 1: 7/31/2001'
  ;;  TVSCL, band1, 0 > 30
  ;;  TV, band2, 1 > 30
  ;;  TV, band3, 2
  ;;
  ;;  WINDOW, 0, XSIZE = imageSize[0], YSIZE = imageSize[1], $
  ;;   TITLE = 'Glowing Gas RGB Image'
  ;;  TV, image, TRUE = 1
  ;;
  ;;
  ;;  tvscl, img, 0
  ;;  TVSCL, Image [, Position] [, /CENTIMETERS] [, /INCHES] [, /NAN] [, /ORDER] [, TOP=value] [, TRUE={1 | 2 | 3}] [, /WORDS] [, XSIZE=value] [, YSIZE=value
  ;
  ;  ;check if the image is ok
  endif else return, pickedfiles[0]
end