function ls_madcal_ref_selector, path, from_auto=from_auto, check_ref=check_ref, trgtday=trgtday, trgtyear=trgtyear

  print, ">>> finding a cost reference image"
  lt_images = strcompress(path+'images\', /rem)
  image_info = find_landtrendr_files_for_cost(lt_images, /ignore_ref)
  maxyear = max(image_info.year)
  minyear = min(image_info.year)
  if keyword_set(trgtyear) eq 1 then targetyear = trgtyear else targetyear = ((maxyear-minyear)/2) + minyear
  if keyword_set(trgtday) eq 0 then medjulday = fix(median(image_info.julday)) else medjulday=trgtday
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
  
  image_info = vct_repopulate_image_info(image_info, /from_auto)
  
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
    dummy = where(img eq 1, noncloud)
    dummy = where(img eq 0, cloud)
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
  ;goods3 = (goods1+goods2) gt 0 ;this goods3 would include TM and ETM+ SLC-on
  goods3 =  refpicker.sensor ne "LE7" ;get rid of ETM+ images as potential cost ref images - Yang advised using only TM sensor 9/15/11 JDB 
  refpicks = refpicker[where(goods3 eq 1)]
  refpicks1 = refpicks[where(refpicks.difmincld lt 0.1)] ;pull out files that have cloud amounts within 10% of the minimum value
  
  ;if 10% does not grab 4 images then loosen the threshold to 20% then finally 30%
  n_refpicks1 = n_elements(refpicks1)
  if n_refpicks1 lt 4 then refpicks1 = refpicks[where(refpicks.difmincld lt 0.2)]
  n_refpicks1 = n_elements(refpicks1)
  if n_refpicks1 lt 4 then refpicks1 = refpicks[where(refpicks.difmincld lt 0.3)]
  n_refpicks1 = n_elements(refpicks1)
  if n_refpicks1 lt 4 then refpicks1 = refpicks[where(refpicks.difmincld lt 0.4)]
  n_refpicks1 = n_elements(refpicks1)
  if n_refpicks1 lt 4 then refpicks1 = refpicks[where(refpicks.difmincld lt 0.5)]
  
  ;for the found files go through and rank them by nearness to median julday, middle of the time series, and amount of cloud
  for i=0, n_elements(refpicks1)-1 do refpicks1[i].order = i ;mark their order so it can be referenced later
  
  cloudsort = sort(refpicks1.difmincld) ;sort the difference from the min cloud amount
  difdaysort = sort(refpicks1.difday) ;sort the difference from median day
  difyearsort = sort(refpicks1.difyear) ;srt the difference from middle year
  
  ;the cost ref img is based on a weighted order of least to greatest for dif from median julday, median year, and dif from min cloud
  ;cloud carries the greatest weight, followed by day of year then year
  for i=0, n_elements(refpicks1)-1 do begin
    theone = where(cloudsort[i] eq refpicks1.order) ;from sorted dif from min cloud choices, figure out which one matches the iteration
    refpicks1[theone].cloudorder = i*.10 ;once it's found, multiply it's place in the order (iteration) by a weight
    theone = where(difdaysort[i] eq refpicks1.order)
    refpicks1[theone].difdayorder = i*.25
    theone = where(difyearsort[i] eq refpicks1.order)
    refpicks1[theone].difyearorder = i*.65
  endfor
  
  imgrank = refpicks1.cloudorder+refpicks1.difdayorder+refpicks1.difyearorder
  sorted = sort(imgrank)
  
  ;---evaluate the top 4 reference picks---
  if keyword_set(check_ref) eq 1 then begin 

    picks = ls_madcal_ref_display(refpicks1, sorted, title, targetyear, medjulday)
   
    print, ""
    print, ">>> what image was best?"
    print, ">>> type: 1, 2, 3 or 4"
    print, ""
    therefimg = get_kbrd()
    if therefimg eq '1' then reffile = picks[0].f
    if therefimg eq '2' then reffile = picks[1].f
    if therefimg eq '3' then reffile = picks[2].f
    if therefimg eq '4' then reffile = picks[3].f
    
    return, reffile
    
  endif else return, picks[0]
end