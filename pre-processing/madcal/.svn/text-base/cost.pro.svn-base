pro cost, path, cost_ref_img=cost_ref_img, from_auto=from_auto, check_ref=check_ref
  ;This program\function creates a reference image used to radiometricly normalized
  ;all other images to.  It converts DN values to reflecance values and applies
  ;the COST atmoshperic correction model (Chavez, 1996).

  ;MTL headers for the images must be in a folder inside of the "images" folder
  ;DO NOT use ETM+ SLC-OFF images

  ;check to see if a landsat reference image already exists
  wilco = file_search(path+"images\", "*radref.bsq", count=nthese) ;find the radref image
  if nthese ge 1 then begin
    wilco = file_search(path, "*radref*", count=nthese) ;if one exists find all associated files
    wilcogoods = where(strmatch(wilco, "*txt") ne 1 and wilco ne '', n_wilco)
    if n_wilco ge 1 then begin
      wilco = wilco[wilcogoods]
      repeat begin
        print, ""
        print, ">>> a prepared LANDSAT reference image..."
        print, ">>> already exists, do you want to overwrite it?"
        print, ">>> it is recommended that you do..."
        print, ">>> otherwise you're going to run into..."
        print, ">>> troubles later - select an option"
        print, ">>> NO (press 1), YES (press 2) STOP, (press 3)
        b = get_kbrd()
      endrep until b eq 1 or b eq 2 or b eq 3
      print, ""
      if b eq 2 then begin
        close, /all
        ;find the normalized images and delete them since they'll all have to change
        normimgs = file_search(path, "*_to_*")
        normimgs_index = where(strmatch(normimgs, "*txt") ne 1 and strmatch(normimgs, "*pro") $
          ne 1 and normimgs ne '', n_normings)
        if n_normings ge 1 then begin
          normimgs = normimgs[normimgs_index]
          file_delete, wilco, normimgs
        endif else file_delete, wilco
        
        tengo = file_search(path, "*_orig*", count=n_tengo)
        if n_tengo ge 1 then begin
          for i=0, n_tengo-1 do begin
            tengolen = strlen(tengo[i])
            tengopos = strpos(tengo[i], "_orig")
            tengopart1 = strmid(tengo[i],0,tengopos)
            tengopart2 = strmid(tengo[i], tengopos+5, tengolen-tengopos+5)
            humptydumpty = strcompress(tengopart1+tengopart2, /rem)
            file_move, tengo[i], humptydumpty
          endfor
        endif
      endif
      if b eq 3 then stop
    endif
  endif
  
  if keyword_set(from_auto) eq 1 then begin
    ;unpack from_auto
    ls_madcal_ref_img = from_auto.ls_madcal_ref_img
    manual_drk_obj = from_auto.dark_object_vals
    ;check to see if the user gave a ref image
    info = size(ls_madcal_ref_img)
    type = info[1]
    if type eq 7 then begin
      searchfor = strcompress("*"+strmid(file_basename(ls_madcal_ref_img),0,40)+"*.bsq", /rem)
      ls_madcal_ref_img = file_search(path+"images\", searchfor, count=n_ls_madcal_ref_img)
      if n_ls_madcal_ref_img eq 1 then ls_ref_img = ls_madcal_ref_img else message, "something is not right about the selected reference image"
    endif else ls_madcal_ref_img = ls_madcal_ref_selector(path, /from_auto, /check_ref)
  endif
  
  ;now that we have a reference file, find the MTL header
  name = file_basename(ls_madcal_ref_img)
  year = strmid(name, 10, 4)
  doy = strmid(name, 15, 3)
  
  ;---find the mtl image header for the cost ref image---
  print, "setting up the cost model"
  print, "  finding the MTL metadata"
  image_dir = strcompress(path+"\images\", /rem)
  
  ydn2md, year, doy, m, d
  
  month = strtrim(string(m),2)
  day = strtrim(string(d),2)
  
  if month lt 10 then month = '0'+month else month=month
  if day lt 10 then day = '0'+day else day=day
  
  mtl = strcompress('*'+string(year)+"_"+doy+"_MTL.txt", /rem)
  ;mtl = strcompress('*'+string(year)+month+day+'_MTL.txt',/remove_all) ;old 9/14/11 JDB
  mtl_hdr = file_search(image_dir, mtl)
  
  ;---check that it found an actual file and print name---
  if file_test(mtl_hdr) eq 0 then message, "MTL header does not exist for reference image"
  
  ;---read in the mtl header---
  openr, lun, mtl_hdr, /get_lun
  file_size = file_lines(mtl_hdr,/noexpand_path)
  file_list = strarr(file_size)
  readf, lun, file_list
  free_lun, lun
  
  lmax_search = ["*LMAX_BAND1*", "*LMAX_BAND2*", "*LMAX_BAND3*", $
    "*LMAX_BAND4*", "*LMAX_BAND5*", "*LMAX_BAND7*", "*SUN_ELEVATION*"]
    
  lmin_search = lmax_search
  min_rad = lmax_search
  max_rad = lmax_search
  for i=0, n_elements(lmax_search)-1 do lmin_search[i] = stringswap(lmax_search[i], "MAX", "MIN")
  for i=0, n_elements(lmax_search)-1 do begin
    for j=0, n_elements(file_list)-1 do begin
      theone = strmatch(file_list[j], lmax_search[i])
      if theone eq 1 then begin
        if strmatch(file_list[j], "*QC*") ne 1 then begin
          theonesplit = strsplit(file_list[j], "=", /extract)
          max_rad[i] = float(strtrim(theonesplit[1],2))
        endif
      endif
      theone = strmatch(file_list[j], lmin_search[i])
      if theone eq 1 then begin
        if strmatch(file_list[j], "*QC*") ne 1 then begin
          theonesplit = strsplit(file_list[j], "=", /extract)
          min_rad[i] = float(strtrim(theonesplit[1],2))
        endif
      endif
    endfor
  endfor
  
  sunelev = float(max_rad[n_elements(min_rad)-1]) ;find the sun elevation
  sunzen =  (90-sunelev) ;calculate the sun zenith angle
  
  min_rad = float(min_rad[0:n_elements(min_rad)-2])
  max_rad = float(max_rad[0:n_elements(max_rad)-2])
  
  gain = (max_rad - min_rad)/255  ;calulate the gains
  bias = min_rad  ;calulate the biases
  
  if n_elements(manual_drk_obj) gt 1 then goto, start_here
  ;---figure out what the dark object values should be---
  ;---only consider pixels that are not cloud and other masked vct masked cover---
  ;zot_img, cost_ref_cld, cldhdr, cldimg
  ;ul = cldhdr.UPPERLEFTCENTER
  ;lr = cldhdr.LOWERRIGHTCENTER
  ;subset = [[ul], [lr]]
  
  zot_img, ls_madcal_ref_img, costhdr, costimg, /hdronly
  
  the_message = "there are"+string(costhdr.n_layers) + " layers, there should be 6!?"
  
  if costhdr.n_layers ne 6 then message, the_message
  
  drk_obj_vals = create_struct('drkobj', 1)
  drk_obj_vals = replicate(drk_obj_vals, 6)
  
  for i=0, costhdr.n_layers-1 do begin
    if i eq 5 then it = 7 else it = i+1
    print, "  finding dark object value for band: ", it
    zot_img, ls_madcal_ref_img, costhdr, costimg, layers=i+1;, subset=subset
    ;cldimg = [[[cldimg]],[[cldimg]],[[cldimg]],$
    ;          [[cldimg]],[[cldimg]],[[cldimg]]]
    ;costimg[where(cldimg gt 0)] = 0
    
    ;---make a histogram---
    histb1 = float(histogram(costimg))
    
    ;goods = where(costimg[*,*,0] gt 0, count)
    ;print, count
    n_pixels = total(histb1)  ;find the total number of pixels in the image   n = 73550040. falls apart
    
    ;prop_histb1 = histb1/count  ;calculate the proportion of each DN value to the total image
    ;prop_histb2 = histb2/count
    ;prop_histb3 = histb3/count
    ;prop_histb4 = histb4/count
    ;prop_histb5 = histb5/count
    ;prop_histb6 = histb6/count
    
    prop_histb1 = histb1/n_pixels  ;calculate the proportion of each DN value to the total image
    prop_histb1_shft = float(shift(prop_histb1, -1))  ;create a shifted copy of the DN proportion histogram for differencing adjacent DNs
    phb1sd = float(prop_histb1_shft-prop_histb1)  ;calculate the difference in DN proportion of adjacent neighbors
    phb1sd[where(phb1sd le 0)] = .0000001   ;get rid of differences of 0, as they mess up the next statement and are not of interest
    b1dn = where(phb1sd ge .0001)  ;select from the differenced adjacent DN proportions the value at which the difference is greater than .01% of the pixels - the value was established by trial and error and seems pretty consistent
    
    ;put a filter in based on the min standard deviation from mean dark object values for all processed scenes per band
    if i eq 0 then b1dn = b1dn[where(b1dn ge 24)]
    if i eq 1 then b1dn = b1dn[where(b1dn ge 10)]
    if i eq 2 then b1dn = b1dn[where(b1dn ge 5)]
    if i eq 3 then b1dn = b1dn[where(b1dn ge 0)]
    if i eq 4 then b1dn = b1dn[where(b1dn ge 0)]
    if i eq 5 then b1dn = b1dn[where(b1dn ge 0)]
    
    drk_obj_vals[i].drkobj = b1dn[0]
    
  endfor
  
  ;---set the dark object value offsets---
  drkobj_offset = [-1,-1,-1,-2,-2,-1]
  
  ;---adjust the dark object values by the offests---
  start_here:
  if n_elements(manual_drk_obj) gt 1 then drkobj = manual_drk_obj + drkobj_offset else $
    drkobj = drk_obj_vals.drkobj + drkobj_offset
    
  bad_drkobj=where(drkobj lt 0, count)
  
  if count gt 0 then drkobj[where(drkobj lt 0)] = 0 ;if dark object values are less than 0 (band 5,6 sometimes)force them to be 0
  ;---check to see that the dark object minimum values seem OK---
  ave_drk_obj = [['ave. 32, stdev. 9'],['ave. 14, stdev. 3'],['ave. 8, stdev. 2'],$  ;average and standard deviation for all of the scenes we've processed
    ['ave. 4, stdev. 2'],['ave. 1, stdev. 2'],['ave. 1, stdev. 1']]
    
  print, ""
  print, '>>> Average dark object minimum values:'
  print, ave_drk_obj
  print, ''
  print, '>>> Dark object minimum values:'
  print, transpose(drkobj)
  print, ''
  print, 'Are these values OK (y) or not (n)?'
  print, 'Press "y" or "n" (lower case, no quotes)'
  
  a = get_kbrd()
  if a ne 'y' then begin
    print, ""
    print, ">>> you don't like the dark object values...
    print, ">>> go back to the batchfile and change them or...
    print, ">>> add ones under the cost section
    return
  endif
  ;print, drkobj
  
  ;---read in the cost reference image and run COST on it---
  print, ""
  print, ">>> applying cost model to: " + ls_madcal_ref_img
  print, ""
  dpr = 57.2958 ;degrees per radian
  solirr = [1957, 1826, 1554, 1036, 215, 80.67] ;solar irradiance
  
  ;---write out the cost image---
  dir = file_dirname(ls_madcal_ref_img)
  name = file_basename(ls_madcal_ref_img, ".bsq")
  new_name = strcompress(strmid(name,0,18)+"_"+timestamp()+"_radref.bsq", /rem)
  outfile = strcompress(dir+"\"+new_name, /rem)
  
  next_start = 0
  for i=0, 5 do begin
    zot_img, ls_madcal_ref_img, costhdr, costimg, layers=(i+1)
    
    costimg = [[fix(round(400*3.14159*(((costimg*gain[i])+bias[i])-((drkobj[i]*gain[i])+bias[i]))/ $
      (cos(sunzen/dpr)* cos(sunzen/dpr)* solirr[i])))]]
      
    negs = where(costimg lt 0, n_negs)
    
    if n_negs ge 1 then costimg[negs] = 0  ;if values are less than 0 force them to be 0
    
    if i eq 0 then openw, un, outfile, /get_lun else $  ;and it eq 1
      openu, un, outfile, /get_lun, /append
    point_lun, un, next_start
    writeu, un, costimg
    point_lun, -un, next_start
    free_lun, un
  endfor
  ;costimg[where(costimg lt 0)] = 0  ;if values are less than 0 force them to be 0
  ;cost[where(cost gt 255)] = 255   ;implement this when we make the switch to using 8-bit
  
  ;  ;---write out the cost image---
  ;  dir = file_dirname(ls_madcal_ref_img)
  ;  name = file_basename(ls_madcal_ref_img, ".bsq")
  ;  new_name = strcompress(name+"_"+timestamp()+"_radref.bsq", /rem)
  ;  outfile = strcompress(dir+"\"+new_name, /rem)
  
  ;  openw, un, outfile, /get_lun
  ;  writeu, un, costimg
  ;  free_lun, un
  costhdr.pixeltype = 6   ;signed 16bit
  write_im_hdr, outfile, costhdr
  
  ;create the metadata structure
  meta = make_metadata_for_preprocessing(outfile, darkobjval=drkobj, /cost)
  metaout = stringswap(outfile, ".bsq", "_meta.txt")
  concatenate_metadata, ls_madcal_ref_img, metaout, params=meta
  
  ;deal with renaming the original archv file so that it does not get picked up in image_info
  thelen = strlen(ls_madcal_ref_img)-4
  new_name_base = strtrim(strmid(ls_madcal_ref_img, 0, thelen),2)
  searchthis = strcompress("*"+file_basename(new_name_base)+"*", /rem)
  thisdir = file_dirname(ls_madcal_ref_img)
  thefiles = file_search(thisdir, searchthis, count=thefilescount)
  if thefilescount ge 1 then begin
    ends = strarr(thefilescount)
    new_name = strarr(thefilescount)
    for i=0, thefilescount-1 do begin
      filelen = strlen(thefiles[i])
      ends[i] = strmid(thefiles[i],thelen,filelen)
      ;ends = strtrim(strmid(thefiles,thelen+1),2)
      new_name[i] = strcompress(new_name_base+"_orig"+ends[i], /rem)
    endfor
    file_move, thefiles, new_name
  endif
  
  print, ">>> done creating COST reference image
end