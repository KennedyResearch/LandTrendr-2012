pro cloud_masking_gui_img_dif_event, event

  widget_control, event.top, get_uvalue=infoptr
  info = *infoptr
  ;img321 = *info.img321pr
  ;img543 = *info.img543pr
  ;therm = *info.thermpr
  img321_min = info.img321_min
  img321_max = info.img321_max
  img321_range = info.img321_range
  img543_min = info.img543_min
  img321_max = info.img321_max
  img321_range = info.img321_range
  mastersubset = info.mastersubset
  widget_control, event.id, get_uvalue=selected
  
  ;get the modes
  display_mode = widget_info(info.display_drop, /droplist_select)
  mask_mode = widget_info(info.cldshdw_drop, /droplist_select)
  case mask_mode of
    0: begin
      widget_control, info.shdwmask_base, sensitive=0
      widget_control, info.cldmask_base, sensitive=1
      if info.n_thermal eq 0 then widget_control, info.therm_slider, sensitive=0
    end
    1: begin
      widget_control, info.shdwmask_base, sensitive=1
      widget_control, info.cldmask_base, sensitive=0
    end
    2: begin
      widget_control, info.shdwmask_base, sensitive=1
      widget_control, info.cldmask_base, sensitive=1
      if info.n_thermal eq 0 then widget_control, info.therm_slider, sensitive=0
    end
  endcase
  
  if selected eq 'imgsize' then begin
    listsize = widget_info(info.imgsize_drop, /droplist_select)
    case listsize of
      0L: imgsize = info.s_imgsize+1
      1L: imgsize = info.m_imgsize
      2L: imgsize = info.l_imgsize
    endcase
    
    widget_control, info.display, draw_xsize=imgsize, draw_ysize=imgsize
    
    subset=mastersubset
    zot_img, info.image, hdr, img321, layers = [3,2,1], subset=subset
    maxsize = float(max([hdr.filesize[0],hdr.filesize[1]]))
    denom = float(imgsize/maxsize)
    xsize = round(hdr.filesize[0]*denom)
    ysize = round(hdr.filesize[1]*denom)
    img321 = reverse(congrid(temporary(img321), xsize, ysize, 3),2) ; reverse,2
    info.img321pr = ptr_new(img321)
    
    subset=mastersubset
    zot_img, info.image, hdr, img543, layers = [5,4,3], subset=subset
    img543 = reverse(congrid(temporary(img543), xsize, ysize, 3),2) ; reverse,2
    info.img543pr = ptr_new(img543)
    
    subset=mastersubset
    zot_img, info.diffile, hdr, difimg, subset=subset
    difimg = reverse(congrid(temporary(difimg), xsize, ysize, 4),2) ; reverse,2
    info.difimgpr = ptr_new(difimg)
    
    if info.n_thermal ne 0 then begin
      subset=mastersubset
      zot_img, info.thermal, hdr, therm, layers = [1], subset=subset
      therm = reverse(congrid(temporary(therm), xsize, ysize, 3),2)
      info.thermpr = ptr_new(therm)
    endif
    
    infoptr = ptr_new(info)
    widget_control, info.base, set_uvalue=infoptr
  endif
  
  img321 = *info.img321pr
  img543 = *info.img543pr
  difimg = *info.difimgpr
  
  if info.n_thermal ne 0 then therm = *info.thermpr
  
  ;get the slider values
  widget_control, info.strmax, get_value=max_value
  widget_control, info.strmin, get_value=min_value
  widget_control, info.strtop, get_value=top_value
  widget_control, info.therm_slider, get_value=therm_value
  widget_control, info.b1_slider, get_value=b1_value
  widget_control, info.b4_slider, get_value=b4_value
  widget_control, info.b5_slider, get_value=b5_value
  
  ;get the multipliers
  min_multiplier = float(min_value)/float(1000)
  max_multiplier = float(max_value)/float(1000)
  top_multiplier = float(top_value)/float(1000)
  
  ;find the threshold values
  ;thermal
  if therm_value ne 0 then begin
    therm_multiplier = float(therm_value)/float(2000)
    therm_adj = info.therm_range*therm_multiplier
    thermal_thresh = round(info.therm_min+therm_adj)
  endif else thermal_thresh = -99999
  
  ;band 1
  if b1_value ne 0 then begin
    b1_multiplier = float(b1_value)/float(2000)
    b1_adj = info.b1range*b1_multiplier
    b1_thresh = round(info.b1max+b1_adj)
    print, 'b1_thresh '+string(b1_thresh)
    print, 'b1_adj '+string(b1_adj)
  endif else b1_thresh = -99999
  
  ;band 4
  if b4_value ne 0 then begin
    b4_multiplier = float(b4_value)/float(2000)
    b4_adj = info.b4range*b4_multiplier
    b4_thresh = round(info.b4min+b4_adj)
  endif else b4_thresh = -99999
  
  ;band5
  if b5_value ne 0 then begin
    b5_multiplier = float(b5_value)/float(2000)
    b5_adj = info.b5range*b5_multiplier
    b5_thresh = round(info.b5min+b5_adj)
  endif else b5_thresh = -99999
  
  info.b1_thresh = b1_thresh
  info.thermal_thresh = thermal_thresh
  info.b4_thresh = b4_thresh
  info.b5_thresh = b5_thresh
  
  ;do the masking and displaying
  if display_mode eq 0 then begin
    min_adj = img321_range*min_multiplier
    max_adj = img321_range*max_multiplier
    top_adj = 256*top_multiplier
    newimgmin = round(img321_min+min_adj)
    newimgmax = round(img321_max-max_adj)
    newimgtop = round(255-top_adj)
    img321temp = bytscl(img321, min=newimgmin, max=newimgmax, top=newimgtop)
    
    ;deal with the thermal slider
    if info.n_thermal ne 0 then begin
      if mask_mode eq 0 or mask_mode eq 2 then begin
        zeros = difimg[*,*,3] ge thermal_thresh
      endif
    endif
    
    ;deal with the b1 slider
    if mask_mode eq 0 or mask_mode eq 2 then begin
      thesize = size(zeros)
      if thesize[1] eq 0 then zeros = difimg[*,*,0] ge b1_thresh else zeros = (difimg[*,*,0] ge b1_thresh)*temporary(zeros)
    endif
    
    ;deal with the b4 slider
    if mask_mode eq 1 or mask_mode eq 2 then begin
      thesize = size(zeros)
      if thesize[1] eq 0 then zeros = difimg[*,*,1] ge b4_thresh else zeros = (difimg[*,*,1] ge b4_thresh)*temporary(zeros)
    endif
    
    ;deal with the b5 slider
    if mask_mode eq 1 or mask_mode eq 2 then begin
      thesize = size(zeros)
      if thesize[1] eq 0 then zeros = difimg[*,*,2] ge b5_thresh else zeros = (difimg[*,*,2] ge b5_thresh)*temporary(zeros)
    endif
    
    zeros = where(temporary(zeros) eq 0, n_zeros)
    if n_zeros ne 0 then begin
      colors = [255,255,0]
      for i=0, 2 do begin
        tempimg = img321temp[*,*,i]
        tempimg[zeros] = colors[i]
        img321temp[*,*,i] = temporary(tempimg)
      endfor
    endif
    
    widget_control, info.display, get_value=display_v
    wset,display_v
    tv, temporary(img321temp), 0, true=3
    
  ;endif
  endif
  
  if display_mode eq 1 then begin
    min_adj = info.img543_range*min_multiplier
    max_adj = info.img543_range*max_multiplier
    top_adj = 256*top_multiplier
    newimgmin = round(info.img543_min+min_adj)
    newimgmax = round(info.img543_max-max_adj)
    newimgtop = round(255-top_adj)
    
    img543temp = bytscl(img543, min=newimgmin, max=newimgmax, top=newimgtop)
    
    ;deal with the thermal slider
    if info.n_thermal ne 0 then begin
      if mask_mode eq 0 or mask_mode eq 2 then begin
        zeros = difimg[*,*,3] ge thermal_thresh
      endif
    endif
    
    ;deal with the b1 slider
    if mask_mode eq 0 or mask_mode eq 2 then begin
      thesize = size(zeros)
      if thesize[1] eq 0 then zeros = difimg[*,*,0] ge b1_thresh else zeros = (difimg[*,*,0] ge b1_thresh)*temporary(zeros)
    endif
    
    ;deal with the b4 slider
    if mask_mode eq 1 or mask_mode eq 2 then begin
      thesize = size(zeros)
      if thesize[1] eq 0 then zeros = difimg[*,*,1] ge b4_thresh else zeros = (difimg[*,*,1] ge b4_thresh)*temporary(zeros)
    endif
    
    ;deal with the b5 slider
    if mask_mode eq 1 or mask_mode eq 2 then begin
      thesize = size(zeros)
      if thesize[1] eq 0 then zeros = difimg[*,*,2] ge b5_thresh else zeros = (difimg[*,*,2] ge b5_thresh)*temporary(zeros)
    endif
    
    zeros = where(temporary(zeros) eq 0, n_zeros)
    if n_zeros ne 0 then begin
      colors = [255,255,0]
      for i=0, 2 do begin
        tempimg = img543temp[*,*,i]
        tempimg[zeros] = colors[i]
        img543temp[*,*,i] = temporary(tempimg)
      endfor
    endif

    widget_control, info.display, get_value=display_v
    wset,display_v
    tv, temporary(img543temp), 0, true=3
    
  endif
  
  if display_mode eq 2 then begin
    range_432 = range([ [[info.img543[*,*,1]]] ,[[info.img543[*,*,2]]],[[info.img321[*,*,1]]] ])
    min_432 = min([ [[info.img543[*,*,1]]] ,[[info.img543[*,*,2]]],[[info.img321[*,*,1]]] ])
    max_432 = max([ [[info.img543[*,*,1]]] ,[[info.img543[*,*,2]]],[[info.img321[*,*,1]]] ])
    min_adj = range_432*min_multiplier
    max_adj = range_432*max_multiplier
    top_adj = 256*top_multiplier
    newimgmin = round(min_432+min_adj)
    newimgmax = round(max_432-max_adj)
    newimgtop = round(255-top_adj)
    
    img432temp = bytscl([[[img543[*,*,1]]],[[img543[*,*,2]]],[[img321[*,*,0]]]], min=newimgmin, max=newimgmax, top=newimgtop)
    
    ;deal with the thermal slider
    if info.n_thermal ne 0 then begin
    if mask_mode eq 0 or mask_mode eq 2 then begin
      zeros = difimg[*,*,3] ge thermal_thresh
    endif
    endif
    
    ;deal with the b1 slider
    if mask_mode eq 0 or mask_mode eq 2 then begin
      thesize = size(zeros)
      if thesize[1] eq 0 then zeros = difimg[*,*,0] ge b1_thresh else zeros = (difimg[*,*,0] ge b1_thresh)*temporary(zeros)
    endif
    
    ;deal with the b4 slider
    if mask_mode eq 1 or mask_mode eq 2 then begin
      thesize = size(zeros)
      if thesize[1] eq 0 then zeros = difimg[*,*,1] ge b4_thresh else zeros = (difimg[*,*,1] ge b4_thresh)*temporary(zeros)
    endif
    
    ;deal with the b5 slider
    if mask_mode eq 1 or mask_mode eq 2 then begin
      thesize = size(zeros)
      if thesize[1] eq 0 then zeros = difimg[*,*,2] ge b5_thresh else zeros = (difimg[*,*,2] ge b5_thresh)*temporary(zeros)
    endif
    
    zeros = where(temporary(zeros) eq 0, n_zeros)
    if n_zeros ne 0 then begin
      colors = [255,255,0]
      for i=0, 2 do begin
        tempimg = img432temp[*,*,i]
        tempimg[zeros] = colors[i]
        img432temp[*,*,i] = temporary(tempimg)
      endfor
    endif
    
    widget_control, info.display, get_value=display_v
    wset,display_v
    tv, temporary(img432temp), 0, true=3
    
  endif
  
  if selected eq 'done' then begin
    ;get rid of gaps and image edges
    subset=mastersubset
    zot_img, info.image, hdr, img, /hdronly, subset=subset
    ;mask = bytarr(hdr.filesize[0], hdr.filesize[1])
    for b=0, hdr.n_layers-1 do begin
      subset=mastersubset
      zot_img, info.image, hdr, img, layers=(b+1), subset=subset
      if b eq 0 then mask = temporary(img) ne 0 else $
        mask = temporary(mask)*(temporary(img) ne 0)
    endfor
    
    for b=0, 3 do begin 
       subset=mastersubset
       zot_img, info.diffile, temphdr, tempimg, subset=subset, layers=b+1
       if b eq 3 then begin 
         mask = (temporary(tempimg) ge info.thermal_thresh)*temporary(mask)
       endif
       if b eq 0 then mask = (temporary(tempimg) ge info.b1_thresh)*temporary(mask)
       if b eq 1 then mask = (temporary(tempimg) ge info.b4_thresh)*temporary(mask)
       if b eq 2 then mask = (temporary(tempimg) ge info.b5_thresh)*temporary(mask)
    endfor
        
    ;add a buffer to the mask
    mask = temporary(mask) eq 0
    
    radius = 1
    kernel = SHIFT(DIST(2*radius+1), radius, radius) LE radius
    mask = convol(temporary(mask), kernel) gt 0  ;gt 0
    mask = convol(temporary(mask), kernel) gt 0  ;gt 0
    
    mask = temporary(mask) eq 0
    
    ;delete any cloudmasks for the given file date
    lt_delete_duplicate, info.image, /cloudmask
    
    ;make a filename for the output mask
    filedir = file_dirname(info.image)+path_sep()
    outfile = strcompress(filedir+strmid(file_basename(info.image), 0, 18)+ $
      "_"+timestamp()+"_cloudmask.bsq", /rem)
      
    ;write out the file
    openw, un, outfile, /get_lun
    writeu, un, mask
    free_lun, un
    hdr.pixeltype = 3
    hdr.n_layers = 1
    write_im_hdr, outfile, hdr
    
    ;create the metadata structure
    if info.ledaps eq 0 then begin
      ;cldmaskthresh = [info.thermal_thresh, info.b1_thresh, info.b4_thresh, info.b5_thresh]
      cldmaskthresh = {data:"cloud mask file",filename:file_basename(outfile),$
        band1_threshold:info.b1_thresh,band4_threshold:info.b4_thresh,$
        band5_threshold:info.b5_thresh,band6_threshold:info.thermal_thresh}
      metaout = stringswap(outfile, ".bsq", "_meta.txt")
      ;meta = make_metadata_for_preprocessing(outfile, cldmaskthresh=cldmaskthresh, cldmskversion=2)
      concatenate_metadata, [info.image,info.diffile], metaout, params=cldmaskthresh
    endif
    
    print, ">>> done making cloud/shadow mask"
    widget_control, event.top, /destroy
  endif
end



pro cloud_masking_gui_img_dif, image, ledaps=ledaps

  search = strcompress("*"+strmid(file_basename(image),0,18)+"*b6.bsq")
  thermal = file_search(file_dirname(image), search, count=n_thermal)
  
  ;find the reference file
  search = strcompress("*"+strmid(file_basename(image),0,18)+"*cld_shdw_dif*.bsq")
  diffile = file_search(file_dirname(image), search, count=n_diffile)
  if n_diffile eq 0 then message, "warning! cannot find a *cld_shdw_dif*.bsq file for this image: "+image
  
  ;make a subset
  zot_img, image, hdr, img, /hdronly
  ul = hdr.upperleftcenter
  lr = hdr.lowerrightcenter
  zot_img, diffile, m_hdr, m_img, /hdronly
  if m_hdr.upperleftcenter[0] gt ul[0] then ul[0] = m_hdr.upperleftcenter[0]
  if m_hdr.upperleftcenter[1] lt ul[1] then ul[1] = m_hdr.upperleftcenter[1]
  if m_hdr.lowerrightcenter[0] lt lr[0] then lr[0] = m_hdr.lowerrightcenter[0]
  if m_hdr.lowerrightcenter[1] gt lr[1] then lr[1] = m_hdr.lowerrightcenter[1]
  
  adjed_lr_coords = adj_int_mult_for_madcal([ul[0],ul[1]], [30,30], [lr[0],lr[1]], /map)
  mastersubset = [[ul[0],ul[1]],[adjed_lr_coords]]
  
  ;start the gui stuff
  leader=widget_base(map=0)
  widget_control, leader, /Realize
  base = widget_base(column=2, group_leader=leader, /modal, title='LandTrendr Cloudmask Fixing')
  
  ;set up the display
  s_imgsize = 800
  m_imgsize = 1500
  l_imgsize = 3000
  
  display_size = 800
  display = widget_draw(base, xsize=s_imgsize+1, ysize=s_imgsize+1, x_scroll_size=s_imgsize, y_scroll_size=s_imgsize)
  
  ;setup button and slider base
  stuffbase = widget_base(base, column=1)
  
  label_ysize = 25
  button_ysize = 140
  
  ;set up image size buttons
  drop_base = widget_base(stuffbase, column=1, /frame)
  mode_label = widget_label(drop_base, value='Display and Mode Settings', ysize=label_ysize)
  imgsize_label = widget_label(drop_base, value='Image Size', /align_left)
  imgsize_list = ['Small','Medium','Large']
  imgsize_drop = widget_droplist(drop_base, value=imgsize_list, uvalue='imgsize', xsize=button_ysize)
  
  ;set up cloud/shadow button
  display_label = widget_label(drop_base, value='Display Mode', /align_left)
  display_list = ['RGB/321','RGB/543','RGB/432']
  display_drop = widget_droplist(drop_base, value=display_list, uvalue='displaydrp', xsize=button_ysize)
  
  ;set up masking mode
  cldshdw_label = widget_label(drop_base, value='Masking Mode', /align_left)
  cldshdw_list = ['Cloud','Shadow','Both']
  cldshdw_drop = widget_droplist(drop_base, value=cldshdw_list, uvalue='cldshdwdrp', xsize=button_ysize)
  
  ;set up the visua; adjustment sliders
  visual_base = widget_base(stuffbase, column=1, /frame)
  stretch_label = widget_label(visual_base, value='Stretch Adjustment', ysize=label_ysize, /align_left)
  stretch_min_slider = widget_slider(visual_base, min=0, max=1000, value=0, title='Min Value Adjustment', uvalue='strmin', xsize=button_ysize)
  stretch_max_slider = widget_slider(visual_base, min=0, max=1000, value=0, title='Max Value Adjustment', uvalue='strmax', xsize=button_ysize)
  stretch_top_slider = widget_slider(visual_base, min=0, max=1000, value=0, title='Top Value Adjustment', uvalue='strtop', xsize=button_ysize)
  
  ;set up the cloud mask adjustment sliders
  cldmask_base = widget_base(stuffbase, column=1, /frame)
  cldmask_label = widget_label(cldmask_base, value='Cloud Mask Adjustment', ysize=label_ysize, /align_left)
  if n_thermal ne 0 then therm_slider = widget_slider(cldmask_base, min=0, max=2000, value=0, title='Thermal (Cloud) Adjustment', uvalue='therm', xsize=button_ysize) else $
    therm_slider = widget_slider(cldmask_base, min=0, max=2000, value=0, title='Thermal (Cloud) Adjustment', uvalue='therm', xsize=button_ysize, sensitive=0)
  b1_slider = widget_slider(cldmask_base, min=0, max=2000, value=0, title='Band 1 (Cloud) Adjustment', uvalue='b1', xsize=button_ysize)
  
  ;set up the shadow mask adjustment sliders
  shdwmask_base = widget_base(stuffbase, column=1, /frame, sensitive=0)
  shdwmask_label = widget_label(shdwmask_base, value='Shadow Mask Adjustment', ysize=label_ysize, /align_left)
  b4_slider = widget_slider(shdwmask_base, min=0, max=2000, value=0, title='Band 4 (Shadow) Adjustment', uvalue='b4', xsize=button_ysize)
  b5_slider = widget_slider(shdwmask_base, min=0, max=2000, value=0, title='Band 5 (Shadow) Adjustment', uvalue='b5', xsize=button_ysize)
  
  ;set up the done button
  done_button = widget_button(stuffbase, value='Done Making Mask', ysize=100, uvalue='done')
  
  ;load in the image difference file
  subset=mastersubset
  zot_img, diffile, hdr, difimg, subset=subset
  maxsize = float(max([hdr.filesize[0],hdr.filesize[1]]))
  denom = float(s_imgsize/maxsize)
  xsize = round(hdr.filesize[0]*denom)
  ysize = round(hdr.filesize[1]*denom)
  
  ;get min and max values for the sliders and thresholding adjustments
  subset=mastersubset
  b1max = min(difimg[*,*,0])
  b1range = range(long(difimg[*,*,0]))
  
  b4min = min(difimg[*,*,1])
  b4range = range(difimg[*,*,1])
  b5min = min(difimg[*,*,2])
  b5range = range(difimg[*,*,2])
  therm_min = min(difimg[*,*,3])
  therm_range = range(difimg[*,*,3])
  ;make its image
  difimg = reverse(congrid(temporary(difimg), xsize, ysize, 4),2)
  
  zot_img, image, hdr, img321, layers = [3,2,1], subset=subset
  maxsize = float(max([hdr.filesize[0],hdr.filesize[1]]))
  denom = float(s_imgsize/maxsize)
  xsize = round(hdr.filesize[0]*denom)
  ysize = round(hdr.filesize[1]*denom)
  img321_min = min(img321)
  img321_max = max(img321)
  img321_range = range(img321)
  img321 = reverse(congrid(temporary(img321), xsize, ysize, 3),2) ; reverse,2
  
  zot_img, image, hdr, img543, layers = [5,4,3], subset=subset
  img543_min = min(img543)
  img543_max = max(img543)
  img543_range = range(img543)
  img543 = reverse(congrid(temporary(img543), xsize, ysize, 3),2) ; reverse,2
  
  if n_thermal ne 0 then begin
    zot_img, thermal, hdr, therm, layers = [1], subset=subset
    therm = reverse(congrid(temporary(therm), xsize, ysize, 3),2)
  endif else begin
    therm_min = 'na'
    therm_range = 'na'
    therm = 'na'
  endelse
 
  img321temp = bytscl(img321)
  
  widget_control, base, /realize
  widget_control, display, get_value=display_v
  wset,display_v
  tv, temporary(img321temp), 0, true=3
  
  img321pr = ptr_new(img321)
  img543pr = ptr_new(img543)
  thermpr = ptr_new(therm)
  difimgpr = ptr_new(difimg)
  
  info = {strmin:stretch_min_slider,$
    strmax:stretch_max_slider,$
    strtop:stretch_top_slider,$
    display:display,$
    difimg:difimg,$
    img321:img321,$
    img321_min:img321_min,$
    img321_max:img321_max,$
    img321_range:img321_range,$
    img543:img543,$
    img543_min:img543_min,$
    img543_max:img543_max,$
    img543_range:img543_range,$
    display_drop:display_drop,$
    therm:therm,$
    therm_min:therm_min,$
    therm_range:therm_range,$
    therm_slider:therm_slider,$
    b1_slider:b1_slider,$
    b4_slider:b4_slider,$
    b5_slider:b5_slider,$
    s_imgsize:s_imgsize,$
    m_imgsize:m_imgsize,$
    l_imgsize:l_imgsize,$
    diffile:diffile,$
    image:image,$
    thermal:thermal,$
    imgsize_drop:imgsize_drop,$
    cldshdw_drop:cldshdw_drop,$
    img321pr:img321pr,$
    img543pr:img543pr,$
    thermpr:thermpr,$
    difimgpr:difimgpr,$
    base:base,$
    b1max:b1max,$
    b1range:b1range,$
    b4min:b4min,$
    b4range:b4range,$
    b5min:b5min,$
    b5range:b5range,$
    cldmask_base:cldmask_base,$
    shdwmask_base:shdwmask_base,$
    b1_thresh:0l,$
    b4_thresh:0l,$
    b5_thresh:0l,$
    thermal_thresh:0l,$
    n_thermal:n_thermal,$
    ledaps:keyword_set(ledaps),$
    mastersubset:mastersubset}
    
  infoptr = ptr_new(info)
  
  widget_control, base, set_uvalue=infoptr
  
  xmanager, 'cloud_masking_gui_img_dif', base, /no_block
end
