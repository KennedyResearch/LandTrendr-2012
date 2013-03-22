function seg_params, path, ppprrr, segparamstxt, image_info_savefile, mask_image=mask_image, subset=subset, eval=eval

  restore, image_info_savefile
  
  ;open the txt file
  openr, lun, segparamstxt, /get_lun
  file_size = file_lines(segparamstxt,/noexpand_path)
  file_list = strarr(file_size)
  readf, lun, file_list
  free_lun, lun
  
  ;get the variable and value separated
  lines = strarr(n_elements(file_list),2)
  for i=0, n_elements(file_list)-1 do begin
    if file_list[i] eq '' then continue
    split = strsplit(file_list[i], '=', /extract)
    lines[i,0] = strtrim(split[0],2)
    lines[i,1] = strtrim(split[1],2)
  endfor
  
  ;figure out how many sets of parameters (runs) were passed
  runs = where(lines[*,0] eq 'run_name', n_runs)
  
  theruns = {run_name: '',$
    index: '' ,$
    background_val: 0l ,$
    divisor: 1s ,$
    minneeded: 6s ,$
    kernelsize: 3s ,$
    pval: 0.05 ,$
    fix_doy_effect: 1s ,$
    max_segments: 6s ,$
    recovery_threshold: 0.5 ,$
    skipfactor: 1s ,$
    desawtooth_val: 0.9 ,$
    distweightfactor: 2s ,$
    vertexcountovershoot: 3s ,$
    bestmodelproportion: 0.0 ,$
    mask_image: '' ,$
    subset: [[0.0,0.0],[0.0,0.0]] ,$
    output_base: '',$
    image_info:image_info}
    
  theruns = replicate(theruns, n_runs)
  
  ;go through each set and package it for processing
  for i=0, n_runs-1 do begin
    theruns[i].index = lines[(where(lines[*,0] eq 'base_index'))[i],1]
    theruns[i].background_val = long(lines[(where(lines[*,0] eq 'background_val'))[i],1])
    theruns[i].divisor = fix(lines[(where(lines[*,0] eq 'divisor'))[i],1])
    theruns[i].minneeded = fix(lines[(where(lines[*,0] eq 'minneeded'))[i],1])
    theruns[i].kernelsize = fix(lines[(where(lines[*,0] eq 'kernelsize'))[i],1])
    theruns[i].pval = float(lines[(where(lines[*,0] eq 'pval'))[i],1])
    theruns[i].fix_doy_effect = fix(lines[(where(lines[*,0] eq 'fix_doy_effect'))[i],1])
    theruns[i].max_segments = fix(lines[(where(lines[*,0] eq 'max_segments'))[i],1])
    theruns[i].recovery_threshold = float(lines[(where(lines[*,0] eq 'recovery_threshold'))[i],1])
    theruns[i].skipfactor = fix(lines[(where(lines[*,0] eq 'skipfactor'))[i],1])
    theruns[i].desawtooth_val = float(lines[(where(lines[*,0] eq 'desawtooth_val'))[i],1])
    theruns[i].distweightfactor = fix(lines[(where(lines[*,0] eq 'distweightfactor'))[i],1])
    theruns[i].vertexcountovershoot = fix(lines[(where(lines[*,0] eq 'vertexcountovershoot'))[i],1])
    theruns[i].bestmodelproportion = float(lines[(where(lines[*,0] eq 'bestmodelproportion'))[i],1])
    if theruns[i].run_name ne 'na' then theruns[i].output_base = path+"outputs\"+theruns[i].index+"\LT_"+landtrendr_version()+"_"+theruns[i].index+"_"+ppprrr+"_"+theruns[i].run_name else $
      theruns[i].output_base = path+"outputs\"+theruns[i].index+"\LT_"+landtrendr_version()+"_"+theruns[i].index+"_"+ppprrr
      
    ;deal with the mask_image_keyword
    if keyword_set(mask_image) eq 1 then begin
      if file_exists(mask_image) ne 1 then begin ;warning about not finding a file
        print, ">>> apparently a mask image for segmentation..."
        print, ">>> was defined by the user, but it can't be found..."
        print, ">>> or it does not exists, check this file:"
        print, mask_image
        stop
      endif else theruns[i].mask_image = mask_image
    endif else begin
      ;since a mask image was not passed as a keyword find out if it is in the params txt
      goods = string(lines[(where(lines[*,0] eq 'mask_image'))[i],1])
      if goods eq '' then begin
        print, ">>> warning!!!"
        print, ">>> trying to define a mask image for segmentation..."
        print, ">>> but there was not one passed as a keyword and..."
        print, ">>> one was not given in the segmentation parameter text file..."
        print, ">>> either define a mask file in the text file or pass one..."
        print, ">>> as a keyword to this fuction"
        stop
      endif else theruns[i].mask_image = goods
    endelse
    
    
    
    ;deal with the subset_keyword
    if keyword_set(subset) eq 1 then theruns[i].subset = subset else begin
      ;figure out if a subset was given in the params txt file
      goods = string(lines[(where(lines[*,0] eq 'ulx'))[i],1])
      if goods eq 'na' then begin
        zot_img, theruns[i].mask_image, hdr, img, /hdronly
        mask_subset = [[hdr.upperleftcenter], [hdr.lowerrightcenter]]
        ok = find_union_area(image_info_savefile, subset=mask_subset, /checkmask)
        if ok.ok ne 1 then stop
        usethissubset = ok.coords
        theruns[i].subset = usethissubset
      endif else begin
        ulx = float(lines[(where(lines[*,0] eq 'ulx'))[i],1])
        uly = float(lines[(where(lines[*,0] eq 'uly'))[i],1])
        lrx = float(lines[(where(lines[*,0] eq 'lrx'))[i],1])
        lry = float(lines[(where(lines[*,0] eq 'lry'))[i],1])
        theruns[i].subset = [[ulx,uly],[lrx,lry]]
      endelse
    endelse
    if keyword_set(eval) eq 1 then theruns[i].skipfactor = 3s
  endfor
  return, theruns
end
