function parse_seg_params, path, ppprrr, segparamstxt, image_info_savefile, mask_image=mask_image, subset=subset, eval=eval, resume=resume, forcenew=forcenew

  restore, image_info_savefile
  
;------ identify path separator -------
  pse = path_sep()


  ;open the txt file
  openr, lun, segparamstxt, /get_lun
  file_size = file_lines(segparamstxt,/noexpand_path)
  file_list = strarr(file_size)
  readf, lun, file_list
  free_lun, lun
  
  ;make holders for the variables and values
  variable = strarr(n_elements(file_list))
  value = strarr(n_elements(file_list))
  
  ;make a run params structure for each set passed
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
    subset: [[0.0D,0.0D],[0.0D,0.0D]] ,$
    output_base: '',$
    output_path: "", $	;added REK march 28 2013
    image_info:image_info}
    
  firsttime = 1 ;set first time to true
  for j=0, n_elements(file_list)-1 do begin
    if file_list[j] eq '' then continue
    split = strsplit(file_list[j], '=', /extract)
    variable[j] = strtrim(split[0],2)
    value[j] = strtrim(split[1],2)
    if firsttime eq 1 then begin
      n_runs = n_elements(strsplit(value[j], ',', /extract))
      theruns = replicate(theruns, n_runs)
      ulx = strarr(n_runs)
      uly = strarr(n_runs)
      lrx = strarr(n_runs)
      lry = strarr(n_runs)
      firsttime = 0 ;set first time to false
    endif
    ;go through each set and package it for processing
    splitup = strtrim(strsplit(value[j], ',', /extract),2)
    if n_elements(splitup) ne n_runs then begin
      print, ">>> parsing out the segmentation parameter file..."
      print, ">>> checking to see if all variables have the same..."
      print, ">>> number of entries and it appears as though one or more..."
      print, ">>> variables does not have the same number of entries as..."
      print, ">>> the variable 'run_name' - please fix this and rerun"
      print, ""
      print, ">>> ending program"
      print, ""
      stop
    endif
    
    ;fill in the variables with their values
    for i=0, n_runs-1 do begin
      if variable[j] eq 'base_index' then theruns[i].index = splitup[i]
      if variable[j] eq 'background_val' then theruns[i].background_val = long(splitup[i])
      if variable[j] eq 'divisor' then theruns[i].divisor = fix(splitup[i])
      if variable[j] eq 'minneeded' then theruns[i].minneeded = fix(splitup[i])
      if variable[j] eq 'kernelsize' then theruns[i].kernelsize = fix(splitup[i])
      if variable[j] eq 'pval' then theruns[i].pval = float(splitup[i])
      if variable[j] eq 'fix_doy_effect' then theruns[i].fix_doy_effect = fix(splitup[i])
      if variable[j] eq 'max_segments' then theruns[i].max_segments = fix(splitup[i])
      if variable[j] eq 'recovery_threshold' then theruns[i].recovery_threshold = float(splitup[i])
      if variable[j] eq 'skipfactor' then theruns[i].skipfactor = fix(splitup[i])
      if variable[j] eq 'desawtooth_val' then theruns[i].desawtooth_val = float(splitup[i])
      if variable[j] eq 'distweightfactor' then theruns[i].distweightfactor = fix(splitup[i])
      if variable[j] eq 'vertexcountovershoot' then theruns[i].vertexcountovershoot = fix(splitup[i])
      if variable[j] eq 'bestmodelproportion' then theruns[i].bestmodelproportion = float(splitup[i])
      
      if variable[j] eq 'run_name' then if splitup[i] ne 'na' then theruns[i].run_name = splitup[i]+"_" else theruns[i].run_name = ""
      
      ;deal with the mask_image_keyword
      if variable[j] eq 'mask_image' then begin
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
          if splitup[i] eq '' then begin
            print, ">>> warning!!!"
            print, ">>> trying to define a mask image for segmentation..."
            print, ">>> but there was not one passed as a keyword and..."
            print, ">>> one was not given in the segmentation parameter text file..."
            print, ">>> either define a mask file in the text file or pass one..."
            print, ">>> as a keyword to this fuction"
            stop
          endif
          if splitup[i] ne 'na' then begin
            if file_exists(splitup[i]) eq 0 then begin
              print, ">>> warning!!! the given mask file in the segmentation parameter file..."
              print, ">>> does not exists - please check this file:"
              print, "    ",splitup[i]
              print, ""
              print, ">>> ending program"
              print, ""
              close
            endif else theruns[i].mask_image = splitup[i]
          endif
        endelse
        
        ;deal with the subset_keyword
        if keyword_set(subset) eq 1 then theruns[i].subset = subset else begin
          ;get the subset from the mask image and the input images
          zot_img, theruns[i].mask_image, hdr, img, /hdronly
          mask_subset = [[hdr.upperleftcenter], [hdr.lowerrightcenter]]
          ;ok = find_union_area(image_info_savefile, subset=mask_subset, /checkmask)
          ;if ok.ok ne 1 then stop
          ;usethissubset = ok.coords
          theruns[i].subset = mask_subset
        endelse
      endif
      ;deal with subsets coords given in the params file
      if variable[j] eq 'ulx' then ulx[i] = splitup[i]
      if variable[j] eq 'uly' then uly[i] = splitup[i]
      if variable[j] eq 'lrx' then lrx[i] = splitup[i]
      if variable[j] eq 'lry' then lry[i] = splitup[i]
      
      ;adjust skipfactor and kernal size if running in evaluation mode
      if keyword_set(eval) eq 1 then begin
        theruns[i].skipfactor = 3
        theruns[i].kernelsize = 3
      endif
    endfor ;for i=0, n_runs-1 do begin
  endfor  ;for j=0, n_elements(file_list)-1 do begin
  
  for i=0, n_runs-1 do begin
    ;the given coords in the parms txt file overwrite the subset of the find union area if ;they don't equal 'na'
    if ulx[i] ne 'na' and uly[i] ne 'na' and $
      lrx[i] ne 'na' and lry[i] ne 'na' then theruns[i].subset = double([[ulx[i],uly[i]],[lrx[i],lry[i]]])
      
    ;initial base name creation
    base_outputpath = path+"outputs"+pse+theruns[i].index+pse ;added REK march 28
    theruns[i].output_path = base_outputpath	;added REK march 28
    outbase = path+"outputs"+pse+theruns[i].index+pse+"LT_"+landtrendr_version()+"_"+theruns[i].index+"_"+ppprrr+"_"
    
    
    ;check to see if an eval output set exists for this scene already
   
if keyword_set(eval) eq 1 then begin
      evals = file_search(file_dirname(outbase),"*eval*diag.sav", count=n_evals)
      if n_evals ge 1 then begin
        pos = strpos(evals, "eval")
        checkit = strmid(evals,(pos+4),2)
        evalnum = max(fix(checkit))+1
        
        if keyword_set(resume) eq 1 then begin
          ftvs = strmatch(evals, '*ftv*')
          goods = where(ftvs eq 0, ngftv)
          pos2 = strpos(evals[goods], "eval")
          evalnum2 = bytarr(ngftv)
          for gf = 0, ngftv-1 do evalnum2[gf] = strmid(evals[goods[gf]], (pos[goods[gf]]+4), 2)
          highest = max(where(evalnum2 eq max(evalnum2)))
          theruns[i].output_base = strmid(evals[goods[highest]], 0, strlen(evals[goods[highest]])-9)
          
          
          
        end else begin
           if evalnum lt 10 then name = strcompress("eval"+'0'+string(evalnum)+"_", /rem) $
           else name = strcompress("eval"+strtrim(string(evalnum),2)+"_", /rem)
           theruns[i].output_base = outbase+name+timestamp()
        end
      endif else begin
        name = "eval01_"
        theruns[i].output_base = outbase+name+timestamp()
      end 
    endif else begin 
      ;changing the logic here so that it will assume we're resuming a run, and have to be forced to 
      ;generate a new timestamp. 
        
	;first see if there is one there with this run name already
	
	previous_file = file_search(file_dirname(outbase), '*'+theruns[i].run_name+'*_diag.sav', count=pn)
  ftvs = matchstr(previous_file, "ftv")
  nftvs = n_elements(ftvs)
  if nftvs[0] ne -1 then begin
    complementftv = bytarr(n_elements(previous_file))
    complementftv[ftvs] = -1
    use =where(complementftv eq 0,pn)
    previous_file = previous_file[use]
  end  
    
    
  
        if pn gt 1 then previous_file = previous_file[pn-1]	;set to latest one if multiples
	
	;then either use that previous one, or generate a new one.  If "force_new", then 
	;   generate a new one regardless

	 if (pn gt 0) and (keyword_set(force_new) eq 0) then $
	 	theruns[i].output_base = stringswap(previous_file, "_diag.sav", "") else $
		theruns[i].output_base = outbase+theruns[i].run_name+timestamp()
	
    endelse
  endfor
  
  return, theruns
end
