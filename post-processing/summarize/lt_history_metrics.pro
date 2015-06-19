;+
; NAME:
;       LT_HISTORY_METRICS
;
; PURPOSE:
;
;  This function create a standard set of historical variables from Landtrendr output.
;  For this routine, consecutive segment of the same type is merged together.
;
;  required input files are:
;       -   *_fitted.bsq
;       -   *_segmse.bsq
;       -   *_vertvals.bsq
;       -   *_vertyrs.bsq
;       -   *_brightness_ftv_fitted.bsq
;       -   *_greenness_ftv_fitted.bsq
;       -   *_wetness_ftv_fitted.bsq
;
;       potential input file:
;       -   *_brightness_ftv_source.bsq
;       -   *_greenness_ftv_source.bsq
;       -   *_wetness_ftv_source.bsq
;
;  calculated metrics variables are:
;       B, G, W at time t  (1-3)
;       Slope of B, G, W at time t (4-6)
;       B, G, W of recent vertex (7-9)
;       time since recent vertex (10)
;       B, G, W of prior vertex (11-13)
;       Duration of prior segment (14)
;
; AUTHOR:
;
; CATEGORY:
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;   each of the calculated metrics is written as a stack of images for the selected time intervals.
;
; EXAMPLE:
;
; MODIFICATION HISTORY
;	REK:  March 2013.  Culling metrics: removing recent vertex, prior vertex, duration of prior segment
;	      March 2013.  Adding in constraint to filter by the greatest_disturbance and longest_recovery layers
;	      			Pixels that are not turned on in either of those two will be considered flat lines, 
;				and the history metrics will reflect that. 
				
;   TODO:
;     1. add subset defintion
;******************************************************************************************;
FUNCTION lt_history_metrics, diagfile, end_year=end_year, start_year=start_year, run=run, $
			suffix=suffix, subset=subset, output_corename=output_corename, $
			maskyes=maskyes, recovery_mask_override=recovery_mask_override

  IF n_elements(diagfile) EQ 0 THEN return, -1
  IF n_elements(suffix) EQ 0 THEN suffix = 'ltmetrix'
  IF n_elements(end_year) EQ 0 THEN end_year=-1
  IF n_elements(start_year) EQ 0 THEN start_year=-1
  IF n_elements(run) EQ 0 THEN run=''
  IF n_elements(maskyes) eq 0 then maskyes = 0	;defaults to NOT using the distrec images for filtering

  ;TODO: how to better handle the file search, currently assuming there is only one
  ; for each file type, but potentially there could be multiple.
  
  ;diag_file = file_search(lt_outputs_path+path_sep()+'*_'+run+'_diag.sav', count=n)
  ;IF n NE 1 THEN message, 'Diag file not found.'
  
  ;sv_file = file_search(lt_outputs_path+path_sep()+'*'+run+'_fitted.bsq', count=n)
  ;IF n NE 1 THEN message, 'Fitted file not found.'
  
  ;se_file = file_search(lt_outputs_path+path_sep()+'*'+run+'_segmse.bsq', count=n)
  ;IF n NE 1 THEN message, 'Segmse file not found.'
  
;  vv_file = file_search(lt_outputs_path+path_sep()+'*_'+run+'_vertvals.bsq', count=n)
;  IF n NE 1 THEN message, 'Vertvals file not found.'
;  
;  vy_file = file_search(lt_outputs_path+path_sep()+'*_'+run+'_vertyrs.bsq', count=n)
;  IF n NE 1 THEN message, 'Vertyrs file not found.'
;  
;  ftv_tcb_file = file_search(lt_outputs_path+path_sep()+'*'+run+'brightness_ftv_fitted.bsq', count=n)
;  IF n NE 1 THEN message, 'FTV brightness file not found.'
;  
;  ftv_tcg_file = file_search(lt_outputs_path+path_sep()+'*'+run+'greenness_ftv_fitted.bsq', count=n)
;  IF n NE 1 THEN message, 'FTV greenness file not found.'
;  
;  ftv_tcw_file = file_search(lt_outputs_path+path_sep()+'*'+run+'wetness_ftv_fitted.bsq', count=n)
;  IF n NE 1 THEN message, 'FTV wetness file not found.'


  diag_file = diagfile
  vv_file = stringswap(diagfile, "_diag.sav", "_vertvals.bsq")
  vy_file = stringswap(diagfile, "_diag.sav", "_vertyrs.bsq")
  ftv_tcb_file = stringswap(diagfile, "_diag.sav", "_brightness_ftv_fitted.bsq")
  ftv_tcg_file = stringswap(diagfile, "_diag.sav", "_greenness_ftv_fitted.bsq")
  ftv_tcw_file = stringswap(diagfile, "_diag.sav", "_wetness_ftv_fitted.bsq")
  ftv_nbr_file = stringswap(diagfile, "_diag.sav", "_nbr_ftv_fitted.bsq")	;june 19 2013 rek


  ;get the greatest_disturbance*loose.bsq file, and the longest_recovery file. Anywhere that 
  ;   does not have a value in these two will have a mean value 
  ; added march '13 REK
  
    diagdir = file_dirname(diag_file)+path_sep()
    diagbase = file_basename(diag_file)
    basename = strmid(diagbase, 0, strlen(diagbase)-9)
   if maskyes eq 1 then begin

     distmask_file = file_search(diagdir, basename + '*greatest_disturbance*loose.bsq')
     recmask_file = file_search(diagdir, basename + '*greatest_recovery*loose.bsq')

      ;it's possible the user wants to override with a special recovery layer
     if n_elements(recovery_mask_override) ne 0 then begin
	rmi = file_info(recovery_mask_override)
	if rmi.exists ne 1 then message, 'could not find '+recovery_mask_override
	recmask_file = recovery_mask_override
     end


      if n_elements(distmask_file) gt 1 then begin
	;could be that we have second greatest disturbance, etc.
	;first get the base nam
	base=file_basename(distmask_file)
	ap = strpos(base, 'greatest_disturbance')
	;find the shortest one.  then see if the other has the same timetamp
	mp = min(ap)
	distmask_file = distmask_file[where(ap eq mp)]
	if n_elements(distmask_file) gt 1 then begin
	  ;could be that files are same name but different directories - Jamie
	  distmask_file = distmask_file[0]
	end
	  
      end
      
      ;stop - Jamie

    ;if n_elements(distmask_file) ne 1 then message, 'Greatest disturbance search found this:'+distmask_file
      if n_elements(recmask_file) ne 1 then message, 'Recovery search found this: '+recmask_file else if recmask_file eq '' then message, 'no recovery mask found for '+recmask_file

    end else distmask_file=(recmask_file="");  if masking is set.  

  
  ;retrieve image info
  restore, diag_file
  all_years = diag_info.image_info.year
  unique_years = fast_unique(all_years)
  all_years = unique_years[sort(unique_years)]
  modifier = get_modifier(diag_info.index)
 
 
 ;if user specify output location and name
  if keyword_set(output_corename) then begin 
    core_name = output_corename
    if file_test(get_pathname(core_name), /dir) eq 0 then file_mkdir, get_pathname(core_name)
  endif else core_name = stringswap(vv_file, '_vertvals.bsq', '')
 
  ;set up diagnosis file, and if it doesn't exist, set up stuff

  histdiagfile = core_name + 'history.sav'
 done = 0	;initialize assuming this has not been completed yet
 
  if file_test(histdiagfile) eq 1 then begin
  restore, histdiagfile 
;   hist_info=$
;    {chunks:chunks, $
;    pixels_per_chunk:pixels_per_chunk, $
;    n_chunks:n_chunk, $
;    current_chunk:current_chunk, $
;    output_image_group:output_image_group, $
;    start_year:start_year, $
;    end_year:end_year, $
;    diag_info:diag_info}

   pixels_per_chunk=hist_info.pixels_per_chunk
   chunks=hist_info.chunks
   n_chunks=hist_info.n_chunks
   current_chunk=hist_info.current_chunk
   core_name=hist_info.core_name
   output_image_group=hist_info.output_image_group
   start_year=hist_info.start_year
   end_year=hist_info.end_year
   diag_info=hist_info.diag_info
   n_years = hist_info.n_years
if current_chunk eq n_chunks then return, {ok:1, hist_info:hist_info, msg:'Already completely processed'}	;set here so we don't reprocess everything. 
   

  end else begin
     ;verify start_year and end_year
  tmp_years = [start_year, end_year]
  start_year = min(tmp_years)
  end_year = max(tmp_years)
  
  if start_year lt 0 or start_year lt min(all_years) then start_year = min(all_years)
  if end_year lt 0 or end_year gt max(all_years) then end_year = max(all_years)
  
  if start_year gt max(all_years) or end_year lt min(all_years) then message, "Improper start_year and/or end_year value"
  
  n_years = end_year - start_year + 1
  ;    src_tcb_file = file_search(lt_outputs_path+path_sep()+'*'+run+'brightness_ftv_source.bsq', count=n)
  ;    IF n NE 1 THEN message, 'FTV brightness file not found.'
  ;
  ;    src_tcg_file = file_search(lt_outputs_path+path_sep()+'*'+run+'greenness_ftv_source.bsq', count=n)
  ;    IF n NE 1 THEN message, 'FTV greenness file not found.'
  ;
  ;    src_tcw_file = file_search(lt_outputs_path+path_sep()+'*'+run+'wetness_ftv_source.bsq', count=n)
  ;    IF n NE 1 THEN message, 'FTV wetness file not found.'
  ;
  
  
  
  output_image_group = ["_b_t.bsq", "_g_t.bsq", "_w_t.bsq", $
    "_db_t.bsq", "_dg_t.bsq", "_dw_t.bsq", $
    			;"_rv_b.bsq", "_rv_g.bsq", "_rv_w.bsq", $
    "_ts_v.bsq", $
    "_nbr_t.bsq", "_dnbr_t.bsq"]	;	added june 19 2013 rek, $
    			;"_pv_b.bsq", "_pv_g.bsq", "_pv_w.bsq", $
    			;"_pdur.bsq"]
    
  ;determine output image dimension
  if n_elements(subset) ne 0 then $
    zot_img, vv_file, hdr, vv_img,  subset=subset, /hdronly else $
    zot_img, vv_file, hdr, vv_img,  /hdronly
    
    
  ;now create the output file
  n_output_layers = n_years
  bytes_per_pixel = 2
  layersize = long(hdr.filesize[0]) * hdr.filesize[1] * bytes_per_pixel
  filesize = ulong64(layersize) * n_output_layers

  for i = 0, n_elements(output_image_group)-1 do begin
    this_file = core_name + output_image_group[i]
    
    openw, un, this_file, /get_lun
    point_lun, un, filesize - bytes_per_pixel
    ;a blank pixel
    writeu, un, 0
    free_lun, un         ;now the file exists on the drive.
    hdr1 = hdr
    hdr1.n_layers = n_output_layers
    hdr1.pixeltype = 6
    write_im_hdr, this_file, hdr1
  end   ;pre-create image
  
  ;now define the chunks to process
  ; now define the chunks
  max_pixels_per_chunk = 200000l
  pixsize = hdr.pixelsize
  subset = [[hdr.upperleftcenter], [hdr.lowerrightcenter]]
  kernelsize = 1
  
  ok = define_chunks3(subset, pixsize, max_pixels_per_chunk, kernelsize)
  
  if ok.ok eq 0 then message, 'error creating chunks'
  
  chunks = ok.subsets
  pixels_per_chunk = ok.pixels_per_chunk
  n_chunks = n_elements(chunks)
  current_chunk = 0
  
  ;set up diag structure
    hist_info=$
    {chunks:chunks, $
    pixels_per_chunk:pixels_per_chunk, $
    n_chunks:n_chunks, $
    current_chunk:current_chunk, $
    core_name:core_name, $
    output_image_group:output_image_group, $
    start_year:start_year, $
    end_year:end_year, $
    n_years:n_years, $
    diag_info:diag_info}
    


  end ;initializizing if no prior hist diag file 
  

  

  ;set up generic time since vertex for case where mask val says we assign mean. 
  generic_tsv = indgen(n_years)
  
  
  for current_chunk = 0, n_chunks-1 do begin
    print, 'Processing chunk ' + string(current_chunk) + ' of ' + string(n_chunks) + ' chunks'
    this_subset =  chunks[current_chunk].coords
    within_layer_offset = chunks[current_chunk].within_layer_offset * 2
    ;read in segmse and fitted file
    ;zot_img, se_file, hdr, se_img, subset = this_subset
    ;zot_img, sv_file, hdr, sv_img, subset = this_subset
    
    ; read vertices
    zot_img, vy_file, hdr, vy_img, subset = this_subset
    this_subset =  chunks[current_chunk].coords
    zot_img, vv_file, hdr, vv_img, subset = this_subset
    
    ; read ftv images
    this_subset =  chunks[current_chunk].coords
    zot_img, ftv_tcb_file, hdr, ftv_tcb, subset = this_subset
    this_subset =  chunks[current_chunk].coords
    zot_img, ftv_tcg_file, hdr, ftv_tcg, subset = this_subset
    this_subset =  chunks[current_chunk].coords
    zot_img, ftv_tcw_file, hdr, ftv_tcw, subset = this_subset
    this_subset =  chunks[current_chunk].coords			;added rek june 19, 2013
    zot_img, ftv_nbr_file, hdr, ftv_nbr, subset = this_subset    ; ditto


    ;read the two mask files ;; added march 2013. REK
     if maskyes then begin
	  this_subset =  chunks[current_chunk].coords
    	zot_img, distmask_file, dm_hdr, distmask, subset=this_subset, layers=[1]	;open the year of onset layer;  if zero in a pixel, then nothing happened
    	this_subset =  chunks[current_chunk].coords
    	zot_img, recmask_file, rm_hdr, recmask, subset=this_subset, layers=[1]	;same as prior
    end    
    
    xsize = hdr.filesize[0]
    ysize = hdr.filesize[1]
    
    ;TODO: convert to 4 dimensional array to simplify output creation 
    ;B, G, W at t
    b_t = intarr(xsize, ysize, n_years)
    g_t = intarr(xsize, ysize, n_years)
    w_t = intarr(xsize, ysize, n_years)
    n_t = intarr(xsize, ysize, n_years)    

    ;delta of B, G, W at t
    db_t = intarr(xsize, ysize, n_years)
    dg_t = intarr(xsize, ysize, n_years)
    dw_t = intarr(xsize, ysize, n_years)
    dn_t = intarr(xsize, ysize, n_years)

    		;recent vertex B, G, W
    		;rv_b = intarr(xsize, ysize, n_years)
    		;rv_g = intarr(xsize, ysize, n_years)
    		;rv_w = intarr(xsize, ysize, n_years)
    
    ;time since recent vertex
    ts_v = intarr(xsize, ysize, n_years)
    
    		;previous vertex B, G, W
    		;pv_b = intarr(xsize, ysize, n_years)
   		 ;pv_g = intarr(xsize, ysize, n_years)
    		;pv_w = intarr(xsize, ysize, n_years)
    
    		;duration of previous segment
    		;p_dur = intarr(xsize, ysize, n_years)
    
    for x = 0, xsize-1 do begin
      for y = 0, ysize-1 do begin
        vertexes = vy_img[x, y, *]
        vertvals = vv_img[x, y, *]
        b_stack = ftv_tcb[x, y, *]
        g_stack = ftv_tcg[x, y, *]
        w_stack = ftv_tcw[x, y, *]
	n_stack = ftv_nbr[x, y, *]

	;adding in the mask filter - if zero in both dist and rec, then we use mean val
	
        if maskyes eq 1 then maskval = (distmask[x,y]+recmask[x,y]) ne 0 else maskval=1	;if set to 0, then assign zeros
			;maskyes is a user  
	
        this_metrics = calculate_history_metrics(all_years, vertexes, vertvals, modifier, b_stack, g_stack, w_stack, n_stack, start_year, end_year)
        
	;now assign values.  If the maskval is 0, then we assign values like this is flat.  
	
	if maskval ne 0 then begin 
	  b_t[x, y, *] = this_metrics.b_t
          g_t[x, y, *] = this_metrics.g_t
          w_t[x, y, *] = this_metrics.w_t
          n_t[x, y, *] = this_metrics.n_t        ;added june 19 2013 rek

          db_t[x, y, *] = this_metrics.db_t
          dg_t[x, y, *] = this_metrics.dg_t
          dw_t[x, y, *] = this_metrics.dw_t
          dn_t[x, y, *] = this_metrics.dn_t   ;added june 19 2013 rek

        	;rv_b[x, y, *] = this_metrics.rv_b
        	;rv_g[x, y, *] = this_metrics.rv_g
        	;rv_w[x, y, *] = this_metrics.rv_w
        
          ts_v[x, y, *] = this_metrics.ts_v
        
        	;pv_b[x, y, *] = this_metrics.pv_b
        	;pv_g[x, y, *] = this_metrics.pv_g
        	;pv_w[x, y, *] = this_metrics.pv_w
        
        	;p_dur[x, y, *] = this_metrics.p_dur
         endif else begin 	;begin if this is a no-change pixel -- set mean, etc.
	  b_t[x, y, *] = mean(this_metrics.b_t)
          g_t[x, y, *] = mean(this_metrics.g_t)
          w_t[x, y, *] = mean(this_metrics.w_t)
          n_t[x, y, *] = mean(this_metrics.n_t) ;added june 19 2013 rek

          db_t[x, y, *] = 0
          dg_t[x, y, *] = 0
          dw_t[x, y, *] = 0
          dn_t[x, y, *] = 0    ;added june 19 2013 rek
	  
          ts_v[x, y, *] = generic_tsv	;set to generic incremental val        
	 
	 end 
      
      
      endfor ; y dimension
    endfor ; x dimension
  
    ;set up params for metadata
    date = systime()

 
    ;b_t
    this_file = core_name + output_image_group[0]
    openu, un, this_file, /get_lun
    for layercount = 0ull, n_years-1 do begin
      point_lun, un, layersize * layercount + within_layer_offset
      writeu, un, b_t[*,*,layercount]
    end
    free_lun, un
     outmeta = stringswap(this_file, 'bsq', 'meta.txt')

if maskyes then begin
      metadata={parent_file1: ftv_tcb_file, $
      parent_file2: vv_file, $
      masking_file1: distmask_file, $
      masking_file2:recmask_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
	 concatenate_metadata, [distmask_file, recmask_file, ftv_tcb_file, vv_file, vy_file],outmeta,params=metadata
end else begin 
       metadata={parent_file1: ftv_tcb_file, $
      parent_file2: vv_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [ftv_tcb_file, vv_file, vy_file],outmeta,params=metadata
end


    ;g_t
    this_file = core_name + output_image_group[1]
    openu, un, this_file, /get_lun
    for layercount = 0ull, n_years-1 do begin
      point_lun, un, layersize * layercount + within_layer_offset
      writeu, un, g_t[*,*,layercount]
    end
    free_lun, un
  outmeta = stringswap(this_file, 'bsq', 'meta.txt')

if maskyes then begin
      metadata={parent_file1: ftv_tcg_file, $
      parent_file2: vv_file, $
      masking_file1: distmask_file, $
      masking_file2:recmask_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [distmask_file, recmask_file, ftv_tcg_file, vv_file, vy_file],outmeta,params=metadata
end else begin
       metadata={parent_file1: ftv_tcg_file, $
      parent_file2: vv_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [ftv_tcg_file, vv_file, vy_file],outmeta,params=metadata
end


  
    ;w_t
    this_file = core_name + output_image_group[2]
    openu, un, this_file, /get_lun
    for layercount = 0ull, n_years-1 do begin
      point_lun, un, layersize * layercount + within_layer_offset
      writeu, un, w_t[*,*,layercount]
    end
    free_lun, un
      outmeta = stringswap(this_file, 'bsq', 'meta.txt')
 
 if maskyes then begin
      metadata={parent_file1: ftv_tcw_file, $
      parent_file2: vv_file, $
      masking_file1: distmask_file, $
      masking_file2:recmask_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [distmask_file, recmask_file, ftv_tcw_file, vv_file, vy_file],outmeta,params=metadata
end else begin
       metadata={parent_file1: ftv_tcw_file, $
      parent_file2: vv_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [ftv_tcw_file, vv_file, vy_file],outmeta,params=metadata
end

 
 
   ;n_t
    this_file = core_name + output_image_group[7]
    openu, un, this_file, /get_lun
    for layercount = 0ull, n_years-1 do begin
      point_lun, un, layersize * layercount + within_layer_offset
      writeu, un, n_t[*,*,layercount]
    end
    free_lun, un
      outmeta = stringswap(this_file, 'bsq', 'meta.txt')
 if maskyes then begin
      metadata={parent_file1: ftv_nbr_file, $
      parent_file2: vv_file, $
      masking_file1: distmask_file, $
      masking_file2:recmask_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [distmask_file, recmask_file, ftv_nbr_file, vv_file, vy_file],outmeta,params=metadata
end else begin
       metadata={parent_file1: ftv_nbr_file, $
      parent_file2: vv_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [ftv_nbr_file, vv_file, vy_file],outmeta,params=metadata
end




    ;db_t
    this_file = core_name + output_image_group[3]
    openu, un, this_file, /get_lun
    for layercount = 0ull, n_years-1 do begin
      point_lun, un, layersize * layercount + within_layer_offset
      writeu, un, db_t[*,*,layercount]
    end
    free_lun, un
  outmeta = stringswap(this_file, 'bsq', 'meta.txt')
 
 if maskyes then begin
      metadata={parent_file1: ftv_tcb_file, $
      parent_file2: vv_file, $
      masking_file1: distmask_file, $
      masking_file2:recmask_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
	 concatenate_metadata, [distmask_file, recmask_file, ftv_tcb_file, vv_file, vy_file],outmeta,params=metadata
end else begin 
       metadata={parent_file1: ftv_tcb_file, $
      parent_file2: vv_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [ftv_tcb_file, vv_file, vy_file],outmeta,params=metadata
end

   
    ;dg_t
    this_file = core_name + output_image_group[4]
    openu, un, this_file, /get_lun
    for layercount = 0ull, n_years-1 do begin
      point_lun, un, layersize * layercount + within_layer_offset
      writeu, un, dg_t[*,*,layercount]
    end
    free_lun, un
  outmeta = stringswap(this_file, 'bsq', 'meta.txt')
 if maskyes then begin
      metadata={parent_file1: ftv_tcg_file, $
      parent_file2: vv_file, $
      masking_file1: distmask_file, $
      masking_file2:recmask_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [distmask_file, recmask_file, ftv_tcg_file, vv_file, vy_file],outmeta,params=metadata
end else begin
       metadata={parent_file1: ftv_tcg_file, $
      parent_file2: vv_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [ftv_tcg_file, vv_file, vy_file],outmeta,params=metadata
end

  
    ;dw_t
    this_file = core_name + output_image_group[5]
    openu, un, this_file, /get_lun
    for layercount = 0ull, n_years-1 do begin
      point_lun, un, layersize * layercount + within_layer_offset
      writeu, un, dw_t[*,*,layercount]
    end
    free_lun, un
  outmeta = stringswap(this_file, 'bsq', 'meta.txt')
  
 if maskyes then begin
      metadata={parent_file1: ftv_tcw_file, $
      parent_file2: vv_file, $
      masking_file1: distmask_file, $
      masking_file2:recmask_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [distmask_file, recmask_file, ftv_tcw_file, vv_file, vy_file],outmeta,params=metadata
end else begin
       metadata={parent_file1: ftv_tcw_file, $
      parent_file2: vv_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [ftv_tcw_file, vv_file, vy_file],outmeta,params=metadata
end




  ;dn_t
    this_file = core_name + output_image_group[8]
    openu, un, this_file, /get_lun
    for layercount = 0ull, n_years-1 do begin
      point_lun, un, layersize * layercount + within_layer_offset
      writeu, un, dn_t[*,*,layercount]
    end
    free_lun, un
      outmeta = stringswap(this_file, 'bsq', 'meta.txt')
 if maskyes then begin
      metadata={parent_file1: ftv_nbr_file, $
      parent_file2: vv_file, $
      masking_file1: distmask_file, $
      masking_file2:recmask_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [distmask_file, recmask_file, ftv_nbr_file, vv_file, vy_file],outmeta,params=metadata
end else begin
       metadata={parent_file1: ftv_nbr_file, $
      parent_file2: vv_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [ftv_nbr_file, vv_file, vy_file],outmeta,params=metadata
end



;ts_v
    this_file = core_name + output_image_group[6]
    openu, un, this_file, /get_lun
    for layercount = 0ull, n_years-1 do begin
      point_lun, un, layersize * layercount + within_layer_offset
      writeu, un, ts_v[*,*,layercount]
    end
    free_lun, un
  outmeta = stringswap(this_file, 'bsq', 'meta.txt')
 
 if maskyes then begin
      metadata={parent_file1: vv_file, $
      masking_file1: distmask_file, $
      masking_file2:recmask_file, $
      creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [distmask_file, recmask_file, vv_file, vy_file],outmeta,params=metadata
end else begin
       metadata={parent_file1: vv_file, $
       creationdate: date}
    if file_test(outmeta) eq 0 then $
         concatenate_metadata, [vv_file, vy_file],outmeta,params=metadata
end


    ;rv_b
    ;this_file = core_name + output_image_group[6]
    ;openu, un, this_file, /get_lun
    ;for layercount = 0ull, n_years-1 do begin
    ;  point_lun, un, layersize * layercount + within_layer_offset
    ;  writeu, un, rv_b[*,*,layercount]
    ;end
    ;free_lun, un

    ;rv_g
    ;this_file = core_name + output_image_group[7]
    ;openu, un, this_file, /get_lun
    ;for layercount = 0ull, n_years-1 do begin
    ;  point_lun, un, layersize * layercount + within_layer_offset
    ;  writeu, un, rv_g[*,*,layercount]
    ;end
    ;free_lun, un

    ;rv_w
    ;this_file = core_name + output_image_group[8]
    ;openu, un, this_file, /get_lun
    ;for layercount = 0ull, n_years-1 do begin
    ;  point_lun, un, layersize * layercount + within_layer_offset
    ;  writeu, un, rv_w[*,*,layercount]
    ;end
    ;free_lun, un

    ;ts_v vv_file, vyfile],outmeta
  
    ;pv_b
    ;this_file = core_name + output_image_group[10]
    ;openu, un, this_file, /get_lun
    ;for layercount = 0ull, n_years-1 do begin
    ;  point_lun, un, layersize * layercount + within_layer_offset
    ;  writeu, un, pv_b[*,*,layercount]
    ;end
    ;free_lun, un

    ;pv_g
    ;this_file = core_name + output_image_group[11]
    ;openu, un, this_file, /get_lun
    ;for layercount = 0ull, n_years-1 do begin
    ;  point_lun, un, layersize * layercount + within_layer_offset
    ;  writeu, un, pv_g[*,*,layercount]
    ;end
    ;free_lun, un

    ;pv_w
    ;this_file = core_name + output_image_group[12]
    ;openu, un, this_file, /get_lun
    ;for layercount = 0ull, n_years-1 do begin
    ;  point_lun, un, layersize * layercount + within_layer_offset
    ;  writeu, un, pv_w[*,*,layercount]
    ;end
    ;free_lun, un

    ;p_dur
    ;this_file = core_name + output_image_group[13]
    ;openu, un, this_file, /get_lun
    ;for layercount = 0ull, n_years-1 do begin
    ;  point_lun, un, layersize * layercount + within_layer_offset
    ;  writeu, un, p_dur[*,*,layercount]
    ;end
    ;free_lun, un

  ;update diag file
   hist_info.current_chunk=current_chunk+1 ;set up for next chunk
  save, hist_info, file = histdiagfile
 



  endfor ; process chunks
  ;add metadata
  
     
  
  
  
  
  
  return, {ok:1, histdiagfile:histdiagfile, hist_info:hist_info}
  
END
