;+
; Name:
;   extract_precursor
;
; Purpose:
;   Given an image with year of interest, extract the information for previous and current segment
;   previous_segment: B, G, W, duration, ΔB, ΔG, ΔW
;   current_segment: B, G, W, offset, ΔB, ΔG, ΔW
;-
pro extract_precursor, diag_file, year_of_interest_file, output_basefile, subset=subset
    on_error, 2

    if file_exists(diag_file) eq 0 then message, 'diag_info_file does not exists'
    
    len = strlen(diag_file)
    endpos = strpos(diag_file, '_diag.sav')
    
    simple_core_name =  strmid(diag_file, 0, endpos)
    vertyr_file = simple_core_name + "_vertyrs.bsq"
    
    ;vertval_file = simple_core_name + "_vertvals.bsq"
    ;mag_file = simple_core_name + "_mags.bsq"
    ;dur_file = simple_core_name + "_durs.bsq"
    ;distrec_file = simple_core_name + "_distrec.bsq"
    
    b_ftv_file = simple_core_name + "brightness_ftv_fitted.bsq"
    g_ftv_file = simple_core_name + "greenness_ftv_fitted.bsq"
    w_ftv_file = simple_core_name + "wetness_ftv_fitted.bsq"
    
    if file_exists(vertyr_file) eq 0 then message, 'Vertyr image not found'
    ;if file_exists(vertval_file) eq 0 then return, {ok:0, message: 'Vertval image not found'}
    ;if file_exists(mag_file) eq 0 then return, {ok:0, message: 'Magnitudes image not found'}
    ;if file_exists(dur_file) eq 0 then return, {ok:0, message: 'Duration image not found'}
    ;if file_exists(distrec_file) eq 0 then return, {ok:0, message: 'Distrec image not found'}
    if file_exists(b_ftv_file) eq 0 then message, 'fitted brightness does not exists'
    if file_exists(g_ftv_file) eq 0 then message, 'fitted greenness does not exists'
    if file_exists(w_ftv_file) eq 0 then message, 'fitted wetness does not exists'
    
    ;retrieve diagnosis information
    restore, diag_file
    image_info = diag_info.image_info
    every_year = image_info.year
    unique_years = fast_unique(every_year)
    all_years = unique_years[sort(unique_years)]
    n_years = n_elements(all_years)
    
    ;define subset
    IF n_elements(subset) EQ 0 THEN BEGIN
        zot_img, year_of_interest_file, hdr, dist_dat, /hdronly
        subset = [[hdr.upperleftcenter], [hdr.lowerrightcenter]]
    ENDIF ELSE BEGIN
        zot_img, year_of_interest_file, hdr, year_of_interest_file, subset=subset, /hdronly
    ENDELSE
    
    fs = hdr.filesize
    layersize = ulong64(fs[0]) * fs[1]
    byte_per_pixel = 2
    n_output_layers = 7 ; always write out three layers image
    
    ;create output recovery file
    previous_segment_file = output_basefile + "_previous_segment.bsq"
    current_segment_file = output_basefile + "_current_segment.bsq"
    
    ;always recreate the file
    openw, un, previous_segment_file, /get_lun
    filesize = layersize * n_output_layers * byte_per_pixel
    point_lun, un, filesize - byte_per_pixel
    writeu, un, 0
    free_lun, un
    hdr1 = hdr
    hdr1.n_layers = n_output_layers
    hdr1.pixeltype = 6 ;signed 16bit
    write_im_hdr, previous_segment_file, hdr1
    
    openw, un, current_segment_file, /get_lun
    filesize = layersize * n_output_layers * byte_per_pixel
    point_lun, un, filesize - byte_per_pixel
    writeu, un, 0
    free_lun, un
    hdr1 = hdr
    hdr1.n_layers = n_output_layers
    hdr1.pixeltype = 6 ;signed 16bit
    write_im_hdr, current_segment_file, hdr1
    
    ; now define the chunks
    max_pixels_per_chunk = 200000l
    pixsize = hdr.pixelsize
    kernelsize = 1
    
    ok = define_chunks3(subset, pixsize, max_pixels_per_chunk, kernelsize)
    IF ok.ok EQ 0 THEN message, 'Error creating chunks.'
    chunks = ok.subsets
    pixels_per_chunk = ok.pixels_per_chunk
    n_chunks = n_elements(chunks)
    current_chunk = 0 ;an index
    
    ;now process each chunk
    FOR current_chunk = 0, n_chunks-1 DO BEGIN
        print, 'Processing chunk ' + string(current_chunk+1) + ' of ' + string(n_chunks) + ' chunks'
        
        this_subset = chunks[current_chunk].coords
        within_layer_offset = chunks[current_chunk].within_layer_offset
        
        ;read year of interest
        zot_img, year_of_interest_file, yhdr, yoi_dat, subset = this_subset, layers=[1]
        
        ;read vert_yrs
        zot_img, vertyr_file, vhdr, vertyr_dat, subset=this_subset
        
        ;read b,g,w stack
        zot_img, b_ftv_file, bhdr, b_dat, subset=this_subset
        zot_img, g_ftv_file, ghdr, g_dat, subset=this_subset
        zot_img, w_ftv_file, whdr, w_dat, subset=this_subset
        
        xsize = yhdr.filesize[0]
        ysize = yhdr.filesize[1]
        ;create output image
        pre_img = intarr(xsize, ysize, 7)
        cur_img = intarr(xsize, ysize, 7)
        
        for x = 0, xsize-1 do begin
            for y = 0, ysize-1 do begin
                if yoi_dat[x,y] gt 0 then begin
                    ok = calculate_precursor(yoi_dat[x,y], vertyr_dat[x,y,*], all_years, b_dat[x,y,*], g_dat[x,y,*], w_dat[x,y,*])
                    if ok.ok eq 1 then begin
                        pre_img[x,y,*] = ok.preimg
                        cur_img[x,y,*] = ok.curimg
                    endif 
                endif ;yod_dat
            endfor ;y
        endfor ;x
        
        openu, un, previous_segment_file, /get_lun
        for layercount = 0, n_output_layers-1 do begin
            point_lun, un, layersize * byte_per_pixel * layercount + within_layer_offset*byte_per_pixel
            writeu, un, pre_img[*,*,layercount]
        endfor
        free_lun, un
        
        openu, un, current_segment_file, /get_lun
        for layercount = 0, n_output_layers-1 do begin
            point_lun, un, layersize * byte_per_pixel * layercount + within_layer_offset*byte_per_pixel
            writeu, un, cur_img[*,*,layercount]
        endfor
        free_lun, un
    ;save file
    ENDFOR
    print, 'precursor semgent information extracted'
end