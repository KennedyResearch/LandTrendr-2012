;**************************************************************************** 
;Copyright © 2008-2011 Oregon State University                                
;All Rights Reserved.                                                         
;                                                                             
;                                                                             
;Permission to use, copy, modify, and distribute this software and its        
;documentation for educational, research and non-profit purposes, without     
;fee, and without a written agreement is hereby granted, provided that the    
;above copyright notice, this paragraph and the following three paragraphs    
;appear in all copies.                                                        
;                                                                             
;                                                                             
;Permission to incorporate this software into commercial products may be      
;obtained by contacting Oregon State University Office of Technology Transfer.
;                                                                             
;                                                                             
;This software program and documentation are copyrighted by Oregon State      
;University. The software program and documentation are supplied "as is",     
;without any accompanying services from Oregon State University. OSU does not 
;warrant that the operation of the program will be uninterrupted or           
;error-free. The end-user understands that the program was developed for      
;research purposes and is advised not to rely exclusively on the program for  
;any reason.                                                                  
;                                                                             
;                                                                             
;IN NO EVENT SHALL OREGON STATE UNIVERSITY BE LIABLE TO ANY PARTY FOR DIRECT, 
;INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST      
;PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
;IF OREGON STATE UNIVERSITYHAS BEEN ADVISED OF THE POSSIBILITY OF SUCH        
;DAMAGE. OREGON STATE UNIVERSITY SPECIFICALLY DISCLAIMS ANY WARRANTIES,       
;INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
;FITNESS FOR A PARTICULAR PURPOSE AND ANY STATUTORY WARRANTY OF               
;NON-INFRINGEMENT. THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS,    
;AND OREGON STATE UNIVERSITY HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE,       
;SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.                            
;                                                                             
;**************************************************************************** 

FUNCTION extract_recovery_slice, dist_file, intervals, vertyrs_file, vertvals_file, diag_file, output_basefile, subset=subset, fix_vertex=fix_vertex, use_delta=use_delta

    IF file_exists(vertvals_file) EQ 0 THEN return, {ok:0, message: 'Vertex value image not found'}
    IF file_exists(vertyrs_file) EQ 0 THEN return, {ok:0, message: 'Vertex year image not found'}
    IF file_exists(dist_file) EQ 0 THEN return, {ok:0, message: 'Disturbance image not found'}
    IF file_exists(diag_file) EQ 0 THEN return, {ok:0, message: 'landtrendr diagnostic file not found'}
    
    ;retrieve diagnosis information
    restore, diag_file
    image_info = diag_info.image_info
    every_year = image_info.year
    unique_years = fast_unique(every_year)
    all_years = unique_years[sort(unique_years)]
    n_years = n_elements(all_years)
    
    ;define subset
    IF n_elements(subset) EQ 0 THEN BEGIN
        zot_img, dist_file, hdr, dist_dat, /hdronly
        subset = [[hdr.upperleftcenter], [hdr.lowerrightcenter]]
    ENDIF ELSE BEGIN
        zot_img, dist_file, hdr, dist_dat, subset=subset, /hdronly
    ENDELSE
    
    fs = hdr.filesize
    layersize = ulong64(fs[0]) * fs[1]
    byte_per_pixel = 2
    n_output_layers = n_elements(intervals) ; always write out three layers image
    
    ;create output recovery file
    recovery_slice_file = output_basefile + "_slice_rate.bsq"
    
    ;3-layer magnitude image signed 16bit
    IF file_exists(recovery_slice_file) EQ 0 THEN BEGIN
        openw, un,  recovery_slice_file, /get_lun
        filesize = ulong(layersize) * n_output_layers*byte_per_pixel
        point_lun, un, filesize - byte_per_pixel
        writeu, un, 0
        free_lun, un
        hdr1 = hdr
        hdr1.n_layers = n_output_layers
        hdr1.pixeltype = 6
        write_im_hdr, recovery_slice_file, hdr1
    END
    
    ; now define the chunks
    max_pixels_per_chunk = 400000l
    pixsize = hdr.pixelsize
    kernelsize = 1
    
    ok = define_chunks3(subset, pixsize, max_pixels_per_chunk, kernelsize)
    IF ok.ok EQ 0 THEN return, {ok:0, message: 'Error creating chunks.'}
    chunks = ok.subsets
    pixels_per_chunk = ok.pixels_per_chunk
    n_chunks = n_elements(chunks)
    current_chunk = 0 ;an index
    
    ;now process each chunk
    FOR current_chunk = 0, n_chunks-1 DO BEGIN
        print, 'Processing chunk ' + string(current_chunk+1) + ' of ' + string(n_chunks) + ' chunks'
        
        this_subset = chunks[current_chunk].coords
        within_layer_offset = chunks[current_chunk].within_layer_offset
        
        ;read disturbance image data, assuming first layer is disturbance year.
        zot_img, dist_file, dist_hdr, distyear_dat, subset=this_subset, layers=[1]
        
        ;read vert_yrs, read all vertex years
        zot_img, vertyrs_file, vertyr_hdr, vertyr_dat, subset=this_subset
        
        ;read vert_vals, read all vertex values
        zot_img, vertvals_file, vertval_hdr, vertval_dat, subset=this_subset
        
        ;get image dimension
        xsize = dist_hdr.filesize[0]
        ysize = dist_hdr.filesize[1]
        
        ;create the rate image layers
        rate_image = intarr(xsize, ysize, n_output_layers)
        
        FOR x = 0, xsize-1 DO BEGIN
            FOR y = 0, ysize-1 DO BEGIN
                IF distyear_dat[x, y] GT 0 THEN BEGIN
                    ok = calculate_recovery_slice(distyear_dat[x, y], intervals, vertyr_dat[x,y,*], vertval_dat[x,y,*], all_years, fix_vertex=keyword_set(fix_vertex), use_delta=keyword_set(use_delta))
                    IF ok.ok EQ 1 THEN rate_image[x,y,*] = ok.rate ELSE return, {ok:0, message:ok.message}
                END
            ENDFOR ;y
        ENDFOR ;x
        
        ;save recovery magnitude
        openu, un, recovery_slice_file, /get_lun
        FOR layercount = 0, n_output_layers-1 DO BEGIN
            point_lun, un, layersize * byte_per_pixel * layercount + within_layer_offset*byte_per_pixel
            writeu, un, rate_image[*,*,layercount]
        END
        free_lun, un
    ENDFOR
    
    return, {ok:1, message:'post-disturbance recovery information extracted'}
    
END