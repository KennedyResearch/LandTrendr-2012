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

function process_ftv_chunks, ftv_run_params, progressbaryesno, interpolate=interpolate
  ;copy over just to make it work with historical code.  lazy
  vertex_image_file = ftv_run_params.vertex_image_file
  
  apply_to_image_info = ftv_run_params.apply_to_image_info
  apply_to_index = ftv_run_params.apply_to_index				;string with name of apply_to_index
 subset=ftv_run_params.subset
  mask_image = ftv_run_params.mask_image
  output_base = ftv_run_params.output_base
  kernelsize = ftv_run_params.kernelsize
  
  background_val = ftv_run_params.background_val
  skipfactor = ftv_run_params.skipfactor
  desawtooth_val = ftv_run_params.desawtooth_val
  ;pval = ftv_run_params.pval
  ;max_segments = ftv_run_params.max_segments
  
  fix_doy_effect = ftv_run_params.fix_doy_effect
  divisor =ftv_run_params.divisor
  ;minneeded = ftv_run_params.minneeded
  ;recovery_threshold=ftv_run_params.recovery_threshold
  
   ;NOV. 2012. now parameter to apply filter after fitting
  
  ;check to see if post-process_params
  tn = tag_names(ftv_run_params)
  zn = strmatch(tn, strupcase("post_process_params"))
  yespp = total(zn eq 1)  ;if post-process_parms are in there
  if yespp eq 1 then post_process_params = ftv_run_params.post_process_params
  
  
   progressval = 0 ;set to record progress
  progressinc = 10  ;for the increment
  
  
  
  ;first, check to see if the output image already exists.  If so
  ;   see if it has a save file to indicate that it was already in
  ;   process, and needs to just be picked up again
  output_base = output_base + "_" + ftv_run_params.apply_to_index
  
  diagfile =  output_base+'_ftv_diag.sav'
  
  if file_exists(diagfile) then begin
    print, 'This file has already had processing done on it'
    restore, diagfile
    vertex_image_file = diag_info.vertex_image_file
    
    apply_to_image_info = diag_info.apply_to_image_info
    apply_to_index = diag_info.apply_to_index
    mask_image = diag_info.mask_image
    output_image_group = diag_info.output_image_group
    
    n_chunks = diag_info.n_chunks
    chunks = diag_info.chunks
    current_chunk = diag_info.current_chunk
    pixels_per_chunk = diag_info.pixels_per_chunk
    kernelsize = diag_info.kernelsize
    
    background_val=diag_info.background_val
    skipfactor=diag_info.skipfactor
    desawtooth_val=diag_info.desawtooth_val
    ;pval=diag_info.pval
    ;max_segments=diag_info.max_segments
    ;normalize=diag_info.normalize
    fix_doy_effect=diag_info.fix_doy_effect
    divisor=diag_info.divisor
    ;recovery_threshold = diag_info.recovery_threshold
  end else begin           ;if this image has not been set up before, then
    ;get set up to do it.
    ;SET UP THE OUTPUT FILE AND CHUNK INFORMATION
    ;First, set up the processing chunks
  
    if n_elements(max_pixels_per_chunk) eq 0 then $
      max_pixels_per_chunk = 500000l
      
    ;to get the pixel size of the image, assume that all are the
    ;   same
    if file_exists(apply_to_image_info[0].image_file) eq 0 then begin
      print, "process_ftv_chunks.pro:  Image in
      print,"    image_list does not exist.  Failing."
      print,"    Image that was not found:
      print, apply_to_image_info[0].image_file
    end
    
    ;zot_img, apply_to_image_info[0].image_file, hdr, img, subset=subset, /hdronly
    ;pixsize = hdr.pixelsize

   zot_img, vertex_image_file, hdr, img, subset=subset, /hdronly
    pixsize = hdr.pixelsize
    max_segments = hdr.n_layers-1
    
    ;now get the chunks
    ok = define_chunks3(subset, pixsize, max_pixels_per_chunk, kernelsize)
    if ok.ok eq 0 then return, {ok:0}
    
    chunks = ok.subsets
    pixels_per_chunk = ok.pixels_per_chunk
    n_chunks = n_elements(chunks)
    current_chunk = 0          ;an index
    
    
    ;define the output images
    ;vertices:   the years of the vertices
    ;vertvals:  the values of the input band at those years
    ;mags:   the magnitude of the segment change between two vertices
    ;distrec:  three layer image
    ;			1:  largest single disturbance
    ;			2:  largest single recovery
    ;			3:  scaled ratio between disturbance and recovery
    ;					-1000 is all recovery
    ;					0 is a balance
    ;					1000 is all disturbance
    
    ;fitted image
    ;			Same number of years as all of the inputs,
    ;			but with fitted values
    ;Stats image for entire fit
    ;			P of f
    ;			f_stat
    ;			ms_regr
    ;			ms_resid
    
    output_image_base = {filename:'', n_layers:0, extension:'', layersize:0l, filesize:0ll, DATA:""}
    
    ;output_image_group = replicate(output_image_base, 7)
    output_image_group = replicate(output_image_base, 6)
    
    output_image_group[0].extension = 	'_ftv_vertvals.bsq'
    output_image_group[0].n_layers  =  max_segments+1
    output_image_group[0].DATA = "FTV Vertex Value"
    
    ;6/27/08 for fitted image, the output will be the
    ;   max of 1 image per year.  If multiple images
    ;   in a given year are provided with the idea of doing
    ;   cloud-mosaicking, then we need to compensate for
    ;   those doubled-images in stack.
    years = apply_to_image_info.year
    un_years = fast_unique(years)
    years = un_years[sort(un_years)]
    
    output_image_group[1].extension = 	'_ftv_fitted.bsq'
    output_image_group[1].n_layers  =  n_elements(years)
    if keyword_set(interpolate) then output_image_group[1].n_layers  = range(years) + 1
    output_image_group[1].DATA = "FTV Fitted Stack"
    
    output_image_group[2].extension = 	'_ftv_stats.bsq'
    output_image_group[2].n_layers  =  10
    output_image_group[2].DATA = "FTV Statistics"
    
    output_image_group[3].extension = 	'_ftv_segmse.bsq'
    output_image_group[3].n_layers  =  max_segments
    output_image_group[3].DATA = "FTV Segment MSE"

    output_image_group[4].extension = 	'_ftv_source.bsq'
    output_image_group[4].n_layers  =  n_elements(years)
    output_image_group[4].DATA = "FTV Source Stack"
    
    output_image_group[5].extension = 	'_ftv_segmean.bsq'
    output_image_group[5].n_layers  =  max_segments    
    output_image_group[5].DATA = "FTV Segment Mean"
    
    for i = 0, n_elements(output_image_group)-1 do begin
      this_file = output_base + output_image_group[i].extension
      output_image_group[i].filename = this_file
      
      openw, un, 	output_image_group[i].filename, /get_lun
      n_output_layers = output_image_group[i].n_layers
      
      bytes_per_pixel = 2
      layersize = long(hdr.filesize[0]) * hdr.filesize[1] * bytes_per_pixel
        
      filesize = ulong64(layersize) * n_output_layers
      point_lun, un, filesize - 2         ;-2 because we're going to write
      ;a blank pixel
      writeu, un, 0
      free_lun, un         ;now the file exists on the drive.
      hdr1 = hdr
      hdr1.n_layers = n_output_layers
      hdr1.pixeltype = 6
      write_im_hdr, 	output_image_group[i].filename, hdr1
      output_image_group[i].layersize = layersize
      output_image_group[i].filesize = filesize
      
      ; now create the metadata file
      this_meta = stringswap(this_file, ".bsq", "_meta.txt")
      
      meta = create_struct("DATA", output_image_group[i].DATA, "FILENAME", file_basename(this_file), "PARENT_FILE", file_basename(vertex_image_file), "Interpolated", keyword_set(interpolate), ftv_run_params)
      
      concatenate_metadata, [vertex_image_file], this_meta, params=meta
    end		;going through images
    
    ;2/7/08  First determine the scaling factor, so
    ;  the image is always in the 0-1000 range.
    ;pick the middle image and look at the max
    n_files = n_elements(apply_to_image_info)
    pickit = n_files/2
    landtrendr_image_read, apply_to_image_info[pickit], hdr, img1, subset, apply_to_index, modifier, background_val
    
    ;if user asks for divisor to be calc'd, do it here
    if divisor eq -1 then begin
      divscale = [1., 10, 100, 1000, 10000, 100000]		; raise 10 to the power of the appropriate apply_to_index to get divisor
      m1 = median(img1)
      div = float(m1)/ 1000		;divide by the number you want to be max
      divisor = 10 ^ (min(where(divscale gt div)))
    end
    
    img1 = 0 ;for memory
    
    ;now write out the diagnostic file so we can keep
    ;   track of what we've complete, in case things crash.
    diag_info = $
      {vertex_image_file:vertex_image_file, $
      apply_to_image_info:apply_to_image_info, $
      apply_to_index:apply_to_index, $
      mask_image:mask_image, $
      output_image_group:output_image_group, $
      pixels_per_chunk:pixels_per_chunk, $
      
      n_chunks:n_chunks, $
      chunks:chunks, $
      current_chunk:current_chunk, $
      version_number:landtrendr_version(), $
      kernelsize:kernelsize, $
      
      background_val:background_val, $
      skipfactor:skipfactor, $
      desawtooth_val:desawtooth_val, $
      ;pval:pval, $
      max_segments:max_segments, $
      ;normalize:n_elements(normalize), $
      fix_doy_effect:fix_doy_effect, $
      divisor:divisor};, $
      ;recovery_threshold:recovery_threshold  }
 
    save, diag_info, file = diagfile
  end
  
  ;at this point, we've either created or restored the diag_info
  ;  this tells us what the chunks are, and what we've written
  ;  to the output image. "Current_chunk" is the important
  ;  index that lets us know where we are in the processing.
  
  ;set up the progress bar:
    if progressbaryesno eq 1 then begin 
    mainprogressBar = Obj_New("PROGRESSBAR", /fast_loop, title = 'Processing chunks')
  mainprogressBar -> Start
  end
  
  
  
  ;get set up with the correct information, based on the chunk
  thebeginning:
  ;diagnosis stuff; comment out for final run
  
  if current_chunk ge n_chunks then begin
    print, 'This image has been entirely processed.'
    print, 'If you want to reprocess it, please delete this file:'
    print, diagfile
    if progressbaryesno eq 1 then mainprogressBar -> Destroy
    return, {ok:2}
  end
  
  ;first, where in the input files do we look?
  subset = chunks[current_chunk].coords
  
  ;second, where do we write out?
  ;calculated in define_chunks2, but in pixel units..
  ;  for file units, need to multiply by 2 because 2 bytes per pixel
  ;  in integer world.
  within_layer_offset = chunks[current_chunk].within_layer_offset * 2
 ; stop 
    ok = run_ftv_single_chunk(vertex_image_file, apply_to_image_info, $
    subset, apply_to_index, mask_image, output_image_group, $
    within_layer_offset, layersize, kernelsize, $
    background_val, $
    skipfactor, desawtooth_val, $
        fix_doy_effect, divisor, progressbaryesno, $
    post_process_params=post_process_params, $  ;added jan 2013
    interpolate=interpolate)  
    
  ;check on main progress bar
  if progressbaryesno eq 1 then begin 
     if mainprogressBar -> CheckCancel() then begin
    mainprogressBar -> destroy
    print, 'chunk', string(current_chunk)
    end
  end
  
  ;increment chunk, keep track of it in case program
  ;   crashes in next piece
  if ok.ok eq 1 then begin 
    current_chunk = current_chunk + 1
    diag_info.current_chunk = current_chunk
    save, diag_info, file = diagfile
  end
 
  
  ;update the progress meeter
  percent_done = (float(current_chunk)/n_chunks)*100
 
  test = round((percent_done) / progressinc)
  if test gt progressval then begin
      print, progressval*progressinc  ;only print if we've bumped to next increment
      progressval = test
      end
      
  
  if progressbaryesno eq 1 then mainprogressBar -> Update, percent_done*100, text = strcompress('Done with chunk '+string(current_chunk)+' of '+string(n_chunks))
  
  if current_chunk lt n_chunks then goto, thebeginning
  
  ;interpolation.
  print, "Done."
  if progressbaryesno eq 1 then mainprogressBar -> destroy
  
  return, {ok:1}
end
