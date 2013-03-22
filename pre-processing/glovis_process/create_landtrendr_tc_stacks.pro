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

pro create_landtrendr_tc_stacks, image_info, fix_these=fix_these


  ;given a stack of madcalled image reflectance stacks (6-bands),
  ;  use the reflectance tasseled-cap values to make stacks and
  ;  put them in the image info

  n_files = n_elements(image_info)
  blank = 0	;integer type
  
  ;basic file stuff for writing headers
  
  n_output_layers = 3
  bytes_per_pixel = 2
  max_pixels_per_chunk = 5000000l
  kernelsize = 1	;needed for define_chunks routine
  
  ;use the reflectance coefficients
  brt_coeffs = [0.2043, 0.4158, 0.5524, 0.5741, 0.3124, 0.2303]
  grn_coeffs = [-0.1603, -0.2819, -0.4934, 0.7940, -0.0002, -0.1446]
  wet_coeffs = [0.0315, 0.2021, 0.3102, 0.1594,-0.6806, -0.6109]
  
  all_coeffs = [[brt_coeffs], [grn_coeffs], [wet_coeffs]]
  
  mainprogressBar = Obj_New("PROGRESSBAR", /fast_loop, title = 'Processing stack to TC')
  mainprogressBar -> Start
  
  for i = 0, n_files - 1 do begin
    ;check for dependencies
    if file_exists(image_info[i].image_file) eq 0 then message, 'File '+image_info[i].image_file+' does not exist.'
    filedir = file_dirname(image_info[i].image_file)+"\"
    
    ;check to see if this is a normalized image, if not warn then skip
    normalized = strmatch(image_info[i].image_file, "*_to_*")
    tced = strmatch(image_info[i].tc_file, "*ltc*")
    if normalized eq 0 and image_info[i].type ne 3 then begin
      print, ""
      print, ">>> this file needs to be normalized..."
      print, "      ",image_info[i].image_file
      print, ">>> skipping"
      print, ""
      goto, skipthisone
    endif
    if tced eq 1 and keyword_set(fix_these) eq 0 then begin
      print, ">>>
      print, ">>> this image has already been..."
      print, ">>> transformed to tasseled cap..."
      print, image_info[i].image_file
      print, ">>> if you would like to redo it..."
      print, ">>> you must enter the year and julian day..."
      print, ">>> into the 'fix_these' variable in the..."
      print, ">>> dynamic variable input section of the..."
      print, ">>> scene's batchfile and rerun...skipping"
      print, ">>>"
      goto, skipthisone
    endif
    if tced eq 1 and keyword_set(fix_these) eq 1 then begin
      ;check to see that the date is in the given fix list
      date = ulong(strcompress(string(image_info[i].year)+string(image_info[i].julday), /rem))
      match = where(fix_these eq date, n_match)
      if n_match eq 1 then lt_delete_duplicate, image_info[i].tc_file, /tcimg else begin
        print, ">>>
        print, ">>> this image has already been..."
        print, ">>> transformed to tasseled cap..."
        print, ">>> ",image_info[i].image_file
        print, ">>> if you would like to redo it..."
        print, ">>> you must enter the year and julian day..."
        print, ">>> into the 'fix_these' variable in the..."
        print, ">>> dynamic variable input section of the..."
        print, ">>> scene's batchfile and rerun...skipping"
        print, ">>>"
        goto, skipthisone
      endelse
    endif
    
    ;set the file name up
    outfile = strcompress(filedir+strmid(file_basename(image_info[i].image_file), 0, 18)+ $
      "_"+timestamp()+"_ltc.bsq", /rem)
      
    ;    ;delete the normalized image if it already exists
    ;    lt_delete_duplicate, image_info[i].image_file, /tcimg
      
    print, "creating TC for: ", file_basename(image_info[i].image_file)
    ;set up chunks for this image
    zot_img, image_info[i].image_file, hdr, img,  /hdronly
    if hdr.n_layers ne 6 then message, 'File must have 6 reflectance bands: '+image_info.image_file
    pixsize = hdr.pixelsize
    image_bounds = [ [hdr.upperleftcenter], [hdr.lowerrightcenter]]
    
    
    ;now get the chunks
    
    ok = define_chunks3(image_bounds, pixsize, max_pixels_per_chunk, kernelsize)
    if ok.ok eq 0 then return
    
    
    chunks = ok.subsets
    pixels_per_chunk = ok.pixels_per_chunk
    n_chunks = n_elements(chunks)
    current_chunk = 0          ;an index
    
    
    ;write the file blank
    
    openw, un, outfile, /get_lun
    layersize = 		long(hdr.filesize[0]) * $          ;we'll use this for writing later
      hdr.filesize[1] *  $
      bytes_per_pixel
      
    filesize = layersize * n_output_layers
    point_lun, un, filesize - 2         ;-2 because we're going to write
    ;a blank pixel
    writeu, un, 0
    free_lun, un         ;now the file e
    
    bighdr = hdr	;copy so small chunks don't mess up
    
    
    ;now go through the image chunks and create TC values
    
    
    chunkprogressBar = Obj_New("PROGRESSBAR", /fast_loop, title = 'Processing chunks')
    chunkprogressBar -> Start
    openw, un, outfile, /get_lun
    
    
    for ch = 0, n_chunks-1 do begin	;go through chunks in the image
    
      subset = chunks[ch].coords
      within_layer_offset = chunks[ch].within_layer_offset*2
      zot_img, image_info[i].image_file, hdr, img, subset=subset
      
      img = reform(img, long(hdr.filesize[0])*hdr.filesize[1], 6)
      
      for layers = 0l, 2 do begin
        ;check on main progress bar
      
        if chunkprogressBar -> CheckCancel() then begin
          chunkprogressBar -> destroy
          print, 'chunk', string(current_chunk)
        end
        
        multiplier = 1
        coeffs = all_coeffs[*,layers]
        ;tcval = fix(round((coeffs ## img)*10))
        tcval = fix(round((coeffs ## img)*multiplier)) ;don;t multiply by 10!!!! we want the scalar for the raw iamges
        point_lun, un, (layers*layersize)+within_layer_offset
        writeu, un, tcval
      end
      percent_done = float(ch+1)/n_chunks
      chunkprogressbar -> Update, percent_done * 100
    end
    
    chunkprogressbar -> Destroy
    
    
    free_lun, un
    
    image_info[i].tc_file = outfile	;assign this file name to the image info
    hdr1 = bighdr
    hdr1.n_layers = 3
    hdr1.pixeltype = 6
    write_im_hdr, 	outfile, hdr1
    
    ;create the metadata structure
    meta = make_metadata_for_preprocessing(outfile, tcsrcimg=image_info[i].image_file, tcmultiplier=multiplier, tccoeffs=all_coeffs)
    metaout = stringswap(outfile, ".bsq", "_meta.txt")
    concatenate_metadata, image_info[i].image_file, metaout, params=meta
    
    
    skipthisone:
    percent_done = float(i+1)/n_files
    mainprogressBar-> update, percent_done * 100, text = strcompress('Done with '+string(i+1)+' of '+string(n_files))
    if mainprogressbar-> checkcancel() then mainprogressbar -> Destroy
  end 	;n _files
  
  mainprogressbar -> Destroy
  print, 'done'
  
  return
end



