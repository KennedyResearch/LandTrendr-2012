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

function create_dist_rec_snapshots, path, diag_info_sav_file
  
  ;------ identify path separator -------
  pse = path_sep()


  ;access the diag_info file
  restore, diag_info_sav_file 
  
  ;find the fitted image
  fitted_image_base = file_basename(diag_info.output_image_group[5].filename)
  fitted_image = file_search(path+"outputs"+pse, "*"+fitted_image_base, count=n_fitted_image)
  if n_fitted_image eq 0 then message, "cannot find a file: "+fitted_image_base+" in "+path+"outputs"+pse
  
  ;find the index
  index = diag_info.index
  
  ;points in the direction of disturbance.  Make sure this is consistent
  ;with the modifier in the change label image if you're doing both!
  ;For wetness, NBR, and NDVI, this should be set to -1
  ;For band 5 or brightness, this should be set to 1
  
  ;using the index - figure out what the modifier should be
  case index of
    'band1':modifier = 1
    'band2':modifier = 1
    'band3':modifier = 1
    'band4':modifier = -1
    'band5':modifier = 1
    'band7':modifier = 1
    'nbr':modifier = -1
    'wetness': modifier = -1
    'brightness': modifier = 1
    'greenness': modifier = -1
    'tcangle':modifier = -1
    'ndvi':modifier = -1
    'biomass':modifier = -1
    'probfor':modifier = -1
  endcase
  
  ;get header
  zot_img, fitted_image, hdr, fit_img, /hdronly
  
  filecomponent = file_basename(fitted_image)
  pathcomponent = file_dirname(fitted_image)+'\'
  
  ;get the core filename without "_fitted.bsq"
  core = strmid(filecomponent, 0, strpos(filecomponent, "_", /reverse_search))
  
  ;make the output image
  outfile1 = pathcomponent + core + '_disturbance.bsq'
  outfile2 = pathcomponent + core + '_recovery.bsq'
  
  ;check and see if they exist - if they do stop the process
  exists1 = file_test(outfile1)
  if exists1 ge 1 then begin
    print, ""
    print, ">>> warning! *disturbance.bsq output has already been created for this diag file:"
    print, "    ",diag_info_sav_file
    print, ">>> if you want to rerun it you have to delete the 'disturbance' output files"   
    print, ""
  endif
  exists2 = file_test(outfile2)
  if exists2 ge 1 then begin
    print, ""
    print, ">>> warning! *recovery.bsq output has already been created for this diag file:"
    print, ">>> ",diag_info_sav_file
    print, ">>> if you want to rerun it you have to delete the 'recovery' output files"   
    print, ""
  endif
  
  ;stop the process if either the disturbance or recovery layer exists for the  given diag file
  if exists1 + exists2 ge 1 then goto, endit 
  
  ;if all is good then continue running
  if hdr.pixeltype eq 6 then bytes_per_pixel = 2 else begin
    envi_report_error, 'Cannot read non-integer types.  Have REK fix.'
    return, {ok:0}
    
  end
  
  
  
  layersize = ulong(hdr.filesize[0]*hdr.filesize[1]*bytes_per_pixel)
  
  openw, un1, outfile1, /get_lun
  
  openw, un2, outfile2, /get_lun
  
  
  
  ;we skip the first layer, but keep it the same size as the
  ;   fitted image just to make it easier to use
  
  zot_img, fitted_image, hdr, odd_img, layers = [1]
  
  for i = 2, hdr.n_layers do begin
    print, ">>> processing band: ",  i+1
    ;read in the image and subtract using even and odd
    ;  to prevent reading image layers twice and swapping
    ;  variables
  
    if (i mod 2) then begin
      zot_img, fitted_image, hdr, odd_img, layers = [i]
      outlayer = odd_img - even_img
    end else begin
      zot_img, fitted_image, hdr, even_img, layers = [i]
      outlayer = even_img-odd_img
    end
    
    ;adjust according to the modifier for this index, since
    ;  disturbance is different direction depending on the index
    
    outlayer =outlayer * modifier
    
    ;then write it out
    point_lun, un1, layersize * (i-1)		;again, skip layer 0
    writeu, un1, (outlayer gt 0)*outlayer		;disturbance
    point_lun, un2, layersize * (i-1)		;again, skip layer 0
    writeu, un2, (outlayer lt 0)*outlayer* (-1)		;recovery
    
  ;envi_report_stat, status_win, i-1, hdr.n_layers-1
  end	;i
  free_lun, un1
  free_lun, un2
  
  ;copy the file header
  inhdr = strmid(fitted_image, 0, strlen(fitted_image)-3)+'hdr'
  outhdr1 = strmid(outfile1, 0, strlen(outfile1)-3)+'hdr'
  file_copy, inhdr, outhdr1, /overwrite
  outhdr2 = strmid(outfile2, 0, strlen(outfile2)-3)+'hdr'
  file_copy, inhdr, outhdr2, /overwrite
  
  
  
  endit:
  return, {ok:1, disturbance_file:outfile1, recovery_file:outfile2}
  
end ;make yoy image
