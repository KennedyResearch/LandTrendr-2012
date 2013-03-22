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

pro add_years_to_fittedimages, path, diag_info_file, from_ftv=from_ftv

  ;given the image info file, this will add the "wavelength"
  ;  to the envi header of the fitted and source images
  ;  for better plotting.

  restore, diag_info_file
  
  ;if we're adding to an image from original fitting, the source
  ;   and fitted are in different bands than if from ftv run.
  
  if n_elements(from_ftv) eq 0 then target_indices = [5,8] else $
    target_indices = [1,4]
  
  ;find the fitted and source images  
  fitted_image_base = file_basename(diag_info.output_image_group[target_indices[0]].filename)
  source_image_base = file_basename(diag_info.output_image_group[target_indices[1]].filename)
  
  fitted_image = file_search(path, fitted_image_base, count=n_fitted_image)
  if n_fitted_image gt 1 then message, "there can only be one image - this is what was found: ", transpose(fitted_image) 
  if n_fitted_image eq 0 then message, "no image was found in the processing folder for file base: ", fitted_image_base 
  
  source_image = file_search(path, source_image_base, count=n_source_image)
  if n_source_image gt 1 then message, "there can only be one image - this is what was found: ", transpose(source_image) 
  if n_source_image eq 0 then message, "no image was found in the processing folder for file base: ", source_image_base

  files = [fitted_image, source_image]
  
  for filecount = 0,1 do begin
    this_image = files[filecount]
    fitted_hdr = strmid(this_image,0, strlen(this_image)-3)+'hdr'
    openu, un, fitted_hdr, /get_lun
    a=''
    readf, un, a
    if strupcase(strcompress(a, /rem)) eq 'ENVI' then begin
    
      ;check to see if wavelength is already in there
      done = 0
      
      while not (eof(un)) do begin
        readf, un, a
        if strmid(a, 0,  12) eq 'wavelength =' then done=1  
      endwhile
      
      ;if we haven't done it already, add the wavelengths
      if done ne 1 then begin
        ;get the years
        
        if keyword_set(from_ftv) eq 1 then begin  
          n_source = diag_info.output_image_group[4].n_layers
          n_fitted = diag_info.output_image_group[1].n_layers
          dif = n_fitted-n_source
          if dif ne 0 then begin
            years = diag_info.apply_to_image_info.year
            yrs = uintarr(n_fitted)
            for this=0, n_fitted-1 do yrs[this] = min(years)+this  
          endif else yrs = fast_unique(diag_info.apply_to_image_info.year)
        endif else yrs = fast_unique(diag_info.image_info.year)
                  
        yrs = yrs[sort(yrs)]
        
        outstring = 'wavelength = { '
        outstring = outstring+string(yrs[0])
        
        for i = 1, n_elements(yrs)-1 do outstring= outstring + ', '+string(yrs[i])
        outstring=outstring+'}'
        ;print, outstring
        
        ;write it
        fi = file_info(fitted_hdr)
        point_lun, un, fi.size
        printf, un, outstring
      end
      free_lun, un   
    end
  end ;going through files
  
  return
  
end

