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

function run_composition, info, subset, outputbase, tc=tc

  if n_elements(tc) eq 0 then tc = 0
  
  years = fast_unique(info.year)
  years = years[sort(years)]
  n_yrs = n_elements(years)
  
  imgattr = {platform:"TM", year:1984, julday:180, label:1984,file:""}
  imglist = replicate(imgattr, n_yrs)
  
  n_images = n_elements(info)
  
  zot_img, info[0].image_file, hdr, img, subset=subset, layer=[1], /hdronly
  if hdr.pixeltype ne 6 and hdr.pixeltype ne 3 and hdr.pixeltype ne 5 then begin
    print, 'run_composition_single_chunk expects the image to be of integer type'
    print, 'this one is type number '+string(hdr.pixeltype)
    return, {ok:0}
  end
  
  ;prepare the image files etc..
  n_layers = 6 ;by default it is for reflectance data
  index = '_refl_'
  imgfiles = info.image_file
  if tc eq 1 then begin
    n_layers = 3
    index = '_tc_'
    imgfiles = info.tc_file
  endif
    
  ;now go through and build it.
  k = 0
  for i = 0, n_yrs-1 do begin
    fileid = i + k
    this = where(info.year eq years[i], n)
    print, 'processing ' + string(years[i])
    ;MULTIPLE IMAGES PER YEAR
    if n gt 1 then begin
      print, '    compositing ...'
      tempsubset=subset
      victims = info[this]
      fileused = imgfiles[this]
      
      ;sort by priority
      vicorder = sort(victims.image_priority)
      victims = victims[vicorder]
      fileused = fileused[vicorder]

      ;result image for this year
      ;img = intarr(hdr.filesize[0], hdr.filesize[1], n_layers)
      zot_img, fileused[0], thdr, img, subset=tempsubset, layers=indgen(n_layers)+1 

      ;current year curent layer
      cur_mask = bytarr(hdr.filesize[0], hdr.filesize[1])
      
      imglist[i].year = victims[0].year
      imglist[i].julday = victims[0].julday
      imglist[i].label = victims[0].year
      
      ;now process it year by year
      for j = 0, n-1 do begin
        ; now read the cloud mask
        ;cloud mask for this year
        cld_img = bytarr(hdr.filesize[0], hdr.filesize[1])
        ; if there is no cloud mask, then just skip this
        if victims[j].cloud_file ne 'none' and victims[j].cloud_file ne '' then begin
          if victims[j].cloud_file eq 'band8' then begin
            zot_img, victims[j].image_file, clhdr, mimg, layers=[8], subset=tempsubset 
          endif else begin
            zot_img, victims[j].cloud_file, clhdr, mimg, subset=tempsubset
          endelse
          cld_img = (mimg eq 0)
        end

        ;identify images that have not been picked by the higher priority image and that are not in the cloud mask for this year
        valid = where(cur_mask eq 0 and cld_img eq 0, n_valid)
        
        for bi = 1, n_layers do begin
          cur_img = img[*,*,bi-1]
          zot_img, fileused[j], chdr, layer_img, subset=tempsubset, layers=[bi]
          
          if n_valid ne 0 then begin
            cur_img[valid] = layer_img[valid]
            cur_mask[valid] = 1 ;mask set to 1 if the pixel is chosen
            img[*,*,bi-1] = cur_img
          end 
        endfor ;bi
      endfor ;j
      ;now write out this image
      outfile = strcompress(outputbase + "_composite_" + index + string(years[i])+".bsq", /re)
      imglist[i].file = outfile

      openw, un, outfile, /get_lun
      writeu, un, img
      free_lun, un
      hdr1 = hdr
      hdr1.n_layers = n_layers
      write_im_hdr, outfile, hdr1
    endif else begin ;only one image for this year
      selected = info[this]
      imglist[i].year = selected.year
      imglist[i].julday = selected.julday
      imglist[i].label = selected.year
      imglist[i].file = imgfiles[this]
    end
  endfor; n_yrs
  
  ;create image list file
  imglist_file = strcompress(outputbase + "_imglist.txt", /re)
  export_structure_to_file, imglist, imglist_file
  
  return, {ok:1, imglist:imglist}
end