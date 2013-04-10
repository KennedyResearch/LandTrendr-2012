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

PRO lt_label_class_filter, label_image_directory, output_image_directory,$
    mmu=mmu, subset=subset, all_neighbors=all_neighbors, by_year=by_year
    
  COMPILE_OPT idl2
  
  file_mkdir, output_image_directory
  
  ;default use 11 pixels as minimum mapping unit
  if N_ELEMENTS(mmu) eq 0 then mmu = 11
  
  ;output patch image
  label_imgs = file_search(label_image_directory, "*.bsq", count=imgcounts)
  
  if imgcounts eq 0 then return
  
  ;ignore __LTlabel.bsq
  apos = strpos(label_imgs, "__LTlabel.bsq")
  valid = where(apos eq -1, nv)
  if nv gt 0 then label_imgs = label_imgs[valid]
  
  ;ignore _ftv_predist.bsq
  apos = strpos(label_imgs, "_ftv_predist.bsq")
  valid = where(apos eq -1, nv)
  if nv gt 0 then label_imgs = label_imgs[valid]
  
  filelabel = strcompress("mmu" + string(mmu)+"_", /re)
  
  for i = 0, nv-1 do begin
  
    print, "Processing " + label_imgs[i]
    
    filtered_image = output_image_directory + filelabel + file_basename(label_imgs[i])
    out_patch_file = output_image_directory + filelabel + "patchid_" + file_basename(label_imgs[i])
    
    
    ;read image
    if N_ELEMENTS(subset) eq 0 then begin
      zot_img, label_imgs[i], hdr, imgdat ;, /hdronly
      subset = [[hdr.upperleftcenter], [hdr.lowerrightcenter]]
    endif else begin
      zot_img, label_imgs[i], hdr, imgdat, subset=subset ;, /hdronly
    endelse
    
    ;ignore file not in duration, magnitude, uration, predisturbance format
    if hdr.n_layers mod 4 ne 0 then begin
      print, "Ignoring file " + label_imgs[i]
      continue
    endif
    
    fs = hdr.filesize
    layersize = ULONG64(fs[0]) * fs[1]
    byte_per_pixel = 2
    
    IF file_exists(filtered_image) EQ 0 THEN BEGIN
      OPENW, un, filtered_image, /get_lun
      POINT_LUN, un, layersize*byte_per_pixel*hdr.n_layers-byte_per_pixel
      WRITEU, un, 0s
      FREE_LUN, un
      this_hdr = hdr
      write_im_hdr, filtered_image, this_hdr
    END
    
    IF file_exists(out_patch_file) EQ 0 THEN BEGIN
      OPENW, un, out_patch_file, /get_lun
      POINT_LUN, un, layersize*4-4
      WRITEU, un, 0u
      FREE_LUN, un
      this_hdr = hdr
      this_hdr.n_layers = 1
      this_hdr.pixeltype = 7 ; u32
      write_im_hdr, out_patch_file, this_hdr
    END
    
    pid = ulonarr(fs[0], fs[1])
    
    ;go through each label and patch filter it.
    for layer = 0, hdr.n_layers-1 do begin
      current_layer = imgdat[*,*,layer]
      
      if max(current_layer) le 0 then continue
      mag = imgdat[*,*,layer+1]
      dur = imgdat[*,*,layer+2]
      pre = imgdat[*,*,layer+3]
      
      if KEYWORD_SET(by_year) then begin
        allyears = fast_unique(current_layer)
        allyears = allyears[sort(allyears)]
        allyears = allyears[where(allyears gt 0)]
        
        for yidx = 0, n_elements(allyears)-1 do begin
          print, string(9b) + " processing year " + string(allyears[yidx])
          
          if KEYWORD_SET(all_neighbors) then $
            dana = OBJ_NEW("BLOB_ANALYZER", current_layer eq allyears[yidx], /all_neighbors) $
          else $
            dana = OBJ_NEW("BLOB_ANALYZER", current_layer eq allyears[yidx])
            
          FOR j = 0, dana->NumberOfBlobs()-1 DO BEGIN
            victims = dana->GetIndices(j)
            ;if patch is too small
            patch_size = N_ELEMENTS(victims)
            IF (patch_size LT mmu) THEN BEGIN
              current_layer[victims] = 0
              mag[victims] = 0
              dur[victims] = 0
              pre[victims] = 0
            ENDIF
          ENDFOR ;end for all patches
          OBJ_DESTROY, dana
          
          mean_yr = median(current_layer, 3)
          mean_mag = median(mag, 3)
          mean_dur = median(dur, 3)
          mean_pre = median(pre, 3)
          
          holes = OBJ_NEW("BLOB_ANALYZER", current_layer eq 0); allyears[yidx])
          FOR j = 0, holes->NumberOfBlobs()-1 DO BEGIN
            victims = holes->GetIndices(j)
            ;if patch is too small than defined threshold
            patch_size = N_ELEMENTS(victims)
            IF (patch_size LT mmu) THEN BEGIN
              current_layer[victims] = max(mean_yr[victims])
              mag[victims] = max(mean_mag[victims])
              dur[victims] = max(mean_dur[victims])
              pre[victims] = fix(mean(mean_pre[victims]))
            ENDIF
          ENDFOR
          OBJ_DESTROY, holes
          
          ;grab patch definitions, only need this once
          if layer eq 0 then begin
          
            wdat = current_layer eq allyears[yidx]
            if max(wdat) gt 0 then begin
                if KEYWORD_SET(all_neighbors) then $
                    dana = OBJ_NEW("BLOB_ANALYZER", current_layer eq allyears[yidx], /all_neighbors) $
                else $
                    dana = OBJ_NEW("BLOB_ANALYZER", current_layer eq allyears[yidx])
              
                this_pid = *(dana->PatchImage())
                pid = temporary(pid) + (this_pid * 100 + allyears[yidx] - 1970) * (this_pid gt 0)
                OBJ_DESTROY, dana
             endif
          endif
        endfor
      ENDIF ELSE BEGIN
        if KEYWORD_SET(all_neighbors) then $
          dana = OBJ_NEW("BLOB_ANALYZER", current_layer, /all_neighbors) $
        else $
          dana = OBJ_NEW("BLOB_ANALYZER", current_layer)
          
        FOR j = 0, dana->NumberOfBlobs()-1 DO BEGIN
          victims = dana->GetIndices(j)
          ;if patch is too small
          patch_size = N_ELEMENTS(victims)
          IF (patch_size LT mmu) THEN BEGIN
            current_layer[victims] = 0
            mag[victims] = 0
            dur[victims] = 0
            pre[victims] = 0
          ENDIF
        ENDFOR ;end for all patches
        OBJ_DESTROY, dana
        
        ;filter out holes of nondisturbance in side disturbance
        ;largest disturbance
        mean_yr = median(current_layer, 3)
        mean_mag = median(mag, 3)
        mean_dur = median(dur, 3)
        mean_pre = median(pre, 3)
        
        holes = OBJ_NEW("BLOB_ANALYZER", current_layer eq 0)
        FOR j = 0, holes->NumberOfBlobs()-1 DO BEGIN
          victims = holes->GetIndices(j)
          ;if patch is too small than defined threshold
          patch_size = N_ELEMENTS(victims)
          IF (patch_size LT mmu) THEN BEGIN
            current_layer[victims] = max(mean_yr[victims])
            mag[victims] = max(mean_mag[victims])
            dur[victims] = max(mean_dur[victims])
            pre[victims] = fix(mean(mean_pre[victims]))
          ENDIF
        ENDFOR
        OBJ_DESTROY, holes
        
        ;grab patchid
        if layer eq 0 then begin
          if KEYWORD_SET(all_neighbors) then $
            dana = OBJ_NEW("BLOB_ANALYZER", current_layer, /all_neighbors) $
          else $
            dana = OBJ_NEW("BLOB_ANALYZER", current_layer)
            
          this_pid = *(dana->PatchImage())
          pid = temporary(pid) + this_pid
          OBJ_DESTROY, dana
        endif
      ENDELSE
      
      ;write out filtered data
      OPENU, un, filtered_image, /get_lun
      point_lun, un, layersize*byte_per_pixel*layer
      writeu, un, current_layer
      writeu, un, mag
      writeu, un, dur
      writeu, un, pre
      free_lun, un
      
      ;write out patch file
      if layer eq 0 then begin
        OPENU, un, out_patch_file, /get_lun
        writeu, un, pid
        free_lun, un
      endif
      
      current_layer = 0
      mag = 0
      dur = 0
      pid = 0
      
      ;move to the next disturbance year layer
      layer = layer+3
    endfor
  endfor
  
  print, "LT_LABEL_CLASS_FILTER done!"
END