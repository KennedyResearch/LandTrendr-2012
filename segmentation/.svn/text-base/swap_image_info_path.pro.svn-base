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

function swap_out_one, astring, old_path, new_path
  l = strlen(old_path)
  
  match = strpos(strupcase(astring), strupcase(old_path))
  
  if match eq -1 then return, astring
  
  this_len = strlen(astring)
  if match eq 0 then firstpart = '' else firstpart=strmid(astring, 0, match)
  secondpart = new_path
  thirdpart = strmid(astring, match+l, this_len-(match+l))
  out_string = firstpart+secondpart+thirdpart

  return, out_string
end


function swap_out, string_array, old_path, new_path
  l = strlen(old_path)
  n = n_elements(string_array)
  
  out_array = strarr(n)
  
  for i = 0, n-1 do begin
    cnt = str_count(strupcase(string_array[i]), strupcase(old_path))

    out_array[i] = string_array[i]
    for k = 1, cnt do out_array[i] = slash_fix(swap_out_one(out_array[i], old_path, new_path))
  end
  return, out_array
end

function swap_image_info_path, image_info, old_path, new_path, $
      outputs_path = outputs_path, mask_image_path = mask_image_path, $
      swap_back = swap_back
      
      on_error, 1
      
    ;use /swap_back to store the old image info in case
    ;
    ;first, find out if this is a diag info structure
  
    t = tag_names(image_info)
    yes_diag = matchstr(t, 'mask_image')
    
  if yes_diag ne -1 then begin
    ;******
    ;this is a diag info
    
    diag_info = image_info
    swap_back = image_info
    
    ;first, see if the mask image path was passed, and if not
    ;  ask the user if it really is in the same place as the 
    ;  other files 
    if n_elements(mask_image_path) eq 0 then begin 
         print, 'There is no "mask_image_path" set.'
         example = swap_out(diag_info.mask_image, old_path, new_path)
         
         
         print, 'It will be set to '+example
         print, 'If this is okay, press "y".  If not, press "N" and add "mask_image_path = <desired path>" to your call to "swap_image_info_path"'
         a = get_kbrd()
         if strupcase(a) ne 'Y' then message, 'aborted'
         mask_image_path = new_path
         
     end
     if n_elements(outputs_path) eq 0 then begin 
         print, 'There is no "outputs_path" set.'
         example = swap_out(diag_info.output_image_group.filename, old_path, new_path)
         
         print, 'It will be set to '+example
         print, 'If this is okay, press "y".  If not, press "N" and add "outputs_path = <desired path>" to your call to "swap_image_info_path"'
         a = get_kbrd()
         if strupcase(a) ne 'Y' then message, 'aborted'
         outputs_path = new_path
     end
    
    diag_info.output_image_group.filename = swap_out(diag_info.output_image_group.filename, old_path, outputs_path)   ;this points to the segmentation outputs
    diag_info.mask_image = swap_out(diag_info.mask_image, old_path, mask_image_path)
    
    ;then fix the image info substructure
       
    diag_info.image_info.image_file = swap_out(diag_info.image_info.image_file, old_path, new_path)
  diag_info.image_info.nbr_file = swap_out(diag_info.image_info.nbr_file, old_path, new_path)
  diag_info.image_info.tc_file = swap_out(diag_info.image_info.tc_file, old_path, new_path)
  diag_info.image_info.b6_file = swap_out(diag_info.image_info.b6_file, old_path, new_path)
  diag_info.image_info.cloud_diff_file = swap_out(diag_info.image_info.cloud_diff_file, old_path, new_path)
  diag_info.image_info.shadow_diff_file = swap_out(diag_info.image_info.shadow_diff_file, old_path, new_path)
  diag_info.image_info.cloud_file = swap_out(diag_info.image_info.cloud_file, old_path, new_path)
  diag_info.image_info.useareafile = swap_out(diag_info.image_info.useareafile, old_path, new_path)
  return, diag_info
    
  
  
  end else begin  
  ;*********************************
    ;if this is only an image info
    
    
    
  swap_back = image_info
  
  ;swap path from one
  image_info.image_file = swap_out(image_info.image_file, old_path, new_path)
  image_info.nbr_file = swap_out(image_info.nbr_file, old_path, new_path)
  image_info.tc_file = swap_out(image_info.tc_file, old_path, new_path)
  image_info.b6_file = swap_out(image_info.b6_file, old_path, new_path)
  image_info.cloud_diff_file = swap_out(image_info.cloud_diff_file, old_path, new_path)
  image_info.shadow_diff_file = swap_out(image_info.shadow_diff_file, old_path, new_path)
  image_info.cloud_file = swap_out(image_info.cloud_file, old_path, new_path)
  image_info.useareafile = swap_out(image_info.useareafile, old_path, new_path)
  return, image_info
  end
  
  
  
  
end

