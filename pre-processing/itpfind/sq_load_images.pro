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

;
; Copyright (c) 1998, Oregon State University.
; This software is currently in beta-test condition.  It may not be altered,
;   copied, or redistributed without express written permission of
;   Robert E. Kennedy, Oregon State University (kennedyr@fsl.orst.edu).
;   This software is supplied as is, with no express or implied
;   warranties.

;#############################################################################
function sq_load_images, image_info, params, diag_un1=diag_un1

  ;called by squeeter.function
  ;image_info is a structure equivalent to "ref" or "input", as defined
  ;   in squeeter.function
  ;params is equivalent to "params" in squeeter.function

  ;The function returns a structure  with the subsetted image, a structure of
  ;   info like that laid out in squeeter for this subsetted image, and
  ;   the potential edge-offset that would need to be applied to the
  ;   next call to this routine, and a flag to determine if the
  ;   routine was valid.
  ;   .img
  ;   .img_info
  ;   .offset
  ;   .valid
  ;   Valid = 0 -- Window size is larger than the image we're working from
  ;   Valid = 1 -- Everything okay.



  ;call define_window to determine the correct positioning of the this window.

  cp = image_info.centerpoint
  if n_elements(diag_un1) ne 0 then $
    write_diagnosis, diag_un1, 'sq_load_images: before def_window'

  win_info = def_window(cp, image_info, params)
;print, 'win_info'
;pst, win_info
;stop

  if n_elements(diag_un1) ne 0 then $
    write_diagnosis, diag_un1, 'sq_load_images: before valid test'

  if win_info.valid eq 0 then return, $
    {img:[0], img_info:[0], offset:[-1,-1], $
    shift:[0,0], valid:win_info.valid}
  ;movevalue is in map units


  ;at this point, define_window has given us the coordinates that will
  ;  give us data within the image:  wininfo.corners
  ;call zot_img with /corner option, so it knows that these subset
  ;   values are the outer edges of the pixels

  sendb = win_info.corners
  sendb = cut_tiny(sendb, .0001)

  if n_elements(diag_un1) ne 0 then $
    write_diagnosis, diag_un1, 'sq_load_images: before zot_img'

  zot_img, image_info.filename, imghdr, tmp_img, subset = sendb, $
    layer=image_info.layer, /corner, valid = valid
  write_diagnosis, diag_un1, 'sq_load_images: completed zot_img'

;stop

  if valid eq 0 then begin
    win_info.valid = 0
    goto, past
  end



  ;catch errors

  if n_elements(tmp_img) eq 0 then return, {valid:0}


  ;because of the "edgezone," we expect to go outside the edge
  ;  of the image occasionally.  In that case, we need to place
  ;  the data in the correct portion of the image and fill the
  ;  rest with zeros(or other ignore value)

  thisdiff = (win_info.corners-sendb)/ $
    ([ [image_info.pixelsize], [image_info.pixelsize] ])*[ [1,-1], [1,-1] ]
;print, thisdiff


  if total(thisdiff) ne 0  then begin
    thisdiff= round(thisdiff)   ;remove weird teeny float point hangers-on
;print, thisdiff
;print, 'imghdr'
;print, imghdr.upperleftcenter
;print, imghdr.lowerrightcenter
;
;stop
;
    write_diagnosis, diag_un1, 'sq_load_images:  before match_type'

    img = match_type(tmp_img, [params.window_size(0), params.window_size(1)]) + $
      image_info.ignore
    write_diagnosis, diag_un1, 'sq_load_images:  after match_type'




    a = img[(0-thisdiff[0,0]):(params.window_size(0)-1-thisdiff[0,1]),$
      (0-thisdiff[1,0]):(params.window_size(1)-1-thisdiff[1,1])]
    asize = size(a, /dim)
    tmpsize= size(tmp_img, /dim)
    if total(asize - tmpsize) ne 0 then $;stop
        return, {img:tmp_img, img_info:0, offset:0, shift:0, valid:0}


    img[(0-thisdiff[0,0]):(params.window_size(0)-1-thisdiff[0,1]),$
      (0-thisdiff[1,0]):(params.window_size(1)-1-thisdiff[1,1])] = tmp_img



    tmp_img=img
  end



  ;mask out places desired by user
  if matchstr(tag_names(image_info), 'maskbelow') ne -1 then begin

    if image_info.maskbelow ne -32767 then begin
      z=where(tmp_img lt image_info.maskbelow, many)
      if many ne 0 then tmp_img(z) = image_info.ignore
    end
  end

if matchstr(tag_names(image_info), 'maskabove') ne -1 then begin
    if image_info.maskabove ne 32767 then begin
      z=where(tmp_img gt image_info.maskabove, many)
      if many ne 0 then tmp_img[z] = image_info.ignore
    end
  end


  ;set up information stuff
  past:
  img_info={filename:image_info.filename, upl:win_info.corners(*,0), $
    lor:win_info.corners(*,1), ignore:image_info.ignore, $
    pixelsize:image_info.pixelsize, centerpoint:win_info.centerpoint, $
    layer:image_info.layer, gifov:image_info.gifov}
  write_diagnosis, diag_un1, '        sq_load_images: finished'
  write_diagnosis, diag_un1, '  '


  return, {img:tmp_img, img_info:img_info, offset:win_info.offset[*,0], $
    shift:win_info.shift, valid:win_info.valid}
end


