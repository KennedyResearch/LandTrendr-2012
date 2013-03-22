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

function find_union_area, image_info_savefile, subset=subset, isinfo=isinfo, checkmask=checkmask, image_list=image_list, checkusearea=checkusearea

  ;image_info_savefile is from a "find_image_stack_files....pro"
  if keyword_Set(image_list) eq 1 then begin
    n_files = n_elements(image_list)
    image_info = create_struct("image_file", "")
    image_info = replicate(image_info, n_files)
    image_info.image_file = image_list
  endif else begin
    if n_elements(isinfo) eq 0 then restore, image_info_savefile else image_info = image_info_savefile
    n_files = n_elements(image_info)
  endelse
  
  minx = (miny=0.d)
  maxx = (maxy=0.d)
  
;  zot_img, strlowcase(image_info[0].image_file), hdr, img, /hdronly
zot_img, image_info[0].image_file, hdr, img, /hdronly
 
 
  ;check if the stack is file coords or map coords
  ul = hdr.upperLeftCenter
  lr = hdr.lowerRightCenter
  if (ul[1] - lr[1]) lt 0 then begin
    print, ">> this stack uses file coordinates..."
    print, ">> checking to make sure they are..."
    print, ">> all the same size."
    xsize = uintarr(n_files)
    ysize = uintarr(n_files)
    for i = 0, n_files - 1 do begin
;      zot_img, strlowcase(image_info[i].image_file), hdr, img, /hdronly
	zot_img, image_info[i].image_file, hdr, img, /hdronly 
     xsize[i] = hdr.filesize[0]
      ysize[i] = hdr.filesize[1]
    endfor
    checkx = fast_unique(xsize)
    checky = fast_unique(ysize)
    if n_elements(checkx) gt 1 or n_elements(checkx) gt 1 then begin
      print, ">> not all files are the same dimensions..."
      print, ">> check your files and make sure that..."
      print, ">> they are the same size"
      print, "x sizes: "
      print, transpose(xsize)
      print, "y sizes: "
      print, transpose(ysize)
      stop
    endif
    minx = 1
    maxy = 1
    new_lor = [xsize[0],ysize[0]]
    goto, start_here
  endif
  
  
  for i = 0, n_files - 1 do begin
   ; zot_img, strlowcase(image_info[i].image_file), hdr, img, /hdronly
   
   zot_img, image_info[i].image_file, hdr, img, /hdronly

   if i eq 0 then begin
      minx=hdr.upperleftcenter[0]
      maxx=hdr.lowerrightcenter[0]
      miny=hdr.lowerrightcenter[1]
      maxy=hdr.upperleftcenter[1]
    end
    
    minx = max([minx, hdr.upperleftcenter[0]])
    maxx = min([maxx, hdr.lowerrightcenter[0]])
    miny = max([miny, hdr.lowerrightcenter[1]])
    maxy = min([maxy, hdr.upperleftcenter[1]])
    
    if n_elements(checkmask) ne 0 then begin
      if (image_info[i].cloud_file ne 'none') then zot_img, image_info[i].cloud_file, hdr, img, /hdronly

			; zot_img, strlowcase(image_info[i].cloud_file), hdr, img, /hdronly
      
      minx = max([minx, hdr.upperleftcenter[0]])
      maxx = min([maxx, hdr.lowerrightcenter[0]])
      miny = max([miny, hdr.lowerrightcenter[1]])
      maxy = min([maxy, hdr.upperleftcenter[1]])
    endif
    
    if keyword_set(checkusearea) eq 1 then begin
      if file_exists(image_info[i].useareafile) eq 1 then begin
        zot_img, image_info[i].useareafile, hdr, img, /hdronly
        minx = max([minx, hdr.upperleftcenter[0]])
        maxx = min([maxx, hdr.lowerrightcenter[0]])
        miny = max([miny, hdr.lowerrightcenter[1]])
        maxy = min([maxy, hdr.upperleftcenter[1]])
      endif
    endif
  endfor
  
  ;then optionally add in the subset information for a single additional file
  
  if n_elements(subset) ne 0 then begin
    sz = size(subset, /dim)
    if sz[0] ne 2 or sz[1] ne 2 then message, 'find_union_area:  Subset must be 2 by 2 array of map coords, [ [uplx, uply], [lorx, lory]]"
    
    minx = max([minx, subset[0,0]])
    maxx = min([maxx, subset[0,1]])
    miny = max([miny, subset[1,1]])
    maxy = min([maxy, subset[1,0]])
  end
  
  
  ;now bring in the buffer a bit for safety.
  
  minx = minx+(2*hdr.pixelsize[0])
  maxy = maxy-(2*hdr.pixelsize[1])
  maxx = maxx-(2*hdr.pixelsize[0])
  miny = miny+(2*hdr.pixelsize[1])
  
  ;then standardize size
  new_lor = adj_int_mult([minx, maxy], hdr.pixelsize, [maxx, miny], /map)
  
  start_here:
  return, {coords: [ [minx, maxy], [new_lor]], ok:1}
end
