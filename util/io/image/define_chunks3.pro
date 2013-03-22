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

function define_chunks3, subset, pixsize, max_pixels_per_chunk, kernelsize

;differs from 2 in that this one takes into account
;  the kernel size
;differs from 1 in that this one does not figure
;   out file names, since we're now in the
;   process_dist_rec_image.pro realm, where we just write
;   one big output image.
;  but it does include the number of pixels per chunk, since
;      we're going to need to point_lun within the file to
;     put things.

;updating june 18, 2006 to make more precise, so
;   the returned "pixels per chunk" is accurate


;figure out the size of the whole subset

;  xsize = (subset[0,1]-subset[0,0]) / pixsize[0]
;  ysize = (subset[1,0]-subset[1,1]) / pixsize[1]



  xsize = round(((subset[0,1]-subset[0,0]) / pixsize[0])) + 1   ;have to add 1 because we want inclusive pixels
  ysize = round(((subset[1,0]-subset[1,1]) / pixsize[1])) + 1      ;i.e. if coords were 0 and 9, there are 10
                                       ; pixels but only 9 if we do 9-0.

if xsize lt 0 or ysize lt 0 then begin
   print, 'Subset coordinates result in negative size'
   print, 'Subset upper left:  '
   print, subset[*,0]
   print, 'Subset lower right: '
   print, subset[*,1]
   return, {ok:0}


end



;totalsize

  totalsize = long(xsize) * long(ysize)


;adjust the max pixels per chunk to consider the fact we need
;  to add buffer if there's a kernelsize
  kernelsize = kernelsize
  pseu_max_pixels_per_chunk = max_pixels_per_chunk - (xsize * kernelsize)
  if pseu_max_pixels_per_chunk lt 0 then message, 'Need to increase max_pixels_per_chunk to at least '+ string(xsize * kernelsize * 2)

;number of chunks

   n_chunks = ceil(float(totalsize) / pseu_max_pixels_per_chunk)
   actual_y_per_chunk = floor(float(pseu_max_pixels_per_chunk) / xsize)     ;what's the y val that will approximate the
                                         ;this actual_y_per_chunk is the size of the non-overlapping section of the image
                                         ;  when actually processing, we'll add on the offsety to the bottom

   diffy = actual_y_per_chunk * pixsize[1]			;keep diffy separate from offsety  b/c diffy used to start upperleft of each
   offsety = kernelsize * pixsize[1]

   actual_pixels_per_chunk = (actual_y_per_chunk+offsety-1) * xsize  ;subtract 1 because we clip the last line so as not to repeat


;OLD
;;now get the yspacing, take the entire x range per chunk
;
;  actual_pixels_per_chunk = totalsize / n_chunks
;  actual_y_per_chunk = actual_pixels_per_chunk / xsize
;  diffy= fix(actual_y_per_chunk * pixsize[1])          ;june 18, added "fix" to ensure integer, not float


;set up subsets

  base = {coords:dblarr(2,2), within_layer_offset:0l}
  subsets = replicate(base, n_chunks)


  ;names = strarr(n_chunks)+filebase

for i = 0, n_chunks-1 do begin

  subsets[i].coords = $
    [ [subset[0,0], subset[1,0]-(i*diffy)], $
       [subset[0,1], subset[1,0]-((i+1)*diffy)-(offsety)] ]


       																					;+pixsize[1] pushes it up one from the next
                                               ; line, so we avoid double-processing that line

  subsets[i].within_layer_offset = i*actual_y_per_chunk*xsize         ;in units of pixels.  still need to multiply by bytes per pixel


  ;names[i] = names[i]+'part'+strcompress(string(i+1)+'.bsq', /rem)
end


subsets[n_chunks-1].coords[1,1] = subset[1,1]   ;get to bottom of image on last chunk


;also calculate the numbe of pixels per
;  chunk




return, {ok:1, subsets:subsets, pixels_per_chunk:actual_pixels_per_chunk}

;return, {ok:1, subsets:subsets, filenames:names}

end
