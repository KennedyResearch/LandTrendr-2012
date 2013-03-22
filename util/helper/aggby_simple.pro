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

function aggby_simple, img, aggbyval, hdr = hdr, ignore = ignore


;unlike aggby, this one is simple
;just starts at upper left and starts aggregating
;assumes same x and y -- aggbyval is a scalar.
;hdr is an imagine header -- if set, this will return
;if an ignore value is found anywhere inside the aggregate
;   area, the result for that aggregate pixel is the ignore
;   value.


if n_elements(ignore) eq 0 then ig = 0 else ig = 1
sz = size(img, /dim)

newdim = ceil(sz[0:1] / float(aggbyval))


if ig eq 0 then begin
if n_elements(sz) eq 3 then begin	;if image with multiple layers
	nlayers = sz[2]
	newimg = img[0:newdim[0]-1, 0:newdim[1]-1, *]	;use passed image as template
	newimg= newimg-newimg	;reset it

			for l = 0, nlayers-1 do begin
			  for i = 0, newdim[0]-1 do begin
			    for j = 0, newdim[1]-1 do begin
             maxx = (i*aggbyval)+aggbyval-1
             maxy = (j*aggbyval)+aggbyval-1
             if maxx ge sz[0] then maxx = sz[0]-1
             if maxy ge sz[1] then maxy = sz[1]-1
             newimg[i,j,l] = mean(img[i*aggbyval:maxx, j*aggbyval:maxy, l])
			    end
			  end
			end

end else begin	;if just single layer image
	  nlayers = 1
  	newimg = img[0:newdim[0]-1, 0:newdim[1]-1]	;use passed image as template
  	newimg= newimg-newimg	;reset it
				for i = 0, newdim[0]-1 do begin
			    for j = 0, newdim[1]-1 do begin
             maxx = (i*aggbyval)+aggbyval-1
             maxy = (j*aggbyval)+aggbyval-1
             if maxx ge sz[0] then maxx = sz[0]-1
             if maxy ge sz[1] then maxy = sz[1]-1

             newimg[i,j] = mean(img[i*aggbyval:maxx, j*aggbyval:maxy])
			    end
			  end

end


end else begin 	;if ignore IS set, need to add in a line for each pixel
if n_elements(sz) eq 3 then begin	;if image with multiple layers
	nlayers = sz[2]
	newimg = img[0:newdim[0]-1, 0:newdim[1]-1, *]	;use passed image as template
	newimg= newimg-newimg	;reset it

			for l = 0, nlayers-1 do begin
			  for i = 0, newdim[0]-1 do begin
			    for j = 0, newdim[1]-1 do begin
             maxx = (i*aggbyval)+aggbyval-1
             maxy = (j*aggbyval)+aggbyval-1
             if maxx ge sz[0] then maxx = sz[0]-1
             if maxy ge sz[1] then maxy = sz[1]-1
             chunk = img[i*aggbyval:maxx, j*aggbyval:maxy, l]
             bads = where(chunk eq ignore, many)
             if many eq 0 then newimg[i,j,l] = mean(chunk) else newimg[i,j,l] = ignore
			    end
			  end
			end

end else begin	;if just single layer image
	  nlayers = 1
  	newimg = img[0:newdim[0]-1, 0:newdim[1]-1]	;use passed image as template
  	newimg= newimg-newimg	;reset it
				for i = 0, newdim[0]-1 do begin
			    for j = 0, newdim[1]-1 do begin
             maxx = (i*aggbyval)+aggbyval-1
             maxy = (j*aggbyval)+aggbyval-1
             if maxx ge sz[0] then maxx = sz[0]-1
             if maxy ge sz[1] then maxy = sz[1]-1
             chunk = img[i*aggbyval:maxx, j*aggbyval:maxy]
             bads = where(chunk eq ignore, many)
             if many eq 0 then newimg[i,j] = mean(chunk) else newimg[i,j] = ignore
			     end
			  end

end
end ; if ignore is set




;if header is set, then we return a new one as well

if n_elements(hdr) ne 0 then begin
  newhdr = hdr
  newhdr.pixelsize = newhdr.pixelsize * aggbyval
  newhdr.upperleftcenter = newhdr.upperleftcenter - $
  										(hdr.pixelsize*0.5)*[1,-1] + $ 	;first go to upper left of small pixel
  										(newhdr.pixelsize*.5)*[1,-1]					;then swoop down to center of new big pixel
  newhdr.lowerrightcenter = newhdr.upperleftcenter + $
  									  ((newdim-[1,1]) * newhdr.pixelsize)*[1,-1]   ;calc lower right from upper left
  									  																						;note that number of increments from centers
  									  																						;  is 1 less than the total dimension, hence
  									  																						;  (newdim-[1,1])

  newhdr.filesize = newdim

  return, {ok:1, newimg:newimg, newhdr:newhdr}
end else return, {ok:1, newimg:newimg}


end

