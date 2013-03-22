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

pro write_im_hdr, bsq_filename, imagine_hdr

;given an imagine hdr format, this will write out to a file
;  enough information to be used later by zot_img_bsq
;The filename passed to this file is the '...bsq' file, and this
;  program swaps ".hdr" for ".bsq"

h=imagine_hdr
nf = strmid(bsq_filename, 0, strlen(bsq_filename)-4)+'.hdr'
openw, un, nf, /get_lun

printf, un, 'BANDS: '+ strcompress(h.n_layers, /rem)
printf, un, 'ROWS: '+ strcompress(h.filesize[1], /rem)
printf, un, 'COLS: '+ strcompress(h.filesize[0], /rem)

;figure out the pixeltype
case 1 of
(h.pixeltype eq 3):  pixeltype = 'U8'
(h.pixeltype eq 5):  pixeltype = 'U16'
(h.pixeltype eq 6):  pixeltype = 'S16'
(h.pixeltype eq 7):  pixeltype = 'U32'
(h.pixeltype eq 8):  pixeltype = 'S32'
(h.pixeltype eq 9):  pixeltype = 'F32'
else:  begin
	print, 'This type of image is not recognized by write_im_hdr.pro'
	print, 'Tough nuggies.'
	stop
       end
endcase


printf, un, 'DATATYPE: '+pixeltype
printf, un, 'UL_X_COORDINATE: '+strcompress(h.upperleftcenter[0],/rem)
printf, un, 'UL_Y_COORDINATE: '+strcompress(h.upperleftcenter[1],/rem)
printf, un, 'LR_X_COORDINATE: '+strcompress(h.lowerrightcenter[0],/rem)
printf, un, 'LR_Y_COORDINATE: '+strcompress(h.lowerrightcenter[1],/rem)
printf, un, 'PIXEL_WIDTH: '+strcompress(h.pixelsize[0], /rem)
printf, un, 'PIXEL_HEIGHT: '+strcompress(h.pixelsize[1], /rem)

free_lun, un
end

