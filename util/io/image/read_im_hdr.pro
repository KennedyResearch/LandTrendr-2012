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

;Procedure read_im_hdr
;
;Reads the text headers created by Imagine when exporting as generic
;	binary.
;
;Variable	type		form
;bands		integer		1 value
;size		integer		[xsize, ysize]
;bytetype	string		'byte' or 'integer'
;coords		double(2,2)	*,0 = [ulx, lrx]
;				*,1 = [uly, lry]
;pixsize	float		pixel size




pro read_im_hdr, filename, bands, size, bytetype, coords, pixsize

;check for the filename
hdrfile = filename+'.hdr'
if file_test(hdrfile) eq 0 then begin
    hdrfile = strmid(filename, 0, strlen(filename)-4)+'.hdr'
    if file_test(hdrfile) eq 0 then message, 'read_im_hdr:  file not found'
end


openr, un, hdrfile, /get_lun

query_file, un, 'BANDS', bands
bands= fix(bands)
query_file, un, 'ROWS', ysize
query_file, un, 'COLS', xsize
size = [fix(xsize), fix(ysize)]

query_file, un, 'DATATYPE', type
type = strtrim (type, 2)
case 1 of
(type eq 'U8'):   bytetype = 3
(type eq 'U16'):  bytetype = 5
(type eq 'S16'):  bytetype = 6
(type eq 'F32'):  bytetype = 9
(type eq 'U32'):  bytetype = 7
else:	begin
		print, 'Data type '+type+ ' not recognized by read_im_hdr.pro'
		print, 'Header read failed. Returning.
		return
		end

end

query_file, un, 'UL_X_COORDINATE', ulx

ulx = double(ulx)
query_file, un, 'UL_Y_COORDINATE', uly

uly = double(uly)
query_file, un, 'LR_X_COORDINATE', lrx
lrx = double(lrx)
query_file, un, 'LR_Y_COORDINATE', lry
lry = double(lry)

coords = [ [ulx,uly ], [lrx, lry]  ]


query_file, un, 'PIXEL_WIDTH', a
query_file, un, 'PIXEL_HEIGHT', b
pixsize = float([a,b])
free_lun, un

return
end




