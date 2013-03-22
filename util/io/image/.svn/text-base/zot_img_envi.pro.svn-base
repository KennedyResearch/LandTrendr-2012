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

pro zot_img_envi, file, output_header, output_image, layers=layers, $
		subset=subset, hdronly=hdronly, ignore=ignore, corner = corner


;need to get the header information

  read_envi_hdr, file, bands, fsize, bytetype, coords, pixsize, $
  			datum=datum, zone=zone, proj=proj

  if n_elements(zone) eq 0 then zone = -1
  if n_elements(datum) eq 0 then datum = 'unk'
  if n_elements(zone) eq 0 then zone = 'unk'
  if n_elements(proj) eq 0 then proj = 'unk'


;is this one arranged as a map or simply file coords?

  confirm = (coords(0,1)-coords(0,0)) gt 0  ;check that xlor gt xupl
  map =     confirm  eq ((coords(1,0)-coords(1,1)) gt 0)
  			;map is 1 if we have map coords, 0 if file coords
  adj = [1, -((map*2)-1)]	;make adjustment [1,-1] in case of map


;figure out the subset information

  if n_elements(subset) eq 0 then $
  	subs = [ [coords(*,0)], [coords(*,1)] ] else $
  	subs = double(subset)

  ;check to make sure the coords are an integer number of pixels from the
  ;		upper left corner

  if n_elements(corner) ne 0 then $
  	subs = [ [edgit(subs(*,0), pixsize, map = map, /tocenter)], $
  	     [edgit(subs(*,1), pixsize, map = map, /lowerright, /tocenter)] ]


  subs = [[adj_int_mult(coords(*,0), pixsize, subs(*,0), map = map)], $
  	    [adj_int_mult(coords(*,0), pixsize, subs(*,1), map = map)] ]


  ;now make sure that the we haven't overstepped the bounds of the image
       bounds = coords
   		 subt = fix_edge(subs, bounds, /map)
			 subt.coords = cutafter(subt.coords, 4)	;remove noise after fourth dec. place
			 if subt.valid eq 0 then begin
			 			print, 'zot_img_envi:  error -- coordinates outside of image'
			 			print, 'failing read'


			 			goto, past
			 			end


			 subs= subt.coords

  subset = subs		;added july 2009 so that the subset keyword will reflect the change


;now make file coords


  sub_coords = round([ [ (subs(*,0)-coords(*,0))/(pixsize*adj) ], $	;in file coords
    		 [ (subs(*,1)-coords(*,0))/(pixsize*adj) ] ])
  sub_size = round(sub_coords(*,1)-sub_coords(*,0)) + [1,1]




;layer info


  if n_elements(layers) eq 0 then layers = indgen(bands)+1
  l_count = n_elements(layers)


;if just doing header, then get outta here

 if n_elements(hdronly) ne 0 then goto, past



;how far into the file are we looking?  We know the info is band-seq.
  openr, un, file, /get_lun
  offset_pixels = ulong64(((sub_coords(1,0))*long(fsize(0))) + sub_coords(0,0))

  case bytetype of
  (3):	begin
  	  mult = 1
  	  image = bytarr(sub_size(0), sub_size(1), l_count)
  	  line = bytarr(sub_size(0))
  	end

  
  
  
  (5):  begin       ;jdb added 8/11/11
      mult = 2
      image = intarr(sub_size(0), sub_size(1), l_count)
      line = intarr(sub_size(0))  
    end
  
  
  (6):	begin				;changed from 5 to 6 on 1/3/06
  	  mult = 2
  	  image = intarr(sub_size(0), sub_size(1), l_count)
  	  line = intarr(sub_size(0))
  	end
  (9):	begin
          mult = 4
  	  image = fltarr(sub_size(0), sub_size(1), l_count)
  	  line = fltarr(sub_size(0))
  	end
  (7):	begin
          mult = 4
  	  image = ulonarr(sub_size(0), sub_size(1), l_count)
  	  line = ulonarr(sub_size(0))
  	end
  else: begin
  	  print, 'zot_img_envi does not recognize a file type of '+string(bytetype)
  	  print, 'Read failed.  Returning.'
  	  free_lun, un
  	  return
  	end
 endcase



;read in the image, one line at a time, with the skip distance between
layersize = long(fsize(0))*fsize(1)*mult
for l = 0, n_elements(layers)-1 do begin

  if layers(l) eq 0 then begin
  		print, 'Zot_img_bsq does not recognize layer 0'
  		print, 'The first layer of an image is defined as layer 1'
  		print, 'Please fix this call to zot_img_bsq.pro'
  		return
  		end


  layeroffset = (ulong(layersize)*(layers(l)-1))


  for i = 0l, sub_size(1)-1 do begin
    point_lun, un, (offset_pixels*mult)+(ulong(fsize(0))*mult*i)+layeroffset
    readu, un, line
    image(*,i, l)=line
  end
end




free_lun, un
output_image = image




;Set up header info
past:
  output_header = {filesize:sub_size, pixelType:bytetype, $
  	upperLeftCenter:subs(*,0), lowerRightCenter:subs(*,1), $
  	pixelSize:pixsize, n_layers:bands, datum:datum, $
  	proj:proj, zone:zone}



return
end




