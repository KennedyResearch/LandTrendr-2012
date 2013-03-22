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




pro read_envi_hdr, filename, bands, size, bytetype, coords, pixsize, $
    datum=datum, zone=zone, proj=proj

  potential_header = [filename+'.hdr', strmid(filename, 0, strlen(filename)-4)+'.hdr']
  checker = [file_exists(potential_header[0]), file_exists(potential_header[1])]
  headerfile = potential_header[where(checker eq 1, many)]
  if many ne 1 then headerfile = headerfile[0]	;just take the zeroth one



  openr, un, headerfile, /get_lun

  query_file2, un, 'samples', xsize, separator = '=', /norestart
  query_file2, un, 'lines', ysize, separator = '=', /norestart
  size = [fix(xsize,type=3), fix(ysize,type=3)]


  query_file2, un, 'bands', bands, separator = '=', /norestart
  bands= fix(bands)



  query_file2, un, 'data type', type, separator = '=', /norestart


  ;need to put the types into the same structure as imagine

  type = fix(strtrim (type, 2))
  case type of
    (1):  bytetype = 3    ;u8
    (2): bytetype = 6			;s16     changed from 5 to 6 on 1/3/06
    (3): bytetype = 8     ;s32
    (4):  bytetype = 9    ;f32
    (12): bytetype = 5    ;u16
    (13):  bytetype = 7		;u32 (ulong)
    else:  begin
      print, 'read_envi_hdr does not recognize byte type of '+string(type)
      print, 'Read failed. Returning.'
      return
    end
  endcase




  ;now do the map info, which is in a structure output style
  ; In envi, the coordinates refer to the upper left of the pixel
  ;  so to be consistent in zot, I need keep track of the xstart
  ;  and ystart values.  If 1.0, then I need to shift things
  ;  1/2 pixel to be in the center.  If 1.5, then the values
  ;  are set for the center of pixel.


  query_file2, un, 'map info', mapinfo, separator='=', /norestart

  if mapinfo ne 'no_match' then begin

    pieces = strsplit(mapinfo, ',', /extract)

    startx = 1.5-double(pieces[1])
    starty= 1.5-double(pieces[2])


    pixsize = [float(pieces[5]), float(pieces[6])]
    ulx = double(pieces[3])+(startx*pixsize[0])
    uly = double(pieces[4])-(starty*pixsize[1])



    lrx = ulx + (xsize-1l)*pixsize[0]
    lry = uly - (ysize-1l)*pixsize[1]		;assumes map orientation, and that pixel values
    ;correspond to the center of the pixels.

    coords = [ [ulx,uly ], [lrx, lry]  ]

    if n_elements(pieces) eq 7 then goto, start_here
    
    ;get the zone and datum info

    proj = strmid(pieces[0], 2, strlen(pieces[0]))

    checkalbers = stregex(strupcase(proj), '.ALBERS.')
    if checkalbers ne -1 then $
      proj = strmid(proj, checkalbers+1, strlen(proj))


    proj = strtrim(proj, 2)
    case proj of
      ('UTM'):   begin
        zone = fix(pieces[7])
        datum = pieces[9]
        north = strcompress(pieces[8],/rem) eq 'North'
      end

      ('{UTM'):   begin
        zone = fix(pieces[7])
        datum = pieces[9]
        north = strcompress(pieces[8],/rem) eq 'North'
      end

      ('Albers Conical Equal Area'): begin
        datum = pieces[7]
      end
	('albers_con_eq_area'): begin
        datum = pieces[7]
      end
	('USGS Landsat Albers'): datum = pieces[7]
	('albers_landsat'): datum = pieces[7]



      else:   stop

    endcase


  end	else begin		;if no map info, assume just a file-orientation coordinate system
    startx = 1
    starty = 1
    pixsize = [1.0, 1.0]
    coords = [ [1.,1.], [xsize, ysize]]		;because ref'd to 1, xsize and ysize unaltered
  end

  start_here:
  
  free_lun, un

  return
end




