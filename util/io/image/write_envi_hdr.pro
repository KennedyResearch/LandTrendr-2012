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

function write_envi_hdr, image_file_name, imagine_format_header, $
		addon = addon, $
			swan_albers=swan_albers, swan_utm_zone=swan_utm_zone

;Given a structure of the imagine-format header, convert to
;  envi.  Taken from "convert_bsq_to_envi.pro", only here
;  I cut out the need to convert the actual file or to
;   read in the header.

;right now, hardwired to SWAN.  will only write out
;  projection information if swan_albers or swan_utm_zone
;  are set.


ENVI, /RESTORE_BASE_SAVE_FILES
ENVI_BATCH_INIT, LOG_FILE = 'batch.log'


if n_elements(addon) eq 0 then addon = '_envi'

outfile = image_file_name
hdr = imagine_format_header



;ALL OF THIS FROM CONVERT FUNCTION
;filename = get_filename(img_file)
;pathname = get_pathname(img_file)
;
;outfile = pathname+strcompress(strmid(filename, 0, strlen(filename)-4)+addon+'.img', /remove_all)
;
;openw, un, outfile, /get_lun


;get the basic header information from the erdas file

;
;  zot_img, img_file, hdr, img, /hdronly


;;read each band and write it out
;
;;if this is a true imagine image, read in and write out bsq
;
;if strmid(filename, strlen(filename)-3, strlen(filename)) eq 'img' then begin
; for i = 0, hdr.n_layers-1 do begin
;    zot_img, img_file, hdr, img, layers = [i+1]
;    writeu, un, img
;    img = 0
;
; end
;free_lun, un
;end
;END OF CONVERT FUNCTION OLD STUFF, JUST FOR THE RECORD



;now set up the various pieces

;data type

  ;in Imagine:
  		;0="u1"
			;1="u2"
			;2="u4"
			;3="u8"
			;4="s8"
			;5="u16"
			;6="s16"
			;7="u32"
			;8= "s32"
			;9="f32"
			;10="f64"
			;11="c64"
			;12="c128"

  ;in envi:

    imagine_types = [3,4, 5, 6, 9]
    envi_types =    [1,2, 12, 2, 4]

				;note:  I convert types 1 through 3 into unsigned 8 bit
				;      and signed 8-bit (type 4) into integer

    pp = where(imagine_types eq hdr.pixeltype, many)
    if many eq 0 then return, {ok:0, message:'Data type of input image not supported.  Harrass Kennedy.'}
    data_type = envi_types[pp]


  ;interleave

    interleave = 0		;bsq


  ;nb, nl, and ns

    nb = hdr.n_layers
    nl = hdr.filesize[1]
    ns = hdr.filesize[0]

  ;offset

   offset = 0		;start at beginning of file


  ;anchor in geographic space

   mc = [0d, 0, hdr.upperleftcenter[0]-(hdr.pixelsize[0]*.5), $
   							hdr.upperleftcenter[1]+(hdr.pixelsize[0]*.5)]     ;changed on may 5, 2006



;ALL OF THIS WILL NEED TO CHANGE WHEN GENERICIZED
;are we doing map information because at swan?

;did the user say this is albers?

if n_elements(swan_albers) ne 0 then begin
    datum = 'North America 1983'

		;albers:  projection number 9
		;params
		; a, b, lat0, lon0, x0, y0, sp1, sp2, [datum], name
;		  a - the equatorial radius (semi-major axis)
;  b - the polar radius (semi-minor axis)
;  lat0 - Latitude of origin of projection
;  lon0 - Longitude of central meridian
;  x0 - False easting
;  y0 - False Northing
;

     lat0 = 50.0		;latitude of origin of projection
     lon0 = -154.0  ;longitude of origin of projection
     x0 = (y0=0.)	;false easting and northing
     sp1 = 55
     sp2 = 65
     a = 6378137.0	;taken for GRS80 in ellipse.txt in C:\RSI\IDL62\products\ENVI42\map_proj
     b = 6356752.3  ;  same


    params = [a, b, lat0, lon0, x0, y0, sp1, sp2]
    projinfo = envi_proj_create($
    				datum = datum, $
    				params = params, $
    				type = 9)		;albers conical equal area

    units = envi_translate_projection_units('Meters')

    map_info = envi_map_info_create(proj=projinfo, $
    				ps = hdr.pixelsize, mc = mc, units=units)



end else if n_elements(swan_utm_zone) ne 0 then begin

   ;DOING THE UTM IN NAD83
   ;set up utm coords
    datum = 'North America 1983'

;     lat0 = 		;latitude of origin of projection
;     lon0 =  ;longitude of origin of projection
;     x0 = (y0=0.)	;false easting and northing
;
;     a = 6378137.0	;taken for GRS80 in ellipse.txt in C:\RSI\IDL62\products\ENVI42\map_proj
;     b = 6356752.3  ;  same

;
;    params = [a, b, lat0, lon0, x0, y0, sp1, sp2]
;    projinfo = envi_proj_create($
;    				datum = datum, $
;    				params = params, $
;    				type = 9)		;

    units = envi_translate_projection_units('Meters')

    map_info = envi_map_info_create(/utm, $
    				zone=swan_utm_zone, datum = datum, $
    				ps = hdr.pixelsize, mc=mc, units=units)


end else begin $		;just set up generic
		map_info = envi_map_info_create(/arbitrary, $
		  /map_base, ps = hdr.pixelsize, $
		  mc = mc)


end

 envi_setup_head, fname=outfile, ns=ns, nl=nl, $
				nb=nb, interleave=interleave, offset=offset, $
				data_type=data_type, map_info=map_info, $
				/write



return, {ok:1, outfile:outfile}

end
