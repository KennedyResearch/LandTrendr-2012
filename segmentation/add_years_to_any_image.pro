;ADD_YEARS_TO_ANY_IMAGE
;  A generic version that can be applied to any image stack. 
;  give it the envi bsq for the file in question, and a 
;  vector of the years.  It will check the match
;  between the layer count and the number of years, and then
;  add to the envi header the years as wavelengths

;****************************************************************************

function add_years_to_any_image, envi_bsq_file, years


    this_image = envi_bsq_file
    
;first check that years match layercount
  zot_img, this_image, hdr, img, /hdronly
  layercount = hdr.n_layers
  n_yrs = n_elements(years)
  if n_yrs ne layercount then return, {ok:0, msg:'Layercount is '+string(layercount)+' but years vector has '+string(n_yrs)+' elements for '+envi_bsq_file}

; then add the years to the header     

       fitted_hdr = stringswap(this_image, 'bsq', 'hdr')
       openu, un, fitted_hdr, /get_lun
        a=''
        readf, un, a
        if strupcase(strcompress(a, /rem)) eq 'ENVI' then begin
        
          ;check to see if wavelength is already in there^M
          done = 0
          
          while not (eof(un)) do begin
            readf, un, a
            if strmid(a, 0,  12) eq 'wavelength =' then done=1 
          endwhile
   
          ;if we haven't done it already, add the wavelengths^M
         yrs =years
 if done ne 1 then begin
            outstring = 'wavelength = { '
            outstring = outstring+string(yrs[0])
            
            for i = 1, n_elements(yrs)-1 do outstring= outstring + ', '+string(yrs[i])
            outstring=outstring+'}'
         ;write it^M
            fi = file_info(fitted_hdr)
            point_lun, un, fi.size
            printf, un, outstring
          end
        free_lun, un
	end else begin
		free_lun, un
		return, {ok:0, msg:'File is not an ENVI format file: '+envi_bsq_file}
	end

     return, {ok:1, msg:'Add years successful'}

end















      
















        

















  


