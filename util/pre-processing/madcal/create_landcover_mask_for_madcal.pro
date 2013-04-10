pro create_landcover_mask_for_madcal, path, ppprrr, templatehdr

  ;find the landcover map
  lc_file = file_search(path+"VCT\prep\", "*landcover.bsq", count=n_lc_file)
  if n_lc_file ge 1 then begin
    zot_img, lc_file, lc_hdr, lc_img
    
    
    ;check for the existance of certain values and set them to 0 and all else to 1 for adding into the madcal mask
    ;0 = background - not processed
    ;11 = water - not processed
    ;12 = snow - not processed
    ;81 = Pasture/Hay - not processed
    ;82 = Cultivated Crops - not processed
    
    ;thebads = where(lc_img eq 0 or lc_img eq 11 or lc_img eq 12 or lc_img eq 81 or lc_img eq 82, complement=thegoods)
    thebads = where(lc_img eq 0 or lc_img eq 11 or lc_img eq 12, complement=thegoods)
    lc_img[thebads] = 0
    lc_img[thegoods] = 1

    lc_msk = strcompress(path+'madcal\'+ppprrr+'_lc_mask.bsq', /rem)
    openw, unit, lc_msk, /get_lun
    writeu, unit, lc_img
    free_lun, unit
    write_im_hdr, lc_msk, lc_hdr

    template_hdr = stringswap(lc_file, "bsq", "hdr")  
    convert_bsq_headers_to_envi, strcompress(path+'madcal\', /rem), templatehdr
      
  endif else message, "cannot find the landcover file in the *\VCT\prep\ folder"
end