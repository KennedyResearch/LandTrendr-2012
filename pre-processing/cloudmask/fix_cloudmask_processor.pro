pro fix_cloudmask_processor, params

  ppprrr=params.ppprrr
  path=params.path
  cldmsk_fix_method=params.cldmsk_fix_method
  cldmsk_ref_dates=params.cldmsk_ref_dates
  fix_these_dates=params.fix_these_dates
  
  image_info_savefile = path+'images\landtrendr_image_info_'+ppprrr+'.sav'
  
  if cldmsk_fix_method eq 2 then begin
    cloud_shadow_mask_reference_image_maker, path, cldmsk_ref_dates, unnormalized=unnormalized, normalized=normalized, /ledaps
    image_difference_maker, path, fix_these_dates, normalized=normalized, unnormalized=unnormalized, /ledaps
  endif
  
  create_image_info, path, image_info_savefile
  repopulate_image_info,image_info_savefile, /ignore_norms
  fix_cloud_masks3, image_info_savefile, cldmsk_fix_method, fixmask=fix_these_dates, from_madcal=from_madcal

  convert_bsq_headers_to_envi, path, params.template_header  
end
