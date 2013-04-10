pro package_stuff_from_gui, status

  ;find path from the passed run file
  scenedirpath = file_dirname(status.path, /mark_directory)
  
  ;find ppprrr from glovis tar.gz files
  glovisfiles = file_search(scenedirpath+"\glovis_targz\", "*.tar.gz", count=n_glovisfiles)
  if n_glovisfiles ge 1 then ppprrr =strmid(file_basename(glovisfiles[0]),3,6) $
  else message, ">>> !!!warning!!! cannot find glovis .tar.gz files"
  
  ;parse out the run file
  runstuff = parse_runfile(status.path)
  
  ;pull out the run alls
  if status.grp1runall eq 1 then begin
    status.progrp1boxa = 1
    status.progrp1boxb = 1
    status.progrp1boxc = 1
    status.progrp1boxd = 1
    status.progrp1boxe = 1
    status.progrp1boxf = 1
    status.progrp1boxg = 1
  endif
  
  if status.grp3runall eq 1 then begin
    status.progrp3boxa = 1
    status.progrp3boxb = 1
  endif
  
  if status.grp4runall eq 1 then begin
    status.progrp4boxa = 1
    status.progrp4boxb = 1
  endif
  
  if status.grp6runall eq 1 then begin
    status.progrp6boxa = 1
    status.progrp6boxb = 1
    status.progrp6boxc = 1
    status.progrp6boxd = 1
  endif
  
  runalls = 7
  
  ;is the run an update
  updatefiles = file_search(scenedirpath+"glovis_targz\update\", "*tar.gz", count=n_updatefiles)
  if n_updatefiles ge 1 then is_update = 1 else is_update = 0
  
  ;get the fix dates
  if status.fixthese eq '' or status.fixthese eq 'Enter Dates to Fix' then fixthese = 0 else begin
    fixsplit = strsplit(status.fixthese, ",", /extract)
    fixthese = fixsplit
  endelse
  
  ;get the delete dates
  if status.deletethese eq '' or status.deletethese eq 'Enter Dates to Delete' then deletethese = 0 else begin
    deletesplit = strsplit(status.deletethese, ",", /extract)
    deletethese = deletesplit
  endelse
  
  ;pack up the info in a strcuture for landtrendr_processor.pro
  run_params = {ppprrr:ppprrr,$
    path:scenedirpath,$
    vct_convert_glovis:status.progrp1boxa,$
    prep_dem:status.progrp1boxc,$
    input_dem:runstuff.input_dem,$
    prep_nlcd:status.progrp1boxd,$
    nlcdimg:runstuff.nlcdimg,$
    run_vct_for_masks:status.progrp1boxe,$
    lt_convert_glovis:status.progrp1boxf,$
    water_off:runstuff.water_off,$
    snow_off:runstuff.snow_off,$
    delete_files:runstuff.delete_files,$
    proj_ref_file:runstuff.proj_ref_file,$
    vct_to_lt_masks:status.progrp1boxg,$
    create_ref_image:status.progrp2boxa,$
    make_image_info:status.progrp7boxa,$
    run_madcal:status.progrp3boxb,$
    create_tc:status.progrp4boxa,$
    dark_object_vals:runstuff.dark_object_vals,$
    ls_madcal_ref_img:status.refimg,$
    fix_cloudmasks:status.progrp3boxa,$
    fix_hdrs:status.progrp7boxc,$
    create_ts_img_list:status.progrp4boxb,$
    print_img_info:status.progrp7boxe,$
    repop_img_info:status.progrp7boxd,$
    convert_hdrs:status.progrp7boxf,$
    fix_only_these:fixthese,$
    make_composites:status.progrp7boxb,$
    preprocessing_mode:runalls,$
    madcal_ref_img:runstuff.madcal_ref_img,$
    delete_lt_dates:status.progrp7boxg,$
    deletedate:deletethese,$
    norm_method:runstuff.norm_method,$
    target_day:runstuff.target_day,$
    target_year:runstuff.target_year,$
    modis_img_path:runstuff.modis_img_path,$
    is_update:is_update,$
    segmentation_eval:status.progrp5boxa,$
    segmentation:status.progrp6boxa,$
    label_segs:status.progrp6boxc,$
    filter_labels:status.progrp6boxd,$
    screen_images:status.progrp1boxb,$
    segparamstxt:runstuff.segparamstxt,$
    label_parameters_txt:runstuff.label_parameters_txt,$
    class_code_txt:runstuff.class_code_txt,$
    fit_to_vertices:status.progrp6boxb,$
    add_years_to_hdr:status.progrp7boxh}
    
  landtrendr_preprocessor, run_params
  
end