
pro make_ts_image_lists, image_info_savefile, path, ppprrr

  restore, image_info_savefile
  
  refl_out_name = strcompress(path+"timesync\"+ppprrr+"_refl_list.txt", /rem)
  tc_out_name = strcompress(path+"timesync\"+ppprrr+"_tc_list.txt", /rem)
  
  ;---pull out image information---
  year = strcompress(strtrim(image_info.year),/remove_all)
  day = strcompress(strtrim(image_info.julday),/remove_all)
  refl = strcompress(strtrim(image_info.image_file),/remove_all)
  tc = strcompress(strtrim(image_info.tc_file),/remove_all)
  header = ['PLATFORM,YEAR,JULDAY,LABEL,FILE']
  
  ts_refl_list = [header, 'TM'+','+year+','+day+','+year+'_'+day+','+refl]
  
  ts_tc_list = [header, 'TM'+','+year+','+day+','+year+'_'+day+','+tc]
  
  ;---make image listss---
  print, 'Reflectance list:'
  print, ts_refl_list
  openw, lun,refl_out_name,/get_lun
  printf, lun,ts_refl_list
  free_lun, lun
  
  print, 'Tasselled Cap list:'
  print, ts_tc_list
  openw, lun,tc_out_name,/get_lun
  printf, lun,ts_tc_list
  free_lun, lun
  
  Print, 'Done printing image lists'
end
