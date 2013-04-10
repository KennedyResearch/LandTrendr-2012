pro vct_clipmasktoa, max_n_years, series, headers, footers, imglist_name, path, ppprrr,$
    images, update=update, singles=singles, delete_files=delete_files
    
  drive = strmid(path, 0, 2) ;'E:'
  ppp = strmid(ppprrr, 0, 3) ;'046'
  rrr = strmid(ppprrr, 3, 3) ;'026
  pr = strcompress("p"+ppp+"r"+rrr ,/rem) ;'p046r026
  
  vct_tools_path = strcompress(path+"VCT\vctTools\", /rem)
  len_tools = strlen(vct_tools_path)-2
  vct_tools_path = strmid(vct_tools_path, 2, len_tools)
  
  if keyword_set(update) eq 1 then vct_output_path = strcompress(path+"VCT\outputs\update\", /rem) else $
    vct_output_path = strcompress(path+"VCT\outputs\", /rem)
  len_output = strlen(vct_output_path)-2
  vct_output_path_full = vct_output_path
  vct_output_path = strmid(vct_output_path, 2, len_output)
  
  ;get the extents of the DEM and NLCD
  dem = strcompress(path+"VCT\ancData\dem_"+pr, /rem)
  nlcd = strcompress(path+"VCT\ancData\lc_"+pr, /rem)
  zot_img, dem, demhdr, demimg, /hdronly
  zot_img, nlcd, nlcdhdr, nlcdimg, /hdronly
  ulx = max([demhdr.upperleftcenter[0],nlcdhdr.upperleftcenter[0]])
  uly = min([demhdr.upperleftcenter[1],nlcdhdr.upperleftcenter[1]])
  lrx = min([demhdr.lowerrightcenter[0],nlcdhdr.lowerrightcenter[0]])
  lry = max([demhdr.lowerrightcenter[1],nlcdhdr.lowerrightcenter[1]])
  
  
;  ;open the selected images .txt file
;  selectedimgsfile = file_search(path+"documentation\", "*images_selected_for_processing.txt*", count=n_selectedimgsfile)
;  if n_selectedimgsfile eq 1 then begin
;    ;read in the file
;    openr, lun, selectedimgsfile, /get_lun
;    file_size = file_lines(selectedimgsfile,/noexpand_path)
;    file_list = strarr(file_size)
;    readf, lun, file_list
;    free_lun, lun
;    goodslist = where(strmatch(file_list, "*,*") eq 1, n_goodslist)
;    
;    if n_goodslist ge 1 then begin
;      file_list = file_list[goodslist]
;      goods = strarr(n_goodslist)
;      for i=0, n_goodslist-1 do begin
;        split = strsplit(file_list[i], ',', /extract)
;        goods[i] = strcompress(split[0], /rem)
;      endfor      
;    endif
;  endif
  
  ;start processing the images
  if keyword_set(singles) eq 1 then max_n_years = n_elements(images)
  for i=0, max_n_years-1 do begin
    if keyword_set(singles) eq 1 then begin
      matchthis = strcompress("*"+images[i]+"*", /rem)
      match = file_search(vct_output_path_full, matchthis, count=n_match)
      ;ok = where(strmatch(goods, matchthis) eq 1, n_ok)
      if n_match eq 0 then begin
        print, ">>> skipping image: ", images[i], " because it was not selected for processing"
        continue 
      endif
      runthisone = images[i]
      print, ">>> processing image: ", runthisone, "  ", strcompress(string(i+1)+"/"+string(max_n_years), /rem)," ..."
      extent_problem = 0
      
      ;get the extent of the image
      image = strcompress(vct_output_path_full+runthisone, /rem)
      
      zot_img, image, hdr, img, /hdronly
      if hdr.upperleftcenter[0] lt ulx then extent_problem = extent_problem+1
      if hdr.upperleftcenter[1] gt uly then extent_problem = extent_problem+1
      if hdr.lowerrightcenter[0] gt lrx then extent_problem = extent_problem+1
      if hdr.lowerrightcenter[1] lt lry then extent_problem = extent_problem+1
      if extent_problem gt 0 then begin
        print, ">>>   skipping: ", runthisone, " because the extent is outside of the ancillary data"
        goto, start_here
      endif
    endif else begin  ;if keyword_set(singles) eq 1 then begin
      runthisone = images[where(series eq (i+1))]
      print, ">>> processing image list: ", strcompress(string(i+1)+"/"+string(max_n_years), /rem)," ..."
    endelse
    vct_img_list_new = [headers, runthisone, footers]
    vct_img_list_new = transpose(vct_img_list_new)
    out = strcompress(imglist_name+string(i+1)+".txt", /rem)
    openw, lun, out, /get_lun
    printf, lun, vct_img_list_new
    free_lun, lun
    cmd = drive+ " && " + strcompress(vct_tools_path + "clipMaskToa.exe", /rem) $
      + " " + strcompress(vct_output_path + pr + "_imagelist"+string(i+1)+".txt", /rem)
    
    if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
    spawn, cmd, /log_output, /hide
    
    bat_list = strcompress(vct_output_path_full+pr+"_calAnaUd.bat" , /rem) ;"F:\4626\VCT\outputs\p046r026_calAnaUd.bat"
    if file_test(bat_list) eq 0 then begin
      print, ">>> clipMaskToa did not run properly"
      print, ">>> ending program"
      stop
      ;continue
    endif
    vct_caludist, path, ppprrr, update=update
    
    mask_file_old = strcompress(drive + vct_output_path + pr + "_commMask", /rem)
    mask_file_hdr_old = strcompress(drive + vct_output_path + pr + "_commMask.hdr", /rem)
    mask_file_new = strcompress(mask_file_old + string(i+1), /rem) ; + ".bsq"
    mask_file_hdr_new = strcompress(file_dirname(mask_file_hdr_old) + "\" + file_basename(mask_file_hdr_old, ".hdr") + string(i+1) + ".hdr", /rem)
    bat_file_old = strcompress(drive + vct_output_path + pr + "_calAnaUd.bat", /rem)
    bat_file_new = strcompress(drive + vct_output_path + pr + "_calAnaUd" + string(i+1) + ".bat", /rem)
    file_move, bat_file_old, bat_file_new, /overwrite
    file_move, mask_file_old, mask_file_new, /overwrite
    file_move, mask_file_hdr_old, mask_file_hdr_new, /overwrite
    
    ;---if requested, delete the unused vct output files--
    if keyword_set(delete_files) eq 1 then begin
      a = ""
      files = ['*toa','*toa.hdr','*distbMagn','*distbMap','*mask_NoConfClass',$
        '*NoConfClass.hdr','*projShadow','*toaBr','*toaBr.hdr','*udist','*udist.hdr','*udist_B2','*udist_B2.hdr',$
        '*udist_B3','*udist_B3.hdr','*udist_B4','*udist_B4.hdr','*udist_B5','*udist_B5.hdr',$
        '*udist_B6','*udist_B6.hdr','*udist_B7','*udist_B7.hdr','*udist_dnbr','*udist_dnbr.hdr',$
        '*udist_ndvi','*udist_ndvi.hdr'] ;'*MTL.txt','*L5','*L5.hdr','*L71','*L71.hdr'
      for j=0, n_elements(files)-1 do begin
        b = file_search(vct_output_path_full, files[j], count=count)
        if count ge 1 then a = [a,b]
      endfor
      if n_elements(a) gt 1 then begin
        a = a[1:(n_elements(a)-1)]
        file_delete, a, /quiet
      endif
    endif
    start_here:
  endfor
  

end