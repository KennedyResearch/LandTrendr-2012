

pro vct_img_list_splitter_junk, scene, drive, vct_tools_path, glovis_img_path, vct_output_path, dem_path, landcover_path
  scene = '2528'
  drive = 'E:\'
  vct_tools_path = "E:\2528\VCT\vctTools\"
  glovis_img_path = "E:\2528\VCT\glovis"
  vct_output_path = "E:\2528\VCT\outputs\"
  dem_path = "E:\2528\VCT\ancData\dem_p25r28"
  landcover_path = "E:\2528\VCT\ancData\lc_p25r28"
  
  
  
  
  ;---get the length of the input strings---
  len_tools = strlen(vct_tools_path)-2
  len_glovis = strlen(glovis_img_path)-2
  len_output = strlen(vct_output_path)-2
  
  ;---pull out pieces of the input strings---
  drive = strmid(drive, 0, 2) ;example = f:
  vct_tools_path = strmid(vct_tools_path, 2, len_tools)
  glovis_img_path = strmid(glovis_img_path, 2, len_glovis)
  vct_output_path = strmid(vct_output_path, 2, len_output)
  
  ;---create command to run convertGeoTiff2Envi.py in dos---
  cmd = drive+ " && " + strcompress(vct_tools_path + "convertGeoTiff2Envi.py", /rem) $
    + " " + glovis_img_path + " " + vct_output_path
  
;  spawn, cmd
  ;---find the vct image list---
  ppp = strcompress("p" + string(0) + strmid(scene, 0, 2), /rem)
  rrr = strcompress("r" + string(0) + strmid(scene, 2, 2), /rem)
  vct_img_list = strcompress(drive + vct_output_path + ppp + rrr + "_imagelist.txt")
  
;  vct_img_list = "F:\4626\VCT\junk\p046r026_imagelist.txt"
  
  ;---read in the image list---
  openr, lun, vct_img_list, /get_lun
  file_size = file_lines(vct_img_list,/noexpand_path)
  file_list = strarr(file_size)
  readf, lun, file_list
  free_lun, lun
  
  ;---pull out the non-image stuff
  header1 = file_list[0]
  header2 = file_list[1]
  headers = [header1, header2]
  footer1 = file_list[((file_size-1)-3)]
  footer2 = file_list[((file_size-1)-2)]
  footers = [footer1, footer2, dem_path, landcover_path]
  
  images = "dummy" ; get a file holder started
  for i=0, n_elements(file_list)-1 do begin
    if strmid(file_list[i],0,1) eq '0' or strmid(file_list[i],0,1) eq '1' then images = [images, file_list[i]]
  endfor

;---find the number of dates per year---
  images = images[1:*] ; get rid of the dummy element
  years = strmid(images, 10, 4)
;    
;  uniq_years = years[uniq(years, sort(years))]
    
  vct_img_info = create_struct("file", " ", "year", 1984, "n_year", 1, "series", 1)
  vct_img_info = replicate(vct_img_info, n_elements(images))

  for i=0, n_elements(images)-1 do begin
    vct_img_info[i].file = images[i]
    vct_img_info[i].year = strmid(images[i], 10, 4)
    not_used = where(years eq vct_img_info[i].year, count)    
    vct_img_info[i].n_year = count
  endfor

  for i=0, n_elements(images)-1 do begin
    for j=0, n_elements(where(vct_img_info.year eq vct_img_info[i].year))-1 do begin
      vct_img_info[i].series = j+1
      i = i+1 ;get the i right for the going through the j section
    endfor
    i = i-1 ;fix the i from the j section
  endfor  

  series = vct_img_info.series

  max_n_years = max(vct_img_info.n_year)
  
  out_name = file_basename(vct_img_list, ".txt")
  out_dir = file_dirname(vct_img_list)
  
  if max_n_years ge 1 then begin
    goods = images[where(series eq 1)] 
    vct_img_list_new = [headers, goods, footers]  
    vct_img_list_new = transpose(vct_img_list_new)
    out = strcompress(out_dir+"\"+out_name+string(1)+".txt", /rem)
    openw, lun, out, /get_lun
    printf, lun, vct_img_list_new
    free_lun, lun
    out_no_drive = vct_output_path
    cmd = drive+ " && " + strcompress(vct_tools_path + "clipMaskToa.exe", /rem) $
      + " " + strcompress(vct_output_path + ppp + rrr + "_imagelist1.txt", /rem) 
    spawn, cmd
    mask_file_old = strcompress(drive + vct_output_path + ppp + rrr + "_commMask", /rem)
    mask_file_hdr_old = strcompress(drive + vct_output_path + ppp + rrr + "_commMask.hdr", /rem)
    mask_file_new = strcompress(mask_file_old + string(1), /rem)
    mask_file_hdr_new = strcompress(file_dirname(mask_file_hdr_old) + "\" + file_basename(mask_file_hdr_old, ".hdr") + string(1) + ".hdr", /rem)
    file_move, mask_file_old, mask_file_new 
    file_move, mask_file_hdr_old, mask_file_hdr_new 
  endif

  if max_n_years ge 2 then begin
    goods = images[where(series eq 2)] 
    vct_img_list_new = [headers, goods, footers]  
    vct_img_list_new = transpose(vct_img_list_new)
    out = strcompress(out_dir+"\"+out_name+string(2)+".txt", /rem)
    openw, lun, out, /get_lun
    printf, lun, vct_img_list_new
    free_lun, lun
    out_no_drive = strcompress(vct_output_path+"\"+out_name+string(2)+".txt", /rem)
    cmd = drive+ " && " + strcompress(vct_tools_path + "clipMaskToa.exe", /rem) $
      + " " + out_no_drive
    spawn, cmd
  endif
  
  if max_n_years ge 3 then begin
    goods = images[where(series eq 3)] 
    vct_img_list_new = [headers, goods, footers]  
    vct_img_list_new = transpose(vct_img_list_new)
    out = strcompress(out_dir+"\"+out_name+string(3)+".txt", /rem)
    openw, lun, out, /get_lun
    printf, lun, vct_img_list_new
    free_lun, lun
    out_no_drive = strcompress(vct_output_path+"\"+out_name+string(3)+".txt", /rem)
    cmd = drive+ " && " + strcompress(vct_tools_path + "clipMaskToa.exe", /rem) $
      + " " + out_no_drive
    spawn, cmd
  endif

  if max_n_years ge 4 then begin
    goods = images[where(series eq 4)] 
    vct_img_list_new = [headers, goods, footers]  
    vct_img_list_new = transpose(vct_img_list_new)
    out = strcompress(out_dir+"\"+out_name+string(4)+".txt", /rem)
    openw, lun, out, /get_lun
    printf, lun, vct_img_list_new
    free_lun, lun
    out_no_drive = strcompress(vct_output_path+"\"+out_name+string(4)+".txt", /rem)
    cmd = drive+ " && " + strcompress(vct_tools_path + "clipMaskToa.exe", /rem) $
      + " " + out_no_drive
    spawn, cmd
  endif

  if max_n_years ge 5 then begin
    goods = images[where(series eq 5)] 
    vct_img_list_new = [headers, goods, footers]  
    vct_img_list_new = transpose(vct_img_list_new)
    out = strcompress(out_dir+"\"+out_name+string(5)+".txt", /rem)
    openw, lun, out, /get_lun
    printf, lun, vct_img_list_new
    free_lun, lun
    out_no_drive = strcompress(vct_output_path+"\"+out_name+string(5)+".txt", /rem)
    cmd = drive+ " && " + strcompress(vct_tools_path + "clipMaskToa.exe", /rem) $
      + " " + out_no_drive
    spawn, cmd  
  endif
  
  if max_n_years ge 6 then begin
    goods = images[where(series eq 6)] 
    vct_img_list_new = [headers, goods, footers]  
    vct_img_list_new = transpose(vct_img_list_new)
    out = strcompress(out_dir+"\"+out_name+string(6)+".txt", /rem)
    openw, lun, out, /get_lun
    printf, lun, vct_img_list_new
    free_lun, lun
    out_no_drive = strcompress(vct_output_path+"\"+out_name+string(6)+".txt", /rem)
    cmd = drive+ " && " + strcompress(vct_tools_path + "clipMaskToa.exe", /rem) $
      + " " + out_no_drive
    spawn, cmd  
  endif

;  out = "f:\4626\VCT\test_junk.csv
;  export_structure_to_file,vct_img_info,out  
    
print, "done"   
     

end






