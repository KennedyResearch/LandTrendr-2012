pro vct_caludist, path, ppprrr, update=update  
    
  drive = strmid(path, 0, 2)
  ppp = strmid(ppprrr, 0, 3) ;'046'
  rrr = strmid(ppprrr, 3, 3) ;'026
  pr = strcompress("p"+ppp+"r"+rrr ,/rem) ;'p046r026
  
  vct_tools_path = strcompress(path+"VCT\vctTools\", /rem) 
  len_tools = strlen(vct_tools_path)-2
  vct_tools_path = strmid(vct_tools_path, 2, len_tools)
  
  if keyword_set(update) eq 1 then bat_list = strcompress(path+"VCT\outputs\update\"+pr+"_calAnaUd.bat" , /rem) else $ ;"F:\4626\VCT\outputs\p046r026_calAnaUd.bat"
    bat_list = strcompress(path+"VCT\outputs\"+pr+"_calAnaUd.bat" , /rem)
  
  openr, lun, bat_list, /get_lun
  file_size = file_lines(bat_list,/noexpand_path)
  file_list = strarr(file_size)
  readf, lun, file_list
  free_lun, lun
  
  lastline = file_size-3
  file_list = file_list[0:lastline]
;  file_list
  
  for i=0, n_elements(file_list)-1 do begin
    length = strlen(file_list[i])-26
    inputs = strmid(file_list[i], 27, length)
    cmd = drive+ " && " + strcompress(vct_tools_path + "calUdist.exe", /rem) + " " + inputs  
    if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
    spawn, cmd, /hide ;, /nowait
    ;repeat found=file_search(path+"VCT\outputs\", "*udist_dnbr*",count=n_found) until n_found ne 0
    ;spawn, "tskill calUdist"
    ;files = file_search(path+"VCT\outputs\", "*udist*",count=n_files)
  endfor
end  

  