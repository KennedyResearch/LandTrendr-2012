pro vct_convertgeotiff2envi, path, ppprrr, update=update
  
  print, ">>> converting glovis .tar.gz tifs to envi format stacks for use in VCT"
  print, ">>> please wait..."
  print, ""
  ;---set up some path variables from the given info---
  ;path stuff
  drive = strmid(path, 0, 2) ;'E:'
  ppp = strmid(ppprrr, 0, 3) ;'046'
  rrr = strmid(ppprrr, 3, 3) ;'026
  pr = strcompress("p"+ppp+"r"+rrr ,/rem) ;'p046r026
  vct_tools_path = strcompress(path+"VCT\vctTools\", /rem) 
  if keyword_set(update) eq 1 then begin
    file_mkdir, strcompress(path+"VCT\outputs\update\", /rem)
    glovis_img_path = strcompress(path+"glovis_targz\update\", /rem) 
    vct_output_path = strcompress(path+"VCT\outputs\update\", /rem)
  endif else begin
    glovis_img_path = strcompress(path+"glovis_targz\", /rem)
    vct_output_path = strcompress(path+"VCT\outputs\", /rem)
  endelse
  vct_ancdata_path = strcompress(path+"VCT\ancData\", /rem)
  
  ;check that targz files are available
  search = strcompress("*"+ppprrr+"*.tar.gz", /rem)
  aretargzs = file_search(glovis_img_path, search ,count=n_aretargzs) 
  if n_aretargzs eq 0 then begin
    print, ""
    print, ">>> warning!!! apparently there are no tar.gz files...
    print, ">>> from glovis that are associated with this scene...
    print, ">>> either: (1) the given path variable is incorrect...
    print, ">>>    path: ", path
    print, ">>> (2) the given ppprrr variable is incorrect...
    print, ">>>    ppprrr: ", ppprrr
    print, ">>> (3) the glovis tar.gz files were never copied into...
    print, ">>> the scene's 'glovis_targz' folder ex. 'J:\043033\glovis_targz\'"
    print, ">>>"
    print, ">>> ending program"
    print, ""
    stop   
  endif
  
  ;---mess with the created variables to get inputs for convertGeoTiff2Envi.py---
  ;get the length of the input strings minus the drive letter and colon 
  len_tools = strlen(vct_tools_path)-2
  len_glovis = strlen(glovis_img_path)-2
  len_output = strlen(vct_output_path)-2
  
  ;get rid of the drive letter and colon from the input strings 
  vct_tools_path = strmid(vct_tools_path, 2, len_tools)
  glovis_img_path = strmid(glovis_img_path, 2, len_glovis)
  vct_output_path = strmid(vct_output_path, 2, len_output)
  
  ;---create command to run convertGeoTiff2Envi.py in dos---
  cmd = drive+ " && " + strcompress(vct_tools_path + "convertGeoTiff2Envi.py", /rem) $
    + " " + glovis_img_path + " " + vct_output_path
   
  
  ;make sure the paths are ok for command prompt
  if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
  
  ;run the command
  spawn, cmd, /hide
end