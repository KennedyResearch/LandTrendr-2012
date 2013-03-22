pro vct_dem_downloader, ppp, rrr, out_path

  ;ftp://ftp.glcf.umd.edu/glcf/SRTM/WRS2_Tiles/p046/SRTM_ffB01_p046r026/SRTM_ffB01_p046r026.tif.gz    ;<example - case is important!!!

  ffb01_filedir = strcompress("SRTM_ffB01_p"+ppp+"r"+rrr, /rem)
  ffb03_filedir = strcompress("SRTM_ffB03_p"+ppp+"r"+rrr, /rem)
  url_path = strcompress("ftp://ftp.glcf.umd.edu/glcf/SRTM/WRS2_Tiles/p"+ppp+"/", /rem)
  
  obj = obj_new('IDLnetUrl')
  result = obj->GetFtpDirList(url=url_path, /short)
   
  hits = where(result eq ffb01_filedir, n_hits)
  if n_hits eq 0 then begin
    print, ">>>   SRTM_ffB01 DEM does not exist for scene: ", ppp+rrr, " - checking SRTM_ffB03 DEM..."
    hits = where(result eq ffb03_filedir, n_hits)
    if n_hits eq 1 then print, ">>>   SRTM_ffB03 DEM exists, using it" else $
      print, ">>>   SRTM_ffB03 DEM does not exist, skipping ", ppp+rrr  
  endif
  
  if n_hits ne 0 then begin
    dem_filename = strcompress(result[hits]+".tif.gz", /rem)
    outfile_path = strcompress(out_path + dem_filename, /rem)
    obj = obj_new('IDLnetUrl')  
    url = string(strcompress(url_path+result[hits]+"/"+dem_filename, /rem))
    result = obj->Get(filename=outfile_path[0], url=url[0])
  endif
end
