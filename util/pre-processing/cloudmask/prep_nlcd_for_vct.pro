pro prep_nlcd_for_vct, path, ppprrr, nlcdimg

  ppp = strmid(ppprrr, 0, 3)
  rrr = strmid(ppprrr, 3, 3)
  pr = strcompress("p"+ppp+"r"+rrr ,/rem)
  vctancdatadir = strcompress(path+"VCT\ancData\", /rem)
  vctprepdir = strcompress(path+"VCT\prep\", /rem)
  
  ;reproject the dem to the proj of the passed nlcd map to check on the extent
  nlcdimgprf = vctprepdir+ppprrr+"_nlcdimg_proj.prf"
  nlcdimgproj = gdal_get_projection(nlcdimg, prf_outfile=nlcdimgprf)
  vctdemfile = file_search(vctancdatadir, "dem_"+pr)
  tempdemout = strcompress(vctprepdir+"temp_dem_in_nlcd_proj.tif", /rem)
  s1 = "gdalwarp -t_srs " + nlcdimgprf
  s2 = " -tr 30 30 -multi -overwrite "
  cmd = s1 + s2 + vctdemfile[0] + " " + tempdemout
  if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
  spawn, cmd, /hide
  
  nlcdprojcoords = gdal_check_extent(checkthisfileextent=nlcdimg, againstthisfileextent=tempdemout)
  if nlcdprojcoords.toosmall eq 1 then begin
      coords = get_subset(imagefile=vctdemfile)
      print, ""
      print, ">>> warning!!! the extent of the passed nlcd map:
      print, ">>>   ",nlcdimg
      print, ">>> is too small..."
      print, ">>> it must be at least:"
      print, ""
      print, "      ulx: ", coords[0]
      print, "      uly: ", coords[1]
      print, "      lrx: ", coords[2]
      print, "      lry: ", coords[3]
      print, ""
      print, ">>> coordinate system:"
      print, ""
      demproj = gdal_get_projection(vctdemfile)
      print, transpose(demproj)
      print, ""
      print, ">>> warning!!! the extent of the passed dem:
      print, ">>>   ",input_dem
      print, ">>> is too small... read above to find the min extent required"
      print, ">>> also, make sure that the passed nlcd map meets this extent"
      print, ""
      print, ">>> ending program"
      print, ""
      stop  
  endif
  
  
  print, ">>> preparing Landsat WRS-2 scene: ", pr, " NLCD map for VCT - please wait..."
  lc_outfile = strcompress(vctprepdir+ppprrr+"_lc_subset.tif")
  
  ;get the subset from the dem warped to the passed nlcd 
  coords = get_subset(subset=nlcdprojcoords.trcoords)
  ;subset nlcd map to native glovis projection
  s1 = "gdal_translate -ot Byte "
  s2 = "-projwin "+string(coords[0])+" "+string(coords[1])+" "+string(coords[2])+" "+string(coords[3])
  s3 = " "+nlcdimg+" "+lc_outfile
  cmd = s1 + s2 + s3
  if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
  spawn, cmd, /hide
  
  ;reproject the subset nlcd map to native glovis projection for vct
  wgs84nlcdout = strcompress(vctancdatadir+"lc_"+pr, /rem)
  s1 = "gdalwarp -t_srs " + strcompress(path+"VCT\prep\"+ppprrr+"_native_glovis_proj.prf", /rem)
  s2 = " -of ENVI -tr 30 30 -overwrite -multi "
  cmd = s1 + s2 + lc_outfile + " " + wgs84nlcdout
  if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
  spawn, cmd, /hide

  ;reproject the subset nlcd map to LT proj glovis projection for vct
  projref_nlcdout = strcompress(vctprepdir+ppprrr+"_landcover.bsq", /rem)
  s1 = "gdalwarp -t_srs " + strcompress(path+"VCT\prep\"+ppprrr+"_ref_img_projection.prf", /rem)
  s2 = " -of ENVI -tr 30 30 -overwrite -multi "
  cmd = s1 + s2 + lc_outfile + " " + projref_nlcdout
  if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
  spawn, cmd, /hide  
  
  
  ;delete the .xml file that is created
  getridof = file_search(path+"\VCT\", "*.xml*", count=n_getridof)
  if n_getridof ge 1 then file_delete, getridof 
  
  ;fix the envi hdr so that it has units and remove the coordinate system string so that vct does not crash
  gdal_envi_hdr_fixer, vctancdatadir
  
end