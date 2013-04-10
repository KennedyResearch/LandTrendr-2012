pro prep_dem_for_vct, path, ppprrr, proj_ref_file, input_dem=input_dem

  ;---make some standard values---
  ppp = strmid(ppprrr, 0, 3)
  rrr = strmid(ppprrr, 3, 3)
  pr = strcompress("p"+ppp+"r"+rrr ,/rem)
  vctancdatadir = strcompress(path+"VCT\ancData\", /rem)
  vctprepdir = strcompress(path+"VCT\prep\", /rem)
  vctoutputsdir = strcompress(path+"VCT\outputs\", /rem)
  
  ;get the projection of the glovis images
  print, ">>> finding VCT GLOVIS images"
  ref_img = file_search(vctoutputsdir, '*L*[4571]', count=count)
  if count eq 0 then begin
    print, ">>>   warning! there are no unzipped glovis image files in this scenes *\VCT\outputs\ folder"
    print, ">>>   ending program..."
    print, ""
    stop
  endif
  
  glovisprojection = strcompress(vctprepdir+ppprrr+"_native_glovis_proj.prf", /rem)
  glovisproj = gdal_get_projection(ref_img[0], prf_outfile=glovisprojection)
  
  ;find the maximum extent of all of the image files to make a subset
  print, ">>> finding the maximum extent of the glovis images"
  images = file_search(vctoutputsdir, '*L*[4571]')
  for i=0, n_elements(images)-1 do begin
    zot_img, images[i], hdr, img, /hdronly
    coords = get_subset(imagefile=images[i])
    if i eq 0 then begin
      ulx = coords[0]
      uly = coords[1]
      lrx = coords[2]
      lry = coords[3]
    endif else begin
      ulx = min([ulx, coords[0]])
      uly = max([uly, coords[1]])
      lrx = max([lrx, coords[2]])
      lry = min([lry, coords[3]])
    endelse
  endfor
  ;add some buffer 10 km
  ulx = ulx-10000
  uly = uly+10000
  lrx = lrx+10000
  lry = lry-10000
  
  ;if a dem was not passed by the user then download them
  if keyword_set(input_dem) eq 0 then begin
    ;find the surrounding path rows for dem download (9 dems)
    ppp = fix(ppp)
    rrr = fix(rrr)
    
    northwest = [ppp+1,rrr-1]
    north     = [ppp,rrr-1]
    northeast = [ppp-1,rrr-1]
    east      = [ppp-1,rrr]
    southeast = [ppp-1,rrr+1]
    south     = [ppp,rrr+1]
    southwest = [ppp+1,rrr+1]
    west      = [ppp+1,rrr]
    center    = [ppp,rrr]
    
    alldems = string([[northwest],[north],[northeast],[east],[southeast],[south],[southwest],[west],[center]])
    for i=0, n_elements(alldems[0,*])-1 do begin
      len = strlen(strtrim(alldems[0,i],2))
      if len eq 1 then zeros='00'
      if len eq 2 then zeros='0'
      if len eq 3 then zeros=''
      alldems[0,i] = strcompress(zeros+alldems[0,i], /rem)
      
      len = strlen(strtrim(alldems[1,i],2))
      if len eq 1 then zeros='00'
      if len eq 2 then zeros='0'
      if len eq 3 then zeros=''
      alldems[1,i] = strcompress(zeros+alldems[1,i], /rem)
    endfor
    
    for i=0, n_elements(alldems[0,*])-1 do begin
      print, ">>> downloading DEM: ",alldems[0,i]+alldems[1,i], " "+strcompress(string(i+1)+"/"+string(9), /rem)
      vct_dem_downloader, alldems[0,i], alldems[1,i], vctprepdir
    endfor
    
    ;decompress the tif.gz's
    drive = strmid(path, 0, 2)
    length = strlen(path)
    pdrive = strmid(path, 2, length-2)
    cmd = strcompress(pdrive + "glovis_targz\7za", /rem)+ " e " + $  ;"VCT\prep\7za"
      strcompress(pdrive+"VCT\prep\", /rem) + " " + strcompress("-o"+drive+pdrive+"VCT\prep", /rem)
    cmd = drive+ " && " + cmd
    if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
    spawn, cmd, /hide
    
    input_dem_file = file_search(vctprepdir, "*tif")
    demout = strarr(n_elements(input_dem_file))
    for i=0, n_elements(input_dem_file)-1 do demout[i] = strcompress(vctprepdir+"dem_temp"+string(i+1)+".tif", /rem)
    
    for i=0, n_elements(input_dem_file)-1 do begin
      ;todo: the reprojection into native glovis- a future improvement would be to check the projections and if they match then do not reproject
      print, ">>> reprojecting DEM: ", file_basename(input_dem_file[i]), " into native glovis projection"
      s1 = "gdalwarp -t_srs " + glovisprojection
      s2 = " -tr 30 30 -ot UInt16 -multi -overwrite "
      cmd = s1 + s2 + input_dem_file[i] + " " + demout[i]
      ;run the command
      if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
      spawn, cmd, /hide
    endfor
    
    ;mosaic the dems and clip to the found extent
    print, ">>> merging the DEMs - please wait..."
    dem_out = strcompress(vctprepdir+"temp_dem_mosaic.tif")
    if file_exists(dem_out) eq 1 then file_delete, dem_out
    tempdems = file_search(vctprepdir, "dem_temp[123456789]*.tif")
    stringem = strjoin(tempdems, " ")
    s1 = "gdal_merge.py -o " + dem_out + " -n 0 "
    s2 = "-ul_lr "+string(ulx)+" "+string(uly)+" "+string(lrx)+" "+string(lry)+" "
    cmd = s1 + s2 + stringem
    if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
    spawn, cmd, /hide
    
  endif else begin
    ;todo: the reprojection into native glovis- a future improvement would be to check the projections and if they match then do not reproject
    ok = file_exists(input_dem)
    if ok eq 1 then begin
      print, ">>> reprojecting DEM: ", file_basename(input_dem), " into native glovis projection"
      print, ">>> please wait..."
      s1 = "gdalwarp -t_srs " + glovisprojection
      s2 = " -tr 30 30 -ot UInt16 -multi -overwrite "
      dem_out = strcompress(vctprepdir+"dem_temp"+string(1)+".tif", /rem)
      ;if file_exists(dem_out) eq 1 then file_delete, dem_out
      cmd = s1 + s2 + input_dem + " " + dem_out
      if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
      spawn, cmd, /hide
      ;check the extent
      
      ok = gdal_check_extent(checkthisfileextent = dem_out, againstthesecoords = [[ulx,uly],[lrx,lry]])
      if ok.toosmall eq 1 then begin
        print, ""
        print, ">>> warning!!! the extent of the passed dem:
        print, ">>>   ",input_dem
        print, ">>> is too small..."
        print, ">>> it must be at least:"
        print, ""
        print, "      ulx: ", ulx
        print, "      uly: ", uly
        print, "      lrx: ", lrx
        print, "      lry: ", lry
        print, ""
        print, ">>> coordinate system:"
        print, ""
        print, transpose(glovisproj)
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
      
    endif else begin
      print, ""
      print, ">>> warning!!! the input dem file:"
      print, ">>>   ",input_dem
      print, ">>> does not exist - check the 'input_dem'..."
      print, ">>> variable of this scene's batchfile to..."
      print, ">>> make sure the given path is correct"
      print, ""
      print, ">>> ending program"
      stop
    endelse
  endelse
  
  ;convert the dem to the way VCT wants it
  print, ">>> preparing Landsat WRS-2 scene: ", pr, " DEM for VCT - please wait..."
  demout = strcompress(vctancdatadir+"dem_"+pr, /rem)
  s1 = "gdal_translate -ot UInt16 -of ENVI "
  s2 = dem_out+" "+demout
  cmd = s1 + s2
  if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
  spawn, cmd, /hide
  
  ;do the reprojection into the ref_img projection so we have a copy for later use
  prf_outfile=vctprepdir+ppprrr+"_ref_img_projection.prf"
  proj = gdal_get_projection(proj_ref_file, prf_outfile=prf_outfile)
  s1 = "gdalwarp -t_srs " + prf_outfile
  s2 = " -of ENVI -tr 30 30 -ot UInt16 -overwrite -multi "
  cmd = s1 + s2 + dem_out + " " + strcompress(vctprepdir+ppprrr+"_dem.bsq", /rem)
  ;run the command
  if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
  spawn, cmd, /hide
  
  ;delete the .xml file that is created
  getridof = file_search(path+"VCT\", "*.xml*", count=n_getridof)
  if n_getridof ge 1 then file_delete, getridof
  
  gdal_envi_hdr_fixer, strcompress(vctancdatadir, /rem)
  
end