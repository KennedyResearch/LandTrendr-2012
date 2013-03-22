function envi_glovis_to_landtrendr_doit, inpath, outpath, overwrite=overwrite, proj=proj, $
    skipThermal=skipThermal, pixelSize=pixelSize
    
  warnings = strarr(1)
  if not keyword_set(overwrite) then overwrite=0B
  if not keyword_set(skipThermal) then skipThermal=0B
  
  
  ; get year and julian day
  ltnr = file_basename(inpath)
  glovis = {year:strmid(ltnr, 9,4), doy :strmid(ltnr,13,3), sensor :strmid(ltnr,1,2), $
    path:strmid(ltnr, 4,2), row :strmid(ltnr, 7,2)}
    
  model = fix(strmid(ltnr,2,1))
  if model eq 0 then return, {ok:0, error:'Could not identify Landsat Model for '+inpath}
  
  ; retrieve filenames for the 6 bands
  if model lt 4 then searchString = '*b[4567]*.tif' else searchString = '*b[123457]*.tif'
  bandFiles = file_search(inpath+searchString, /fold_case)
  bandFiles = bandFiles[sort(bandFiles)]
  if bandFiles[0] eq "" then return, {ok:0, error:'No file found in '+inpath}
  
  ; retrieve filename for the thermal band
  if model gt 4 then thermal = file_search(inpath+'*b[69]*.tif', /fold_case) else thermal = ''
  if thermal[0] eq "" then warnings=[warnings,'No thermal band found in '+inpath] else $
    thermal = thermal[sort(thermal)]
    
  ; create output filename
  outSubPath = outpath+glovis.year+'\'
  outfilebase='L'+glovis.sensor+'0'+glovis.path+'0'+glovis.row+'_'+glovis.year+'_'+glovis.doy
  time = timestamp()  ;string(bin_date(systime()), format='(I4,I02,I02,"_",I02,I02,I02)')
  outfile = outSubPath+outfilebase+'_'+time+'_archv.bsq'
  outtfile = outSubPath+outfilebase+'_'+time+'_b6.bsq'
  
  ;if this date already exists delete everything but the meta data file
  lt_delete_duplicate, outfile, /archv, /b6thermal
  
  ; check if outfiles already exist
  oExists = file_test(outfile)
  tExists = file_test(outtfile)
  if oExists eq 1 and overwrite then warnings=[warnings,outfile +' was overwritten.']
  if tExists eq 1 and overwrite then warnings=[warnings,outtfile+' was overwritten.']
  if oExists eq 1 and overwrite eq 0 then warnings=[warnings,outfile +' already exists']
  if tExists eq 1 and overwrite eq 0 then warnings=[warnings,outtfile+' already exists']
  
  
  ;determine if we actually want to write the output
  write_6band  = oExists le overwrite
  write_thermal = (tExists le overwrite) and thermal[0] ne "" and skipThermal eq 0
  
  
  ;How many bands do we have here?
  n_bands = n_elements(bandFiles)
  if n_elements(bandFiles) ne 6 then begin ;check for 6 bands: 1,2,3,4,5,7
    print, ">>> the number of bands is not correct..."
    print, ">>> check the following directory..."
    print, ">>> ", file_dirname(bandFiles[i])
    stop
  endif
  
  ;Create outpath if it doesn't exist already
  if file_test(outSubPath, /directory) ne 1 then file_mkdir, outSubPath
  
  ;move the mtl files so they stay with the images
  mtlfile = file_search(inpath, "*MTL.txt", count=n_mtlfile)
  wofile = file_search(inpath, "*WO.txt", count=n_wofile)
  if n_mtlfile eq 1 and n_wofile eq 1 then begin
    print, ">>> apparently this directory: "
    print, "  ", inpath
    print, ">>> contains both an MTL and WO metadata file..."
    print, ">>> this shouldn't happen, see what's up"
    stop
  endif
  if n_mtlfile eq 0 and n_wofile eq 0 then begin
    print, ">>> apparently this directory: "
    print, "  ", inpath
    print, ">>> does not contain either an MTL or WO metadata file"
    print, ">>> check the directory and see what's up"
    stop
  endif
  
  if n_mtlfile eq 1 then begin
    metafilebase=strcompress('L'+glovis.sensor+'0'+glovis.path+'0'+glovis.row+'_'+glovis.year+'_'+glovis.doy+'_MTL.txt', /rem)
    metaout = strcompress(outSubPath+metafilebase, /rem)
    file_move, mtlfile[0], metaout, /overwrite
  endif else begin
    metafilebase=strcompress('L'+glovis.sensor+'0'+glovis.path+'0'+glovis.row+'_'+glovis.year+'_'+glovis.doy+'_WO.txt', /rem)
    metaout = strcompress(outSubPath+metafilebase, /rem)
    file_move, wofile[0], metaout, /overwrite
  endelse
  
  
  dir = file_dirname(outfile)
  dirlen = strlen(dir)
  path = strmid(dir, 0, dirlen-11)
  
  ;convert the .tiff bands to .bsq using gdal
  bands = [1,2,3,4,5,7] ;make a list of band numbers to be added to a tempory name
  for i=0, n_bands-1 do begin
    temp_name_bsq = strcompress(bandFiles[i]+"_temp"+string(bands[i])+".bsq", /rem) ;define temporary name tiff to bsq conversion (.bsq)
    
    ;create command to project image using gdalwarp
    proj = file_search(path+"VCT\prep\", "*ref_img_projection.prf", count=n_proj)
    if n_proj eq 1 then begin
      s1 = "gdalwarp -t_srs " + proj
      s2 = " -of ENVI -tr 30 30 -overwrite -multi "
      cmd = s1 + s2 + bandFiles[i] + " " + temp_name_bsq
      if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
      spawn, cmd, /hide
    endif else message, "can't find proj ref .prf file or there are more than one"
  endfor
  
  ;find the converted bands
  searchString = '*temp[123457].bsq'
  bandFiles = file_search(inpath+searchString)
  bandFiles = bandFiles[sort(bandFiles)]
  
  ;How many bands do we have here?
  n_bands = n_elements(bandFiles)
  if n_elements(bandFiles) ne 6 then begin ;check for 6 bands: 1,2,3,4,5,7
    print, ">>> the number of bands is not correct..."
    print, ">>> check the following directory..."
    print, ">>> ", file_dirname(bandFiles[i])
    stop
  endif
  
  ;read in the bsq's and make sure they are the same size
  struct = create_struct("x", 10000, "y", 10000)
  img_size = replicate(struct,6)
  
  for i=0, n_elements(bandfiles)-1 do begin
    zot_img, bandFiles[i], hdr, img, /hdronly
    img_size[i].x = hdr.filesize[0]
    img_size[i].y = hdr.filesize[1]
  endfor
  
  n_unique = n_elements(fast_unique(img_size.x))
  if n_unique gt 1 then begin
    print, ">>> the GLOVIS images bands are of different X dimensions"
    print, ">>> check the following directory
    print, ">>> ",  file_dirname(bandFiles[i])
    stop
  endif
  
  n_unique = n_elements(fast_unique(img_size.y))
  if n_unique gt 1 then begin
    print, ">>> the GLOVIS images bands are of different Y dimensions"
    print, ">>> check the following directory
    print, ">>> ",  file_dirname(bandFiles[i])
    stop
  endif
  
  ;if the band dims are the same then create a 6 band image and fill it with each individual glovis band
  ;create the img holder
  img_holder = bytarr(img_size[0].x, img_size[0].y, 6)
  for i=0, n_elements(bandFiles)-1 do begin
    zot_img, bandFiles[i], hdr, img
    img_holder[*,*,i] = img
  endfor
  
  openw, unit, outfile, /get_lun
  writeu, unit, img_holder
  free_lun, unit
  hdr.n_layers = 6    ;6 layers
  hdr.pixeltype = 3   ;unsigned 8 bit
  write_im_hdr, outfile, hdr
  
  ;write out the metadata
  output_metadata_file = stringswap(outfile, ".bsq", "_meta.txt")
  meta = make_metadata_for_preprocessing(outfile)
  openw, fun, output_metadata_file, /get_lun
  printf, fun, convert_struct_to_string(meta)
  free_lun, fun
  
  ;convert the flat hdr so that gdal can read it for the next lines
  convert_bsq_headers_to_envi, file_dirname(outfile), strcompress(path+"study_area\mrlc_template_headerfile.hdr", /rem)
  
  
  ;now deal with the thermal band (convert from tiff to bsq and reproject to albers)
  if file_test(thermal[0]) eq 1 then begin ;check if it exists - ETM+ has 2 thermals... using the first one per Dirk's code and because ETM+  b61 is the thermal that VCT uses 
    ;now reproject the thermal band and put it in the correct place
    ;create command to project image using gdalwarp (into albers)
    s1 = "gdalwarp -t_srs " + proj
    s2 = " -of ENVI -tr 30 30 -overwrite -multi "
    cmd = s1 + s2 + thermal[0] + " " + outtfile
    ;run the command
    spawn, cmd, /hide
    
    ;write out the metadata
    output_metadata_file = stringswap(outtfile, ".bsq", "_meta.txt")
    meta = make_metadata_for_preprocessing(outtfile)
    openw, fun, output_metadata_file, /get_lun
    printf, fun, convert_struct_to_string(meta)
    free_lun, fun
    
  endif else print, ">>> there is no thermal band"
  
  ;get rid of the temporary files
  temp_files = file_search(file_dirname(bandFiles[0]), "*temp*", count=count)
  if count ge 1 then file_delete, temp_files else begin
    print, ">>> program attempting to clean temporary files..."
    print, ">>> there are no temporary files to delete..."
    print, ">>> if this seems strange, check the following directory..."
    print, ">>> ", file_dirname(bandFiles[0])
    stop
  endelse
  
  close, /all
  return, {ok:1, message:'  Finished '+ltnr+' without errors.', warnings:warnings}
  
end