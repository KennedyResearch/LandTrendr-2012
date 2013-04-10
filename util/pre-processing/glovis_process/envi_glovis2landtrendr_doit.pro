FUNCTION envi_glovis2landtrendr_doit, inpath, outpath, overwrite=overwrite, proj=proj, skipThermal=skipThermal, pixelSize=pixelSize, from_auto=from_auto
  
  ; Changing resampling and warping only effects thermal band
  ; the rest is reprojected within envi's stack proceedure which only does
  ; triangulation and nearest-neighbor resampling
  resampling = 0  ; {NN:1, BL:1, CC:2}
  warping = 2     ; {Polynomial:1, Triangulation:2, Rigorous:3}
  backgroundVal=0
  warnings = strarr(1)
  if not keyword_set(overwrite) then overwrite=0B
  if not keyword_set(skipThermal) then skipThermal=0B
  if n_elements(proj) eq 0 then proj=0B
  if n_elements(pixelSize) eq 0 then pixelSize=30D
 
  ; - - - projection parameter - - - - -
  albers = envi_proj_create(datum="North America 1983", name="MRLC_albers_con_eq_area", $
                               params=[6378137.D, 6356752.3D, 23D,  -96D, $
                                       0D, 0D, 29.5D, 45.5D], type=9, units=0)
  orlam = envi_proj_create(datum="North America 1983", name="Oregon_Lambert", $
                               params=[6378137.D, 6356752.3D, 41.75D,  -120.5D, $
                                       400000D, 0D, 43D, 45.5D], type=4, units=0)
  swan = envi_proj_create(datum="North America 1983", name="SWAN_albers_con_eq_area", $
                              params=[6378137.D, 6356752.3D, 50.0D,  -154D, $
                                      0D, 0D, 55.0D, 65.0D], type=9, units=0) 
  ; - - - projection parameter - - - - -
 
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
  outSubPath = outpath+glovis.path+glovis.row+'/glovis/'+glovis.year+'/'
  if keyword_set(from_auto) eq 1 then outSubPath = outpath+glovis.year+'\'
  outfilebase='L'+glovis.sensor+'0'+glovis.path+'0'+glovis.row+'_'+glovis.year+'_'+glovis.doy
  outfile = outSubPath+outfilebase+'_archv.bsq'
  outtfile = outSubPath+outfilebase+'_b6.bsq'

  ; check if outfiles already exist
  oExists = file_test(outfile)
  tExists = file_test(outtfile)
  if oExists eq 1 and overwrite then warnings=[warnings,outfile +' was overwritten.']
  if tExists eq 1 and overwrite then warnings=[warnings,outtfile+' was overwritten.']
  if oExists eq 1 and overwrite eq 0 then warnings=[warnings,outfile +' already exists']
  if tExists eq 1 and overwrite eq 0 then warnings=[warnings,outtfile+' already exists']  

  
  ; determine if we actually want to write the output
  write_6band  = oExists le overwrite
  write_thermal = (tExists le overwrite) and thermal[0] ne "" and skipThermal eq 0

  
  ; How many bands do we have here?
  n_bands = n_elements(bandFiles)
  ;if n_elements(bandFiles) ne 6 then return, {ok:0, error:'Need 6 bands'}
  
  ; Create outpath if it doesn't exist already
  if file_test(outSubPath, /directory) ne 1 then file_mkdir, outSubPath

  ;move the mtl files so they stay with the images
  mtlfile = file_search(inpath, "*MTL.txt")
  if mtlfile eq '' then mtlfile = file_search(inpath, "*WO.txt")
  mtlname = file_basename(mtlfile)
  mtlout = strcompress(outSubPath+outfilebase+mtlname, /rem)
  mtlstruct = {file:mtlfile, year:strmid(mtlname, 12,4), outfile:mtlout}
  
  if mtlfile ne '' then file_move, mtlstruct.file, mtlstruct.outfile, /overwrite
  
  ; Open band files
  fids = lonarr(n_bands)
  for i=0, n_bands-1 do begin
   envi_open_data_file, bandFiles[i], r_fid=s_fid
   fids[i] = s_fid
  endfor

  ; check if reading was successful
  inds = where(fids eq -1, cnt)
  if cnt gt 0 then return, {ok:0, error:'Could not read '+strjoin(bandFiles[inds], ',')}

  map_info = envi_get_map_info(fid=s_fid[0])
  
  case proj of
    0 : oProj = albers
    1 : oProj = map_info.proj
    2 : oProj = orlam
    3 : oProj = swan
  else: return, {ok:0, error:'Illegal projection parameter'}
  endcase


  ; stack and reproject
  if write_6band then begin
    dims = lonarr(5, n_bands)
    for i=0, n_bands-1 do begin
      envi_file_query, fids[i], NS = ns, NL = nl, NB = nb, data_type=dt
      dims[*,i] = [-1, 0, ns - 1, 0, nl - 1]
    endfor
    envi_doit, 'envi_layer_stacking_doit', dims=dims, fid=fids, interp=0, out_dt=dt, $
      out_bname= (["Band 1","Band 2","Band 3","Band 4","Band 5","Band 7"])[0:n_bands-1], $
      out_name=outfile, out_proj=oproj, out_ps=[pixelSize,pixelSize], pos=intarr(n_bands), r_fid=r_fid 
  endif    

  if write_thermal then begin
  
    ; open thermal band (low-gain for landsat 7 if both are supplied)
    envi_open_data_file, thermal[0], r_fid=t_fid
    if t_fid eq -1 then return, {ok:0, error:'Could not read thermal '+thermal[0]}
  
    envi_file_query, t_fid, NS = ns, NL = nl, NB = nb
    tdims = [-1, 0, ns - 1, 0, nl - 1]
    envi_convert_file_map_projection, background=backgroundVal, dims=tdims, fid=t_fid, $
          o_proj=oProj, out_name=outtfile, out_bname="Band 6", pos=0, r_fid=rt_fid, $
          resampling=resampling, warp_method=warping, o_pixel_size=[pixelSize,pixelSize]
  endif


  ;clean up interface
  for i=0, n_bands-1 do envi_file_mng, /remove, id=fids[i]
  if n_elements(r_fid) gt 0 then envi_file_mng, /remove, id=r_fid
  if n_elements(t_fid) gt 0 then  envi_file_mng, /remove, id=t_fid
  if n_elements(rt_fid) gt 0 then envi_file_mng, /remove, id=rt_fid  
  
  return, {ok:1, message:'Finished '+ltnr+' without errors.', warnings:warnings}

END