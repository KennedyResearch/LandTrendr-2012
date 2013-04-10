pro reproject_ledaps, path, ppprrr

  ;find the ledaps image and the projection reference image
  ledaps_img = file_search(path+"madcal\", "*ledaps_sr*.bsq", count=n_ledaps_img)
  if n_ledaps_img gt 1 then begin
    print, ""
    print, ">>> !!!warning there is more than one '*ledaps*.bsq' file in the..."
    print, ">>> madcal directory, there should only be one ledaps reference image..."
    print, ">>> please remove one and it's associated .hdr file and rerun."
    print, ">>> ending program"
    print, ""
    stop
  endif
  if n_ledaps_img eq 0 then begin
    print, ""
    print, ">>> !!!warning there is no '*ledaps_sr*.bsq' reference image in the madcal directory..."
    print, ">>> please check that you put a stacked 7 band ledaps file in the madcal directory..."
    print, ">>> with the naming convention: image code (glovis .tar.gz filename) followed by '_ledaps_sr.bsq'..."
    print, ">>> example: LT50450302001208EDC01_ledaps_sr.bsq"
    print, ">>> ending program"
    print, ""
    stop
  endif
  
  ;check to see if a reference image already exists
  radref = file_search(path, "*radref.bsq", count=n_radref) ;find the radref image
  if n_radref ge 1 then begin
    wilco = file_search(path, "*radref*", count=nthese) ;if one exists find all associated files
    wilcogoods = where(strmatch(wilco, "*txt") ne 1 and wilco ne '', n_wilco)
    
    if n_wilco ge 1 then begin
      wilco = wilco[wilcogoods]
      message1 = transpose(["A reference image already exists, do you want to overwrite it?","", $
        "It is recommended that you do otherwise you're going to run into troubles later.  If you say yes then all " +$
        "of the normalized images and their Tasseled Cap transformations will be deleted since they are derived from " +$
        "the reference image and will no longer be representative of the new reference image.", "","Do you want to " +$
        "to overwrite the existing reference image and its dependants?"])
      b = dialog_message(message1, /question, /cancel)
      
      if b eq 'Yes' then begin
        close, /all
        ;find the normalized images and delete them since they'll all have to change
        normimgs = file_search(path, "*_to_*")
        tcimgs = file_search(path, "*ltc*")
        todelete = [normimgs,tcimgs]
        todelete_index = where(strmatch(todelete, "*txt") ne 1 and strmatch(todelete, "*pro") $
          ne 1 and todelete ne '', n_todelete)
        if n_todelete ge 1 then begin
          todelete = todelete[todelete_index]
          file_delete, wilco, todelete, /allow_nonexistent
        endif else file_delete, wilco, /allow_nonexistent
        tengo = file_search(path, "*_orig*", count=n_tengo)
        if n_tengo ge 1 then begin
          for i=0, n_tengo-1 do begin
            tengolen = strlen(tengo[i])
            tengopos = strpos(tengo[i], "_orig")
            tengopart1 = strmid(tengo[i],0,tengopos)
            tengopart2 = strmid(tengo[i], tengopos+5, tengolen-tengopos+5)
            humptydumpty = strcompress(tengopart1+tengopart2, /rem)
            file_move, tengo[i], humptydumpty
          endfor
        endif 
      endif
      if b eq 'Cancel' then stop
    endif
  endif
  
  proj = file_search(path+"VCT\prep\", "*ref_img_projection.prf", count=n_proj)
  
  ;get year and julian day
  ltnr = file_basename(ledaps_img)
  glovis = {year:strmid(ltnr, 9,4), doy :strmid(ltnr,13,3), sensor :strmid(ltnr,1,2), $
    path:strmid(ltnr, 4,2), row :strmid(ltnr, 7,2)}
    
  ;create output filename
  outSubPath = path+"madcal\"
  outfilebase='L'+glovis.sensor+'0'+glovis.path+'0'+glovis.row+'_'+glovis.year+'_'+glovis.doy
  time = timestamp()  ;string(bin_date(systime()), format='(I4,I02,I02,"_",I02,I02,I02)')
  outfile = outSubPath+outfilebase+'_'+time+'_ledaps_radref.bsq'
  outtfile = outSubPath+outfilebase+'_'+time+'_ledaps_radref_b6.bsq'
  
  ;create command to project image using gdalwarp
  if n_proj eq 1 then begin
    temp_name_bsq = strcompress(file_dirname(ledaps_img)+"\temp_"+file_basename(ledaps_img), /rem)
    s1 = "gdalwarp -t_srs " + proj
    s2 = " -of ENVI -tr 30 30 -overwrite -multi "
    cmd = s1 + s2 + ledaps_img + " " + temp_name_bsq
    if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
    print, ">>> reprojecting file: "+file_basename(ledaps_img)
    spawn, cmd, /hide
  endif else message, "can't find proj ref .prf file or there are more than one"
  
  zot_img, temp_name_bsq, hdr, img, /hdronly
  ;create a command to subset band 1,2,3,4,5,7 and band 6
  print, ">>> subsetting bands: 1, 2, 3, 4, 5, and 7"
  img_holder = intarr(hdr.filesize[0], hdr.filesize[1],6)
  layers = [1,2,3,4,5,7]
  for i=0, 6-1 do begin
    zot_img, temp_name_bsq, hdr, img, layers=layers[i]
    img[where(img eq -9999)] = 0
    img_holder[*,*,i] = temporary(img)
  endfor
  
  openw, unit, outfile, /get_lun
  writeu, unit, temporary(img_holder)
  free_lun, unit
  hdr.n_layers = 6    ;6 layers
  hdr.pixeltype = 6   ;unsigned 8 bit
  write_im_hdr, outfile, hdr
  
  ;now deal with the thermal band (convert from tiff to bsq and reproject to albers)
  print, ">>> subsetting band: 6"
  zot_img, temp_name_bsq, hdr, img, /hdronly
  ;create a command to subset band 1,2,3,4,5,7 and band 6
  img_holder = intarr(hdr.filesize[0], hdr.filesize[1])
  
  zot_img, temp_name_bsq, hdr, img, layers=6
  img[where(img eq -9999)] = 0
  img_holder = temporary(img)
  
  openw, unit, outtfile, /get_lun
  writeu, unit, temporary(img_holder)
  free_lun, unit
  hdr.n_layers = 1    ;6 layers
  hdr.pixeltype = 6   ;unsigned 8 bit
  write_im_hdr, outtfile, hdr
  
  ;get rid of the temporary files
  temp_files = file_search(file_dirname(temp_name_bsq), "*temp*", count=count)
  if count ge 1 then file_delete, temp_files else begin
    print, ">>> program attempting to clean temporary files..."
    print, ">>> there are no temporary files to delete..."
    print, ">>> if this seems strange, check the following directory..."
    print, ">>> ", file_dirname(temp_name_bsq)
    stop
  endelse
  
  fix_cloud_masks3, 'dummy', 1, fixmask=fixmask, from_madcal=outfile, /ledaps
  
end