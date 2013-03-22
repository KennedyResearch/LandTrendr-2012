;This script converts the values in the vct masks to
;the value required for LT masks and renames and moves
;the files so they align with their respective images dates
;
;vct cover types aggregated include: 1=water 2=shadow 3=shadow edge
;4=cloud edge 5=cloud 7=snow

pro create_lt_masks_from_vct_masks, path, ppprrr, proj, templatehdr, outpath=outpath, water_off=water_off,$
    snow_off=snow_off   ;, update=update
    
  ;---set up paths to various places---
  ;drive = strmid(path, 0, 2) ;'E:'
  ppp = strmid(ppprrr, 0, 3) ;'046'
  rrr = strmid(ppprrr, 3, 3) ;'026
  pr = strcompress("p"+ppp+"r"+rrr ,/rem) ;'p046r026
  
  
  ;  if keyword_set(update) eq 1 then vct_output_path = strcompress(path+"VCT\outputs\update\", /rem) else $
  ;    vct_output_path = strcompress(path+"VCT\outputs\", /rem)
  
  
  vct_output_path = strcompress(path+"VCT\outputs\", /rem)
  lt_img_path = strcompress(path+'images\', /rem)
  studyarea_dir = strcompress(path+'study_area\', /rem)
  usearea_composite = strcompress(path+"VCT\outputs\usearea_composite\", /rem)
  proj = file_search(path+"VCT\prep\", "*ref_img_projection.prf", count=n_proj)
  if n_proj ne 1 then message, "can't find proj ref .prf file or there are more than one"
  
  ;---put a .bsq at the end of the vct cover masks---
  m_files = file_search(vct_output_path, '*_mask', count=count) ;find the mask images
  if count gt 0 then begin
    for i=0, n_elements(m_files)-1 do begin ;cycle through the mask images and rename them with .bsqs
      new_m_file = strcompress(m_files[i]+".bsq", /rem) ;add the bsq
      file_move, m_files[i], new_m_file, /overwrite  ;create a new file with the bsq
    endfor
  endif
  
  ;---find the VCT masks and common area mask---
  m_files = file_search(vct_output_path, '*_mask.bsq')
  comarea_file = file_search(usearea_composite, '*_vct_usearea.bsq')
  
  ;---reproject the commonarea mask---
  ;if keyword_set(update) eq 0 then begin
  ;if we're creating one, make sure that one does not already exist
  vctuseareafiles = file_search(studyarea_dir, "*vct_usearea*", count=count1)
  if count1 ge 1 then begin
    files = vctuseareafiles[where(strmatch(vctuseareafiles, "*.txt") ne 1)] ;don't delete the metadata if it does exist
    file_delete, vctuseareafiles ;delete it if it does
  endif
  
  time = timestamp()  ;string(bin_date(systime()), format='(I4,I02,I02,"_",I02,I02,I02)')
  outcomareafile = strcompress(studyarea_dir+ppprrr+"_"+time+"_vct_usearea.bsq")
  outcomareafilehdr = strcompress(studyarea_dir+ppprrr+"_"+time+"_vct_usearea.hdr")
  if keyword_set(update) ne 1 then begin
    if file_test(outcomareafile) eq 1 then file_delete, outcomareafile ;delete the file if it exists, otherwise gdal will not like it, a newer version allows delete
    if file_test(outcomareafilehdr) eq 1 then file_delete, outcomareafilehdr
    
    ;create command to project image using gdalwarp
    s1 = "gdalwarp -t_srs " + proj
    s2 = " -of ENVI -tr 30 30 -overwrite -multi "
    cmd = s1 + s2 + comarea_file + " " + outcomareafile
    ;run the command
    spawn, cmd, /hide
  endif
  ; endif else outcomareafile = file_search(path+"study_area\", "*_vct_usearea.bsq")
  
  ;---batch process the mask images---
  for i=0, n_elements(m_files)-1 do begin
    vct = file_basename(m_files[i])
    print, ">>> processing: ", vct
    
    ;---convert month-day in the file names to julian day so we can get the names into LT convention---
    year = strmid(vct,10,4)
    month = strmid(vct,14,2)
    day = strmid(vct,16,2)
    julday = strtrim(string(ymd2dn(year,month,day)),2)
    
    ;---pull out pieces of the vct output file names---
    vct_substr = {year:strmid(vct,10,4), sensor_nbr:strmid(vct,19,1), $
      path:strmid(vct,1,2), row:strmid(vct,4,2)}
      
    if vct_substr.sensor_nbr eq 7 then sensor_name = 'LE' else sensor_name = 'LT'
    
    ;---create lt mask outputs---
    ;---reproject the vct mask to albers---
    
    ;delete any temp files in the folder
    
    ;make an output name
    part1 = sensor_name+vct_substr.sensor_nbr+'0'+vct_substr.path+'0'+vct_substr.row+'_'+vct_substr.year+'_'+julday
    part2 = '_'+timestamp()
    part3 = '_cloudmask.bsq'
    outfilebase = part1+part2+part3
    outfile = strcompress(lt_img_path+vct_substr.year+'\'+outfilebase, /rem)
    exists = file_search(lt_img_path+vct_substr.year+'\', strcompress("*"+part1+"*"+part3+"*", /rem), count=n_exists)
    if n_exists eq 1 then if file_test(exists) eq 1 then begin
      print, ">>>   skipping because it already exists"
      continue
    endif
    
    print, "  reprojecting vct mask"
    
    
    ;delete old ones
    lt_delete_duplicate, outfile, /cloudmask
    
    dir = strcompress(lt_img_path+vct_substr.year+'\', /rem)
    tempfiles = file_search(dir, "*temp*", count=count)
    if count ge 1 then file_delete, tempfiles, /quiet
    
    temp_out = strcompress(lt_img_path+vct_substr.year+'\temp.bsq', /rem)
    
    filedir = file_dirname(outfile)+"\"
    searchforbase = sensor_name+vct_substr.sensor_nbr+'0'+vct_substr.path+'0'+vct_substr.row+'_'+vct_substr.year+'_'+julday
    searchfor = strcompress("*"+searchforbase+"*archv.bsq", /rem)
    ;---find the raw image file, read it in, and subset it---
    archvfile = file_search(filedir, searchfor, count=n_archvfile)
    if n_archvfile ne 1 then begin
      ;try a different name
      searchfor = strcompress("*"+searchforbase+"*archv_orig.bsq", /rem)
      archvfile = file_search(filedir, searchfor, count=n_archvfile)
      if n_archvfile ne 1 then message, "archive file not found"
    endif
    
    ;---create command to project image using gdalwarp---
    s1 = "gdalwarp -t_srs " + proj
    s2 = " -of ENVI -tr 30 30 -overwrite -multi "
    cmd = s1 + s2 + m_files[i] + " " + temp_out
    ;run the command
    spawn, cmd, /hide
    
    ;read the projected vct mask in
    zot_img, temp_out, hdr, img
    
    ul = hdr.upperleftcenter
    lr = hdr.lowerrightcenter
    mastersubset = [[ul],[lr]]
    
    
    ;sieve out small patches of cloud shadow
    shadowedge = img eq 2
    
    temp_shdwmsk = strcompress(lt_img_path+vct_substr.year+'\temp_shdwmsk.bsq', /rem)
    openw, un, temp_shdwmsk, /get_lun
    writeu, un, shadowedge
    free_lun, un
    write_im_hdr, temp_shdwmsk, hdr
    convert_bsq_headers_to_envi, strcompress(lt_img_path+vct_substr.year, /rem), templatehdr
    
    spawn, "gdal_sieve.py -st 6 -8 -of ENVI " + temp_shdwmsk
    
    ;read the sieved cloud shadow image in and merge with the other classes
    subset=mastersubset
    zot_img, temp_shdwmsk, shdwhdr, shdwimg, subset=subset
    
    ;---aggregate the vct classes---
    print, "  aggregating vct classes for cloudmask"
    
    if keyword_set(water_off) eq 1 then clouds = (img eq 0)+(img eq 4)+(img eq 5)+(img eq 7)+shdwimg ;+(img eq 2)+(img eq 3)
    if keyword_set(water_off) eq 1 then print, "  ***warning*** water will NOT be excluded from the image"
    
    if keyword_set(snow_off) eq 1 then clouds = (img eq 0)+(img eq 1)+(img eq 4)+(img eq 5)+shdwimg ;+(img eq 2)+(img eq 3)
    if keyword_set(snow_off) eq 1 then print, "  ***warning*** snow will NOT be excluded from the image"
    
    if keyword_set(water_off) eq 1 and keyword_set(snow_off) eq 1 then clouds = (img eq 0)+(img eq 4)+(img eq 5)+shdwimg ;+(img eq 2)+(img eq 3)
    
    if keyword_set(water_off) ne 1 and keyword_set(snow_off) ne 1 then clouds = (img eq 0)+(img eq 1)+(img eq 4)+(img eq 5)+(img eq 7)+shdwimg  ;+(img eq 2)+(img eq 3)
    if keyword_set(water_off) ne 1 and keyword_set(snow_off) ne 1 then print, "  ***warning*** water and snow WILL be excluded from the image"
    
    img = 0 ;save memory
    
    includedclasses = ["shadow", "cloud", "cloud edge"]  ;, "shadow edge"
    if keyword_set(water_off) ne 1 then includedclasses = [includedclasses, "water"]
    if keyword_set(snow_off) ne 1 then includedclasses = [includedclasses, "snow"]
    includedclasses = strjoin(includedclasses, ", ")
    
    radius = 1
    kernel = SHIFT(DIST(2*radius+1), radius, radius) LE radius
    clouds = convol(temporary(clouds), kernel) gt 0
    clouds = convol(temporary(clouds), kernel) gt 0
    
    ;clouds[where(comarea_mask eq 0)]=1
    clouds = byte(temporary(clouds) eq 0);with this clouds are now 0
    
    ;---check if the image has gaps, if so, create a mask for them and combine it with the cloudmask---
    if vct_substr.year ge 2003 and sensor_name eq 'LE' then begin
      print, "  making gap mask"
      
      subset=mastersubset
      zot_img, archvfile, gaphdr, empties, subset=subset, /hdronly
      
      empties = bytarr(gaphdr.filesize[0], gaphdr.filesize[1])
      for b=0, gaphdr.n_layers-1 do begin
        subset=mastersubset
        zot_img, archvfile, gaphdr, img, subset=subset, layers=(b+1)
        if b eq 0 then empties = temporary(img) ne 0 else $
          empties = temporary(empties)*(img ne 0)
      endfor
      
      ;        outimg = "E:\045027\test\mask_test.bsq"
      ;        openw, unit, outimg, /get_lun
      ;        writeu, unit, empties
      ;        free_lun, unit
      ;        gaphdr.n_layers = 1
      ;        gaphdr.pixeltype = 3
      ;        write_im_hdr, outimg, gaphdr
      
      ;empties = product(temporary(empties),3) ;(gapimg[*,*,0] ne 0)*(gapimg[*,*,1] ne 0)*(gapimg[*,*,2] ne 0)*(gapimg[*,*,3] ne 0)*(gapimg[*,*,4] ne 0)*(gapimg[*,*,5] ne 0)
      
      
      ;---create a 0/1 raster where empties are 1 and goods are 0---
      empties = byte(temporary(empties) eq 0)
      
      radius = 1
      kernel = SHIFT(DIST(2*radius+1), radius, radius) LE radius
      empties = convol(temporary(empties), kernel) gt 0
      
      ;---combine the gap mask and the buffered vct mask pixels and write it out to file---
      empties = ((temporary(empties) eq 0) * clouds); ne 0
      empties = byte(temporary(empties))
      
      ;write out cloudmask
      openw, un, outfile, /get_lun
      writeu, un, empties
      free_lun, un
      hdr.pixeltype = 3
      write_im_hdr, outfile, hdr
      
      ;create the metadata structure
      metaout = stringswap(outfile, ".bsq", "_meta.txt")
      meta = make_metadata_for_preprocessing(outfile, maskedclasses=includedclasses)
      concatenate_metadata, archvfile, metaout, params=meta
      
    endif else begin
    
      ;---if not a gappy image then write out the mask---
      ;write out cloudmask
      openw, un, outfile, /get_lun
      writeu, un, clouds
      free_lun, un
      hdr.pixeltype = 3
      write_im_hdr, outfile, hdr
      
      ;create the metadata structure
      metaout = stringswap(outfile, ".bsq", "_meta.txt")
      meta = make_metadata_for_preprocessing(outfile, maskedclasses=includedclasses)
      concatenate_metadata, archvfile, metaout, params=meta
    endelse
    
    ;---temp file clean up---
    deletethese = file_search(file_dirname(temp_out), "*temp*", count=n_deletethese)
    if n_deletethese ge 1 then file_delete, deletethese, /quiet else message, "cannot find temporary files to delete - could mean that image date did not process correctly"
  
  endfor
  
;  ;if an update, move the mask files to the no-update folder
;  if keyword_set(update) eq 1 then begin
;    ;vct_output_path is OK for updates - it gets redirected up top
;    mask = file_search(vct_output_path, "*mask.bsq", count=maskcount)
;    maskhdr = file_search(vct_output_path, "*mask.hdr", count=maskhdrcount)
;    ;commmask = file_search(vct_output_path, "*commMask*", count=commmaskcount)
;    outdir = strcompress(path+"VCT\outputs\", /rem)
;    if maskcount ge 1 then file_move, mask, outdir
;    if maskhdrcount ge 1 and maskcount eq maskhdrcount then file_move, maskhdr, outdir else begin
;      print, ">>> trying to move the mask files..."
;      print, ">>> from the vct\outputs\update\ folder to the..."
;      print, ">>> vct\outputs\ folder, but the number of mask..."
;      print, ">>> files does not match the number of .hdr files..."
;      print, ">>> go to the below location and figure out what's up"
;      print, "  ", vct_output_path
;    endelse
;    if keyword_set(delete_files) eq 1 then begin
;      ;make sure that we are deleting the update folder contents and not the outputs contents
;      close, /all, /force
;      goodtogo = strmatch(vct_output_path, "*update*")
;      if goodtogo eq 1 then file_delete, vct_output_path, /recursive
;    endif
;  endif
end