;****************************************************************************
;Copyright © 2008-2011 Oregon State University
;All Rights Reserved.
;
;
;Permission to use, copy, modify, and distribute this software and its
;documentation for educational, research and non-profit purposes, without
;fee, and without a written agreement is hereby granted, provided that the
;above copyright notice, this paragraph and the following three paragraphs
;appear in all copies.
;
;
;Permission to incorporate this software into commercial products may be
;obtained by contacting Oregon State University Office of Technology Transfer.
;
;
;This software program and documentation are copyrighted by Oregon State
;University. The software program and documentation are supplied "as is",
;without any accompanying services from Oregon State University. OSU does not
;warrant that the operation of the program will be uninterrupted or
;error-free. The end-user understands that the program was developed for
;research purposes and is advised not to rely exclusively on the program for
;any reason.
;
;
;IN NO EVENT SHALL OREGON STATE UNIVERSITY BE LIABLE TO ANY PARTY FOR DIRECT,
;INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST
;PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
;IF OREGON STATE UNIVERSITYHAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
;DAMAGE. OREGON STATE UNIVERSITY SPECIFICALLY DISCLAIMS ANY WARRANTIES,
;INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
;FITNESS FOR A PARTICULAR PURPOSE AND ANY STATUTORY WARRANTY OF
;NON-INFRINGEMENT. THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS,
;AND OREGON STATE UNIVERSITY HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE,
;SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
;
;****************************************************************************

pro envi_glovis_to_landtrendr, path, proj_ref_file, overwrite=overwrite, skipthermal=skipthermal, $
    pixelSize=pixelSize, from_auto=from_auto, update=update, file_cleanup=file_cleanup
    
  ;create some path variables
  inpath = strcompress(path+"glovis_targz\tar\", /rem)
  outpath = strcompress(path+'images\', /rem)
  
  drive = strmid(path, 0, 2)
  length = strlen(path)
  pdrive = strmid(path, 2, length-2)
  
  ;decompress the .gz files
  ;figure out if this file has been decompressed for VCT already
  if keyword_set(update) eq 0 then gzfull = file_search(strcompress(path+"glovis_targz", /rem), "*tar.gz", count=count) $
  else gzfull = file_search(strcompress(path+"glovis_targz\update", /rem), "*.gz", count=count)
  
  ;gzfull = file_search(strcompress(path+"glovis_targz", /rem), "*tar.gz", count=count)
  
  
  ;decompress the .gz files
  if count ge 1 then begin
    gz = file_basename(gzfull)
    glovis = replicate(create_struct("sensor","","ppp","","rrr","","year","",$
      "julday","", "day",31, "day1","","month",12,"month1","","glovis","", "glovis_update","",$
      "glovis_update_hdr","","glovis_update_mtl","",$
      "vct","","sensor1","","tarpath","","gzpath","", "gzpathupdate","", "mtl",""), count)
    for i=0, count-1 do begin
      glovis[i].sensor = strmid(gz[i],0,3)
      glovis[i].ppp = strmid(gz[i],3,3)
      glovis[i].rrr = strmid(gz[i],6,3)
      glovis[i].year = strmid(gz[i],9,4)
      glovis[i].julday = strmid(gz[i],13,3)
      glovis[i].sensor1 = strmid(gz[i],1,2)
      glovis[i].tarpath = strcompress(path+"glovis_targz\tar\"+file_basename(gz[i]), /rem)
      glovis[i].gzpath = gzfull[i]
      if keyword_set(update) eq 1 then glovis[i].gzpathupdate = strcompress(path+"glovis_targz\"+file_basename(gz[i]), /rem)
      
      if strmid(glovis[i].sensor, 2,1) eq '7' then glovis[i].sensor = 'L71'
      if strmid(glovis[i].sensor, 2,1) eq '5' then glovis[i].sensor = 'L5'
      if strmid(glovis[i].sensor, 2,1) eq '4' then glovis[i].sensor = 'L4'
      
      ydn2md,glovis[i].year,glovis[i].julday,m,d
      glovis[i].month = m
      glovis[i].day = d
      
      if glovis[i].month lt 10 then glovis[i].month1 = strcompress('0'+string(glovis[i].month), /rem) else glovis[i].month1 = string(glovis[i].month)
      if glovis[i].day lt 10 then glovis[i].day1 = strcompress('0'+string(glovis[i].day), /rem) else glovis[i].day1 = string(glovis[i].day)
      
      glovis[i].glovis = strcompress(path+"VCT\outputs\"+glovis[i].ppp+glovis[i].rrr+"_"+glovis[i].rrr+glovis[i].year+ $
        string(glovis[i].month1)+string(glovis[i].day1)+glovis[i].sensor, /rem)
        
      glovis[i].glovis_update = strcompress(path+"VCT\outputs\update\"+glovis[i].ppp+glovis[i].rrr+"_"+glovis[i].rrr+glovis[i].year+ $
        string(glovis[i].month1)+string(glovis[i].day1)+glovis[i].sensor, /rem)
        
      glovis[i].glovis_update_hdr = strcompress(path+"VCT\outputs\update\"+glovis[i].ppp+glovis[i].rrr+"_"+glovis[i].rrr+glovis[i].year+ $
        string(glovis[i].month1)+string(glovis[i].day1)+glovis[i].sensor+".hdr", /rem)
        
      glovis[i].glovis_update_mtl = strcompress(path+"VCT\outputs\update\"+glovis[i].ppp+glovis[i].rrr+"_"+glovis[i].rrr+glovis[i].year+ $
        string(glovis[i].month1)+string(glovis[i].day1)+glovis[i].sensor+"_mtl.txt", /rem)
        
      glovis[i].mtl = strcompress(glovis[i].glovis+"_MTL.txt", /rem)
      
      ;check if the file exists in the VCT folder
      ;if keyword_set(update) ne 1 then glovis[i].vct = file_test(glovis[i].glovis) else $   ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!file_exists(glovis[i].glovis)
      ;  glovis[i].vct = file_test(glovis[i].glovis_update) ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!file_exists(glovis[i].glovis_update)
      
      glovis[i].vct = file_test(glovis[i].glovis)
      
      ;if the file DOES exists in the vct folder then use that one instead of unpacking the file again
      if glovis[i].vct eq 1 then begin
        ;if an update, move the .tar.gz file out of update folder and into the glovis_targz folder since we don't need it
        if keyword_Set(update) eq 1 then file_move, glovis[i].gzpath, glovis[i].gzpathupdate
        outsubpath = strcompress(outpath+glovis[i].year+"\", /rem)
        ; Create outpath if it doesn't exist already
        if file_test(outSubPath, /directory) ne 1 then file_mkdir, outsubpath
        outfilebase='L'+glovis[i].sensor1+glovis[i].ppp+glovis[i].rrr+'_'+glovis[i].year+'_'+glovis[i].julday
        mtloutfile = strcompress(outSubPath+outfilebase+'_MTL.txt', /rem)
        
        ;create an output name
        ;time = string(bin_date(systime()), format='(I4,I02,I02,"_",I02,I02,I02)')
        time = timestamp()
        outfile = strcompress(outSubPath+outfilebase+"_"+time+'_archv.bsq', /rem)
        outtfile = strcompress(outSubPath+outfilebase+"_"+time+'_b6.bsq', /rem)
        
        exists = file_search(outsubpath, strcompress("*"+outfilebase+"*archv.bsq", /rem), count=n_exists)
        if n_exists eq 0 then begin
          isradref = file_search(outsubpath, "*radref.bsq", count=n_isradref)
          if n_isradref eq 1 then begin
            if file_test(isradref) eq 1 then begin
              print, ">>>   skipping because it is the radiometric reference image"
              continue
            endif
          endif
        endif
        if n_exists eq 1 then begin
        
          if file_test(exists) eq 1 then begin
            print, ">>>   skipping because it already exists"
            continue
          endif
        endif
        
        ;if this date already exists delete everything but the meta data file
        lt_delete_duplicate, outfile, /archv, /b6thermal
        
        ;find and move the mlt metadata file
        ;        if keyword_set(update) ne 1 then file_copy, glovis[i].mtl, mtloutfile, /overwrite else begin
        ;          file_copy, glovis[i].glovis_update_mtl, strcompress(path+"VCT\outputs\", /rem), /overwrite ;put a copy of the mtl header in the VCT\outputs folder
        ;          file_copy, glovis[i].glovis_update_mtl, mtloutfile, /overwrite ;put a copy of the mtl header in the image\year folder
        ;        endelse
        
        file_copy, glovis[i].mtl, mtloutfile, /overwrite
        
        ;get the reference image projection
        proj = file_search(path+"VCT\prep\", "*ref_img_projection.prf", count=n_proj)
        if n_proj eq 0 then begin
          prf_outfile=strcompress(path+"VCT\prep\"+glovis[i].ppp+glovis[i].rrr+"_ref_img_projection.prf", /rem)
          proj = gdal_get_projection(proj_ref_file, prf_outfile=prf_outfile)
        endif
        
        ;check the header for correct number of bands
        hdr = gdal_get_hdr_info(glovis[i].glovis)
        if hdr.n_layers lt 6 or hdr.n_layers gt 7 then begin
          print, ">>> the file: "
          print, outfile
          print, ">>> has ", hdr.n_layers, " bands"
          print, ">>> it should have 6 or 7 depending on the thermal layer"
          stop
        endif
        
        ;if the number of bands is seven then subset and reproject the thermal band then the others
        if hdr.n_layers eq 7 then begin
          ;if keyword_set(update) ne 1 then doitfile = glovis[i].glovis else doitfile = glovis[i].glovis_update
        
          doitfile = glovis[i].glovis
          
          ;reproject bands VCT glovis file
          print, ">>> reprojecting file: ", doitfile
          s1 = "gdalwarp -t_srs " + proj[0]
          s2 = " -of ENVI -tr 30 30 -overwrite -multi "
          
          outfile_temp = file_dirname(outfile)+"\temp.bsq"
          cmd = s1 + s2 + doitfile + " " + outfile_temp
          if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
          spawn, cmd
          
          ;subset and write out b6 thermal
          print, ">>> subsetting band 6 thermal for: ", doitfile
          s1 = "gdal_translate -ot Byte -of ENVI -b 6 "
          cmd = s1 + outfile_temp + " " + outtfile
          if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
          spawn, cmd
          
          ;write out the metadata for thermal
          meta = make_metadata_for_preprocessing(outtfile)
          
          ;subset and write out bands 1,2,3,4,5,7
          print, ">>> subsetting bands 1,2,3,4,5,7 for file: ", doitfile
          s1 = "gdal_translate -ot Byte -of ENVI -b 1 -b 2 -b 3 -b 4 -b 5 -b 7 "
          cmd = s1 + outfile_temp + " " + outfile
          if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
          spawn, cmd
          
          ;write out the metadata for bands 1,2,3,4,5,7
          meta = make_metadata_for_preprocessing(outfile)
          
          checkit = file_exists(outfile)+file_exists(outtfile)
          
          ;if an update, move the VCT file from the VCT\outputs\update folder to the VCT\outputs folder
          ;but first make sure that all the new files exist
          if checkit eq 2 then begin
            temps = file_search(file_dirname(outfile),"*temp*",count=n_temps)
            if n_temps ne 0 then file_delete, temps
          ;            if keyword_set(update) eq 1 then begin
          ;              file_move, glovis[i].glovis_update, glovis[i].glovis, /overwrite
          ;              file_move, glovis[i].glovis_update_hdr, glovis[i].mtl, /overwrite
          ;            endif
          endif else message, "something did not go right when while subsetting bands and reprojecting"
        endif  ;hdr.n_layers eq 7
      endif ; if files in vct folder
      
      ;if the .tar.gz file was not processed by VCT then decompress the .tar.gz file into the tar folder
      file_mkdir, strcompress(drive+pdrive+"glovis_targz\tar", /rem) ;make sure the tar folder exists
      if glovis[i].vct eq 0 then begin
        if keyword_set(update) eq 1 then folder = "glovis_targz\update\" else folder = "glovis_targz\"
        cmd = strcompress(pdrive + "glovis_targz\7za", /rem)+ " x " + $  ;"VCT\prep\7za"
          strcompress(pdrive+folder+file_basename(glovis[i].gzpath), /rem) + " " + strcompress("-o"+drive+pdrive+"glovis_targz\tar", /rem)
        cmd = drive+ " && " + cmd
        if !Version.(1) eq "Win32" then cmd = stringswap(cmd, "/", "\")
        spawn, cmd, /hide
        ;move the tar.gz file out of the update folder if it is an update
        if keyword_set(update) eq 1 then file_move, glovis[i].gzpath, path+"glovis_targz\"
      endif
    endfor  ;for gzfile count
  endif ;if gzfile count ge 1
  
  ;if there were files not processed by VCT then go about the unpacking, stacking, and projecting
  winning = where(glovis.vct eq 0, n_winning)
  if n_winning ge 1 then begin
    ;now decompress the .tar files
    print, ">>> decompressing .tar glovis files"
    print, "  ..."
    targz_path = strcompress(path+"glovis_targz\tar\", /rem)
    imglist = file_search(targz_path, "*tar")
    for i=0, n_elements(imglist)-1 do begin
      file = imglist[i]
      len = strlen(file)
      outdir = strmid(file, 0, len-4)
      infile = strmid(file, 2, len-2)
      file_mkdir, outdir
      cmd = strcompress(pdrive + "glovis_targz\7za", /rem)+ " e " + $  ;"VCT\prep\7za"
        infile + " " + strcompress("-o"+outdir, /rem)
      cmd = drive+ " && " + cmd
      spawn, cmd, /hide
    endfor
    
    ; make sure path ends with '/'
    if keyword_set(from_auto) eq 0 then begin
      inpath = file_dirname(inpath,/mark_directory)+file_basename(inpath)+'/'
      outpath = file_dirname(outpath,/mark_directory)+file_basename(outpath)+'/'
    endif
    
    ; get archive directories
    dirnames = file_search(inpath,'L*', /fold_case, /test_directory,  /mark_directory, count=n_dirnames)
    
    ;error message if there was no glovis archive directories
    if n_dirnames lt 1 then begin
      print, "there are no glovis archive directories at this location"
      print, inpath
      stop
    endif
    
    ;start the unpacking, stacking , reprojecting
    print, ">>> unpacking, stacking, and reprojecting:
    for i=0, n_dirnames-1 do begin
      print, "  ",file_basename(dirnames[i])
      ok = envi_glovis_to_landtrendr_doit(dirnames[i], outpath,overwrite=overwrite, $
        proj=proj, skipThermal=skipThermal,pixelSize=pixelSize)
      if ok.ok eq 1 then begin
        print, ok.message
        print, ok.warnings
        ;clean up all files except the glovis .tar.gz files
        if keyword_set(file_cleanup) eq 1 then begin
          tarfilelen = strlen(dirnames[i])
          tarfile = strcompress(strmid(dirnames[i], 0, tarfilelen-1)+".tar", /rem)
          if file_test(tarfile) eq 1 then file_delete, tarfile
          file_delete, dirnames[i], /recursive
        endif
      endif else print, '>> '+ok.error
    endfor ;for n_archive files do the stacking and reprojecting
  endif ;if files were not processed by VCT
end