pro make_file_mgmt_savefile, path

  ;find glovis tar.gz files in the path+"glovis_targz\" folder
  targz = file_search(path+"glovis_targz\", "*.tar.gz", count=n_targz)
  if n_targz eq 0 then begin
    img_list = file_search(path+"images\", "*source_img_list.txt", count=n_imglist)
    if n_imglist ne 0 then begin
      ;open the image list
      openr, lun, img_list, /get_lun
      file_size = file_lines(img_list,/noexpand_path)
      targz = strarr(file_size)
      readf, lun, targz
      free_lun, lun
      n_targz = n_elements(targz)
    endif else message, "cannot find '*.tar.gz' files or an '*source_img_list.txt' file"
  endif
  
  ;if files were found make all info that is needed to for all files
  
  ;make a structure to hold the info
  file_mgmt = {ltimgdir:"",$
    ltmadcaldir:"",$
    vctimgdir:"",$
    vctprepdir:"",$
    glovisimg:"",$
    ltsensor:"",$
    vctsensor:"",$
    ppp:"",$
    rrr:"",$
    ppprrr:"",$
    doy:"",$
    dom:"",$
    year:"",$
    month:"",$
    ltimgbase:"",$
    vctimgbase:"",$
    unpackedin:"",$
    targzdldate:"",$
    yeardoy:"",$
    ymd:"",$
    nativeproj:"",$
    reproj:"",$
    vctmaskfile:"",$
    cmaskfixed:"no",$
    cloudthresh:0,$
    shadowthresh:0,$
    vctmaskclasses:"na"}
    
  ;todo: add in information in the metadata type (mtl or wo)
  ;todo: add in info on the ephemeris type (predictive or definitive)
  ;todo: add in info on the product type (L1T or other)
    
  ;replicate the structure
  file_mgmt = replicate(file_mgmt, n_targz)
  
  for i=0, n_targz-1 do begin
    ;gather info and place in structure
    file_mgmt[i].ltimgdir = path+"images\"
    file_mgmt[i].ltmadcaldir = path+"madcal\"
    file_mgmt[i].vctimgdir = path+"VCT\outputs\"
    file_mgmt[i].vctprepdir = path+"VCT\prep\"
    file_mgmt[i].glovisimg = targz[i]
    targzbase = file_basename(file_mgmt[i].glovisimg)
    file_mgmt[i].ltsensor = strmid(targzbase,0,3)
    file_mgmt[i].ppp = strmid(targzbase,3,3)
    file_mgmt[i].rrr = strmid(targzbase,6,3)
    file_mgmt[i].ppprrr = file_mgmt[i].ppp+file_mgmt[i].rrr
    file_mgmt[i].year = strmid(targzbase,9,4)
    file_mgmt[i].doy = strmid(targzbase,13,3)
    
    ydn2md,file_mgmt[i].year,file_mgmt[i].doy,m,d
    if m lt 10 then file_mgmt[i].month = strcompress('0'+string(m), /rem) else file_mgmt[i].month = strtrim(string(m),2)
    if d lt 10 then file_mgmt[i].dom = strcompress('0'+string(d), /rem) else file_mgmt[i].dom = strtrim(string(d),2)
    
    if strmid(file_mgmt[i].ltsensor, 2,1) eq '7' then file_mgmt[i].vctsensor = 'L71'
    if strmid(file_mgmt[i].ltsensor, 2,1) eq '5' then file_mgmt[i].vctsensor = 'L5'
    
    file_mgmt[i].ltimgbase = strcompress(file_mgmt[i].ltsensor+file_mgmt[i].ppprrr+"_"+file_mgmt[i].year+$
      "_"+file_mgmt[i].doy, /rem)
      
    file_mgmt[i].vctimgbase = strcompress(file_mgmt[i].ppprrr+"_"+file_mgmt[i].rrr+file_mgmt[i].year+file_mgmt[i].month+$
      file_mgmt[i].dom+file_mgmt[i].vctsensor)
      
    vct_img_list = file_search(path+"images\", "*vct_img_list.txt", count=n_vct_img_list)
    if n_vct_img_list eq 1 then begin
      openr, lun, vct_img_list, /get_lun
      file_size = file_lines(vct_img_list,/noexpand_path)
      vctfiles = strarr(file_size)
      readf, lun, vctfiles
      free_lun, lun
      vctimg = where(vctfiles eq file_mgmt[i].vctimgbase, n_vctimg)
      if n_vctimg ge 1 then file_mgmt[i].unpackedin = "vct" else file_mgmt[i].unpackedin = "lt"
    endif else begin
      vctimg = file_search(file_mgmt[i].vctimgdir+file_mgmt[i].vctimgbase, count=n_vctimg)
      if n_vctimg ge 1 then file_mgmt[i].unpackedin = "vct" else file_mgmt[i].unpackedin = "lt"
    endelse
    
    
    downdate = file_info(file_mgmt[i].glovisimg)
    file_mgmt[i].targzdldate = systime(0,downdate.ctime)
    
    file_mgmt[i].yeardoy = file_mgmt[i].year+"_"+file_mgmt[i].doy
    file_mgmt[i].ymd = file_mgmt[i].year+file_mgmt[i].month+file_mgmt[i].dom
    
    nativeprojfile = file_search(file_mgmt[i].vctprepdir, file_mgmt[i].ppprrr+"_native_proj.txt", count=n_nativeprojfile)
    reprojfile = file_search(file_mgmt[i].vctprepdir, "*projection.txt", count=n_reprojfile)
    file_mgmt[i].vctmaskfile =  file_search(file_mgmt[i].vctimgdir, file_mgmt[i].vctimgbase+"_mask.bsq", count=n_vctmaskfile)
    
    
    if n_nativeprojfile eq 1 then begin
      ;open the image info and extract the projection information for the native glovis image
      openr, lun, nativeprojfile, /get_lun
      file_size = file_lines(nativeprojfile,/noexpand_path)
      file_list = strarr(file_size)
      readf, lun, file_list
      free_lun, lun
      ;read through the file and pull out the projection lines
      start = where(strmatch(file_list, "*Coordinate System is:*")) + 1
      finish = where(strmatch(file_list, "*Origin*")) - 1
      thegoods = file_list[start:finish]
      file_mgmt[i].nativeproj = strcompress(strjoin(thegoods))
    endif
    
    
    if n_reprojfile eq 1 then begin
      ;open the image info and extract the projection information for the native glovis image
      openr, lun, reprojfile, /get_lun
      file_size = file_lines(reprojfile,/noexpand_path)
      file_list = strarr(file_size)
      readf, lun, file_list
      free_lun, lun
      ;read through the file and pull out the projection lines
      start = where(strmatch(file_list, "*Coordinate System is:*")) + 1
      finish = where(strmatch(file_list, "*Origin*")) - 1
      thegoods = file_list[start:finish]
      file_mgmt[i].reproj = strcompress(strjoin(thegoods))
    endif
  endfor
  ;write out the file_mgmt structure as a .sav file
  save, file_mgmt, filename = strcompress(file_mgmt[0].ltimgdir+"landtrendr_file_mgmt_"+file_mgmt[0].ppprrr+".sav", /rem)
end