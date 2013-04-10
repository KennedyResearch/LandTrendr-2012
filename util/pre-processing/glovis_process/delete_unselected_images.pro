pro delete_unselected_images, path, ppprrr, update=update
  
  ;set the search path - paths differ depending on whether it is an update or not
  if keyword_set(update) eq 1 then begin
    vct_outputs = path+"VCT\outputs\update\"
    glovis_targz = path+"glovis_targz\update\"
  endif else begin
    vct_outputs = path+"VCT\outputs\"
    glovis_targz = path+"glovis_targz\"
  endelse
    
  doc_path = path+"documentation\"

  goods_file = file_search(doc_path, "*images_selected_for_processing.txt*", count=n_goods_file)
  
  if n_goods_file ge 1 then begin
    openr, lun, goods_file, /get_lun
    file_size = file_lines(goods_file,/noexpand_path)
    file_list = strarr(file_size)
    readf, lun, file_list
    free_lun, lun
    goodslist = where(strmatch(file_list, "*,*") eq 1, n_goodslist)
    
    if n_goodslist ge 1 then begin
      file_list = file_list[goodslist]
      goods = strarr(n_goodslist)
      for i=0, n_goodslist-1 do begin
        split = strsplit(file_list[i], ',', /extract)
        goods[i] = strcompress(split[0], /rem)
      endfor
      
      year = strmid(file_basename(goods),10,4)
      month = strmid(file_basename(goods),14,2)
      day = strmid(file_basename(goods),16,2)
      julday = strtrim(string(ymd2dn(year,month,day)),2)
      
      vctcheck = year+month+day
      vctcheck = vctcheck[uniq(vctcheck, sort(vctcheck))]
    endif
    
    ;find the vct images
    vct_image_files = file_search(vct_outputs, "*L*[4571]*")
    
    for i=0, n_elements(vctcheck)-1 do begin
      search = strcompress("*"+vctcheck[i]+"*")
      print, "checking: ",search
      bad_vct_files = where(strmatch(vct_image_files, search) ne 1, n_vct_bads)
      if n_vct_bads ge 1 then vct_image_files = vct_image_files[bad_vct_files]
    endfor
    
    image_file = where(strmatch(vct_image_files, "*L*[4571]") eq 1)
    image_file= vct_image_files[image_file]
    year = strmid(file_basename(image_file),10,4)
    month = strmid(file_basename(image_file),14,2)
    day = strmid(file_basename(image_file),16,2)
    julday = strtrim(string(ymd2dn(year,month,day)),2)
    
    
    ;glovis_files= strarr(n_elements(julday))
    glovis_files=[""]
    for i=0, n_elements(julday)-1 do begin
      search = strcompress("*"+year[i]+julday[i]+"*", /rem)
      finds = file_search(glovis_targz, search, count=n_finds)
      if n_finds ge 1 then glovis_files = [glovis_files, finds] else message, "no glovis file for: "+search 
    endfor
    glovis_files = glovis_files[1:*]
    
    deletenote = glovis_files+" because it was not selected for processing"
    outfile = path+"documentation\"+ppprrr+"_images_not_processed.txt"
    if file_exists(outfile) eq 1 then begin
      ;read in the file and append the new info
      openr, lun, outfile, /get_lun
      file_size = file_lines(outfile,/noexpand_path)
      file_list = strarr(file_size)
      readf, lun, file_list
      free_lun, lun
      fulltext = transpose([file_list,deletenote])
      openw, lun,outfile,/get_lun
      printf, lun,fulltext
      free_lun, lun
    endif else message, "images not selected for processing .txt list does not exist"

    file_delete, vct_image_files, glovis_files
  endif
end