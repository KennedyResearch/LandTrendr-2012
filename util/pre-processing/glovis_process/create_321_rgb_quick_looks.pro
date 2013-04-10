pro create_321_rgb_quick_looks, path, ppprrr, update=update

  ;make sure an output directory exists for putting the quicklook images
  file_mkdir, path+"images\321rgb_quicklooks"
  
  ;set the search path - paths differ depending on whether it is an update or not
  if keyword_set(update) eq 1 then begin
    vct_outputs = path+"VCT\outputs\update\"
    glovis_targz = path+"glovis_targz\update\"
  endif else begin
    vct_outputs = path+"VCT\outputs\"
    glovis_targz = path+"glovis_targz\"
  endelse
    
  ;find the vct image files
  image_files = file_search(vct_outputs, "*L*[4571]")
  image_files_base = file_basename(image_files)
  
  ;find the year and julian day
  year = strmid(image_files_base,10,4)
  unique_years = year[uniq(year, sort(year))]
  
  ;start a list of images to process
  final_goods = [""]

  for i=0, n_elements(unique_years)-1 do begin
    year_files = where(strmatch(image_files, strcompress("*"+unique_years[i]+"*", /rem)) eq 1)
    yearset = image_files[year_files]
    
    year = strmid(file_basename(yearset),10,4)
    month = strmid(file_basename(yearset),14,2)
    day = strmid(file_basename(yearset),16,2)
    julday = strtrim(string(ymd2dn(year,month,day)),2)
    yearday = year+"_"+julday
    for j=0, n_elements(yearset)-1 do yearday[j] = string(j)+": "+yearday[j]+","
    yearday = strjoin(yearday, /single)
    
    title = yearday
    reload:
    window, 0, xsize=1260, ysize=945, title=title
    for f=0, n_elements(yearset)-1 do begin
      print, ">>> loading image: ", strcompress(string(f+1)+"/"+string(n_elements(yearset)), /rem)+" for "+year[0]+" please wait..."
      
      if f eq 0 then checkit = string(sindgen(n_elements(yearset))) 
      
      zot_img, yearset[f], hdr, img, layers = [3,2,1]
      
      maxsize = float(max([hdr.filesize[0],hdr.filesize[1]]))
      denom = float(315/maxsize)
      xsize = round(hdr.filesize[0]*denom)
      ysize = round(hdr.filesize[1]*denom)
      
      img = reverse(hist_equal(congrid(img, xsize, ysize, 3),  minv=10, maxv=175),2) ;,  minv=10, maxv=175
      tv, img, f, true=3
    endfor
    
    repeat begin
      wait,5
      reask:
      picks = ''
      read, picks, prompt = ">>> please type the images you want ex. 0,1,3:"
      thepicks = strsplit(picks, ",", /extract)
      for k=0, n_elements(thepicks)-1 do begin
        ok = where(strmatch(checkit,strcompress("*"+thepicks[k]+"*", /rem)) eq 1, n_ok)
        if n_ok ne 1 and thepicks[k] ne "reload" then begin
          print, ">>> !!!warning!!! the typed entry: "+thepicks[k]+" is not valid, try again"
          goto, reask
        endif
      endfor
      if picks eq "reload" then begin
        wdelete
        goto, reload
      endif
      check = ''
      read, check, prompt = ">>> are these picks ok: "+string(picks)+" (use 1 for no and 2 for yes):"
    endrep until check eq '2'
    
    picks = strsplit(picks, ",", /extract)
    picks = fix(picks)
    if n_elements(picks) eq 1 then picks = picks[0]
    
    ;put the goods into the final holder
    yearsetpicks = yearset[picks]
    final_goods = [final_goods,yearsetpicks]
    
    ;write out the images as a tif
    tiff_out_file = path+"images\321rgb_quicklooks\"+ppprrr+"_"+year[0]+'_321rgb_quicklooks.tif'
    xyouts, .7, .2, title, /norm, color = '0000ff'xl
    tiffit = tvrd(/true) ;get the window as a variable
    write_tiff, tiff_out_file, reverse(tiffit,3) ;write out the combined tiffs
    wdelete
    
    note = "the following images were selected for processing:"
    fulltext = transpose([note, yearsetpicks+" , "+string(i)])
    outfile = path+"documentation\"+ppprrr+"_images_selected_for_processing.txt
    if file_exists(outfile) eq 1 then begin
      ;read in the file and append the new info
      openr, lun, outfile, /get_lun
      file_size = file_lines(outfile,/noexpand_path)
      file_list = strarr(file_size)
      readf, lun, file_list
      free_lun, lun
      fulltext = transpose([file_list,yearsetpicks+" , "+string(i)])
    endif
    openw, lun,outfile,/get_lun
    printf, lun,fulltext
    free_lun, lun
    
  endfor
  final_goods = final_goods[1:*] ;get rid of the dummy starter
  processing_img_list = strcompress(path+"images\"+ppprrr+"_processing_img_list.sav", /rem)
  save, final_goods, filename = processing_img_list
  
  ;delete the unselected images
  delete_unselected_images, path, ppprrr, update=update
     
end
