pro create_321_rgb_quick_looks1, path, ppprrr, update=update

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
  
  ;create a structure for image selection
  yrday = ["NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"]
  status = ['','','','','','','','','','','','']
  doit = [0,0,0,0,0,0,0,0,0,0,0,0]
  yeargroup = {year:'',file:[''],yearday:yrday, doit:doit, grpindex:0, status:status}
  yeargroup = replicate(yeargroup, n_elements(unique_years))
  
  for i=0, n_elements(unique_years)-1 do begin
    year_files = where(strmatch(image_files, strcompress("*"+unique_years[i]+"*", /rem)) eq 1)
    yeargroup[i].grpindex = i
    
    ;yrday = replicate(yrday, 12)
    yearset = image_files[year_files]
    
    year = strmid(file_basename(yearset),10,4)
    month = strmid(file_basename(yearset),14,2)
    day = strmid(file_basename(yearset),16,2)
    julday = strtrim(string(ymd2dn(year,month,day)),2)
    yearday = year+" "+julday
    ;for h=0, n_elements(yearday)-1 do yrday[h] = yearday[h]
    ;for j=0, n_elements(yearset)-1 do yearday1[j] = string(j)+": "+yearday[j]+","
    ;yearday1 = strjoin(yearday, /single)
    
    window, 0, xsize=1260, ysize=945, title=title
    for f=0, n_elements(yearset)-1 do begin
      print, ">>> loading image: ", strcompress(string(f+1)+"/"+string(n_elements(yearset)), /rem)+" for "+year[0]+" please wait..."
      
      if f eq 0 then checkit = string(sindgen(n_elements(yearset)))
      
      zot_img, yearset[f], hdr, img, layers = [3,2,1]
      
      maxsize = float(max([hdr.filesize[0],hdr.filesize[1]]))
      denom = float(315/maxsize)
      xsize = round(hdr.filesize[0]*denom)
      ysize = round(hdr.filesize[1]*denom)
      
      img = hist_equal(congrid(temporary(img), xsize, ysize, 3),  minv=10, maxv=175) ;,  minv=10, maxv=175
      
      vct = file_basename(yearset[f])     
      year = strmid(vct,10,4)
      month = strmid(vct,14,2)
      day = strmid(vct,16,2)
      julday = strtrim(string(ymd2dn(year,month,day)),2)
      ;---pull out pieces of the vct output file names---
      vct_substr = {year:strmid(vct,10,4), sensor_nbr:strmid(vct,19,1), $
        path:strmid(vct,1,2), row:strmid(vct,4,2)}
  
      if vct_substr.sensor_nbr eq 7 then sensor_name = 'LE' else sensor_name = 'LT' 
      basename = sensor_name+vct_substr.sensor_nbr+'0'+vct_substr.path+'0'+vct_substr.row+ $
        '_'+vct_substr.year+'_'+julday+'_archv_321rgb_quickview.tif'
      
      tiff_out_file = strcompress(path+"images\321rgb_quicklooks\"+basename, /rem)
      write_tiff, tiff_out_file, red=img[*,*,0], green=img[*,*,1], blue=img[*,*,2], PLANARCONFIG=2
      
      img = reverse(temporary(img),2) ;,  minv=10, maxv=175
      tv, img, f, true=3
      
    endfor
    ;write out the images as a tif
    tiff_out_file = path+"images\321rgb_quicklooks\"+ppprrr+"_"+year[0]+'_321rgb_quicklooks.tif'
    ;xyouts, .7, .2, title, /norm, color = '0000ff'xl
    tiffit = tvrd(/true) ;get the window as a variable
    write_tiff, tiff_out_file, reverse(tiffit,3) ;write out the combined tiffs
    wdelete
    
    yeargroup[i].yearday = yearday
    yeargroup[i].year = year[0]
    yeargroup[i].file = yearset[0]
    
    save_out_file = path+"images\321rgb_quicklooks\"+ppprrr+'_321rgb_quicklooks.sav'
    save, yeargroup, filename = save_out_file
  endfor
end
