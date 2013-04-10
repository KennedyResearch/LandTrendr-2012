
;this prgram breaks up the original VCT iamge list so that there is only 
;one date per year - multiple new lists are created.  the lists are then run
;through vct_clipmasktoa.pro to clip them, create a mask, and toa images
 
pro vct_img_list_splitter, path, ppprrr, update=update, singles=singles, delete_files=delete_files

  print, "breaking up the original image list"
  ;---set up some path variables from the given info---
  ppp = strmid(ppprrr, 0, 3) ;'046'
  rrr = strmid(ppprrr, 3, 3) ;'026
  pr = strcompress("p"+ppp+"r"+rrr ,/rem) ;'p046r026
  if keyword_set(update) eq 1 then vct_output_path = strcompress(path+"VCT\outputs\update\", /rem) else $
    vct_output_path = strcompress(path+"VCT\outputs\", /rem)
  vct_ancdata_path = strcompress(path+"VCT\ancData\", /rem)
  
  ;ancdata stuff
  dem_name = strcompress("dem_"+pr, /rem)
  dem_path = strcompress(vct_ancdata_path+dem_name, /rem) ;"E:\2528\VCT\ancData\dem_p25r28"
  landcover_name = strcompress("lc_"+pr, /rem)
  landcover_path = strcompress(vct_ancdata_path+landcover_name, /rem) ;"E:\2528\VCT\ancData\lc_p25r28"
  
  ;---find the vct image list that was just made---
  vct_img_list = strcompress(vct_output_path+pr+"_imagelist.txt", /rem) ;  vct_img_list = "F:\4626\VCT\junk\p046r026_imagelist.txt"
  
  ;---read in the image list---
  openr, lun, vct_img_list, /get_lun
  file_size = file_lines(vct_img_list,/noexpand_path)
  file_list = strarr(file_size)
  readf, lun, file_list
  free_lun, lun
  
  ;---pull out the non-image stuff from the imagelist and hold on to it so it can be put back in later---
  header1 = file_list[0]
  header2 = file_list[1]
  headers = [header1, header2]
  footer1 = file_list[((file_size-1)-3)]
  footer2 = file_list[((file_size-1)-2)]
  footers = [footer1, footer2, dem_path, landcover_path]
  
  ;---pull out the images and put them in a list---
  images = "dummy" ; get a file holder started
  for i=0, n_elements(file_list)-1 do begin
    char1 = strmid(file_list[i],0,1) 
    if char1 eq '0' or char1 eq '1' or char1 eq '2' then images = [images, file_list[i]]
  endfor

  ;---find the number of dates per year---
  images = images[1:*] ; get rid of the dummy element
  years = strmid(images, 10, 4) ;pull out the year from the file
  
  ;create a structure to hold the year and date info for all images
  vct_img_info = create_struct("file", " ", "year", 1984, "n_year", 1, "series", 1)
  vct_img_info = replicate(vct_img_info, n_elements(images))
  
  ;go through the list of images and fill in the file, year, and date per year count 
  for i=0, n_elements(images)-1 do begin
    vct_img_info[i].file = images[i]
    vct_img_info[i].year = strmid(images[i], 10, 4)
    not_used = where(years eq vct_img_info[i].year, count) ;the count    
    vct_img_info[i].n_year = count ;placing the count in the structure
  endfor

  ;fill in the series number per image date per year
  for i=0, n_elements(images)-1 do begin
    for j=0, n_elements(where(vct_img_info.year eq vct_img_info[i].year))-1 do begin
      vct_img_info[i].series = j+1
      i = i+1 ;get the i right for the going through the j section
    endfor
    i = i-1 ;fix the i from the j section
  endfor  
  
 
  series = vct_img_info.series
  max_n_years = max(vct_img_info.n_year)
  
  out_name = file_basename(vct_img_list, ".txt")
  out_dir = file_dirname(vct_img_list)
  imglist_name = strcompress(out_dir+"\"+out_name, /rem) 
  
  vct_clipmasktoa, max_n_years, series, headers, footers, imglist_name, path,$
    ppprrr, images, update=update, singles=singles, delete_files=delete_files

end
  
  