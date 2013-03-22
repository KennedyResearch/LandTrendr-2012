pro vct_mask_fixer_for_al, path_to_images, template_header

  cloud_images = file_search(path_to_images, "*_cloudmask.bsq")
  file_name = file_basename(cloud_images)
  file_dir = file_dirname(cloud_images)
  
  for i=0, n_elements(cloud_images)-1 do begin
    print, cloud_images[i]
  
    year = strmid(file_name[i],10,4)
    sensor = strmid(file_name[i],0,2)
    
    ;do the TM images
    if sensor eq "LT" then begin
      print, "SLC-ON, fixing the mask value" 
      ;do one test image to make sure it works
      if i eq 0 then begin 
        print, "  this one is a test"
        zot_img, cloud_images[i], hdr, img
        img = img*2500
        out = strcompress(file_dir[i] + "\" + "mask_fix_test.bsq", /rem)
        openw, un, out, /get_lun
        writeu, un, img
        free_lun, un
        write_im_hdr, out, hdr
        
        convert_bsq_headers_to_envi, path_to_images, template_header  
        
        print, "*************************************************"
        print, ""
        print, 'open the file: '
        print, "  ", out
        print, "  check and see that the value of clouds are 2500 and background is 0"
        print, "  if OK, type y, else type n"
        print, ""
        a = get_kbrd()
        if a eq 'y' then print, "  cool, good to go!" else message, "  it didn't work, tell justin"
        print, ""
        print, "*************************************************"
        
        flatfile = strcompress(file_dirname(out)+"\"+file_basename(out, ".bsq")+".hdr.flat", /rem)
        file_delete, out, /quiet
        file_delete, flatfile, /quiet
      endif
      
      ;if it worked do the real deal
      zot_img, cloud_images[i], hdr, img
      img = img*2500
      openw, un, cloud_images[i], /get_lun
      writeu, un, img
      free_lun, un
      write_im_hdr, cloud_images[i], hdr    
      flatfile = strcompress(file_dirname(cloud_images[i])+"\"+file_basename(cloud_images[i], ".bsq")+".hdr.flat", /rem)
      file_delete, flatfile, /quiet
    
    endif
    
    ;do the LE7 images ealier than 2003
    if sensor eq "LE" and year lt '2003' then begin
      print, "SLC-ON" 
      zot_img, cloud_images[i], hdr, img
      img = img*2500
      
      openw, un, cloud_images[i], /get_lun
      writeu, un, img
      free_lun, un
      write_im_hdr, cloud_images[i], hdr
      
      flatfile = strcompress(file_dirname(cloud_images[i])+"\"+file_basename(cloud_images[i], ".bsq")+".hdr.flat", /rem)
      file_delete, flatfile, /quiet
    endif  
  endfor
  convert_bsq_headers_to_envi, path_to_images, template_header   
end