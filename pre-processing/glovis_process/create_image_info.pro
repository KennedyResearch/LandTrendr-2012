pro create_image_info, path, filename, useareafile=useareafile
;------ identify path separator -------
  pse = path_sep()

  images_path = strcompress(path+"images"+pse , /rem)
  image_info = find_landtrendr_files(images_path)
  
  ;---establish the use area subset---
  ;find the use area for following procedures - assumes that vct_to_lt_masks has been run
  if keyword_set(useareafile) eq 0 then begin
    studyarea_dir = path+"study_area"+pse
    useareafile = file_search(path+"study_area"+pse, "*vct_usearea.bsq", count=vusecount)
    if vusecount eq 0 then begin
      print, ">>> since a useareafile was not passed..."
      print, ">>> trying to find a '*vct_usearea.bsq' file here:"
      print, "  ", path+"study_area"+pse
      print, ">>> but apparently it's not there, make sure..."
      print, ">>> so filling image info with 'none' instead"
      useareafile = 'none'
    endif
    if vusecount gt 1 then begin
      print, ">>> since a useareafile was not passed..."
      print, ">>> trying to find a '*vct_usearea.bsq' file here:"
      print, "  ", path+"study_area"+pse
      print, ">>> but apparently there are more than 1..."
      print, ">>> it can't be this way, delete extras"
      stop
    endif
  endif
  
  for i = 0, n_elements(image_info)-1 do image_info[i].useareafile = useareafile

  
  ok = find_union_area(image_info, /checkmask, /checkusearea, /isinfo)
  subset2 = ok.coords
  
  for i = 0, n_elements(image_info)-1 do image_info[i].subset = subset2
  
  save, image_info, filename = filename
  
end
