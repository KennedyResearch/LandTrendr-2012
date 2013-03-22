pro make_metadata_for_cldmsk_ref_img, image_list, mask_list, file_name

image_list = file_basename(image_list)
mask_list = file_basename(mask_list)

new_imglist = ","+image_list
new_imglist[0] = image_list[0] 
new_msklist = ","+mask_list
new_msklist[0] = mask_list[0]  

metadata = {data: "cloud\shadow reference image",$
  filename: file_basename(file_name),$
  source_images:new_imglist,$
  source_masks:new_msklist}

  output_metadata_file = stringswap(file_name, ".bsq", "_meta.txt")
  openw, fun, output_metadata_file, /get_lun
  printf, fun, convert_struct_to_string(metadata)
  free_lun, fun

end