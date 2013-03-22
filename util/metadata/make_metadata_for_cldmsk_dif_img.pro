pro make_metadata_for_cldmsk_dif_img, file_name, input_img, ref_img
 
metadata = {data: "cloud\shadow difference image",$
  filename: file_basename(file_name),$
  input_image:file_basename(input_img),$
  cloud_shadow_ref_image:file_basename(ref_img)}
  
  output_metadata_file = stringswap(file_name, ".bsq", "_meta.txt")
  concatenate_metadata, ref_img, output_metadata_file, params=metadata
  
  
;  openw, fun, output_metadata_file, /get_lun
;  printf, fun, convert_struct_to_string(metadata)
;  free_lun, fun

end