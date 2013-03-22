pro make_metadata_for_ledaps, path
	;------ identify path separator -------
  		pse = path_sep()

  new_path = path+"images"+pse
  
  ledaps_ref = file_search(new_path, "*ledaps.bsq", count=n_ledaps_ref)
  ledaps_tc = file_search(new_path, "*tc.bsq", count=n_ledaps_tc)
  ledaps_cld = file_search(new_path, "*cloudmask.bsq", count=n_ledaps_cld)
  
  if n_elements(uniq([n_ledaps_ref, n_ledaps_ref, n_ledaps_ref])) ne 1 then message, "# of ref, tc, cloud images do not match"
  
  ledaps_ref_id = strmid(file_basename(ledaps_ref), 10, 8)
  ledaps_tc_id = strmid(file_basename(ledaps_tc), 10, 8)
  ledaps_cld_id = strmid(file_basename(ledaps_cld), 10, 8)
  
  for i=0, n_ledaps_ref-1 do begin
    tcindex = where(ledaps_tc_id eq ledaps_ref_id[i], n_tcindex)
    if n_tcindex ne 0 then tcfile = ledaps_tc[tcindex] else tcfile = "unknown"
    cldindex = where(ledaps_cld_id eq ledaps_ref_id[i], n_cldindex)
    if n_cldindex ne 0 then cldfile = ledaps_cld[cldindex] else cldfile = "unknown"
    
    files = file_basename([ledaps_ref[i],tcfile,cldfile])
    
    ;reflectance
    refmetadata = {data: "ledaps image",$
      filename:files[0],$
      cloudmask_file:files[2],$
      tasseled_cap_file:files[1]}
      
    ;tassled cap
    tcmetadata = {data: "tasseled cap image",$
      filename:files[1],$
      cloudmask_file:files[2],$
      reflectance_file:files[0]}
      
    ;cloud mask
    cldmetadata = {data: "cloud mask image",$
      filename:files[2],$
      reflectance_file:files[0],$
      tasseled_cap_file:files[1]}
      
    ;write out the ;reflectance metadata file
    ref_output_metadata_file = stringswap(ledaps_ref[i], ".bsq", "_meta.txt")
    openw, fun, ref_output_metadata_file, /get_lun
    printf, fun, convert_struct_to_string(refmetadata)
    free_lun, fun
    
    ;write out the tassled cap metadata file
    tc_output_metadata_file = stringswap(tcfile, ".bsq", "_meta.txt")
    openw, fun, tc_output_metadata_file, /get_lun
    printf, fun, convert_struct_to_string(tcmetadata)
    free_lun, fun
    
    ;write out the cloud mask metadata file
    cld_output_metadata_file = stringswap(cldfile, ".bsq", "_meta.txt")
    openw, fun, cld_output_metadata_file, /get_lun
    printf, fun, convert_struct_to_string(cldmetadata)
    free_lun, fun
  endfor
end