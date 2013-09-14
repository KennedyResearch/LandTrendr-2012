pro create_history_variables, diagfile, template_hdr, maskyes=maskyes, recovery_mask_override=recovery_mask_override


;------ identify path separator -------
  pse = path_sep()

	;maskyes uses the disturbance and recovery maps to filter -- only nondisturbed
	; and nonrecovering pixels change values, all others are static. 

  if n_elements(maskyes) eq 0 then maskyes = 0 else maskyes = 1

  for i=0, n_elements(diagfile)-1 do begin
    print, "making history variables for: ", diagfile[i]
    restore, diagfile[i]
    years = diag_info.image_info.year
    start_year = min(years)
    end_year = max(years)
    dir = file_dirname(diagfile)
    outdir = dir+pse+"history"+pse
    if file_test(outdir) eq 0 then file_mkdir, outdir
    output_corename = outdir+pse+stringswap(file_basename(diagfile[i]), "_diag.sav", "")    
    
    ;index = diag_info.index
    ;tsa = strsplit(file_basename(diagfile[i]),"_", /extract)
    ;tsa = tsa[3]
    ;corename = stringswap(file_basename(diagfile[i]), "_diag.sav", "")
    ;output_corename = outdir + index + pse + tsa + pse + corename
    ;file_mkdir, outdir + pse + index + pse + tsa
    
    
    ok = lt_history_metrics(diagfile[i], maskyes=maskyes, end_year=end_year, $
		start_year=start_year, output_corename=output_corename, $
		recovery_mask_override=recovery_mask_override);, subset=[[-2043945.0000,2878005.0000],[-2013495.0000, 2843955.0000]]
 
 
 
    convert_bsq_headers_to_envi, outdir, template_hdr
    
    ;add the years to the outputs and add metadata
    hist_info=ok.hist_info
    yrs = hist_info.start_year+indgen(hist_info.n_years)  

n_outputs = n_elements(hist_info.output_image_group)
    for j=0, n_outputs-1 do begin
       this_image=hist_info.core_name+hist_info.output_image_group[j]
	ok=add_years_to_any_image(this_image, yrs)
       if ok.ok eq 0 then print, "Problem in adding years:" + ok.msg
     end ;j number of outputss
  endfor  ;diag files
end
