pro create_madcal_summary_pdf, path
  ;madcal path
  madcalpath = path+"madcal\"
  
  ;find the madcal .sav file
  summary_file = file_search(madcalpath, "*madcal_results.sav", count=n_summary_files)
  
  ;find the summary script
  summary_script = file_search(madcalpath, "madcal_results_plotter.r", count=n_summary_script)
  
  doit = n_summary_files+n_summary_script
  
  if doit eq 2 then begin
    restore, summary_file
    output_csv_file = stringswap(summary_file, "sav", "csv")
    export_structure_to_file, norm_info, output_csv_file
    
    ;open the summary script and edit it
    openr, lun, summary_script, /get_lun
    file_size = file_lines(summary_script,/noexpand_path)
    file_list = strarr(file_size)
    readf, lun, file_list
    free_lun, lun
    
    outdirline = where(strmatch(file_list, "*outdir =*") eq 1, n_outdir)
    if n_outdir eq 1 then begin
      putthisin = 'outdir = '+ '"'+stringswap(madcalpath, "\", "/")+'"'
      
      file_list[outdirline] = putthisin
    endif
    
    srcfileline = where(strmatch(file_list, "*src_file =*") eq 1, n_srcfileline)
    if n_srcfileline eq 1 then begin
      putthisin = "src_file = "+ '"'+stringswap(output_csv_file, "\", "/")+'"'
      file_list[srcfileline] = putthisin
    endif
    
    openw, lun,summary_script,/get_lun
    printf, lun,transpose(file_list)
    free_lun, lun
    
    cmd = "rscript "+'"'+summary_script[0]+'"'
    spawn, cmd;, /log_output
    
  endif
end