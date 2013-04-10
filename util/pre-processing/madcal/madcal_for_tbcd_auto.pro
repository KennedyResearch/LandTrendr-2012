

;run_params = {ignore: 0, $  ;make sure the background value is the same for all images (mask out if necessary)
;;        subset_size: 1000, $  ;keep between 500 and 1000 unless compelling reason to change
;        subset_coordinates:subset_coordinates, $
;;        min_correlation:min_correlation, $
;        screen_prob: 0.02, $
;        waterthresh:waterthresh, $
;        cloudthresh:cloudthresh, $
;        fix_only_these: [2009221], $    ;set to -1 to run all or [1992233, 2001212] to run only the julian-day 233 image in 1992 and the 212 julian day image in 2001
;        dist_screen: 0.075 $  ;this value is used to eliminate gobs of pixels off-diagonal in the scaterplot of input and reference images
;                    ;  INcrease the number if it seems the scatterplots are getting too truncated.
;         }

function madcal_for_tbcd_auto, image_info, run_params, output_csv_file, $
    norm_method, useareafile=useareafile, apply_to_entire_image=apply_to_entire_image, $
    min_nochange_count=min_nochange_count, eightbit=eightbit
    
  rootdirpath = run_params.path
  
  ;find the reference image stuff
  ref_index = where(image_info.type eq 3, refcount)
  if norm_method eq 2 then begin
    if refcount eq 0 then begin
      print, 'The image to be used for the MADCAL reference '
      print, 'was not found in the image info structure. '
      print, 'Make sure that find_tbcd_files is used to find'
      print, 'files, and that the reference image is an MTBS file'
      print, 'that has been converted to COST and ends in _cost_refl.img'
      return, {ok:-1}
    endif else begin
      refimgfile = image_info[ref_index].image_file
      refcloudfile = image_info[ref_index].cloud_file
      refyear = image_info[ref_index].year    ;for file output name  '_to_'
      refday = image_info[ref_index].julday  ;for file output name  '_to_'
    endelse
  endif
  if norm_method eq 3 then begin
    ;find the radref image in the madcal folder
    refimgfile = file_search(rootdirpath+"madcal\", "*radref.bsq", count=n_refimgfile)
    if n_refimgfile ne 1 then message, "!!!there is no ledaps reference file!!!" else refimgfile = refimgfile
    refcloudfile = file_search(rootdirpath+"madcal\", "*cloudmask.bsq", count=n_refcloudfile)
    if n_refcloudfile ne 1 then message, "!!!there is no ledaps reference cloudmask file!!!" else refcloudfile = refcloudfile
    refyear = strmid(file_basename(refimgfile),10,4)
    refday = strmid(file_basename(refimgfile),15,3)
  endif
  if norm_method eq 1 then begin
    refimgfile = file_search(rootdirpath+"madcal\", "*modis_reference.bsq", count=n_refimgfile)
    if n_refimgfile ne 1 then message, "!!!there is no modis reference file!!!" else refimgfile = refimgfile
    refcloudfile = ""
    refyear = "modis"
    refday = ""
  endif
  
  ;deal with fixing images
  if run_params.fix_only_these[0] ne 0 then begin
    fixes = strcompress(string(run_params.fix_only_these), /rem)
    fixyear = strmid(fixes,0,4)
    fixday = strmid(fixes,4,3)
    search = strcompress("*"+fixyear+"_"+fixday+"*",  /rem)
    for i=0, n_elements(search)-1 do begin
      if i eq 0 then begin
        fixfind = where(strmatch(image_info.image_file, search[i]) eq 1, n_fixfind)
        if n_fixfind eq 1 then fix_index = fixfind else message, "warning! the fix date: ", search[i], " does not exist in the image info structure"
      endif else begin
        fixfind = where(strmatch(image_info.image_file, search[i]) eq 1, n_fixfind)
        if n_fixfind eq 1 then fix_index = [fix_index,fixfind] else message, "warning! the fix date: ", search[i], " does not exist in the image info structure"
      endelse
    endfor
    image_info = image_info[fix_index]
  end
  
  ;setup the base info structure
  madcal_control_info = {reffile:refimgfile, $
    refmask:refcloudfile, $
    depfile:'', $
    depmask:'',$
    ignore:run_params.ignore, $
    run_name:''}
    
  ;replicate the madcal_control_info
  n_all_files = n_elements(image_info)
  madcal_control_info = replicate(madcal_control_info, n_all_files)
  
  ;fill in the info for the reference images
  madcal_control_info.reffile = refimgfile
  madcal_control_info.refmask = refcloudfile
  
  ;create a run name
  if norm_method eq 1 then madcal_control_info.run_name = strcompress('_to_modis', /rem)
  if norm_method eq 2 then madcal_control_info.run_name = strcompress('_to_'+string(refyear)+"_"+string(refday), /rem)
  if norm_method eq 3 then madcal_control_info.run_name = strcompress('_to_ledaps_'+string(refyear[0])+"_"+string(refday[0]), /rem)
  
  ;set up summary structure
  output_summaries = {depfile:'', year:0U, doy:0U, mean_correlation:0.0,$
    successful_run:0, num_subsets:0, $
    b1_slope:0., b1_int:0. , b1_corr:0., $
    b2_slope:0., b2_int:0. , b2_corr:0., $
    b3_slope:0., b3_int:0. , b3_corr:0., $
    b4_slope:0., b4_int:0. , b4_corr:0., $
    b5_slope:0., b5_int:0. , b5_corr:0., $
    b6_slope:0., b6_int:0. , b6_corr:0. , $
    notes:'', time:0., mean_n_nochng:0U, link:"" }
    
  output_summaries = replicate(output_summaries, n_all_files)
  
  ;Now go through and run it on all of the files
  for run = 0, n_all_files - 1 do begin
    t1 = systime(1)  ;track the time for each date
    ;if using a modis nbar the does not have a mask, set it to the dependent images mask
    if norm_method eq 1 then madcal_control_info[run].refmask = image_info[run].cloud_file
    
    ;set the dependent file info
    madcal_control_info[run].depfile = image_info[run].image_file
    madcal_control_info[run].depmask = image_info[run].cloud_file
    
    print, ">>> normalizing: ", madcal_control_info[run].depfile
    
    ;move on if this is the radref img
    if image_info[run].type eq 3 then goto, skip
    ;move on if this is file does not have a cloudmask
    if image_info[run].cloud_file eq "none" then begin
      print, "  >>> this image does not have a cloudmask, skipping...
      goto, skip1
    endif
    ;check to see if a normalized file already exists, if so and this is not a fix, then skip running madcal on the date
    checkit = strmid(file_basename(image_info[run].image_file),0,18)
    normed = file_search(file_dirname(image_info[run].image_file), checkit+"*_to_*.bsq", count=n_normed)
    if run_params.fix_only_these[0] eq 0 then begin
      if n_normed ge 1 then begin
        print, "  >>> this image has already been normalized..."
        print, "  >>> if you would like to redo it..."
        print, "  >>> then you must enter the year and julian day..."
        print, "  >>> in the batchfile variable: 'fix_only_these'..."
        print, "  >>> under the run_madcal section of the..."
        print, "  >>> dynamic variable inputs and make sure you are either..."
        print, "  >>> processing procedure group 2 or that you are..."
        print, "  >>> running the 'run_madcal' procedure in manaul mode"
        goto, skip1
      endif
    endif
    notes = ''    ;keeps track of the key issues, if any
    
    ;set up the file to keep track of all output from this run
    results_path = get_pathname(madcal_control_info[run].depfile)
    filename = get_filename(madcal_control_info[run].depfile)
    writefile = strmid(filename, 0, strlen(filename)-4)+madcal_control_info[run].run_name+'_madcaloutputs.txt'
    
    if n_elements(useareafile) eq 0 then footprint = madcal_control_info[run].reffile else $
      footprint = useareafile
      
    ;---make file name for normalized output image---
    ncf = madcal_control_info[run].depfile
    ;time = timestamp() ;string(bin_date(systime()), format='(I4,I02,I02,"_",I02,I02,I02)')
    output_base = strcompress(strmid(ncf, 0, strlen(ncf)-26)+"_"+timestamp()+madcal_control_info[run].run_name, /rem)  ;time
    
    ;check for and delete dates that have already been normalized (delete "_to_" and it's TC img)
    output_check = strcompress(output_base+".bsq", /rem)
    lt_delete_duplicate, output_check, /radnorm, /tcimg
    
    nochange_mask = output_base+'_nochangepixels.bsq'
    ;nochange_mask = results_path+'radcal_'+madcal_control_info[run].run_name+'_nochangepixels.bsq' ;old jdb 9/26/11
    
    this_run_info = {file1:madcal_control_info[run].reffile, $
      file2:madcal_control_info[run].depfile, $
      file1mask:madcal_control_info[run].refmask, $
      file2mask:madcal_control_info[run].depmask, $
      ; usearea:madcal_control_info[run].usearea,$ ;the below footprint is the usearea file
      run_name:madcal_control_info[run].run_name, $
      results_path:results_path, $
      ignore:madcal_control_info[run].ignore, $
      footprint:footprint,$
      nochange:nochange_mask}
      
    ;---find no change pixels---
    if norm_method eq 1 then modis_ref = 1
    madcal_pixel_picker, rootdirpath, this_run_info, image_info, param_set=param_set, modis_ref=modis_ref
    
    ;---pack up some inputs for regress---
    nochange_info = {nochange_file:nochange_mask,$
      fname1:this_run_info.file1,$
      fname2:this_run_info.file2}
    use_dist_screen = run_params.dist_screen
    use_screen_prob = run_params.screen_prob
    
    ;---do the regression on the no-change pixels for the ref and dep image
    print, "starting regression..."
    ok2 = regress_from_nochange_dist_auto(nochange_info, output_base, ignore=madcal_control_info[run].ignore,$
      screen_prob = use_screen_prob, apply_to_entire_image=apply_to_entire_image, $
      dist_screen = use_dist_screen, madcal_control_info[run].depmask, eightbit=eightbit)
      
    ;create a single tif of the 6 bands and their regression
    dir = file_dirname(nochange_info.fname2)
    title = file_basename(nochange_info.fname2)
    tifs = file_search(dir, "*.tif")
    ;    if n_elements(tifs) ne 6 then  ;NEED TO FINISH THIS
    
    if n_elements(tifs) eq 8 then begin
      img1 = read_image(tifs[0]) ;this is the 3band img
      img2 = read_image(tifs[1])
      img3 = read_image(tifs[2])
      img4 = read_image(tifs[3])
      img5 = read_image(tifs[4])
      img6 = read_image(tifs[5])
      img7 = read_image(tifs[6])
      img8 = read_image(tifs[7])
      
      !P.MULTI=[0,4,2]
      window, xsize=1200, ysize=700, title=title, 7 ;create a window to hold all of the regression tifs
      loadct,0 ;load a color table
      TVImage,reverse(img1,3),true=1 ;display the images in the window
      TVImage,reverse(img2,3),true=1
      TVImage,reverse(img3,3),true=1
      TVImage,reverse(img4,3),true=1
      TVImage,reverse(img5,3),true=1
      TVImage,reverse(img6,3),true=1
      TVImage,reverse(img7,3),true=1
      TVImage,reverse(img8,3),true=1
      
      aaa = tvrd(/true) ;get the window as a variable
      
      ;create filename for combined plots
      filename = file_basename(tifs[0])
      len = strlen(filename)-9 ;was 7    -9/8/2011
      filename = strcompress(strmid(filename,0, len)+"_regression.tif")
      dirlen = strlen(dir)
      madcal_folder = strcompress(strmid(dir,0,dirlen-11)+"madcal\" , /rem)
      ;delete pervious versions
      
      searchthis = strcompress(string(image_info[run].year)+ $
        "_"+string(image_info[run].julday)+"*.tif", /rem)
      oldsummary = file_search(madcal_folder, searchthis, count=n_oldsummary)
      if n_oldsummary ge 1 then file_delete, oldsummary, /allow_nonexistent, /quiet
      
      output_tiff = strcompress(madcal_folder+string(image_info[run].year)+ $
        "_"+string(image_info[run].julday)+"_"+filename, /rem)
      ;output_tiff = strcompress(madcal_folder+filename, /rem) ;old jdb 9/26/11
        
      ;write it out
      write_tiff, output_tiff, reverse(aaa,3) ;write out the combined tiffs
    endif
    
    ;delete the individual files
    file_delete, tifs, /quiet
    
    ;fill in the output summaries
    output_summaries[run].depfile = nochange_info.fname2
    output_summaries[run].year = image_info[run].year
    output_summaries[run].doy = image_info[run].julday
    output_summaries[run].mean_correlation = mean(ok2.band_stats.corr)
    ;    output_summaries[run].num_subsets = nochange_info.num_subsets
    output_summaries[run].successful_run = 1  ;if we got here, it ran
    output_summaries[run].b1_slope = ok2.band_stats[0].slope
    output_summaries[run].b1_int = ok2.band_stats[0].intercept
    output_summaries[run].b1_corr = ok2.band_stats[0].corr
    
    output_summaries[run].b2_slope = ok2.band_stats[1].slope
    output_summaries[run].b2_int = ok2.band_stats[1].intercept
    output_summaries[run].b2_corr = ok2.band_stats[1].corr
    
    output_summaries[run].b3_slope = ok2.band_stats[2].slope
    output_summaries[run].b3_int = ok2.band_stats[2].intercept
    output_summaries[run].b3_corr = ok2.band_stats[2].corr
    if n_elements(ok2.band_stats) gt 3 then begin
      output_summaries[run].b4_slope = ok2.band_stats[3].slope
      output_summaries[run].b4_int = ok2.band_stats[3].intercept
      output_summaries[run].b4_corr = ok2.band_stats[3].corr
    end
    if n_elements(ok2.band_stats) gt 4 then begin
      output_summaries[run].b5_slope = ok2.band_stats[4].slope
      output_summaries[run].b5_int = ok2.band_stats[4].intercept
      output_summaries[run].b5_corr = ok2.band_stats[4].corr
    end
    if n_elements(ok2.band_stats) gt 5 then begin
      output_summaries[run].b6_slope = ok2.band_stats[5].slope
      output_summaries[run].b6_int = ok2.band_stats[5].intercept
      output_summaries[run].b6_corr = ok2.band_stats[5].corr
    end
    
    ;image_info[run].image_file =  ok2.calibrated_image
    output_summaries[run].notes = notes
    output_summaries[run].mean_n_nochng = round(mean(ok2.band_stats.n_nochng))
    output_summaries[run].link = output_tiff
    
    ;create the metadata structure
    thisrunsummary = output_summaries[run]
    meta = make_metadata_for_preprocessing(ok2.calibrated_image, madcal_summary=thisrunsummary, norm_method=norm_method)
    metaout = stringswap(ok2.calibrated_image, ".bsq", "_meta.txt")
    concatenate_metadata, madcal_control_info[run].depfile, metaout, params=meta
    
    skip: ;if image_info[run].type eq 3 (refimg)
    t2 = systime(1)
    time = float((t2-t1)/60)
    output_summaries[run].time = time
    
    ;---write out the summary as both a csv and .sav---
    if image_info[run].type eq 3 then begin
      output_summaries[run].year = image_info[run].year
      output_summaries[run].doy = image_info[run].julday
    endif
    
    ;create name for .sav file, .csv is already defined
    result_file_base = strcompress(file_dirname(output_csv_file)+"\", /rem)
    norm_summary_savefile = strcompress(result_file_base+file_basename(output_csv_file, ".csv")+".sav", /rem)
    search_string = strcompress("*"+strmid(file_basename(output_summaries[run].depfile),0,18)+"*", /rem)
    
    ;find out if the .sav file exists
    exists = file_exists(norm_summary_savefile)
    if exists eq 1 then begin
      restore, norm_summary_savefile
      ;see if the current image is new to the .sav summary file
      duplicate = where(strmatch(norm_info.depfile, search_string) eq 1, dupcount)
      ;newone = where(file_basename(norm_info.depfile) ne search_string, newcount)
      if dupcount ge 1 then begin ;deal with duplicates
        norm_info[duplicate] = output_summaries[run]
        save, norm_info, filename =norm_summary_savefile ;save the structure
      endif else begin
        norm_info = [norm_info,output_summaries[run]]
        dones = where(norm_info.year ne 0)
        norm_info = norm_info[dones]
        save, norm_info, filename =norm_summary_savefile ;save the structure
      endelse
    endif else begin ;do this if the norm_info .savfile does not exist
      ;save the summary info structure as a .sav file
      dones = where(output_summaries.depfile ne '')
      norm_info = output_summaries[dones]
      save, norm_info, filename =norm_summary_savefile ;save the structure
    endelse
    
    restore, norm_summary_savefile
    wilco = where(norm_info.year eq 0, n_wilco)
    if n_wilco ge 1 then begin
      wilco = where(norm_info.year ne 0)
      norm_info = norm_info[wilco]
      save, norm_info, filename =norm_summary_savefile ;save the structure
    endif
    
    export_structure_to_file, norm_info, output_csv_file ;export the structure to a .csv
    print, "time: ", time," minutes"
    ;skip: ;skips the reference image and images that do not have a cloudmask
    skip1:
  endfor ;loop for images
  
  
  ;save the summary info structure as a .sav file
  norm_info = output_summaries
  save, norm_info, filename =image_info_savefile;save the structure
  export_structure_to_file, norm_info, output_csv_file ;export the structure to a .csv
  
  return, {ok:1, image_info:image_info, output_summaries:output_summaries}
end

