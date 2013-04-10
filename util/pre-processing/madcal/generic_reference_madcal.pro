pro generic_reference_madcal, path, ppprrr, run_params, useareafile, $ ;image_info_savefile,
    radiometric_ref_img, ls_madcal_ref_img=ls_madcal_ref_img, eightbit=eightbit,$
    pcaonthefly=pcaonthefly, trgtday=trgtday, trgtyear=trgtyear
  
  ;if radiometric_ref_img eq 0 then message, "you must define an image path for the variable 'radiometric_ref_img' in the batchfile"
    wilco = file_search(path+"images\", "*radref.bsq", count=nthese) ;find the radref image
  if nthese ge 1 then begin
    wilco = file_search(path, "*radref*", count=nthese) ;if one exists find all associated files
    wilcogoods = where(strmatch(wilco, "*txt") ne 1 and wilco ne '', n_wilco)
    if n_wilco ge 1 then begin
      wilco = wilco[wilcogoods]
      repeat begin
        print, ""
        print, ">>> a prepared LANDSAT reference image..."
        print, ">>> already exists, do you want to overwrite it?"
        print, ">>> it is recommended that you do..."
        print, ">>> otherwise you're going to run into..."
        print, ">>> troubles later - select an option"
        print, ">>> NO (press 1), YES (press 2) STOP, (press 3)
        b = get_kbrd()
      endrep until b eq 1 or b eq 2 or b eq 3
      print, ""
      if b eq 2 then begin
        close, /all
        ;find the normalized images and delete them since they'll all have to change
        normimgs = file_search(path, "*_to_*")
        normimgs_index = where(strmatch(normimgs, "*txt") ne 1 and strmatch(normimgs, "*pro") $
          ne 1 and normimgs ne '', n_normings)
        if n_normings ge 1 then begin
          normimgs = normimgs[normimgs_index]
          file_delete, wilco, normimgs
        endif else file_delete, wilco
        tengo = file_search(path, "*_orig*", count=n_tengo)
        if n_tengo ge 1 then begin
          for i=0, n_tengo-1 do begin
            tengolen = strlen(tengo[i])
            tengopos = strpos(tengo[i], "_orig")
            tengopart1 = strmid(tengo[i],0,tengopos)
            tengopart2 = strmid(tengo[i], tengopos+5, tengolen-tengopos+5)
            humptydumpty = strcompress(tengopart1+tengopart2, /rem)
            file_move, tengo[i], humptydumpty
          endfor
        endif
      endif
      if b eq 3 then stop
    endif
  endif
  
  
  
  
  
  
  searchfor = strcompress("*"+strmid(file_basename(ls_madcal_ref_img),0,40)+"*.bsq", /rem)
  ls_madcal_ref_img = file_search(path+"images\", searchfor, count=n_ls_madcal_ref_img)
  if n_ls_madcal_ref_img eq 1 then ls_ref_img = ls_madcal_ref_img else message, "something is not right about the selected reference image"
  
  ;create a temporary image_info
  lt_images = strcompress(path+'images\', /rem)
  image_info = find_landtrendr_files_for_cost(lt_images, /ignore_ref)
  image_info = vct_repopulate_image_info(image_info, /from_auto)
  ok = find_union_area(image_info, /checkmask, /checkusearea, /isinfo)
  image_info.subset = ok.coords
  
  ;perform madcal on the landsat reference image using the above MODIS image as the reference
  ;subset the image_info so that it is only the selected LS reference image
  ref_info_loc = where(image_info.image_file eq ls_ref_img[0], count)
  if count lt 1 then begin
    print, ">>> apparently the file:"
    print, "  ", ref_info_loc
    print, ">>> does not exists in image_info.image_file"
    print, ">>> check on this - here is the location of image_info"
    print, "  ", image_info_savefile
    stop
  endif
  if count gt 1 then begin
    print, ">>> apparently there are multiple files of:"
    print, "  ", ref_info_loc
    print, ">>> in image_info.image_file
    print, ">>> check on this - here is the location of image_info"
    print, "  ", image_info_savefile
    stop
  endif
  
  image_info = image_info[ref_info_loc] ;subet image_info should only include the LS ref img
  
  ;set up the run params structure
  ref_index = 0
  madcal_control_info = {reffile:image_info[ref_index].image_file, $
    refmask:image_info[ref_index].cloud_file, $
    depfile:'', $
    depmask:'',$
    ignore:run_params.ignore, $
    run_name:''}
    
  ignore = run_params.ignore
  
  ;setup the summary structure
  output_summaries = {depfile:'', year:0U, doy:0U, mean_correlation:0.0,$
    successful_run:0, num_subsets:0, $
    b1_slope:0., b1_int:0. , b1_corr:0., $
    b2_slope:0., b2_int:0. , b2_corr:0., $
    b3_slope:0., b3_int:0. , b3_corr:0., $
    b4_slope:0., b4_int:0. , b4_corr:0., $
    b5_slope:0., b5_int:0. , b5_corr:0., $
    b6_slope:0., b6_int:0. , b6_corr:0. , $
    notes:'', time:0., mean_n_nochng:0U, link:"" }
    
  n_all_files = 1
  for run = 0, n_all_files - 1 do begin
    t1 = systime(1)
    output_summaries[run].depfile = ls_ref_img
    print, "normalizing: ",output_summaries[run].depfile
    if image_info[run].type eq 3 then goto, skip
    if image_info[run].cloud_file eq "none" then begin
      print, ">>> this image does not have a cloudmask, skipping...
      goto, skip
    endif
    
    notes = ''    ;keeps track of the key issues, if any
    
    madcal_control_info.depfile = image_info[run].image_file
    madcal_control_info.depmask = image_info[run].cloud_file
    time = timestamp()  ;string(bin_date(systime()), format='(I4,I02,I02,"_",I02,I02,I02)')
    madcal_control_info.run_name = strcompress('_'+time+'_radref', /rem)
    
    ;    madcal_control_info.run_name = strcompress(string(image_info[run].year)+$
    ;      string(image_info[run].julday) + $
    ;      '_to_'+string(image_info[ref_index].year), /rem)
    
    ;set up the file to keep track of all output from this run
    results_path = get_pathname(madcal_control_info[run].depfile)
    filename = get_filename(madcal_control_info[run].depfile)
    writefile = strmid(filename, 0, strlen(filename)-4)+madcal_control_info[run].run_name+'_madcaloutputs.txt'
    
    if n_elements(useareafile) eq 0 then footprint = madcal_control_info[run].reffile else $
      footprint = useareafile
      
      
    ledzep_dir = file_dirname(ls_ref_img)
    ledzep_file = strmid(file_basename(ls_ref_img),0,18)
    nochange_mask = results_path+ledzep_file+madcal_control_info[run].run_name+'_nochangepixels.bsq'
    
    ;nochange_mask = results_path+'radcal_'+madcal_control_info[run].run_name+'_nochangepixels.bsq'
    led_dir = file_dirname(radiometric_ref_img)
    led_file = strmid(file_basename(radiometric_ref_img),0,18)
    searchit = strcompress("*"+led_file+"*cloudmask.bsq", /rem)
    ref_mask = file_search(led_dir, searchit)
    
    this_run_info = {file1:radiometric_ref_img, $
      file2:madcal_control_info[run].depfile, $
      file1mask:ref_mask, $ ;the same as file2mask since we don't have a mask for the modis reference image
      file2mask:madcal_control_info[run].depmask, $
      ; usearea:madcal_control_info[run].usearea,$ ;the below footprint is the usearea file
      run_name:madcal_control_info[run].run_name, $
      results_path:results_path, $
      ignore:madcal_control_info[run].ignore, $
      footprint:footprint,$
      nochange:nochange_mask}
      
    ;---find no change pixels---
    ; param_set = 0
    ; redo:
    ; param_set = param_set+1
    madcal_pixel_picker, path, this_run_info, image_info, param_set=param_set,$
      radiometric_ref_img=radiometric_ref_img
      
    ;---pack up some inputs for regress---
    nochange_info = {nochange_file:nochange_mask,$
      fname1:this_run_info.file1,$
      fname2:this_run_info.file2}
    use_dist_screen = run_params.dist_screen
    use_screen_prob = run_params.screen_prob
    
    ;---make file name for normalized output image---
    ncf = nochange_info.fname2
    output_base = strcompress(strmid(ncf, 0, strlen(ncf)-26)+madcal_control_info[run].run_name, /rem)
    
    ;---do the regression on the no-change pixels for the ref and dep image
    print, "starting regression..."
    
    ;    it = 0
    ;    endthis = 0.0001
    ;    cap_adj = 0
    ;    repeat begin
    ;      it = it+1
    
    ok2 = regress_from_nochange_dist_auto(nochange_info, output_base, ignore=ignore,$
      screen_prob = use_screen_prob, /apply_to_entire_image, $
      dist_screen = use_dist_screen, madcal_control_info[run].depmask, eightbit=eightbit) ;, it, bads=bads, endthis
      
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
      len = strlen(filename)-8 ;was 7    -9/8/2011
      filename = strcompress(strmid(filename,0, len)+"regression.tif")
      dirlen = strlen(dir)
      madcaldir = strmid(dir,0,dirlen-11)+"madcal\"
      output_tiff = strcompress(madcaldir+string(image_info[run].year)+ $
        "_"+string(image_info[run].julday)+"_"+filename, /rem) ;added the "_"  -9/8/2011
        
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
    
    t2 = systime(1)
    time = float((t2-t1)/60)
    output_summaries[run].time = time
    
    output_summaries[run].mean_n_nochng = round(mean(ok2.band_stats.n_nochng))
    output_summaries[run].link = output_tiff
    
    ;create the metadata structure
    thisrunsummary = output_summaries[run]
    meta = make_metadata_for_preprocessing(ok2.calibrated_image, madcal_summary=thisrunsummary, /generic_ref)
    metaout = stringswap(ok2.calibrated_image, ".bsq", "_meta.txt")
    concatenate_metadata, madcal_control_info[run].depfile, metaout, params=meta
    
    ;---write out the summary as both a csv and .sav---
    ;create name for .sav file, .csv is already defined
    output_csv_file = path+"madcal\"+ppprrr+"_madcal_results_for_ref_img.csv"
    output_sav_file = path+"madcal\"+ppprrr+"_madcal_results_for_ref_img.sav"
    
    ;save the summary info structure as a .sav file
    norm_info = output_summaries
    save, norm_info, filename = output_sav_file;save the structure
    
    export_structure_to_file, output_summaries, output_csv_file ;export the structure to a .csv
    print, "time: ", time," minutes"
    skip: ;skips the reference image and images that do not have a cloudmask
    
    ;deal with renaming the original archv file so that it does not get picked up in image_info
    thelen = strlen(nochange_info.fname2)-4
    new_name_base = strtrim(strmid(nochange_info.fname2, 0, thelen),2)
    searchthis = strcompress("*"+file_basename(new_name_base)+"*", /rem)
    thisdir = file_dirname(nochange_info.fname2)
    thefiles = file_search(thisdir, searchthis, count=thefilescount)
    if thefilescount ge 1 then begin
      ends = strarr(thefilescount)
      new_name = strarr(thefilescount)
      for i=0, thefilescount-1 do begin
        filelen = strlen(thefiles[i])
        ends[i] = strmid(thefiles[i],thelen,filelen)
        ;ends = strtrim(strmid(thefiles,thelen+1),2)
        new_name[i] = strcompress(new_name_base+"_orig"+ends[i], /rem)
      endfor
      file_move, thefiles, new_name
    endif
  endfor ;loop for images
end