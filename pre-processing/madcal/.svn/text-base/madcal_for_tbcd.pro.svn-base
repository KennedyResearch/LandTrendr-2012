;**************************************************************************** 
;Copyright © 2008-2011 Oregon State University                                
;All Rights Reserved.                                                         
;                                                                             
;                                                                             
;Permission to use, copy, modify, and distribute this software and its        
;documentation for educational, research and non-profit purposes, without     
;fee, and without a written agreement is hereby granted, provided that the    
;above copyright notice, this paragraph and the following three paragraphs    
;appear in all copies.                                                        
;                                                                             
;                                                                             
;Permission to incorporate this software into commercial products may be      
;obtained by contacting Oregon State University Office of Technology Transfer.
;                                                                             
;                                                                             
;This software program and documentation are copyrighted by Oregon State      
;University. The software program and documentation are supplied "as is",     
;without any accompanying services from Oregon State University. OSU does not 
;warrant that the operation of the program will be uninterrupted or           
;error-free. The end-user understands that the program was developed for      
;research purposes and is advised not to rely exclusively on the program for  
;any reason.                                                                  
;                                                                             
;                                                                             
;IN NO EVENT SHALL OREGON STATE UNIVERSITY BE LIABLE TO ANY PARTY FOR DIRECT, 
;INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST      
;PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
;IF OREGON STATE UNIVERSITYHAS BEEN ADVISED OF THE POSSIBILITY OF SUCH        
;DAMAGE. OREGON STATE UNIVERSITY SPECIFICALLY DISCLAIMS ANY WARRANTIES,       
;INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
;FITNESS FOR A PARTICULAR PURPOSE AND ANY STATUTORY WARRANTY OF               
;NON-INFRINGEMENT. THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS,    
;AND OREGON STATE UNIVERSITY HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE,       
;SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.                            
;                                                                             
;**************************************************************************** 

function madcal_for_tbcd, image_info, run_params, output_csv_file, $
			useareafile=useareafile, apply_to_entire_image=apply_to_entire_image, $
			min_nochange_count=min_nochange_count



;only use "apply_to_entire_image" for two image case

  ;version numbers:

  versionnumber = 'TBCD MADCAL version 2.01 February 2008 REK'
  ;does not require colorsavefile


  ;the procedure to run the madcal routine

  w, 0, 300, 500
  xyouts, .1, .8, 'Beginning MADCAL', /norm

  path = 'C:\temp'

  d = timestamp()
  code = d.month+d.date+d.hours+d.minutes+d.seconds
  dfile = path+ '\madcal_diagfile'+code+'.txt'


  xyouts, .1, .7, 'log file', /norm
  xyouts, .1, .6, path+'\madcal_diagfile'+code+'.txt', /norm


  write_diagnosis, dfile, 'Running MADCAL on '+code
  write_diagnosis, dfile, versionnumber
;  write_diagnosis, dfile, 'Getting madcal_control_file from user'
;
;  madcal_control_file = dialog_pickfile(path=path, /read, /must_exist)

  ;madcal_control_file = 'D:\NPS\SWAN\p70r17\madcal_runfile_athome_p70r17_1987_to_2000.txt'

;
;  if madcal_control_file eq '' then begin
;    write_diagnosis, dfile, 'User picked no file'
;    return
;  end
;

;
;  write_diagnosis, dfile, 'Reading madcal_control_file'
;
;
;  output = read_madcal_control_file(madcal_control_file)
;;
;  if output.ok eq 0 then begin
;    w, 0, 300, 100
;    xyouts, .1, .1, 'Error in read of madcal control file. See diag file.'
;
;    write_diagnosis, dfile, 'Problem reading madcal control file'
;    write_diagnosis, dfile, 'Name of file: '
;    write_diagnosis, dfile, madcal_control_file
;    write_diagnosis, dfile, 'Error message:'
;    write_diagnosis, dfile, output.error
;
;
;    return
;  end


  ;use the image info to set up runs

	  ref_index = where(image_info.type eq 3, refcount)
	  if refcount eq 0 then begin
	  	print, 'The image to be used for the MADCAL reference '
	  	print, 'was not found in the image info structure. '
	  	print, 'Make sure that find_tbcd_files is used to find'
	  	print, 'files, and that the reference image is an MTBS file'
	  	print, 'that has been converted to COST and ends in _cost_refl.img'
	  	return, {ok:-1}

	  end


  ;set up structure for running

	base = {reffile:image_info[ref_index].image_file, $
			depfile:'', $
			ignore:run_params.ignore, $
			run_name:'', $
			subset_size:run_params.subset_size, $

			coordinates:run_params.subset_coordinates}


	;if we just want to fix some of them, subset the image
	; info

		original_image_info = image_info
		if run_params.fix_only_these[0] ne -1 then begin
		   ;the fix_only_these is a vector with the years to fix
		   n_fixes = n_elements(run_params.fix_only_these)
		   ;fix_indices = lonarr(n_fixes)
           pointer = 0


		   for fi = 0, n_fixes-1 do begin

			  ;check to see about the julian day.
			  if run_params.fix_only_these[fi] gt 3000 then begin	;if only the year
				 this_year = floor(run_params.fix_only_these[fi]/1000. )
				 this_julday = run_params.fix_only_these[fi]-(long(this_year)*1000)
			  	 match= where(image_info.year eq this_year and $
			  	 				image_info.julday eq this_julday, n_matches)
		      end else begin 	;if user only gave year
		      	this_year = run_params.fix_only_these[fi]
				this_julday = -1
		      	match = where(image_info.year eq run_params.fix_only_these[fi], n_matches)
		      end


		      if n_matches eq 0 then message, 'MADCAL_for_TBCD:  Cannot fix this year because not in image list'+string( run_params.fix_only_these[fi])

		      if fi eq 0 then fix_indices = lonarr(n_matches) else expand_cols, fix_indices, n_matches, outdims

			  for th = 0, n_matches-1 do begin		;number of images in this year that match

		      fix_indices[pointer+th] = match[th]


		      	;now need to revert to the source image being the original, just in case
			;   we're re-running madcal and there is an existing one in there.

			  imgfile = original_image_info[match[th]].image_file
			  s=strlen(imgfile)
			  check = (strpos(imgfile, '_to_') ne -1)


			  sep = path_sep()

			  if check then begin
			  	path = get_pathname(image_info[match[th]].image_file)
				length =strlen(path)
				slash = strpos(path, sep, length, /reverse_search)  ;start at 1, in case there's on at the end that would confuse things


			  ;go through each type looking for a clue.

			   mtbs_file = file_search(path, '*refl.img')
			   n_mtbs_files = n_elements(mtbs_file)-total(mtbs_file eq "")
			   if n_mtbs_files ne 0 then begin

			   	for i = 0, n_mtbs_files-1 do   begin
			      juldayoffset = 12
			      julday_tentative = fix(strmid(mtbs_file[i],slash+juldayoffset, 3))
				  ;compare with the julian day passed by the user, if one was passed
				  if this_julday ne -1 then tester = this_julday else tester=julday_tentative

				  if julday_tentative eq tester and $
				  		julday_tentative eq image_info[match[th]].julday then begin
				       original_image_info[match[th]].image_file = mtbs_file[i]
				       goto, getout
				       end
				end
			   end



			   nlaps_file= file_search(path, '*nlaps.img')
			   n_nlaps_files = n_elements(nlaps_file)-total(nlaps_file eq "")
  				if n_nlaps_files ne 0 then begin

			   	for i = 0, n_nlaps_files-1 do   begin
			      juldayoffset = 14
			      julday_tentative = fix(strmid(nlaps_file[i],slash+juldayoffset, 3))
				   ;compare with the julian day passed by the user, if one was passed
				  if this_julday ne -1 then tester = this_julday else tester=julday_tentative

				  if julday_tentative eq tester and $
				  		julday_tentative eq image_info[match[th]].julday then begin
				       original_image_info[match[th]].image_file = nlaps_file[i]
				       goto, getout
				       end
				end
			   end


			   other_file= file_search(path, '*6band.img')	;these are images either purchased or inherited and then rectified
			   n_other_files = n_elements(other_file)-total(other_file eq "")
				if n_other_files ne 0 then begin

			   	for i = 0, n_other_files-1 do   begin
			      juldayoffset = 14
			      julday_tentative = fix(strmid(other_file[i],slash+juldayoffset, 3))
				   ;compare with the julian day passed by the user, if one was passed
				  if this_julday ne -1 then tester = this_julday else tester=julday_tentative

				  if julday_tentative eq tester and $
				  		julday_tentative eq image_info[match[th]].julday then begin
				       original_image_info[match[th]].image_file = other_file[i]
				       goto, getout
				       end
				end
			   end



			   archv_file= file_search(path, '*archv.img')	;these are images either purchased or inherited and then rectified
			   n_archv_files = n_elements(archv_file)-total(archv_file eq "")
				if n_archv_files ne 0 then begin

			   	for i = 0, n_archv_files-1 do   begin
			      juldayoffset = 16
			      julday_tentative = fix(strmid(archv_file[i],slash+juldayoffset, 3))
				   ;compare with the julian day passed by the user, if one was passed
				  if this_julday ne -1 then tester = this_julday else tester=julday_tentative

				  if julday_tentative eq tester and $
				  		julday_tentative eq image_info[match[th]].julday then begin
				       original_image_info[match[th]].image_file = archv_file[i]
				       goto, getout
				       end
				end
			   end






										 ;			   case 1 of
						;			   	(n_mtbs_files ne 0):  original_image_info[match[th]].image_file = mtbs_file
						;				(n_nlaps_files ne 0):  original_image_info[match[th]].image_file = nlaps_file
						;				(n_other_files ne 0):  original_image_info[match[th]].image_file = other_file
						;				else:  message, 'Cannot find a file to base madcal -- searching for*refl, *nlaps, *6band in path ' +path
						;				endcase

				print, 'no match found'
				stop

				getout:  ;found it, now get out


			  end

			 end;loop for number of images in this year
			 pointer = pointer + n_matches


		   end

			image_info = original_image_info[fix_indices]

		end




	n_all_files = n_elements(image_info)




	madcal_control_info = replicate(base, n_all_files)

  ;set up summary structure
    thisbase= {depfile:'', mean_correlation:0.0, successful_run:0, $
    			num_subsets:0, $
    			b1_slope:0., b1_int:0. , b1_corr:0., $

    			b2_slope:0., b2_int:0. , b2_corr:0., $

    			b3_slope:0., b3_int:0. , b3_corr:0., $

    			b4_slope:0., b4_int:0. , b4_corr:0., $

    			b5_slope:0., b5_int:0. , b5_corr:0., $

    			b6_slope:0., b6_int:0. , b6_corr:0. , $
    			notes:'' }

    output_summaries = replicate(thisbase, n_all_files)


;****************
;Now go through and run it on all of the files

for run = 0, n_all_files - 1 do begin
     output_summaries[run].depfile = image_info[run].image_file
	notes = ''		;keeps track of the key issues, if any

    if image_info[run].type eq 3 then goto, skip

	madcal_control_info.depfile = image_info[run].image_file
	madcal_control_info.run_name = strcompress(string(image_info[run].year)+$
					string(image_info[run].julday) + $
					'_to_'+string(original_image_info[ref_index].year), /rem)


	;set up the file to keep track of all output from this run
		results_path = get_pathname(madcal_control_info[run].depfile)
		filename = get_filename(madcal_control_info[run].depfile)
		writefile = strmid(filename, 0, strlen(filename)-4)+madcal_control_info[run].run_name+'_madcaloutputs.txt'

	;make a note in the log of what's happening.

		write_diagnosis, dfile,'Setting up results file:'
		write_diagnosis, dfile, results_path+writefile

		;write out the information on this run
		wfile = results_path + writefile


		write_diagnosis, wfile, 'MADCAL for TBCD output file'
		write_diagnosis, wfile, code
		write_diagnosis, wfile, '-------'
		write_diagnosis, wfile, 'Inputs for MADCAL taken from image info structure and'
		write_diagnosis, wfile, 'user run parameters'

		write_diagnosis, wfile, 'Reference file'
		write_diagnosis, wfile, madcal_control_info.reffile
		write_diagnosis, wfile, 'Dependent file'
		write_diagnosis, wfile, madcal_control_info.depfile
		write_diagnosis, wfile, 'Subset size'
		write_diagnosis, wfile, madcal_control_info.subset_size
		write_diagnosis, wfile, 'Coordinates of subsets'
		write_diagnosis, wfile, madcal_control_info.coordinates





		  write_diagnosis, dfile, 'Starting madcal to identify no-change pixels.'


		  if n_elements(useareafile) eq 0 then footprint = madcal_control_info[run].reffile else $
		  			footprint = useareafile

		  this_run_info = {file1:madcal_control_info[run].reffile, $
		    file2:madcal_control_info[run].depfile, $
		    subset_size:madcal_control_info[run].subset_size, $
		    subset_upls:madcal_control_info[run].coordinates, $
		    run_name:madcal_control_info[run].run_name, $
		    wfile:wfile, $
		    dfile:dfile, $
		    results_path:results_path, $
		    ignore:madcal_control_info[run].ignore, $
		    footprint:footprint, $
		    waterthresh:run_params.waterthresh, $
		    cloudthresh:run_params.cloudthresh}  ;,$  removed REK 10/8/09
		    					  ;layers:run_params.layers}


		  ;apply radcal to find no-change areas and coefficients
		rerun_count = 0		;keep track if we end up redoing this.
		min_corr = run_params.min_correlation
		rerun: 		;come here if the regress from no change didn't work, so retry with
						;tougher standards



		move_on = 0
		check_count = 0

		while(move_on eq 0) do begin 	;keep doing apply_multiple_radcals until at least one subset
										;makes it past water and cloud thresholds

		  status = apply_multiple_radcals(this_run_info, min_nochange_count=min_nochange_count, /overwrite)

		  if status.ok ne 1 then begin
		    w, 0, 300, 100
		    xyouts, .1, .1, 'Error in madcal finding no-change. See diag file.'
		    write_diagnosis, dfile, 'Problem with MADCAL.'
		    free_lun, logun
		    free_lun, wfile
		    notes = notes+'_fundamental problem with layers or image sizes_
		    goto, skip

		  end

		  ;check to see if all of the subsets were screened out by cloud or water

		  outside_bounds = where(status.status_score eq 5, n_outsides)
		  if n_outsides ne 0 then notes = notes +'_some subsets outside bounds_'
		  valid_subsets = where(status.status_score ne 5, n_valid_subsets)	;5 are those outside the bounds

		  cloud_failed = where(status.status_score eq 2, n_cloud_failed)
		  water_failed = where(status.status_score eq 3, n_water_failed)
		  if n_cloud_failed+n_water_failed eq n_valid_subsets  then begin
		      ;this_run_info.waterthresh[1] = this_run_info.waterthresh[1] * .75
		      this_run_info.cloudthresh[1] = this_run_info.cloudthresh[1] * 1.25
			  if check_count eq 0 then notes = notes + '_adjusting cloud thresholds_'

		   end else move_on = 1

		  ;now just in case, add to check_count, and resize the window if we've done this three times
		  check_count = check_count+1
		  if check_count eq 4 then this_run_info.cloudthresh=run_params.cloudthresh

		  if check_count ge 4 then begin
			this_run_info.waterthresh=run_params.waterthresh
  			this_run_info.cloudthresh[1]=this_run_info.cloudthresh[1]*1.25
		  	this_run_info.subset_size=this_run_info.subset_size*.75
		  	if check_count eq 4 then notes = notes + '_adjusting window size_'
		  end

		  if check_count eq 8 then begin
		  		move_on = 1	;finally, just bail
		  		notes = notes + '_all adjustments failed to reach 3 subsets_'
		   end

		endwhile

		  ;then combine the no-change areas

		  write_diagnosis, dfile, 'Combining no-change areas from the different subsets'
		move_on = 0
		check_count = 0




		while move_on eq 0 do begin

		  status1 = combine_decent_nochange_files(status.outfile, dfile, wfile, $
		  		min_correlation=min_corr, /overwrite)

		 if status1.ok eq 0 then begin
		 	write_diagnosis, wfile, 'No subsets passed min_threshold test for '
		 	write_diagnosis, wfile, status.outfile
		 	IF check_count eq 0 then notes = notes + "_adjusted min corr threshold to include more subsets_"

			if check_count eq 3 then begin
				output_summaries[run].successful_run = 0
				write_diagnosis, wfile, 'Giving up after 3 reductions in threshold'
				notes=notes+"_no subsets passed adjusted thresholds_"
		 		goto, skip
		    end


		 	write_diagnosis, wfile, 'Trying with reduced threshold'
		 	min_corr = min_corr * .85

		  end

		  ;now just in case, add to check_count, and bail if we've done this three times
		  check_count = check_count+1
		  if check_count eq 4 then move_on = 1

		endwhile




		 output_summaries[run].successful_run = 1	;this one is run



		;clean up the nochange

		;	clean_status = clean_nochange(status1.nochange_info, this_run_info, $
		;			dfile, wfile, ignore=madcal_control_info[run].ignore)






		  write_diagnosis, wfile, '-------------------------------------------------------'
		  write_diagnosis, wfile, 'Performing final calibration using all no-change pixels'
		  write_diagnosis, wfile, '-------------------------------------------------------'
		  write_diagnosis, wfile, '    The following should be included in meta-data'
		  write_diagnosis, wfile, '              for the dependent image.'
		  write_diagnosis, wfile, '   '


		  ;then apply the nochange areas
		  ncf = status1.nochange_info.fname2
		  output_base = strcompress(strmid(ncf, 0, strlen(ncf)-4)+madcal_control_info[run].run_name, /rem)

	move_on = 0
		check_count = 0
		min_corr = run_params.min_correlation
		use_screen_prob = run_params.screen_prob
		use_dist_screen = run_params.dist_screen
		while move_on eq 0 do begin


		  ok2 = regress_from_nochange_dist(status1.nochange_info, output_base, $
		    dfile, wfile, ignore = madcal_control_info[run].ignore, $
		    screen_prob = use_screen_prob, apply_to_entire_image=apply_to_entire_image, $
		    dist_screen = use_dist_screen)


		  if ok2.ok eq -1 then begin		;this only occurs if there really are no good data to work with
	 		write_diagnosis, wfile, 'PROBLEM in regress_from_nochange '
		 	write_diagnosis, wfile, 'in '+output_base
		 	write_diagnosis, wfile, 'in band '+string(ok2.bad_band)
		 	write_diagnosis, wfile, 'This one is skipped'
		 	output_summaries[run].successful_run = 0
		 	notes = notes + "_problem in regressing band "+string(ok2.bad_band)+"_"
		 	goto, skip
		  end


		  if ok2.ok eq 1 then move_on = 1

		  check_count = check_count + 1
		  if check_count eq 1 and move_on ne 1 then notes = notes + "_upping screen prob to improve regression_"
		  use_screen_prob = use_screen_prob * 2		;double it

		  if check_count eq 2 and move_on ne 1 then begin
		  		notes=notes+"_trying more restrictive water and cloud thresholds band "+string(ok2.bad_band)+"_"
				;try from the top, but work on the assumption that there are too
				;    many messy subsets, so try more restrictive set of parameters
					  this_run_info = {file1:madcal_control_info[run].reffile, $
		    file2:madcal_control_info[run].depfile, $
		    subset_size:madcal_control_info[run].subset_size, $
		    subset_upls:madcal_control_info[run].coordinates, $
		    run_name:madcal_control_info[run].run_name, $
		    wfile:wfile, $
		    dfile:dfile, $
		    results_path:results_path, $
		    ignore:madcal_control_info[run].ignore, $
		    footprint:footprint, $
		    waterthresh:run_params.waterthresh, $
		    cloudthresh:run_params.cloudthresh}


			this_run_info.waterthresh[1] = this_run_info.waterthresh[1]*1.10		;screen out more water
			this_run_info.cloudthresh[1] = this_run_info.cloudthresh[1]*.75		;lower this to screen out more
			this_run_info.cloudthresh[2] = this_run_info.cloudthresh[2]*.75		;make the proportion of subset to filter lower
			min_corr  = min_corr * 1.10		;up the mininum correlation too


			;this will continue until screen prob runs out of values

			;need to keep track of how often we do this

			rerun_count = rerun_count + 1
			if rerun_count eq 5 then goto, skip else goto, rerun
			;goto, rerun
;		  		output_summaries[run].successful_run = 0
;		  		goto, skip
		  end
		endwhile






		; add to the madcal summary information structure


		  output_summaries[run].depfile = status1.nochange_info.fname2
		  output_summaries[run].mean_correlation = mean(ok2.band_stats.corr)
		  output_summaries[run].num_subsets = status1.nochange_info.num_subsets
		  output_summaries[run].successful_run = 1	;if we got here, it ran
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



		  write_diagnosis, wfile, "Done."
		  write_diagnosis, wfile, "Calibrated image is named: "+ok2.calibrated_image
		  write_diagnosis, wfile, "Overall savefile is named: "+ok2.savefile
		  write_diagnosis, wfile, "Header information for calibrated image: "

		  write_diagnosis, dfile, "Done."
		  write_diagnosis, dfile, "Calibrated image is named: "+ok2.calibrated_image
		  write_diagnosis, dfile, "Overall savefile is named: "+ok2.savefile
		  write_diagnosis, dfile, "Header information for calibrated image: "



		;now update the file in the image info so that we'll know to use it for tbcd

		  image_info[run].image_file =  ok2.calibrated_image

		;and finally, write out the output summaries after each run, so in case
		;   anything happens, we don't lose what happened on the good ones


skip:

print, 'exporting'
output_summaries[run].notes = notes

		export_structure_to_file, output_summaries, output_csv_file



end	;this image, now go onto the next


  ;update the image info in the full image info (the original)
  if run_params.fix_only_these[0] ne -1 then begin
  	 for fi = 0, n_fixes-1 do original_image_info[fix_indices[fi]] = image_info[fi]
  	 image_info = original_image_info	;fill back in new vals
   end




  return, {ok:1, image_info:image_info, output_summaries:output_summaries}

end


;may 21, 2006.  changed output so that it will attach the run name to the
;  output file.

