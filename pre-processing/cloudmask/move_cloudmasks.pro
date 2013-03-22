pro move_cloudmasks, ledaps_dir, alt_mask_dir, backup_ledaps_dir=backup_ledaps_dir;  move_cloudmasks, "C:\045029\", "any path to cloudmasks", backup_ledaps_dir = "path to a new directory where ledaps_masks_can be moved -- note: NOT as subdirectory of the ledaps_dir

 ;get the path separator
  psp = path_sep()

  ;first test to make sure that the end of each path has a separator
   test1 = strmid(ledaps_dir, strlen(ledaps_dir)-1,1)
   if test1 ne psp then begin
         print, 'move_cloudmasks:  expects directory to end with path separator '+psp
	 print, 'please fix in the batchfile, check the separators elsewhere, and re-run'
      return
    end
 
   test2 = strmid(alt_mask_dir, strlen(alt_mask_dir)-1,1)
   if test2 ne psp then begin
	print, 'move_cloudmasks:  expects directory to end with path separator '+psp
	print, 'please fix in the batchfile, check the separators elsewhere, and re-run'
	return
   end
 

   if n_elements(backup_ledaps_dir) ne 0 then begin 
       test3 = strmid(backup_ledaps_dir, strlen(backup_ledaps_dir)-1, 1)
       if test3 ne psp then begin 
          pirnt, 'backup ledaps directory must end in '+pse
      return
     end
end



  ;find the cloudmask files in both directories
  ledaps_masks = file_search(ledaps_dir, "*cloudmask.bsq", count=n_ledaps_masks)
  ledaps_there = 1
  ;if cloudmask files cannot be found then use the reflectance files
  if n_ledaps_masks eq 0 then begin
    ledaps_there = 0
    print, "cannot find cloudmask files in this directory: "+ledaps_dir
    ledaps_masks = file_search(ledaps_dir, "*ledaps.bsq", count=n_ledaps_masks)
  endif
  
  alt_masks = file_search(alt_mask_dir, "*cloudmask.bsq", count=n_alt_masks)
  if n_alt_masks eq 0 then message, "cannot find cloudmask files in this directory: "+alt_mask_dir
  

  ;then put in a structur
   baseledaps = {path:'', corename:'', yearday:'', year:'', used:0}
   ledaps = replicate(baseledaps, n_ledaps_masks)
   for i=0, n_ledaps_masks-1 do begin 
       ledaps[i].path = get_pathname(ledaps_masks[i])
       temp = file_basename(ledaps_masks[i])
	ledaps[i].corename = strmid(temp, 0, strlen(temp)-4) ; get the core name without bsq 

	ledaps[i].year = strmid(ledaps[i].corename, 10,4)
        ledaps[i].yearday = strmid(ledaps[i].corename, 10, 8)
 end

;do the same for the alt masks
 
  basealt = {path:'', corename:'', yearday:'', year:'', used:0}
  alt = replicate(basealt, n_alt_masks)

  for i =0, n_alt_masks-1 do begin
     	alt[i].path = get_pathname(alt_masks[i])
      	temp = file_basename(alt_masks[i])
	alt[i].corename = strmid(temp, 0, strlen(temp)-4)  ;get the core name iwthout bsq
	alt[i].year = strmid(alt[i].corename, 10, 4)
	alt[i].yearday = strmid(alt[i].corename, 10, 8)
  end

  ;get pathseparator
   psp = path_sep()


  ;for each unique ledaps year-day id, find corresponding alt masks and move them
  for i=0,n_ledaps_masks-1 do begin
    these_alt = where(alt.yearday eq ledaps[i].yearday, n_alt)
    if n_alt gt 1 then message, 'found two alternative masks for the same ledaps year/day combo:'+ledaps[i].yearday

    if n_alt eq 1 then begin 
	;use just this pathname to identify all of the files associated with this corename

            delete_these = file_search(ledaps[i].path, ledaps[i].corename+"*", count=num_to_delete)	;look for all instances of corename in that single directory

	;first check to see if we're moving files
       if n_elements(backup_ledaps_dir) ne 0 then begin 


	    for j = 0,  num_to_delete-1 do begin 
                proposed_name = backup_ledaps_dir + pse+ledaps.year+pse+file_basename(delete_these[j])
		check = file_exists(proposed_name)
	

		if check gt 0 then begin 
		   print, 'warning.  Mask file in backup directory already exists. Not overwriting.'
		   print, proposed_name
		end else file_move, delete_these[j], proposed_name
	    end 
	endif  ;done backing up files iff called for

	;now delete them
	;for j = 0, num_to_delete-1 do file_delete, move_these[j]


	;now move the existing alt maasks into their place
	move_these = file_search(alt[these_alt].path, alt[these_alt].corename+"*", count = num_to_move)	;find all files associated with this core name
	for j=0, num_to_move-1 do begin
		proposed_name = ledaps[i].path+ file_basename(move_these[j])
		check = file_exists(proposed_name)
		if check gt 0 then begin 
		  print, 'warning.  Alt mask file already exists in ledaps target directory.  Not overwriting.'
		  print, proposed_name
		  ;now check to see if this was in the list of ones to delete 
			undelete = where(delete_these eq proposed_name, n_undeletes)
			if n_undeletes ne 0 then delete_these[undelete] = 'keep'
		end else file_copy, move_these[j], proposed_name
		

		alt[these_alt].used = 1
	     
	end	;moving 
	;now delete the prior ones
	for j = 0, num_to_delete-1 do begin
		if delete_these[j] ne 'keep' then file_delete, delete_these[j]	;only delete if it wsan't the same
	end


	ledaps[i].used = 1

    endif else print, 'No alt mask found for '+ledaps[i].yearday ;if there is an alt mask for this ledaps mask

  endfor ;end going through all of the ledaps masks


  ;now check to see whether there are some alt-masks that did not have ledaps equivalents.
  missed_altmasks = where(alt.used eq 0, n_missed)
 if n_missed ne 0 then begin
	print, 'There were '+string(n_missed)+' alt masks that did not have ledaps equivalents.'
        for i = 0, n_missed-1 do begin 
	    print, alt[missed_altmasks[i]].yearday

	   ;now see if the yearday exists in the ledaps directory 
	    this_year = alt[missed_altmasks[i]].year

	    refl_files_thisyear = file_search(ledaps_dir+this_year+psp, '*ledaps.bsq', count = n_found)
	    refl_files_thisyear_names = file_basename(refl_files_thisyear)
	    refl_files_thisyear_yearday = strmid(refl_files_thisyear_names, 10, 8) 

	    ;check if this yearday matches the one we're after
	    match = where(refl_files_thisyear_yearday eq alt[missed_altmasks[i]].yearday, n_matches)
            if n_matches eq 1 then begin
	
	      ;find the alt-mask files associated with this yearday
		move_these = file_search(alt[missed_altmasks[i]].path, alt[missed_altmasks[i]].corename+"*", count=n_to_move)
		for j= 0, n_to_move-1 do begin
			proposed_name = ledaps_dir+this_year+psp+file_basename(move_these[j])

			if file_exists(proposed_name) ne 0 then begin 
				print, 'warning. Alt cloud mask already exists in ledaps dir'		
				print, proposed_name
			endif else file_copy, move_these[j], proposed_name
		end
		
		alt[missed_altmasks[i]].used=1

	    endif else  begin
		print, 'alt mask '+alt[missed_altmasks[i]].corename
		print, '  appears to have no ledaps equivalent. Check to ensure that ledaps omission is intentional'
 	    end
	  

	endfor ;going through missed ones
end ;section which tries to copy fixed ones

  print, "done moving masks"
end 
