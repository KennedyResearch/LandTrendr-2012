;this program will delete any trace of landtrendr files and

pro delete_lt_files, path, ppprrr, date, nowarning=nowarning
  
  message1 = transpose(["You have choosen to remove image dates from processing.  "+$
              "Would you like to move the files to a folder called 'images_not_used'? " $
              ,"","If you respond 'No' then the files will be deleted, if you respond 'Cancel' "+$
              "the deletion program will end without doing anything."])
  jump = dialog_message(message1, /cancel, /question)
  
  case jump of
    'No': move = 0 
    'Yes': move = 1
    'Cancel': return
  endcase
  
  if date[0] eq 0 then begin
    print, ""
    print, ">>> warning!!! you must define a date to delete..."
    print, ">>> in the 'deletedate' variable or this scene's batchfile"
    print, ">>> ending program"
    print, ""
    stop
  endif
  
  file_mkdir, strcompress(path+"\images_not_used", /rem) ;make a directory to hold unused images
  close, /all ;make sure IDL does not have any of the files open
  
  ;for the number of dates given to delete go through each of them
  for i=0, n_elements(date)-1 do begin
    ;get variations of how the date is described
    date1 = strtrim(string(date[i]),2)
    year = long(strmid(date1, 0,4))
    julday = long(strmid(date1, 4,3))
    ydn2md, year, julday, month, day
    
    if month lt 10 then month = strcompress('0'+string(month), /rem) else month = string(month)
    if day lt 10 then day = strcompress('0'+string(day), /rem) else day = string(day)
    year = string(year)
    julday = string(julday)
    
    ;make some search strings for matching
    ltsearchfor = strcompress("*"+year+"_"+julday+"*", /rem)
    vctsearchfor = strcompress("*"+year+month+day+"*", /rem)
    glovissearchfor = strcompress("*"+year+julday+"*", /rem)
    
    print, ""
    print, ">>> finding files to delete for: ", strcompress(year+"_"+julday, /rem)
    imagefiles = file_search(path, ltsearchfor, count=n_imagefiles)
    vctfiles = file_search(path+"vct\outputs\", vctsearchfor, count=n_vctfiles)
    glovisfiles = file_search(path+"glovis_targz\", glovissearchfor, count=n_glovisfiles)
    deletethese = [imagefiles,vctfiles,glovisfiles]
    theseones = where(deletethese ne '', n_deletethese)
    if n_deletethese ne 0 then begin
      deletethese = deletethese[theseones]
      if keyword_set(nowarning) eq 1 then begin
        if move eq 1 then begin
          movetodir = path+"images_not_used\"
          file_mkdir, movetodir ;make sure there is a directory to move images to
          base = file_basename(deletethese)
          newnames = movetodir+base
          file_move, deletethese, newnames, /allow_same, /overwrite
        endif else file_delete, deletethese, /allow_nonexistent, /quiet
        continue
      endif
      repeat begin
        print, ""
        print, ">>> are you sure you want to move/delete the following files:"
        print, "******************************************"
        print, ""
        print, transpose(deletethese)
        print, ""
        print, "******************************************"
        print, ">>> NO (press 1), YES (press 2)
        a = get_kbrd()
        print, ""
        if a eq '1' then print, "aborting moving/deletion of listed files"
        if a eq '2' then begin
          note = ''
          print, ""
          print, ">>> please type a short description of why you are moving/deleting this date..."
          print, ">>> example 1: 'too much cloudcover that cannot be masked without masking out basically the whole image'"
          print, ">>> example 2: 'for some reason this image date does not normalize well'"
          print, ""
          read, note, prompt = ">>> type your message:"
          print, ""
          print, ">>> thanks!"
          print, ""
          
          ;write out a txt file with the note and the image date
          dateremoved = transpose(["scene: "+string(ppprrr), "date: "+strcompress(year+"_"+julday, /rem), "note: "+note])
          filename = strcompress(path+"documentation\"+string(ppprrr)+"_"+year+"_"+julday+"_deletion_note.txt", /rem)
          openw, lun,filename,/get_lun
          printf, lun,dateremoved
          free_lun, lun
          
          ;delete the files
          if move eq 1 then begin
            movetodir = path+"images_not_used\"
            file_mkdir, movetodir ;make sure there is a directory to move images to
            base = file_basename(deletethese)
            newnames = movetodir+base
            file_move, deletethese, newnames, /allow_same, /overwrite
          endif else file_delete, deletethese, /allow_nonexistent, /quiet
          print, ""
          print, ">>> the above files have been moved/deleted..."
          print, ">>> if another copy of any of the listed..."
          print, ">>> files exists on another drive, server, or computer..."
          print, ">>> it/they should be moved/deleted as well for consistency..."
          print, ""
          print, ">>> would you like to search other directories?
          print, ">>> NO (press 1), YES (press 2)
          b = get_kbrd()
          
          ;check another directory
          if b eq '2' then begin
            repeat begin
              searchdir = dialog_pickfile(title='Select a directory to search for files to move/delete' ,/directory, /must_exist)
              imagefiles = file_search(searchdir, ltsearchfor, count=n_imagefiles)
              vctfiles = file_search(searchdir,vctsearchfor, count=n_vctfiles)
              glovisfiles = file_search(searchdir, glovissearchfor, count=n_glovisfiles)
              deletethese = [imagefiles,vctfiles,glovisfiles]
              theseones = where(deletethese ne '', n_deletethese)
              if n_deletethese ne 0 then begin
                deletethese = deletethese[theseones]
                repeat begin
                  print, ""
                  print, ">>> are you sure you want to move/detele the following files:"
                  print, "******************************************"
                  print, ""
                  print, transpose(deletethese)
                  print, ""
                  print, "******************************************"
                  print, ">>> NO (press 1), YES (press 2)
                  c = get_kbrd()
                  print, ""
                endrep until c eq '1' or c eq '2'
                if c eq '1' then print, "aborting moving/deletion of listed files"
                if c eq '2' then begin
                  if move eq 1 then begin
                    movetodir = path+"images_not_used\"
                    file_mkdir, movetodir ;make sure there is a directory to move images to
                    base = file_basename(deletethese)
                    newnames = movetodir+base
                    file_move, deletethese, newnames, /allow_same, /overwrite
                  endif else file_delete, deletethese, /allow_nonexistent, /quiet
            
                  repeat begin
                    print, ""
                    print, ">>> the above files have been moved/deleted..."
                    print, ">>> if another copy of any of the listed..."
                    print, ">>> files exists on another drive, server, or computer..."
                    print, ">>> it/they should be moved/deleted as well for consistency..."
                    print, ""
                    print, ">>> would you like to search other directories?
                    print, ">>> NO (press 1), YES (press 2)
                    d = get_kbrd()
                  endrep until d eq '1' or d eq '2'
                endif ;if c eq '2' then begin
              endif ;if n_deletethese ne 0 then begin
            endrep until b eq '1' or b eq '2'
          endif ;if b eq '2'
        endif ;if a eq '2' then begin
      endrep until a eq '1' or a eq '2'
    endif else begin
      print, ""
      print, ">>> the search returned no files for this date..."  ;if n_deletethese ne 0 then begin
      print, ""
    endelse
    
    ;check if this files is in madcal summary .sav file
    norm_files = file_search(path, "*madcal_results*", count=n_norm_info_file)
    if n_norm_info_file eq 0 then begin
      print, ""
      print, ">>> madcal result files do not exists"
      print, ""
    endif
    if n_norm_info_file ne 0 then begin
      norm_info_sav = where(strmatch(norm_files, "*madcal_results.sav") eq 1, n_norm_info_sav)
      norm_info_csv = where(strmatch(norm_files, "*madcal_results.csv") eq 1, n_norm_info_csv)
      if n_norm_info_sav ne 0 and n_norm_info_csv ne 0 then begin
        norm_info_sav = norm_files[norm_info_sav]
        restore, norm_info_sav
        norm_info_csv = norm_files[norm_info_csv]
        summarygoods = where(strmatch(norm_info.depfile, ltsearchfor) eq 1, n_summaryloc)
        if n_summaryloc ne 0 then summarygoods = where(strmatch(norm_info.depfile, ltsearchfor) eq 0)
        norm_info = norm_info[summarygoods]
        save, norm_info, filename=norm_info_sav
        export_structure_to_file, norm_info, norm_info_csv
      endif
    endif
  endfor
  print, ""
  print, ">>> done moving/deleting files
  print, ""
end