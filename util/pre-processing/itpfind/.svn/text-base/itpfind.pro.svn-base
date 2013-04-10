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


;
; Copyright (c) 1998, Oregon State University.
; This software is currently in beta-test condition.  It may not be altered,
;   copied, or redistributed without express written permission of
;   Robert E. Kennedy, Oregon State University (kennedyr@fsl.orst.edu).
;   This software is supplied as is, with no express or implied
;   warranties.

;last update:  may20th


pro itpfind_handler, ev


  widget_control, ev.id, get_uvalue = name

  child = widget_info(ev.top, /child)
  widget_control, child, get_uvalue = pass


  ddd = 0
  case 1 of


    (name eq 'done'):  $
      begin
      widget_control, ev.top, /destroy
      ddd=1
    end
    (name eq 'sel_tab'): $
      if ev.type eq 4 then begin   ;if user selects cell
      if total(ev.sel_left+ev.sel_right+ev.sel_top+ev.sel_bottom) $
        eq -4 then goto, notyet

      ;the sel_top and sel_bottom must be the same

      if ev.sel_top ne ev.sel_bottom then begin
        widget_control, pass.status_win, $
          set_value = '*** Select only 1 cell ***', /append
        goto, notyet
      end

      ;so we have cell.  Now set the selected file

      pass.selected_file = ev.sel_bottom
      pass.selected_type = 0    ;this is file type, not parameter
      widget_control, pass.selected_file_win, $
        set_value = pass.file_info.filename(pass.selected_file)


      notyet:
    end else widget_control, pass.status_win, $
      set_value = '*** Please select a full cell ***', /append


    (name eq 'par_tab'):  $
      if ev.type eq 4 then begin
      ;the sel_top and sel_bottom must be the same

      if ev.sel_top ne ev.sel_bottom then begin
        widget_control, pass.status_win, $
          set_value = '*** Select only 1 cell ***', /append
        goto, notyet
      end

      ;we have the parameter cell.  Set selected file and type to param

      if ev.sel_bottom ne -1 then begin
        pass.selected_file = ev.sel_bottom
        pass.selected_type = 1  ;this is parameter type of file
        widget_control, pass.selected_file_win, $
          set_value = pass.param_filenames(pass.selected_file)
      end



    end




    (name eq 'pairs_tab'): $
      if ev.type eq 4 then begin
      if total(ev.sel_left+ev.sel_right+ev.sel_top+ev.sel_bottom) $
        eq -4 then goto, notyet2


      ;CHECK AGAINST MULT. CELLS
      ;the sel_top and sel_bottom, right and left must be the same

      if ev.sel_top ne ev.sel_bottom or $
        ev.sel_right ne ev.sel_left then begin
        widget_control, pass.status_win, $
          set_value = '*** Select only 1 cell ***', /append
        goto, notyet2
      end


      ;ASSIGN FILEPOINTERS TO CELLS
      ;so they picked a cell. Which one?

      cellpoint = [ev.sel_left, ev.sel_top]

      ;we can't put a parameter file into columns 0,1 or
      ;   an image file into column 2

      if (pass.selected_type eq 1 and cellpoint(0) lt 2) or $
        (pass.selected_type eq 0 and cellpoint(0) eq 2) then $
        begin
        widget_control, pass.status_win, $
          set_value = '*** Inappropriate type for selected cell ***'
        goto, notyet2
      end



      ;assign the 'selected_file' to this one

      if pass.selected_file eq -1 then begin
        widget_control, pass.status_win, $
          set_value ='*** No selected file ***'
        goto, notyet2
      end
      pass.pairs(cellpoint(0), cellpoint(1)) = pass.selected_file


      ;FILLED TABLE?  RESIZE IF NECESSARY
      ;taken out 9/9/03 to enable VM version.

      ;get number of pairs
      count = n_elements(pass.pairs)/( (dims(pass.pairs))[0])

      ;if pass.pairs(0,count-1) ne -1 and $
      ;    pass.pairs(1,count-1) ne -1 then begin

      ;                    ;add a row.  Need to resize the structure
      ;                      z= pass.pairs
      ;                      expand_rows, z, 1, n
      ;                      count = count + 1
      ;
      ;                      z(0, count-1) = [-1,-1, -1]
      ;                      pass = change_str(pass, 'pairs', z)
      ;
      ;                  ;add a row to the table
      ;
      ;                    widget_control, pass.pairs_tab, /insert_row
      ;              end


      ;WRITE THE NEW TABLE TO THE SCREEN
      ;set up the strings of files

      values = strarr(3, count)

      for i = 0, count-1 do begin
        ff = pass.pairs(0,i)
        if ff ne -1 then values(0,i) = pass.file_info.filename(ff) else $
          values(0,i) = '> '

        ff = pass.pairs(1,i)
        if ff ne -1 then values(1,i) =  pass.file_info.filename(ff) else $
          values(1,i) = '> '

        ff = pass.pairs(2,i)
        if ff ne -1 then values(2,i) = pass.param_filenames(ff) else $
          values(2,i) = '> '

      end



      ;pass those values to the widget table

      widget_control, pass.pairs_tab, set_value = values

      notyet2:
    end


    (name eq 'doit'):  $
      begin

      ;zippa = dialog_message('Starting run of ITPFIND', /info)

      ;get number of pairs
      ; as of 9/9/03, this is different from before
      ;  because I don't do structure changing on the fly in the virtual
      ;  machine because of the problem with the "execute" statement
      ;  in the change_str routine.  therefore, I set a hard limit on the number
      ;  image pairs that can be done.  Because of this, there will be
      ;  potentially many -1's in there.


      count = n_elements(pass.pairs)/( (dims(pass.pairs))[0])

      ;since there will be -1's in the last column, take care of
      ;   this
      diff = 0
      diffdo:
      diff = diff+1
      a= pass.pairs(0,count-diff) eq -1 or $
        pass.pairs(1,count-diff) eq -1
      if a ne 0 then goto, diffdo

      good_pairs = pass.pairs(*,0:count-diff)
      good_count = count-diff+1

      ;call squeetstart for each of these

      for i = 0, good_count -1 do begin


        refpt=pass.pairs(0,i)
        inppt=pass.pairs(1,i)
        parpt=pass.pairs(2,i)

        ref_info = {filename:pass.file_info.filename(refpt), $
          ignore:pass.file_info.ignore(refpt), $
          centerpoint:pass.file_info.centerpoint(*,refpt),$
          gifov:pass.file_info.gifov(*,refpt), $
          rotation:pass.file_info.rotation(refpt), $
          codename:pass.file_info.codename(refpt), $
          layer:pass.file_info.layer(refpt), $
          maskbelow:pass.file_info.maskbelow(refpt), $
          maskabove:pass.file_info.maskabove(refpt)}
        inp_info = {filename:pass.file_info.filename(inppt), $
          ignore:pass.file_info.ignore(inppt), $
          centerpoint:pass.file_info.centerpoint(*,inppt),$
          gifov:pass.file_info.gifov(*,inppt), $
          rotation:pass.file_info.rotation(inppt), $
          codename:pass.file_info.codename(inppt), $
          layer:pass.file_info.layer(inppt), $
          maskbelow:pass.file_info.maskbelow(inppt), $
          maskabove:pass.file_info.maskabove(inppt)}

        ;zippa = dialog_message('calling sq_rw_params', /info)

        sq_rw_params, pass.param_filenames(parpt), params



        ;zippa = dialog_message('calling squeetstart', /info)

        squeetstart, params, ref_info, inp_info

      end






    end







    else:
  endcase

  if ddd ne 1 then widget_control, child, set_uvalue = pass, /no_copy

  return
end



pro itpfind


  ;
  ; NAME:  itpfind
  ;
  ;
  ; PURPOSE:  Widget-based program to automatically find GCPs
  ;
  ;
  ; CATEGORY:  Geometric registration
  ;
  ;
  ; CALLING SEQUENCE:  Called from main
  ;
  ;
  ; PROCEDURES/FUNCTIONS CALLED:
  ;
  ;
  ; INPUTS:
  ;
  ; OPTIONAL INPUT PARAMETERS:
  ;
  ;
  ; KEYWORD Parameters:
  ;
  ;
  ; OUTPUTS:  Produces
  ;
  ;
  ; COMMON BLOCKS:
  ;
  ;
  ; SIDE EFFECTS:
  ;
  ;
  ; RESTRICTIONS:
  ;
  ;
  ; PROCEDURE:
  ;
  ;
  ; EXAMPLES:
  ;
  ; MODIFICATION HISTORY:
  ;
  ;
  ;


  ;Layout overview:  Part I:  Establish parameters and set up widget layout
  ;        Part II:  Make call to SQUEET_handler.pro


  ;****************************************************************************
  ;             PART I
  ;****************************************************************************


  ;XXXXXXXXXXXXXXXXX
  ;READ IN VARIABLES
  ;XXXXXXXXXXXXXXXXX


  ;There is a master file that holds the name of the default parameter file.
  ;  The default parameter file name can change if user does so in the program,
  ;  but the master file name cannot
  ;*(def_file= string, filename of parameter defaults file)

  userhome = getenv('home')
  if file_exists('paramfiles/squeet_master.txt') then $
    masterfile = 'paramfiles/squeet_master.txt' else $
    masterfile = dialog_pickfile(path=userhome, /read, /must_exist, $
    title = 'Please identify squeet_master.txt file')


  ;masterfile = 'C:\_Robert\Current\library\idl\itp_find\paramfiles\squeet_master.txt'
  ; masterfile = 's:\for_rek\squeet_master.txt'



  if not(file_exists(masterfile)) then begin
    out = dialog_message('squeet_master.txt file not found. failing', /error)
    return
  end

  openr, un, masterfile, /get_lun
  query_file, un, 'File of available parameter files', param_files
  if n_elements(param_files) eq 1 then begin
    if param_files[0] eq 'no_match' then begin
      out = dialog_message('File of available parameter files not found. failing', /error)
      free_lun, un
      return
    end
  end

  query_file, un, 'File of available images', image_file
  if n_elements(image_file) eq 1 then begin
    if image_file[0] eq 'no_match' then begin
      free_lun, un
      out = dialog_message('File of available images not found. failing', /error)
      return
    end
  end
  free_lun, un


  ;Get names of parameter files

  if file_exists(param_files) eq 0 then begin
    errormessage = strcompress('File of available parameter files '+param_files+' not found. Check for full path name.')
    free_lun, un
    out = dialog_message(errormessage, /error)
    return
  end

  openr, un, param_files, /get_lun

  pcount = 0
  a=''
  while not eof(un) do begin
    pcount = pcount+1
    if pcount eq 1 then param_filenames = strarr(1) else $
      expand_rows, param_filenames, 1, newdims
    readf, un, a
    param_filenames(pcount-1) = a
  end
  free_lun, un

  ;Read the available image file
  if file_exists(image_file) eq 0 then begin
    errormessage = strcompress('File of available image files '+image_file+' not found. Check for full path name.')
    out = dialog_message(errormessage, /error)
    return
  end
  ok = sq_rw_imagefile(image_file, file_info)
  if ok eq 0 then begin
    errormessage = strcompress('Problems reading image files. Failing.')
    out = dialog_message(errormessage, /error)
    return
  end

  ;Make sure that the available image array is in correct form

  quz = size(file_info.filename, /dim)
  if n_elements(quz) eq 2 then file_info.filename = reform(file_info.filename)




  ;Set up non-changing variables

  disp_win_size = [500,500]



  ;XXXXXXXXXXXXXXXXXXXX
  ;   SET UP WIDGET
  ;XXXXXXXXXXXXXXXXXXXX

  ;Set up layout
  maxpairs = 10    ;determines the size of the image pair table.



  base = widget_base(/col, title = 'ITPFIND v2.2')
  storeit = widget_base(base)    ;child to store variables to be passed

  ;Set up selection table

  sel_base = widget_base(base, /row)
  column_labels = ['Available files']
  if n_elements(size(file_info.filename, /dim)) eq 2 then $
    values = file_info.filename else $
    values = [ [transpose(file_info.filename)]]

  sel_tab = widget_table(sel_base, column_labels=column_labels, value = values, $
    /resizeable_columns, x_scroll_size = 100, y_scroll_size = 50, scr_ysize = 200,$
    column_widths = 500, units = pixels, uvalue = 'sel_tab', /all_events)

  text_base = widget_base(sel_base, /col)

  ;list parameter files

  values = param_filenames
  collab = ['Available parameter files']
  sel_par = widget_table(text_base, column_labels = collab, $
    value = values, /resizeable_columns, x_scroll_size =100, $
    scr_ysize = 200, y_scroll_size = 30, $
    column_widths= 500, units = pixels, uvalue = 'par_tab', /all_events)




  a=widget_label(text_base, value = 'Selected file:')
  selected_file_win=widget_text(text_base, xsize = 40, ysize= 1, value = '')


  status_win = widget_text(text_base, xsize = 40, ysize= 5, /scroll, $
    value = ['.....'], uvalue = 'textplace')

  doit = widget_button(text_base, value = 'Submit for processing', $
    uvalue = 'doit')

  ;Set up the pairs table
  column_labels = ['Reference image', 'Input image', 'Parameter file']
  values = replicate( '>', 3, maxpairs)  ;initialize pairs section ;added 9/9/03 for vm
  pairs = replicate(-1, 3, maxpairs)   ; added 9/9/03 for vm

  pairs_tab= widget_table(base, column_widths = [350,350,350], units= pixels, $
    /resizeable_columns, value = values, column_labels=column_labels,  $
    /editable, uvalue = 'pairs_tab', /all_events, scr_ysize = 200)


  ;Quit

  quitbase = widget_base(base, /row)
  done = widget_button(quitbase, uvalue = 'done', value = 'Done')

  ;Draw Widget

  widget_control, base, /realize


  ;widget_control, refbase, get_value = ref_win_index
  ;o= randomu(seed, 600,600)*255
  ;wset, ref_win_index
  ;tvscl, o
  ;help, o
  ;Set up structure to pass info.

  pass = {param_filenames:param_filenames, file_info:file_info, $
    status_win:status_win, selected_type:[0], $
    selected_file:-1, selected_file_win:selected_file_win, $
    pairs_tab:pairs_tab, pairs:pairs}

  widget_control, storeit, set_uvalue = pass, /no_copy

  ;Call xmanager and start widget interactions

  xmanager, 'itpfind', base, event_handler = 'itpfind_handler'

  return


  past:
end








