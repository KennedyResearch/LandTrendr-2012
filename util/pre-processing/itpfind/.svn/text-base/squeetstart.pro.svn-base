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
; Copyright (c) 2001, Oregon State University.
; This software is currently in beta-test condition.  It may not be altered,
;   copied, or redistributed without express written permission of
;   Robert E. Kennedy, Oregon State University (kennedyr@fsl.orst.edu).
;   This software is supplied as is, with no express or implied
;   warranties.

;last updated April 30, 2001

pro squeetstart, params, ref, inp, passback=passback


;if passback is signalled, the program does NOT write anything to external
;    files.  Instead, it passes back to the calling program the start and
;    and end system times and the GCPs.


device, window_state = winstate
if winstate[0] eq 0 then w,0
if winstate[1] eq 0 then w,1, 600,600

  ;calculate relative rotation first

  rotation = inp.rotation - ref.rotation
  if abs(rotation) lt 0.001 then rotation = 0


;	;zippa = dialog_message('squeetstart: getting basic info', /info)

  orig_date = systime()
  ;STEP 1
  ;Get the basic info. on the files
   filenames = [ref.filename, inp.filename]
   for i = 0,1 do begin

     zot_img, filenames(i), hdr, a, /hdronly

     upl=edgit(hdr.upperleftcenter, hdr.pixelsize, /map)
     lor=edgit(hdr.lowerrightcenter, hdr.pixelsize, /lowerright, /map)

     if i eq 0 then $
        refinfo = {filename:ref.filename, upl:upl, lor:lor, $
        pixelsize:hdr.pixelsize, gifov:ref.gifov, centerpoint:ref.centerpoint,$
        layer:ref.layer, ignore:ref.ignore, maskbelow:ref.maskbelow, $
        maskabove:ref.maskabove} else $

        inpinfo = {filename:inp.filename, upl:upl, lor:lor, $
        pixelsize:hdr.pixelsize, gifov:inp.gifov, centerpoint:inp.centerpoint,$
        layer:inp.layer, ignore:inp.ignore,rotation:rotation, maskbelow:inp.maskbelow, $
        maskabove:inp.maskabove}
   end

    blowupby = refinfo.gifov / inp.gifov


    ;SET MINDISTS AND MINCOUNTS
    ;  Used for determining whether a given point is unusual relative to
    ;  the other points.  Mindist is the minimum distance by which two
    ;  points must be separated in order to get an estimate for gifov
    ;  mincount is the minimum number of appropriately-separated points
    ;  necessary to get the mean estimate of gifov.


      mindist = [50,50]
      mincount = [25,25]

	;zippa = dialog_message('squeetstart: just before pathanme', /info)


   ;DETERMINE PATH NAME FOR ALL FILES ASSOCIATED WITH THIS RUN
      ;determine base file name from input file path and from ref.file

if !version.os_family eq "Windows" then slash = '\' else slash = '/'

        pathname= ''
        f = str_sep(filenames(1), slash)    ;input file
        g= n_elements(f)
        for i = 0, g-2 do begin
         pathname= pathname+f(i)+slash
        end
        inpcode = inp.codename
        f= str_sep(filenames(0), slash)   ;ref file
        g=n_elements(f)
        q= f(g-1)
        refcode= ref.codename
        pathname=strcompress(pathname+inpcode+refcode, /remove_all)

      ;add the date
        f= str_sep(orig_date, ' ')

        if f(2) eq '' then begin
            time = str_sep(f[4], ':')
            time = strcompress(time[0]+'_'+time[1], /remove_all)

             datetime=strcompress(f(1)+'_'+f(3)+'_'+time, /remove_all)
          end else begin
            time = str_sep(f[3], ':')
            time = strcompress(time[0]+'_'+time[1], /remove_all)
             datetime=strcompress(f(1)+'_'+f(2)+'_'+time, /remove_all)
          end

        pathname_file = strcompress(pathname+$
           datetime, /remove_all)
      ;set up extensions for GCP file
      ext = ['_xref.txt', '_yref.txt', '_xinput.txt', '_yinput.txt']


	;zippa = dialog_message('squeetstart: just before diagnosis file', /info)

     ;set up the diagnosis file


       diagnosis_file = pathname_file+'diagnosis.txt'
			 diag_un1 = diagnosis_file
			 write_diagnosis, diag_un1, 'Diagnosis for '+systime()
;       openw, diag_un1, diagnosis_file, /get_lun
;       printf, diag_un1, 'Diagnosis for ',systime()

       ;diag_un1 = -2    ;uncomment this if you want comments on screen

       ;diagnosis_file2= strcompress(pathname_file+'_diagnosis.idl', /remove_all)
       ;openw, diag_un2, diagnosis_file2, /get_lun
       diag_un2 = -1

  ;STEP 2
  ;LOOK FOR POINTS.  Keep running through until we've hit the wall
  ;   on all four sides.
	;zippa = dialog_message('squeetstart: just before sq_reset_dirvect', /info)


  gcp_count = 0
  average_move = dblarr(2,2)
  sq_reset_bigdirvect, dirvect
  ;rotvect = calc_rot_vect(inpinfo.rotation)
  ;backrotvect = calc_rot_vect((-1)*inpinfo.rotation)

  rotvect = calc_rot_vect((-1)*inpinfo.rotation)    ;changed may27
  backrotvect = calc_rot_vect(inpinfo.rotation)



  original_inpgifov = inpinfo.gifov


  ;set up move-adjuster to 1 initially

  move_adj_mult = .2    ;the 'slop' factor when we move a long ways --
         ;the number of window_spacings gt 1 is multiplied
         ;by this value -- i.e. if move =2.3 *window_spacing,
         ;then the move_adj would be 1 + (move_adj_mult * 1.3)

  move_adj = 1

  ;draw a window to show progress

    col = 'ffee33'Xl
    xpp = [inpinfo.upl(0), inpinfo.lor(0)]
    ypp = [inpinfo.upl(1), inpinfo.lor(1)]
    plot, [xpp(0),xpp(0), xpp(1), xpp(1), xpp(0)], $
        [ypp(1), ypp(0), ypp(0), ypp(1), ypp(1)], color =col

    status_count = 0    ;we keep track of the validity of every try
             ;of the gcp finder, and this is the index
             ;of how many tries we've made

           ;The status variable:  binary type
           ;  0  if no problems
           ;  1  if not valid from squeeter
           ;  2  if movement too great relative to other points
           ;  4  if gifov didn't work

	;zippa = dialog_message('squeetstart: just before loop', /info)


  loop: ;the loop that incrementally gets GCPs

    ;diagnosis
    ;if diag_un1 ne -2 then openu, diag_un1, diagnosis_file, /get_lun, /append
  ;      printf, diag_un1,

	 write_diagnosis, diag_un1, 'Gcp_count+1: '+string( gcp_count+1)


    orig_ref_centerpoint = refinfo.centerpoint
    orig_inp_centerpoint = inpinfo.centerpoint
        diagstring = 'The proposed center point in the reference image is: '+string(refinfo.centerpoint)
        write_diagnosis, diag_un1, diagstring
          diagstring = 'The proposed center point in the input image is: '+string(inpinfo.centerpoint)
        write_diagnosis, diag_un1, diagstring



;
;        printf, diag_un1, 'The proposed center point in the reference image is:'
;        printf, diag_un1, '    '+string(refinfo.centerpoint)
;        printf, diag_un1, '    In the input images is:'
;        printf, diag_un1, '    '+string(inpinfo.centerpoint)
;


    sq_resetdirvect, dirvect_point

    searchloop: ;the loop that searches until a GCP is found


      ;GET POINTS

       ;If the program has moved a good distance from the previous point,
       ;   then we need to make give squeeter some slop to work with
       ;   in case there is any slight problem in GIFOVs

;
;       printf, diag_un1, '=-----='
;       printf, diag_un1, 'Before call to squeeter'
;       printf, diag_un1, '    refinfo'
;       printf, diag_un1, refinfo.centerpoint
;       printf, diag_un1, '    inpinfo'
;       printf, diag_un1, inpinfo.centerpoint
;       printf, diag_un1, '-------'

        write_diagnosis, diag_un1, 'Before call to squeeter'

	     ;zippa = dialog_message('squeetstart: just before call to squeeter', /info)


       gcpinfo = squeeter(refinfo, inpinfo, params, diag_un1=diag_un1, $
               diag_un2=diag_un2)

        write_diagnosis, diag_un1, 'Returned from squeeter'



;
;       printf, diag_un1, '=-----='
;       printf, diag_un1, 'After call to squeeter'
;       printf, diag_un1, '    refinfo'
;       printf, diag_un1, refinfo.centerpoint
;       printf, diag_un1, '    inpinfo'
;       printf, diag_un1, inpinfo.centerpoint
;       printf, diag_un1, '-------'
;
;       ;make a space in the status variable for the validity of this try

         status_count = status_count + 1
         if status_count eq 1 then status=[0] else $
              expand_rows, status,1,newdims

      if status_count gt 4 then $
           write_diagnosis, diag_un1, '      '+string(status[status_count-3:status_count-1])

;         printf, diag_un1, '+++++++++++'
;         printf, diag_un1, '  '
;         printf, diag_un1, '    status'
;         if status_count gt 4 then $
;            printf, diag_un1, '      '+string(status[status_count-3:status_count-1])
;         printf, diag_un1, '+++++++++++'


       ;for diagnosis only

         ;openw, 1, 'statusfile.txt'
         ;printf, 1, status_count, status
         ;close, 1



       ;IF NOT VALID IN SQUEETER, THEN MOVE WINDOW

         if gcpinfo.valid eq 0 then begin   ;if we need to move the window.


					write_diagnosis, diag_un1, 'Not valid, moving window'

           ;update the status count
              status[status_count-1] = status[status_count-1] or 1

           ;move the center
              sq_movecenter, dirvect_point, refinfo, inpinfo, params, $
                   maxmove=params.maxmove, /nudge
;                 printf, diag_un1, refinfo.centerpoint
;                 printf, diag_un1, inpinfo.centerpoint


           ;if we couldn't find any points here, we need just bail and go
           ;    to the next candidate position based on the movement of
           ;    window_spacing


              if dirvect_point.valid eq 0 then goto, bigmove

           ;or else go back and try again nearby
              goto, searchloop
         end

    ;DETERMINE HOW FAR WE WENT AND SEE IF IT'S LIKE THE OTHER
    ;    POINTS WE'VE FOUND SO FAR


      ;First determine how far it actually moved relative
      ;   to what we expected

         rotinpmove = backrotvect#transpose(gcpinfo.inp_move)
         equivref= ( (rotinpmove/inpinfo.pixelsize)/ $
                  (refinfo.gifov/inpinfo.gifov))* $
                  refinfo.pixelsize
         tmd = gcpinfo.ref_move - equivref
         ;printf, diag_un1, 'gcpinfo.conf'+string(gcpinfo.conf)



         ;Standardize this to one window-spacing, so that
         ;   if we're going further than one window-spacing
         ;   we allow more slop

         if gcp_count gt 0 then distance = sqrt( $
           total(( (gcpinfo.ref_gcp-gcp[0:1,gcp_count-1])/ $
             (refinfo.pixelsize*params.window_spacing))^2) $
                  )+1 else distance = [1,1]

         tm = tmd/[distance, distance]

;         printf, diag_un1, 'squeetstart: Actual move = '+string(tmd)
;         printf, diag_un1, 'squeetstart: Total adjusted move = '+string(tm)


     ;if we're doing rubber sheeting, we don't need this check, and it
     ;  becomes untenable with a lot of itps.  therefore, turn it
     ;  off (added september 12, 2004)
;      tts = systime(/seconds)

      if params.gifov_test eq 1 then begin

         ;Then check to see if the input gifov estimate makes
         ;   sense.  Call sq_get_gifov.  It returns a structure
         ;   with the mean of the gifov estimates from all
         ;   prior points, after first taking out outliers.
         ;   It also returns the standard deviation of the estimates
         ;   of gifov from all prior gcp pairs.
         ;  {gifov: <mean value X,Y>, $
         ;   stdv_gifov: <stdv of values X,Y>, $
         ;   valid: X,Y


       thisgcp = [gcpinfo.ref_gcp, gcpinfo.inp_gcp]

       write_diagnosis, diag_un1, 'calling sq_get_gifov'

       dummy = sq_get_gifov(gcp, refinfo, inpinfo, $
              backrotvect, thisgcp=thisgcp, $
              confthisgcp=gcpinfo.conf, $
              mindist = mindist, mincount=mincount)
     end else thisgcp = [1,1]		;just a dummy value to trick
     														;the rest into not processing gcp


;      print, systime(/seconds) - tts
;      wait, .5
;     ;If thisgcp is changed to 0, then the new gcp is weird
     ;   relative to the prior points.  If it's one, then we're okay


       if total(thisgcp) ne 2 then begin
              ;POINT IS NOT OKAY BASED ON GIFOV TEST, NUDGE IT

                write_diagnosis, diag_un1, 'NOT VALID BASED ON GIFOV TEST'


                 status[status_count-1] = status[status_count-1] or 4
                 sq_movecenter, dirvect_point, refinfo, inpinfo, params, $
             maxmove=params.maxmove, /nudge

           ;check out the movement -- if not valid, then we
           ;  just skip to the next

             if dirvect_point.valid eq 0 then begin
                   dirvect.hanchor = [[orig_ref_centerpoint], $
                   [orig_inp_centerpoint]]
                 goto, bigmove
             end

                  ;If this point was odd, and we haven't used up the
           ; the options in the neighborhood, keep
           ; trying

             goto, searchloop

       end else $
            begin
              ;POINT IS OKAY* -- ADJUST EVERYTHING
              ;   (* or there aren't enough other points to test it)

              write_diagnosis, diag_un1, 'POINT IS OKAY BASED ON GIFOV TEST'

              if gcp_count eq 0 then begin
			           gcp = dblarr(4,1)
			           bulk_move = dblarr(2,1)   ;for total ref movement
			         end else begin
			               expand_rows, gcp, 1, newdims
			               expand_rows, bulk_move, 1, newdims
			         end

              gcp(*,gcp_count) = [gcpinfo.ref_gcp, gcpinfo.inp_gcp]


              bulk_move(*,gcp_count) = tmd  ;total move can be set if this
                  ;was a good point

             ;plot the point
               wset, 0
               plot, [xpp(0),xpp(0), xpp(1), xpp(1), xpp(0)], $
                [ypp(1), ypp(0), ypp(0), ypp(1), ypp(1)], /ynozero, color =col
               oplot, [gcp(2,*)], [gcp(3,*)], psym = 1, color =col

             ;If we're not passing back to a calling program, then write
             ;  the values to a file

              if n_elements(passback) eq 0 then write_gcps, pathname_file, ext, gcp
;stop


						 ;if we're doing gifov test, then update gifov
             ;Set the gifov to the gifov based on new and improved
             ;    GCP set

             if params.gifov_test eq 1 then begin
               newgifov = sq_get_gifov(gcp, refinfo, inpinfo, backrotvect)

               if total(newgifov.valid) eq 2 then inpinfo.gifov = newgifov.gifov
						 end

                ;Move around the reference centerpoint to better match it
                ;  up next time around
                ;adjust reference based on offset from this point

                  dirvect.hanchor[0:1] = dirvect.hanchor[0:1] - $
                  tmd    ;made into tmd from tm, april 27 1999
                      ;It should be the real move, not adjusted


          end



    ;MOVE THE CENTER POINT IN PREP FOR THE NEXT ONE

       gcp_count = gcp_count + 1
       bigmove:

       ;if diag_un1 ne -2 then free_lun, diag_un1
;         printf, diag_un1, 'squeetstart:  ref and inp centerpoints before bigmove'
;         printf, diag_un1, refinfo.centerpoint
;         printf, diag_un1, inpinfo.centerpoint

       zz = [refinfo.centerpoint, inpinfo.centerpoint]

       write_diagnosis, diag_un1, 'calling sq_bigmove'

       sq_bigmove, dirvect, refinfo, inpinfo, params, movevalue=movevalue
       write_diagnosis, diag_un1, 'Returned from sq_bigmove'



;         printf, diag_un1, 'squeetstart: Movement after sq_bigmove'
;
;         printf, diag_un1, [refinfo.centerpoint, inpinfo.centerpoint]-zz
;         printf, diag_un1, 'squeetstart: Input GIFOV '
;         printf, diag_un1, inpinfo.gifov


      ;If we're done, goto the final save

        if dirvect.done eq 1 then goto, savepoints

    ;if we're not done, go back to the loop
;printf, diag_un1, 'waiting for keyboard'

;h = get_kbrd(1)
  goto, loop


   ;SAVE POINTS

   savepoints:
     ; if diag_un1 ge 0 then free_lun, diag_un1
      if diag_un2 ge 0 then free_lun, diag_un2


		 ;july 21, 2005
		 ;if we are supposed to filter out the points, do it here
		 ;
			if params.postfilterrms ne 0 then begin
			  tempgcp = gcp
			  u = threshold_gcps(tempgcp, params.postfilterrms, 1, tracker=tracker)
			  gcp = u.gcps
			  new_gcp_count = size(gcp, /dim)
			  if n_elements(new_gcp_count) lt 2 then new_gcp_count = 0 else new_gcp_count=new_gcp_count[1]

			end else new_gcp_count=gcp_count


     ;give diagnostic info
       write_diagnosis, diag_un1, 'squeetstart: Started at: '+string( orig_date)


       finish_time = systime()
       write_diagnosis, diag_un1, 'squeetstart: Finished at: '+string( finish_time)
       write_diagnosis, diag_un1, 'squeetstart: Original Number of GCPs found = '+string( gcp_count)
			 write_diagnosis, diag_un1, 'squeetstart: Post-filtered gcps = '+string( new_gcp_count     )

   if n_elements(passback) eq 0 then begin  ;if we're not
                  ;passing parameters back, then
                  ;we write to file


     ;set up extensions

        readme_file = pathname_file+'.readme'
        ext = ['_xref.txt', '_yref.txt', '_xinput.txt', '_yinput.txt']


        write_diagnosis, diag_un1,  'squeetstart: Files written:'
        write_diagnosis, diag_un1, 'readme file' + readme_file

     ;write readme file

        openw, un, readme_file, /get_lun
        printf, un,  'Readme information for GCP files with the following
        printf, un,  'foundation name: '
        printf, un,  pathname_file
        printf, un,  'Created with Squeeter at ', finish_time
        printf, un,  ' '
        printf, un,  'Number of GCPs: '+ string(new_gcp_count)
        printf, un,  'Start time: '+ orig_date
        printf, un,  ' '
        printf, un,  'Reference File: ''
        printf, un,  filenames(0)
        printf, un,  'Input File: '
        printf, un,  filenames(1)
        printf, un, 'Original coordinates of input images.'
        printf, un, 'Reference Image: ', $
            edgit(refinfo.upl, refinfo.pixelsize, /map, /tocenter), $
            edgit(refinfo.lor, refinfo.pixelsize, /map, /tocenter, /lowerright)
        printf, un, 'Input Image: ', $
            edgit(inpinfo.upl, inpinfo.pixelsize, /map, /tocenter), $
            edgit(inpinfo.lor, inpinfo.pixelsize, /map, /tocenter, /lowerright)

        printf, un, 'Parameters used in Squeeter:'

        ;write parameters using standard program
          sq_rw_params, ' ', params, /write, unit = un

        ;write each image's critical info

          ok = sq_rw_imagefile(' ', ref, /write, unit = un)
          ok = sq_rw_imagefile(' ', inp, /write, unit = un)



        free_lun, un



     ;write gcp files

      write_gcps, pathname_file, ext, gcp

   end else begin   ;if we are passing back to the calling program

       passback = {start:orig_date, finish:finish_time, gcp:gcp}
   end


;	zippa = dialog_message('done!', /info)


 return
 end


