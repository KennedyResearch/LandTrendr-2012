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

function apply_multiple_radcals, run_info, overwrite = overwrite,$
	 min_nochange_count=min_nochange_count


  ;unpack structure

  file1= run_info.file1
  file2= run_info.file2
  results_path = run_info.results_path
  ignore = run_info.ignore
  subset_upls = run_info.subset_upls
  subset_size = run_info.subset_size
  run_name = run_info.run_name
  wfile = run_info.wfile
  dfile = run_info.dfile
  waterthresh=run_info.waterthresh
  cloudthresh=run_info.cloudthresh

  ;adjust the subset size for the pixel size of the image

  write_diagnosis, dfile, 'apply_multiple_radcals:Reading header information from '+file1
  zot_img, file1, hdr, dummy, /hdronly
  subset_size = subset_size * hdr.pixelsize

;now check the second image and come up with smallest area

   zot_img, file2, hdr2, dummy, /hdronly
   uplx = max([hdr.upperleftcenter[0], hdr2.upperleftcenter[0]])
   uply = min([hdr.upperleftcenter[1], hdr2.upperleftcenter[1]])
   lorx = min([hdr.lowerrightcenter[0], hdr2.lowerrightcenter[0]])
   lory = max([hdr.lowerrightcenter[1], hdr2.lowerrightcenter[1]])



  image_bounds = [ [uplx, uply], [lorx, lory]]
  write_diagnosis, dfile, 'apply_multiple_radcals:Image bounds are'+string(image_bounds)



  ;first, figure out if this one already exists


  write_diagnosis, dfile, 'apply_multiple_radcals:Setting up outfile '
  outfile = results_path+ 'radcal_'+run_name+'.sav'

  if n_elements(overwrite) eq 0 then overwrite = 0
  if not(file_exists(outfile)) or overwrite ne 0 then begin
    write_diagnosis, dfile, 'apply_multiple_radcals:  going through subsets and running madcal'

	;first, determine the true value for the cloud threshold.
	;   It should be offset by the start of the band (typically band 1)
	;   which can be quite variable.  To do this, we first find the location  in the
	;  histogram of the image where the numbers start to pick up sequentially.

	   if n_elements(cloudthresh) eq 3 then begin	;if it's set
	      
	   		zot_img, file2, temphdr, cldbndimg, layers=[cloudthresh[0]]
	   		jh = histogram(cldbndimg, omin=omin)
			jhs = shift(jh, 1)
			diff = jh-jhs

	   		njh = n_elements(jh)-1
	   		jhc = (cumulate(jh[1:njh])) / (lindgen(njh-1)+1)	;get mean of all previous
	   		diff = diff[1:njh]
	   		div = diff/jhc
	   		md = max(div)		;first peak is always highest because later ones penalied by
	   							;cumulative value of earlier peaks.

			wmd = where(div eq md, nwmd)
			if nwmd gt 1 then wmd = wmd[0]

			negatives = where(finite(div[0:wmd]) lt 0, n_negs)		;find small ones
			if n_negs ne 0 then lowval = max(negatives)	else lowval = wmd
					;find the highest negative just before the slope up to the peak
					; if there are no negatives, then just use the peak

			lowval = lowval + 1 + omin



;this didn't work because of fixed value
;	   		steeps = where(div gt 10, n_steeps)
;	   		steeps=steeps+1   ; to account for subsetting jh to 1:njh
;	   		if n_steeps eq 0 then stop
;			lowval = min(steeps)+omin	;find lowpoint
;


	   		cloudthresh_adj = cloudthresh+[0,lowval,0]
	   		cldbndimg = 0	;memory saving.
			print, 'lowval = '+string(lowval)
			wait, .2

		end





    ;go through subsets and run radcal

    n_subsets = n_elements(subset_upls)/2

	status_score = intarr(n_subsets)	;keep track of what happens for each subset


    for i = 0, n_subsets-1 do begin
      write_diagnosis, wfile, 'Starting subset '+string(i+1)+' of '+string(n_subsets)
      write_diagnosis, dfile, 'apply_multiple_radcals: starting subset '+string(i+1)+' of '+string(n_subsets)


      subsetname = strcompress('subset'+string(i+1), /rem)
      output_base = results_path + 'radcal_'+run_name+'_'+subsetname
      subset = [ [subset_upls[0,i], subset_upls[1,i]], $
        [subset_upls[0,i]+subset_size[0], subset_upls[1,i]-subset_size[1]] ]

      ;check that the subset actually occurs in this image
      write_diagnosis, dfile, 'apply_multiple_radcals:	testing edge'

      test = fix_edge(subset, image_bounds, /map, /noshrink)
      if test.valid eq 0 or total(abs(test.diffs)) ne 0 then begin
;        write_diagnosis, wfile, 'fatal error:  subset coordinates exceed bounds of image. failing.'
;        write_diagnosis, dfile, 'apply_multiple_radcals: subset coordinates exceed bounds of image. failing.'
;        return, {ok:0}
         write_diagnosis, wfile, 'subset coordinates exceed bounds of image for subset '+string(i+1)

		;we'll skip this one, but since this may be the first one
		;   we need to be able to write out the template. this also
		;   sets the correlation for this subset to 0


          band_stats = {slope:0., intercept:0., slope_sigma:0., intercept_sigma:0., $
			    rma_slope:0., rma_intercept:0., rma_rmse:0., $
			    stats_diff: fltarr(4), corr:0.}
			  band_stats = replicate(base, num_channels)
		ok = {band_stats:band_stats, status:5}   ;status 5 = subset out of bounds

      end else begin ;if subset is within bounds


		      write_diagnosis, dfile, 'apply_multiple_radcals: Beginning radcal adapted'

		      ok = radcal_adapted(file1, file2, subset, dfile, wfile,  $
		        run_info.footprint, output_base = output_base, ignore = ignore,$
		        waterthresh=waterthresh, cloudthresh=cloudthresh_adj, $
		        min_nochange_count=min_nochange_count, /noplot)

		      write_diagnosis, dfile, 'apply_multiple_radcals: done with radcal adapted'


		      ;check if problem radcal_adapted

		      if ok.ok eq 0 then begin
		        write_diagnosis, wfile, 'Problem in radcal adapted. Failing.'
		        return, {ok:0}
		      end


		      write_diagnosis, dfile, 'apply_multiple_radcals: updated summary stats'
	end


	  if n_elements(summary1) eq 0 then summary1 = ok.band_stats else expand_rows, summary1, 1

      summary1[*,i] = ok.band_stats
      layers = (size(summary1, /dim))[0]

  status_score[i] = ok.status		;1 = ok, 2=cloud threshold violated, 3=water thresh, 4=min nochange

    ; a = get_kbrd(1)

    end

    write_diagnosis, dfile, 'apply_multiple_radcals: done with loop for subsets'

    ;save the summary stuff

    write_diagnosis, dfile, 'apply_multiple_radcals: writing savefile'

    outfile = results_path+ 'radcal_'+run_name+'.sav'
    write_diagnosis, dfile, 'apply_multiple_radcals: '+outfile

    save, summary1, file = outfile



    ;print out results

    ;print out results


    write_diagnosis, wfile, 'slopes and intercepts from ortho regress:'
    write_diagnosis, wfile, ''
    for i = 0, n_elements(layers)-1 do begin 		;for the 6 bands
      write_diagnosis, wfile, '    -----------------------'
      write_diagnosis, wfile, 'BAND '+string(layers[i])
      write_diagnosis, wfile, '    '
      for j = 0,n_subsets-1 do write_diagnosis, wfile, string(summary1[i,j].slope + summary1[i,j].intercept)

    end

    write_diagnosis, wfile, ' '
    write_diagnosis, wfile, ' '
    write_diagnosis, wfile, 'slopes and intercept from rma regress'
    write_diagnosis, wfile, ''
    for i = 0, n_elements(layers)-1 do begin 		;for the 6 bands
      write_diagnosis, wfile, '    -----------------------'
      write_diagnosis, wfile, 'BAND '+string(layers[i])
      write_diagnosis, wfile, '    '
      for j = 0,n_subsets-1 do write_diagnosis, wfile, string(summary1[i,j].rma_slope + summary1[i,j].rma_intercept)

    end

  end 		;if the file doesn't exist already

  write_diagnosis, dfile, 'apply_multiple_radcals: done'

  return, {ok:1, outfile:outfile, status_score:status_score}

end
