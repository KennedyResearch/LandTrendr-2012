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

function regress_from_nochange_dist, nochange_info,  output_base, dfile, wfile, ignore=ignore, $
			screen_prob = screen_prob, apply_to_entire_image=apply_to_entire_image, $
			ddist_screen = dist_screen


  ;first, assess the smallest area common to all
  ;  three images

  write_diagnosis, wfile, 'regress_from_nochange: reading header info'


  zot_img, nochange_info.nochange_file, noch_hdr, mask_nochange, /hdronly
  zot_img, nochange_info.fname1, h1, img1, layers=[1], /hdronly
  zot_img, nochange_info.fname2, h2, targetimg, layers=[1], /hdronly

  subset = [ [max([noch_hdr.upperleftcenter[0], h1.upperleftcenter[0], h2.upperleftcenter[0]]), $
    min([noch_hdr.upperleftcenter[1], h1.upperleftcenter[1], h2.upperleftcenter[1]])], $
    [min([noch_hdr.lowerrightcenter[0], h1.lowerrightcenter[0], h2.lowerrightcenter[0]]), $
    max([noch_hdr.lowerrightcenter[1], h1.lowerrightcenter[1], h2.lowerrightcenter[1]])] ]

  ;print, noch_hdr.upperleftcenter, noch_hdr.lowerrightcenter
  ;print, h1.upperleftcenter, h1.lowerrightcenter
  ;print, h2.upperleftcenter, h2.lowerrightcenter
  ;
  ;print, 'subset'
  ;print, subset
  ;
  ;stop
  ;
  ;

  ;check on the logfile_un

  if n_elements(logfile_un) eq 0 then logfile_un = -1	;set to standard output.



  ;read in the mask image

  write_diagnosis, dfile, 'regress_from_nochange: reading in mask image'
  orig_subset = subset

  zot_img, nochange_info.nochange_file, noch_hdr, mask_nochange, subset=subset
  mask_nochange = where(mask_nochange eq 1, num_nochange)
  subset = orig_subset


  ;open the output write file

  output_file = output_base+'.bsq'


  ;to match with radcal, split the nochange into different groups
  ; holdout 1/3 for test purposes

  write_diagnosis, dfile, 'regress_from_nochange: setting up no change tests'
  indices1 = where(indgen(n_elements(mask_nochange)) mod 3,complement=indices2)
  mask_test = mask_nochange[indices2]
  mask_train = mask_nochange[indices1]
  n_test = n_elements(mask_test)
  n_train = n_elements(mask_train)
  n_nochange = n_elements(mask_nochange)

  ;set up to match with radcal patterns, for use in orthoregress
  num_channels = nochange_info.num_layers
  aa= fltarr(num_channels) ; slope of orthogonal regression curve
  xm = fltarr(num_channels); mean of X
  ym = fltarr(num_channels); mean of Y

  ;make a structure to hold onto the data
  base = {slope:0., intercept:0., slope_sigma:0., intercept_sigma:0., $
    rma_slope:0., rma_intercept:0., rma_rmse:0., $
    stats_diff: fltarr(4), corr:0.}
  band_stats = replicate(base, num_channels)	;to store information on the fits

  ;go through each layer, find the coefficients

  write_diagnosis, dfile, 'regress_from_nochange: beginning calibration'
  next_start = 0


  for i = 0, num_channels-1 do begin
    if i eq 0 then openw, un, output_file, /get_lun else $
      openu, un, output_file, /get_lun, /append
    point_lun, un, next_start    ;

    targetimg=(img1=(img2=(img3=0)))	;reset for memory's sake


    write_diagnosis, dfile, 'regress_from_nochange:    Reading in band '+string(i+1)+' from '+nochange_info.fname1
    zot_img, nochange_info.fname1, h1, img1, layers=[i+1], subset =subset
    img1 = transpose(img1[mask_train])  ;transpose to work in orthoregress
	subset = orig_subset


    write_diagnosis, wfile, '   Reading in band '+string(i+1)+' from '+nochange_info.fname2
    zot_img, nochange_info.fname2, h2, targetimg, layers=[i+1], subset=subset
    img2 = transpose(targetimg[mask_train])
	subset=orig_subset

    ;screen for probability

   if n_elements(screen_prob) ne 0 then begin
	    ok = screen_vals_by_prob(img1, img2, threshold = screen_prob)
	    if ok.ok eq 0 then begin

	      write_diagnosis, dfile, 'Problem screening points in x and y vals'
	      write_diagnosis, dfile, 'Channel: '+string(i+1)+' of '+string(num_channels)
			free_lun, un
	      return, {ok:-1, bad_band:i+1, notes:'_screen vals caused problems_'}
	    end
		 img1_screen= ok.x
    	img2_screen= ok.y


    end else begin

    img1_screen = img1
 	img2_screen = img2

	end



    ;take these screened values from here forward





    ;check for ignore values

    if n_elements(ignore) ne 0 then begin
      goodsx = img2_screen ne ignore
      goodsy = img1_screen ne ignore
      goods = where( (goodsx * goodsy) eq 1)
    end else goods=lindgen(n_train)		;if no ignore, just set up reference to all of the elements






    write_diagnosis, wfile,'Overall correlation betweeen images'
    write_diagnosis, wfile,string(correlate(img1_screen[goods], img2_screen[goods]))

    write_diagnosis, wfile, '   Performing iterative distance-filtered orthoregression '

 subsetgoods = goods		;set to the whole population now, we'll later const
 base = {slope:0., intercept:0., correlation:0., xm:0., ym:0., aa:0., sigma_aa:0., sigma_bb:0.}
 n_iter_runs = 10

 iter_tracker = replicate(base, n_iter_runs)


 for q = 0, n_iter_runs-1 do begin

    ortho_regress, transpose(img2_screen[goods[subsetgoods]]), transpose(img1_screen[goods[subsetgoods]]), ai, xmi, ymi, sigma_aa, sigma_bb
    aa[i]=ai
    xm[i]=xmi
    ym[i]=ymi


   ;rma_out = rma(transpose(img2_screen[goods[subsetgoods]]), transpose(img1_screen[goods[subsetgoods]]), yfit=yfit)



   window,10+i,xsize=400,ysize=400,xpos=10*i,ypos=10*i,title="Regression All No-change"


    ok = make_2d_hist_image_ff(img2_screen[goods[subsetgoods]], img1_screen[goods[subsetgoods]], window = 10+i, $
      nbins = [100,100], blowup=3,background = 255, $
      percent_cutoffs=[0.01, 0.99], $

      xtitle="uncalibrated",ytitle="reference", $
      title="channel"+string(i+1), $
      plotrange = plotrange)

    Oplot,[0,plotrange[1]],[ym[i]-aa[i]*xm[i],ym[i]-aa[i]*xm[i]+plotrange[1]*aa[i]],thick = 2, color='0000ff'xl
    xyouts, .7, .2, 'ORTHO_REGRESS', /norm, color = '0000ff'xl

;get the counts associated with the values in the original arrays

  counts = ascribe_2d_hist_vals(img2_screen[goods[subsetgoods]], img1_screen[goods[subsetgoods]], ok.histvals, ok.min1, ok.min2, ok.max1, ok.max2, ok.bin1, ok.bin2)
  ;scale it
  count_score = (1-(float(counts) / max(counts)))+1	;make the lowest count get the highest count_score
  												;because of penalty of low counts. range is [1:4]




     iter_tracker[q].slope = aa[i]
     iter_tracker[q].intercept = ym[i]-aa[i]*xm[i]
    iter_tracker[q].correlation = correlate(img1_screen[goods[subsetgoods]], img2_screen[goods[subsetgoods]])	;correlate the subset
    iter_tracker[q].aa = aa[i]
    iter_tracker[q].xm = xm[i]
    iter_tracker[q].ym = ym[i]
    iter_tracker[q].sigma_aa = sigma_aa
    iter_tracker[q].sigma_bb = sigma_bb


	;NEW WITH DIST VERSION:   Use distance from the regrssion line to help filter

   	dists = perp_dist( iter_tracker[q].slope,  iter_tracker[q].intercept, img2_screen[goods], img1_screen[goods], /absolute)	;get the distance from the whole population
			;gives the distance, with near-zero value having high counts and near line
			;  want to filter out the distant points

    	;now take the dists and multiply by the count score, to make lower counts get shoved further away
    	dists = dists*count_score

    w = c_prob(dists, bin_count_min = 20)	;get the cumulative distribution.  we're interest in screening out the high stuff -- over, say, 98%.  whatever
    					;the screen probability is.
    subset_threshold = max(where(w lt (1.0-dist_screen), many))
    if many eq 0 then message, 'Screen probability is too high'
    subsetgoods = where(dists lt subset_threshold, n_narrowed)		;constrain for next run
print, 'slope'+string(iter_tracker[q].slope)
print, 'intercept'+string(iter_tracker[q].intercept)
print, 'n post filtering = '+string( n_narrowed)
print, 'correlation = '+string(iter_tracker.correlation)
;    ggg = get_kbrd()


end		;q


;pick the best one by correlation, then assign to the values again -- aa[i], etc. to pick up
;  where thisgot lost.

bestone = where(iter_tracker.correlation eq max(iter_tracker.correlation), many)
if many gt 1 then bestone = bestone[0]
aa[i] = iter_tracker[bestone].slope
intercept = iter_tracker[bestone].intercept
slope = aa[i]
xm[i] = iter_tracker[bestone].xm
ym[i] = iter_tracker[bestone].ym
sigma_aa = iter_tracker[bestone].sigma_aa
sigma_bb = iter_tracker[bestone].sigma_bb
correlation = iter_tracker[bestone].correlation


    ;plot it out
    window,10+i,xsize=400,ysize=400,xpos=10*i,ypos=10*i,title="Regression All No-change"


    ok = make_2d_hist_image_ff(img2_screen[goods], img1_screen[goods], window = 10+i, $
      nbins = [100,100], blowup=3,background = 255, $
      percent_cutoffs=[0.01, 0.99], $

      xtitle="uncalibrated",ytitle="reference", $
      title="channel"+string(i+1), $
      plotrange = plotrange)

    Oplot,[0,plotrange[1]],[ym[i]-aa[i]*xm[i],ym[i]-aa[i]*xm[i]+plotrange[1]*aa[i]],thick = 2, color='0000ff'xl
    xyouts, .7, .2, 'ORTHO_REGRESS', /norm, color = '0000ff'xl

;perpendicular distance


;Thus, the distance, framed as slope/intercept, is:
;
;D = |(-m)*h + v - b| / sqrt(m^2 + 1)




    ;  		plot, img2, img1, pSym=3,xtitle="uncalibrated",ytitle="reference", title="channel"+string(i+1), $
    ;		       color=0,background='FFFFFF'XL
    ;
    write_diagnosis, wfile, 'Results from orthoregress for channel:'+string( i+1)
    write_diagnosis, wfile,'intercept='+string(ym[i]-aa[i]*xm[i])+'   +-'+string(sigma_bb)
    write_diagnosis, wfile,'slope='+string(aa[i])+'   +-'+string(sigma_aa)

    ;assign values
    band_stats[i].slope = slope
    band_stats[i].slope_sigma = sigma_aa
    band_stats[i].intercept = intercept
    band_stats[i].intercept_sigma = sigma_bb
  ;  band_stats[i].corr = correlate(img1_screen[goods], img2_screen[goods])
	band_stats[i].corr = correlation

if band_stats[i].corr lt 0.3 then begin
		free_lun, un
		return, {ok:0, bad_band:i+1}		;set flag that this didn't work
		end


    ;  ;do the reduced major axis regression
    ;      write_diagnosis, wfile, '  Performing reduced major axis'
    ;
    ;       ok = rma(img2[goods], img1[goods])
    ;       band_stats[i].rma_slope = ok.slope
    ;       band_stats[i].rma_intercept = ok.intercept
    ;       band_stats[i].rma_rmse = ok.rmse
    ;       oplot, [0,plotrange[1]], [ok.intercept, ok.intercept+(plotrange[1]*ok.slope)], thick = 2, color = '0000ff'xl
    ;    xyouts, .7, .1, 'RMA REGRESS', color = '0000ff'xl, /norm

    ;       write_diagnosis, wfile, 'Results from orthoregress for channel:'+string( i+1)
    ;		   write_diagnosis, wfile,'intercept='+string(ok.intercept)
    ;		   write_diagnosis, wfile,'slope='+string(ok.slope)

    ;if write out this plot

    aaa = tvrd(10+i, /true)
    output_tiff = strcompress( output_base+'_ch'+string(i)+'.tif', /rem)
    write_tiff, output_tiff, reverse(aaa,3)
    aaa= 0

    ;calibrate and write out using the orthoregress coefficients
    ;first, read in the whole targetimg and then
    ;  apply it to the whole image. so just read int he
    ;   image without the subset.  7/19/05

    if n_elements(apply_to_entire_image) eq 0 then $
    zot_img, nochange_info.fname2, h2, targetimg, subset=subset, layers=[i+1] else $
    zot_img, nochange_info.fname2, h2, targetimg, layers=[i+1]



    subset=orig_subset

	;identify pixels that are ignore values so we maintain them as ignore
    t_ignore = where(targetimg eq ignore, n_t_ig)


    write_diagnosis, wfile, output_file
    write_diagnosis, wfile, 'writing image for layer '+string(i+1)
    img3 = fix(intercept+targetimg*slope)

	;reset the pixels that had the ignore value on input

	if n_t_ig ne 0 then img3[t_ignore]=ignore

    writeu, un, img3

    ;get the pointer to the end of the file

    point_lun, -un, next_start


    ;do the comparison on the test pixels

    mom = moment(img1[mask_test]-img3[mask_test])
    band_stats[i].stats_diff = mom


    write_diagnosis, wfile, ' '
    write_diagnosis, wfile, ' '

    free_lun, un


  end  	;i  through layers.

  ;write out header for calibrated image
  ;

  h = h2
  h.n_layers = num_channels
  h.pixeltype = 6
  write_im_hdr, output_base+'.hdr', h


  ;save the info

  info = {band_stats:band_stats, calibrated_image:output_file, $
    mask_train:mask_train, mask_test:mask_test}

  savefile = output_base+'.sav'
  save, info, file = savefile




  write_diagnosis, dfile, 'regress_from_nochange.pro complete.  returning.'


  return, {ok:1, savefile:savefile, band_stats:band_stats, calibrated_image:output_file, header:h}


end

;may 21, 2006.  removed the pass of the colorsavefile, so
;  that the new version of make_2d_hist_image_ff.pro will
;  retrieve the colors from get_madcal_colors.pro, thus
;  allowing .sav file version
