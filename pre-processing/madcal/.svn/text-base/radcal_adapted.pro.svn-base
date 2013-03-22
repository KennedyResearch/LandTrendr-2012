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

function radcal_adapted, fname1, fname2, subset, dfile, wfile, footprint, output_base = output_base, $
		layers = layers, ignore=ignore, noplot=noplot, min_nochange_count=min_nochange_count, $
		mask_image=mask_image, waterthresh=waterthresh, cloudthresh=cloudthresh


	;adapting the "radcal.pro" that Todd Schroeder obtained from M. Canty
	;Here's the info on the original code from Canty:
	;------------------------------------------------------------------------------------
	; Radiometric calibration using MAD
	; M. Canty, 2003
	; Ref: Canty et al. Remote Sensing of Environment
	; Requires gen_eigenproblem.pro
	;          ortho_regress.pro
	;          progressbar__define.pro
	; Reference and target spatial subsets must have equal spatial and spectral dimensions,
	; be in byte format and registered to one another.
	; Once the regression coefficients have been determined, they can be used to
	; calibrate another file, for example a full scene.
	;------------------------------------------------------------------------------------
	;
	;
	;What I want to do is free it up from needing ENVI,since we have only
	;    one license, and I want to be able to put this into a virtual machine
	;
	;The general strategy is to take the guts of the radcal program
	;    and swap in the places where it requires ENVI to do anything.
	;layers = layers to use, with 1 as first layer


	;check on the logfile unit

	; if n_elements(logfile_un) eq 0 then logfile_un = -1		;text output

	if n_elements(min_nochange_count) eq 0 then min_nochange_count = 300


	;SET UP THE STATUS VARIABLE.  IF CLOUD, WATER, OR MIN NO CHANGE THRESHOLD TRIP
	;  THIS ONE, WE'LL NOTE IT

    STATUS = 1		;Start out assuming that the status is good.


	;get the two images
	write_diagnosis, dfile, 'radcal_adapted: Reading hdr info image 1 '+fname1
master_subset = subset
	zot_img, fname1, hdr1, image1, subset = subset, /hdronly
	write_diagnosis, dfile, 'radcal_adapted: Reading hdr info  image 2 '+fname2
subset=master_subset
	zot_img, fname2, hdr2, image2, subset = subset, /hdronly

	;first, determine which layers

	if n_elements(layers) eq 0 then use_layers = indgen(hdr1.n_layers)+1 else $
		begin
		;note:  layers is
		if max(layers) gt hdr1.n_layers then begin
			write_diagnosis, dfile, 'layers passed exceed available layers'
			return, {ok:0}
		end

		use_layers = layers
	end



	;set numbers to type defined in radcal
	num_channels = n_elements(use_layers)
	num_columns = hdr1.filesize[0]
	num_lines = hdr1.filesize[1]
	num_pixels = num_columns * num_lines


	;check
	if hdr2.filesize[0] ne num_columns or $
		hdr2.filesize[1] ne num_lines then begin
		write_diagnosis, dfile,  'images not same size. failing.'
		write_diagnosis, dfile, 'Check to make sure pixels same size.'

		return, {ok:0}
	end

	write_diagnosis, dfile, 'radcal_adapted: setting up variables'



;ADD IN MASK IMAGE HERE -- IF CONSTRAINING TO A PARTICULAR POPULATION
	if n_elements(mask_image) ne 0 then begin
			subset=master_subset

			 zot_img, mask_image, msk_hdr, mask_img, subset=subset
			 usepopn = where(mask_img eq 1, n_masks)
			 if n_masks eq 0 then message, 'Mask img in call to radcal adapted must have 1s to indicate population'
			 num_pixels = n_masks

	end

	;now need to determine now many pixels, in case we're using a mask image



	;set up variables, direct from radcal.pro

	maskout1 = (maskout2=bytarr(num_pixels))	;will keep track of which pixels to take out
	tempimage1=fltarr(num_channels,num_pixels)
	tempimage2=fltarr(num_channels,num_pixels)



	m1s=fltarr(num_channels) ; means of reference image
	m2s=fltarr(num_channels) ; means of target image
	m3s=fltarr(num_channels) ; means of normalized image
	var1s=fltarr(num_channels) ; variances of reference image
	var2s=fltarr(num_channels) ; variances of target image
	var3s=fltarr(num_channels) ; variances of normalized image
	mu4=fltarr(num_channels) ; 4th moment for error in variance
	df=intarr(num_channels) ; degrees of freedom
	aa= fltarr(num_channels) ; slope of orthogonal regression curve
	xm = fltarr(num_channels); mean of X
	ym = fltarr(num_channels); mean of Y

	; MAD transformation
	write_diagnosis, dfile, 'radcal_adapted: reading in layers of image'






	;read in the images and look for usable pixels (not ignores)

	for i=0,num_channels-1 do begin
		zot_img, fname1, hdr1, dummy, subset = subset, layers = use_layers[i]

		if n_elements(mask_image) ne 0 then dummy = dummy[usepopn]	;if we have a popn to choose from, then subset now

		if n_elements(ignore) ne 0 then $
			maskout1 = maskout1+(dummy ne ignore)	;add up all -- looking for actual gaps, so it must be a gap in all channels
		tempimage1[i,*] = dummy[*]


		;this one is the input image that will be normalized

		zot_img, fname2, hdr2, dummy, subset = subset, layers = use_layers[i]

		if n_elements(mask_image) ne 0 then dummy = dummy[usepopn]	;if we have a popn to choose from, then just subset now



		;if the user passed a water or cloud threshold, then test here.  The basic
		;  idea is that if more than a quarter of the subset is in a range identified
		;  by the user as water or cloud, we should skip this subset

		if n_elements(waterthresh) ne 0 then begin
			water_band_to_check = waterthresh[0]


			if i+1 eq water_band_to_check then begin
			    huj = n_elements(dummy)

			    waters = where(dummy lt waterthresh[1], n_waters)
			    if float(n_waters)/huj gt waterthresh[2] then  begin
					;make a structure to hold onto the data
					; corr is set to 2 which essentially turns off this subset window
					base = {slope:0., intercept:0., slope_sigma:0., intercept_sigma:0., $
									rma_slope:0., rma_intercept:0., rma_rmse:0., $
									stats_diff: fltarr(4), corr:0.}

					band_stats = replicate(base, num_channels)	;to store information on the fits
					write_diagnosis, wfile, 'Water threshold failed.'
					mask_nochange = [-1]
					status = 3		;to flag that water threshold failed
					goto, FINAL
				endif
			
				if n_waters gt 0 then dummy[waters] = ignore
			end
	    end

		if n_elements(cloudthresh) ne 0 then begin
			cloud_band_to_check = cloudthresh[0]
			if i+1 eq cloud_band_to_check then begin
			    huj = n_elements(dummy)

			    clouds = where(dummy gt cloudthresh[1], n_clouds)
			    if float(n_clouds)/huj gt cloudthresh[2] then  begin
					;make a structure to hold onto the data
					; corr is set to 2 which essentially turns off this subset window
					base = {slope:0., intercept:0., slope_sigma:0., intercept_sigma:0., $
									rma_slope:0., rma_intercept:0., rma_rmse:0., $
									stats_diff: fltarr(4), corr:0.}

					band_stats = replicate(base, num_channels)	;to store information on the fits
					mask_nochange = [-1]
					write_diagnosis, wfile, 'Cloud threshold failed.'
					status = 2	;to flag that cloud threshold failed.
					goto, FINAL
				endif
				
        if n_clouds gt 0 then dummy[clouds] = ignore
			end
	    end

		if n_elements(ignore) ne 0 then $
			maskout2 = maskout2+(dummy ne ignore)
		tempimage2[i,*] = dummy[*]
	end



	;now get mean of non-ignore and scale it

	maskout1 = maskout1 gt 0	;look for anywhere that was not ignore across all bands
	maskout2 = maskout2 gt 0	;ditto

	maskout=maskout1*maskout2
	maskout1 = 0
	maskout2 = 0


	ign_goods = where(maskout eq 1,n_ign_goods)

	image1=fltarr(num_channels,n_ign_goods)
	image2=fltarr(num_channels,n_ign_goods)
	image3=fltarr(num_channels,n_ign_goods)



	for i = 0, num_channels-1 do begin
		image1[i,*] = tempimage1[i,ign_goods]
		m1s[i] = mean(image1[i,*])
		image1[i,*]=image1[i,*]-m1s[i]

		image2[i,*] = tempimage2[i,ign_goods]
		m2s[i] = mean(image2[i,*])
		image2[i,*]=image2[i,*]-m2s[i]


	endfor
	tempimage1=0
	tempimage2=0	;save memory




	samples = transpose([[transpose(image1)],[transpose(image2)]])

	write_diagnosis, dfile, 'radcal_adapted: correlating'

	sigma = correlate(samples,/covariance,/double)

	sigma_xx = sigma[0:num_channels-1,0:num_channels-1]
	sigma_yy = sigma[num_channels:2*num_channels-1,num_channels:2*num_channels-1]
	sigma_xy = sigma[num_channels:2*num_channels-1,0:num_channels-1]
	sigma_yx = sigma[0:num_channels-1,num_channels:2*num_channels-1]

	C1 = sigma_xy ## invert(sigma_yy,/double) ## sigma_yx
	B1 = sigma_xx
	C2 = sigma_yx ## invert(sigma_xx,/double) ## sigma_xy
	B2 = sigma_yy

	write_diagnosis, dfile, 'radcal_adapted: gen_eigenproblem'

	gen_eigenproblem, C1, B1, A, lambda ; C1 a = lambda B1 a
	gen_eigenproblem, C2, B2, B, lambda ; C2 b = lambda B2 b

	write_diagnosis, wfile, 'eigenvalues: '+string(lambda)
	sigMADs = sqrt(2*(1-sqrt(lambda)))
	write_diagnosis, wfile,  'sigma MADs: '+string( sigMADs)

	; ensure positive correlation
	for i=0,num_channels-1 do $
		if (transpose(A[i,*])##sigma_xy##B[i,*] lt 0) $
		then B[i,*] = -B[i,*]

	MADs = image1##A-image2##B

	write_diagnosis, dfile, 'radcal_adapted: Getting mask for no-change'

	; get mask for no-change pixels
	for i=0,num_channels-1 do MADs[i,*]=MADs[i,*]/sigMADs[i]
	threshold = chisqr_cvf(0.995,num_channels)
	;threshold = chisqr_cvf(0.975, num_channels)

	write_diagnosis, wfile, 'chi-square threshold for no-change ='+string(threshold)

	;check whether there is any no change pixels
	mask_nochange = where(sqrt(total(MADs*MADs,1)) lt threshold, nochangecount)

	; YANG
	;a minimum 500 pixels are required to have a reliable regression.
	if (nochangecount lt min_nochange_count) then begin
		;make a structure to hold onto the data
		; corr is set to 2 which essentially turns off this subset window
		base = {slope:0., intercept:0., slope_sigma:0., intercept_sigma:0., $
						rma_slope:0., rma_intercept:0., rma_rmse:0., $
						stats_diff: fltarr(4), corr:0.}

		band_stats = replicate(base, num_channels)	;to store information on the fits

		out_array = intarr(num_columns,num_lines,num_channels) ; place holders
		status = 4	;to flag that min count was failed

		goto, FINAL
	endif



	; holdout 1/3 for test purposes
	indices1 = where(indgen(n_elements(mask_nochange)) mod 3,complement=indices2)
	mask_test = mask_nochange[indices2]
	mask_train = mask_nochange[indices1]
	n_test = n_elements(mask_test)
	n_train = n_elements(mask_train)
	n_nochange = n_elements(mask_nochange)



	;skip the parts with envi roi

	write_diagnosis, dfile, 'radcal_adapted: building structure to hold data'

	;make a structure to hold onto the data
	base = {slope:0., intercept:0., slope_sigma:0., intercept_sigma:0., $
		rma_slope:0., rma_intercept:0., rma_rmse:0., $
		stats_diff: fltarr(4), corr:0.}
	band_stats = replicate(base, num_channels)	;to store information on the fits

	; put back means
	for i=0,num_channels-1 do begin
		image1[i,*] = image1[i,*]+m1s[i]
		image2[i,*] = image2[i,*]+m2s[i]
	endfor

	;set up colors
	;
	;  restore, 'C:\_Robert\Current\library\idl\color_density2.sav'
	;  tvlct, rrr, ggg, bbb
	;
	;


	write_diagnosis, dfile, 'radcal_adapted: Getting arrays for regression'

	; get arrays for regression
	;X = intarr(num_channels,n_train)  original version
	;Y = intarr(num_channels,n_train)
	X = fltarr(num_channels,n_train)
	Y = fltarr(num_channels,n_train)
	for i=0,num_channels-1 do begin
		X[i,*] = (image2[i,*])[mask_train]
		Y[i,*] = (image1[i,*])[mask_train]
	endfor






	write_diagnosis, wfile, "orthogonal regression using "+string(n_train)+" no-change pixels ..."
	write_diagnosis, dfile, 'radcal_adapted: doing orthogonal regression and rma'

	for i=0,num_channels-1 do begin
		if n_elements(noplot) eq 0 then window,10+i,xsize=400,ysize=400,xpos=10*i,ypos=10*i,title="Regression"
		if n_elements(noplot) eq 0 then wset,10+i

		;screen out low-count combos of values (even though this
		;   should have been done in the MAD, somehow a lot go through
		;   in low-frequencies at any given ref/dep combo)

		write_diagnosis, dfile, 'radcal_adapted:  beginning screen vals'


		;  ok = screen_vals_by_prob(x[i,*], y[i,*])
		;    if ok.ok eq 0 then begin
		;
		;      write_diagnosis, dfile, 'Problem screening points in x and y vals'
		;      write_diagnosis, dfile, 'Channel: '+string(i+1)+' of '+string(num_channels)
		;
		;      return, {ok:0}
		;    end


		;take these screened values from here forward

		;    xx= ok.x
		;    yy= ok.y

		xx = x[i,*]
		yy = y[i,*]


		;    ;check for ignore values
		;
		;    if n_elements(ignore) ne 0 then begin
		;      goodsx = xx ne ignore
		;      goodsy = yy ne ignore
		;      goods = where( (goodsx * goodsy) eq 1)
		;    end else goods=bindgen(n_train)		;if no ignore, just set up reference to all of the elements
		;;
		;    print, 'number of goods: '+string(n_elements(goods))
		;    print, 'number of potential pixels: '+string(n_elements(xx))
		;




		;		   plot, X[i,*], Y[i,*], pSym=3,xtitle="uncalibrated",ytitle="reference", title="channel"+string(i+1), $
		;		       color=0,background='FFFFFF'XL
		;		   ; orthogonal regression on ax+c
		; ortho_regress, transpose(xx[goods]), transpose(yy[goods]), ai, xmi, ymi, sigma_aa, sigma_bb
		;	take out goods because handle ignore upstream of it all now;
		;	because of that, don't need to do the transpose

		ortho_regress, xx, yy, ai, xmi, ymi, sigma_aa, sigma_bb
		aa[i]=ai
		xm[i]=xmi
		ym[i]=ymi
		; Oplot,[plotrange[0],plotrange[1]],[ym[i]-aa[i]*xm[i]+plotrange[0]*aa[i],ym[i]-aa[i]*xm[i]+plotrange[1]*aa[i]],color='0000ff'xl, thick = 2
		offset = (ymi-(ai*xmi))



		if n_elements(noplot) eq 0 then begin

			;ok = make_2d_hist_image_ff(xx[goods], yy[goods], window = 10+i, $
			ok = make_2d_hist_image_ff(xx, yy, window = 10+i, $
				nbins = [100,100], blowup=3,background = 255, $
				percent_cutoffs=[0.01, 0.99], $

				xtitle="uncalibrated",ytitle="reference", $
				title="channel"+string(i+1), $
				plotrange = plotrange)
			oplot, [0,plotrange[1]], [offset, offset+(ai*plotrange[1])], color = '0000ff'xl, thick = 2
			xyouts, .7, .2, 'ORTHO_REGRESS', /norm, color = '0000ff'xl

		end



		write_diagnosis, wfile, 'channel:'+string( i+1)
		write_diagnosis, wfile,'b='+string(ym[i]-aa[i]*xm[i])+'   +-'+string(sigma_bb)
		write_diagnosis, wfile,'a='+string(aa[i])+'   +-'+string(sigma_aa)

		;write_diagnosis, wfile, string(correlate(xx[goods], yy[goods]))
		write_diagnosis, wfile, string(correlate(xx, yy))


		;assign values to the stats structure

		band_stats[i].slope = aa[i]
		band_stats[i].slope_sigma = sigma_aa
		band_stats[i].intercept = ym[i]-aa[i]*xm[i]
		band_stats[i].intercept_sigma = sigma_bb
		;band_stats[i].corr = correlate(xx[goods], yy[goods])
		band_stats[i].corr = correlate(xx, yy)



	;do the reduced major axis regression

	;       ok = rma(xx[goods], yy[goods])
	;       band_stats[i].rma_slope = ok.slope
	;       band_stats[i].rma_intercept = ok.intercept
	;       band_stats[i].rma_rmse = ok.rmse
	;       oplot, [0, plotrange[1]], [ok.intercept, ok.intercept+(plotrange[1]*ok.slope)], color = '0000ff'xl, thick = 2
	;       xyouts, .7, .1, 'RMA REGRESS', color = '0000ff'xl, /norm

	;qqq = get_kbrd(1)


	;if we're doing the output, write out this image

	;    if n_elements(output_base) ne 0 then begin
	;
	;      aaa = tvrd(10+i, /true)
	;      output_file = strcompress( output_base+'_ch'+string(i)+'.tif', /rem)
	;      write_tiff, output_file, reverse(aaa,3)
	;    end


	;display it, but skip the top and bottom 3% so it's scaled okay.

	;      ok = make_2d_hist_image_ff(x[i,*], y[i,*], window = 0, $
	;      			nbins = [100,100], blowup=1,background = 255, $
	;      			 percent_cutoffs=[0.03, 0.97], colorsavefile = 'madcalcolors.sav')




	endfor


	write_diagnosis, dfile, "radcal_adapted: calibrating to reference image ..."

	;for i=0,num_channels-1 do image3[i,goods]=ym[i]-aa[i]*xm[i]+image2[i,goods]*aa[i]
	for i=0,num_channels-1 do image3[i,*]=ym[i]-aa[i]*xm[i]+image2[i,*]*aa[i]

	;result = intarr(num_columns,num_lines,num_channels) original
	;result = fltarr(num_columns,num_lines,num_channels)

	;for i = 0,num_channels-1 do begin
		;use maskout as a way to keep track of things in the right
		;   dimensions, since we have to work backwards to
		;	get the ignore values right.

;		maskout[ign_goods] = image3[i,*]


;		result[*,*,i] = reform(maskout,num_columns,num_lines,/overwrite)

;	end

	;now have to deal with the mask_nochange pixels, since that's what really
	;   gets used if this is a good subset.  right now, they're referenced to the
	;   good pixels (the non-ignore value pixels).  So we need to fix that.

	maskout[*]=0
	maskout[ign_goods[mask_nochange]]=1
	mask_nochange = where(maskout eq 1)




	; truncate

	;result = result<255
	;result = result>0
	;out_array = byte(result)


;	out_array= fix(round(result))


	write_diagnosis, dfile, 'radcal_adapted: testing pixels'


	; determine means and standard deviations
	; using holdout test pixels only and print comparison
	for i=0,num_channels-1 do begin
		dummy1=(image1[i,*])[mask_test]
		dummy2=(image2[i,*])[mask_test]
		dummy3=(image3[i,*])[mask_test]
		m1s[i]=mean(dummy1)
		m2s[i]=mean(dummy2)
		m3s[i]=mean(dummy3)
		var1s[i]=variance(dummy1)
		var2s[i]=variance(dummy2)
		var3s[i]=aa[i]*aa[i]*var2s[i]
		mu4[i]=mean((dummy3-m3s[i])^4)

		;assign comparison stat to stats structure
		mom = moment(dummy1-dummy3)
		band_stats[i].stats_diff= mom

	endfor
	write_diagnosis, wfile,"Comparison Statistics using"+string(n_test)+" test pixels"
	write_diagnosis, wfile,"Means"
	write_diagnosis, wfile,"target     "+string(m2s)
	write_diagnosis, wfile,"reference  "+string(m1s)
	write_diagnosis, wfile,"normalized "+string(m3s)
	write_diagnosis, wfile,"std dev    "+string(sqrt(var2s[*]/n_test))
	write_diagnosis, wfile,"Variances"
	write_diagnosis, wfile,"target     "+string(var2s)
	write_diagnosis, wfile,"reference  "+string(var1s)
	write_diagnosis, wfile,"normalized "+string(var3s)
	write_diagnosis, wfile,"std dev    "+string(sqrt((mu4-var1s^2)/n_test))

FINAL:
	;save the info on this fil

	info = {fname1:fname1, fname2:fname2, subset:subset, $
		band_stats:band_stats, num_channels:num_channels, $
		num_columns:num_columns, num_line:num_lines, $
		num_pixels:num_pixels, use_layers:use_layers, $
		out_array:-1, mask_nochange:mask_nochange, $
		footprint:footprint}

	savefile = output_base + '.sav'
	write_diagnosis, wfile, 'Saving to savefile '+savefile

	save, info, file = savefile
	write_diagnosis, dfile, 'radcal_adapted: done'




	return, {ok:1, savefile:savefile, use_layers:use_layers, $
		band_stats:band_stats, status:status} ;
						;status: 1: okay, 2: cloud threshold failed, 3: water threshold vfailed, 4: min count failed

end


;may 21, 2006.  removed the pass of the colorsavefile, so
;  that the new version of make_2d_hist_image_ff.pro will
;  retrieve the colors from get_madcal_colors.pro, thus
;  allowing .sav file version
