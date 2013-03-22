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

function run_tbcd_single_chunk, info,  $
		subset, index, mask_image, output_image_group, $
		within_layer_offset, layersize, kernelsize, $
		background_val, $
		skipfactor, desawtooth_val, $
		pval, max_segments, normalize , $
		fix_doy_effect, divisor , $
		recovery_threshold, minneeded, $
		distweightfactor,  vertexcountovershoot, $
    	bestmodelproportion

	;differs from s3 in that it writes out images according to
	;  abrupt vs. slow disturbance.
	;differs from s4 in that this one eventually calls find_segments6 instead
	;  of find_segments7.  that one was an aborted line in evolution that
	;  used the non-linear fitting.

	;eventually calls find_segments6, which uses quick approach to
	;  id segments.

	;this is intended for raw bands (either 5 or 7) for the sample
	; scenes, but is founded from the biomass fitting programs
	;  that use the segmentation rather than the iterative non-linear
	;  curvefitting.  Also, this will allow two disturbance/recovery sections
	;  subject to some constraints to prevent overfitting.

	;version 3 differs from priors in that here we use only
	;   the hypothesized models, not all possible combos of models.

	;version 22 differs from 2 in that this one uses groups of output iamges
	;   to keep filesize manageable.
	;biomass2 version uses segmentation -- totally different strategy --
	;  that results in different output layers.


	minimum_number_years_needed = minneeded    ;if we have fewer years than this, we can't do it.

	;6/20/08 the number of years is not the number of info items, because now we allow
	;  multiple images per year.  So check on the number of unique years in the
	;  year array

	years = fast_unique(info.year)
	years = years[sort(years)]
	n_yrs = n_elements(years)

	n_images = n_elements(info)	;because we need to go through each image, even if multiple per year

	if n_yrs lt minimum_number_years_needed then begin
		print, 'run_tbcd_single_chunk:  there are fewer than the minimum
		print, 'number of years available for disturbance/recovery extraction'
		print, 'the minimum is: '+string(minimum_number_years_needed)
		print, 'the number of files given to extract_disturbance_recovery4.pro: '+string(n_yrs)
		print, 'confirm that the information from find_image_stack_files is correct'
		return, {ok:0}
	end



	;check on the mask image

	if file_exists(mask_image) eq 0 then begin
		print, 'run_tbcd_single_chunk.pro needs to have a mask image.'
		print, 'the mask image should be 0s and 1s, with 1s indicating where
		print, ' to run the curve fitting.
		return, {ok:0}
	end


	;for the first year, just get the full subset, then use that
	;as the template.

	if n_elements(subset) eq 0 then begin
		print, 'run_tbcd_single_chun k needs to have a "subset" keyword set'
		return, {ok:0}
	end



	;check on the mask image

tempsubset = subset
	zot_img, mask_image, mask_hdr, mask_img, subset=tempsubset


	if max(mask_img) gt 1 then begin
		print, 'The mask image must have a maximum value of 1, to indicate
		print, '  where to run the curve-fitting.  The mask image
		print, mask_image
		print, '   has a maximum of '+string(max(mask_img))
		return, {ok:0}
	end


	;  image_file:'', $
	;   			image_path:'', $
	;   			type:0, $				;1 mtbs 2 nonmtbs 3 reference year mtbs
	;   					nbr_file:'', $
	;   					tc_file:'', $
	;   					b6_file:'', $
	;   					year:0, $
	;   					julday:0, $
	;   					unique_year:0, $    ;flagged 1 if only 1 image in this year
	;   					n_in_year:0, $		;number of images in this year
	;   					image_priority:0, $	;priority of picking if more than one image per year
	;   					cloudyear:0, $
	;   					cloud_diff_file:'', $
	;   					shadow_diff_file:'', $
	;   					tc_cloud_diff_file:'', $
	;   					cloud_file:'none', $
	;   					subset:[ [0.d, 0.d],[0.d, 0.d]], $
	;   					useareafile: ''}


	;First, build an image to hold the different years, then read them in

	;use first year as a template
tempsubset=subset

	zot_img, info[0].image_file, hdr, img, subset=tempsubset, layer=[1], /hdronly

	;lcount = n_elements(layer)

	;if lcount eq 0 then layer = 1
	;zot_img, path+file_list[0], hdr, img, subset=subset, layer=layer, /hdronly
	;       if lcount eq 2 then begin
	;           layer2 = layer[1]
	;           layer1 = layer[0]
	;        end else layer1 = layer



	if hdr.pixeltype ne 6 and hdr.pixeltype ne 3 and hdr.pixeltype ne 5 then begin
		print, 'run_tbcd_single_chunk expects the image to be of integer type'
		print, 'this one is type number '+string(hdr.pixeltype)

		return, {ok:0}
	end

	;make up a new image with the right dimensions.
	;the new image could potentially have multiple values for a given year,
	;which will be handled by the cloud mask.
	img = intarr(hdr.filesize[0], hdr.filesize[1], n_yrs)
	cld_img = bytarr(hdr.filesize[0], hdr.filesize[1], n_yrs)		;added v4
	usedmask = intarr(hdr.filesize[0], hdr.filesize[1]) ;valide values for years with multiple image

	;which image was used
	idx_img = bytarr(hdr.filesize[0], hdr.filesize[1], n_yrs)

	;now go through and build it.
	k = 0
	for i = 0, n_yrs-1 do begin
	;for i = 0, n_images-1 do begin
		; zot_img, info[i].image_file, hdr, img1, subset = subset, layer=layer1
		fileid = i + k
		this = where(info.year eq years[i], n)

		;current year
		cur_mask = bytarr(hdr.filesize[0], hdr.filesize[1])
		cur_img = intarr(hdr.filesize[0], hdr.filesize[1])
		cur_idx = bytarr(hdr.filesize[0], hdr.filesize[1])

		;YEARS WITH SINGLE IMAGE

		if n eq 1 then begin
			tempsubset = subset
			landtrendr_image_read, info[fileid], hdr, img1, tempsubset, index, modifier, background_val
			sz = size(img1, /dim)

			;now check vs. background. If so, then assign to the cloud
			;   image, since that's what I check before calling the
			;   fitting algorithm.
			bads = where(img1 eq background_val, n_bads)
			if n_bads ne 0 then cld_img[*,*,i] = (cld_img[*,*,i]+ (img1 eq background_val)) ne 0 	;ne 0 needed incase cloud image and background val!

			if n_elements(sz) gt 2 then begin
				print, 'run_tbcd_single_chunks: each image layer must have a single layer'
				print, 'image '+file_list[fileid]+ 'has more than 1 layer'
				return, {ok:0}
			end

			img[*,*,i] = img1/divisor	;added 2/7/08 this will scale to max of 1000

			idx_img[*,*,i] = replicate(fileid, size(img1, /dim))

			;now read the cloud mask
			; if there is no cloud mask, then just skip this
			if info[fileid].cloud_file ne 'none' and info[fileid].cloud_file ne '' then begin
				tempsubset=subset
				if info[fileid].cloud_file eq 'band8' then $
					zot_img, info[fileid].image_file, clhdr, mimg, layers=[8], subset=tempsubset else $
					zot_img, info[fileid].cloud_file, clhdr, mimg, subset=tempsubset
				cld_img[*,*,i] = (cld_img[*,*,i] + (mimg eq 0)) ne 0
			;  cld_img[*,*,i] = (cld_img[*,*,i] + (img1 ne 0)) ne 0 ;0 is no-cloud in cheng's cloudmasks
			end
		end

		;MULTIPLE IMAGES PER YEAR
		;if multiple image exists for this year, select one and make the others masked out
		if n gt 1 then begin
			victims = info[this]
			;sort by priority
			vicorder = sort(victims.image_priority)
			victims = victims[vicorder]
			;read in the cloud image
			for j = 0, n-1 do begin
				tempsubset=subset

				landtrendr_image_read, victims[j], hdr, img1, tempsubset, index, modifier, background_val

				sz = size(img1, /dim)
				if n_elements(sz) gt 2 then begin
					print, 'run_tbcd_single_chunks: each image layer must have a single layer'
					print, 'image '+file_list[fileid+j]+ 'has more than 1 layer'
					return, {ok:0}
				end

				;now read the cloud mask
				; if there is no cloud mask, then just skip this

				mimg = replicate(0, size(img1, /dim))
				if victims[j].cloud_file ne 'none' and victims[j].cloud_file ne '' then begin
						tempsubset=subset
						if victims[j].cloud_file eq 'band8' then $
						zot_img, victims[j].image_file, clhdr, mimg, layers=[8], subset=tempsubset else $
						zot_img, victims[j].cloud_file, clhdr, mimg, subset=tempsubset
						;cld_img[*,*,fileid+j] = (cld_img[*,*,fildid+j] + (mimg gt 2300)) ne 0
				end


				;identify pixels that are not background, that haven't been picked by the higher
				;   priority image, and that are not in the cloud mask

				valid = where(img1 ne background_val and cur_mask eq 0 and mimg eq 1, n_valid)
				if n_valid ne 0 then begin
					cur_img[valid] = img1[valid]
					cur_mask[valid] = 1					;mask gets set to 1 if the pixel is chosen
					cur_idx[valid] = replicate(this[vicorder[j]], n_valid)
				end
			endfor
			k = k + n - 1
			img[*,*,i] = cur_img/divisor
			cld_img[*,*,i] = cur_mask ne 1		;any cur_mask pixels still remaining 0 were not chosen
			idx_img[*,*,i] = cur_idx
		end
	end

	img1 = 0 ;reset to save space
	cur_img = 0
	cur_mask = 0
	;
	;    ;write out the stacked image
	;     outfile = strmid(write_file, 0, strlen(write_file)-4)+'_stack.bsq'
	;
	;     openw, un, outfile, /get_lun
	;     writeu, un, img
	;     free_lun, un
	;     hdr.n_layers = n_yrs
	;     write_im_hdr, outfile, hdr
	;

	;observe year stuff

	sz = size(img, /dim)
	;      n_yrs = sz[2]
	;        x_axis = indgen(n_yrs)

	;v4 has the actual years, offset by the min

	min_year = min(info.year)
	;  x_axis = info.year-min_year	+ 1		;set up by the year, but set to 1 to be consistent with
	;fitting functions


	x_axis = years		;these were "uniqued" early on, so should be okay.
	;x_axis = info.year

	;x_axis = info.year		;changed on 9/5 to match with


	;set up the progress bar:

	progressBar = Obj_New("PROGRESSBAR", /fast_loop, title = 'Curve-fitting:  percent done')
	progressBar -> Start

	vertyear_image = intarr(sz[0], sz[1], output_image_group[0].n_layers)
	vertvals_image = intarr(sz[0], sz[1], output_image_group[1].n_layers)
	mag_image = intarr(sz[0], sz[1],output_image_group[2].n_layers)
	dur_image = intarr(sz[0], sz[1], output_image_group[3].n_layers)
	distrec_image = intarr(sz[0], sz[1], output_image_group[4].n_layers)

	fitted_image = intarr(sz[0], sz[1], output_image_group[5].n_layers)
	stats_image = intarr(sz[0], sz[1], output_image_group[6].n_layers)

	segmse_image = intarr(sz[0], sz[1], output_image_group[7].n_layers)
	source_image = intarr(sz[0], sz[1], output_image_group[8].n_layers)
	segmean_image = intarr(sz[0], sz[1], output_image_group[9].n_layers)

	totalcount = float(sz[0]*sz[1])


	ksq=kernelsize^2
	seed= randomseed()
	if n_elements(skipfactor) eq 0 then skipfactor = 3

	offset = (kernelsize-1)/2



	for x = offset, sz[0]-(offset+1), skipfactor do begin
		for y = offset, sz[1]-(offset+1), skipfactor do begin

			;check on the mask image to see if we should run this pixel
			if mask_img[x,y] eq 1 then begin
				;check for clouds
				chunk = img[x-offset:x+offset, y-offset:y+offset, *]
				usable = cld_img[x-offset:x+offset, y-offset:y+offset, *] eq 0

				slice = total(chunk*usable,1)
				slice_usable = total(usable, 1)

				vals = total(slice,1)/total(slice_usable, 1)

				goods= where(cld_img[x,y,*] ne 1, ngds)
				if ngds gt minimum_number_years_needed then begin


					;first check to see if fix the doy effect
					if n_elements(fix_doy_effect) ne 0 then begin

						idxs = idx_img[x,y,*]

						uniques = fast_unique(info[idxs[goods]].julday)
						if n_elements(uniques) gt 4 then begin
							r = poly_fit(info[idxs[goods]].julday, vals[goods],2, chisq=chisq,yfit = yfit)
							m = mean(yfit)
							zzz = calc_fitting_stats3(vals[goods], yfit, 3, resid=resid)
							if zzz.p_of_f lt pval then outvals = m+resid else $
								outvals = vals[goods]
						end else outvals = vals[goods]
					end else outvals = vals[goods]


					ok=fit_trajectory_v2(x_axis,goods, outvals, $
						minneeded, background_val, $
						modifier, seed, $
						desawtooth_val, pval, $
						max_segments, recovery_threshold, $
						distweightfactor,  vertexcountovershoot, $
    					bestmodelproportion)


					if ok.ok eq 1 then begin
						;take out the bad year
						;fitted_image[x,y,*] = round(ok.best_model.yfit[uniq(info.year)])	;all years, including masked out, will get fittedvals
						fitted_image[x,y,*] = round(ok.best_model.yfit)	;prior version's line (commented, above) resulted in flatline at end
												;because of illogic -- all of the yfits belong in the fitted image
;plot, x_axis[goods], outvals, psym = 4
;oplot, x_axis, round(ok.best_model.yfit), color = '444499'xl
;qqw = get_kbrd()


					    source_image[x,y,goods] = outvals	;



						vertyear_image[x,y,*] = ok.best_model.vertices		;these are true years
						vertvals_image[x,y,*] = ok.best_model.vertvals		;in the units fed to fit_trajectory_v1
						segmse_image[x,y,*] = ok.best_model.segment_mse		;mse of each segment
						for ss = 0, ok.best_model.n_segments-1 do $			;mean of each segment
								segmean_image[x,y,ss] = (vertvals_image[x,y,ss]+vertvals_image[x,y,ss+1])/2.

						;get the magnitudes and the proportions
						temp = shift(ok.best_model.vertvals, -1) - ok.best_model.vertvals
						mag_image[x,y,0:ok.best_model.n_segments-1] = temp[0:ok.best_model.n_segments-1]

						maxdist = max(mag_image[x,y,0:ok.best_model.n_segments-1], min=maxrec)
						distrec_image[x,y, 0]=max([maxdist,0])
						distrec_image[x,y, 1]=min([maxrec, 0])



						totalmag = total(abs(mag_image[x,y, *]))	;the total distance traversed, up or down
						summag = float(total(mag_image[x,y, *]))			;the actual value with pluses and minuses
						if totalmag eq 0 then distrec_image[x,y, 2] = (-1500) else $
							distrec_image[x,y, 2] = (summag/totalmag)*1000	;will be -1000 if all rec, + 1000 if all dist

						;get the durations
						temp = shift(ok.best_model.vertices, -1) - ok.best_model.vertices

						dur_image[x,y,0:ok.best_model.n_segments-1] = temp[0:ok.best_model.n_segments-1]


						;					mag_image[x,y,*] =
						;					 = intarr(sz[0], sz[1], max_segments)
						;			distrec_image = intarr(sz[0], sz[1], max_segments)




						if ok.best_model.f_stat gt 300 then ok.best_model.f_stat = 300
						stats_image[x,y,0] = round(ok.best_model.p_of_f*100)
						stats_image[x,y,1] = round(ok.best_model.f_stat*100)
						stats_image[x,y,2] = round(ok.best_model.ms_regr/10.)
						stats_image[x,y,3] = round(ok.best_model.ms_resid/10.)
						stats_image[x,y,4] = 1			;directly run?
						stats_image[x,y,5] = ok.best_model.n_segments
						stats_image[x,y,6] = x_axis[goods[0]]	;set to minimum usable year
						stats_image[x,y,7] = n_elements(goods)	;number of usable years
						;stats_image layers 8 and 9 are used only for interpolation, giving the offset of the pixel
					end
				end



			end


			if progressBar -> CheckCancel() then begin
				;progressBar -> destroy
				print, 'x and y', string(x)+string(y)
				stop
			;    return, {ok:0}

			end

		end ;y


		percent_done = (float(x)*y)/ totalcount
		progressBar -> Update, percent_done*100
	end		;x



	progressBar -> Destroy
	progressBar = Obj_New("PROGRESSBAR", /fast_loop, title = 'Interpolating:  percent done')
	progressBar -> Start

	;now interpolate
	desired_kernel_size = 15
	ks = min([desired_kernel_size, sz[0], sz[1]])		;make sure that the kernel size is not
	desired_kernel_size = ks
	;bigger than the size of the chunk

	;make a distance matrix that can be clipped to all
		ok = make_distance_grid([(ks * 2)+1, (ks * 2)+1], start = [desired_kernel_size, desired_kernel_size])
					;this is twice as big as anything we'll get, so we can then subset it.

 	 	master_geo_dist = (ok.matrix / (desired_kernel_size/2)) + 1	;scale so penalty is 2 for things kernel_size dist away.



	dmat = fltarr(ks,ks)
	halfval = (ks-1)/2
	for x = 0, sz[0]-1 do begin			;start and end one pixel in,
		for y = 0, sz[1]-1 do begin
			checkval = (stats_image[x,y,4] ne 1) + $
				(mask_img[x,y] eq 1)


			if checkval eq 2 then begin

				;first, get cloud info for the desired pixel
				goods_pix = cld_img[x,y,*] ne 1

				;then calc the range of neighborhood pixels

				start_x = max([x-halfval, 0])		;first set up starting point, make sure in image
				start_y = max([y-halfval, 0])
				end_x = min([start_x+ks-1, sz[0]-1])  ;if we're near the other side, bump
				end_y = min([start_y+ks-1, sz[1]-1])
				start_x = end_x-ks+1								 ;jiggle start if we had to bump
				start_y = end_y-ks+1							 ;won't affect anything if we're not near the end

				;set the source image to img[x,y,combined_goods].
				;  this is not interpolated.

				wh_goods_pix = where(goods_pix eq 1, n_goods_pix)
				if n_goods_pix ne 0 then source_image[x,y,wh_goods_pix] = img[x,y,wh_goods_pix]

;w, 0

				;make a penalty score matrix from the geo distance
				geo_offset_x = 	desired_kernel_size-(x-start_x)
				geo_offset_y = desired_kernel_size-(y-start_y)

				penalty_geo_dist = master_geo_dist[ $
						geo_offset_x:geo_offset_x+desired_kernel_size-1, $
						geo_offset_y:geo_offset_y+desired_kernel_size-1]



		count=0		;then go through and get distance in spectral/temporal space

				for i = start_x, end_x do begin
					for j= start_y, end_y do begin



						;if this is a real curve-fitted pixel, then see
						;  how far away

						if stats_image[i,j,4] eq 1 then begin
							goods_test = cld_img[i,j,*] ne 1
							combined_goods = where(goods_test*goods_pix eq 1, ngds)
							if ngds ne 0 then $
								;						  	      dmat[i-start_x, j-start_y] = $
								;						  					sqrt(total(img[i,j,combined_goods] - $
								;						  					img[x,y,combined_goods])^2) else dmat[i-start_x, j-start_y] = 2e32
								dmat[i-start_x, j-start_y] = $
								total(abs(img[i,j,combined_goods] - $
								img[x,y,combined_goods])) else dmat[i-start_x, j-start_y] = 2e32

;							if ngds ne 0 and x gt 17 and y gt 2 then begin
;							    ;w, count, 400,400
;							    if count eq 0 then plot, img[x,y, combined_goods]
;							    oplot, img[i,j, combined_goods] , color = i*12455 + j*945454
;							    xyouts, .1, .7, string(dmat[i-start_x, j-start_y]), /norm
;							    count=count+1
;							    ;a = get_kbrd()
;							end

;plot, img[i,j, combined_goods], psym = 4
;oplot, img[x,y, combined_goods], color = '444499'xl
;print, dmat[i-start_x, j-start_y]
;qqw = get_kbrd()



						end else dmat[i-start_x, j-start_y] = 2e32		;just set to an absurdly high number

					end
				end


				;penalize appropriately
				decents = where(dmat ne 2e32, n_decents)
				if n_decents gt 0 then 	dmat[decents] = dmat[decents] * penalty_geo_dist[decents]

				;then pick closest one

				closest = where(dmat eq min(dmat))
				closest = closest[0]
				pos = getxy(closest, ks, ks)

;stop
				xoffset = (pos[0]+start_x)-x
				yoffset = (pos[1]+start_y)-y
				match_pos_x = x+xoffset
			    match_pos_y = y+yoffset



;if start_x gt 0 and start_y gt 0 then stop


				if min(dmat) ne 2e32 then begin
;					w, count+1
;					plot, img[x,y,*], thick = 2
;					oplot, img[x,y,*], thick = 4, color = '00ff00'xl
;					oplot, img[match_pos_x, match_pos_y, *], thick = 4
;					xyouts, .6, .2, string(dmat(closest)), /norm
;if  xoffset eq 3 and yoffset eq 4 and x gt 17 and y gt 2 then stop
;
;;						a = get_kbrd()

					vertyear_image[x,y,*] = vertyear_image[match_pos_x,match_pos_y, *]
					vertvals_image[x,y,*] = vertvals_image[match_pos_x,match_pos_y, *]
					segmse_image[x,y,*] = segmse_image[match_pos_x,match_pos_y, *]
					segmean_image[x,y,*] = segmean_image[match_pos_x,match_pos_y, *]

					mag_image[x,y,*] = mag_image[match_pos_x,match_pos_y, *]
					distrec_image[x,y,*] = distrec_image[match_pos_x,match_pos_y, *]
					dur_image[x,y,*] = dur_image[match_pos_x,match_pos_y, *]

					fitted_image[x,y,*] = fitted_image[match_pos_x,match_pos_y, *]
					stats_image[x,y,0:3] = stats_image[match_pos_x,match_pos_y,0:3]
					stats_image[x,y,4] = 2	;interpolated
					stats_image[x,y,5] = stats_image[match_pos_x,match_pos_y, 5]
   					stats_image[x,y,6] = stats_image[match_pos_x,match_pos_y, 6]
   					stats_image[x,y,7] = stats_image[match_pos_x,match_pos_y, 7]
   					stats_image[x,y,8] = xoffset
   					stats_image[x,y,9] = yoffset


;					vertyear_image[x,y,*] = vertyear_image[pos[0]+start_x, pos[1]+start_y, *]
;					vertvals_image[x,y,*] = vertvals_image[pos[0]+start_x, pos[1]+start_y, *]
;					segmse_image[x,y,*] = segmse_image[pos[0]+start_x, pos[1]+start_y, *]
;					segmean_image[x,y,*] = segmean_image[pos[0]+start_x, pos[1]+start_y, *]
;
;					mag_image[x,y,*] = mag_image[pos[0]+start_x, pos[1]+start_y, *]
;					distrec_image[x,y,*] = distrec_image[pos[0]+start_x, pos[1]+start_y, *]
;					dur_image[x,y,*] = dur_image[pos[0]+start_x, pos[1]+start_y, *]
;
;					fitted_image[x,y,*] = fitted_image[pos[0]+start_x, pos[1]+start_y, *]
;					stats_image[x,y,0:3] = stats_image[pos[0]+start_x,pos[1]+start_y,0:3]
;					stats_image[x,y,4] = 2	;interpolated
;					stats_image[x,y,5] = stats_image[pos[0]+start_x, pos[1]+start_y, 5]
;   					stats_image[x,y,6] = stats_image[pos[0]+start_x, pos[1]+start_y, 6]
;   					stats_image[x,y,7] = stats_image[pos[0]+start_x, pos[1]+start_y, 7]


				end else begin 			;if no good vals
					vertyear_image[x,y,*] = -1
					vertvals_image[x,y,*] = -1
					segmse_image[x,y,*] = -1
					segmean_image[x,y,*] = -1
					mag_image[x,y,*] = -1
					distrec_image[x,y,*] = -1
					dur_image[x,y,*] = -1


					fitted_image[x,y,*] = -1
					stats_image[x,y,*] = [1.0, 0, 0, 0, 2, -1, 0, 0, 0, 0]	;set p to 1.0, and set to interpolated
				end

			end	;checkval okay
		end	;y
		percent_done = (float(x)*y)/ totalcount
		progressBar -> Update, percent_done*100
	end	;x

	progressBar->Destroy

	;write 'em out

	;vertices


	openu, un, output_image_group[0].filename, /get_lun
	for layercount = 0ll, output_image_group[0].n_layers-1 do begin
		point_lun, un, (output_image_group[0].layersize * $
			layercount)+within_layer_offset
		writeu, un, vertyear_image[*,*,layercount]
	end
	free_lun, un

	;vertvals

	openu, un, output_image_group[1].filename, /get_lun
	for layercount = 0ll, output_image_group[1].n_layers-1 do begin
		point_lun, un, (output_image_group[1].layersize * $
			layercount)+within_layer_offset
		writeu, un, vertvals_image[*,*,layercount]*modifier		;added modifier july 9 2008 so values make sense
	end
	free_lun, un


	;mag image

	openu, un, output_image_group[2].filename, /get_lun
	for layercount = 0ll, output_image_group[2].n_layers-1 do begin
		point_lun, un, (output_image_group[2].layersize * $
			layercount)+within_layer_offset
		writeu, un, mag_image[*,*,layercount]
	end
	free_lun, un

	;duration image


	openu, un, output_image_group[3].filename, /get_lun
	for layercount = 0ll, output_image_group[3].n_layers-1 do begin
		point_lun, un, (output_image_group[3].layersize * $
			layercount)+within_layer_offset
		writeu, un, dur_image[*,*,layercount]
	end
	free_lun, un

	;distrec image


	openu, un, output_image_group[4].filename, /get_lun
	for layercount = 0ll, output_image_group[4].n_layers-1 do begin
		point_lun, un, (output_image_group[4].layersize * $
			layercount)+within_layer_offset
		writeu, un, distrec_image[*,*,layercount]
	end
	free_lun, un
	;fitted

	openu, un, output_image_group[5].filename, /get_lun
	for layercount = 0ll, output_image_group[5].n_layers-1 do begin
		point_lun, un, ulong64(output_image_group[5].layersize) * $
			layercount+within_layer_offset
		writeu, un, fitted_image[*,*,layercount]
	end
	free_lun, un

	;stats

	openu, un, output_image_group[6].filename, /get_lun
	for layercount = 0ll, output_image_group[6].n_layers-1 do begin
		point_lun, un, (output_image_group[6].layersize * $
			layercount)+within_layer_offset
		writeu, un, stats_image[*,*,layercount]
	end
	free_lun, un

	;segment mse

	openu, un, output_image_group[7].filename, /get_lun
	for layercount = 0ll, output_image_group[7].n_layers-1 do begin
		point_lun, un, (output_image_group[7].layersize * $
			layercount)+within_layer_offset
		writeu, un, segmse_image[*,*,layercount]
	end
	free_lun, un

	;source image

	openu, un, output_image_group[8].filename, /get_lun
	for layercount = 0ll, output_image_group[8].n_layers-1 do begin
		point_lun, un, (output_image_group[8].layersize * $
			layercount)+within_layer_offset
		writeu, un, source_image[*,*,layercount]
	end
	free_lun, un

	;segmean image

	openu, un, output_image_group[9].filename, /get_lun
	for layercount = 0ll, output_image_group[9].n_layers-1 do begin
		point_lun, un, (output_image_group[9].layersize * $
			layercount)+within_layer_offset
		writeu, un, segmean_image[*,*,layercount]
	end
	free_lun, un




	return, {ok:1}

end

