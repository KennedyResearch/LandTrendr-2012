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

function make_2d_hist_image_ff, image1, image2, window = window, nbins = nbins, $
    background = background, colorsavefile=colorsavefile, $
    ndigits = ndigits, legend_textwidth = legend_textwidth, $
    blowupby=blowupby, percent_cutoffs=percent_cutoffs, _extra=e, $
    plotrange=plotrange, one_to_one=one_to_one


  ;right now, took out the legend

  if n_elements(window) eq 0 then window = 1
  if n_elements(xmargin) eq 0 then xmargin = [50,20]
  if n_elements(ymargin) eq 0 then ymargin = [30,20]
  if n_elements(nbins) eq 0 then nbins = [50,50]
  if n_elements(background) eq 0 then background = 0
  if n_elements(blowupby) eq 0 then blowupby = 4  ;blowup the histogram 2d
  if n_elements(percent_cutoffs) eq 0 then percent_cutoffs = [0.,1.0]   ;proportions, i.e. [0.02, 0.98] eliminates the top and bottom 2% from display
  if n_elements(one_to_one) eq 0 then one_to_one = 0   ;set to 1 if x and y axis to be the same range.


  ;_extra is passed on to the contour, for formatting and such

  ;for the legend
  if n_elements(ndigits) eq 0 then ndigits = 4  ;for the legend
  if n_elements(legend_textwidth) eq 0 then $
    legend_textwidth = [.8] ;relative to the single image size


  ;legend parameters

  legend_scale = [.1, .7]   ;relative to image
  legend_pos = [.1, .1] ;relative to size of image
  ;x is for distance between image and
  ;   legend, y is for offset from bottom


  charthick = 1.5



  ;get info on the images
  ;   use the probability distribution to pick
  ;   the bounds of the range to display




  ;;cp_im1 = c_prob(reform(image1), bin_count_min=100, vals=cp1vals)
  artype = size(image1, /type)
  if artype eq 4 or artype eq 5 then $
    cp_im1 = c_prob(reform(image1), vals=cp1vals, bin_count_min=100) else $
    cp_im1 = c_prob(reform(image1), vals=cp1vals)


  minwhere1= min(where(cp_im1 ge percent_cutoffs[0]))
  maxwhere1= max(where(cp_im1 le percent_cutoffs[1]))
  min1 = cp1vals[minwhere1]
  max1 = cp1vals[maxwhere1]



  ;cp_im2 = c_prob(reform(image2), bin_count_min=100, vals=cp2vals)
  artype = size(image1, /type)
  if artype eq 4 or artype eq 5 then $
    cp_im2 = c_prob(reform(image2), vals=cp2vals, bin_count_min=100) else $
    cp_im2 = c_prob(reform(image2), vals=cp2vals)

  minwhere2= min(where(cp_im2 ge percent_cutoffs[0], n_minwhere2))
  if n_minwhere2 eq 0 then minwhere2=where(cp_im2 eq min(cp_im2))
  maxwhere2= max(where(cp_im2 le percent_cutoffs[1], n_maxwhere2))
  if n_maxwhere2 eq 0 then maxwhere2=where(cp_im2 eq max(cp_im2))
  min2 = cp2vals[minwhere2]
  max2 = cp2vals[maxwhere2]

  minall = min([min1, min2])
  maxall = max([max1, max2])


  ;now do things differently depending on whether x and y axis to be
  ;   same range or not

  if one_to_one eq 1 then begin
    min1 = minall
    max1 = maxall
    min2 = minall
    max2 = maxall

  end
  bin1 = float(max1-min1) / nbins[0]
  bin2 = float(max2-min2) / nbins[1]



  ;what is the binsize, given the number of bins?

;  bin1 = float(maxall-minall) / nbins[0]  ;x axis
;  bin2 = float(maxall-minall) / nbins[1]  ;y axis

  ho = hist_2d(reform(image1), reform(image2), bin1 = bin1, bin2 = bin2, min1=min1, min2=min2, max1=max1, max2=max2)
  hd = blowup(ho, blowupby)


  !order = 0

  ;get size of the 2d image

  sz = size(hd, /dim)

  ;if the ranges are not the same at all, we'll want to scale the
  ;  two axes so the display is square.
  ;    rs = float(sz[0])/sz[1]
  ;
  ;    if rs gt 1.25 then begin
  ;       hd = aggby(hd, [1.0, 1/rs])
  ;       sz = size(hd, /dim)
  ;    end
  ;    if rs lt 0.75 then begin
  ;       hd = aggby(hd, [rs, 1])
  ;       sz = size(hd, /dim)
  ;    end
  ;



  ;figure out the legend size in terms of window pixels

  legendbarsize = [sz[0]*legend_scale[0], sz[1]*legend_scale[1]]
  legendsize = [legendbarsize[0]+(sz[0]*legend_textwidth[0]), legendbarsize[1]]
  legendlowleft = [xmargin[0]+sz[0]+(legend_pos[0]*sz[0]), (legend_pos[1]*sz[1])+ymargin[0]]

  ;figure out window size

  ; winsize = [legendlowleft[0]+legendsize[0]+xmargin[1], sz[1]+total(ymargin)]
  winsize = [xmargin[0]+sz[0]+xmargin[1], ymargin[0]+sz[1]+ymargin[1]]



  ;load the colors

  ;if n_elements(colorsavefile) ne 0 then restore, colorsavefile else begin
  c = get_madcal_colors()
  rrr=c.rrr
  ggg=c.ggg
  bbb=c.bbb
  ;end

  tvlct, rrr, ggg, bbb


  ;make window and paint to background color
  w, window, winsize[0], winsize[1], ypos= 400

  bimg = intarr(winsize[0], winsize[1])+background
  tv, convert_256im_to_ff(bimg, rrr, ggg, bbb)  ;set background


  ;scale the image

  sthd = stretchit(long(hd), getrange = range)

  ;now convert it to 3-lay 24bit, according to the rrr, ggg, bbb
  ff_img = convert_256im_to_ff(sthd, rrr, ggg, bbb)


  tv, ff_img, xmargin[0], ymargin[0], true=3



  ;XXXXXXXXXXXXXX
  ;  ;put the shade bar on there
  ;     legend_norm_pos = [ [legendlowleft/winsize], $
  ;                              [(legendlowleft[0]/winsize[0])+(legendbarsize[0]/winsize[0]), $
  ;                               (legendlowleft[1]/winsize[1])+(legendbarsize[1]/winsize[1])] ]
  ;
  ;
  ;    !p.clip = [0, 0, sz[0]-1, sz[1]-1]
  ;
  ;
  ;     shade_bar256, hd, legend_norm_pos, minmax=userange, $     ;use the original (unscaled) hd for legend
  ;               textcolor = 1, ndigits = ndigits, $
  ;               assumedtextheight = .03, charsize = 1.5, $
  ;               textoffset = .02, charthick = charthick, $
  ;             /roundit

  ;put a box around the deal.
  ;  wset, window
  ;    contour,sthd,/noerase,/xst,/yst, $  ;Do the contour
  ;                  pos = [xmargin[0],ymargin[0], $
  ;                        xmargin[0]+sz[0], ymargin[0]+sz[1]], /dev, $
  ;               /nodata,xrange=[minall, maxall],yrange=[minall, maxall], $
  ;               _extra= e, color = 1
  ;  wset, window


  wset, window
  contour,sthd,/noerase,/xst,/yst, $  ;Do the contour
    pos = [xmargin[0],ymargin[0], $
    xmargin[0]+sz[0], ymargin[0]+sz[1]], /dev, $
    /nodata,xrange=[min1, max1],yrange=[min2, max2], $
    _extra= e, color = 1
  wset, window






  ;put the one:one line on, if app:
  if one_to_one eq 1 then $
    plots, [minall, maxall], [minall,maxall], color = 1


  ;   contour,sthd,/noerase,/xst,/yst,$  ;Do the contour
  ;                pos = [px(0),py(0), px(0)+swx,py(0)+swy],/dev,$
  ;             /nodata,xrange=xrange,yrange=yrange,_extra= e, $
  ;             color = ccolor, xticklayout = xticklayout, yticklayout = yticklayout

  plotrange=[minall, maxall]  ;set this for user if called

  info = {min1:min1, min2:min2, max1:max1, max2:max2, bin1:bin1, bin2:bin2, histvals:ho}	;return the raw info from which we can assign densities to spectral combos


  return, info

end


;May 21, 2006.  Updating to take advantage of the
;  predefined colors I set up in "get_madcal_colors.pro"

