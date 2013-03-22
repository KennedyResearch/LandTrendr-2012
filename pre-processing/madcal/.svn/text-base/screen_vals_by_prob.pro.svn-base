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

function screen_vals_by_prob, x, y, threshold = threshold, $
    percent_cutoffs = percent_cutoffs
    
    
  ;take the x and the y vals
  ;make a 2-d histogram
  ;  then cumulate that to see what the low-count combos are
  ;  then take those out, and return
  ;  the indices of the good combos
    
  if n_elements(threshold) eq 0 then threshold = 0.00	;below this count in the 2-d histogram, throw them out
  if n_elements(percent_cutoffs) eq 0 then percent_cutoffs = [0.00, 1.0]
  
  
  
  ;get info on the images
  ;   use the probability distribution to pick
  ;   the bounds of the range to display
  
  cp_im1 = c_prob(reform(x), bin_count_min=100, vals=cp1vals)
  minwhere1= min(where(cp_im1 ge percent_cutoffs[0]))
  maxwhere1= max(where(cp_im1 le percent_cutoffs[1]))
  min1 = cp1vals[minwhere1]
  max1 = cp1vals[maxwhere1]
  
  cp_im2 = c_prob(reform(y), bin_count_min=100, vals=cp2vals)
  minwhere2= min(where(cp_im2 ge percent_cutoffs[0]))
  maxwhere2= max(where(cp_im2 le percent_cutoffs[1]))
  min2 = cp2vals[minwhere2]
  max2 = cp2vals[maxwhere2]
  
  minall = min([min1, min2])
  maxall = max([max1, max2])
  
  
  
  ;what is the binsize, given the number of bins?
  
  nbins = [100,100]
  
  
  bin1 = float(maxall-minall) / nbins[0]  ;x axis
  bin2 = float(maxall-minall) / nbins[1]  ;y axis
  
  hd = hist_2d(reform(x), reform(y), bin1 = bin1, bin2 = bin2, min1=minall, min2=minall, max1=maxall, max2=maxall)
  
  nozeros = where(hd ne 0, manynozeros)
  
  
  ;APPLY THE THRESHOLD
  
  ;get the range
  
  mincount = min(hd)
  maxcount = max(hd)
  
  
  ;figure out the threshold value
  if threshold gt 1.0 then begin
    print, 'Threshold is from 0 to 1.0, and represents the
    print, '    the proportion of the range of counts in the
    print, '    2-d histogram below which to screen out'
    return, 0
  end
  
  
  threshval = (maxcount-mincount) * threshold
  
  ;set up the bytarrs for masking
  
  xmask = (ymask = bytarr(n_elements(x)))
  
  
  
  
  ;now just go through a dumb loop and selection.
  ;  this is ugly
  
  
  sz = size(hd, /dim)	; the size of the 2-d histogram
  for i = 0, sz[0]-2 do begin
    xvals = (x ge minall+(i*bin1)) * $
      (x lt minall+((i+1)*bin1))
      
    for j = 0, sz[1]-2 do begin
    
      if hd[i,j] gt threshval then begin
      
        yvals = (y ge minall+(j*bin2)) * $
          (y lt minall+((j+1)*bin2))
          
        mtemp = xvals * yvals  ;find the common points
        xtemp = xvals * mtemp	;then apply those back to each
        ytemp = yvals * mtemp
        xmask = xmask + xtemp	;then add to mask, so 1's indicate use points
        ymask = ymask + ytemp
        
        
      end
    end
  end
  
  goods_x = where(xmask eq 1, manyx)
  goods_y = where(ymask eq 1, manyy)
  
  if manyx gt 0 and manyy gt 0 then begin
    xx = x[where(xmask eq 1)]
    yy = y[where(ymask eq 1)]
    return, {ok:1, x: xx, y:yy}
    
  end else return, {ok:0}		;no good points
  
  
  
  
  return, 0
end
