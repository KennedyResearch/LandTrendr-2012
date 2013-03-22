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
;	copied, or redistributed without express written permission of
;	Robert E. Kennedy, Oregon State University (kennedyr@fsl.orst.edu).
;	This software is supplied as is, with no express or implied
;	warranties.

;basic variogram program
;
;Take one base image layer, one move image, calculate semivariogram
;	User specifies the point in base_image space where they want
;	the move image calculation stuff to start -- negative values
;	simply mean that we want the move image upper left to start
;	outside the upper left of the base image
;The program starts the covariance calculations by anchoring both images
;	at their centers.  If they are not the same size, it doesn't matter.

pro covar, base_image, move_image, var, tie_coords, $
	varsize=varsize, ignore=ignore, cutoff = cutoff, $
	variog = variog, diag_un1=diag_un1


;if varsize is not chosen, default is 21

if n_elements(varsize) eq 0 then varsize = 21

;cutoff is the percentage of pixels that must be good (i.e. not ignore vals)
;	for the variogram to function

if n_elements(cutoff) eq 0 then cutoff = .70

;if the user hasn't chosen the variogram, then we do the covariance

if n_elements(variog) eq 0 then variog = 0


;get size stats on these images

  a=size(base_image)
  b_im_size = [a(1),a(2)]
  a=size(move_image)
  m_im_size = [a(1),a(2)]


   ;if image is too small and varsize hasn't been chosen explicitly,
   ; we return to the user rather than run an unreasonable variogram

       testv = (varsize -1)/2
       minrat=2.		;how many times larger the
       				;image must be than the largest move of
       				;variogram

      write_diagnosis, diag_un1, 'covar:  testing image size relative to variogram size'

       if m_im_size(0)/minrat lt testv or m_im_size(1)/minrat lt testv or $
       	b_im_size(0)/minrat lt testv or b_im_size(1)/minrat lt testv then begin
       			var = fltarr(m_im_size(0), m_im_size(1)) - 99.0
       			 write_diagnosis, diag_un1, 'covar: Image size too small'
       			 write_diagnosis, diag_un1, 'covar: m_im_size = '+string(m_im_size)
       			 write_diagnosis, diag_un1, 'covar: b_im_size = '+string(b_im_size)
       			  write_diagnosis, diag_un1, 'covar: testv = '+string(testv)


       			print, "COVAR: Image size too small.  Returning."
       			return
       			end


;we start at the center of the base_image
;ending point is the point to the lower right of the base
;	image that is symmetrical to the upper left start_coords
;       If start_coords are negative, then subtracting them (adds to)
;	pushes the lower right of the move_image out the lower
;	right side of the base image.

  base_center = b_im_size/2	;remember that this array has 2 elements
  start_coords = fix(base_center - (m_im_size/2) - (varsize/2))
  end_coords = fix(base_center - (m_im_size/2) + (varsize/2))


;set up the variable that will hold the variogram values
  var = fltarr(end_coords(0)-start_coords(0)+1,$
  	 end_coords(1)-start_coords(1)+1)

  a=size(var)
  varsize = [a(1), a(2)]	;if varsize started out even, it
  				;will be changed to odd.
  d=0	;for diagnosis

;LOOP through all the positions in between starting and
;	ending points, extract overlapping values, get semi-variance

   write_diagnosis, diag_un1, 'covar: beginning loop through lags'

  for h = start_coords(0), end_coords(0) do begin

    ;for each h, the x coordinates of the windows stay the same,
    ;    so we only need to figure these out once


    for v = start_coords(1), end_coords(1) do begin

      ;calculate the coordinates for the area in the move image
      ;	that overlaps the base image
      ; The first two are the [x,y] of the start, the next are the end coords
      ;  overlap_move(*,0) = starting coords
      ;  overlap_move(*,1) = ending coords


        overlap_move = [ [-h,-v], [b_im_size(0)-1-h, b_im_size(1)-1-v] ]

        ;check for the interior situation , where these points are moot

        l= where(overlap_move(*,0) lt 0, many)
        if many ne 0 then overlap_move(l,0) = 0
        if overlap_move(0,1) gt m_im_size(0)-1 then $
        	overlap_move(0,1) = m_im_size(0)-1
        if overlap_move(1,1) gt m_im_size(1)-1 then $
        	overlap_move(1,1) = m_im_size(1)-1

      ;calculate the coordinates for the area in the base image
      ;	that overlaps the move_image

        overlap_base = [ [h,v], [h+m_im_size(0)-1, v+m_im_size(1)-1] ]

        ;check for the exterior situation, where you'll have to
        ;  truncate the the coordinates so you don't try to sample from
        ;  outside the base image

          l= where(overlap_base(*,0) lt 0, many)
          if many ne 0 then overlap_base(l,0) = 0

          if overlap_base(0,1) gt b_im_size(0)-1 then $
          	overlap_base(0,1) = b_im_size(0)-1
          if overlap_base(1,1) gt b_im_size(1)-1 then $
          	overlap_base(1,1) = b_im_size(1)-1

      ;extract the values from the two images


        move_comp = move_image(overlap_move(0,0):overlap_move(0,1), $
        			overlap_move(1,0):overlap_move(1,1) )
        base_comp = base_image(overlap_base(0,0):overlap_base(0,1), $
        			overlap_base(1,0):overlap_base(1,1) )

      ;for analysis, check to make sure we're getting the right values
        disp_im = base_image
        disp_im(overlap_base(0,0):overlap_base(0,1), $
        			overlap_base(1,0):overlap_base(1,1) ) = move_comp



      ;figure out which point in the variogram array this
      ;	corresponds to

        xpoint = h-start_coords(0)
        ypoint = v-start_coords(1)

      ;do the variogram

        if n_elements(ignore) ne 0 then $
        	k = where(move_comp ne ignore and base_comp ne ignore, N) else $
        	k = where(move_comp eq move_comp, N)

      ;if there just aren't enough good points, we return with flags in
      ;		the variogram value
        if N lt (cutoff*(n_elements(move_comp))) then begin

        		var(*,*)=-99
						write_diagnosis, diag_un1, 'covar: not enough points in window'
						write_diagnosis, diag_un1, 'covar: number of points = '+string(N)
						write_diagnosis, diag_un1, 'covar: cutoff = ' + string(cutoff * (n_elements(move_comp)))


        		return
     			end

      ;now we check to see whether to do covariance or variance


      ;aa=base_comp & aa(where(base_comp eq ignore)) = 0
      ;bb=move_comp & bb(where(move_comp eq ignore)) = 0
      ;cursor, x, y

      ;tv, stretch(aa+bb)
      ;if variog eq 0 then begin
       ;covariogram
        ms = (mean(base_comp(k)) * mean(move_comp(k)) )
        c= base_comp(k) * move_comp(k)
       ;xyouts, .3, .1, d, color = 0, /normal	;for diagnosis

       d=total(c-ms)/ float(N)		;for diagnosis
        var(xpoint, ypoint) = d
      ;end else begin
      ;  c= ( base_comp(k) - move_comp(k) ) ^2
      ;  sum = total(c)
      ; var(xpoint, ypoint) = sum/float(2*N)
      ;end

      ;xyouts, .3, .1, var(xpoint, ypoint), color = 'ffffff'Xl, /normal	;for diagnosis
      ;wait, .5

    end ;h
  end  ;v

 write_diagnosis, diag_un1, 'covar: Done with loop'

tie_coords = start_coords
return
end


