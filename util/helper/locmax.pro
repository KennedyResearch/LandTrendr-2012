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

;-------------------------------------------------------------
;+
; NAME:
;       ONE2TWO
; PURPOSE:
;       Convert from 1-d indices to 2-d indices.
; CATEGORY:
; CALLING SEQUENCE:
;       one2two, in, arr, ix, iy
; INPUTS:
;       in = 1-d indices (may be a scalar).  in
;       arr = array to use (for size only).  in
;         Alternatively, arr can be [nx, ny]
;         where nx and ny are the image sizes
;         in x and y (saves space).
; KEYWORD PARAMETERS:
; OUTPUTS:
;       ix, iy = equivalent 2-d indices.     out
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner, 25 May, 1986.
;       Johns Hopkins Applied Physics Lab.
;       R. Sterner, 19 Nov, 1989 --- converted to SUN.
;       R. Sterner, 9 Jun, 1993 --- Allowed [nx,ny] instead of ARR.
;
; Copyright (C) 1986, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	pro one2two, in, arr, inx, iny, help=hlp
 
	if (n_params(0) lt 4) or keyword_set(hlp) then begin
	  print,' Convert from 1-d indices to 2-d indices.'
	  print,' one2two, in, arr, ix, iy'
	  print,'   in = 1-d indices (may be a scalar).  in'
	  print,'   arr = array to use (for size only).  in'
	  print,'     Alternatively, arr can be [nx, ny]'
	  print,'     where nx and ny are the image sizes'
	  print,'     in x and y (saves space).'
	  print,'   ix, iy = equivalent 2-d indices.     out'
	  return
	endif
 
	s = size(arr)
	if n_elements(arr) eq 2 then s = [0,arr]
 
	inx = in mod s(1)
	iny = in/s(1)
 
	return
 
	end

;-------------------------------------------------------------
;+
; NAME:
;       IMGFRM
; PURPOSE:
;       Puts a specified border around an image.
; CATEGORY:
; CALLING SEQUENCE:
;       imgfrm, img, vals
; INPUTS:
;       vals = array of frame values. in.
; KEYWORD PARAMETERS:
; OUTPUTS:
;       img = Image to modify.        in, out.
; COMMON BLOCKS:
; NOTES:
;       Notes: values in array vals are applied from
;         outside border of the image inward.  A single
;         scalar values just replace the border values
;         in the image.  Good for zeroing image edge.
; MODIFICATION HISTORY:
;       R. Sterner. 25 Sep, 1987.
;       R. Sterner, 27 Jan, 1993 --- dropped reference to array.
;       Johns Hopkins University Applied Physics Laboratory.
;
; Copyright (C) 1987, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	PRO IMGFRM, IMG, VALD, help=hlp
 
	IF (N_PARAMS(0) LT 2) or keyword_set(hlp) THEN BEGIN
	  print,' Puts a specified border around an image.'
	  PRINT,' imgfrm, img, vals'
	  PRINT,'   img = Image to modify.        in, out.'
	  PRINT,'   vals = array of frame values. in.'
	  print,' Notes: values in array vals are applied from'
	  print,'   outside border of the image inward.  A single'
	  print,'   scalar values just replace the border values'
	  print,'   in the image.  Good for zeroing image edge.'
	  RETURN
	ENDIF
 
	VAL = VALD
	NV = N_ELEMENTS(VAL)
	SZ = SIZE(IMG)
	NX = SZ(1)
	NY = SZ(2)
 
	FOR I = 0, NV-1 DO BEGIN
	  T = FLTARR(NX-I-I) + VAL(I)
	  IMG(I,I) = T
	  IMG(I,NY-1-I) = T
	  T = TRANSPOSE(FLTARR(NY-I-I) + VAL(I))
	  IMG(I,I) = T
	  IMG(NX-1-I,I) = T
	ENDFOR
 
	RETURN
	END

;-------------------------------------------------------------
;+
; NAME:
;       LOCMAX
; PURPOSE:
;       Find local maxima in an image.
; CATEGORY:
; CALLING SEQUENCE:
;       locmax, img
; INPUTS:
;       img = image to process.               in
; KEYWORD PARAMETERS:
;       Keywords:
;         MASK=m returns a mask image with 1 at all
;           local maxima and 0 elsewhere.
;         WHERE=w returns 1-d indices of all local maxima.
;           -1 if no local maxima.
;         VALUES=v returns values of img at all local maxima.
;         VALUE_IMAGE=vimg use vimg to determine values.
;           Instead of img.
;         IX=ix returns x index of all local maxima.
;         IY=iy returns y index of all local maxima.
;         /SORT sorts local maxima by descending image values.
;         /NOEDGE ingores any maxima at image edges.
; OUTPUTS:
; COMMON BLOCKS:
; NOTES:
;       Notes: All output is through keywords.
;         Ignores plateaus.  May not work for
;         all edge points.
; MODIFICATION HISTORY:
;       R. Sterner, 17 Aug, 1990
;       R. Sterner, 27 Aug, 1990 --- added value_image.
;
; Copyright (C) 1990, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	pro locmax, img, mask=m, where=w, ix=ix, iy=iy, sort=srt, $
	  values=v, value_image=vimg, noedge=noed, help=hlp
 
	if (n_params(0) lt 1) or keyword_set(hlp) then begin
 	  print,' Find local maxima in an image.'
	  print,' locmax, img'
	  print,'   img = image to process.               in'
	  print,' Keywords:'
	  print,'   MASK=m returns a mask image with 1 at all'
	  print,'     local maxima and 0 elsewhere.'
	  print,'   WHERE=w returns 1-d indices of all local maxima.'
	  print,'     -1 if no local maxima.'
	  print,'   VALUES=v returns values of img at all local maxima.'
	  print,'   VALUE_IMAGE=vimg use vimg to determine values.'
	  print,'     Instead of img.'
	  print,'   IX=ix returns x index of all local maxima.'
	  print,'   IY=iy returns y index of all local maxima.'
	  print,'   /SORT sorts local maxima by descending image values.'
	  print,'   /NOEDGE ingores any maxima at image edges.'
	  print,' Notes: All output is through keywords.'
	  print,'   Ignores plateaus.  May not work for'
	  print,'   all edge points.'
	  return
	endif
 
	fuzz = 1.e-8		; Ignore values below this.
 
	;-----  Shift eight ways  -----
	dx1 = shift(img,1,0)
	dx2 = shift(img,-1,0)
	dy1 = shift(img,0,1)
	dy2 = shift(img,0,-1)
	
	dxy1 = shift(img, 1,1)
	dxy2 = shift(img, 1,-1)
	dxy3 = shift(img, -1,1)
	dxy4 = shift(img, -1,-1)	
	
	;------  compare each pixel to 8 surrounding values  -------
	m = (img gt dx1) and (img gt dx2) and (img gt dy1) and (img gt dy2) $
		and (img gt dxy1) and (img gt dxy2) and (img gt dxy3) and (img gt dxy4)  
	if keyword_set(noed) then imgfrm, m, 0
	
	;------  number of local maxima  --------
	w = where(m eq 1, count)	; Find local maxima.
	if count eq 0 then begin	; If none found right away, then 
			ix=(iy=[-1])	; set values to -1.  Added REK 3/31/98
			goto, past
			end
	fzz = img(w)			; Pull out values.
	wfzz = where(fzz lt fuzz, c)	; Look for values below fuzz.
	if c gt 0 then begin		; Found any?
	  m(w(wfzz)) = 0		;   Yes, zap them.
	  w = where(m eq 1, count)	;   Now try again for local maxima.
	endif
	;------  if any continue  ------
	if count gt 0 then begin
	  if n_elements(vimg) eq 0 then begin	; Pick off values at maxima.
	    v = img(w)
	  endif else begin
	    v = vimg(w)
	  endelse
	  if keyword_set(srt) then begin	; Sort?
	    is= reverse(sort(v))		; yes.
	    v = v(is)
	    w = w(is)
	  endif
	  one2two, w, img, ix, iy		; Convert to 2-d indices.
	endif
       
        past:
	return
 
	end
