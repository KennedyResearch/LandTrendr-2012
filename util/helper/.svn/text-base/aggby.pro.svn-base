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

function aggby, im, blowupby, ignore = ignore

;on_error, 2
;If the program is looking for a blowupby of 1, then we need do nothing

if n_elements(blowupby) ne 2 then begin
		print, 'aggby needs blowupby to be [x,y]'
		return,0
		end


if blowupby[0] eq 1.0 and blowupby[1] eq 1.0 then return, im
;print, systime()
;posx, posy

sz = size(im)
if sz(0) eq 2 then type = sz(3) else if sz(0) eq 3 then message, 'Aggby cannot handle 3-d images'

;make an image with a 1-pixel buffer to handle edge effects later on

  image = fltarr(sz(1)+2, sz(2)+2)
  image(1:sz(1), 1:sz(2))=im

;Figure out where we need interpolates, anchor around the middle

  ;what's the maximum number of pixels?

    inpsize = sz[1:2]
    diff = inpsize /float(blowupby)	;what's the max pixels?

  ;what's the offset?

    windims= long(diff)	;take off decimal to get the number
     				;of ref-sized pixels we're going to
     				;create in the resample.

  ;whatever's left over is how much of the original image
  ;  we lose because of non-integer pixel sizes. Note that
  ;  this value is in units of (blowupby), which is equivalent
  ;  to the pixel size of the blown up image.  So we need to
  ;  multiply by blowupby to find out how many inp.pixels that
  ;  means.  Then we divide it in half to split it evenly between each
  ;  side of the image, thus keeping the centerpoint the
  ;  same.

    offset = [.5,.5]*(diff-windims)*blowupby
         	;the upper left of the resultant
         	;interpolated input image, in units
         	; of the original input image.


  ;we also need to calculate the offset to bring us to the middle
  ;	of the new-sized pixels for the interpolate command later.
  ;	Interpolate starts with (0,0) as the center of the first pixel
  ;   	(this is how it really functions; don't believe the docs)
  ;   	That offset is half of a new-pixel size (represented in
  ;   	old-pixels by the 'blowup' value)  - 0.5 because the upperleft
  ;   	portion of the pixel is -.5, -.5

    pixoffset = (blowupby / 2.0)- [.5, .5]


  ;create an array of locations that we need interpolates from

    posx = (findgen(windims[0])*blowupby[0])+offset[0]+pixoffset[0]
    posy = (findgen(windims[1])*blowupby[1])+offset[1]+pixoffset[1]

;if we're doing a sub-pixel blowupby, it makes more sense to use interpolate

if blowupby(0) lt 1.0 and blowupby(1) lt 1.0 then $
		return, interpolate(im, posx, posy, /grid)


nx = windims(0)
ny = windims(1)

new_image = fltarr(nx,ny)

window = [posx(1)-posx(0), posy(1)-posy(0)]  ;what's our pixel size?
					;check between first two locations

hw = window / 2.0
area = window(0)*window(1)
max_inf = ceil(window)+1
hm = max_inf/2.0

;set up boundary matrix
	     ; (   # x's  , #y's      , [x,y],[upl, lor], [bounds, selectfrompixs]

  m_posx = fill_arr(posx, windims(1))
  m_posy = transpose(fill_arr(posy, windims(0)))

  bounds = fltarr(windims[0], windims[1],     2,     2,  2)
  bounds(*,*,0, 0, 0)=m_posx-hw(0)	  ;upperleftx
  bounds(*,*,0, 1, 0)=m_posx+hw(0)   ;lowerrightx
  bounds(*,*,1, 0, 0)=m_posy-hw(1)   ;upperlefty
  bounds(*,*,1, 1, 0)=m_posy+hw(1)   ;lowerrighty

  ;now calc the pixels to sample from
  ;these are the upper left and lower right values of the pixels

  m_roundx = round(m_posx+.5 - hm(0))
  m_roundy = round(m_posy+.5 - hm(1))

  bounds(*,*,0, 0, 1)=m_roundx	  ;upperleftx
  bounds(*,*,0, 1, 1)=m_roundx+max_inf(0)-1   ;lowerrightx
  bounds(*,*,1, 0, 1)=m_roundy   ;upperlefty
  bounds(*,*,1, 1, 1)=m_roundy+max_inf(1)-1   ;lowerirghty

;now set up the test value, which is the difference from
	;the righthand side of the furthest left pixel
	;in the bunch.  If this is positive, that means that
	;the lefthand pixel is part of the equation.  If
	;this is negative, then the lefthand pixel is not,
	;and the second pixel in from the left needs to be
	;considered by the proportional distance away from
	;the left hand side it is.  The converse is true forh
	;the right hand side.

  testval =  (bounds(*,*,*,*,1)+.5)-bounds(*,*,*,*,0)
  testval(*,*,*,1) = bounds(*,*,*,1,0)-(bounds(*,*,*,1,1)-.5)	;redo the
  					;lower right direction of subtraction
  ;testval is (   # x's  , #y's      , [x,y],[upl, lor])

  ;now we set up the comparison array.   If testval is greater than 0,
  ;this array will hold 1; if testval is less than 0,it'll hold 0

  comparis = testval ge 0
  opp = comparis xor 1


 ;set up multiplier set
   xmult = fltarr(max_inf(0),windims[0], windims[1])+1
   ymult = fltarr(max_inf(1),windims[0], windims[1])+1

   xmult(0, *,*) = comparis(*,*,0,0)*testval(*,*,0,0)
   xmult(1, *,*) = 1+(opp(*,*,0,0)*testval(*,*,0,0))
   xmult(max_inf(0)-2,0,0)=xmult(max_inf(0)-2,*,*)+(opp(*,*,0,1)*testval(*,*,0,1))
   xmult(max_inf(0)-1,*,*)=comparis(*,*,0,1)*testval(*,*,0,1)


   ymult(0,*,*) = comparis(*,*,1,0)*testval(*,*,1,0)
   ymult(1,*,*) = 1+(opp(*,*,1,0)*testval(*,*,1,0))
   ymult(max_inf(1)-2,0,0)= ymult(max_inf(1)-2,*,*)+(opp(*,*,1,1)*testval(*,*,1,1))
   ymult(max_inf(1)-1,*,*)= comparis(*,*,1,1)*testval(*,*,1,1)

   ;print, 'Start of loop'
   ;print, systime()

if n_elements(ignore) eq 0 then begin
  for i = 0, windims[0]-1 do begin
    for j = 0, windims[1]-1 do begin

    ;make coefficient matrix

      mat = xmult(*, i,j) # ymult(*,i,j)


    ;multiply it out
    ;  Note:we add one because we added a buffer at the beginning
    ;       This allows us to skip having to test if we're outside the
    ;       bounds of the original image every time.


      new_image(i,j) = total( image(bounds(i,j,0,0,1)+1:bounds(i,j,0,1,1)+1, $
    			bounds(i,j,1,0,1)+1:bounds(i,j,1,1,1)+1)* mat) / $
    				area
;      print, size(image(bounds(i,j,0,0,1)+1:bounds(i,j,0,1,1)+1, $
;    			bounds(i,j,1,0,1)+1:bounds(i,j,1,1,1)+1), /dim)
;    	stop

   end
  end
end else begin
  for i = 0, windims[0]-1 do begin
    for j = 0, windims[1]-1 do begin
     ;make coefficient matrix

      mat = xmult(*, i,j) # ymult(*,i,j)

    ;multiply it out
    ;  Note:we add one because we added a buffer at the beginning
    ;       This allows us to skip having to test if we're outside the
    ;       bounds of the original image every time.
      c=where( image(bounds(i,j,0,0,1)+1:bounds(i,j,0,1,1)+1, $
    			bounds(i,j,1,0,1)+1:bounds(i,j,1,1,1)+1) eq ignore, many)
      if many eq 0 then $

      new_image(i,j) = total( image(bounds(i,j,0,0,1)+1:bounds(i,j,0,1,1)+1, $
    			bounds(i,j,1,0,1)+1:bounds(i,j,1,1,1)+1)* mat) / $
    				area else $

      new_image(i,j) = ignore
   end
  end
end


;print, 'End of loop'
;print, systime()

;make new_image same type as incoming

case 1 of
 (type eq 1): new_image = byte(round(new_image))
 (type eq 2): new_image = fix(round(new_image))
 (type eq 3): new_image = long(round(new_image))
 (type eq 4):	;no need cuz already float
else:  message, 'Aggby does not support this type of data. Returning float.'
endcase

;print, 'Image re-done'
;print, systime()

return, new_image
end




