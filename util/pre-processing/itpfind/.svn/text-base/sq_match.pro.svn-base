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
;   Robert E. Kennedy, Oregon State University (robert.kennedy@orst.edu)
;   This software is supplied as is, with no express or implied
;   warranties.

;THIS FILE CONTAINS:
;    sq_match --> the main matching program

;Function sq_match.pro
;
;Give it two images, it returns the map coordinates (x,y) of the
;   ref_img that correspond to point where the inp_img
;   matches.
;
;Additionally, the function returns analysis values in the same structure
;   that contains the subscripts.  Thus the structure returned is
;  str.coords   ->    x,y values as above
;  str.valid    -> 1 if value is valid, 0 if threshold not passed.
;
;b_upleft_coords are the coordinates (in FILE values) of the upperleft EDGE of
;   the base image.  Note that we use the EDGE, not the center
;If ignore value is not set, it will not filter out bad values.  Generally,
;   edges of images are 0 and thus ignore should be set to zero.
;Neighborhood is the range (in pixels) within which we expect to find a
;   match.  If this is not set, 10 pixels in either direction is assumed
;
;Cutoff is the percentage of either image that must be valid pixels for the
;   coordinate matching to continue.  For small areas, .75 is good.  For
;   larger, .5 is sufficient (i.e. when doing the original image)
;Threshold determines whether the algorithm considers that it's found a
;   an image match.  The covariogram is anchored at zero and the maximum
;   value divided by the median.  If this is below the threshold, we
;   throw this point out

function sq_match, ref_img, inp_img, pixsize, ref_centerpoint, $
     ignore=ignore,    neighborhood = neighborhood, cutoff=cutoff, $
     threshold=threshold, diag_un2=diag_un2, diag_un1=diag_un1




if n_elements(neighborhood) eq 0 then varsize=21 else varsize=(neighborhood*2)+1
if n_elements(cutoff) eq 0 then cutoff = .70
if n_elements(threshold) eq 0 then threshold = {minsteepness: .35}
if n_elements(diag_un) eq 0 then diag_un = -1

;
double_thr = .5 ;if there is a second peak that meets the criterion, then
       ;the difference between it and the first peak must be
       ;greater than double_thr * threshold.minsteepness
       ;This should probably be put into


;set up the 'too_small' variable, which will keep track of how many times
;   we expand the neighborhood and retry the variogram.

too_small = 1


;normalize the images
   write_diagnosis, diag_un1, 'sq_match: Normalizing images'

if n_elements(ignore) eq 0 then begin
    send_ref = normalize(ref_img)
    send_inp = normalize(inp_img)
end else begin
        send_ref = normalize(ref_img, ignore = ignore(0))
        send_inp = normalize(inp_img, ignore = ignore(1))
        end
wset, 1



getit:

   write_diagnosis, diag_un1, 'sq_match: Calling covar'

if n_elements(ignore) eq 0 then $
    covar, send_ref, send_inp, cov, tie_coords,$
         varsize=varsize, cutoff=cutoff, diag_un1=diag_un1 else $
    covar, send_ref, send_inp, cov, tie_coords, varsize=varsize, $
       ignore=-99, cutoff=cutoff, diag_un1=diag_un1
;stop

print, "min covar" + string(min(cov))
print, "max covar" + string(max(cov))

;qqq = get_kbrd(1)


  ;if more than the cutoff percentage is in the ignore
  ;value, then we return a -1 in the valid variable
    wset, 1
    ;shade_surf, cov


    if cov(0,0) eq -99 then begin
            write_diagnosis, diag_un1, 'sq_match: Covar did not find points'

         xyouts, .1, .1, 'not enough points', /norm
           print, 'Not enough valid points in this window'
           valid=-1
           goto, past
           end


 ;if the whole image is not the ignore value, then
 ;test
    cov = cov- min(cov) ;anchor at zero
    valid = 0

 ;fit a function to eliminate minor peaks

    ;cov = sfit(cov, 5) ;5th order
    shade_surf, cov
    wset, 1
tvscl, ref_img
wset, 1
tvscl, inp_img,0


  ;new routine using locmax from Sterner IDL library. See locmax for copyrights

   locmax, cov, ix=ix, iy=iy, values=values, /sort

   ;Find the best peak.
      peak = (okay=0)   ;set up flags

     if ix[0] ne -1 then begin  ;if we found a peak of some sort
       peak = 1
       sp = score_peaks(cov, ix, iy)
       sortsubs = reverse(sort(sp))
         ix = ix[sortsubs]
         iy = iy[sortsubs]
         sp = sp[sortsubs]
         j = where(sp gt threshold.minsteepness, many)
       print, '-----'
       print, '    Scores for peaks are'
       print, sp


   ;compute confidence in the peak
   ;   if there's only one peak above the confidence, confidence
   ;   is a squared function that increases the further we get
   ;   from the

      if many le 1 then $
      conf = (sp[0]^2)/threshold.minsteepness^2 else $
      conf = ((sp[0]-sp[1])^2)/ threshold.minsteepness^2

       print, '    Confidence score'
       print, conf


        print, '-----'


       case 1 of
       (many eq 0): okay = 0
       (many eq 1):     okay = 1
       (many ge 2):     begin
                 if sp[0]-sp[1] gt $
                 double_thr*threshold.minsteepness then $
                 okay = 1 else okay = 0
               end
       endcase

     end


     ;if there was a peak but not threshold


     if (peak eq 1 and okay eq 0) then $
         j=[0]  ;set these up to be caught in the next test


     if (peak eq 1 and okay eq 1) then $  ;if a peak, and if peak okay
         max_pos = [ix[j], iy[j]] else max_pos = [-1]  ;else reset max_pos

     if peak eq 0 then begin    ;if no peak, then
         j=[0]   ;set these up to be caught in the next test
         ix[0]=0    ; we want to force it to expand neighborhood.
         iy[0]=0
     end




   ;If there was no peak found, or if peak is right at the edge,
   ;   we expand the neighborhood and try again




     if (ix[j[0]] eq 0) or (ix[j[0]] eq varsize[0]-1) or $
      (iy[j[0]] eq 0) or (iy[j[0]] eq varsize[1]-1) then begin
         too_small = too_small +1
         valid = (okay = 0)
         if too_small eq 4 then begin   ;   if we've already doubled the size
                   ;   twice, we give up

            goto, past
            end
         varsize =  ((varsize(0)-1)*2)+1
         print, 'Trying again to get it with larger neighborhood'

            ;NOTE: ideally, I should only run the covariance
            ;  stuff for the portions of the neighborhood that
            ;  I haven't already tried.  That will require changing
            ;  how the covar program references things -- right
            ;  now it just figures the x and y offsets to use.
            ;  what you'd need to do is either pass the existing
            ;  covar, or pass the x and y offsets explicitly, so
            ;  you leave out the existing ones.



         goto, getit    ;try again with this larger variable
   end

   if max_pos[0] ne -1 then valid = 1   ;we have
           ;test because a non-valid peak that
           ;is not in the center will pass through
           ;all the other tests.





past:

if valid le 0 then begin
       print, 'Not valid in sq_match.'

       str = {coords:[0,0], valid:valid}

         return, str
    end


;if this is valid, calculate the new coords

    offset = (max_pos + tie_coords)*pixsize*[-1,-1]
    coords = ref_centerpoint-offset
    print, 'Valid in sq_match'



  ;if we're producing the diagnosis file, write to it now
a= size(cov)
sizecov = a(1)*a(2)
if diag_un2 ne -1 then begin

   writeu, diag_un2, a(1)
   writeu, diag_un2, a(2)
   writeu, diag_un2, cov

end

;assign structure
   str = {coords:coords, offset:offset, valid:valid, $
    sizecov:sizecov, conf:conf}


return, str
end



