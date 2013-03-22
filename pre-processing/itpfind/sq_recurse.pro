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
;   copied, or redistributed without express written permission of
;   Robert E. Kennedy, Oregon State University (kennedyr@fsl.orst.edu).
;   This software is supplied as is, with no express or implied
;   warranties.




;#############################################################################
pro sq_recurse, is, rs, i_center, r_center, params, iteration, movevalue, $
    valid, conf=conf, diag_un1=diag_un1, diag_un2=diag_un2, $
    threshold=threshold, cutoff=cutoff


if n_elements(cutoff) eq 0 then cutoff= .70 ;min proportion of okay pixels
                 ;necessary for covarcalc
if n_elements(diag_un1) eq 0 then diag_un1=-2
if n_elements(diag_un2) eq 0 then diag_un2=-1
  i_dims = dims(is.img)
  r_dims = dims(rs.img)
  master_r_center = r_center    ;we keep these just in case, after lots of
  master_i_center = i_center    ;moving around, we couldn't locate a match
  master_movevalue = movevalue
  write_diagnosis, diag_un1, 'Iteration' +string( iteration)
  write_diagnosis, diag_un1, 'Master_r_center'+string( master_r_center)
  write_diagnosis, diag_un1, 'Master_i_center'+string( master_i_center)

    ;first extract subset of whole image.  we focus in on the
    ;   center point.  The image is returned.  Note that if
    ;   the center point is near the edge of is.img, we'll need
    ;   to bail out and return to squeeter's calling program
    ;   with a flag notifying that we're too far off.

       ;figure out the window size for this iteration.  It
       ;is based on the original dimension of the parent image,
       ;modified by the zoom factor

          winsize =  i_dims / params.zoom(iteration)

       ;set up dirvect for this layer

          sq_resetdirvect, dirvect

      insideloop:       ;the internal loop in case we don't find a valid
               ;point on the first try

       ;first make sure that the center is an integer multiple of
       ;pixels.  We use adj_int_mult to move the center point to an
       ;upper corner of the center pixel, then add a half-pixel if
       ;the window-size is an odd number.

          ic= i_center
          i_center = adj_int_mult([-.5, -.5], [1.0, 1.0], i_center) + $
                ([.5, .5] * [evenodd(winsize(0)), evenodd(winsize(1))] )
          movevalue(*,1)= movevalue(*,1)+[ic-i_center]
          rc= r_center
          r_center = adj_int_mult([-.5, -.5], [1.0, 1.0], r_center) + $
                ([.5, .5] * [evenodd(winsize(0)), evenodd(winsize(1))] )
          movevalue(*,0)= movevalue(*,0)+[rc-r_center]

print, 'movevalue
print, movevalue


       ;then extract the reference image.  The center point of the reference
       ;   image may have moved, so we need to make sure that
       ;   we do it first, just in case it needs to be resized because
       ;   it's too near the edge for the window to fit all the way in.

				 write_diagnosis, diag_un1, 'Extracting reference subset'

         r_sub = sq_extractsubset(rs.img, r_center, winsize, diag_un1=diag_un1)
         if r_sub.valid eq 0 then begin
              valid = 0
              goto, past
              end

         r_subimg = r_sub.img
         new_r_dims = dims(r_subimg)    ;did we have to change the size of the
                  ;window?  We just make sure that this
                  ;window size gets passed to the
                  ;next extraction

       ;now extract the input image

	         write_diagnosis, diag_un1, 'Extracting input subset'

          i_sub = sq_extractsubset(is.img, i_center, new_r_dims, diag_un1=diag_un1)
          if i_sub.valid eq 0 then begin
             valid = 0
             goto, past
             end



          i_subimg = i_sub.img


          new_i_dims = dims(i_subimg)

          if new_r_dims(0) ne new_i_dims(0) or $
            new_r_dims(1) ne new_r_dims(1) then begin
            valid =0
            goto, past
            end



      ;then aggregate by pixel_aggs.
         write_diagnosis, diag_un1, 'Aggregating reference image'

         isub = aggby(i_subimg, [params.pixel_aggs(iteration), $
            params.pixel_aggs(iteration)], ignore = is.img_info.ignore)



           write_diagnosis, diag_un1, 'Aggregating input image'

         rsub = aggby(r_subimg, [params.pixel_aggs(iteration),$
             params.pixel_aggs(iteration)], ignore = rs.img_info.ignore)
      ;send version of threshold

        sendthr = {minsteepness:params.threshold.minsteepness(iteration)}



      ;GET MATCHED COORDS!

       write_diagnosis, diag_un1, 'Calling sq_match'

        r = sq_match(rsub, isub, $
            [params.pixel_aggs(iteration), params.pixel_aggs(iteration)],$
              r_center, threshold=sendthr, neighborhood=params.nbhds(iteration), $
              ignore=[rs.img_info.ignore, is.img_info.ignore], $
              diag_un2=diag_un2, diag_un1=diag_un1, cutoff=cutoff)



      ;if this level worked, then call the next level, assuming we're
      ; not at the maximum number of iterations



        if r.valid gt 0 then begin
           ;printf, diag_un1, 'This level valid.'

           ;Set up confidence

             if n_elements(conf) eq 0 then conf = r.conf else $
                  conf = (conf + r.conf)/2.

           ;how much did we end up moving?
           write_diagnosis, diag_un1, 'Reference offset: '+string( r.offset)

           movevalue(*,0) = movevalue(*,0) + r.offset
                   ;how much did we have to move the reference
                   ;center point? recall that offsets are in the
                   ;standard form of:  new=proposed-offset
                   ;Also, the units are pixels of the original
                   ;images, since we sent sq_match the agg value
                   ;as the fake pixel size.

           valid = 1
           write_diagnosis, diag_un1, 'r.coords'+string( r.coords)
           r_center = r.coords



           iteration = iteration + 1
           if iteration eq params.iterations then goto, past    ;if we've done
                      ;all levels
       ;otherwise, go to the next level

       sq_recurse, is, rs, i_center, r_center, params, iteration, $
              movevalue, valid, diag_un1=diag_un1, diag_un2=diag_un2, $
              conf=conf, cutoff=cutoff

       ;when we return from the next level, we've either got a good point or
       ;    we couldn't find on in our constraints of maxmove. If the former
       ;    then we just need to get out of here.  If the latter, we
       ;    need to move the image at this level, so we set r.valid to 0
       print, 'valid score for second level'
       print, valid

       if valid ne 0 then goto, past

       ;if we're here, it means that this level was valid, but
       ;    that we couldn't get anything to work at the next level
       ;    down.  In that case, we need to set valid to zero, and
       ;    we also need to erase the covariogram that we wrote
       ;    at this level

       r.valid = 0


       if diag_un2 ne (-1) then begin
         point_lun, -diag_un2, wh  ;get current position
         newpos = wh - (r.sizecov*4) - 8   ;8 for the two size parameters,
                 ;r.sizecov is returned from sq_match in
                 ;  n_elements of covariogram, need to
                 ;*4 because of floatpoint type.
         point_lun, diag_un2, newpos
       end


     end

    ;if we're here, then that means we've either not found a point
    ;   at this location and this level, or that we did find a point here,
    ;   but searched and searched at the next level and couldn't find
    ;   a point.  In either case, we need to nudge our search window.
    ;   In order to use sq-movecenter, we need to make some fake
    ;   map info and such.

         write_diagnosis, diag_un1, 'not valid at level '+string( iteration)

         fake_refinfo = {gifov:[1.0,1.0], pixelsize:[1.0,1.0], $
            centerpoint:r_center, $
           upl:[-.5, r_dims(1)-.5], lor:[r_dims(0)-.5, -.5]}
             fake_inpinfo = {gifov:[1.0,1.0], pixelsize:[1.0,1.0], $
                  centerpoint:i_center, rotation:0, $
           upl:[-.5, i_dims(1)-.5], lor:[i_dims(0)-.5, -.5]}


             xyouts, .1, .4, 'just before sq_movecenter', /norm
             sq_movecenter, dirvect, fake_refinfo, fake_inpinfo, $
                  {window_size:winsize, nudgefactor:params.nudgefactor, $
                  edgezone:params.edgezone},$
                  movevalue=movevalue, maxmove=params.maxmove, $
                  /nudge
                       ;note that movevalue contains the all the movement
                       ;from the starting point, including the coarse
                       ;jump to the new center and the movevalue carried
                       ;down from higher recursions of this program, if
                       ;applicable.
              write_diagnosis,diag_un1, 'dirvect.valid in sq_recurse'+string(dirvect.valid)


             if dirvect.valid eq 0 then begin   ;if we've tried all directions
                          ;and shells
                  ;if we tried a bunch of places here and didn't
                  ; get a hit, we need to reset the i_center and r_center
                  ; for the next layer up, since that layer's going to
                  ; nudge the image based on them.

                  r_center = master_r_center
                  i_center = master_i_center
                  movevalue = master_movevalue
                  ;set valid flag to zero
                  valid = 0
                  iteration = iteration -1

                  goto, past   ;we didn't find a point here
                  end

             i_center = fake_inpinfo.centerpoint
             r_center = fake_refinfo.centerpoint
             goto, insideloop   ;try it again
 past:
 return
end
