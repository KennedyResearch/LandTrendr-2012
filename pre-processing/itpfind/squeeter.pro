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


function squeeter, reference, input, params, diag_un1 = diag_un1, diag_un2=diag_un2



;
; NAME:  SQUEETER
;
;
; PURPOSE:  The actual GCP finding routine, called by squeet.pro
;
;
; CATEGORY:  Geometric registration
;
;
; CALLING SEQUENCE:  Called from "squeet.pro"
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
;
; INPUTS:  ref.filename --> filename of reference image
;      ref.upl -->  [x,y] of upperleft corner, map coords.
;      ref.lor -->  [x,y] of lowerright corner (will be checked
;              against the header of the image)
;      ref.pixelsize --> [x,y]  in map units.
;      ref.centerpoint --> [x,y] the starting point for the
;             search in the reference image
;      ref.layer -->  the layer used to make this image from the disk
;      ref.gifov -->  the ground-instantaneous field of view. I.e. the
;          area on the ground covered by one pixel.
;          Likely the same as pixelsize.
;      ref.ignore -->  the value in the image that is background
;      input.filename, .upl, .lor, .pixelsize, .startpoint, layer
;     for image to be registered.  Note that these are in map
;     coordinates of the image on disk (which, if not registered,
;     will just be some arbitrary system).  We also need to
;     keep track of the actual area on the ground represented
;     by the input pixels, in units of the reference image.  That
;     value is:
;      input.gifov
;
;      input.rotation -->  rotation angle in degrees clockwise from
;          the reference image
;
;      params.window_size -->  window_size in pixels of the region
;             in reference image to be extracted
;             and tested.
;      .window_spacing -->  spacing between centers of regions
;             to be extracted and tested.
;     .iterations -->  the number of recursions to zoom in the
;             test regions.  The remaining
;             tags on params must have one value
;             for each iterations.
;      .zoom -->  zoom factor for area to test on each iteration.
;             Cannot be <1.
;      .pixel_aggs -->  aggregate by groups of this many pixels
;             on iteration 'n'
;      .nbhds -->  neighborhoods to test the variogram value in.
;             In units of aggregated pixels.
;      .thr_dist -->  the distance from the highest covariogram
;             value that we'll test against the
;             the 'thr_thr' value
;      .nudgefactor -->  used before this level, the nudgefactor
;          is the decimal proportion of a window_size
;          to nudge the window if no match is found in
;          the prior window.
;      .edgezone -->  If, after moving a window, we're outside the
;          bounds of the image but still within .edgezone
;          pixels of the boundary, we're allowed to move
;          the window right up to the edge.
;
; OPTIONAL INPUT PARAMETERS:
;
;
; KEYWORD Parameters:
;
;
; OUTPUTS:  Produces
;
;
; COMMON BLOCKS:
;
;
; SIDE EFFECTS:
;
;
; RESTRICTIONS:  Assumes that the starting matchpoints are within the
;      image.
;
;
; PROCEDURE:
;
;
; EXAMPLES:
;
; MODIFICATION HISTORY:
;
;
;

if n_elements(diag_un1) eq 0 then diag_un1=-2
;Layout overview:
;     load the two images
;     expand the input image so pixel size matches reference image
;     rotate the input image

;SET UP THE MOVE_VALUE.


   ;this value will keep track of all the movements of the centerpoint.
   ;We do this for both the input and the reference image.  It should be
   ;in units of reference image map units.  (*,0) = reference, (*,1) = input

   movevalue = dblarr(2,2)

 ;cutoff = the minimum proportion of pixels necessary for calc of covariogram

   cutoff= .3


;LOAD THE IMAGES:

  ;(*rs = reference small image)
  ;(*is = input small image)

write_diagnosis, diag_un1, 'squeeter: before rs adjustment'



    rs = sq_load_images(reference, params, diag_un1=diag_un1)
    if rs.valid le 0 then valid = 0
    if rs.valid le 0 then goto, past

    ;the movevalue is set.  We keep it in terms of pixels

    movevalue(*,0) = rs.shift

    ;the area of the extracted image was calculated by
    ;   params.window_size * reference.pixelsize.  In
    ;   case pixelsize is not the same for the input image,
    ;   we need to make sure that the same area on the ground
    ;   extracted from the input image.  This is achieved by
    ;   adjusting the desired window_size that we extract.

      ;first figure out the relationship between pixel sizes

			;march 19 2007 -- added scale on the edgezone, so different-sized
			;  images don't go off the edge.

         blowupby = reference.gifov / input.gifov
         input_windowsize = round(params.window_size * blowupby)
         input_edgezone= round(params.edgezone * blowupby)

         inp_p = params
         inp_p.window_size = input_windowsize
         inp_p.edgezone = input_edgezone

      ;now, if we had to move the reference image around because
      ; it was at the edge of the image, it would show up as a
      ; non-zero rs.offset value.  So we need to offset the
      ; the input center point.
      ;The rotation we use is the negative of the angle -- since
      ;    a move in the referene image will appear to move in the
      ;    opposite direction of the rotation of the input image.
      ;    I.e. an input image rotated 30 degrees clockwise; move to
      ;    the right 10 pixels in the reference image, it would mean
      ;    that apparent new position in the input image is to the right
      ;    and UP.

write_diagnosis, diag_un1, 'squeeter: before ir adjustment'
        ir = calc_rot_vect(-input.rotation)
        e= ir ## ((rs.shift/reference.pixelsize) * blowupby * input.pixelsize)
        inp_t = input
        inp_t.centerpoint = input.centerpoint - e
;print, 'offset of input based on reference move'
;print, e

    ;load the image


write_diagnosis, diag_un1, 'squeeter: before is load images'
    is = sq_load_images(inp_t, inp_p, diag_un1 = diag_un1)


    if is.valid le 0 then valid = is.valid
    if is.valid le 0 then goto, past




        ;the returned information.
        ;is.img -->  image
        ;is.img_info -->  information in format like at the begin of this prog.
        ;is.offset --> the offset of the edge of the desired window from the
        ;   edge of the image -- if 0, then we're entirely w/in image
        ;is.shift -->  that part of the offset that is due simply to shifting
        ;   the pixel to the center of a pixel.  We need to keep
        ;      this separate, since this type of movement in the is.
        ;   is necessary often, but we can't accept other movements
        ;   caused by going outside the edge of the image.
        ;is.valid -->  0 if not valid

       movevalue(*,1) = is.shift + e
           ;the input centerpoint was moved to
              ;match the reference image(e), and also
              ;potentially for its own reasons.  We keep
              ;this in pixels of the input image

;print, 'is.shift is the shift in the input image'
		;at this point, the rs.img_info.centerpoints jive with
		;the actual values in the image in the center of the image




    ;we compare the total offset to the shift offset.  If they're not zero,
    ;that means that the input image had to go outside the bounds in order to
    ;match up with the reference image.  This is not good, so we send a not-
    ;valid sign back.

    write_diagnosis, diag_un1, 'Ref center after loading image:  '+string(rs.img_info.centerpoint)
    write_diagnosis, diag_un1, 'Inp center after loading image:  '+string(is.img_info.centerpoint)
    write_diagnosis, diag_un1, '  Reference offset: '+string( movevalue(*,0))
    write_diagnosis, diag_un1, '  Input offset: '+string( movevalue(*,1))



    ;if is.offset(0)-is.shift(0) ne 0 or is.offset(1)-is.shift(1) ne 0 then begin
    ;             valid=0
    ;             goto, past
    ;             end



;RESAMPLE INPUT IMAGE IF NECESSARY TO MATCH REFERENCE


  if blowupby(0) ne 1 or blowupby(1) ne 1 then is = sq_eqpixsize(rs.img_info, is)


;ROTATE IMAGES RELATIVE TO EACH OTHER

  ;we know that the input image has a rotation associated with it, stored
  ;   in the tag input.rotation (passed from the program that called this
  ;   one).

  ;now call the routines to rotate the input image
;print, is.img[50,50]

ii = is.img
non_interpd = sq_rotate(is, input.rotation)
;print, non_interpd.img[50,50]

  if input.rotation ne 0 then is = sq_rotate(is, input.rotation, /interp)
;print, is.img[50,50]
;stop


;USE COVARIOGRAM TO ALIGN IMAGES

i_dims = dims(is.img)
r_dims = dims(rs.img)



 ;we now work in file pixels for the image alignment (since map
 ;   units are meaningless for the rotated/blownup image).
 ;so we need to get the original center in terms of pixels.
 ;  The coordinates start with -.5, -.5.  Centers of pixels are
 ;      integer values.  These are units of reference pixels
 ;The i_center will change according to the covariogram results.
 ;At the end of the loop that begins below, we convert back to
 ;    ur-coordinate system for the input image and reference image.

   i_center =  (i_dims-1)/2.0
   r_center =  (r_dims-1)/2.0

 ;Call the recursive GCP finding portion

  iteration = 0
  move_value_recurse = dblarr(2,2)


     ;is.img_info.centerpoint, rs.img_info.centerpoint do not
     ;   get changed in the recursion


	;zippa = dialog_message('starting sq_recurse', /info)

   sq_recurse, is, rs, i_center, r_center, params, iteration, $
     move_value_recurse, loop_valid, diag_un1=diag_un1, $
     diag_un2=diag_un2, conf = conf, cutoff=cutoff


;	zippa = dialog_message('ending sq_recurse', /info)

  ;at this point, if valid is zero, we've got to go back to the calling
  ;   program empty-handed.



  if loop_valid eq 0 then begin
         valid = 0
         goto, past
         end

;*********
;Adjust coords
  write_diagnosis, diag_un1, 'sent r_center - returned r_center' +string( ((r_dims-1)/2.0)-r_center)
  write_diagnosis, diag_un1, 'How much the reference center was moved in recursion'+string( move_value_recurse(*,0))
  write_diagnosis, diag_un1, 'sent i_center - returned i_center'+string(((i_dims-1)/2.0)-i_center)
  write_diagnosis, diag_un1, 'How much the input center was moved in recursion'+string( move_value_recurse(*,1))


  ;r_move_map = move_value_recurse(*,0) * [1,-1] * rs.img_info.pixelsize ;in map units
  ;4/24/08 need to consider the reference movement in terms of the rotated direction too, especially
  ;   because the sq_match offset is actually applied to the reference image, and if it's moving
  ;   it's also in input space

  ;r_move_map = calc_rot_vect(-input.rotation) ## [move_value_recurse(*,0) * [1,-1] * rs.img_info.pixelsize] ;in map units
  r_move_map = calc_rot_vect(input.rotation) ## [move_value_recurse(*,0) * [1,-1] * rs.img_info.pixelsize] ;in map units

  new_rcenter = rs.img_info.centerpoint - r_move_map    ;the new coords

  write_diagnosis, diag_un1, 'Move value reference'+string(move_value_recurse(*,0))+string( r_move_map)



  ;same basic idea with input image. However, we need to account for
  ;  the rotation and blowing up.  Rotation is taken care of by
  ;  calc_rot_vect in the equation.  Blowing up was taken care of by
  ;  the sq_eqpixsize subroutine -- is.img_info.pixelsize is blowupby*original
  ;  pixel size.



  ;i_move_map = calc_rot_vect(-input.rotation) ## [move_value_recurse(*,1) * $
  i_move_map = calc_rot_vect(input.rotation) ## [move_value_recurse(*,1) * $
               is.img_info.pixelsize * [1,-1]]


  new_icenter = is.img_info.centerpoint - i_move_map
  total_move_ref = r_move_map + (movevalue(*,0))    ;add the movements that
              ;took place before the recurse call
  total_move_inp = i_move_map + (movevalue(*,1))





  valid = 1

past:
   ;Return the values
   ;the move values are in map units of the respective images.
write_diagnosis, diag_un1, 'squeeter: at the end'

  if valid ne 0 then ret = {ref_gcp:new_rcenter, inp_gcp:new_icenter, $
       ref_move:total_move_ref, inp_move:total_move_inp, $
         valid:valid, conf:conf} else $
       ret = {ref_gcp:[0,0], inp_gcp:[0,0], ref_move:[0,0], $
         inp_move:[0,0], valid:0, conf:0}


return, ret
end





