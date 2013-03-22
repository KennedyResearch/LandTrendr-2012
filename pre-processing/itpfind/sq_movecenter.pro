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




pro sq_movecenter, dirvect, refinfo, inpinfo, params, movevalue=movevalue, $
		nudge=nudge, maxmove = maxmove

;	zippa = dialog_message('starting sq_movecenter', /info)

;if nudge is set, then that means we've not found a good point and
;   need to nudge the window over by some factor of window size.  If nudge
;   is not set, then we're moving a window by window-spacing.
;movevalue is the [2,2] array that contains the amount by which the point
;   was moved, including the adjustment to center the window

;dirvect has following tags:
;    .vect  -- [x,y] vector of direction. Changes when we get to a corner
;    .pos   -- [x,y] position relative to starting point, in units of the
;			move-distance.  This value is multiplied by the
;			move-distance to find the new coordinates. Always
;			starts upper right (1,1) and moves clockwise.
;    .side  -- which side we're on.  0 = right, 1 = bottom, 2= left, 3=top
;    .distance -- scalar.  Which layer out from center we're at.
;    .valid -- scalar.  1 if we found a new point within the bounds,
;		goes to zero if we've searched everywhere and are outside
;		the bounds of the images on all sides.
;    .wall -- four-element array, all set to zero initially.  Whenever we
;		hit the edge of one of the images in a certain direction,
;		then that side's .wall value goes to 1.  When all are at
;		1, we know there are no more points to be found.


;Figure out how much we need to move each image, based on the ratios of
;    pixel sizes.  params.window_size and params.window_spacing
;    are in units of pixels of the reference image.  We need to adjust that
;    to map units for the reference image, and adjust to map units of the
;    the input image.  Also, for the input image, we need to use the
;    rotation based on the inpinfo.rotation and use different pixel count
;    based on the ratio of gifovs

   if n_elements(nudge) eq 0 then adjuster = params.window_spacing else $
   		adjuster = params.window_size*params.nudgefactor


 ;Since squeeter will use the ratio of gifov's to extract the images,
 ;   we need to do the same to test the window.  Thus we calc the ratio
 ;   here and use it when we're figuring out how large the window will need
 ;   to be

   ratio =  [refinfo.gifov/inpinfo.gifov]
   rotvect = calc_rot_vect(-inpinfo.rotation)	;changed to negative, Jun10 99


 ;calculate the adjustment value in the units of each image

   ref_adjuster = adjuster*refinfo.pixelsize
   inp_adjuster = adjuster*inpinfo.pixelsize*ratio

 ;set up the directional vector for each side of the box

   vect = [ [0,-1], [-1,0], [0,1], [1,0] ]

 ;set up the dummy structure to pass to the test algorithms

   ref_t = refinfo
   inp_t = inpinfo

adjust:

;MAKE TRIAL CENTERPOINTS AND TEST THEM


   test_ref_centerpoint = refinfo.centerpoint + (ref_adjuster*dirvect.vect)
   test_inp_centerpoint = inpinfo.centerpoint + rotvect#(inp_adjuster*dirvect.vect)


  ;Use define_window to get the corners of the window and see if
  ;    we'd need to move it to make it fit into the constraints of
  ;    the image.

     ref_t.centerpoint = test_ref_centerpoint
     ref_win = define_window(ref_t, params)
     p=where(ref_win.offset(0,*) ne 0, many)
     if many eq 0 then p =0 else if many eq 1 then p=ref_win.offset(0,p) else $
     	if many eq 2 then message, 'Window larger than image'
     q=where(ref_win.offset(1,*) ne 0, many)
     if many eq 0 then q =0 else if many eq 1 then q=ref_win.offset(1,q) else $
     	if many eq 2 then message, 'Window larger than image'
     ref_offset = [p,q]


     ;adjust the window size of the input image and define the window
       input_windowsize = ceil(params.window_size * ratio)
       inp_p = params
       inp_p.window_size = input_windowsize
     inp_t.centerpoint = test_inp_centerpoint
     inp_win = define_window(inp_t, inp_p)
      p=where(inp_win.offset(0,*) ne 0, many)
     if many eq 0 then p =0 else if many eq 1 then p=inp_win.offset(0,p) else $
     	if many eq 2 then message, 'Window larger than image'
     q=where(inp_win.offset(1,*) ne 0, many)
     if many eq 0 then q =0 else if many eq 1 then q=inp_win.offset(1,q) else $
     	if many eq 2 then message, 'Window larger than image'
     inp_offset = [p,q]



       ;print, 'ref_win.offset', ref_win.offset
       ;print, 'inp_win.offset', inp_win.offset


 ;Check what our offsets are by calling sq_checkoversteps
 ;If we had to adjust the window, we need to find out which side(s)
 ;we're too close to. Run sq_checkoversteps.  Badpoint_inp will
 ;be zero if no edges exceeded, or 1 if any exceeded. Dirvect.wall
 ;for the appropriate edge will be set to one if we've breached the edge
 ;Note that the offset = offset due to placement near edge + shift due to
 ;				placement in non-center of window.
 ;So to find out the offset due to placement near edge, subtract and send to
 ;   sq_checkoversteps



    sq_checkoversteps, inp_offset, $
    		params.edgezone*inpinfo.pixelsize, $
    		inpinfo.pixelsize, dirvect, badpoint_inp

    sq_checkoversteps, ref_offset, $
    		params.edgezone*refinfo.pixelsize, $
    		refinfo.pixelsize, dirvect, badpoint_ref



   if ((total(badpoint_ref ne 0)) or (total(badpoint_inp ne 0))) and (n_elements(nudge) eq 0)$
     then begin
   	;print, "####"
   	;print, '    '
   	;print, 'Reference upl, lor', refinfo.upl, refinfo.lor
   	;print, 'Corners of test area', ref_win.corners

   	;print, 'Input upl, lor', inpinfo.upl, inpinfo.lor
        ;print, 'Corners of test area', inp_win.corners

     end
;stop
;IF WE WENT OUTSIDE THE IMAGE, WE NEED TO MARK THE FLAGS HERE BEFORE
; 	WE POTENTIALLY UPDATE THE SIDE

   dirvect.wall= dirvect.wall or [badpoint_inp,badpoint_ref]


;ASSIGN MASTER CENTERPOINTS
   ;but first assign the movevalue if caller requested it
   if n_elements(movevalue) ne 0 then $
   	movevalue=movevalue+ [ [refinfo.centerpoint - test_ref_centerpoint], $
   			[inpinfo.centerpoint - test_inp_centerpoint] ]




  refinfo.centerpoint = test_ref_centerpoint
  inpinfo.centerpoint = test_inp_centerpoint

;SET UP STUFF FOR NEXT TIME THIS PROGRAM IS CALLED


  ;Update the current position, now that we know it works

    dirvect.pos = dirvect.pos + dirvect.vect

  ;And then we update the side we're on

    pos = dirvect.pos eq [dirvect.distance, dirvect.distance]
    neg = dirvect.pos eq (-1)*[dirvect.distance, dirvect.distance]
    sides = [ [2*pos(0)+pos(1)], [pos(0)+2*neg(1)],$
    		 [2*neg(0)+neg(1)], [2*pos(1)+neg(0)] ]

    oldside = dirvect.side
    dirvect.side = (where(sides eq max(sides)))[0]

  ;if we're now on side 0 and the last side was 3, then we need to increment
  ;    the distance

    addside = ((dirvect.side eq 0) and (oldside eq 3))
    dirvect.distance = dirvect.distance + addside

     ;Check to see if we've passed maxmove

     ;print, 'In sq_movecenter, dirvect.valid', dirvect.valid
     if n_elements(maxmove) ne 0 then begin
     		dirvect.valid = dirvect.distance le maxmove

     		if dirvect.valid eq 0 then goto, past
     		end




  ;now we update the directional vector by assigning to its current side --
  ;  except in the case we're in the upper right corner, in which case we
  ;  need to make the directional vector shoot us out to the next shell, so
  ;  assign the direction of side 3 once more.

    if addside eq 0 then dirvect.vect = vect(*,dirvect.side) else $
    		dirvect.vect = vect(*,3)

;EITHER RE-RUN THIS PROGRAM OR RETURN TO CALLER

;Now, if we've gone outside the realm of either of the images, we need to
;   back to this program and try again.  We'll just keep moving until
;   at some point we get back into the image.

  if total(badpoint_inp) ne 0 or total(badpoint_ref) ne 0 then begin
  	dirvect.valid = (total(dirvect.wall(*)) ne 8)
        if dirvect.valid eq 0 then goto, past
        goto, adjust
  end

past:
;stop

	;zippa = dialog_message('ending sq_movecenter', /info)

return
end



