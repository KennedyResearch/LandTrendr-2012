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

; adsfsdf
; Copyright (c) 1998, Oregon State University.
; This software is currently in beta-test condition.  It may not be altered,
;	copied, or redistributed without express written permission of
;	Robert E. Kennedy, Oregon State University (kennedyr@fsl.orst.edu).
;	This software is supplied as is, with no express or implied
;	warranties.

function fix_edge, proposed, const, map=map, noshrink=noshrink, $
	equalshrink=equalshrink, edgezone=edgezone



; Written Dec., 9 1997
; Both proposed and constraints must be in form:
;     proposed = [ [x,yupl], [x,ylor] ]
;     or, put another way,    prop(*,0) = [x,yupl]
;if map is set, that means the coordinates are in map coordinates
;   otherwise the coordinates are assumed to be in file coordinates
;  Map coordinates have y increasing to the north.  File coordinates
;    have y increading to the south.
;In normal mode, only those values that are outside the constraints are
;	changed.  The area contained within the proposed coordinates
;	will shrink if any adjustments are made.
;	If /noshrink is set, all coordinates are adjusted so
;	the entire area of the proposed coordinates is within the constriants
;Returned value is a structure with:
;		.coords --> the new coordinates
;		.diffs --> proposed + .diffs = .coords
;		.valid --> 1 for okay, 0 for not valid

;if EDGEZONE is set, it corresponds to the distance (in coordinate units)
;   that the proposed values may extend past the constraints, and
;   also the boundary of the values that fix_edge will move offending
;   proposed coordinates back to.

  ;INITIAL STUFF, VARIABLES

   constraints = const	;make copy to play with

   forward_function fix_edge	;need to do this because fix edge is
				;called recursively!

   if n_elements(noshrink) ne 0 and n_elements(equalshrink) ne 0 then message, $
			'Cannot set both /noshrink and /equalshrink'
   if n_elements(map) ne 0 then $
   	mapadj = [ [1,-1], [1,-1], [1,-1], [1,-1]] else $
   	mapadj = bytarr(2,4)+1

   if n_elements(edgezone) ne 0 then $
   	constraints = [ [constraints[*,0] - (mapadj[*,0]*edgezone)], $
   	    	   [constraints[*,1] + (mapadj[*,0]*edgezone)] ]


   valid = 1 ;we assume that we're valid. The only case where we wouldn't
   	     ;is when  proposed values are larger than constraint on both
   	     ;sides, and /noshrink is set.


  ;GET THE DIFFERENCES

    diffs = [ [proposed(*,0)-constraints(*,0)], $
	      [proposed(*,1)-constraints(*,0)], $
	      [constraints(*,1)-proposed(*,0)], $
	      [constraints(*,1)-proposed(*,1)]]

    diffs = diffs*mapadj
    diffs = cut_tiny(diffs, 0.00001)


  ;PROCESS THE DIFFS

    ;any negative values in "diffs" are outside bounds
      outside = diffs lt 0	;1's represent offending points

      ;if none outside, then we're peachy; go home.
         if total(outside) eq 0 then return, {coords:proposed, diffs:intarr(2,2), valid:1}

      ;otherwise, stabilize okay points by setting to zero

         diffs = diffs * outside


    ;adjust  the points
      diffs = diffs*mapadj		;swap back to normal direction if map
      diffs(*,2:3) = diffs(*,2:3)*(-1)	;in all cases, we have to reverse
					;sign of lowerrights to maintain
					;the coords = proposed - diffs

  ;BASED ON USER INPUT, PROCESS THE POINTS

  case 1 of
  (n_elements(noshrink) eq 1): $

      ;NO SHRINK OPTION
      begin
        ;First check that the proposed window isn't outside
        ;   both corners of the constraint window

          check =  (total(outside[0,0] + outside[0,3]) eq 2) + $	;x's
          	   (total(outside[1,0] + outside[1,3]) eq 2)	;y's
          if check ne 0 then return, {coords:proposed, diffs:bytarr(2,2), valid:0}


        ;we can now shrink diffs down to represent the offset
        ;   necessary to bring the proposed into the constraints


          diffs = [ [diffs[*,0]], [diffs[*,3]] ]

          for i = 0, 1 do begin 	;for x and y
            a= where(diffs(i,*) eq 0, many)	;find the good points
            diffs[i, a] = diffs[i,1-a]	;With the noshrink option, we need
	  				;  shift the other corners the same
	  				;  as the shifted corners
          end

        ;Now re-check with new coords
          testcoords = proposed - diffs
	  if n_elements(map) eq 0 then $
		temp = fix_edge(testcoords, constraints) else $
		temp = fix_edge(testcoords, constraints, /map)
	  if total(temp.diffs^2) ne 0 then $
	  	return, {coords:proposed, diffs:bytarr(2,2), valid:0}
	    		;if we had to change any component after all coords
	    		;were moved, then we know that the window is still
	    		;too large.
          coords = testcoords
      end

(n_elements(equalshrink) eq 1): $

      ;EQUAL SHRINK OPTION
         begin 	;we maintain center point and make the window
         	;shrink equally on either side

          ;First check to see if both corners of the proposed
          ;	window aren't outside the image in the same direction
          ;	i.e. the case where the whole proposed image lies
          ;	outside the constraint window.  If that's the
          ;     case, then we just recall fixedge using the /noshrink option


            check =  (outside[0,0] eq outside[0,1] and outside[0,0] ne 0) + $
            	     (outside[1,0] eq outside[1,1] and outside[1,0] ne 0) + $
            	     (outside[0,2] eq outside[0,3] and outside[0,2] ne 0) + $
            	     (outside[1,2] eq outside[1,3] and outside[1,2] ne 0)

            if check ne 0 then begin

              if n_elements(map) eq 0 then $
            	temp = fix_edge(proposed, constraints, /noshrink) else $
            	temp = fix_edge(proposed, constraints, /noshrink, /map)
              return, {coords:temp.coords, diffs:temp.diffs, valid:temp.valid}
            end

          ;If our proposed window overlaps with the constraint window
          ;	somewhere,  so do the shrink

            ;Set diffs to the 2-component version
              diffs = [ [diffs[*,0]], [diffs[*,3]] ]



            ;We take the absolute value of diffs, and
	    ;  for the x and y take the maximum value.
	    ;  By that value on both sides we need to
	    ;  shrink the proposed values

	      adj = dblarr(2,2)	;we use this to adjust
						;diffs to shrink
	      for e = 0,1 do begin	;x,y
		md= max(abs(diffs(e,*)))
		adj(e,*)= [ [-md], [md]] ;upl, lor
       	      end

	     diffs = adj*mapadj	;account for map adjustments.
	     coords = proposed - diffs

	     ;if we've overcorrected
	     if coords [0,0] lt coords[0,1] or coords[1,0] lt coords [1,1] then valid = 0


	end

else:	begin
	 if total(  [   total(outside(0,0:1)) eq 2, $
	 		total(outside(1,0:1)) eq 2, $
	 		total(outside(0,2:3)) eq 2, $
	 		total(outside(1,2:3)) eq 2] ) eq 0 then $
	 begin

	   diffs = [ [diffs[*,0]], [diffs[*,3]] ]
	   coords = proposed - diffs	;when we don't care about shrinking
	 end else begin
	   valid = 0
	   coords = [0,0]
	   diffs= [0,0]
	 end

	end

endcase

past:
return, {coords:coords, diffs:diffs, valid:valid}

end


