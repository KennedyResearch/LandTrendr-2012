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

pro sq_reset_bigdirvect, bigdirvect
   bigdirvect = {	hpos:0, $
   			vpos:0, $
   			hanchor:[[0.d, 0.d],[0.d, 0.d] ], $
   			vanchor:[[0.d, 0.d],[0.d, 0.d] ], $
   			done:0}
return
end


pro sq_bigmove, dirvect, refinfo, inpinfo, params, movevalue = movevalue

;Procedure to move the main search window in squeetstart.pro
;

  ;set variables
    movevector = [0,0]	;determines the next point
    adjuster = params.window_spacing


    ;Since squeeter will use the ratio of gifov's to extract the images, 
    ;   we need to do the same to test the window.  Thus we calc the ratio
    ;   here and use it when we're figuring out how large the window will need
    ;   to be
 
      ratio =  [refinfo.gifov/inpinfo.gifov]		
      ;rotvect = calc_rot_vect(inpinfo.rotation)
      ;backrotvect = calc_rot_vect((-1)*inpinfo.rotation)
      rotvect = calc_rot_vect((-1)*inpinfo.rotation)
      backrotvect = calc_rot_vect(inpinfo.rotation)
 
    ;calculate the adjustment value in the units of each image
 
      ref_adjuster = adjuster*refinfo.pixelsize
      inp_adjuster = adjuster*inpinfo.pixelsize*ratio
      inp_params = params
      inp_params.window_size = round(inp_params.window_size*ratio)



  ;Check dirvect for .hpos = 0 -- the condition where the 
  ;    directly after searching for FIRST point in this run.
  
    if dirvect.hpos eq 0 then begin
        dirvect.hanchor = (dirvect.vanchor = $			;set the anchor
          [ [refinfo.centerpoint], $		;points to the current position
	    [inpinfo.centerpoint] ])
        dirvect.hpos = (dirvect.vpos=1)		;initialize for movement now
    
    end
    reloop = 0
    
 loop:	;we'll jump back here if we're way outside the input image
    
    
   ;If we've jumped internally, reset
    
    if reloop eq 1 then print, 'sq_bigmove: Relooping' 
    reloop = 0
    
    
  ;Check the state of .hpos and make appropriate settings
    
    vposvectors = [ [0,1], [0,-1] ]	;*,0 for vpos = 1, *,1 for vpos =3
  
    case 1 of 
    (dirvect.hpos eq 1): begin	;keep moving right
     	   movevector = [1,0]
     	 end
    (dirvect.hpos eq 3): begin	;keep moving left
           movevector = [-1,0]
         end
    ((dirvect.hpos eq 2) or (dirvect.hpos eq 4)): $
         begin
           case dirvect.vpos of
           (1):  begin
           	 movevector = [0,1]
           	 
           	 end
           (2):  begin	;we've gotten to top and need to jump back
           		;to the startpoint and go the other direction
           		
           	   dirvect.hanchor = dirvect.vanchor
           	   dirvect.vpos = 3
           	   dirvect.hpos = 3
           	   movevector = [-1,0]
           	   print, 'sq_bigmove: Jumping to middle'
           	   
           	   
           	 end
           (3):  begin
           	 movevector = [0,-1]
		 end
           (4):  begin	;we're done!
             dirvect.done = 1
             return
             end
           else:
           endcase  
           
          
        end
    else:
    endcase
    
  print, 'sq_bigmove: XXXX'
  print, 'sq_bigmove:     MOVEVECTOR IS '+string(movevector)
  print, 'sq_bigmove:       '
  print, 'sq_bigmove:       '
  
   ;MAKE A TEST MOVE
                 
     new_ref_centerpoint = dirvect.hanchor[*,0] + (ref_adjuster*movevector)
     new_inp_centerpoint = dirvect.hanchor[*,1] + rotvect#(inp_adjuster*movevector)
   
         ;set the anchor to this, the next predicted point on the grid,
     ;   regardless whether we need to adjust it 

        dirvect.hanchor = [ [new_ref_centerpoint], $	
             			 [new_inp_centerpoint] ]
             


;TEST AGAINST EDGES

   test_ref_test = def_window(new_ref_centerpoint, refinfo, params)
   test_inp_test = def_window(new_inp_centerpoint, inpinfo, inp_params)
   
   
   if test_ref_test.valid eq 0 or test_inp_test.valid eq 0 then begin
   	print, 'sq_bigmove: The proposed window was larger than the constraint window.'
  	print, 'sq_bigmove: Returning.'
  	return
  	end
     
       ;NOTE:  The output from def_window will be some non-integer
       ;	multiple of the movevector if we're near an edge.  
       ;	If we're near an edge in both images, we need to 
       ;	figure out how to be happy in both. Therefore:

;IF MOVES HAVE BEEN MADE, IS NORMALIZED MOVE OKAY?	
     
   if total ([test_ref_test.offset[*,0] ne 0, $       ;any moves made?
   	      test_inp_test.offset[*,0] ne 0]) ne 0 $ 
   	then begin	
        
          print, 'sq_bigmove: ttttttttt'
          print, 'sq_bigmove:       some movement'
          print, 'sq_bigmove:       test_ref_test.offset ', test_ref_test.offset[*,0]
          print, 'sq_bigmove:       test_inp_test.offset ', test_inp_test.offset[*,0]
          
          
        
          ;FIRST CHECK TO SEE IF NORMALIZED MOVES OKAY
        
          ;Normalize the input movement to the reference space
          
            total_inp_offset = test_inp_test.offset[*,0]  ;+ test_inp_test.shift
            total_ref_offset = test_ref_test.offset[*,0]  ;+ test_ref_test.shift
            
            
            ;Convert input to GIFOVs, then multiply by refinfo.pixelsize
             
            norm_inp_offset = $
            	(backrotvect#(total_inp_offset/(ratio*inpinfo.pixelsize)))$
            		*refinfo.pixelsize
                        
          
          ;What's the largest offset in x and y?
          ;Make an 2 element array with the greatest offset of the ref and inp for x,y
             maxvals = $
             [ ([total_ref_offset[0],norm_inp_offset[0]]) $
             		[(abs(total_ref_offset[0]) lt $
              			abs(norm_inp_offset[0]))], $
              ([total_ref_offset[1],norm_inp_offset[1]]) $
              		[(abs(total_ref_offset[1]) lt $
             			abs(norm_inp_offset[1]))] ]
             			
          ;Is this horizontal or vertical move?  Note that
          ;	we only keep track of the reference movement in the 
          ;     y-direction -- searching only for the situation where
          ;     we've gone to the bottom edge of the reference image.
          
            ;We don't really care about small adjustments, less than 
            ;  refinfo.pixelsize or the equivalent size of input
            ;  pixel in ref space
            
              inpinref = refinfo.pixelsize / ratio
              small = [ $
              		([refinfo.pixelsize[0], inpinref[0]]) $
              			[refinfo.pixelsize[0] lt inpinref[0]], $
              		([refinfo.pixelsize[1], inpinref[1]]) $
              			[refinfo.pixelsize[1] lt inpinref[1]] $
              	      ]
                        
              testmaxvals = [$
              			([maxvals[0], 0]) $
              			[abs(maxvals[0]) lt small[0]], $
              			([total_ref_offset[1], 0]) $
              			[abs(total_ref_offset[1]) lt small[1]] $
              		    ]
              		    
            ;now determine which direction we had to move
                     
             movetype = signof(testmaxvals)
             inpmaxvals = rotvect# $
             	((maxvals/refinfo.pixelsize)*ratio*inpinfo.pixelsize)	
             		;April 5, 2001 added multiply by inpinfo.pixelsize
             		
             	
         
         
             print, 'sq_bigmove: ******'
             print, 'sq_bigmove: Maxvals '
             print, maxvals
             print, 'sq_bigmove:       '
             print, 'sq_bigmove:      '
         
          ;if we're way outside the image, set up the flag here to reloop
          
            if total(abs(maxvals) gt $
            	.75*(params.window_spacing*refinfo.pixelsize)) ne 0 $
            	then reloop = 1
          	
          
          ;make new centerpoint adjustments
          
          
          
               ;With maxval, make a second set of matching centerpoints
               ;	and test it again with def_window
          
                 print, 'sq_bigmove: Hit edge in sq_bigmove'
                 print, 'sq_bigmove: Original edge that I tried:'
                 print, new_ref_centerpoint
                 print, new_inp_centerpoint
                 
                  test_ref_cp2 = new_ref_centerpoint - maxvals
                    
                  test_inp_cp2 = new_inp_centerpoint - inpmaxvals
             
                  test_ref_test2 = def_window(test_ref_cp2, refinfo, params)
                  test_inp_test2 = def_window(test_inp_cp2, inpinfo, inp_params)
             
         
                ;At this point, if there's any movement, we bail  
                  total_inp_offset2 = abs((test_inp_test2.offset[*,0]+$
                  	test_inp_test2.shift)/inpinfo.pixelsize)
                  total_ref_offset2 = abs((test_ref_test2.offset[*,0]+$
                  	test_ref_test2.shift)/refinfo.pixelsize)
                  	
                  	;make sure this isn't greater than half the neighborhood value,
                  	;  which is the max adj_int_mult should go.
                  th = params.nbhds[0]/2.	;use nbhds[0] because
                  				;this program only 
                  				;affects the first iteration
                  				;of the covar program
                  				
                  
                  if total ([total_inp_offset2 gt th, $
             		total_ref_offset2 gt th]) ne 0 $
             		then begin
             		reloop = 1
             		;print, 'sq_bigmove: ERROR. Decide what to do"
             		
             		goto, zzz
             		end	;decide what to do here.
                   
             
                ;ASSIGN ADJUSTED VALUES TO NEW_XXX_CENTERPOINT
                ;	(because these are assigned later to master centerpoints)
          
                   new_ref_centerpoint = test_ref_cp2
                   new_inp_centerpoint = test_inp_cp2
               
          
          
          
             zzz:
             
             
             
          ;ADJUST THE HPOS, VPOS VALUES for next time through
             case 1 of 
             
             		  ;only reset if the move type was horizontal,
             		  ;otherwise we're likely skirting along a top 
             		  ;or bottom and shouldn't change direction
             		  
             (dirvect.hpos eq 1): begin
             		
             		print, 'sq_bigmove: movetype ', movetype
             		print, 'sq_bigmove: ref ', test_ref_test.offset
             		print, 'sq_bigmove: inp ', test_inp_test.offset
             		print, 'sq_bigmove: maxvals', maxvals
             		print, 'sq_bigmove: inpmaxvals', inpmaxvals
             		print, 'sq_bigmove: hpos ', dirvect.hpos
             		print, 'sq_bigmove: vpos ', dirvect.vpos
             		
             		if movetype[0] eq 1 then dirvect.hpos = 2
             		end
             			
             (dirvect.hpos eq 3): begin
             		
             		print, 'sq_bigmove: movetype ', movetype
             		print, 'sq_bigmove: ref ', test_ref_test.offset
             		print, 'sq_bigmove: inp ', test_inp_test.offset
             		print, 'sq_bigmove: maxvals', maxvals
             		print, 'sq_bigmove: inpmaxvals', inpmaxvals
             		print, 'sq_bigmove: hpos ', dirvect.hpos
             		print, 'sq_bigmove: vpos ', dirvect.vpos
             		
             		if movetype[0] eq -1 then dirvect.hpos = 4
             		end
             			 		
             (dirvect.hpos eq 2):   $
             	begin
             	 print, 'sq_bigmove: Here'
             	 print, 'sq_bigmove: movetype ', movetype
             		print, 'sq_bigmove: ref ', test_ref_test.offset
             		print, 'sq_bigmove: inp ', test_inp_test.offset
             		print, 'sq_bigmove: maxvals', maxvals
             		print, 'sq_bigmove: inpmaxvals', inpmaxvals
             		print, 'sq_bigmove: hpos ', dirvect.hpos
             		print, 'sq_bigmove: vpos ', dirvect.vpos
             		
             	             	 
             	 dirvect.hpos = 3
             	 
             	 
             	 if movetype[1] eq 1 then begin	;if reference vert. move.
             	    
             	    case dirvect.vpos of 	;we're not expecting dirvect.vpos 
             	    				;to be 2 or 4, as these should
             	    				;have been caught at the beginning
             	    				;of the program
             	    (1): begin
             	    	dirvect.vpos = 2
             	    	
             	    	end
             	    	
             	    (3): begin
             	        dirvect.vpos = 4
             	        stop
             	        end
             	        
             	    else:	message, 'Wrong thinking!'
             	    endcase
             	 end
             	
                end
            (dirvect.hpos eq 4):  $
                begin
             	 print, 'sq_bigmove: Here'
             	 print, 'sq_bigmove: movetype ', movetype
             		print, 'sq_bigmove: ref ', test_ref_test.offset
             		print, 'sq_bigmove: inp ', test_inp_test.offset
             		print, 'sq_bigmove: maxvals', maxvals
             		print, 'sq_bigmove: inpmaxvals', inpmaxvals
             		print, 'sq_bigmove: hpos ', dirvect.hpos
             		print, 'sq_bigmove: vpos ', dirvect.vpos
             		
             	 
             	 
             	 dirvect.hpos = 1
             	 
             	 
             	 if movetype[1] eq -1 then begin	
             	 				;if reference vert. move.
             	    
             	    case dirvect.vpos of 	;we're not expecting dirvect.vpos 
             	    				;to be 2 or 4, as these should
             	    				;have been caught at the beginning
             	    				;of the program
             	    (1): dirvect.vpos = 2
             	    (3): begin
             	    	
             	    	dirvect.vpos = 4
             	        ;stop
             	        end
             	        
             	    else:	message, 'Wrong thinking!'
             	    endcase
             	 
             
             	 end
             	
                end
                
             else:	message, 'Wrong thinking, 2 !'
             endcase
                         	        	
        
       
          
          
   end  else begin	;if there were no adjustments necessary
   	if dirvect.hpos eq 2 then dirvect.hpos = 3
   	if dirvect.hpos eq 4 then dirvect.hpos = 1    
     end
     
    
    
        
  ;SET OUR NEW CENTERPOINT VALUES
          
             
    
   ;If user wants to keep track of the movevalue, then figure it out here.
     
     if n_elements(movevalue) ne 0 then $
   	movevalue=movevalue+ [ [refinfo.centerpoint - new_ref_centerpoint], $
   			[inpinfo.centerpoint - new_inp_centerpoint] ]
   	
   ;assign new centerpoint 
             
        refinfo.centerpoint = new_ref_centerpoint
        inpinfo.centerpoint = new_inp_centerpoint
    
        
   ;If we know that we're way off, go back to the loop 
      if reloop eq 1 then goto, loop 
return
end
 
   
    	   	 
     	   		
    	
    