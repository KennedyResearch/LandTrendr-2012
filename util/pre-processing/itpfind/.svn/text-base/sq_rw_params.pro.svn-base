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
; Copyright (c) 2002, Oregon State University.
; This software is currently in beta-test condition.  It may not be altered,
;	copied, or redistributed without express written permission of
;	Robert E. Kennedy, Oregon State University (kennedyr@fsl.orst.edu).
;	This software is supplied as is, with no express or implied
;	warranties.

;sq_rw_params.pro
;  Last updated Nov 2002: took out reading of "stdvtest", which is not used
;      in version 2.0 (replaced by use of confidence score for peak * 3.0)


;-----------------------------------------------------------------------------
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function gcp_pick_readdef, unit, checkstr, iterations

query_file, unit, checkstr, a
a=str_sep(a, ',')
if n_elements(a) ne iterations then begin
	print, 'Variable '+checkstr+' does not match iteration count'
	return, -99
	end
r= double(a)
return, r
end


;-----------------------------------------------------------------------------
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro sq_rw_params, filename, params, write=write, unit=unit




;if we have already defined a unit on the outside, then
;	set unit=that unit, otherwise the program goes by the filename

if n_elements(write) eq 0 then begin	;if we're reading 'em

  if n_elements(unit) eq 0 then openr, un, filename, /get_lun else un=unit


  query_file, un, 'Window Size', a
  a=str_sep(a, ',')
  window_size = [ fix(a(0)), fix(a(1))]
  query_file, un, 'Window Spacing', a
  a=str_sep(a, ',')
  window_spacing = [ fix(a(0)), fix(a(1))]


  query_file, un, 'Number of Iterations', a
  iterations = fix(a)

  query_file, un, 'Maximum move', a
  maxmove= fix(a)

  query_file, un, 'Nudgefactor', a
  nudgefactor = float(a)

  query_file, un, 'Gifov test', a
  if a eq 'no_match' then gifov_test = 1 else gifov_test = fix(a)

  query_file, un, 'Postfilter_rms', a
   if a eq 'no_match' then postfilterrms = 0 else postfilterrms = float(a)



  pixel_aggs = float(gcp_pick_readdef(un, 'Pixel Aggregations', iterations))
  nbhds = float(gcp_pick_readdef(un, 'Search Neighborhoods', iterations))
   minsteepness = float(gcp_pick_readdef(un, 'Threshold min. steepness', iterations))
  threshold = {minsteepness:minsteepness}


  zoom = float(gcp_pick_readdef(un, 'Zoom Factors', iterations))


  params = {window_size:window_size, window_spacing:window_spacing, $
	iterations:iterations, pixel_aggs:pixel_aggs, nbhds:nbhds, $
	threshold:threshold, zoom:zoom, nudgefactor:nudgefactor, $
	maxmove:maxmove, edgezone:round(window_size*0.5), $	;edgezone=window_spacing*.75
	gifov_test:gifov_test, $    ;added 9/12/2004 to allow for turning off gifov test for
														;rubbersheeting type exercises with many many itps
  postfilterrms:postfilterrms}  		;7/21/05 postfilterlevel is the rms that will be used if postfiltering is set



end else begin		;if we're writing the parameters
   if n_elements(unit) eq 0 then openw, un, filename, /get_lun else un=unit
   printf, un, format = '("Window Size: ", 2(d20.10,:, ","))', params.window_size
   printf, un, format = '("Window Spacing: ", 2(d20.10, :, ","))', params.window_spacing
   printf, un, format = '("Number of Iterations: ", i3)', params.iterations
   printf, un, format = '("Maximum move: ", i3)', params.maxmove
   printf, un, format = '("Nudgefactor: ", f8.4)', params.nudgefactor


  ;for all the multiple parameters (have as many values as iterations)

   its = strcompress(string(params.iterations), /remove_all)
   basic = ','+its+'(f8.4, :, ","))'


   printf, un, format = '("Pixel Aggregations: "'+basic, params.pixel_aggs
   printf, un, format = '("Search Neighborhoods: "'+basic, params.nbhds
   printf, un, format = '("Threshold min. steepness: "'+basic, params.threshold.minsteepness
   printf, un, format = '("Zoom Factors: "'+basic, params.zoom


end


if n_elements(unit) eq 0 then free_lun,  un
return
end

;April 23rd, 1999 added the stdvtest read write
;Nov 18, 2002 took it out again -- supplanted by use of confidence score for
;   peaks.

