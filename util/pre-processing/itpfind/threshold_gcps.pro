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

function threshold_gcps, gcps, proportional_rmse, degree, tracker=tracker

;all the way just sets this
s = size(gcps)
if s(0) ne 2 then return,0  else if s(1) ne 4 then return, 0

;gcps should be (4,N) array.  	(0,*)= ref x
;			      	(1,*)= ref y
;				(2,*)= inp x
;				(3,*)= inp y

;iteratively throw out bad points
tracker = fltarr(1)
count = 0

doit:
s=dims(gcps)
resids = dblarr(3, s(1))

polywarp, gcps(0,*), gcps(1,*), gcps(2,*), gcps(3,*), $
	degree, rkx, rky

a=project_it( [ [transpose(gcps(2,*))],[transpose(gcps(3,*))] ], rkx, rky)
rev_reproj = [ transpose(a(*,0)), transpose(a(*,1)) ]

resids(0:1,*) = gcps(0:1,*)-rev_reproj
resids(2,*) = sqrt( resids(0,*)^2 + resids(1,*)^2)	;RMS (distance)
rmsx = sqrt(total(resids(0,*)^2)/s(1))
rmsy = sqrt(total(resids(1,*)^2)/s(1))
rmse = sqrt(rmsx^2 + rmsy^2)
tracker[count] = rmse

;get the proportional rms improvement by removing this one.


;if the first one, then just put in a dummy value to force going on

if count eq 0 then propdiff = proportional_rmse+1. else $
  propdiff = abs(tracker[count-1]-rmse)/tracker[count-1]

;if the removal of this last point caused a proportional
;  improvement less than the threshold, then no need
;  to remove it, so get outta here


if propdiff lt proportional_rmse then goto, out

;otherwise, take out the worst offending point and
;  retry

	a=where(resids(2,*) ne max(resids(2,*)), many)
	if many le (degree+1)^2 then goto, out		;just ran out of points, return with min.

	gcps = gcps(*,a)
  count=count+1
  expand_cols, tracker, 1, newdims
	goto, doit

out:
return, {gcps:gcps, kx:rkx, ky:rky, rmse:rmse}

end





