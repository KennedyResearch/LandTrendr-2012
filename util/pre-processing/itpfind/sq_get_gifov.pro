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

;Updates
;  July 21, 2005.  Changing the gifov test so that
;     only a max of 25 points are used for the
;     estimate of the gifov, to prevent slow-downs
;    when many many ITPs are sought.


function  sq_remove_outliers, gcp, refinfo, inpinfo, backrotvect, $
	mindist = mindist, mincount=mincount, outliers=outliers, $
	stop=stop, numdevs = numdevs


;Given the gcps in gcp [4,n_gcps], figure out what the estimate
;   of the input gifov would be

;Set up valid as yes, then adjust if necessary

  valid = [1,1]

;mindist is the minimum distance between two points needed before
;    calculating the GIFOV estimate.  If two points are closer than
;    this distance, they are not used in the GIFOV estimate
;    this is in units of reference pixels.  The first is for
;    x gifovs and the second is for the y gifovs

  if n_elements(mindist) eq 0 then mindist = [50,50]


;mincount is the minimum number of points needed to estimate gifov
;    First the candidate points are determined by meeting the
;    mindistance criterion.  If there are enough of those points, as
;    determined by mincount, then the input gifov is estimated.

  if n_elements(mincount) eq 0 then mincount = [25,25]	;changed 7/21/05

;numdevs is the number of standard deviations used to define outliers

  if n_elements(numdevs) eq 0 then numdevs = 2.0

;If outliers is set, then we return the two outlier arrays


  ;build grids of points

     sg = size(gcp, /dim)

     ;check to make sure we've got enought to make statistics

       if n_elements(sg) eq 1 then return, {valid:[-2,-2]}
       if sg[1] lt 4 then return, {valid:[-2,-2]}

     gridsize = sg[1]^2	;all grids will have this many elements

     rgridx = fill_arr(gcp[0,*], sg[1])	;make a square grid
     rgridy = fill_arr(gcp[1,*], sg[1])	;  of existing gcps
     igridx = fill_arr(gcp[2,*], sg[1])
     igridy = fill_arr(gcp[3,*], sg[1])


          ;calculate the distance from each point to all other points,
      	  ;   in units of pixels

      	     rdiffx= (rgridx-transpose(rgridx))/refinfo.pixelsize[0]
      	     rdiffy= (rgridy-transpose(rgridy))/refinfo.pixelsize[1]
      	     idiffx= (igridx-transpose(igridx))/inpinfo.pixelsize[0]
	     idiffy= (igridy-transpose(igridy))/inpinfo.pixelsize[1]

            ;adjust the input differences for rotation

      	       rotdiff = backrotvect#[reform(idiffx,1,sg[1]^2), $
      	       			      reform(idiffy,1,sg[1]^2)]

      	       idiffx = reform(rotdiff[0,*], sg[1], sg[1])
      	       idiffy = reform(rotdiff[1,*], sg[1], sg[1])

      	    ;now make differences into absolute value to
      	    ;   check against minimum distance thresholds

      	       rdiffx=abs(rdiffx)
      	       rdiffy=abs(rdiffy)
      	       idiffx=abs(idiffx)
      	       idiffy=abs(idiffy)



      	    ;Now take out those points that are too close to each other
      	    ;   in either the x or y.  We do this by testing against
      	    ;   the minimum distance.  If they're close, we give idiff
      	    ;   a zero value, which will cause the x or y est to be
      	    ;   Infinity, which will be culled out in the
      	    ;   calls to finite lower down 20 lines or so.

      		goodx = (rdiffx gt mindist[0]) and (idiffx gt mindist[0])
      		goody = (rdiffy gt mindist[1]) and (idiffy gt mindist[1])

      		idiffx=idiffx * goodx
      		idiffy=idiffy * goody


             ;get estimate of the ratio between input and reference gifovs
      	     ;   refpix moved     inpgifov
      	     ;   ------------  =  --------
      	     ;   inppix moved     refgifov
      	     ;
      	     ;Since *diff* are estimates of pixmoved, then
      	     ;  x,yest * refgifov = estimate of inpgifov

               xest = rdiffx/idiffx
      	       yest = rdiffy/idiffy


      	     ;See if any of the points has abnormal values
      	     ;  First, get the means across x dimension of
      	     ;  xest and yest arrays (by using ,2 in total call)
      	     ;  i.e. xmeans[0] = total(xest[0,*])/sg[1]


      	       ;only use the finite values
      	       finitex = finite(xest)
      	       finitey = finite(yest)


      	       ;If there aren't enough non-finite values
      	       ;   flag!

      	       whinfinitex = where(finitex eq 0, many)
      	         if gridsize-many lt mincount[0] then valid[0] = 0

      	       whinfinitey = where(finitey eq 0, many)
      	         if gridsize-many lt mincount[1] then valid[1] = 0


      	       ;count how many good points there are in each column

      	        totalsx = total(finitex, 2)	;get total of finite
      	        totalsy = total(finitey, 2)	;vals for each column


      	          ;reset the bad points to zero -- they
      	          ; won't be in mean or in total

      	       if whinfinitex[0] ne -1 then $
      	       		xest(whinfinitex)=0
      	       if whinfinitey[0] ne -1 then $
      	       		yest(whinfinitey)=0




      	      ;get means across x dimension of xest array
      	       xmeans = total(xest,2) / totalsx	;get the means
      	       ymeans = total(yest,2) / totalsy



      	       ;FIND OUTLIERS
      	       ;find the outliers within those means
      	       ;find_outliers routine  looks
      	       ;  for points greater than 2.0 standard deviations from
      	       ;  the mean value

      	         xoutliers = find_outliers(xmeans, numdevs = numdevs, minabs=.01)

      	         youtliers = find_outliers(ymeans, numdevs = numdevs, minabs=.01)

                if youtliers[n_elements(youtliers)-1] eq 1 then print, ymeans

      	       ;Assign the outliers keyword to be passed back if
      	       ;  user set it.

      	         outliers = [ [xoutliers], [youtliers] ]


      	   ;GO THROUGH THE POSSIBLE OUTCOMES OF OUTLIERS

      	     case 1 of

      	        ;We met the end of the iteration and never had a case
      	        ;   with non-outliers

      	     (total([xoutliers, youtliers]) eq -2 or $
      	        total([xoutliers, youtliers]) eq -4):	ret = {valid:[-2,-2]}


      	        ;We got to the end and there aren't outliers here.  Go back
      	        ;   up

      	     (total([xoutliers, youtliers]) eq 0): 	$
      	     		ret = {gcp:gcp, xest:moment(xmeans), $
                 		yest:moment(ymeans), valid:valid}
      	       ;new approach -- allow multiples, and subset


             (total([xoutliers, youtliers]) gt 0): $
                 begin
                  goodpoints = where(xoutliers ne 1 and youtliers ne 1, many)

                  ;if we have enough points to get a mean, take it
                  if many gt 3 then begin
                     newgcp = gcp[*,goodpoints]

                     ret = {gcp:newgcp, xest:moment(xmeans[goodpoints]), $
                       yest:moment(ymeans[goodpoints]), valid:valid}

                  end else ret = {valid:[-2,-2]}
                end

              else:  message, 'sq_get_gifov:   unknown test condition'
                endcase



past:
if n_elements(stop) ne 0 then stop


return, ret


end



function sq_get_gifov, gcp, refinfo, inpinfo, backrotvect, $
		thisgcp=thisgcp, mincount=mincount, mindist=mindist, $
		numdevs=numdevs, confthisgcp=confthisgcp


;Given a gcp array of [4,numpoints], get an estimate of the input
;   gifov.  First, the outlier points are thrown out and then the
;   the input gifov is calculated from the remaining points
;   If thisgcp is set, then we compare it to the other points


   if n_elements(mincount) eq 0 then mincount = [25,25]
   if n_elements(mindist) eq 0 then mindist = [50,50]
   if n_elements(numdevs) eq 0 then numdevs = 2.0
   if n_elements(max_point_count) eq 0 then max_point_count = 50	;7/25/05 for max number used to test gifov.



;thisgcpconf is the confidence score for the gcp that is being
;  passed as thisgcp.  If not set, then it's simply put at one

  if n_elements(confthisgcp) eq 0 then confthisgcp = 1.0


			;7/21/05 Taking a subset of the gcps so that
			;  that we don't have to calculate too many distances
			;  in the distance matrix since that slows things down
			;  too much.


       ;now check how many points there are, and subset
       ;   if need be.
       if n_elements(gcp) gt 4 then begin


        n_good_points = (size(gcp, /dim))[1]
        if n_good_points gt max_point_count then begin
       		subset = non_repeat_random(max_point_count, n_good_points)
       		s_gcp = gcp[*, subset]
         end else s_gcp = gcp
       end


   ;Call the sq_remove_outliers program

     ;get rid of first layer of outliers

ttz = systime(/seconds)
      ret = sq_remove_outliers(s_gcp, refinfo, inpinfo, backrotvect, $
       				mindist=mindist, mincount=mincount)


       if total(ret.valid eq 1) eq 0 then goto, past

     ;Call it a second time

       ret = sq_remove_outliers(ret.gcp, refinfo, inpinfo, backrotvect, $
       				mindist=mindist, mincount=mincount)
print, 'n_elements s_gcp
print, n_elements(s_gcp)

print, 'time
print, systime(/seconds)-ttz

     past:

   ;IF USER WANTED TO KNOW ABOUT THIS GCP, THEN TEST HERE

   if n_elements(thisgcp) ne 0 and total(ret.valid) ge 1 then begin

      ;First we add this gcp to the set of existing ones,
        newgcp = [ [ret.gcp], [thisgcp]]

      ;then call sq_remove_outliers with the information on
      ;  the confidence we have in this gcp incorporated into the
      ;  number of deviations that we'll tolerate on this point
      ;  If the gcp is just at the threshold defined by the user,
      ;  the confidence will be around 1.0.  It goes up quadratically


      dummy = sq_remove_outliers(newgcp, refinfo, inpinfo, backrotvect, $
      		outliers = outliers, numdevs = 3.0 * confthisgcp)



      ;If there are enough points to make a call for x or y, and if
      ;   the new x or y is an outlier, then we set this_gcp[x,y] to
      ;   to 0.  If there are not enough points, then we have to
      ;   just set this_gcp to 1.  If there are enough
      ;   points and the new point is not an outlier, we also set
      ;   this_gcp to one

      z = (size(outliers, /dim))[0]
      new_thisgcp = bytarr(2)

      if dummy.valid[0] eq 1 and outliers[z-1,0] eq 1 then $
      	new_thisgcp[0]=0 else $
      	new_thisgcp[0]=1

      if dummy.valid[1] eq 1 and outliers[z-1,1] eq 1 then $
      	new_thisgcp[1]=0 else $
      	new_thisgcp[1]=1



      ;transfer the validity of this gcp point back to this_gcp,
      ;   overwriting the existing gcp coordinates

        thisgcp = new_thisgcp

   end else thisgcp = [1,1]	;we can't make any claim about it ,
   				;so we just call it good

   ;Using the estimates from sq_remove_outliers, calculate
   ;   the input gifov
   ;  sq_remove_outliers returns the subsetted gcps (the ones without
   ;     outliers) and the xest and yest, which are the estimates
   ;     of the ratio between the GIFOVs of ref and inp.  Therefore,
   ;    the new input gifov is refinfo.gifov * x,yest


   ;If there were only outliers, then we still can't make
   ;   a call

     if ret.valid[0] eq -2 then retval = $
     		{gifov:inpinfo.gifov, stdv_gifov:[0,0], valid:[0,0]} else $

     begin
       ;Set up blank inp_gifov

        inp_gifov = fltarr(2)
        inp_gifov_stdv = fltarr(2)
        valid = bytarr(2)

       ;Calculate the input gifov, assuming there's validity

        if ret.valid[0] eq 1 then begin
           inp_gifov[0]=refinfo.gifov[0]*ret.xest[0]
           inp_gifov_stdv[0]= refinfo.gifov[0]*ret.xest[1]
           print, "X input GIFOV valid"
        end else begin
           inp_gifov[0]= inpinfo.gifov[0]
           inp_gifov_stdv[0] = inpinfo.gifov[0]
           valid[0]=0
        end

        if ret.valid[1] eq 1 then begin
           inp_gifov[1]=refinfo.gifov[1]*ret.yest[0]
     	   inp_gifov_stdv[1]=refinfo.gifov[1]*ret.yest[1]
     	   print, "Y input GIFOV valid"
        end else begin
           inp_gifov[1]=inpinfo.gifov[1]
           inp_gifov_stdv[1] = 0
           valid[1]=0
        end

	    print, 'Estimate of GIFOV is'
	    print, inp_gifov
	    print, 'Estimate of STDV of GIFOV is'
	    print, inp_gifov_stdv

        retval = {gifov:inp_gifov, stdv_gifov:inp_gifov_stdv, valid:valid}

     end

return, retval


end









