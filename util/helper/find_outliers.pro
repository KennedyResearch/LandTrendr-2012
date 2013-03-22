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

function find_outliers, array, numdevs = numdevs, iterative =iterative, $
	indices = indices, one=one, minabs=minabs


;Find outiers in an array by comparing each point
;   to the mean of the other points in the array.
;   Returns an array of 0's and 1's equal to size of array,
;   with 1 being an outlier

;If there are not enough points to calculate means and standard deviations
;  then we return a -2
;  If /indices is set and there are no outliers, we return a -1

;Before doing anything, first check to see if we have enough elements int
;  the array to get a mean and standard deviation.  We will be taking out
;  one point and getting mean from the others, and 3 others are necessary,
;  so anything less than 4 points is too few.  In that case, return a
;  -2 flag.
;minabs is the minimum allowable standard deviation to accept,
;   as a proportion of the mean.  If the mean is 100, and minabs is
;   set to .01, then even if the standard deviation of all the other
;   points is less than 1 (0.01 * 100), 1 will be the value
;   used for the comparison to determine if to threshold


  if n_elements(array) lt 4 then return, [-2]



;  Numdevs is the number of stdvs that is used
;   as a threshold for calling a point an outlier
;  If not set, 2.0 is used

   if n_elements(numdevs) eq 0 then numdevs = 2.0


;if iterative is set, then we go through until no more outliers are found

   if n_elements(iterative) eq 0 then iterative = 0


;if indices is set, we return the indices of the outliers,
;  not an array of 0's and 1's

   if n_elements(indices) eq 0 then indices = 0 else indices = 1


;if 'one' is set, then we only return the most outlierish of points

   if n_elements(one) eq 0 then one = 0 else one = 1


new = array
curcount = n_elements(new)
indx = lindgen(curcount)
outlier = bytarr(curcount)
maxoutlier = fltarr(2)	;the pointer to the index of the maximum outlier

;Check for non-finite values, since these need to be thrown out first

  fin = finite(array)
  if total(fin) ne curcount then begin
     inf_ind = where(fin ne 1)
     outlier[inf_ind] = 1
     maxoutlier = [0,inf_ind[0]]	;point to the first infinite number
     					;in case /one was set
     goto, done
     end




;With only good values, circle through all and find outliers

   for i = 0, curcount-1 do begin
     yes = where(indx ne i)
     other = moment(new(yes))
     dist = (new[i]-other[0])^2

   ;override if the existing points have a teensy stdv

      if n_elements(minabs) ne 0 then $
        if other[1] / other[0] lt minabs then other[1] = other[0]*minabs


     if dist gt (numdevs^2)*other[1] then outlier[i]=1


    ;keeping track of maxoutlier, if this distance is greater than the
    ;    previous maximum, then set maxoutlier to this distance

      if dist gt maxoutlier[0] then $
      	maxoutlier = [dist, i]

   end


 done:		;done finding all outliers
 		;If there are non-finite numbers we get shunted here as well


  ;if we're only sending back one value, then return only the
  ;    biggest outlier's value.

     if one eq 1 then begin
     	mask = bytarr(curcount)
     	mask[maxoutlier[1]] = 1
     	outlier = outlier and mask	;if outlier[maxoutlier[1]] is 1, it
     					;stays.  All other points are zeroed.
     end



  ;if we're not iterative, then go home with just these vals

     if iterative eq 0 then goto, out


  ;if we're doing it iteratively, we check out how many 1's there are
  ;   If there aren't any, then we just go home with -1

     if total(outlier) eq 0 then goto, out


  ;otherwise, we recursively call this program

     okay = where(outlier ne 1, many)

     subpoints = find_outliers(new[okay], numdevs=numdevs, $
     			/iterative, /indices)
     if subpoints[0] eq -2 then return, [-2]	;if there weren't enough
     						;points, then we return -2

  ;if there were any outliers below, then the indices of subpoints need
  ;   to be set in this current level of outliers.  If there were no
  ;   outliers, then subpoints will return -1

     if subpoints[0] ne -1 then outlier[okay[subpoints]] = 1

out:

if indices eq 0 then 	return, outlier else $
			return, where(outlier eq 1)
end
