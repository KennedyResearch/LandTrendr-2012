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

function matchstr, stringarray, search_string, match_case=match_case, $
       total_match=total_match

;Find any places where any part of string array the entire search
;   string match up, or where any part of the search string and one
;   or more of the full elements of the stringarry match up.

;
;total_match means that the search_string must match the entire
;   element of the string vector, not just a piece of it.
;   this is useful if you're just searching for an identical
;   match in a string array.



 s = size(stringarray)
  if s(0) eq 0 then return, -1
  if size(stringarray, /type) ne 7 then return, -1  ;if not a string
  if strlen(search_string) eq 0 then return, -1     ;for empty strings

  if n_elements(match_case) eq 0 then match_case = 0



  matchcount = 0
  n=n_elements(stringarray)

;if we're matching the entire string in both, then it's easy.
;  do it and return quickly

if n_elements(total_match) ne 0 then begin
  matches = bytarr(n)
  if match_case eq 0 then $
     for i = 0,n-1 do matches[i] = (strupcase(stringarray[i]) eq strupcase(search_string)) else $
     for i = 0,n-1 do matches[i] = stringarray[i] eq search_string
  pos = where(matches eq 1, many)
  return, pos
end

;otherwise, need to search in both directions.
;First, search through the string array for the search string


  for i = 0,n-1 do begin
     if match_case eq 0 then $
       a = strpos(strupcase(stringarray(i)), strupcase(search_string)) else $
       a = strpos(stringarray(i), search_string)

     if a ne -1 then begin
      if matchcount eq 0 then $
            pos= lonarr(1) else expand_cols, pos, 1, newdims
       pos(matchcount) = i
       matchcount=matchcount+1
     end

   end


;Second, search through the string for components that are in the string array

for i = 0,n-1 do begin
  if match_case eq 0 then $
       a = strpos(strupcase(search_string), strupcase(stringarray[i])) else $
       a = strpos(search_string, stringarray[i])

  if strlen(stringarray[i]) eq 0 then a = -1	;have special case of blank parts of string array
  					;that would match with anything. so need to avoid them.


  if a ne -1 then begin
    if matchcount eq 0 then $
        pos= lonarr(1) else expand_cols, pos, 1, newdims
    pos(matchcount) = i
    matchcount = matchcount+1
  end

end


;There's a chance that we could double up on the same index, so we need
;   to take out double hits.  Also, if we have no hits, return, -1


if matchcount ne 0 then ind = pos(uniq(pos)) else ind = [-1]

return, ind
end

