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

pro query_file, fileunit, searchterm, ret_val, nocolon=nocolon, $
		norestart=norestart, nextlineonly = nextlineonly



;nocolon just searches for the literal string, otherwise the
;	program assumes that there's a colon

  if n_elements(nocolon) eq 0 then searchterm = searchterm+': '

;norestart prevents the program from pointing to the beginning of the
;	file unit each time -- allows for end of file searching

  if n_elements(norestart) eq 0 then norestart = 0 else norestart = 1


;get the current file pointer position

  point_lun, -fileunit, current_pos



;This procedure assumes that the fileunit is already open
;
;It then searches line by line through the opened fileunit for
;	a string that matches the searchterm.  If found, it
;	takes the remainder of the line with that searchterm
;	and returns it in the ret_val
;
;pointer will return the index count of the searchterm
;	i.e. if the searchterm is the first thing in the file, then
;		pointer will be 1

   if norestart eq 0 then point_lun, fileunit, 0


;determine length of the searchterm in characters




st_length= strlen(searchterm) ;length of search term
a=''


  ;if /nextlineonly is set, then we don't loop



     while not eof(fileunit) do begin
        readf, fileunit, a
  	test = strmid(a, 0, st_length)	;the descriptive part of the

  					;file entry
  	if searchterm eq test then begin
  			match_len=strlen(a) - (st_length)	;plus 2 for the ': '

  			ret_val= strmid(a, st_length, match_len)

  			goto, getout
			end
    ;before we stop, though, we need to reset the pointer so we don't skip over the next file

     if n_elements(nextlineonly) ne 0 then begin
           point_lun, fileunit, current_pos ;reposition to where we started
           goto, no_val	;if we didn't find it in the next line, alert calling program
     end



     endwhile

no_val:
ret_val = 'no_match'
goto, getout
nofile:
print, !err_string
ret_val = 'no_file'
getout:

return

end
