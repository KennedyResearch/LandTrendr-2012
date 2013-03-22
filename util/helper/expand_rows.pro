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

pro expand_rows, variable, addrows, newdims

si=size(variable)
ndims=si(0)
ncols = si(1)	;number of columns

;get the current number of rows
  case 1 of
  (ndims eq 0):	stop		;if the array is empty, we can't continue
  				;because we don't know how many columns to create
  (ndims eq 1):	nrows = 1 	;if it's just a single
				;row, we have to make
				;sure IDL doesn't look for
				;a row count
  (ndims gt 1):   nrows = si(2)
  else:
  endcase

if ndims eq 3 then nstacks = si(3)		;if it's a three-d
						;array, we need to keep
						;track of how many layers
						;deep we should go
type=si(si(0)+1)	;the type of file
			;1=bytarr
			;2=intarr
			;3=lonarr
			;4=fltarr
			;7=strarr
			;8=structure array

old=variable
new_rownum=nrows+addrows

  if ndims le 2 then begin	;if we're going to end up with a
				;two-d array (even if we start with a line,
				;we're adding the row so we'll get a 2-d)
  case 1 of
    (type eq 1):	variable=bytarr(ncols, new_rownum)
    (type eq 2):	variable=intarr(ncols, new_rownum)
    (type eq 3):	variable=lonarr(ncols, new_rownum)
    (type eq 4):	variable=fltarr(ncols, new_rownum)
    (type eq 5):	variable=dblarr(ncols, new_rownum)
    (type eq 7):	variable=strarr(ncols, new_rownum)
    (type eq 8):	begin
    			  base = old[0]	;get copy of zeroth element

    			  variable = replicate(base, ncols, new_rownum)

    			end


    else:	stop
  endcase
  variable(*, 0:nrows-1)=old
  end else begin		;if we're working with a three-d
  				;array
  case 1 of
    (type eq 1):	variable=bytarr(ncols, new_rownum, nstacks)
    (type eq 2):	variable=intarr(ncols, new_rownum, nstacks)
    (type eq 3):	variable=lonarr(ncols, new_rownum, nstacks)
    (type eq 4):	variable=fltarr(ncols, new_rownum, nstacks)
    (type eq 5):	variable=dblarr(ncols, new_rownum, nstacks)
    (type eq 7):	variable=strarr(ncols, new_rownum, nstacks)
    (type eq 8):	begin
    			  base = old[0]	;get copy of zeroth element
    			  variable = replicate(base, ncols, new_rownum, nstacks)
    			end
    else:	stop
  endcase
  variable[*, 0:nrows-1, *]=old
  end

;assign the new dimensions
if ndims ne 3 then newdims = [ncols, new_rownum] else newdims = [ncols, new_rownum, nstacks]
return
end

  ;Sep18 2000.  Added ability to expand structure arrays

