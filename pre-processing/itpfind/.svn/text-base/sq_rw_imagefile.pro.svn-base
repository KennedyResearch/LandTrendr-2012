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

;squeet_procbundle


;-----------------------------------------------------------------------------
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function sq_rw_imagefile, read_filename, file_info, write=write, unit = unit

ok = 1	;good flag -- will change to 0 if there are any problems


on_error, 2
read = 1
if n_elements(write) ne 0 then read = 0

if n_elements(unit) eq 0 then begin
	if read eq 1 then begin
	on_ioerror, nofile
	openr, un, read_filename, /get_lun
	end else openw, un, read_filename, /get_lun
end else un=unit


goto, file
  nofile:

    errormessage =  'Problems with available image file: '+read_filename+ !error_state.msg
    out = dialog_message(errormessage, /error)
    return, 0

file:

if read eq 0 then begin	;IF read is 0, then it means we need to WRITE!
  count=n_elements(file_info.filename)
  for i = 0, count-1 do begin
  printf, un, 'Filename: '+file_info.filename(i)
  printf, un, 'File codename: '+file_info.codename(i)
  printf, un, format='("Start point:(x,y): ", d20.8, ",", d20.8)', file_info.centerpoint(*,i)
  printf, un, format='("Pixel center to pixel center distance:(x,y): ", d20.8, ",", d20.8)',file_info.gifov(*,i)
  printf, un, 'Rotation: '+string(file_info.rotation(i))

  printf, un, 'Layer to use: '+string(fix(file_info.layer(i)))
  printf, un, 'Ignore: '+string(fix(file_info.ignore(i)))
  ;if maskbelow has not been set, then don't write it

  if n_elements(file_info.maskbelow) ne 0 then $
  		printf, un, 'Mask below: '+string(fix(file_info.maskbelow[i]))
  if n_elements(file_info.maskabove) ne 0 then $
  	  printf, un, 'Mask above: '+string(fix(file_info.maskabove[i]))
  printf, un, '       '
  end
  ;print, "Available image file written"
  goto, past2
end else begin
 count = 0

 ;VVVVVVVVVVVVVVVVVVVVVVVVV


 while not eof(un) do begin
  count=count+1
  if count eq 1 then begin
    filename = strarr(1)
    codename = strarr(1)
    layer = bytarr(1)
    gifov = fltarr(2,1)
    centerpoint = dblarr(2,1)
    rotation = fltarr(1)
    ignore = intarr(1)
    maskbelow = intarr(1)
    maskabove = intarr(1)

  end else begin
    expand_rows, filename, 1, n
    expand_rows, codename, 1, n
    expand_rows, centerpoint, 1, n
    expand_rows, rotation, 1, n
    expand_rows, gifov, 1, n
    expand_rows, layer, 1, n
    expand_rows, ignore, 1, n
    expand_rows, maskbelow, 1, n
    expand_rows, maskabove, 1, n
  end

  query_file, un, 'Filename', a, /norestart
    ;print, a
      if a eq 'no_match' then begin
        filename=filename(0:count-2)
        codename=codename(0:count-2)
        centerpoint = centerpoint(*,0:count-2)
        gifov = gifov(*,0:count-2)
        layer =layer(0: count-2)
        goto, past
      end
      filename(count-1) = a

  query_file, un, 'File codename', a, /norestart
      if a eq 'no_match' then a= '_'
      codename(count-1) = a
  query_file, un, 'Start point:(x,y)', a, /norestart
      if a eq 'no_match' then begin

      	   errormessage =  ('The image file '+read_filename+' is missing a correct start point:(x,y): line.  Please check syntax and  re-run the program.')
      	   out = dialog_message(errormessage, /error)
           free_lun, un
           return, 0
      	   end

      centerpoint(*, count-1) = double(str_sep(a,','))
  query_file, un, 'Pixel center to pixel center distance:(x,y)', a, /norestart

      if a eq 'no_match' then begin
      	    errormessage =  ('The image file '+read_filename+' is missing a correct Pixel center to pixel center distance:(x,y): line.  Please check syntax and  re-run the program.')
      	   out = dialog_message(errormessage, /error)
           free_lun, un
           return,0

           end
     gifov(*, count-1)= double(str_sep(a, ','))

  query_file, un, 'Rotation', a, /norestart
      if a eq 'no_match' then begin
      	      errormessage =  ('The image file '+read_filename+' is missing a correct Rotation: line.  Please check syntax and  re-run the program.')
      	   out = dialog_message(errormessage, /error)
           free_lun, un
           return,0

      	   end
      rotation(count-1) = float(a)
  query_file, un, 'Layer to use', a, /norestart
      if a eq 'no_match' then begin
      	  layer(count-1) = 0
      	  ;print, 'Using first layer for '+read_filename

      end else layer(count-1) = fix(a)

  query_file, un, 'Ignore', a, /norestart
      ignore(count-1) = fix(a)


  ;add for clouds
  ;   need to fix this eventually to handle other more diverse
  ;   data types if necessary.

  query_file, un, 'Mask below', a, /norestart, /nextlineonly
    if a eq 'N.A.' or a eq 'no_match' then $
    		maskbelow[count-1] = -32767 else $
			  maskbelow[count-1] = fix(a)


   query_file, un, 'Mask above', a, /norestart, /nextlineonly
    if a eq 'N.A.' or a eq 'no_match' then $
    	maskabove[count-1] = 32767 else $
       maskabove[count-1] = fix(a)

 end
end

;^^^^^^^^^^^^^^^^^^^^^^^^^


past:

file_info = {filename:filename, codename:codename, centerpoint:centerpoint,$
	gifov:gifov, layer:layer, ignore:ignore, rotation:rotation, maskbelow:maskbelow, $
	maskabove:maskabove}
past2:
if n_elements(unit) eq 0 then free_lun, un

return, ok

end

