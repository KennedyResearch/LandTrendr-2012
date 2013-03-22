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

;Load from a CSV file, reading the header from the first line.
;use this to load Excel files saved as CSV.


function read_generic_csv, filename

if file_exists(filename) eq 0 then begin
   print, 'Error in read_generic_csv.pro'
   print, 'No file named: '
   print, filename
   print, 'Returning.'
   return, {okay:0}
   end

;open the file.

  openr, un, filename, /get_lun


;get the name of the filename, for use in the structure

  name = get_filename(filename)
  name = fix_name(name)


;get the types for the columns


   tempstring = ''
   readf, un, tempstring
   types = strsplit(tempstring, ',' , /extract)
   n_cols = n_elements(types)
;   z = ptrarr(n_cols, /allocate_heap)
;   for i = 0, n_cols-1 do begin
;      case 1 of
;      (types[i] eq 'integer'):  *z[i] = 0
;      (types[i] eq 'float'):  *z[i] = 0.
;      (types[i] eq 'string'):  *z[i] = '""'
;      (types[i] eq 'long'): *z[i] = 0l
;      else:	begin
;               print, 'No types defined on first row. Beware unexpected results.'
;               *z[i] = '""'	;default to string, always recoverable
;            end
;
;      endcase
;
;   end
  z= strarr(n_cols)

 for i = 0, n_cols-1 do begin
      case 1 of
      (types[i] eq 'integer'):  z[i] = '0'
      (types[i] eq 'float'):  z[i] = '0.'
      (types[i] eq 'string'):  z[i] = '""'
      (types[i] eq 'long'): z[i] = '0l'
      else:	begin
               print, 'No types defined on first row for column '+string(i)+' Beware unexpected results.'
               z[i] = '""'	;default to string, always recoverable
            end

      endcase

   end
;now get the header names

    readf, un, tempstring
    tempstring = strcompress(tempstring, /remove_all)	;get rid of all spaces

    tempstring = fix_name(tempstring)	;take out periods, minus signs, etc.

    tags = strsplit(tempstring, ',', /extract)
    if n_cols ne n_elements(tags) then begin
       print, 'Error in load_generic_csv.pro'
       print, 'Number of columns of tags not same as number of columns of typedefs'
       print, 'Returning empty handed'
       return, {okay:0}
       end

 ;create one copy of the structure




   ; command =  'oneval = create_struct(name = name, tags '
    command =  'oneval = create_struct(tags '

    for i = 0, n_cols-1 do $
      command = strcompress(command + ',' + z[i])

    command = strcompress(command + ')')


    ok = execute(command)
    if ok eq 0 then stop



 ;read the file through to end, counting number of entries

   count = 0
   while not(eof(un)) do begin
      readf, un, tempstring
      count = count + 1
   end
   ;print, 'check the count'



  ;define the size of the structure
     vals = replicate(oneval, count)	;make the full stack of structures

  ;reset to beginning of file
   point_lun, un, 0


  ;skip the first couple lines
    readf, un, tempstring
    readf, un, tempstring


  ;now go through and read the file
   ;on_ioerror, errorplace
   suspicious_count = 0

   for i = 0, count-1 do begin
     readf, un, tempstring

     seps = strsplit(tempstring, ',', /extract, /preserve_null)
     ns = n_elements(seps)
     countto = n_cols

     if ns ne n_cols then begin
        suspicious_count = suspicious_count+1
        if n_elements(suspicious) eq 0 then suspicious = lonarr(1) else $
        			expand_rows, suspicious, 1,dummy
        suspicious[suspicious_count-1] = i
        countto = min([ns, n_cols])	;need to take the smaller number
        							;because it could be:
        							;   excel has put in extra blank fields (ns > n_cols)
     end

     for j = 0, countto-1 do (oneval).(j) = seps[j]


     vals[i] = oneval
	 errorplace:	;just skip the error

   end
   if n_elements(suspicious) eq 0 then suspicious = -1



   ;free up the file and pointer

  ;  ptr_free, z
  ;  z = 0
    free_lun, un



   return, {okay:1, suspicious:suspicious, vals:vals}
   end






