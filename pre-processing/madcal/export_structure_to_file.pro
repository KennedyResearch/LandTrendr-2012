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

pro export_structure_to_file, struct, filename, append=append, noheader=noheader

  if n_elements(noheader) eq 0 then noheader = 1 else noheader = 0

  if n_elements(append) eq 0 then $
    openw, un, filename, /get_lun else $
    openw, un, filename, /get_lun, /append

  n = n_elements(struct)


  tn = tag_names(struct)
  nt = n_elements(tn)

  ;get the types of the output for screening for -Nan and Inf

  isfloat = bytarr(nt)
  for i = 0, nt-1 do isfloat[i] = (size(struct[0].(i), /type) eq 4)
  ispoint = bytarr(nt)
  for i =0, nt-1 do ispoint[i] = (size(struct[0].(i), /type) eq 10)
  isbyte = bytarr(nt)
  for i = 0,nt-1 do isbyte[i] = (size(struct[0].(i), /type) eq 1)

  ws = ''
  if noheader eq 1 then begin
    for i = 0, nt-2 do ws = ws+tn[i]+','
    ws = ws +tn[nt-1]
    printf, un, ws
  end

  for i = 0l, n-1 do begin
    ws = ''
    bu = struct[i]
    ;sort through any non-finite values


    for j = 0,nt-2 do begin
      if (ispoint[j]) then $
        thisone = '<pointer>' else $
        if (isbyte[j]) then thisone = string(fix(bu.(j))) else $	;have to convert bytes before stringing, else get ASCII
        begin

        thisone = string(bu.(j))
        if (isfloat[j]) then begin
          if (finite(bu.(j))) ne 1 then thisone = '' else $	;reset Nan and Inf
            if bu.(j) ge 1000000 then thisone = string(bu.(j), format = '(f20.5)'  )
        end
      end

      ws = strcompress(ws + thisone+',')
    end


    ;add the last one without a comma
    if (ispoint[j]) then $
      thisone = '<pointer>' else $
      begin
      thisone = string(bu.(nt-1))
      if (isfloat[nt-1]) then begin
        if (finite(bu.(nt-1))) ne 1 then thisone = '' else $	;reset Nan and Inf
          if bu.(nt-1) ge 1000000 then thisone = string(bu.(nt-1), format = '(f20.5)'  )
      end
    end


    ws = strcompress(ws+thisone)
    printf, un, ws

  ; print, ws



  end
  free_lun, un

  return

end
