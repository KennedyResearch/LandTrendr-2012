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

function lt_collapse_segments, vertices, vertvals, static_model, dist_thresh, rec_thresh
    goods = where(vertices ne 0, n_verts)
    
    ;if we're off the image, just return what came in and note
    if n_verts eq 0 then return, {ok:0, vertices:vertices, vertvals:vertvals}
    n_segs = n_verts-1
    
    command = 'new_vv = fix(' + static_model + '(vertvals))'
    ok = execute(command)
    new_vv = new_vv * (vertices gt 0)

    new_vt = (vertices[goods]-min(vertices[goods])) * 5
    use = bytarr(n_verts)+1

    for i = 1, n_verts-2 do begin
        xc = new_vt[i-1:i+1]
        yc = new_vv[i-1:i+1]
        
        angle = compare_angles(xc, yc)
        
        if yc[2] gt yc[1] and yc[1] gt yc[0] then $ 	; two recocery
            if abs(angle) lt rec_thresh then use[i] = 0
            
        if yc[2] lt yc[1] and yc[1] lt yc[0] then $  ;disturbance
            if abs(angle) lt dist_thresh then use[i] = 0
    end
    
    use_em = where(use eq 1, n_use)
    
    if n_use eq 0 then message, 'collapse segments:  no more good segments -- something wrong'
    
    ret_vv = vertvals[goods[use_em]]
    ret_vt = vertices[goods[use_em]]
    
    ;pad it back to original size, this may not be necessary, need to check down stream processing.
    n_removed = n_elements(vertices) - n_elements(ret_vv)
    if n_removed gt 0 then begin
      padzeros = intarr(n_removed)
      ret_vv = [ret_vv, padzeros]
      ret_vt = [ret_vt, padzeros]
    endif
    
    return, {ok:1, vertvals:ret_vv, vertices:ret_vt}
end