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
;   copied, or redistributed without express written permission of
;   Robert E. Kennedy, Oregon State University (kennedyr@fsl.orst.edu).
;   This software is supplied as is, with no express or implied
;   warranties.

function get_pathname, filename, unix=unix

if n_elements(filename) eq 0 then return, 'no_filename'

  ;if unix type, use different slashes

  if n_elements(unix) ne 0 then slash = '/' else slash = '\'


  ;check if user gave a path or a filename -- if a filename, then grab the
  ;directory the file is in; if a directory, then grab the parent directory


  l =strlen(filename)-1
  if strmid(filename, l, 1) eq slash then back = 3 else back = 2

  pathname= ''
  f = str_sep(filename, slash)
  g= n_elements(f)
  for i = 0, g-back do begin
    pathname= pathname+f(i)+slash
  end
  ;pathname=strcompress(pathname, /remove_all)  commented out nov 22, 2003 because of spaces in pathnames
 
 ;even if the user didn't give it, we may need to check to see if the
 ;   correct path separator is used.  
 
   if strlen(pathname) eq 0 and slash ne path_sep() then begin 
          
          ;set the slash to the path separator
          
          slash = path_sep()
          l =strlen(filename)-1
      if strmid(filename, l, 1) eq slash then back = 3 else back = 2
    
      pathname= ''
      f = str_sep(filename, slash)
      g= n_elements(f)
      for i = 0, g-back do begin
        pathname= pathname+f(i)+slash
      end
  
    end
  
 
 
 
  return, pathname
end
