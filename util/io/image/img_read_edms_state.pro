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

pro img_read_Edms_State, fileunit, numvirtualblocks, numobjectsperblock,  $
  	blocksize, compresstype, blockaddresses
  	
;get the number of blocks
; and the pixels per block

  numvirtualblocks = img_readlong(fileunit)
  numobjectsperblock = img_readlong(fileunit)
  blank = img_readlong(fileunit)
  skip = img_readshort(fileunit)
  
;set up the variable that will point to each blocks location in the file
;	and the variable that keeps track of whether a given block is 
;	compressed or not.

  blockaddresses = lonarr(numvirtualblocks)
  compresstype = lonarr(numvirtualblocks)
  blocksize = lonarr(numvirtualblocks)
    
;read in the block addresses

  howmany = img_readlong(fileunit)
  whereisit = img_readlong(fileunit)
  point_lun, fileunit, whereisit
  
 ;read the virtual block information
   
  for i = 0l, numvirtualblocks-1 do begin
    skip = img_readshort(fileunit)
    pointer = img_readlong(fileunit)
    blockaddresses(i) = pointer
    blocksize(i) = img_readlong(fileunit)
    skip = img_readshort(fileunit)
    compresstype(i) = img_readshort(fileunit)
  end
  
;I'm not interested the FreeIDList, which is next, or in the TIME
  
return
end
