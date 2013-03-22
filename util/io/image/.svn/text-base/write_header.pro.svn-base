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

pro write_header, filewrite, se
openw, un, filewrite, /get_lun
                printf, un, 'Readme file version: '+se.version   ;version of readme maker
  		printf, un, 'Filename: '+se.file
  		printf, un, 'Filetype: '+se.filetype
  		printf, un, 'Created on: '+se.createdate
  		printf, un, 'Spheroid: '+se.spher
  		printf, un, 'Datum: '+se.datum
  		printf, un, 'Projection: '+se.proj
  		printf, un, 'Zone: '+string(format=se.formzone, se.zone)
  		printf, un, 'X dimension: '+string(format=se.formdims, se.dimx)
  		printf, un, 'Y dimension: '+string(format=se.formdims, se.dimy)
  		print, '2'
  		printf, un, 'Byte format: '+string(se.bytetype)
  		printf, un, '# Bands: '+string(se.nband)
  		print, '3'
  		printf, un, 'Upper left pixel:'
  		printf, un, 'X: '+string(format=se.formcoor, se.upx)
  		printf, un, 'Y: '+string(format=se.formcoor, se.upy)
  		printf, un, 'Pixel Size: '+string(format=se.formpix, se.pixsize)
  		printf, un, 'Units: '+se.units
  		printf, un, 'Parent file 1: '+se.parent1
  		printf, un, 'P. file 1 type: '+se.p1type
  		printf, un, 'Parent file 2: '+se.parent2
  		printf, un, 'P. file 2 type: '+se.p2type
  		printf, un, 'Program or routine that created this file'
  		printf, un, ' : '+se.createprog
  		
  		printf, un, 'UrOwner: '+se.urowner  		
  		free_lun, un
  		
  return
  end
  