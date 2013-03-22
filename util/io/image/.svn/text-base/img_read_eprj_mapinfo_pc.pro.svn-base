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

pro img_read_Eprj_MapInfo_pc, fileunit, proName, upperLeftCenter, lowerRightCenter, $
	pixelSize, units

;first we need to see how many characters are in the name of the projection,
;   and where IT is

  numchars = img_readlong_pc(fileunit)
  position = img_readlong_pc(fileunit)

;move to that place and get the name of the projection
  point_lun, fileunit, position

  proName = img_readchar(fileunit, numchars)


;now get info on the coordinates

  numcoords = img_readlong_pc(fileunit)
  coords_pos = img_readlong_pc(fileunit)

;move to the coordinate starting point
  point_lun, fileunit, coords_pos
  upperleftx= img_readdouble_pc(fileunit)
  upperlefty= img_readdouble_pc(fileunit)
  upperLeftCenter = [upperleftx, upperlefty]

;do the same for the lower right coordinates
  numcoords = img_readlong_pc(fileunit)
  coords_pos = img_readlong_pc(fileunit)

  point_lun, fileunit, coords_pos
  lowerrightx = img_readdouble_pc(fileunit)
  lowerrighty = img_readdouble_pc(fileunit)
  lowerRightCenter = [lowerrightx, lowerrighty]

;locate the pixel size info
  numsizes = img_readlong_pc(fileunit)
  size_pos =img_readlong_pc(fileunit)

  point_lun, fileunit, size_pos
  xsize = img_readdouble_pc(fileunit)
  ysize = img_readdouble_pc(fileunit)
  pixelSize = [xsize,ysize]

;get units for projection

  numchars = img_readlong_pc(fileunit)
  chars_pos = img_readlong_pc(fileunit)

  point_lun, fileunit, chars_pos
  units = img_readchar(fileunit, numchars)

return
end
