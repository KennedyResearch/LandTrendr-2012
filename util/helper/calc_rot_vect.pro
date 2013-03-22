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

function calc_rot_vect, ang, rad=rad

;This function returns a (2,2) array of x,y coefficients to 
;   translate map coordinates in the an unrotated image to 
;   map coordinates in the rotated image.
;The returned vector - rot_vect	[a,b]
;				[c,d]
;works as follows:
;   Given an offset in the original map coordinate system -  [x, y],
;   where x is easting and y is northing, the offset in the 
;   rotated system is :   (xrot,yrot) = calc_rot_vect(ang)##[x,y]
;
;We assume values in degrees (requiring a change to radians before using
;	trig functions), but can handle rads if user specifies by 
;	simply not transforming.
;
;Angle is in degrees clockwise from north; i.e. if the rotated coordinate
;	system is oriented toward northeast, then angle would be +45.0

if n_elements(rad) eq 0 then angle = ang*!dtor else angle=ang
return,  [ [cos(angle), sin(angle)], [-sin(angle), cos(angle)] ]
end


