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

; $Id: fast_unique.pro,v 1.1 1995/09/18 15:19:16 landers Exp $
;+
; NAME:
;	FAST_UNIQUE
; PURPOSE:
;	Faster version of UNIQUE - at least it's faster at large reductions.
;	If the unique version of the array is much smaller than the original,
;	then this is faster than UNIQUE.
;	Also, FAST_UNIQUE will work on COMPLEX arrays.
;	Also, FAST_UNIQUE does not sort the result.
; CALLING SEQUENCE:
;	result = FAST_UNIQUE( array )
; INPUTS:
;	array - An array to unique.
; OUTPUTS:
;	result - The uniqued array.  A version of array that has all 
;		duplicate elements removed.
; RESTRICTIONS:
;	Dose not work with structures.
; PROCEDURE:
;	Repeatedly remove duplicate elements from array, saving only one copy.
;-
; MODIFICATION HISTORY:
;	Based on source in TIPS.  Updated for User Lib by D. Landers, May 95


FUNCTION fast_unique, array
on_error, 2
a=array		; make local copy - don't destroy user's data
unq = a(0)	; a bogus init value, to copy the type of A

WHILE 1b DO BEGIN  
	unq = [ unq, a(0) ]
	notequal = WHERE( a  NE a(0) )
	IF ( notequal(0) eq -1 ) THEN RETURN, unq(1:*)
	a = a(notequal) 
ENDWHILE

END
