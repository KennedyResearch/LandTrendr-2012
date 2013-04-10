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

;-------------------------------------------------------------
;+
; NAME:
;       IMGFRM
; PURPOSE:
;       Puts a specified border around an image.
; CATEGORY:
; CALLING SEQUENCE:
;       imgfrm, img, vals
; INPUTS:
;       vals = array of frame values. in.
; KEYWORD PARAMETERS:
; OUTPUTS:
;       img = Image to modify.        in, out.
; COMMON BLOCKS:
; NOTES:
;       Notes: values in array vals are applied from
;         outside border of the image inward.  A single
;         scalar values just replace the border values
;         in the image.  Good for zeroing image edge.
; MODIFICATION HISTORY:
;       R. Sterner. 25 Sep, 1987.
;       R. Sterner, 27 Jan, 1993 --- dropped reference to array.
;       Johns Hopkins University Applied Physics Laboratory.
;
; Copyright (C) 1987, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	PRO IMGFRM, IMG, VALD, help=hlp
 
	IF (N_PARAMS(0) LT 2) or keyword_set(hlp) THEN BEGIN
	  print,' Puts a specified border around an image.'
	  PRINT,' imgfrm, img, vals'
	  PRINT,'   img = Image to modify.        in, out.'
	  PRINT,'   vals = array of frame values. in.'
	  print,' Notes: values in array vals are applied from'
	  print,'   outside border of the image inward.  A single'
	  print,'   scalar values just replace the border values'
	  print,'   in the image.  Good for zeroing image edge.'
	  RETURN
	ENDIF
 
	VAL = VALD
	NV = N_ELEMENTS(VAL)
	SZ = SIZE(IMG)
	NX = SZ(1)
	NY = SZ(2)
 
	FOR I = 0, NV-1 DO BEGIN
	  T = FLTARR(NX-I-I) + VAL(I)
	  IMG(I,I) = T
	  IMG(I,NY-1-I) = T
	  T = TRANSPOSE(FLTARR(NY-I-I) + VAL(I))
	  IMG(I,I) = T
	  IMG(NX-1-I,I) = T
	ENDFOR
 
	RETURN
	END
