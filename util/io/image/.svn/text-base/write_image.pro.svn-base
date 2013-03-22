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

; $Id: write_image.pro,v 1.16 2002/02/06 21:45:57 scottm Exp $
;
; Copyright (c) 1998-2002, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	WRITE_IMAGE
;
; PURPOSE:
;       The WRITE_IMAGE procedure writes an image and its color table
;       vectors, if any, to a file of a specified type. WRITE_IMAGE
;       can write most types of image files supported by IDL.  See
;       QUERY_IMAGE for a list of supported formats.
;
; CATEGORY:
;       Input/Output
;
; CALLING SEQUENCE:
;       WRITE_IMAGE, Filename, Format, Data [, Red, Green, Blue])
;
; INPUTS:
;	Filename: A scalar string containing the name of the file to write.
;
;       Format: A scalar string containing the name of the file format to
;       write.  See QUERY_IMAGE for a list of supported formats.
;
;       Data: An IDL variable containing the image data to write to the file.
;
;       Red: A vector containing the red channel of the color table
;            if a colortable exists.
;
;       Green: A vector containing the green channel of the color table
;            if a colortable exists.
;
;       Blue: A vector containing the blue channel of the color table
;            if a colortable exists.
;
;
; OPTIONAL KEYWORDS:
;
;       APPEND - Set this keyword to force the image to be appended to
;       the file instead of overwriting the file.  APPEND may be used
;       with image formats which support multiple images per file, and
;       is ignored for formats which support only a single image per file.
;
; EXAMPLE:
;       WRITE_IMAGE, 'myImage.png', 'PNG', data, r, g, b
;
; MODIFICATION HISTORY:
; 	Written by:	Scott Lasica, July, 1998
;-
;

pro WRITE_IMAGE, filename, format, data, red, green, blue, APPEND=append, _EXTRA = extra
  ON_ERROR, 2			;Return to caller if error

  ; let user know about demo mode limitation.
  ; all write options disabled in demo mode
  if (LMGR(/DEMO)) then begin
      MESSAGE, 'Feature disabled for demo mode.'
      return
  endif

  case STRUPCASE(format) of
    'BMP': begin
      WRITE_BMP, filename, data, red, green, blue, /RGB, _EXTRA=extra
    end
    'GIF': begin
      dims = SIZE(data, /DIMENSIONS)
      nDims = SIZE(data,/N_DIMENSIONS)
      if (nDims eq 3) then begin
        if (dims[0] eq 3) then ileave=1
        if (dims[1] eq 3) then ileave=2
        if (dims[2] eq 3) then ileave=3
       	tmpData = COLOR_QUAN(data, ileave, r, g, b, /DITHER)
       	WRITE_GIF, filename, tmpData, r, g, b, _EXTRA=extra
      endif $
      else begin
        if (SIZE(data, /N_DIMENSIONS) ne 2) then begin
          MESSAGE,'Array must have 2 or 3 dimensions.'
          return
        endif
        WRITE_GIF, filename, data, red, green, blue, _EXTRA=extra
      endelse
    end
    'JPEG': begin
      dims = SIZE(data, /DIMENSIONS)
      nDims = SIZE(data,/N_DIMENSIONS)
      if (nDims eq 3) then begin
        if (dims[0] eq 3) then ileave=1
        if (dims[1] eq 3) then ileave=2
        if (dims[2] eq 3) then ileave=3
        WRITE_JPEG, filename, data, TRUE=ileave, _EXTRA=extra
      endif $
      else begin
        if (SIZE(data, /N_DIMENSIONS) ne 2) then begin
          MESSAGE,'Array must have 2 or 3 dimensions.'
          return
        endif
        if ((N_ELEMENTS(red) gt 0) and (N_ELEMENTS(green) gt 0) and $
            (N_ELEMENTS(blue) gt 0)) then begin
          newData = BYTARR(3,dims[0],dims[1])
          newData[0,*,*] = red[data]
          newData[1,*,*] = green[data]
          newData[2,*,*] = blue[data]
          WRITE_JPEG, filename, newData, TRUE=1, _EXTRA=extra
        endif $
        else $
          WRITE_JPEG, filename, data, _EXTRA=extra
      endelse
    end
    'PNG': begin
      WRITE_PNG, filename, data, red, green, blue, _EXTRA=extra
    end
    'PPM': begin
      nDims = SIZE(data,/N_DIMENSIONS)
      dims = SIZE(data, /DIMENSIONS)
      if (nDims eq 2) then begin
        if ((N_ELEMENTS(red) gt 0) and (N_ELEMENTS(green) gt 0) and $
            (N_ELEMENTS(blue) gt 0)) then begin
          newData = BYTARR(3,dims[0],dims[1])
          newData[0,*,*] = red[data]
          newData[1,*,*] = green[data]
          newData[2,*,*] = blue[data]
          WRITE_PPM, filename, newData, _EXTRA=extra
          return
        endif
      endif
      WRITE_PPM, filename, data
    end
    'SRF': begin
      if (N_ELEMENTS(red) gt 0) then $
         WRITE_SRF, filename, data, red, green, blue, _EXTRA=extra $
      else $
         WRITE_SRF, filename, data, _EXTRA=extra
    end
    'TIFF': begin
      WRITE_TIFF, filename, data, RED=red, GREEN=green, BLUE=blue, $
        APPEND=append, _EXTRA=extra
    end
    else: begin
      MESSAGE,'Unknown image file format: '+format
    end
  endcase
end
