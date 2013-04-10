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

;+
; NAME:
;  BACK_VERTS
;
; PURPOSE:
;
; Truncate (remove and interpolate) the segemented LandTrendr trajectory 
; to an earlier END_YEAR. The script modifies VERTYR (vertex year) and 
; VERTVAL (vertex value) images. 
;
; AUTHOR:
;
;
; CATEGORY:
;     Landtrendr, Post-Processing
;
; CALLING SEQUENCE:
;
;
; INPUTS:
;   * VERTVAL vertex value array

;   * VERTYR  vertex year array
;
;   * END_YEAR  new end year of trajectory either as scalar or array with 
;               dimensions equal to VERTVAL and VERTYR
;
;
; KEYWORD PARAMETERS:
;   * use /replace to update input VERTVAL and VERTYR inplace
;   * T_VERTVAL - provide named variable to return updated VERTVAL array
;   * T_VERTYR  - provide named variable to return updated VERTYR array
;
; OUTPUTS:
;   see keyword/parameters section
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;   06.14.2011 Intial creation by Dirk Pflugmacher
;   07.12.2011 Added option to specify variable start years
;
;
;******************************************************************************************;




PRO back_verts, VERTYR, VERTVAL, END_YEAR=END_YEAR, START_YEAR=START_YEAR,  T_VERTYR=T_VERTYR, $
                   T_VERTVAL=T_VERTVAL, REPLACE=REPLACE, SSPAN=SSPAN, ESPAN=ESPAN, check=check

    COMPILE_OPT IDL2

    
    ; make copy of VERTYR & VERTVAL to hold new values
    T_VERTYR = VERTYR
    T_VERTVAL= VERTVAL
    
    
    ; if VERTYR and VERTVAL are vectors (single pixel) reform them
    ; as 2-D array so that we can proceed, we assume the last dimension
    ; denotes the vertices
    dim = (size(T_VERTYR, /dimensions))
    n_dim = size(T_VERTYR, /n_dimensions)
    if n_dim lt 3 then begin
      dim = [replicate(1,3-n_dim),dim]
      T_VERTYR  = reform(T_VERTYR, dim, /overwrite)
      T_VERTVAL = reform(T_VERTVAL, dim, /overwrite)
    endif
    
    dim = (size(T_VERTYR, /dimensions))[0:1]
    n_vert = (size(T_VERTYR, /dimensions))[2]

    if n_elements(check) ne 0 then begin
      print, 'POINT    :', fix(check)    
      print, 'VERTYR   :', reform(VERTYR[check[0],check[1],*])
      print, 'VERTVAL  :', reform(VERTVAL[check[0],check[1],*])
      print, '--------------------------------'  
    endif

    ; check if start year is scalar then extrapolate to image dim
    if n_elements(START_YEAR) eq 1 then startyr = make_array(dim, value=START_YEAR, type=2)
    if n_elements(START_YEAR) gt 1 then startyr = START_YEAR
    ; if END_YEAR is scalar then extrapolate value to image dimension
    if n_elements(END_YEAR) eq 1 then endyr = make_array(dim, value=END_YEAR, type=2)
    if n_elements(END_YEAR) gt 1 then endyr = END_YEAR



    ; ===============================================
    ; if START_YEAR is supplied
    if n_elements(START_YEAR) ne 0 then begin
     
      ; if START_YEAR is time span instead of actual year
      if keyword_set(SSPAN) and n_elements(END_YEAR) ne 0 then startyr = endyr-startyr

      ; create mask to zero out pixels where start year is smaller than minimum year in vertices
      zmask = startyr ge T_VERTYR[*,*,0]
      zmask = rebin(zmask, [dim,n_vert], /sample)
      
      ; compute duration and magnitude
      durs = fix((shift(T_VERTYR,  [0,0,-1])-T_VERTYR )[*,*,0:(n_vert-2)] > 0)
      mags =     (shift(T_VERTVAL, [0,0,-1])-T_VERTVAL)[*,*,0:(n_vert-2)] * (durs gt 0)


      for i=0, n_vert-1 do T_VERTYR[*,*,i] = (T_VERTYR[*,*,i] > startyr) * (T_VERTYR[*,*,i] gt 0)
      
      
      ; adjust duration
      t_durs = (shift(T_VERTYR, [0,0,-1])-T_VERTYR)[*,*,0:(n_vert-2)] ;> 0 
      
     ; create mask to remove dangling end years 
      mask = ([[[t_durs ne 0]], [[make_array(dim, value=1, type=2)]]] * T_VERTYR gt 0) * temporary(zmask)


      t_durs = fix(t_durs > 0)

      
      ; masked VERTYR
      T_VERTYR *= mask
          
      ; compute delta values from delta duration
      d_durs = durs - t_durs
      d_val = fix(round((temporary(d_durs)*temporary(mags))/float(temporary(durs))))


      ; compute new vertvals by updating old VERTVAL with delta values
      T_VERTVAL = (T_VERTVAL + [[[d_val]], [[intarr(dim)]]] ) * temporary(mask)
      d_val=0
;      print, 'T_VERTVAL:', reform(T_VERTVAL[check[0],check[1],*])  
      

      ; remove 0's left of vertices by shifting everything selectively
      ; 2-d mask of valid pixels
      mask = total(T_VERTYR ne 0, 3, /preserve_type) gt 0

      ; shift all valid pixels left as long as they start with 0
      j = where(T_VERTYR[*,*,0] eq 0 and mask eq 1, n)
      while n gt 0 do begin
          
          temp_vertval = shift(T_VERTVAL, [0,0,-1])
          temp_vertyr  = shift(T_VERTYR,  [0,0,-1])

          ; map shifted array to original data at selected locations
          ; don't know a better way then looping through the vertices
          for k=0, n_vert-1 do begin
            temp = T_VERTVAL[*,*,k]
            temp[j] = (temp_vertval[*,*,k])[j]
            T_VERTVAL[*,*,k] = temp
            
            temp = T_VERTYR[*,*,k]
            temp[j] = (temp_vertyr[*,*,k])[j]
            T_VERTYR[*,*,k] = temp
            
          endfor

          j = where(T_VERTYR[*,*,0] eq 0 and mask eq 1, n)
      
      endwhile
      mask=0  
    endif ; done fixing start year



    if n_elements(END_YEAR) ne 0 then begin

        if keyword_set(ESPAN) and n_elements(START_YEAR) ne 0 then endyr = startyr+endyr
    
        ; create mask to zero out pixels where end year is greater than most recent year in vertices
        zmask =  endyr le max(T_VERTYR, dimension=3)
        zmask = rebin(zmask, [dim,n_vert], /sample)
    
        ; make sure end year is after or at start year
        if n_elements(START_YEAR) ne 0 then zmask = rebin(endyr ge startyr, [dim,n_vert], /sample) and zmask
    
        ; compute duration and magnitude
        durs = fix((shift(T_VERTYR, [0,0,-1])-T_VERTYR)[*,*,0:(n_vert-2)] > 0)
        mags = (shift(T_VERTVAL, [0,0,-1])-T_VERTVAL)[*,*,0:(n_vert-2)] * (durs gt 0)
    
    
        ; update maximum (END_YEAR) in T_VERTYR using the '<' operator, which
        ; converts [1972, 1976, 1980, 1989, 1990, 0] < 1980 to [1972, 1976, 1980, 1980, 1980, 0]
        ; to do that we need to loop through all vertex layers
        for i=0, n_vert-1 do T_VERTYR[*,*,i] = (T_VERTYR[*,*,i] < endyr) ; * (T_VERTYR[*,*,i] gt 0)
            
        ; compute new duration also needed for masking out dangling years (e.g. 1980s in the example above)
        t_durs = (shift(T_VERTYR, [0,0,-1])-T_VERTYR)[*,*,0:(n_vert-2)] > 0
        
        ; create mask to remove dangling end years 
        mask = [[[make_array(dim, value=1, type=2)]], [[t_durs gt 0]]] * temporary(zmask)
        
        ; masked VERTYR
        T_VERTYR *= mask    
            
        ; compute delta values from delta duration
        d_durs = durs - t_durs
     ;   d_val = fix(round((temporary(d_durs)*temporary(mags))/float(temporary(durs))))
        d_val = fix(round(((d_durs)*(mags))/float((durs))))
        
        ; compute new vertvals by updating old VERTVAL with delta values
        T_VERTVAL = (T_VERTVAL - [[[intarr(dim)]], [[d_val]]] ) * temporary(mask) 

    endif ; END_YEAR


    T_VERTVAL = reform(T_VERTVAL, /overwrite)
    T_VERTYR  = reform(T_VERTYR, /overwrite)


    if n_elements(check) ne 0 then begin
      print, 'T_VERTYR :', reform(T_VERTYR[check[0],check[1],*])
      print, 'T_VERTVAL:', reform(T_VERTVAL[check[0],check[1],*])  
    endif

    if keyword_set(REPLACE) then begin
      VERTVAL = temporary(T_VERTVAL)
      VERTYR  = temporary(T_VERTYR)
    endif



 
END