

; calculate magnitude and duration arrays from vertex year and value array

PRO vert_mag_dur, VERTYR, VERTVAL, MAG=MAG, DUR=DUR, DISTREC=DISTREC    
    
    
    dim = size(VERTYR, /dimension)
    n = dim[2]
        
    DUR = (shift(VERTYR, [0,0,-1])-VERTYR)[*,*,0:(n-2)] > 0
    MAG = (shift(VERTVAL, [0,0,-1])-VERTVAL)[*,*,0:(n-2)] * (DUR GT 0) * (-1)

    ; calculate distrec
    DISTREC = make_array([dim[0:1], 3], type=2)
    DISTREC[*,*, 0] = max(MAG, dimension=3, min=maxrec)
    DISTREC[*,*, 1] = temporary(maxrec)
    
    totalmag = total(abs(MAG),3, /integer)  ;the total distance traversed, up or down
    summag   = float(total(MAG,3))      ;the actual value with pluses and minuses
    temp     = make_array(dim[0:1], type=2)
    
    j = where(totalmag eq 0, n_j, complement=k, ncomplement=n_k)
    
    if n_j gt 0 then temp[j] = -1500

    if n_k gt 0 then temp[k] = (summag[k]/totalmag[k])*1000 ;will be -1000 if all rec, + 1000 if all dist
    
    DISTREC[*,*,2] = temporary(temp)

    
END