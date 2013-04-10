function cover_model_035037, nbr, equation=equation ;nbr100, nbr0   - add these into a percent cover parameter file
  
  ;nbr100 is the nbr value for a sample of cells that have 100 percent tree canopy cover according to aerial imagery or plot data
  ;nbr0 is the nbr value for a sample of cells that have 0 percent tree canopy cover according to aerial imagery or plot data 
  
  nbr100 = 650
  nbr0   = 50
  
  slopefactor = (nbr100-nbr0)/(100-0);  y2-y1/x2-x1
  
  cover = (nbr-nbr0)/slopefactor  ;  (nbr value - offset)/slope factor
    
  low = where(cover lt 0, lowcount) 
  high = where(cover gt 100, highcount)
  if lowcount gt 0 then cover[low] = 0
  if highcount gt 0 then cover[high] = 100
  
  ;print, cover 
  return, cover
  
end