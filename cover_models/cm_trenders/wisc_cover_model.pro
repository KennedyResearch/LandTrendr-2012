function wisc_cover_model, spectral_value, equation=equation 
  
  cover_value = (spectral_value-450.0)*0.4348
  equation = '(spectral_value-450.0)*0.4348'
  
  low = where(cover_value lt 0, lowcount) 
  high = where(cover_value gt 100, highcount)
  if lowcount gt 0 then cover_value[low] = 0
  if highcount gt 0 then cover_value[high] = 100
   
  return, cover_value
  
end