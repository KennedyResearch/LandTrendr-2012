function mr_224_static_tcangle_cover_model, spectral_value, equation=equation 

  ;cover_value = 1.49 + (spectral_value * 1.96)  
  ;cover_value = 1.49 + ((spectral_value/!radeg*10) * 1.96)
  cover_value = 1.49 + ((spectral_value/10.0) * 1.96)
  equation = '1.49 + ((spectral_value/10.0) * 1.96)'
  
  low = where(cover_value lt 0, lowcount) 
  high = where(cover_value gt 100, highcount)
  if lowcount gt 0 then cover_value[low] = 0
  if highcount gt 0 then cover_value[high] = 100
   
  return, cover_value
  
end