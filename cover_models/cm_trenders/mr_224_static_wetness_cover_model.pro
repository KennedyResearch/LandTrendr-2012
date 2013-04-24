function mr_224_static_wetness_cover_model, spectral_value, equation=equation
  
  ;cover_value = 90.1 + (spectral_value * 434.18)
  cover_value = 90.1 + ((spectral_value/1000.0) * 434.18)
  equation = '90.1 + ((spectral_value/1000.0) * 434.18)'
  
  low = where(cover_value lt 0, lowcount) 
  high = where(cover_value gt 100, highcount)
  if lowcount gt 0 then cover_value[low] = 0
  if highcount gt 0 then cover_value[high] = 100
   
  return, cover_value 
  
end