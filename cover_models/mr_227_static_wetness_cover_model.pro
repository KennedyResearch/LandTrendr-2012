function mr_227_static_wetness_cover_model, spectral_value, equation=equation
  
  ;cover_value = 90.1 + (spectral_value * 434.18)
  cover_value = 91.81 + ((spectral_value/1000.0) * 428.81)
  equation = '91.81 + ((spectral_value/1000.0) * 428.81)'
  
  low = where(cover_value lt 0, lowcount) 
  high = where(cover_value gt 100, highcount)
  if lowcount gt 0 then cover_value[low] = 0
  if highcount gt 0 then cover_value[high] = 100
   
  return, cover_value 
  
end