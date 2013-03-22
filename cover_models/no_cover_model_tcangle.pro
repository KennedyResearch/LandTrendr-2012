
function no_cover_model_tcangle, spectral_value, equation=equation

;  'band1':modifier       =  1
;  'band2':modifier       =  1
;  'band3':modifier       =  1
;  'band4':modifier       = -1
;  'band5':modifier       =  1
;  'band7':modifier       =  1
;  'nbr':modifier         = -1
;  'wetness': modifier    = -1
;  'brightness': modifier =  1
;  'greenness': modifier  = -1
;  'tcangle':modifier     = -1
;  'ndvi':modifier        = -1
;  'biomass':modifier     = -1
;  'probfor':modifier     = -1
;  
;  if the above modifier for the index of interest is equal to 1 then you must multiply the spectral_value...
;  by -1 and then get the values above 0 by adding an adjustment factor of sufficient size to do so across...
;  the range of values for the given index
  
  cover_value = spectral_value
  equation = 'spectral_value'

  return, cover_value

end