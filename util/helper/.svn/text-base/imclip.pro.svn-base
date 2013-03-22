function imclip, image, percent=percent

  ;check arguments
  if (n_params() ne 1) then $
    message, 'usage: result = imclip(image)'
  if (n_elements(image) eq 0) then $
    message, 'argument image is undefined'
    
  ;check keywords
  if (n_elements(percent) eq 0) then percent = 2.0
  
  ;get image min and max
  min_value = min(image, max=max_value)
  
  ;compute histogram
  nbins = 100
  binsize = float(max_value - min_value) / float(nbins)
  hist = histogram(float(image), binsize=binsize)
  bins = lindgen(nbins + 1) * binsize + min_value
  
  ;compute normalized cumulative sum
  sum = fltarr(n_elements(hist))
  sum[0] = hist[0]
  for i = 1L, n_elements(hist) - 1L do $
    sum[i] = sum[i-1] + hist[i]
  sum = 100.0 * (sum / float(n_elements(image)))
  
  ;find and return the range
  range = [min_value, max_value]
  index = where((sum ge percent) and $
    (sum le (100.0 - percent)), count)
  if (count ge 2) then $
    range = [bins[index[0]], bins[index[count -1]]]
  return, range
  
end