function pca1, image, maskimg, layers, subset=subset, othermask=othermask

  if keyword_set(subset) ne 1 then begin
    zot_img, image, m_hdr, m_img, /hdronly
    ul = m_hdr.upperleftcenter
    lr = m_hdr.lowerrightcenter
    
    zot_img, maskimg, m_hdr, m_img, /hdronly
    if m_hdr.upperleftcenter[0] gt ul[0] then ul[0] = m_hdr.upperleftcenter[0]
    if m_hdr.upperleftcenter[1] lt ul[1] then ul[1] = m_hdr.upperleftcenter[1]
    if m_hdr.lowerrightcenter[0] lt lr[0] then lr[0] = m_hdr.lowerrightcenter[0]
    if m_hdr.lowerrightcenter[1] gt lr[1] then lr[1] = m_hdr.lowerrightcenter[1]
    
    subset = [[ul],[lr]]
  endif
  
  
  ;read in the image and the mask with the subset
  mastersubset=subset
  ;ok = adjust_subset(mastersubset, image)
  subset=mastersubset
  zot_img, image, dep_hdr, img, layers=layers, subset=subset
  ;ok = adjust_subset(mastersubset, image)
  subset=mastersubset
  zot_img, maskimg, mhdr, mimg, subset=subset
  
  ;create some things for congrid
  dim = dep_hdr.filesize
  maxsize = float(max([dim[0],dim[1]]))
  denom = float(1000/maxsize)
  xsize = round(dim[0]*denom)
  ysize = round(dim[1]*denom)
  
  ;apply the mask
  if keyword_set(othermask) eq 1 then mimg=temporary(mimg)*othermask
  for i=0, n_elements(layers)-1 do begin
    img[*,*,i] = temporary(img[*,*,i])*mimg
  endfor  

;  outimg = "F:\047030\masked_img.bsq"
;  openw, unit, outimg, /get_lun
;  writeu, unit, img
;  free_lun, unit
;  dep_hdr.n_layers = 3
;  dep_hdr.pixeltype = 3
;  write_im_hdr, outimg, dep_hdr  
  
  ;good up to here with the masking
  
  
  ;reduce the size of the image
  img = congrid(temporary(img), xsize, ysize, 3)
  
  ;create an image holder
  img1 = bytarr(3, xsize * ysize)
  
  ;reform the data
  for i=0, n_elements(layers)-1 do img1[i,*] = Reform(img[*,*,i], 1, xsize * ysize)
  ;img = reform(img, 3, xsize*ysize, /overwrite)
  
  ;get the covariance and eigenvectors
  covariance = correlate(img1, /covariance)
  eigenvalues = eigenql(covariance, eigenvectors=eigenvectors)
  ;eigenvectors = round(temporary(eigenvectors)*10000)
  
  ;read in and set the original image up for multiplication
  img1 = bytarr(3, dim[0] * dim[1])
  for i=0, n_elements(layers)-1 do begin
    subset=mastersubset
    zot_img, image, dep_hdr, img, layers=layers[i], subset=subset
    img1[i,*] = Reform((img*mimg), 1, dim[0] * dim[1])
  endfor
  img = 0
  mimg = 0 ;save memory 
  
;  ok = adjust_subset(mastersubset, image)
;  zot_img, image, dep_hdr, img, layers=layers, subset=ok.subset
;  
;  img = reform(img, dim[0]*dim[1], 3, /overwrite)
  
  ;multiply
  ;img1 = eigenvectors ## transpose(temporary(img1))
  img1 = matrix_multiply(temporary(img1),eigenvectors, /atranspose)
  
  img1 = reform(temporary(img1), dim[0], dim[1], 3)
  
  img1 = temporary(img1[*,*,0]) ;pull out component 1
  
  
;  outimg = "F:\047030\madcal\pca_from_pca1c.bsq"
;  openw, unit, outimg, /get_lun
;  writeu, unit, img1
;  free_lun, unit
;  dep_hdr.n_layers = 1
;  dep_hdr.pixeltype = 9
;  write_im_hdr, outimg, dep_hdr
  return, img1
  
end