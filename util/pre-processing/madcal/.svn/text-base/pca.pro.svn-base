function pca, image, imglayers, maskimg, subset=subset, writeout=writeout

  

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
  
  mastersubset=subset  ;to avoid having different sized subsets after reading by zot_img

  subset=mastersubset
  zot_img, image, hdr, img, layers=imglayers, subset=subset
  subset=mastersubset
  zot_img, maskimg, mhdr, mimg, subset=subset 
  
  ;check is the values in the iamge are going to get through PCA
  unique = fast_unique(img)
  if n_elements(unique) eq 1 then begin
    print, ""
    print, ">>> warning!!! preparing to run PCA..."
    print, ">>> but it appears that the image has only..."
    print, ">>> one value: ", unique
    print, ">>> if it is zero there is a problem with..."
    print, ">>> the cloudmask, if else, open the image:"
    print, "      ", img
    print, ">>> and investigate..."
    print, ""
    print, ">>> ending program"
    print, ""
    stop
  endif
  unique = 0; save memory
   
  for i=0, n_elements(imglayers)-1 do img[*,*,i] = img[*,*,i]*mimg  ;apply the mask to the image
  mimg = 0 ;free memory
  
  ;setup and run the princple component
  n3d = n_elements(imglayers)
  nvariables = 1
  rowsize = hdr.filesize[0]*hdr.filesize[1]
  img = transpose(reform(temporary(img),rowsize,n3d))
  img = pcomp(img, nvariables=nvariables)
  img = reform(transpose(img),hdr.filesize[0],hdr.filesize[1],nvariables)
  ;img = img[*,*,0]
  
  
  if keyword_set(writeout) eq 1 then begin
    rootdir = strmid(image,0, (strlen(file_dirname(image))-11))
    outfile = strcompress(rootdir+"madcal\pcomp1.bsq", /rem)
    openw, un, outfile, /get_lun
    writeu, un, img
    free_lun, un
    hdr.pixeltype = 9   ;signed 16bit    ;need to be changed to _____ for 8-bit when we switch
    hdr.n_layers = 1
    write_im_hdr, outfile, hdr
  endif else return, img
end