

pro fix_cloud_masks2, path, ppprrr, image_info_savefile, fixmask=fixmask, from_madcal=from_madcal, ledaps=ledaps

  if keyword_set(from_madcal) eq 1 then goto, single_date_from_madcal
  restore, image_info_savefile
  
  ;check for dates that don't have a cloudmask
  nomask = where(image_info.cloud_file eq 'none', n_nomask)
  if n_nomask ge 1 then nomask = image_info[nomask].image_file
  
  ;check for fix dates
  if keyword_set(fixmask) eq 1 then begin
    fixthese = strarr(n_elements(fixmask))
    for i=0, n_elements(fixmask)-1 do begin
      fixthis = strcompress(string(fixmask[i]), /rem)
      year = strmid(fixthis, 0, 4)
      doy = strmid(fixthis, 4, 3)
      fixthis = strcompress("*"+year+"_"+doy+"*", /rem)
      imgmatch = strmatch(image_info.image_file, fixthis)
      thisone = where(imgmatch eq 1, n_thisone)
      if n_thisone eq 1 then fixthese[i] = image_info[thisone].image_file
      if n_thisone gt 1 then begin
        print, "there is more than one match for the cloud fix"
        print, "there can be only one"
        print, image_info[thisone].image_file
        return
      endif
      if n_thisone eq 0 then begin
        print, "there is no match for fixing"
        print, "check this file"
        print, fixmask[i]
        return
      endif
    endfor
  endif
  
  ;fill in the full list of images to fix cloudmask for
  if n_nomask ge 1 and keyword_set(fixmask) eq 1 then begin
    dothese = [nomask,fixthese]
    if n_nomask ge 2 then printthis = transpose(nomask) else printthis = nomask
    print, ""
    print, ">>> there are dates that are missing cloudmasks:"
    print, printthis
    print, ">>> fixing these before doing the requested fixes"
    print, ""
  endif
  if n_nomask ge 1 and keyword_set(fixmask) eq 0 then begin
    dothese = nomask
    print, ""
    print, ">>> there are dates that are missing cloudmasks:"
    print, "    ",nomask
    print, ""
  endif
  if n_nomask eq 0 and keyword_set(fixmask) eq 1 then dothese = fixthese
  
  single_date_from_madcal:
  if keyword_set(from_madcal) eq 1 then dothese = from_madcal
  
  ;---display the image---
  for i=0, n_elements(dothese)-1 do begin
    imgmatch = dothese[i]
    
    start_here1: ;if the final mask was not good then retry
    finalcloudmask = 0 ;free memory if this is redo
    
    ;get rid of gaps and image edges
    zot_img, imgmatch, gaphdr, empties, subset=subset, /hdronly
    gapimg = bytarr(gaphdr.filesize[0], gaphdr.filesize[1])
    for b=0, gaphdr.n_layers-1 do begin
      zot_img, imgmatch, gaphdr, img, subset=subset, layers=(b+1)
      if b eq 0 then gapimg = temporary(img) ne 0 else $
        gapimg = temporary(gapimg)*(img ne 0)
    endfor
    img = 0 ;save memory
    
    ;    zot_img, imgmatch, gaphdr, gapimg, subset=subset
    ;    gapimg = product(temporary(gapimg),3)
    ;    gapimg = temporary(gapimg) ne 0
    
    search = strcompress("*"+strmid(file_basename(imgmatch),0,18)+"*b6.bsq")
    thermal = file_search(file_dirname(imgmatch), search, count=n_thermal)
    if n_thermal eq 1 then usethisimg = thermal else usethisimg = imgmatch
    zot_img, usethisimg, depmaskhdr, maskimg, layers = 1, subset=subset
    
    maxsize = float(max([depmaskhdr.filesize[0],depmaskhdr.filesize[1]]))
    denom = float(800/maxsize)
    xsize = round(depmaskhdr.filesize[0]*denom)
    ysize = round(depmaskhdr.filesize[1]*denom)
    if keyword_set(ledaps) eq 1 then goods = intarr(depmaskhdr.filesize[0], depmaskhdr.filesize[1], 3) else $
      goods = bytarr(depmaskhdr.filesize[0], depmaskhdr.filesize[1], 3)
      
    print, ""
    print, ""
    print, ">>> follow console instructions to fix clouds masks for:"
    print, ">>> ", imgmatch
    print, ">>>"
    print, ">>> beginning routine to check for clouds"
    print, ">>> please wait..."
    
    zot_img, imgmatch, hdr, img321, layers = [3,2,1], subset=subset
    ;    for r=0, 2 do begin
    ;      zot_img, imgmatch, hdr, img321, layers = 3-r, subset=subset
    ;      goods[*,*,r] = img321
    ;    endfor
    ;img321 = reverse(hist_equal(congrid(goods, xsize, ysize, 3)),2) ; reverse,2 minv=10, maxv=175
    img321 = reverse(congrid(temporary(img321), xsize, ysize, 3),2) ; reverse,2
    if keyword_set(ledaps) eq 1 then img321 = hist_equal(bytscl(temporary(img321)), minv=1, maxv=175)
    
    reload1:
    title = file_basename(imgmatch)
    window, 0, xsize=800, ysize=800, title=title
    tvscl, img321, 0, true=3
    wait, 1
    repeat begin
      print, ">>>"
      print, ">>> are there clouds in this image?"
      print, ">>> NO (press 1), YES (press 2)"
      areclouds = ''
      read, areclouds, prompt = '>>> type answer:'
    endrep until areclouds eq '1' or areclouds eq '2' or areclouds eq 'reload'
    wdelete
    if areclouds eq 'reload' then goto, reload1
    
    if areclouds eq '2' then begin
      it1 = 0
      adj = 90
      start_here2:
      colors = [250,30,200]
      
      repeat begin
        for r=0, 2 do begin
          zot_img, imgmatch, hdr, img321m, layers = 3-r, subset=subset
          if n_thermal eq 1 then begin
            ;goods[*,*,r] = img321m*((maskimg ge adj)
            img321m[where(maskimg lt adj)] = colors[r]
            goods[*,*,r] = temporary(img321m)
          endif else begin
            ;goods[*,*,r] = img321m*(maskimg le adj)
            img321m[where(maskimg gt adj)] = colors[r]
            goods[*,*,r] = temporary(img321m)
          endelse
          
        endfor
        ;img321m = reverse(hist_equal(congrid(goods, xsize, ysize, 3),  minv=10, maxv=175),2) ;,  minv=10, maxv=175
        img321m = reverse(congrid(goods, xsize, ysize, 3),2) ;,  minv=10, maxv=175
        if keyword_set(ledaps) eq 1 then img321m = hist_equal(bytscl(temporary(img321m)), minv=1, maxv=175)
        
        reload2:
        for i=0, 5 do begin
          title = file_basename(imgmatch)
          if i eq 0 then window, 0, xsize=800, ysize=800, title=title;create a window to hold all of the regression tifs
          if i gt 0 then erase
          tv, img321, 0, true=3
          wait, .8
          ;window, 0, xsize=800, ysize=800, title=title;create a window to hold all of the regression tifs
          erase
          tv, img321m, 0, true=3
          wait, .8
        endfor
        
        repeat begin
          print, ">>>"
          print, ">>> are the clouds covered by black???"
          print, ">>>"
          print, ">>> increase mask (press 1)"
          print, ">>> decrease mask (press 2)"
          print, ">>> the mask is good (press 3)"
          print, ">>> there is smoke\haze that won't go away (press 4)"
          print, ">>> see full resolution image (press 5)
          status = ''
          read, status, prompt = '>>> type answer:'
          wdelete
          if status eq '5' then begin
            ;repeat begin
            it1 = it1 + 1
            delete = 0
            zot_img, imgmatch, hdr, newimg, layers = 1, subset=subset
            if n_thermal eq 1 then newimg = temporary(newimg)*(maskimg ge adj) else newimg = temporary(newimg)*(maskimg le adj)
            if it1 eq 1 then range1 = imclip(newimg[where(newimg gt 0)])
            newimg = reverse(bytscl(temporary(newimg), min=range1[0], max=range1[1]), 2)
            slide_image, temporary(newimg), xvisible=600, yvisible=600, /block
            status = ''
            read, status, prompt = '>>> type answer:'
          ;endrep until status ne '5'
          endif
        endrep until status eq '1' or status eq '2' or status eq '3' or status eq '4' or status eq 'reload'
        
        if status eq 'reload' then goto, reload2
        
        if status eq '1' then begin
          print, ">>>
          increase = ''
          print, ">>> current DN: ", adj
          read, increase, prompt = ">>> how many DN?"
          if n_thermal eq 1 then adj = adj+increase else adj = adj-increase
        endif
        if status eq '2' then begin
          print, ">>>
          print, ">>> current DN: ", adj
          decrease = ''
          read, decrease, prompt = ">>> how many DN?"
          if n_thermal eq 1 then adj = adj-decrease else adj = adj+decrease
        endif
        
        haze = 0 ;creat a "keyword"
        if status eq '4' then begin
          if n_thermal eq 1 then begin
            haze = 1 ;turn on this "keyword"
            zot_img, imgmatch, depmaskhdradded, maskimgadded, layers = [1], subset=subset ;load in band 1
            
            adj1 = 90
            it2 = 0
            repeat begin
              for r=0, 2 do begin
                zot_img, imgmatch, hdr, img321m, layers = 3-r, subset=subset
                img321m[where(maskimg lt adj)] = colors[r]
                img321m[where(maskimgadded ge adj1)] = colors[r]
                goods[*,*,r] = img321m
              endfor
              
              ;img321m = reverse(hist_equal(congrid(goods, xsize, ysize, 3),  minv=10, maxv=175),2) ;,  minv=10, maxv=175
              img321m = reverse(congrid(goods, xsize, ysize, 3),2)
              if keyword_set(ledaps) eq 1 then img321m = hist_equal(bytscl(temporary(img321m)), minv=1, maxv=175)
              
              reload3:
              for i=0, 5 do begin
                title = file_basename(imgmatch)
                if i eq 0 then window, 0, xsize=800, ysize=800, title=title;create a window to hold all of the regression tifs
                if i gt 0 then erase
                tv, img321, 0, true=3
                wait, .8
                ;window, 0, xsize=800, ysize=800, title=title;create a window to hold all of the regression tifs
                erase
                tv, img321m, 0, true=3
                wait, .8
              endfor
              
              repeat begin
                print, ">>>"
                print, ">>> is the smoke\haze covered by black???"
                print, ">>>"
                print, ">>> increase mask (press 1)"
                print, ">>> decrease mask (press 2)"
                print, ">>> the mask is good (press 3)"
                print, ">>> see full resolution image (press 4)
                status = ''
                read, status, prompt = '>>> type answer:'
                wdelete
                if status eq '4' then begin
                  ;repeat begin
                  it2=it2+1
                  zot_img, imgmatch, hdr, newimg, layers = 1, subset=subset
                  newimg = temporary(newimg)*(((maskimg gt adj)*(maskimgadded lt adj1)) ne 0)
                  if it2 eq 1 then range2 = imclip(newimg[where(newimg gt 0)])
                  newimg = reverse(bytscl(temporary(newimg), min=range2[0], max=range2[1]), 2)
                  slide_image, temporary(newimg), xvisible=600, yvisible=600, /block
                  status = ''
                  read, status, prompt = '>>> type answer:'
                ;endrep until status ne '5'
                endif
              endrep until status eq '1' or status eq '2' or status eq '3' or status eq 'reload'
              
              if status eq 'reload' then goto, reload3
              
              if status eq '1' then begin
                print, ">>> "
                increase = ''
                print, ">>> current DN: ", adj1
                read, increase, prompt = ">>> how many DN?"
                adj1 = adj1-increase
              endif
              if status eq '2' then begin
                print, ">>>
                print, ">>> current DN: ", adj1
                decrease = ''
                read, decrease, prompt = ">>> how many DN?"
                adj1 = adj1+decrease
              endif
            endrep until status eq '3'
            smkhzadj = adj1
          endif else begin ;if thermal eq 1
            print, ""
            print, ">>> sorry, there is no thermal band..."
            print, ">>> so you're already working with band 1..."
            print, ">>> which is the best we can do for getting..."
            print, ">>> rid of smoke and haze.  try increasing..."
            print, ">>> the mask some more, if you end up losing..."
            print, ">>> too many good pixels, consider dropping..."
            print, ">>> the date and/or filling in the year..."
            print, ">>> with other dates..."
            print, ""
            print, ">>> what do you want to do?"
            print, ">>> move on (press 1) or increase the mask (press 2)
            answer = ''
            read, answer, prompt = '>>> type answer:'
            if answer eq '2' then goto, start_here2 else status = '3'
          endelse ;if thermal eq 1
        endif ;status eq '4'
      endrep until status eq '3'
      cloudadj = adj
    endif
    
    ;-------------now do the cloud shadows-------------------
    img321 = 0  ;save memory
    img321m = 0  ;save memory
    
    print, ">>>"
    print, ">>> beginning routine to check for cloud shadows"
    print, ">>> please wait..."
    zot_img, imgmatch, depmaskhdr, maskimg, layers = [4], subset=subset
    for r=0, 2 do begin
      zot_img, imgmatch, hdr, img543, layers = 5-r, subset=subset
      goods[*,*,r] = img543
    endfor
    
    ;img543 = reverse(hist_equal(congrid(goods, xsize, ysize, 3),  minv=10, maxv=175),2) ;,  minv=10, maxv=175
    img543 = reverse(congrid(goods, xsize, ysize, 3),2)
    if keyword_set(ledaps) eq 1 then img543 = hist_equal(bytscl(temporary(img543)), minv=1, maxv=175)
    
    reload4:
    title = file_basename(imgmatch)
    window, 0, xsize=800, ysize=800, title=title
    tv, img543, 0, true=3
    wait, 1
    repeat begin
      print, ">>>"
      print, ">>> are there cloud shadows in this image?"
      print, ">>> NO (press 1), YES (press 2)"
      areshadows = ''
      read, areshadows, prompt = '>>> type answer:'
    endrep until areshadows eq '1' or areshadows eq '2' or areshadows eq 'reload'
    wdelete
    if areshadows eq 'reload' then goto, reload4
    
    ;colors1 = [24,95,252] ;blue
    colors1 = [255,255,0] ;yellow
    if areshadows eq '2' then begin
      adj = 15
      it3=0
      repeat begin
        for r=0, 2 do begin
          zot_img, imgmatch, hdr, img543m, layers = 5-r, subset=subset
          img543m[where(maskimg le adj)] = colors1[r]
          goods[*,*,r] = img543m
        endfor
        
        ;img543m = reverse(hist_equal(congrid(goods, xsize, ysize, 3),  minv=10, maxv=175),2) ;,  minv=10, maxv=175
        img543m = reverse(congrid(goods, xsize, ysize, 3),2)
        if keyword_set(ledaps) eq 1 then img543m = hist_equal(bytscl(temporary(img543m)), minv=1, maxv=175)
        
        reload5:
        for i=0, 5 do begin
          title = file_basename(imgmatch)
          if i eq 0 then window, 0, xsize=800, ysize=800, title=title;create a window to hold all of the regression tifs
          if i gt 0 then erase
          tv, img543, 0, true=3
          ;tvscl
          wait, .8
          ;window, 0, xsize=800, ysize=800, title=title;create a window to hold all of the regression tifs
          erase
          tv, img543m, 0, true=3, top=100
          wait, .8
        endfor
        
        repeat begin
        
          print, ">>>"
          print, ">>> are the cloud shadows covered by white???"
          print, ">>>"
          print, ">>> increase mask (press 1)"
          print, ">>> decrease mask (press 2)"
          print, ">>> the mask is good (press 3)"
          print, ">>> see full resolution image (press 4)
          status = ''
          read, status, prompt = '>>> type answer:'
          wdelete
          if status eq '4' then begin
            ;repeat begin
            it3=it3+1
            zot_img, imgmatch, hdr, newimg, layers = 4, subset=subset
            newimg[where(maskimg le adj)] = 255
            if it3 eq 1 then range3 = imclip(newimg[where(newimg gt 0 and newimg lt 255)], percent=percent)
            newimg = reverse(bytscl(temporary(newimg), min=range3[0], max=range3[1]), 2)
            slide_image, temporary(newimg), xvisible=600, yvisible=600, /block
            status = ''
            read, status, prompt = '>>> type answer:'
          ;endrep until status ne '5'
          endif
        endrep until status eq '1' or status eq '2' or status eq '3' or status eq 'reload'
        ;figure out what to do with the status
        if status eq 'reload' then goto, reload5
        if status eq '1' then begin
          print, ">>>
          increase = ''
          print, ">>> current DN: ", adj
          read, increase, prompt = ">>> how many DN?"
          adj = adj+increase
        endif
        if status eq '2' then begin
          print, ">>>
          print, ">>> current DN: ", adj
          decrease = ''
          read, decrease, prompt = ">>> how many DN?"
          adj = adj-decrease
        endif
      endrep until status eq '3'
      shadowadj = adj
      
    endif ;are there cloud shadows
    img543m = 0 ;save memory
    
    ;----------------display the final mask-----------------------
    
    zot_img, usethisimg, depmaskhdr, maskimg, layers = [1], subset=subset
    
    if n_thermal eq 1 then begin
      if areclouds eq 1 then cloudadj = -9999 ;adjust for no threshold given
      clouds = maskimg ge cloudadj ;get the clouds to be 0 and all else 1
    endif else begin
      if areclouds eq 1 then cloudadj = 9999 ;adjust for no threshold given
      clouds = maskimg le cloudadj ;get the clouds to be 0 and all else 1
    endelse
    
    zot_img, imgmatch, depmaskhdr, maskimg, layers = [4], subset=subset
    if areshadows eq 1 then shadowadj = -9999 ;adjust for no threshold given
    shadows = maskimg ge shadowadj
    
    ;zot_img, vctcomarea, hdr, img, subset=subset
    if areclouds eq 1 then haze = 0
    if haze eq 1 then begin
      zot_img, imgmatch, depmaskhdr, maskimg, layers = [1], subset=subset
      smkhz = maskimg le smkhzadj
      finalcloudmask = clouds*shadows*smkhz
      smkhz = 0
    endif else finalcloudmask = clouds*shadows
    
    shadows = 0
    clouds = 0
    maskimg = 0 ;save memory
    
    finalcloudmask = finalcloudmask*gapimg
    gapimg = 0
    
    ;do the masked image
    for r=0, 2 do begin
      zot_img, imgmatch, hdr, img543m, layers = 5-r, subset=subset
      ;img543m = finalcloudmask * img543m
      if r eq 0 then img543m[where(finalcloudmask eq 0)] = 255
      if r eq 1 then img543m[where(finalcloudmask eq 0)] = 255
      if r eq 2 then img543m[where(finalcloudmask eq 0)] = 0
      goods[*,*,r] = img543m
    endfor
    ;img2 = goods
    img543m = reverse(congrid(goods, xsize, ysize, 3),2) ;,
    if keyword_set(ledaps) eq 1 then img543m = hist_equal(bytscl(temporary(img543m)), minv=1, maxv=175)
    
    goods = 0 ;free memory
    
    ;display the images
    reload6:
    
    for i=0, 5 do begin
      title = file_basename(imgmatch)
      if i eq 0 then window, 0, xsize=800, ysize=800, title=title;create a window to hold all of the regression tifs
      if i gt 0 then erase
      tv, img543, 0, true=3
      ;tvscl
      wait, .8
      ;window, 0, xsize=800, ysize=800, title=title;create a window to hold all of the regression tifs
      erase
      tv, img543m, 0, true=3, top=100
      wait, .8
    endfor
    ;    title = file_basename(imgmatch)
    ;    window, 0, xsize=1280, ysize=640, title=title
    ;    tv, img543, 0, true=3
    ;    tv, img543m, 1, true=3
    ;    wait, 1
    repeat begin
      print, ">>>"
      print, ">>> the yellow is the final mask"
      print, ">>> is it OK???"
      print, ">>>"
      print, ">>> NO (press 1)"
      print, ">>> YES (press 2)"
      ;print, ">>> see full resolution image (press 3)
      status = ''
      read, status, prompt = '>>> type answer:'
      wdelete
    ;      if status eq '3' then begin
    ;        ;repeat begin
    ;        zot_img, imgmatch, hdr, newimg, layers = 1, subset=subset
    ;        newimg[where(finalcloudmask eq 0)] = 255
    ;        range3 = imclip(newimg[where(newimg gt 0)], percent=percent)
    ;        newimg = reverse(temporary(newimg), 2)
    ;        ;newimg = reverse(bytscl(temporary(newimg), min=range3[0], max=range3[1]), 2)
    ;        slide_image, temporary(newimg), xvisible=600, yvisible=600, /block
    ;        status = ''
    ;        read, status, prompt = '>>> type answer:'
    ;      ;endrep until status ne '5'
    ;      endif
      
    endrep until status eq '1' or status eq '2' or status eq 'reload'
    if status eq 'reload' then goto, reload6
    
    img543 = 0 ;free memory
    img543m = 0 ;free memory
    
    if status eq '1' then begin
      print, ">>> starting over"
      goto, start_here1
    endif
    if status eq '2' then begin
      print, ">>>"
      print, ">>> writing image mask, please wait..."
      print, ""
      print, ""
    endif
    
    
    finalcloudmask = finalcloudmask eq 0 ;get the cloud to be the value of 1 so we can add a buffer
    
    radius = 1
    kernel = SHIFT(DIST(2*radius+1), radius, radius) LE radius
    finalcloudmask = convol(finalcloudmask, kernel) gt 0
    finalcloudmask = convol(finalcloudmask, kernel) gt 0
    ;finalcloudmask = fix(finalcloudmask)
    
    finalcloudmask = finalcloudmask eq 0
    
    lt_delete_duplicate, imgmatch, /cloudmask
    
    filedir = file_dirname(imgmatch)+"\"
    
    if keyword_set(ledaps) eq 1 then outfile = strcompress(filedir+strmid(file_basename(imgmatch), 0, 18)+ $
      "_"+timestamp()+"_ledaps_radref_cloudmask.bsq", /rem) else $
      outfile = strcompress(filedir+strmid(file_basename(imgmatch), 0, 18)+ $
      "_"+timestamp()+"_cloudmask.bsq", /rem)
      
    ;write out the file
    openw, un, outfile, /get_lun
    writeu, un, finalcloudmask
    free_lun, un
    hdr.pixeltype = 3
    hdr.n_layers = 1
    write_im_hdr, outfile, hdr
    
    ;create the metadata structure
    if keyword_set(ledaps) ne 1 then begin
      if haze eq 1 then smokeadj = smkhzadj else smokeadj = -9999
      if n_thermal eq 1 then usethermal = 1 else usethermal = 0
      cldmaskthresh = [usethermal,cloudadj,shadowadj,smokeadj]
      metaout = stringswap(outfile, ".bsq", "_meta.txt")
      meta = make_metadata_for_preprocessing(outfile, cldmaskthresh=cldmaskthresh, cldmskversion=1)
      concatenate_metadata, imgmatch, metaout, params=meta
    endif
  endfor
end