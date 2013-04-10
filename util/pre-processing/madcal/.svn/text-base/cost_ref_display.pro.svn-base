
function cost_ref_display, refpicks, sorted, title, targetyear, medjulday

  print, "preparing selected ref images for viewing..."
  picks = replicate(create_struct("f","","doy","","year",""),4)
  for i=0,3 do begin
    print, "  ..."
    picks[i].f = strcompress(refpicks[where(sorted eq i)].file, /rem)
    file = strcompress(file_basename(picks[i].f), /rem)
    picks[i].year = strcompress(strmid(file, 10, 4), /rem)
    picks[i].doy = strcompress(strmid(file, 15, 3), /rem)
    zot_img, picks[i].f, hdr, img, layers = [3,2,1]
    xsize = round(hdr.filesize[0]/20)
    ysize = round(hdr.filesize[1]/20)
    if i eq 0 then img1 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
    if i eq 1 then img2 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
    if i eq 2 then img3 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
    if i eq 3 then img4 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
  endfor
  ;
  ;  pickedfile1 = refpicks[where(sorted eq 0)].file
  ;  pickedfile2 = refpicks[where(sorted eq 1)].file
  ;  pickedfile3 = refpicks[where(sorted eq 2)].file
  ;  pickedfile4 = refpicks[where(sorted eq 3)].file
  ;
  ;
  ;  zot_img, pickedfile1, hdr, img, layers = [3,2,1]
  ;  xsize = round(hdr.filesize[0]/20)
  ;  ysize = round(hdr.filesize[1]/20)
  ;  img1 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
  ;  zot_img, pickedfile2, hdr, img, layers = [3,2,1]
  ;  xsize = round(hdr.filesize[0]/20)
  ;  ysize = round(hdr.filesize[1]/20)
  ;  img1 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
  ;  zot_img, pickedfile1, hdr, img, layers = [3,2,1]
  ;  xsize = round(hdr.filesize[0]/20)
  ;  ysize = round(hdr.filesize[1]/20)
  ;  img1 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
  ;  zot_img, pickedfile2, hdr, img, layers = [3,2,1]
  ;  xsize = round(hdr.filesize[0]/20)
  ;  ysize = round(hdr.filesize[1]/20)
  ;  img1 = reverse(hist_equal(congrid(img, xsize, ysize, 3), minv=10, maxv=175),2)
  title = strcompress("target year: "+string(targetyear)+" :: target day-of-year: "+string(medjulday)+" :: "+$
    "top>bottom left>right... 1: "+picks[0].year+" "+picks[0].doy+" :: 2: "+picks[1].year+" "+picks[1].doy+" :: 3: "+$
    picks[2].year+" "+picks[2].doy+" :: 4: "+picks[3].year+" "+picks[3].doy)
    
  print, ">>> which image is the best: 1, 2, 3, or 4?"
  print, ">>> this order is the algorithm's top 4 picks"
  print, ">>> the window title describes the positions"
  window, xsize=900, ysize=900, title=title;create a window to hold all of the regression tifs
  device, decomposed=1
  tv, img1, 0, true=3
  tv, img2, 1, true=3
  tv, img3, 2, true=3
  tv, img4, 3, true=3
  
  
  repeat begin
    print, ">>> which image is the best: 1, 2, 3, or 4?"
    print, ">>> this order is the algorithm's top 4 picks"
    print, ">>> the window title describes the positions"
    window, xsize=900, ysize=900, title=title;create a window to hold all of the regression tifs
    device, decomposed=1
    tv, img1, 0, true=3
    tv, img2, 1, true=3
    tv, img3, 2, true=3
    tv, img4, 3, true=3
    wait,30
    wdelete
    print, "do you need more time?
    print, "type: y or n"
    therefimg = get_kbrd()
    if therefimg eq 'y' then dothis = 0
    if therefimg eq 'n' then dothis = 1
  endrep until dothis eq 1
device, decomposed=0
return, picks
end