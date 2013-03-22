
;checkthesecoords = [[ulx,uly],[lrx,lry]]
;againstthesecoords = [[ulx,uly],[lrx,lry]]
function gdal_check_extent, checkthesecoords=checkthesecoords, againstthesecoords=againstthesecoords, $
  checkthisfileextent=checkthisfileextent, againstthisfileextent=againstthisfileextent
  
  if keyword_set(checkthisfileextent) eq 1 then begin
    hdrcheck = get_subset(imagefile=checkthisfileextent)
    check_ulx = hdrcheck[0]
    check_uly = hdrcheck[1]
    check_lrx = hdrcheck[2]
    check_lry = hdrcheck[3]
  endif else begin
    check_ulx = checkthesecoords[0,0]
    check_uly = checkthesecoords[1,0]
    check_lrx = checkthesecoords[0,1]
    check_lry = checkthesecoords[1,1]
  endelse
  if keyword_set(againstthisfileextent) eq 1 then begin
    trcoords=get_subset(imagefile=againstthisfileextent)
    targetulx = trcoords[0]
    targetuly = trcoords[1]
    targetlrx = trcoords[2]
    targetlry = trcoords[3]
  endif else begin
    targetulx = againstthesecoords[0,0]
    targetuly = againstthesecoords[1,0]
    targetlrx = againstthesecoords[0,1]
    targetlry = againstthesecoords[1,1]
  endelse
  
  ;todo deal with whether the coords are from center of ul - have the 90 buffer in there for now
  toosmall = total([check_ulx gt targetulx+90 ,$
               check_uly lt targetuly-90 ,$
               check_lrx lt targetlrx-90 ,$
               check_lry gt targetlry+90]) gt 0
   
  return, {toosmall:toosmall, trcoords:[[targetulx,targetuly],[targetlrx,targetlry]], $
           chkcoords:[[check_ulx,check_uly],[check_lrx,check_lry]]} 
end