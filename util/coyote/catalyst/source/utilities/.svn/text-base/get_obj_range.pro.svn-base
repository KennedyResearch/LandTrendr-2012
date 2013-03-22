;$Id: //depot/idl/IDL_64/idldir/lib/utilities/get_obj_range.pro#1 $
;
;  Copyright (c) 1997-2007, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.
;
function get_obj_range, $
  oObj, $           ; IN: object to find boundaries of.
  oParent, $        ; IN: is PATH for GetTextDimensions, if needed.
  oDestDevice, $    ; IN: supplies GetTextDimensions() if needed.
  Range, $          ; OUT: [3,2] coordinates of bounding box coords
  ignore=ignore, $  ; IN (opt): Default: 'IDLgrLight'.
  skip=skip         ; IN (opt): non-recursive ignore.
compile_opt hidden

if obj_valid(oObj) eq 0 then return, 0

_ignore = n_elements(ignore) eq 0 ? 'IDLgrLight' : ignore

for i=0,n_elements(_ignore)-1 do begin
    if obj_isa(oObj, _ignore[i]) then return, 0
endfor

for i=0,n_elements(skip)-1 do begin
    if obj_isa(oObj, skip[i]) then return, 0
endfor

if obj_isa(oObj, 'IDLgrModel') then begin
;
;       Find the boundary of oObj's children.
;
    oChildArr = oObj->Get(/all, count=count)
    gotBox = 0L
    path = [oParent, oObj]
    for ichild=0L, count-1 do begin ;Get each box and accumulate bounding box
      skipping = 0b
      for i=0,n_elements(skip)-1 do begin
        if obj_isa(oChildArr[ichild], skip[i]) then begin
          skipping = 1b
          break
        endif
      endfor
      if not skipping then begin
        if get_obj_range(oChildArr[ichild], path, $
                         oDestDevice, child_range, ignore=ignore) then begin
          if gotBox then range = [[range[0:2] < child_range[0:2]], $
                                  [range[3:5] > child_range[3:5]]] $
          else range = child_range
          gotBox = 1
        endif
      endif
    endfor
    if gotBox then begin        ;Got a box?  Transform it..
        oObj->GetProperty, TRANSFORM=model_tm ;Transform it and get range
        p1 = fltarr(4,8)
        for i=0, 7 do p1[0,i] = [range[0, i and 1],     $
                                 range[1, (i/2) and 1], $
                                 range[2, (i/4) and 1], $
                                 1.0 ]
        p1 = MATRIX_MULTIPLY(p1, model_tm, /ATRANSPOSE)
        range = [[min(p1[*,0], max=xmax), min(p1[*,1], max=ymax), $
                  min(p1[*,2], max=zmax)], $
                 [xmax, ymax, zmax]]
    endif                       ;if gotBox
    return, gotBox

endif else if obj_isa(oObj, 'IDLgrGraphic') then begin


    if obj_isa(oObj, 'IDLgrText') then begin
;
;           GetTexTDimensions is called for this side effect: oObj's
;           xrange, yrange & zrange are updated.
;
        if obj_valid(oDestDevice) then begin
            if obj_valid(oParent[0]) then $
              void = oDestDevice->GetTextDimensions(oObj, path=[oParent]) $
            else void = oDestDevice->GetTextDimensions(oObj)
        endif
    endif

    oObj->GetProperty, xrange=xrange, yrange=yrange, zrange=zrange, $
      xcoord_conv=xcc, ycoord_conv=ycc, zcoord_conv=zcc
                                      ; Scale and save extrema
    Range = [[min(xrange * xcc[1] + xcc[0], max=xmax), $
              min(yrange * ycc[1] + ycc[0], max=ymax), $
              min(zrange * zcc[1] + zcc[0], max=zmax)], $
             [xmax, ymax, zmax]]
;
;   IDLgrContour objects can have NaN ranges if their properties
;   yield no contours (e.g. DATA_VALUES are all 0, or C_VALUE
;   is [-1] when DATA_VALUES are positive).
;
    if min(finite(range)) lt 1 then $
        return, 0
    return, 1
endif else begin                ;Dunno what it is
    return, 0
endelse
end


