function get_dominant_axis_info, xys

;xys is the [2,n] array of n edge pixels. 
;this returns the length of the dominant axis and minor axis

sz = size(xys, /dim)
if n_elements(sz) ne 2 then begin
    print, 'dominant_axis_info: must have at least 2 points!'
    return, -1
    end
if sz[0] ne 2 then begin
    print, 'dominant_axis_info: coords must be of form [2,n]'
    return, -1
    end
n_points = sz[1]

mnx = mean(xys[0,*])
mny = mean(xys[1,*])
p = [xys-fill_arr([mnx, mny], n_points)]

;need to set up in case of a straight line
catch, error_status
if error_status ne 0 then begin
   ;so far, the only error we've seen is where it's straight line.  thus just need to calculate values
   ;under that assumption
   ;r= fltarr(2, n_points)
   ;distance = sqrt( (xys[0,*]-replicate(min(xys[0,*]),n_points))^2+$
	;	    (xys[1,*]-replicate(min(xys[1,*]),n_points))^2)
   ;r[0,*] = distance
   ;r[1,*] = 1
  mid = n_points/2
  p[0, mid] = p[0,mid]-1	;slightly tweak one so it's not singular 

  catch, /cancel
end

r = pcomp(p, variances =v)


;to get the length of the dominant axis, compare min and max of the pc
;

dominant_axis_length = max(r[0,*])-min(r[0,*])
normal_axis_length = max(r[1,*])-min(r[1,*])

ratio = dominant_axis_length / normal_axis_length

return, {dominant_axis_length: dominant_axis_length, $
          normal_axis_length: normal_axis_length, $
          ratio_dominant_to_normal: ratio, $
          variance_explained_by_dominant: v[0]}
          
end
