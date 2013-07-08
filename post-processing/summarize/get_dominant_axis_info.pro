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
