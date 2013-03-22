
pro zonal_summary, target_file, zone_file, output_file, trange=trange, zrange=zrange

  zot_img, target_file, t_hdr, t_dat, /hdronly
  zot_img, zone_file, z_hdr, z_dat, /hdronly
  
  ;find common area
  ul = t_hdr.upperleftcenter
  lw = t_hdr.lowerrightcenter
  
  if z_hdr.upperleftcenter[0] gt ul[0] then ul[0] = z_hdr.upperleftcenter[0]
  if z_hdr.upperleftcenter[1] lt ul[0] then ul[0] = z_hdr.upperleftcenter[1]
  if z_hdr.lowerrightcenter[0] lt lw[0] then lw[0] = z_hdr.lowerrightcenter[0]
  if z_hdr.lowerrightcenter[1] gt lw[1] then lw[1] = z_hdr.lowerrightcenter[1]
  
  constraints = [[ul], [lw]]
  
  adj_constraints = constraints
  adj_constraints[*,0] = adj_int_mult(t_hdr.upperleftcenter, t_hdr.pixelsize, constraints[*,0], /map)
  adj_constraints[*,1] = adj_int_mult(t_hdr.upperleftcenter, t_hdr.pixelsize, constraints[*,1], /map)
  
  ;first layer is the year
  zot_img, target_file, target_hdr, t_dat, layers=[1], subset=adj_constraints
  
  adj_constraints = constraints
  adj_constraints[*,0] = adj_int_mult(z_hdr.upperleftcenter, z_hdr.pixelsize, constraints[*,0], /map)
  adj_constraints[*,1] = adj_int_mult(z_hdr.upperleftcenter, z_hdr.pixelsize, constraints[*,1], /map)
  zot_img, zone_file, zone_hdr, z_dat, layers=[1], subset=adj_constraints
  
  if keyword_set(zrange) then begin
    z_min = zrange[0]
    z_max = zrange[1]
  endif else begin
    zs = fast_unique(z_dat)  
    zi = where(zs ne 0)
    z_min = min(zs[zi])
    z_max = max(zs[zi])
  endelse
  
  if keyword_set(trange) then begin
    t_min = trange[0]
    t_max = trange[1]
  endif else begin
    ts = fast_unique(t_dat)
    ti = where(ts ne 0)
    t_min = min(ts[ti])
    t_max = max(ts[ti])
  endelse
  
  format = strcompress('('+string(z_max-z_min+1)+'(I,","))', /re)
  r = hist_2d(z_dat, t_dat, bin1=1, bin2=1, min1=z_min, max1=z_max, min2=t_min, max2=t_max)
  openw, fun, output_file, /get_lun
  printf, fun, r, format=format
  free_lun, fun
end



