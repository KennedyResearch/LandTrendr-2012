pro min_dist_to_clouds, path, fix_date




mask_file = file_search(path+"VCT\outputs\", "*mask.bsq", n_mask_files)
if n_mask_files eq 1 then img_file = stringswap(mask_files, ".bsq", "")


zot_img, mask_file, mskhdr, mskimg, /hdronly
zot_img, img_file, imghdr, imgimg, /hdronly

ulx = max([mskhdr.upperleftcenter[0], imghdr.upperleftcenter[0]])
uly = min([mskhdr.upperleftcenter[1], imghdr.upperleftcenter[1]])
lrx = min([mskhdr.lowerrightcenter[0], imghdr.lowerrightcenter[0]])
lry = max([mskhdr.lowerrightcenter[1], imghdr.lowerrightcenter[1]])

mastersubset = [[ulx,uly],[lrx,lry]]

subset=mastersubset
zot_img, mask_file, mskhdr, mskimg, subset=subset

subset=mastersubset
zot_img, img_file, imghdr, imgimg, subset=subset

clouds = where(mskimg eq 4 or maskimg eq 5, n_clouds)
if n_clouds ge 1 then begin
 for i=0, 7 do begin
  subset=mastersubset
  zot_img, img_file, imghdr, imgimg, subset=subset, layers=i
  if i eq 0 then means = round(mean(imgimg[clouds]))
  means = [means, round(mean(imgimg[clouds])]
endif


means = bytarr(imghdr.filesize[0], imghdr.filesize[1], imghdr.n_layers)
for i=0, 7 do begin means[*,*,i] = means[i]

subset=mastersubset
zot_img, img_file, imghdr, imgimg, subset=subset
distance = sqrt(((imgimg-means)^2))
distance = total(distance,3)
  







;;create the
;;find the vct mask files
;mask_files = file_search(path+\"VCT\outputs\", "*mask.bsq", n_mask_files)
;image_files =
;
;
;
;zot_img, image_file, hdr, img
;
;struct = {meanb1:0.0,meanb2:0.0,meanb3:0.0,meanb4:0.0,meanb5:0.0,meanb6:0.0,meanb7:0.0 $
;          stdevb1:0.0,stdevb2:0.0,stdevb3:0.0,stdevb4:0.0,stdevb5:0.0,stdevb6:0.0,stdevb7:0.0}
;
;for i=0, n_mask_files do begin
;  for b=0, 6 do begin
;    zot_img, image_file, hdr, img, layers=b
;
;    case b of
;      0: do begin
;          struct[i].meanb1 = mean(img)
;          struct[i].stdevb1 = stdev(img)
;        end
;      1: do begin
;          struct[i].meanb2 = mean(img)
;          struct[i].stdevb2 = stdev(img)
;        end
;      2: do begin
;          struct[i].meanb3 = mean(img)
;          struct[i].stdevb3 = stdev(img)
;        end
;      3: do begin
;          struct[i].meanb4 = mean(img)
;          struct[i].stdevb4 = stdev(img)
;        end
;      4: do begin
;          struct[i].meanb5 = mean(img)
;          struct[i].stdevb5 = stdev(img)
;        end
;      5: do begin
;          struct[i].meanb6 = mean(img)
;          struct[i].stdevb6 = stdev(img)
;        end
;      6: do begin
;          struct[i].meanb7 = mean(img)
;          struct[i].stdevb7 = stdev(img)
;        end
;    endcase
;
;
;  endfor
;  struct_final = {meanb1:mean(struct.meanb1),meanb2:mean(struct.meanb2),meanb3:mean(struct.meanb3), $
;                  meanb4:mean(struct.meanb4),meanb5:mean(struct.meanb5),meanb6:mean(struct.meanb6),meanb7:mean(struct.meanb7) $
;                  stdevb1:mean(struct.stdevb1),stdevb2:mean(struct.stdevb1),stdevb3:mean(struct.stdevb1), $
;                  stdevb4:mean(struct.stdevb1),stdevb5:0.0,stdevb6:0.0,stdevb7:0.0}
;
;endfor



