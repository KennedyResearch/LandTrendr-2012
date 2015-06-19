function make_mosaic_patchids, patchfile_csv, select_plots_csv

ok = read_generic_csv(patchfile_csv)
patchinfo = ok.vals


ok = read_generic_csv(select_plots_csv)
sp = ok.vals


;get outputimage 

outfile = stringswap(select_plots_csv, '.csv', '.bsq')


;go through the patchfiles and check that they are all there
;  pluse determine the bounds

bounds = [[0.d, 0.d], [0.d, 0.d]]


n_files = n_elements(patchinfo)
for i = 0 , n_files-1 do begin
  zot_img, patchinfo[i].patch_file, hdr, img, /hdronly
  if i eq 0 then bounds = [[hdr.upperleftcenter], [hdr.lowerrightcenter]] else $
     bounds = [[min([bounds[0,0], hdr.upperleftcenter[0]]), $
		max([bounds[1,0], hdr.upperleftcenter[1]])], $
		[max([bounds[0,1], hdr.lowerrightcenter[0]]), $
		min([bounds[1,1], hdr.lowerrightcenter[1]])]]
end

;get size

 xsize =long( ((bounds[0,1]-bounds[0,0]) / hdr.pixelsize[0])+1)
 ysize =long( ((bounds[1,0]-bounds[1,1]) / hdr.pixelsize[1])+1)

;make blank
 

 image = bytarr(xsize, ysize)

;fill er up

for i =0, n_files-1 do begin
   zot_img, patchinfo[i].patch_file, hdr, img, layers =[1]
   sz = size(img, /dim)
   workingimage=img-img

;need to find the right patches.  

;  h1 = histogram(img, min=1, rev=r)

;which of the selected are in this tsa?
  matches = where(sp.tsa eq patchinfo[i].tsa, many)
  if many eq 0 then print, 'No patches found for TSA '+patchinfo[i].tsa else begin

     for j = 0,many-1 do begin 
     	thisone = sp[matches[j]].plot_id
	;img[r[r[thisone-1]:r[thisone] 
	these = where(img eq thisone, many2)
	workingimage[these]=thisone
     endfor	;identifying through patches

  ;pop out the middle
 	;maskoutinterior = 1-erode(workingimage, intarr(3,3)+1)	;only interiors wil have 0's
	img = byte(workingimage ne 0);  set to 0,1 for easier conversion later

  

  ;find location of this image within larger file
    xoffset = long(hdr.upperleftcenter[0]-bounds[0,0])/hdr.pixelsize[0]
    yoffset = long(bounds[1,0]-hdr.upperleftcenter[1])/hdr.pixelsize[1]
   
   image[xoffset:xoffset+sz[0]-1, yoffset:yoffset+sz[1]-1] = img + image[xoffset:xoffset+sz[0]-1, yoffset:yoffset+sz[1]-1]  ;if img hsa more, no problme. 
   image=image gt 0	;sometimes we get 2's if the same patch is selected
			;  so we have to reset to 1 here


   endelse ;if matches were found

end  ;n_files

openw, un, outfile,/get_lun
writeu, un, image
free_lun, un

hdr1=hdr
hdr1.pixeltype=3
hdr1.filesize = [xsize,ysize]
hdr1.upperleftcenter = bounds[*,0]
hdr1.lowerrightcenter = bounds[*,1]

write_im_hdr, outfile, hdr1


return, outfile
end






 
