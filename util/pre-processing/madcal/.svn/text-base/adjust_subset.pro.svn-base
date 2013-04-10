function adjust_subset, subset_coords, image_file
  
  
  ulx = subset_coords[0,0]
  uly = subset_coords[1,0]
  lrx = subset_coords[0,1]
  lry = subset_coords[1,1]
  
  rows = floor(uly-lry)/30
  columns = floor(lrx-ulx)/30
  
  ;!!!!!assumes 30 meter resolution!!!!!
  ;todo - have program check on the resolution
  meters_x = columns*30
  meters_y = rows*30
   
  zot_img, image_file, hdr, img, subset=[[ulx,uly],[lrx,lry]], /hdronly
  
  ulx = hdr.upperleftcenter[0]
  uly = hdr.upperleftcenter[1]
  
  lrx = ulx+meters_x
  lry = uly-meters_y
   
  new_subset = [[ulx,uly],[lrx,lry]]
  return, {subset:new_subset}
end