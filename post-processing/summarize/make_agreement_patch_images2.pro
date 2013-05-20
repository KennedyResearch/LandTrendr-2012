function make_agreement_patch_images2, patch_id_image1, patch_id_image2, disturbance_image1, $
    disturbance_image2, outputfile_base, subset=subset

    
;FIRST DEAL WITH IDENTIFYING OVERLAP AREA, ALSO IN RELATION TO SUBSET PASSED BY USER
    
  zot_img, patch_id_image1, p1_hdr, p1_dat, /hdronly
  zot_img, patch_id_image2, p2_hdr, p2_dat, /hdronly
  
  ;find common area
  ul = p1_hdr.upperleftcenter
  lw = p1_hdr.lowerrightcenter
  
  if p2_hdr.upperleftcenter[0] gt ul[0] then ul[0] = p2_hdr.upperleftcenter[0]
  if p2_hdr.upperleftcenter[1] lt ul[0] then ul[0] = p2_hdr.upperleftcenter[1]
  if p2_hdr.lowerrightcenter[0] lt lw[0] then lw[0] = p2_hdr.lowerrightcenter[0]
  if p2_hdr.lowerrightcenter[1] gt lw[1] then lw[1] = p2_hdr.lowerrightcenter[1]
  
  constraints = [[ul], [lw]]
  
  ;if user passes a subset, check it out here
  
  if n_elements(subset) ne 0 then begin
      ok = fix_edge(subset, constraints, /map)
      constraints = ok.coords
  end
  
  
  adj_constraints = constraints
  adj_constraints[*,0] = adj_int_mult(p1_hdr.upperleftcenter, p1_hdr.pixelsize, constraints[*,0], /map)
  adj_constraints[*,1] = adj_int_mult(p1_hdr.upperleftcenter, p1_hdr.pixelsize, constraints[*,1], /map)
  
  constraints = adj_constraints 

;THEN START GOING THROUGH THE FILES. 
;  Basic premise: Look at each patch ID in the first image
;      find the year of the disturbance associate with each patch.
;      determine how many pixels in the other image agree in terms of year
;      note all of the patch ids in that other image that are in that category
;      
;      
;      

;First, look at the patch ids of each image
  sub = constraints
  print, 'loading patch id image 1'
  zot_img, patch_id_image1, p1_hdr, p1_dat, subset=sub
  ;mn_p1 = min(p1_dat[where(p1_dat gt 0)])
  ;h_p1 = histogram(p1_dat, min=mn_p1, omin=omin_p1, omax=omax_p1, rev=rev_p1)
  
 
  ;stop
  print, 'starting fastunique'
  
  p1_ids = fast_unique_h(p1_dat, ignore=0)
  print, 'past'
  p1_ids = p1_ids[sort(p1_ids)]
  if p1_ids[0] eq 0 then p1_ids = p1_ids[1:n_elements(p1_ids)-1]
  np1s = n_elements(p1_ids)
  
  sub = constraints
  print, 'loading disturbance image 1'
  zot_img, disturbance_image1, d1_hdr, d1_dat, subset=sub, layers =[1]
   
  
  sub = constraints
  print, 'loading patch id image 2'
  zot_img, patch_id_image2, p2_hdr, p2_dat, subset=sub
  p2_ids = fast_unique_h(p2_dat,ignore=0)
  p2_ids = p2_ids[sort(p2_ids)]
  if p2_ids[0] eq 0 then p2_ids = p2_ids[1:n_elements(p2_ids)-1]
  np2s = n_elements(p2_ids)
  
  sub = constraints
  print, 'loading disturbance image 2'
  zot_img, disturbance_image2, d2_hdr, d2_dat, subset=sub, layers = [1]
  
  ;set up output images
 
  outpatchimg = p1_ids-p1_ids  ;set to zero in appropriate type
  sz = size(p1_dat, /dim)
  outproportionimg1 = bytarr(sz[0], sz[1]);(outproportionimg2=bytarr(sz[0], sz[1]) ) ;layer 1 for proportion of pixels in patch 1, 2 for 2
  outpatchimg = ulonarr(sz[0], sz[1]) ; a new patch image 
  outyearimg = intarr(sz[0], sz[1]) ;a new image of year of disturbance
  outpatchsizeimg = lonarr(sz[0], sz[1])  ;keep track of the size of patches. 
  
  ;set up an output structure 
  
  base = {id:0ul, patchsizepixels:0l, proportion:0b, year:0}
  


  
  
  ;Now go through each patch and document. 
  print, 'Number of  patches: '+string(np1s)
  print, 'Starting through them!'
  
    
  index = 1ul   ;will keep track of the index of the new patch id. 
  
  for i = 0, np1s-1 do begin
       test=i/500.
       if test eq long(test) then print, 'Patch '+string(i)+' of '+string(np1s)
      ; print, 'patch '+string(i)
       
       this = where(p1_dat eq p1_ids[i], n_in_p1) ;n_in_p1 keeps track of how many pixels in the patch are from p1
       if n_in_p1 ne 0 then begin ;why would some be zero?  It's if they've been erased from a prior patch 
                                  ;i.e. if p1 overlapped with p2 which overlapped with p1 again, all get reset
                                  
       year1 = median(d1_dat[this])
       masterwhere = this ;a master list of indices for this patch, we'll add from p2. 
     
      
       
       ;find the intersects in the other image
       intersects = d2_dat[this]  ;find the year of disturbance for the pixels in d2 image that agree
       agrees = where(intersects eq year1, countem) ;find the pixels in d2 that match the year in d1
      
       ;stop
       ;need to cycle through this 
       
       if countem ne 0 then begin   ;if we found some pixels...
          get_p2_ids = p2_dat[this[agrees]]  ;get ids from p2 that are  in the image and year match pixels
         ; print, n_elements(get_p2_ids)
          these_pd2_ids = fast_unique_h(get_p2_ids)  ;these are the ids from p2 that match
                 
          n_un = n_elements(these_pd2_ids)  ;the number of unique patches with the correct year that overlap
          n_in_p2 = 0 ;this will keep track of how many pixels in the patch are form p2
          mw_ind = n_in_p1  ;will keep track of the index of the masterwhere
          for j = 0, n_un-1 do begin  ;go through the pixels associated with the other patches and expand the patch
            these = where(p2_dat eq these_pd2_ids[j], addem)
            
            ;handle the growing masterwhere index
            expand_cols, masterwhere, addem, newdims  ;expand the masterwhere
            masterwhere[mw_ind:newdims[0]-1]=these  ;place the indices into the growing masterwhere
            mw_ind=mw_ind+addem ;update for next round if needed. 
            
            ;now check back to the first image to see what you overlap
            
            intersects2 = d1_dat[these] ;these are the ones in d1 that overlap with this patch from p2
            agrees2 = where(intersects2 eq year1, countem2)
            if countem2 ne 0 then begin 
                get_p1_ids = p1_dat[these[agrees2]]
                these_p1_ids = fast_unique_h(get_p1_ids)
                n_un2 = n_elements(these_p1_ids)
                for j2 = 0, n_un2-1 do begin 
                   these3 = where(p1_dat eq these_p1_ids[j2], addem2)
                   
                   expand_cols, masterwhere, addem2, newdims2
                   masterwhere[mw_ind:newdims2[0]-1] = these3
                   mw_ind=mw_ind + addem2
                end   ;stop at these; ideally, we should do this recursively using pointer to the big array, but too much for me to figure out now
             end  ;countem2 -- 
          end ;going through the p2 patches that intersect with the original p1 patch
        end;if there are any p2 patches that intersect 
        
        ;at this point, we've got a bunch of masterwhere.  Now we need to count stuff up in them
          
          
          ;since we didn't reset things, we have found some of these more than once.  need to cull
          zz = fast_unique_h(masterwhere)
          
          n_pixels_in_patch = n_elements(zz)
            
          ;filter here -- there could be small patches caused by water filter
          ;
          if n_pixels_in_patch gt 10 then begin
          ;then count
          inp1 = where(d1_dat[zz] eq year1, n_in_p1)
          inp2 = where(d2_dat[zz] eq year1, n_in_p2)
          proportion_p1 = (float(n_in_p1)/n_pixels_in_patch)*100b
          proportion_p2 = (float(n_in_p2)/n_pixels_in_patch)*100b
       
          ;write stuff out
          outpatchimg[zz]=index
          outyearimg[zz]=year1
          outproportionimg1[zz]=(101b-proportion_p1)+proportion_p2
          outpatchsizeimg[zz] = n_pixels_in_patch
           
         
          
          ;do the structure
          
          if index eq 1 then patchinfo = base else expand_cols, patchinfo, 1, newdims
          patchinfo[index-1].id = index
          patchinfo[index-1].patchsizepixels = n_pixels_in_patch
          patchinfo[index-1].proportion = (101b-proportion_p1)+proportion_p2
          patchinfo[index-1].year = year1
                                   
          ;now increment the index
          index = index + 1
          end  ;if there are enough pixels in the patch
        
        
          ;reset values so nothing found later.  We erase everything 
          ;   that overlaps this patch, so we don't overwrite this patch later in these
          ;locations. Otherwise we can get slivers at the end.
          
          p1_dat[zz]=0
          d1_dat[zz]=0
          p2_dat[zz]=0
          d2_dat[zz]=0
        
    end ;if there are actual pixels in p1
   end  ;looping through all of the patches
   
   ;now the only thing left is patches that are only in the P2 image
   ;  but we know that none of these overlap a p1 patch, so it's trivial to add these
   
   ;first, we see how many are left.  Any that have been used got reset to 
   ;    zerohelp
   
    p2_ids = fast_unique_h(p2_dat, ignore=0)
    p2_ids = p2_ids[sort(p2_ids)]
    if p2_ids[0] eq 0 then p2_ids = p2_ids[1:n_elements(p2_ids)-1]
    np2s = n_elements(p2_ids)
    for i = 0, np2s-1 do begin 
        this = where(p2_dat eq p2_ids[i], n)
        year = median(d2_dat[this])
        
        if n gt 10 then begin
          outpatchimg[this]=index
          outyearimg[this]=year
          outproportionimg1[this]=200b  ;all this one, so we use 200. 
         
          if index eq 1 then patchinfo = base else expand_cols, patchinfo, 1, newdims
          patchinfo[index-1].id = index
          patchinfo[index-1].patchsizepixels = n
          patchinfo[index-1].proportion = 200b
          patchinfo[index-1].year = year
      
          index=index+1
        end
        
    end
       
   
   ;now write out
   
   propimagename = outputfile_base+'_prop.bsq'
   openw, un, propimagename, /get_lun
   writeu, un, outproportionimg1
   ;writeu, un, outproportionimg2
   free_lun, un
   hdr1=p1_hdr
   hdr1.n_layers=1
   hdr1.pixeltype=3
   write_im_hdr, propimagename, hdr1
   
   
   
   patchimagename = outputfile_base+'_patchid.bsq'
   openw, un, patchimagename, /get_lun
   writeu, un, outpatchimg
   free_lun, un
   write_im_hdr, patchimagename, p1_hdr
   
   
   
   
   yearimagename = outputfile_base+'_year.bsq'
   openw, un, yearimagename, /get_lun
   writeu, un, outyearimg
   free_lun, un
   hdr1=p1_hdr
   hdr1.n_layers=1
   hdr1.pixeltype=6
   write_im_hdr, yearimagename, hdr1
   
   patchsizeimagename = outputfile_base+'_patchsize.bsq'
   openw, un, patchsizeimagename, /get_lun
   writeu, un, outpatchsizeimg
   free_lun, un
   hdr1=p1_hdr
   hdr1.n_layers=1
  
   write_im_hdr, patchsizeimagename, hdr1
   
   
   ;write out the save file
   patchinfo_file = outputfile_base + '_patchinfo.sav'
   save, patchinfo, file = patchinfo_file
      
   
   print, 'DONE WRITING IMAGES'
   
   return, {patchimagename:patchimagename, yearimagename:yearimagename, $
        patchsizeimagename:patchsizeimagename, patchinfo_file:patchinfo_file}
   
   
   
   end
   
      
       
       
       
  
  
  
  
  
  
  
  

