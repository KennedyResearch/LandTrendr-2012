pro summarize_by_patch, patch_file, pathrow, sumby_info, outcsvfile


;given a patch_id file, summarize the desired vals for sumby_info -
;   mean, stdv, median, mode, range
;   print, 'Options:  mean, stdev, med, range, mode'
;   put into outcsvfile
;   also, get basic patch information (size, shape stuff) if not already done. 
;   if outcsvfile already exists, it will only add on stuff that hasn't been done
;file_nicknames is the string that will get attached at the beginning of the column name. 
;
;
;OLD: target_file1,target_file2, $
;    output_base, patchinfofile, tag_nickname
    
    mmu= 11 ;ensure that all patches are 11 pixels or more. 
        
    
 ;check on the sumby INFO structure
;structure:
;     {file:<string, full path>, layer:<integer for the layer>, $
;     nickname:<string to use to label the column header>, $
;     stat: <string for which statistic to calculate}

n_sumbys = n_elements(sumby_info)
sumbytype = size(sumby_info, /type)
if sumbytype ne 8 then begin 
      print, 'sumby_info must be a structure'
      return
      end
      
;figure out the tag names we'll be using later.  Need these 
;  now so we can do comparison if the .csv file already exists. 


sumby_column_names = strarr(n_sumbys)
for f = 0, n_sumbys-1 do $
  sumby_column_names[f] = strcompress(sumby_info[f].nickname+sumby_info[f].stat, /rem)
      
    
;PREP THE OUTCSVFILE. 
;   If it's there, read it to see what we already have, and if not, start building it. 

  if file_exists(outcsvfile) ne 0 then begin
      ;first, load it in
      
      v = read_generic_csv(outcsvfile)
      info=v.vals
      
      
      these_tn = tag_names(info)
      n_these_tn = n_elements(these_tn)
      
      notexisting_flag = bytarr(n_sumbys)  ;a flag to keep track of which sumbys have already been done
      
      for i = 0, n_sumbys-1 do $
          notexisting_flag[i] = (matchstr(these_tn, sumby_column_names[i])) eq -1
      
        ;at this point, all "notexisting_flag" vals set to 1 are new
        
      n_new = total(notexisting_flag)
      
      if n_new eq 0 then begin
          print, 'summarize_by_patch:  existing csv file already has all information requested'
          print, 'file: '+outcsvfile
          return
      end 
      
      ;if we have new ones, we have to 
      ;  get prepped. 
      
      skip_patch_stats = 1
      news = where(notexisting_flag eq 1)
      sumby_info = sumby_info[news] ;subset the sumby_info
      n_sumbys=n_elements(sumby_info)
      sumby_column_names = strarr(n_sumbys)
     for f = 0, n_sumbys-1 do $
      sumby_column_names[f] = strcompress(sumby_info[f].nickname+sumby_info[f].stat, /rem)
      
    
  end else skip_patch_stats = 0   ;if the csvfile does not exist, set this flag so patch stats will get called. 
  
 ;CHECK TO SEE IF THE IMAGE LIST HAS BEEN GENERATED
      
     metafile = file_dirname(patch_file)+path_sep()+stringswap(file_basename(patch_file), '.bsq', '_meta.txt')
   
if file_exists(metafile) then begin
          imagelist_file = stringswap(outcsvfile, '.csv', '_image_list.csv')
          if file_exists(imagelist_file) eq 0 then begin  ;if not already made
              base = {platform:'', imgyear:0, julday:0, reflfile:'', tcfile:'', cloudfile:''}
              icount = 0
              cloudfile = '' 

   		;get the path name for the images
                    zed = strpos(patch_file, 'outputs')
                    baseimagepath = strmid(patch_file, 0, zed)+'images'+path_sep()

              openr, mun, metafile,/get_lun
              while cloudfile ne 'no_match' do begin
                 query_file2, mun, 'DATA: ledaps image', /norestart
                 query_file2, mun, 'FILENAME: ', imagefile, /norestart, /nextlineonly
                 query_file2, mun, 'CLOUDMASK_FILE: ', cloudfile, /norestart, /nextlineonly
                 if cloudfile ne 'no_match' then begin
                   query_file2, mun, 'TASSELED_CAP_FILE: ', tcfile, /norestart, /nextlineonly
                    ;get the year
                   thisname = file_basename(strcompress(imagefile, /rem))
                   breakone = strpos(thisname, '_')
                   year = fix(strmid(thisname, breakone+1, 4))
                   julday = fix(strmid(thisname, breakone+6, 3))
                   if year lt 1970 or year gt 2050 then message, 'could not get year.  image file '+thisname
                   if julday lt 0 or julday gt 366 then message, 'could not get julday.  image file '+thisname
   
		   ;get the full pathname 
	            imagefile = baseimagepath+strcompress(string(year),/rem)+path_sep()+$
					strcompress(imagefile, /rem)
		    tcfile = baseimagepath+strcompress(string(year),/rem)+path_sep()+$
                                        strcompress(tcfile, /rem)
		    cloudfile = baseimagepath+strcompress(string(year),/rem)+path_sep()+$
                                        strcompress(cloudfile, /rem) 	

		
                   if icount eq 0 then imagelist_struct = base else expand_cols, imagelist_struct, 1, newdims
                   imagelist_struct[icount].reflfile = strcompress(imagefile, /rem)
                   imagelist_struct[icount].tcfile = strcompress(tcfile, /rem)
                   imagelist_struct[icount].cloudfile = strcompress(cloudfile,/rem)
                    imagelist_struct[icount].imgyear = year
                   imagelist_struct[icount].julday = julday
		   if strmid(thisname,2,1) eq '5' then imagelist_struct[icount].platform = 'TM' else $
                            imagelist_struct[icount].platform = 'ETM'
         
                   icount=icount+1
                 end
              end
;              ;presumably if we are here, it has stopped because the cloudmask
;              ;  did not work -- that means the block of filename, cloudmask_file, and tasseled_cap_file
;              ;  has ended.  But it means that we added an unnecessary image at the end
;              
;               imagelist_struct = imagelist_struct[0:count-2] ;truncate last one. 
;               

		;fix dupes
		zzz = fast_unique(imagelist_struct.reflfile)
			;order the unique files
		nzzz = n_elements(zzz)	;how many uinque files
		zzz_ind = lonarr(nzzz)	;set up index
		for ifile = 0, nzzz-1 do $
		   zzz_ind[ifile] = (where(imagelist_struct.reflfile eq zzz[ifile]))[0]
		
		;now assign them
		imagelist_struct=imagelist_struct[zzz_ind]
 		    



	;	print, 'need to set up approach to get unique vals for each of these in case we somehow snage dupes'
	;	print, 'consider fast_Unique, then loop of wheres'
	;	stop


              export_structure_to_file,imagelist_struct,  imagelist_file, /include_type
               
          end  ;making the image list file
          
          
    end else print, 'WARNING:  NO METADATA FILE FOUND -- NO IMAGE LIST FILE CREATED' ;checking the metafile 
    
   
  
  
  
  
 ;GET THE PATCH INFO
  
zot_img, patch_file, phdr, pimg

 ;get the subset from the patch image
  upl = phdr.upperleftcenter
  lor = phdr.lowerrightcenter
  constraints = [[upl], [lor]]
  adj_constraints=constraints
  adj_constraints[*,0] = adj_int_mult(phdr.upperleftcenter, phdr.pixelsize, constraints[*,0], /map)
  adj_constraints[*,1] = adj_int_mult(phdr.upperleftcenter, phdr.pixelsize, constraints[*,1], /map)
    
  master_subset = adj_constraints 

;get the histogram, focusing on the reverse indices.
;  note that we set min to 1, because we've defined the 
;  patch ids to start with 1. this avoids zero background. 
 ;usage of the reverse indices:
  ;    The subscripts of the original array elements falling in the ith bin, 0 ≤ i < N, are given by the expression: R(R[i] : R[i+1]-1), where R is the reverse index list. If R[i] is equal to R[i+1], no elements are present in the ith bin.
  ;For example, make the histogram of array A:
  ;
  ;H = HISTOGRAM(A, REVERSE_INDICES = R)
  ; 
  ;;Set all elements of A that are in the ith bin of H to 0.
  ;IF R[i] NE R[i+1] THEN A[R[R[I] : R[i+1]-1]] = 0

  h= histogram(pimg, min=1, omin=omin, omax=omax, rev=r)
  nonzeros = where(h ge mmu, n_patches) ;many numbers have patches that were too small and thus filtered
 
;at this point, if the csv file already exists, we don't need to recalculate all 
; of the patch stuff. 


if skip_patch_stats eq 0 then begin 

;develop the edge image

  maskoutinterior = 1-erode(pimg, intarr(3,3)+1)  ;only interiors will have 0's
  pimg_edge = pimg * maskoutinterior  ;now all of the interiors are zeroed. 
  he = histogram(pimg_edge, min=1, omin=ominE, omax=omaxE, rev=re)
  nonzerosE = where(he ge mmu, n_patches_e) ;could have more patches if an isthmus gets broken. 
          ;just keep in mind that you might have multiple groups for a given patch. 
          
     

;BASE INFORMATION FIRST:  X,Y POSITION, NUMBER OF PIXELS, EDGE, SHAPE INDEX
    patchfilebase = file_basename(patch_file)
    
    base = {patchfilename:patchfilebase, object_id:0ul, plot_id:0ul, x:0., y:0., latx:0., laty:0., $
              pixelcount:0ul, $
              edgepixelcount:0ul, edgearearatio:0., tsa: 0ul, $
              dom_axis_length:0., norm_axis_length:0., ratio_dom_norm:0., $
              variance_explained_dom:0., ratio_dom_area:0.}
              
              
    info = replicate(base, n_patches) ;to keep track of the patches

;FIRST IDENTIFY THE REFERENCE POINT OF THE PATCH
; mostly this should be the center of mass, but if a weird shape, make sure it's within the patch

;get map information set up.  For now, assume that it's albers conical equal area

  mapstruct = map_proj_init(103,semimajor_axis = 6378137.0, semiminor_axis=6356752.3, $
      standard_par1=29.5, standard_par2=45.5, $
        center_longitude=-96.00, center_latitude=23, /gctp)
;get the kernel for the edge
   k = intarr(3,3)+1
          
  
for i = 0, n_patches-1 do begin
    test=i/5000.
    if test eq long(test) then print, 'Patch '+string(i)+' of '+string(n_patches-1)
    ;we want to use the histogram reverse indices, but we don't need for all
    ;  of the zero counts.  So we use the nonzeros to help
    
    thisindex = nonzeros[i] ;get the i'th patch
    
    ;MAIN LOOP THROUGH PATCHES 
    
    if (r[thisindex+1]-r[thisindex]) gt mmu then begin    ;need to have mmu pixels at least to say something about the patch
     
     ;Get the reference point for later spectral values. 
     
       indices = r[r[thisindex]:r[thisindex+1]-1] ;which pixels in this patch?
       xys = getxy(indices, phdr.filesize[0], phdr.filesize[1]) ; what are X,Y coords?
       targetcenter = [round(mean(xys[0,*])), round(mean(xys[1,*]))]
       diffx=abs(xys[0,*]-targetcenter[0])  ;get distance of each pixel from the target
       diffy=abs(xys[1,*]-targetcenter[1])  ;  for x and y
       td= diffx+diffy  ;add them together
       md= min(td)      ;find minimum
       z =where(td eq md, many)
       hit = z[0] ;get the pixel nearest to the target, and if >1, get the first 
       refcoord = xys[*,hit]
       
       ;now we have the pixel identified, but need to convert back to reg coords
       
       xcoord_native = (refcoord[0]*phdr.pixelsize[0])+phdr.upperleftcenter[0]
       ycoord_native = phdr.upperleftcenter[1]-(refcoord[1]*phdr.pixelsize[1])
       
       ;convert to latlong
       
       these = map_proj_inverse(xcoord_native, ycoord_native, map_structure = mapstruct)
       thisID = pimg[indices[0]]  ;get one pixel as an example (all have same ID)
       
       info[i].object_id = thisID
       info[i].plot_id = thisID
       info[i].tsa = pathrow
       info[i].x = xcoord_native
       info[i].y = ycoord_native
       info[i].latx = these[0]
       info[i].laty = these[1]
  
  
     ;now deal with edges and shape stuff
       
       info[i].pixelcount = n_elements(indices)
       
       
       ;can't use the morphology on the whole image, since a patch embedded in another patch will not get cleaned out. 
        
         xmn = min(xys[0,*], max=xmx)
         xmn = max([0, xmn-2])  ;want a 2-pixel buffer around the edge
         xmx = min([phdr.filesize[0]-1, xmx+2]) 
                  
         ymn = min(xys[1,*], max=ymx)
         ymn = max([0, ymn-2])
         ymx = min([phdr.filesize[1]-1, ymx+2])
                 
         
         temp = pimg[xmn:xmx, ymn:ymx] eq thisID ;get the pixels
         edge = temp * (1-erode(temp, k)) ;only edge pixels will have a 1
         he = histogram(edge, min=1, omin=omin, omax=omax, rev=re)
         nonzerosE = where(he ne 0, n_patches_e) 
         
         ;this histogram is just of 0,1, so the histogram is just one value (since we set min to 1)
                
         info[i].edgepixelcount = he[0] ;just use the histogram to get the count of edge pixels
         info[i].edgearearatio = float(info[i].edgepixelcount)/info[i].pixelcount

;         print, ' '
;         print, 'Edge area ratio'
;         
;         print, info[i].edgearearatio
;       
         
         
       ;get the greatest distance and the d
       ;   distribution of edge pixels
      
       indicesE = rE[rE[0]:rE[1]-1] ;which pixels in this patch?
       szi = size(temp, /dim)
       
       xysE = getxy(indicesE, szi[0], szi[1]) ; what are X,Y coords?
       dax = get_dominant_axis_info(xysE)
       info[i].dom_axis_length = dax.dominant_axis_length
       info[i].norm_axis_length = dax.normal_axis_length
       info[i].ratio_dom_norm = dax.ratio_dominant_to_normal
       info[i].variance_explained_dom = dax.variance_explained_by_dominant
       info[i].ratio_dom_area = dax.dominant_axis_length / info[i].pixelcount
       
       
          ;maxpairE = max_dist_pair(xysE)
          ;indists = pnt_line_rek(xysE, maxpairE[*,0], maxpairE[*,1], pl, pointonline)
       ;          w, 0
;          plot, xysE[0,*], xysE[1,*], psym = 4, /ynoz
;          pst, dax
;          
;          aaa=get_kbrd()
       
     ;stop;  distmatrix = fake_distance_matrix(xys) ;use the x,y coords
       
       
       
;stop
    end
end

end ;if skip patch stats eq 0 -- this is the end of the section if there is no csv file yet. 

;FILTER
  ;there are some that don't have enough pixels to get a hit, so they end up zeros. 
  ;  here, we want to eliminate those
  
  
  good_patches = where(info.object_id ne 0, n_patches_real) ;
  info = info[good_patches]
  
;NEXT MAJOR STEP:  EXTRACTION

;we already defined the number of elements above -- n_sumbys
;   if we are just adding a column, the sumbys have already been subset to 
;   reflect only the new one. 


for f = 0, n_sumbys -1 do begin 
   subset = master_subset
      
   zot_img, sumby_info[f].file, hdr, img, subset=subset, layers = [sumby_info[f].layer], /hdronly
   
   ;check to make sure it's the same size as the patch image. 
   
    check = hdr.filesize-phdr.filesize
    if total(check) eq 0 then begin
        subset=master_subset
         zot_img, sumby_info[f].file, hdr, thislayer1, subset=subset, layers = [sumby_info[f].layer]
     end else  begin
	;if we're here, that means the extraction image is smaller than the patch image. 
	;  if it's larger, it's already been truncated down to the patch image.
	;  so all we have to do is fill out the gap.  Use fix edge, with the extraction
	;  image as the constraint image and the patch image as the proposed image

          s1 = master_subset    ;this is the patch image
	  s2 = subset		;this one has been truncated to match the patch
	  d1 = fix_edge(s1, s2, /map)	 ;proposed, constraint
	  diff1 = d1.diffs / [[hdr.pixelsize], [hdr.pixelsize]]
	  
          subset=master_subset
          zot_img, sumby_info[f].file, hdr, temp, subset=subset, layers = [sumby_info[f].layer]
          thislayer1 = replicate(temp[0]-temp[0], phdr.filesize[0]) 	;set up zeros of appropriate dta type
	  thislayer1 = fill_arr(thislayer1, phdr.filesize[1])	;then fill down
	  upleft = long(diff1[*,0])*[-1,1]
	  thislayer1[upleft[0]:upleft[0]+hdr.filesize[0]-1, upleft[1]:upleft[1]+hdr.filesize[1]-1]=temp
    end
 


   print, 'Working on '+sumby_info[f].nickname
   
   ;add the tags to the structure
   base = thislayer1[0]-thislayer1[0] ;just get the first element as a template
   
   addarray = replicate(base, n_patches_real)  ;make a column of these zeros
   
   newtag=sumby_column_names[f]
   info = struct_add_tag(info, newtag, addarray)
   dummy = tag_names(info) ;get list of all tags, with the new one at the end
   statindex = n_elements(dummy)-1  ;the new one's index is at the end
   stat = sumby_info[f].stat  ;use for layer
   
   
   for i = 0, n_patches_real-1 do begin
    test=i/5000.
    if test eq long(test) then print, 'Patch '+string(i)+' of '+string(n_patches_real-1)
    
    thisindex = info[i].object_id-1  ;the histogram (defined way above) was set to a minimum of 1.
                                  ;thus, patch 105 shows up in the histogram at spot 104.  
                                  ;We need to find the location of this patch this way, because
                                  ;the info.object_id already has been truncated from the original 
                                  ; histogram, since there were some that did not have values.  This is 
                                  ; particularly needed if we write out the .csv and are reading it in here.  
                                  
    ;thisindex = nonzeros[i] ;get the i'th patch
 
  ;now, find the pixels and get stat! 
 
    if r[thisindex+1] - r[thisindex] gt mmu then begin
       indices = r[r[thisindex]:r[thisindex+1]-1]
    
     ;as a doublecheck, we make sure that the image matches
     ;we're going to need this later anyway
     
         thisID = pimg[indices[0]]  ;get the unique id of these pixels to match up. 
          
         matchit = where(info.plot_id eq thisID, nmatch)
         if nmatch eq 0 then message, 'Patch '+string(thisID)+' not found in patch structure'
                           
                                      ;OLD:  t
    
       vals1 = thislayer1[indices]
       goods1 = where( vals1 ne 0, n)
       if n gt mmu then begin 
        
         final= vals1(goods1)
         
         
         case 1 of
         stat eq 'mean': thisval=mean(final)
         stat eq 'stdev': thisval=stdev(final)
         stat eq 'med': thisval=median(final)
         stat eq 'range': thisval= range(final)
         stat eq 'mode': begin 
                        j=histogram(final, omin=omin)
                        thisval = (where(j eq max(j)))[0]+omin
                       
                        
                        end
         else:  begin 
                  print, 'Stat type not found'
                  print, 'Options:  mean, stdev, med, range, mode'
                  return
                end
         endcase
                                          
         ;add to the structure
         ;note that matchit was determined before the stats. 
               
         info[matchit].(statindex)=thisval
                 
      endif
      
       
    end ;if there are any in this bin (i.e. if r[i] ne r[i+1]
 end  ;patches
 
end ;sumby info


;WRITE IT OUT!  


export_structure_to_file, info, outcsvfile, /include_type 


return
end
