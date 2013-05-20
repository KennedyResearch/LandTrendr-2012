function summarize_by_combinedpatch, patch_file, target_file1,target_file2, $
    output_base, patchinfofile, tag_nickname
    
    
    ;tag nickname is the name to give to the columns associated with the target files
    
    
    

;read in the patchinfo file

restore, patchinfofile  ;structure is "patchinfo"
n_patches_st = n_elements(patchinfo)


;take a patch file of disturbance made by combining two, and summarize other files from the two two
;;   runs, for each file in all its layers by patch.
; output is an image of same dimensions and type as the target files, but with the mean or variance of the whole patch. 
; 

zot_img, patch_file, phdr, pimg

;get the histogram, focusing on the reverse indices.
;  note that we set min to 1, because we've defined the 
;  patch ids to start with 1. this avoids zero background. 

h= histogram(pimg, min=1, omin=omin, omax=omax, rev=r)
n_patches = n_elements(h) ;because we've set it up this way in the make_agreement_patch_images2 -- the number of patches will equal the number of bins.
if n_patches ne n_patches_st then stop; message, 'Number patches in the structure and the patch file do not match'


  ;usage of the reverse indices:
  ;    The subscripts of the original array elements falling in the ith bin, 0 ≤ i < N, are given by the expression: R(R[i] : R[i+1]-1), where R is the reverse index list. If R[i] is equal to R[i+1], no elements are present in the ith bin.
  ;For example, make the histogram of array A:
  ;
  ;H = HISTOGRAM(A, REVERSE_INDICES = R)
  ; 
  ;;Set all elements of A that are in the ith bin of H to 0.
  ;IF R[i] NE R[i+1] THEN A[R[R[I] : R[i+1]-1]] = 0

;read in the two target files
  ;get the subset from the patch image
  upl = phdr.upperleftcenter
  lor = phdr.lowerrightcenter
  constraints = [[upl], [lor]]
  adj_constraints=constraints
  adj_constraints[*,0] = adj_int_mult(phdr.upperleftcenter, phdr.pixelsize, constraints[*,0], /map)
  adj_constraints[*,1] = adj_int_mult(phdr.upperleftcenter, phdr.pixelsize, constraints[*,1], /map)
  
   
  subset = adj_constraints 
  zot_img, target_file1, thdr, timg1, subset=subset
  ;zot_img, target_file2, thdr, timg2, /hdronly
  
  n_layers = thdr.n_layers
 
  meanimg = timg1-timg1
  stdvimg = timg1-timg1
  if n_layers eq 1 then tempmean=(tempstdv=meanimg) else $
        tempmean = (tempstdv=meanimg[*,*,0])  ;get one layer for working through
  
  
;now go through each layer and patch
for l = 0, n_layers-1 do begin  
  subset=adj_constraints
  zot_img, target_file1, hdr, thislayer1, layers=[l+1], subset=subset
  subset=adj_constraints
  zot_img, target_file2, hdr, thislayer2, layers=[l+1], subset=subset
   print, 'Layer '+string(l)+' of '+string(n_layers-1)
   
   ;add the tags to the structure
   base = thislayer1[0]-thislayer1[0]
   addarray = replicate(base, n_patches)
   newtag=strcompress(tag_nickname+string(l+1)+'mean', /rem)
   patchinfo = struct_add_tag(patchinfo, newtag, addarray)
   dummy = tag_names(patchinfo) ;get list of all tags, with the new one at the end
   meanindex = n_elements(dummy)-1  ;the new one's index is at the end
   
   newtag=strcompress(tag_nickname+string(l+1)+'stdv', /rem)
   patchinfo = struct_add_tag(patchinfo, newtag, addarray)
   stdvindex = meanindex + 1
   
   
  for i = 0, n_patches-1 do begin
    test=i/500.
    if test eq long(test) then print, 'Patch '+string(i)+' of '+string(n_patches-1)
      
    if r[i] ne r[i+1] then begin
       indices = r[r[i]:r[i+1]-1]
    
       vals1 = thislayer1[indices]
       goods1 = vals1 ne 0
       vals2 = thislayer2[indices]
       goods2 = vals2 ne 0
       combo = goods1+(2*goods2)
       final = (((combo eq 3)*vals1)+((combo eq 3)*vals2))/2.+ $ ;where both are there, we take both
               ((combo eq 2)*vals2) + $ ;where vals2 alone has value
               ((combo eq 1)*vals1)
       mn=mean(final)
       sd=stdev(final)
       
       tempmean[indices]= mn
       tempstdv[indices]= sd ;this will be floating point initially, but will be in the same range as original, so IDL should place appropriately
       
       ;add to the structure
       matchit = where(patchinfo.id eq i+1, nmatch)
       if nmatch eq 0 then message, 'Patch '+string(i+1)+' not found in patch structure'
            
       patchinfo[matchit].(meanindex)=mn
       patchinfo[matchit].(stdvindex)=sd
       
       
       
    end ;if there are any in this bin (i.e. if r[i] ne r[i+1]
 end  ;patches
 
 ;now we've done one layer.  need to assign. 
 
    if n_layers eq 1 then begin
      meanimg=tempmean
      stdvimg=tempstdv
    end else begin
      meanimg[*,*,l]=tempmean
      stdvimg[*,*,l]=tempstdv
    end
end ;going through layers

 
 ;write them out
   outfilemean = output_base+'_mean.bsq'
   openw, un, outfilemean, /get_lun 
   writeu, un, meanimg
   free_lun, un
   write_im_hdr, outfilemean, thdr
   
   outfilestdv = output_base+'_stdv.bsq'
   openw, un, outfilestdv, /get_lun 
   writeu, un, stdvimg
   free_lun, un
   write_im_hdr, outfilestdv, thdr
   
   save, patchinfo, file = patchinfofile
;stop



return, 0
end
