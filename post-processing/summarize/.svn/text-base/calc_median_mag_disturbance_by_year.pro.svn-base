;**************************************************************************** 
;Copyright © 2008-2011 Oregon State University                                
;All Rights Reserved.                                                         
;                                                                             
;                                                                             
;Permission to use, copy, modify, and distribute this software and its        
;documentation for educational, research and non-profit purposes, without     
;fee, and without a written agreement is hereby granted, provided that the    
;above copyright notice, this paragraph and the following three paragraphs    
;appear in all copies.                                                        
;                                                                             
;                                                                             
;Permission to incorporate this software into commercial products may be      
;obtained by contacting Oregon State University Office of Technology Transfer.
;                                                                             
;                                                                             
;This software program and documentation are copyrighted by Oregon State      
;University. The software program and documentation are supplied "as is",     
;without any accompanying services from Oregon State University. OSU does not 
;warrant that the operation of the program will be uninterrupted or           
;error-free. The end-user understands that the program was developed for      
;research purposes and is advised not to rely exclusively on the program for  
;any reason.                                                                  
;                                                                             
;                                                                             
;IN NO EVENT SHALL OREGON STATE UNIVERSITY BE LIABLE TO ANY PARTY FOR DIRECT, 
;INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST      
;PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
;IF OREGON STATE UNIVERSITYHAS BEEN ADVISED OF THE POSSIBILITY OF SUCH        
;DAMAGE. OREGON STATE UNIVERSITY SPECIFICALLY DISCLAIMS ANY WARRANTIES,       
;INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
;FITNESS FOR A PARTICULAR PURPOSE AND ANY STATUTORY WARRANTY OF               
;NON-INFRINGEMENT. THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS,    
;AND OREGON STATE UNIVERSITY HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE,       
;SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.                            
;                                                                             
;**************************************************************************** 

function calc_median_mag_disturbance_by_year, change_label_filename, output_csv, $
      stratifying_files=stratifying_files

;layer to summarize by:  
  layer = 1   ;we assume that we're using year from the first layer
  maglayer = 2  ;

;first, find the area we're going to be using for stratifying the summaries
 zot_img, change_label_filename, chdr, cimg, /hdronly

s_st = size(stratifying_files, /dim)

if n_elements(s_st) eq 1 then n_stratifying_files = 1 else n_stratifying_files = s_st[1]
  
if n_stratifying_files ne 0 then begin
     for i = 0, n_stratifying_files-1 do begin 
      
        if file_exists(stratifying_files[0,i]) eq 0 then message, 'Could not find '+stratifying_files[i]
        
        zot_img, stratifying_files[0,i], hdr, img, /hdronly
        
        if i eq 0 then constraints = [[hdr.upperleftcenter], [hdr.lowerrightcenter]]
        
        constraints[0,0] = max([constraints[0,0], hdr.upperleftcenter[0]])  ;find left side
        constraints[0,1] = min([constraints[0,1], hdr.lowerrightcenter[0]]) ;find right side
        constraints[1,0] = min([constraints[1,0], hdr.upperleftcenter[1]])  ;find top
        constraints[1,1] = max([constraints[1,1], hdr.lowerrightcenter[1]]) ;find bottom
        
          ;note that these coords are all in relation to their own files and may not be integer mults of each other. 
          
     end
      ;at this point the constraints should be the union area of the stratifying files. 
      ;now cast in terms of the change label filename
      
 end else constraints = [[chdr.upperleftcenter], [chdr.lowerrightcenter]]
 
      
      adj_constraints = constraints
      adj_constraints[*,0] = adj_int_mult(chdr.upperleftcenter, chdr.pixelsize, constraints[*,0], /map)
      adj_constraints[*,1] = adj_int_mult(chdr.upperleftcenter, chdr.pixelsize, constraints[*,1], /map)
 
 
 zot_img, change_label_filename, chdr, yearimg, layers = [1], subset=adj_constraints
 
 if n_stratifying_files ne 0 then begin 
   uniquecombos = lonarr(chdr.filesize[0], chdr.filesize[1])
   
   zot_img, stratifying_files[0,0], firstimghdr, firstimg, subset=adj_constraints
   ok = create_lookup_table_image(firstimg)
   if ok.ok ne -1 then begin
      image1 = ok.outimage
      lookuptable = ok.lookup ;first element is index in new image, second is the original value
   end
     
   for i =1, n_stratifying_files-1 do begin 
   
     zot_img, stratifying_files[0,i], image2hdr, image2, subset=adj_constraints
          
    
      ok = combine_two_images(image1, image2, image1_lookup = lookuptable) ;image1 is always the base, image2 the newbie
      
     if ok.ok ne 0 then begin
         image1 = ok.outimage
         lookuptable = ok.lookup
        
     end
   end
  
 
 end else begin     ;there is not a stratifying file, so use the change label file as a proxy
    image1 = (yearimg-yearimg)+1
    lookuptable = [1,1]
     stratifying_files=[' ', 'All']
 end
 
 ;now set up the summary stuff
  
  ;the structure will be:
  ;   class in stratimage1, class in stratimage2, etc. ; earliest year thru last year
  
  command = 'base = {'
  for i = 0, n_stratifying_files-1 do command = command + stratifying_files[1,i]+':"", '
  
  ;how many years?
  ;   we use layer 1 (set at beginning of file) to get the year
  
    zot_img, change_label_filename, chdr, cimg, layers=[layer], subset=adj_constraints
    
    z = fast_unique(cimg)
    z=z[sort(z)]
    
    zmin = min(z[where(z ne 0)])
    zmax = max(z[where(z ne 0)])
    n_years = zmax-zmin+1   ;populate all years even if gaps
    
    years = indgen(n_years)+zmin
    
    for i = 0, n_years-2 do $ 
      command = command + strcompress('y'+string(years[i]), /rem)+':0., '
    
    command = command + strcompress('y'+string(years[n_years-1]), /rem)+':0.}'
    
    
    
    ok = execute(command)
        ;now we have the base structure 
   
    ;get the year pointers
    
      yrpointers = lindgen(n_years)
      match_yr = (where(tag_names(base) eq strupcase(strcompress('y'+string(years[0]), /rem))))
      
      yrpointers = yrpointers + match_yr[0]
   
   ;replicate it
      
      sz = size(lookuptable, /dim)
      summary = replicate(base, sz[1])
      
   ;now get the median image from the maglayer
   
      zot_img, change_label_filename, maghdr, magimg, layers=[maglayer], subset=adj_constraints
   
   
     
     for i = 0, sz[1]-1 do begin 
        
         for j = 0, n_stratifying_files-1 do summary[i].(j) = lookuptable[j+1, i]  ;set each column associated with the stratifying files to the value in that file for which this row applies
         
         pixels_in_this_stratum = (image1 eq lookuptable[0,i])
                  
         for j = 0, n_years-1 do begin 
             match = (cimg eq years[j])
             crossmatches = where(match * pixels_in_this_stratum eq 1, many)
             if many ne 0 then outval = median(magimg[crossmatches]) else outval = 0
             
             summary[i].(yrpointers[j]) = round(outval)
         end
      end
      
 
 export_structure_to_file, summary, output_csv
 
 end
 
 