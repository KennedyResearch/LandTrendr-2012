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

function find_landtrendr_files_for_cost, path, ignore_ref=ignore_ref

  ;Finds files for landtrendr
  ;This step occurs before MADCALLING
  ;It assumes that all MTBS scenes follow the standard
  ;  naming convention (6-band images ending in *_refl.img).
  ;  and that NLAPS files have been named with *_nlaps.img.
  ;IMPORTANT:  Each separate image year must be in a
  ;   subfolder under "path" that identifies the year
  ;   as that folder name (say, 1985) is how the
  ;   program figures out the year.
  ;It also assumes that the reference year image
  ;  (the one used for MADCALLING) has had basic COST
  ;  processing done (see the processing spreadsheet
  ;  for this path row), and that there now exists for
  ;   THAT YEAR ONLY, a file called "xxxxx_cost_refl.img"


  ;find MTBS files

  mtbs_files = file_search(path, '*refl.img')
  n_mtbs_files = n_elements(mtbs_files)-total(mtbs_files eq "")

  reference_file = file_search(path, '*refl_cost.img')
  n_ref_files = n_elements(reference_file)-total(reference_file eq "")
  
  if keyword_set(ignore_ref) eq 1 then goto, start_here
  if n_ref_files ne 1 then message, 'There must exist a single reference file that has had cost processing, named ***_refl_cost.img'
  start_here:

  nlaps_files = file_search(path, '*nlaps.img')
  n_nlaps_files = n_elements(nlaps_files)-total(nlaps_files eq "")

  archv_files = [file_search(path, '*archv.img'),file_search(path, '*archv.bsq')]

  where_valid_files = where(archv_files ne '', n_archv_files)

  if n_archv_files gt 0 then archv_files = archv_files[where_valid_files]

  other_files = file_search(path, '*6band.img')	;these are images either purchased or inherited and then rectified
  n_other_files = n_elements(other_files)-total(other_files eq "")

  ; duplicates an existing _refl.img

  n_all_files = n_mtbs_files + n_other_files + n_nlaps_files +n_archv_files+ n_ref_files


  allyears = intarr(n_all_files)
  type = bytarr(n_all_files)			;1 for mtbs
  ;2 for nlaps
  ;3 for reference image
  ;4 for other


  sep = path_sep()



  ;do the mtbs files first

  count = 0

  for i = 0, n_mtbs_files-1 do begin
    dir = get_pathname(mtbs_files[i])

    length =strlen(dir)
    slash = strpos(dir, sep, length-2, /reverse_search)  ;start at 1, in case there's on at the end that would confuse things
    allyears[count+i] = strmid(dir, slash+1, 4)	;get the year frm the slash


  end
  count = count + n_mtbs_files

	if (n_mtbs_files gt 0) then type[0:n_mtbs_files-1] = 1	;set to mtbs



  ;do the nlaps files next

  for i = 0, n_nlaps_files-1 do begin
    dir = get_pathname(nlaps_files[i])

    length =strlen(dir)


    slash = strpos(dir, sep, length-2, /reverse_search)  ;start at 1, in case there's on at the end that would confuse things
    allyears[count+i] = strmid(dir, slash+1, 4)	;get the year frm the slash

  end
  count = count + n_nlaps_files

  if n_nlaps_files ne 0 then type[n_mtbs_files:n_mtbs_files+n_nlaps_files-1] = 2

 ;do the archv files next

  for i = 0, n_archv_files-1 do begin
    dir = get_pathname(archv_files[i])

    length =strlen(dir)


    slash = strpos(dir, sep, length-2, /reverse_search)  ;start at 1, in case there's on at the end that would confuse things
    allyears[count+i] = strmid(dir, slash+1, 4)	;get the year frm the slash

  end
  count = count + n_archv_files

  if n_archv_files ne 0 then type[n_mtbs_files+n_nlaps_files:n_mtbs_files+n_nlaps_files+n_archv_files-1] = 5








  ;and then the other files

  for i = 0, n_other_files-1 do begin
    dir = get_pathname(other_files[i])

    length =strlen(dir)


    slash = strpos(dir, sep, length-2, /reverse_search)  ;start at 1, in case there's on at the end that would confuse things
    allyears[count+i] = strmid(dir, slash+1, 4)	;get the year frm the slash

  end
  count = count + n_other_files

  if n_other_files ne 0 then type[(n_mtbs_files+n_nlaps_files+n_archv_files):(n_mtbs_files+n_nlaps_files+n_archv_files+n_other_files-1)] = 4





  ;get the reference year

  if n_ref_files ne 0 then begin
    dir = get_pathname(reference_file)
    length = strlen(dir)
    slash = strpos(dir, sep, length-2, /reverse_search)  ;start at 1, in case there's on at the end that would confuse things
    ref_year = fix(strmid(dir, slash+1, 4))			;assumes that the cost image is based on an MTBS image!
    allyears[n_all_files-1] = ref_year
    type[n_all_files-1] = 3

  end


  ;;make sure there's just one image per year -- not used now because we want to be able
  ;   to accept multiples from one year
  ;
  ;
  ;   uniqs = fast_unique(allyears)
  ;   if n_elements(uniqs) lt n_elements(allyears) then begin
  ;      print, 'find_tbcd_files:  Please pick only one image for each year.'
  ;      print, ' Change the name of the refl.img of the non used years to something like refl_skip.img.'
  ;      print, allyears[sort(allyears)]
  ;      return, 'no'
  ;   end


  ;now put all of the files together

  m_files = strarr(n_all_files)

	if (n_mtbs_files gt 0) then m_files[0:n_mtbs_files-1] = mtbs_files


  if n_nlaps_files ne 0 then  m_files[n_mtbs_files:n_mtbs_files+n_nlaps_files-1] = nlaps_files
  IF n_archv_files ne 0 then m_files[(n_mtbs_files+n_nlaps_files):(n_mtbs_files+n_nlaps_files+n_archv_files-1s)] = archv_files


  if n_other_files ne 0 then  $
    m_files[(n_mtbs_files+n_nlaps_files+n_archv_files):(n_mtbs_files+n_nlaps_files+n_archv_files+n_other_files-1)] = other_files

  if n_ref_files ne 0 then m_files[n_all_files-1] = reference_file



  ;sort them


  yrsort = sort(allyears)
  m_files= m_files[yrsort]

  years = allyears[yrsort]
  type = type[yrsort]



  ;now get the files.

  image_files = m_files
  image_paths = m_files
  for i = 0,n_all_files-1 do image_paths = get_pathname(m_files[i])

  nbr_files = strarr(n_all_files)
  b6_files = strarr(n_all_files)
  juldays = intarr(n_all_files)
  tc_files = strarr(n_all_files)


  ;go through each directory and get the info


  for i = 0, n_all_files -1 do begin
    len= strlen(image_files[i])
    slash = strpos(image_files[i], sep, len-2, /reverse_search)+1 ;start at 1, in case there's on at the end that would confuse things


    case 1 of
      (type[i] eq 1 or type[i] eq 3):   juldayoffset = 11	;format: 5047027008621310_refl.img
      (type[i] eq 2):  juldayoffset = 13					;format: LT5047027009121150_nlaps.img
      (type[i] eq 4):  begin
      					if strmid(image_files[i], slash+9,1) eq '_' then juldayoffset = 14 else $					;format: l5046029_1995_231_albers30m_topo_6band.img
						    juldayoffset = 11			;format: 5047027008621310_6band.img
      				   end

      (type[i] eq 5):  juldayoffset = 15					;format: lt5027026_1999_205_archv.img or lt5027026_1999_205_archv.img

      ELSE:  stop ;message, 'No type associated with file '+string(i)
    endcase

    juldays[i] = fix(strmid(image_files[i],slash+juldayoffset, 3))
    if juldays[i] lt 100 or juldays[i] gt 300 then begin
        test1 = fix(strmid(image_files[i],slash+juldayoffset-1, 3))
		test2 = fix(strmid(image_files[i],slash+juldayoffset+1, 3))

        if test1 lt 300 and test1 gt 100 then juldays[i] = test1 else $
           if test2 lt 300 and test2 gt 100 then juldays[i] = test2 else $
           	   message, 'Find_landtrendr_files:  cannot find a valid julian day for '+image_files[i]
    end



    case 1 of
      (type[i] eq 1):   core = strmid(image_files[i],0, len-8)		;  assumes "_REFL.img" is the tail
      (type[i] eq 2):   core = strmid(image_files[i],0, len-9)		;  assumes "_nlaps.img" is the tail
      (type[i] eq 3):   core = strmid(image_files[i],0, len-13)		;	assumes "_cost_refl.img" is the tail
      (type[i] eq 4):   core = strmid(image_files[i],0, len-9)		;	assumes "_6band.img" is the tail
	  (type[i] eq 5):   core = strmid(image_files[i],0, len-9)		;	assumes "_archv.img" is the tail


      else: stop
    endcase


    nbr_files[i] = core + 'NBR.img'
    test = file_exists(nbr_files[i])
    if not(test) then nbr_files[i] = 'na'

    tc_files[i] = core + 'TC.img'
    test = file_exists(tc_files[i])
    if not(test) then tc_files[i] = 'na'

    b6_files[i] = core + 'B6.img'
    test = file_exists(b6_files[i])
    if not(test) then begin
      ;check if it's an l7 image with b9
      b6_files[i] = core + 'B9.img'
      test = file_exists(b6_files[i])
      if not(test) then b6_files[i] = 'na'
    end

  end



  ;we assume we're using these to build cloud masks, so we won't do that yet.



  base = { image_file:'', $
    image_path:'', $
    type:0, $				;1 mtbs 2 nonmtbs 3 reference year mtbs
    nbr_file:'', $
    tc_file:'', $
    b6_file:'', $
    year:0, $
    julday:0, $
    unique_year:0, $    ;flagged 1 if only 1 image in this year
    n_in_year:0, $		;number of images in this year
    image_priority:0, $	;priority of picking if more than one image per year
    cloudyear:0, $
    madcal_mask_file:'none',$
    cloud_diff_file:'', $
    shadow_diff_file:'', $
    tc_cloud_diff_file:'', $
    cloud_file:'none', $
    subset:[ [0.d, 0.d],[0.d, 0.d]], $
    useareafile: ''}


  info = replicate(base, n_all_files)


  info.image_file = image_files
  info.nbr_file = nbr_files
  info.b6_file = b6_files
  info.tc_file = tc_files
  info.type = type
  info.year = years
  info.julday = juldays


  ;now take out any files that are duplicated with the
  ;   reference image



  ref_index = where(info.type eq 3, nrefs)
  if nrefs ne 0 then begin

    ;find matches
    dupe = (info.julday eq info[ref_index].julday) * $
      (info.year eq info[ref_index].year) * $
      (info.type ne 3)
    keeps = where(dupe eq 0)
    info = info[keeps]	;just ignore hthe dupes
  ;reference year and the mtbs years
  end

  ;calculate median julian day

  mj = median(info.julday)


  ;finally, for any years where there are more than 1 image
  ;   identify the order of preference in picking them,
  ;   based on relation to median julian day

  unique_years = fast_unique(info.year)
  n_uniqs = n_elements(unique_years)

  for i = 0, n_uniqs-1 do begin
    this = where(info.year eq unique_years[i], n)
    info[this].n_in_year = n
    if n eq 1 then info[this].unique_year = 1
    if n gt 1 then begin
      dist = abs(info[this].julday - mj)
      info[this].image_priority = dist
    end
  end

  return, info

end


